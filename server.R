# Program: ShinyDisVar
# Developer: Khunanon Chanasongkhram

library(DisVar)
library(shiny)
library(sqldf) # For SQL-style data querying
library(data.table) # Read large VCF file
library(dplyr) # For data manipulation
library(shinyjs) # Show/hide elements
library(tools) # For file_ext function
library(DT) # For interactive tables
library(plotly) # For interactive plots
library(shinycssloaders) # For loading animations
library(shinydashboard) # For dashboard layout
library(waiter) # For loading screen
library(shinyjs) # For JavaScript interactions
library(shinyalert) # For custom alerts
library(writexl) # For writing Excel files

options(shiny.maxRequestSize = 9000*1024^2)

server <- function(input, output, session) {
  # Initialize waiter loading screen
  w <- Waiter$new(
    html = spin_flower(),
    color = transparent(.3)
  )

  # Initialize reactive values
  values <- reactiveValues(
    vcf_data = NULL,
    result_data = NULL,
    processing_status = NULL,
    error_message = NULL,
    last_update = Sys.time(),
    processing_history = data.frame(
      timestamp = character(),
      action = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
  )

  # Disable buttons until ready
  shinyjs::disable("runButton")
  shinyjs::disable("downloadData")
  shinyjs::disable("downloadExcel")

  observeEvent(input$vcfFile, {
    if (!is.null(input$vcfFile)) {
      shinyjs::enable("runButton")
    } else {
      shinyjs::disable("runButton")
    }
  })

  # Download handler for test file
  output$downloadTestFile <- downloadHandler(
    filename = function() {
      "Test_VCF_file.vcf"
    },
    content = function(file) {
      file.copy("Test_VCF_file.vcf", file)
    }
  )

  # Helper function for processing history
  log_action <- function(action, status) {
    values$processing_history <- rbind(
      values$processing_history,
      data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
        action = action,
        status = status,
        stringsAsFactors = FALSE
      )
    )
  }

  # Helper function to read VCF file with progress updates
  read_vcf_with_progress <- function(filepath, is_gz = FALSE, progress_obj) {
    tryCatch({
      # Validate file path
      if (!file.exists(filepath)) {
        stop("The specified file does not exist.")
      }

      # Count total lines
      progress_obj$set(message = "Counting total lines...", value = 0)
      total_lines <- if (is_gz) {
        as.numeric(system(sprintf("zcat '%s' | wc -l", filepath), intern = TRUE))
      } else {
        as.numeric(system(sprintf("wc -l < '%s'", filepath), intern = TRUE))
      }

      if (is.na(total_lines) || total_lines <= 0) {
        stop("Failed to count lines or file is empty.")
      }

      # Create a temp file for processed data
      temp_file <- tempfile()
      progress_obj$set(message = "Processing variants...", value = 0.1)

      # AWK command to process VCF
      cmd <- if (is_gz) {
        sprintf("zcat '%s' | awk '!/^##/' > '%s'", filepath, temp_file)
      } else {
        sprintf("awk '!/^##/' < '%s' > '%s'", filepath, temp_file)
      }

      # Execute the AWK command
      system(cmd)

      # Verify the temp file has data
      if (file.info(temp_file)$size == 0) {
        stop("No data was extracted from the VCF file.")
      }

      # Read the processed VCF into memory
      progress_obj$set(message = "Reading processed data...", value = 0.8)
      vcf_data <- fread(temp_file,
                        header = TRUE,
                        sep = "\t",
                        select = 1:8,
                        showProgress= FALSE)

      # Clean up and return
      unlink(temp_file)
      progress_obj$set(message = "Completed.", value = 1)
      return(vcf_data)
    }, error = function(e) {
      # Log the error and return NULL
      progress_obj$close()
      message("Error in read_vcf_with_progress: ", e$message)
      return(NULL)
    })
  }

  # VCF file validation function
  validate_vcf <- function(file) {
    tryCatch({
      if (is.null(file)) return(list(valid = FALSE, message = "No file selected"))

      ext <- tools::file_ext(file$name[1])
      # Check if it's a .vcf.gz file
      if (grepl("\\.vcf\\.gz$", file$name[1])) ext <- "vcf.gz"

      if (!ext %in% c("vcf", "vcf.gz", "gz")) {
        return(list(valid = FALSE, message = paste0("Invalid file type: ", file$name,
                                                    ". Please upload .vcf or .vcf.gz files only.")))
      }

      # Check file size
      if (file$size > 9000*1024^2) {
        return(list(valid = FALSE, message = paste0("File too large: ", file$name,
                                                    ". Maximum size is 9GB.")))
      }

      # Basic header check
      first_line <- if(ext %in% c("gz", "vcf.gz")) {
        system(sprintf("zcat '%s' 2>/dev/null | head -n 1", file$datapath), intern = TRUE)
      } else {
        readLines(file$datapath, n = 1)
      }

      if (length(first_line) == 0 || !grepl("^##", first_line)) {
        return(list(valid = FALSE, message = "Invalid VCF format. File must start with ##."))
      }

      return(list(valid = TRUE, message = "File validation successful."))
    }, error = function(e) {
      return(list(valid = FALSE, message = paste("Validation error:", e$message)))
    })
  }

  # Show list of uploaded files
  output$uploadedFilesList <- renderUI({
    req(input$vcfFile)
    files <- input$vcfFile
    if (nrow(files) == 0) return(NULL)

    tagList(
      tags$p(strong(sprintf("%d file(s) uploaded:", nrow(files))),
             style = "margin-top: 8px; margin-bottom: 4px;"),
      tags$ul(
        lapply(seq_len(nrow(files)), function(i) {
          tags$li(
            sprintf("%s (%.2f MB)", files$name[i], files$size[i] / 1024^2)
          )
        })
      )
    )
  })
  # File preview handler
  output$vcfPreviewHeadTail <- renderDT({
    req(values$vcf_data)

    # Check if total rows is less than 20
    if (nrow(values$vcf_data) <= 20) {
      preview_data <- values$vcf_data  # Show all rows if less than or equal to 20
    } else {
      preview_data <- rbind(
        head(values$vcf_data, 10),
        tail(values$vcf_data, 10)
      )
    }

    datatable(preview_data,
              rownames = FALSE,
              options = list(
                dom = 't',
                scrollX = TRUE,
                pageLength = 20
              ))
  })

  # File upload handler with progress updates
  observeEvent(input$vcfFile, {
    w$show()
    files <- input$vcfFile

    # Validate all files first
    for (i in seq_len(nrow(files))) {
      validation <- validate_vcf(files[i, ])
      if (!validation$valid) {
        shinyalert(title = "Validation Error", text = validation$message, type = "error")
        w$hide()
        return()
      }
    }

    log_action("File Upload", "Started")

    withProgress(message = 'Processing VCF file(s)...', value = 0, {
      tryCatch({
        local_progress <- list(
          set = function(message, value) setProgress(value = value, message = message),
          close = function() {}
        )

        n_files <- nrow(files)

        if (n_files == 1) {
          local_progress$set(message = "Reading VCF file...", value = 0.1)
          is_gz <- isTRUE(grepl("\\.gz$", files$name[1]))
          data <- read_vcf_with_progress(files$datapath[1], is_gz, progress_obj = local_progress)
          if (is.null(data)) stop("Failed to read VCF file.")   # ← NULL check

        } else {
          local_progress$set(message = "Merging VCF files...", value = 0.05)
          merged_temp <- tempfile(fileext = ".vcf")

          for (i in seq_len(n_files)) {
            local_progress$set(
              message = sprintf("Processing file %d of %d: %s", i, n_files, files$name[i]),
              value = 0.05 + (i - 1) / n_files * 0.6
            )

            is_gz <- isTRUE(grepl("\\.gz$", files$name[i]))
            src <- files$datapath[i]

            if (i == 1) {
              cmd <- if (is_gz) {
                sprintf("zcat '%s' 2>/dev/null | awk '!/^##/' >> '%s'", src, merged_temp)
              } else {
                sprintf("awk '!/^##/' '%s' >> '%s'", src, merged_temp)
              }
            } else {
              cmd <- if (is_gz) {
                sprintf("zcat '%s' 2>/dev/null | awk '!/^#/' >> '%s'", src, merged_temp)
              } else {
                sprintf("awk '!/^#/' '%s' >> '%s'", src, merged_temp)
              }
            }
            system(cmd)
          }

          if (file.info(merged_temp)$size == 0) stop("Merged VCF file is empty.")

          local_progress$set(message = "Reading merged data...", value = 0.7)
          data <- fread(merged_temp, header = TRUE, sep = "\t", select = 1:8, showProgress = FALSE)
          unlink(merged_temp)

          if (is.null(data) || nrow(data) == 0) stop("No variant data could be loaded from merged file.")
        }

        values$vcf_data <- data
        values$processing_status <- "File(s) loaded successfully"
        log_action("File Upload", "Completed")

        shinyalert("Success",
                   paste0(
                     if (n_files > 1) paste0(n_files, " files merged. ") else "",
                     "Loaded ", format(nrow(data), big.mark = ","), " variants."
                   ),
                   type = "success")
      },
      error = function(e) {
        values$error_message <- e$message
        log_action("File Upload", paste("Error:", e$message))
        shinyalert(title = "Error", text = paste("Error reading file(s):", e$message), type = "error")
      })
    })
    w$hide()
  })

  # Run DisVar analysis
  observeEvent(input$runButton, {
    log_action("Analysis", "Started")
    req(values$vcf_data)
    w$show()

    withProgress(
      message = 'Analyzing variants...',
      value = 0,
      {
        tryCatch({
          # Create local progress object for consistency
          local_progress <- list(
            set = function(message, value) {
              setProgress(value = value, message = message)
            }
          )

          local_progress$set(message = "Initializing analysis...", value = 0.1)

          # Get selected databases and parameters
          selected_dbs <- input$databases
          pVal <- input$pValue

          if (length(selected_dbs) == 0) {
            stop("Please select at least one database for analysis")
          }

          # Run DisVar with progress updates
          local_progress$set(message = "Running DisVar analysis...", value = 0.3)

          aligned_df <- DisVar(
            values$vcf_data,
            GWASdb = "GWASdb" %in% selected_dbs,
            GRASP = "GRASP" %in% selected_dbs,
            GWASCat = "GWASCat" %in% selected_dbs,
            GAD = "GAD" %in% selected_dbs,
            JohnO = "JohnO" %in% selected_dbs,
            ClinVar = "ClinVar" %in% selected_dbs,
            p_value = pVal,
            merge_result = FALSE,
            runOnShiny = TRUE
          )

          if (is.null(aligned_df) || nrow(aligned_df) == 0) {
            stop("No variants found matching the selected criteria")
          }

          aligned_df <- aligned_df[aligned_df$Disease != ".", ]
          aligned_df$`Allele DB`[aligned_df$`Allele DB` == "NA>NA"] <- NA

          values$result_data <- aligned_df
          # Create interactive data table
          output$alignedPreview <- DT::renderDataTable({
            aligned_df$"Variant ID" <- sapply(aligned_df$"Variant ID", function(rsid) {
              paste0('<a href="https://www.ncbi.nlm.nih.gov/snp/', rsid, '" target="_blank">', rsid, '</a>')
            })

            DT::datatable(
              aligned_df,
              escape = FALSE,
              rownames = FALSE,
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                dom = 'lftip'
              ),
              class = 'cell-border stripe'
            )
          })


          local_progress$set(message = "Generating visualizations...", value = 0.7)

          # Create interactive plots
          # Variants per database
          output$variants_hits <- renderPlotly({
            plot_ly(
              data = values$result_data,
              x = ~DB,
              type = "histogram",
              color = ~DB
            ) %>%
              layout(
                title = "Variants per Database",
                xaxis = list(title = "Database"),
                yaxis = list(title = "Count (Hits)")
              )
          })

          # Top diseases or traits
          all_diseases <- values$result_data %>%
            filter(!is.na(Disease)) %>%
            mutate(
              Disease = gsub("_", " ", as.character(Disease)),
              Disease = tolower(Disease),
              Disease = gsub("'s\\b|s'\\b", "", Disease, ignore.case = TRUE),
              Disease = gsub("'$", "", Disease, ignore.case = TRUE),
              Disease = gsub("\\s*\\(.*?\\)", "", Disease),
              Disease = gsub("\\s+", " ", Disease),
              Disease = trimws(Disease),
              # Strip leading/trailing special characters (%, #, -, etc.)
              Disease = gsub("^[^a-z0-9]+|[^a-z0-9]+$", "", Disease),
              # Synonym normalisation
              Disease = case_when(
                grepl("\\bt2d\\b|type 2 diabet|type ii diabet|non.insulin.dependent diabet", Disease) ~ "type 2 diabetes",
                grepl("\\bt1d\\b|type 1 diabet|type i diabet|insulin.dependent diabet", Disease) ~ "type 1 diabetes",
                grepl("alzheimer", Disease) ~ "alzheimer's disease",
                grepl("\\bbmi\\b|body mass index|obesity|overweight", Disease) ~ "bmi / obesity",
                grepl("systolic blood pressure|diastolic blood pressure|hypertension|blood pressure", Disease) ~ "blood pressure / hypertension",
                grepl("ldl|hdl|triglycerid|cholesterol|lipid", Disease) ~ "lipid levels / cholesterol",
                grepl("coronary artery disease|coronary heart disease|\\bcad\\b|\\bchd\\b", Disease) ~ "coronary artery disease",
                grepl("schizophrenia", Disease) ~ "schizophrenia",
                grepl("parkinson", Disease) ~ "parkinson's disease",
                grepl("breast cancer", Disease) ~ "breast cancer",
                grepl("prostate cancer", Disease) ~ "prostate cancer",
                grepl("colorectal cancer|colon cancer", Disease) ~ "colorectal cancer",
                grepl("rheumatoid arthritis", Disease) ~ "rheumatoid arthritis",
                grepl("crohn|inflammatory bowel|ulcerative colitis", Disease) ~ "inflammatory bowel disease",
                grepl("multiple sclerosis", Disease) ~ "multiple sclerosis",
                grepl("atrial fibrillation", Disease) ~ "atrial fibrillation",
                grepl("stroke|cerebrovascular", Disease) ~ "stroke",
                TRUE ~ Disease
              )
            ) %>%
            filter(Disease != "", !is.na(Disease)) %>%
            group_by(Disease) %>%
            tally(sort = TRUE)

          top_diseases <- all_diseases %>%
            slice_head(n = 10)

          output$top_diseases <- renderPlotly({
            plot_ly(top_diseases,
                    x = ~n,
                    y = ~reorder(Disease, n),
                    type = "bar",
                    orientation = 'h',
                    color = ~Disease,
                    marker = list(
                      line = list(color = "black", width = 0.8),
                      opacity = 0.9
                    )) %>%
              layout(
                title = list(
                  text = "Top Diseases or Traits",
                  x = 0.5
                ),
                xaxis = list(title = "Count (Hits)"),
                yaxis = list(title = "Disease/Trait"),
                showlegend = FALSE
              )
          })

          output$downloadDiseasesData <- downloadHandler(
            filename = function() paste0("diseases_traits_", Sys.Date(), ".tsv"),
            content = function(file) {
              write.table(
                all_diseases %>% rename("Disease/Trait" = Disease, "Count" = n),
                file, sep = "\t", row.names = FALSE, quote = FALSE
              )
            }
          )

          output$downloadVariantsDB <- downloadHandler(
            filename = function() paste0("variants_per_database_", Sys.Date(), ".tsv"),
            content = function(file) {
              variants_db <- values$result_data %>%
                group_by(DB) %>%
                tally(sort = TRUE) %>%
                rename("Database" = DB, "Count" = n)
              write.table(variants_db, file, sep = "\t", row.names = FALSE, quote = FALSE)
            }
          )

          # Update result summary
          output$resultSummary <- renderUI({
            div(
              class = "summary-box",
              h4("Analysis Summary"),
              tags$ul(
                tags$li(sprintf("Total variants analysed: %s", format(nrow(values$vcf_data), big.mark = ","))),
                tags$li(sprintf("Variants with hits: %s", format(nrow(values$result_data), big.mark = ","))),
                tags$li(sprintf("Databases queried: %d", length(selected_dbs))),
                tags$li(sprintf("P-value threshold: %.2e", pVal))
              )
            )
          })

          # Enable download buttons
          shinyjs::enable("downloadData")
          shinyjs::enable("downloadExcel")

          log_action("Analysis", "Completed")
          local_progress$set(message = "Analysis complete!", value = 1)
        },
        error = function(e) {
          log_action("Analysis", paste("Error:", e$message))
          shinyalert(
            title = "Analysis Error",
            text = paste("Error during analysis:", e$message),
            type = "error"
          )
        })
      }
    )
    w$hide()
  })

  # Download handlers
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("DisVar_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    },
    content = function(file) {
      req(values$result_data)
      tryCatch({
        write.table(values$result_data, file, quote = FALSE, sep = "\t", row.names = FALSE)
        log_action("Download", "TSV file exported successfully")
      }, error = function(e) {
        log_action("Download", paste("Error exporting TSV:", e$message))
        stop(paste("Error exporting data:", e$message))
      })
    }
  )

  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("DisVar_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(values$result_data)
      tryCatch({
        write_xlsx(values$result_data, file)
        log_action("Download", "Excel file exported successfully")
      }, error = function(e) {
        log_action("Download", paste("Error exporting Excel:", e$message))
        stop(paste("Error exporting data:", e$message))
      })
    }
  )

  # Processing history table
  output$processingHistory <- renderDataTable({
    DT::datatable(
      values$processing_history,
      options = list(
        pageLength = 5,
        order = list(list(0, 'desc')),
        dom = 'tp'
      )
    )
  })

  # System information
  output$systemInfo <- renderUI({
    div(
      class = "system-info",
      h4("System Information"),
      tags$ul(
        tags$li(sprintf("R Version: %s", R.version.string)),
        tags$li(sprintf("Available Memory: %.2f GB", as.numeric(system("free -m | grep Mem | awk '{print $7}'", intern = TRUE)) / 1024)),
        tags$li(sprintf("CPU Cores: %d", parallel::detectCores())),
        tags$li(sprintf("Last Update: %s", format(values$last_update, "%Y-%m-%d %H:%M:%S")))
      )
    )
  })
}
