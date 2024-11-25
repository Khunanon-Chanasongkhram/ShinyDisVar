# Program: ShinyDisVar
# Developer: Khunanon Chanasongkhram

library(DisVar)
library(shiny)
library(sqldf)
library(data.table)
library(dplyr)
library(shinyjs)
library(tools)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(waiter)
library(shinyjs)
library(shinyalert)
library(writexl)

options(shiny.maxRequestSize = 9000*1024^2)

server <- function(input, output, session) {
  # Initialize waiter loading screen
  w <- Waiter$new(
    html = spin_flower(),
    color = transparent(.7)
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
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
      if (is.null(file)) {
        return(list(valid = FALSE, message = "No file selected"))
      }

      ext <- tools::file_ext(file$name)
      # Check if it's a .vcf.gz file
      if (grepl("\\.vcf\\.gz$", file$name)) {
        ext <- "vcf.gz"
      }

      if (!ext %in% c("vcf", "vcf.gz", "gz")) {
        return(list(valid = FALSE, message = "Invalid file type. Please upload a .vcf or .vcf.gz file."))
      }

      # Check file size
      if (file$size > 9000*1024^2) {
        return(list(valid = FALSE, message = "File too large. Maximum size is 9GB."))
      }

      # Basic header check
      first_line <- if(ext %in% c("gz", "vcf.gz")) {
        system(sprintf("zcat '%s' | head -n 1", file$datapath), intern = TRUE)
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

  # File preview handler
  output$vcfPreviewHeadTail <- renderDT({
    req(values$vcf_data)
    preview_data <- rbind(
      head(values$vcf_data, 10),
      tail(values$vcf_data, 10)
    )
    datatable(preview_data,
              options = list(
                dom = 't',
                scrollX = TRUE,
                pageLength = 20
              ))
  })

  # Enhanced file upload handler with progress updates
  observeEvent(input$vcfFile, {
    w$show()
    validation <- validate_vcf(input$vcfFile)

    if (!validation$valid) {
      shinyalert(
        title = "Validation Error",
        text = validation$message,
        type = "error"
      )
      w$hide()
      return()
    }

    log_action("File Upload", "Started")

    withProgress(
      message = 'Processing VCF file...',
      value = 0,
      {
        tryCatch({
          is_gz <- grepl("\\.gz$", input$vcfFile$name)
          # Create a new progress environment
          local_progress <- list(
            set = function(message, value) {
              setProgress(value = value, message = message)
            }
          )

          data <- read_vcf_with_progress(input$vcfFile$datapath, is_gz, progress_obj = local_progress)
          values$vcf_data <- data

          # Update processing status
          values$processing_status <- "File loaded successfully"
          log_action("File Upload", "Completed")

          # Show success message
          shinyalert("Success",
                     paste("Loaded", format(nrow(data), big.mark = ","), "variants"),
                     type = "success")
        },
        error = function(e) {
          values$error_message <- e$message
          log_action("File Upload", paste("Error:", e$message))
          shinyalert(
            title = "Error",
            text = paste("Error reading file:", e$message),
            type = "error"
          )
        })
      }
    )
    w$hide()
  })

  # Run DisVar analysis
  observeEvent(input$runButton, {
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
            GAD = "GADCDC" %in% selected_dbs,
            JohnO = "JohnO" %in% selected_dbs,
            ClinVar = "ClinVar" %in% selected_dbs,
            p_value = pVal,
            runOnShiny = TRUE
          )

          if (is.null(aligned_df) || nrow(aligned_df) == 0) {
            stop("No variants found matching the selected criteria")
          }

          values$result_data <- aligned_df
          # Create interactive data table
          output$alignedPreview <- DT::renderDataTable({
            aligned_df$"Variant ID" <- sapply(aligned_df$"Variant ID", function(rsid) {
              paste0('<a href="https://www.ncbi.nlm.nih.gov/snp/', rsid, '" target="_blank">', rsid, '</a>')
            })

            DT::datatable(
              aligned_df,
              escape = FALSE,
              options = list(
                scrollX = TRUE,
                pageLength = 10
              ),
              class = 'cell-border stripe'
            )
          })

          local_progress$set(message = "Generating visualizations...", value = 0.7)

          # Create interactive plots
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

          top_diseases <- values$result_data %>%
            group_by(Disease) %>%
            tally(sort = TRUE) %>%
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
                  text = "Top 10 Diseases or Traits",
                  x = 0.5
                ),
                xaxis = list(title = "Count (Hits)"),
                yaxis = list(title = "Disease/Trait"),
                showlegend = FALSE
              )
          })

          # Update result summary
          output$resultSummary <- renderUI({
            div(
              class = "summary-box",
              h4("Analysis Summary"),
              tags$ul(
                tags$li(sprintf("Total variants analyzed: %d", nrow(values$vcf_data))),
                tags$li(sprintf("Variants with hits: %d", nrow(values$result_data))),
                tags$li(sprintf("Databases queried: %d", length(selected_dbs))),
                tags$li(sprintf("P-value threshold: %.2e", pVal))
              )
            )
          })

          # Enable download buttons
          shinyjs::show("downloadData")
          shinyjs::show("downloadExcel")

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
