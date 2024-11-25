# Program: ShinyDisVar
# Developer: Khunanon Chanasongkhram

library(shiny)
library(shinydashboard)
library(shinyjs)
library(waiter)
library(shinyalert)
library(DT)
library(plotly)
library(shinycssloaders)

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = div(
      img(src = "ShinyDisVar_logo.gif", height = "50px")
    )
  ),

  dashboardSidebar(
    # Sidebar Menu Items
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("dna")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("History", tabName = "history", icon = icon("history")),
      menuItem("Tutorial", tabName = "tutorial", icon = icon("book")),
      menuItem("About", icon = icon("info-circle"),
               menuSubItem("About ShinyDisVar", tabName = "about_shinydisvar"),
               menuSubItem("Contact", tabName = "about_Contact")
      )
    )
  ),

  dashboardBody(
    # Use useWaiter() to show/hide loading spinner
    useWaiter(),
    # Use shinyjs to show/hide elements
    useShinyjs(),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #0073e6; }
        .summary-box {
          padding: 15px;
          background: #fff;
          border-radius: 5px;
          margin-bottom: 20px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .system-info {
          padding: 15px;
          background: #fff;
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .custom-file-input { margin-bottom: 15px; }
        .progress { margin-top: 10px; }
        .stats-box {
          padding: 20px;
          border-radius: 5px;
          background: #fff;
          margin-bottom: 20px;
          text-align: center;
        }
        .stats-box h3 {
          margin-top: 0;
          color: #0073e6;
        }
        .plot-container {
          background: #fff;
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 20px;
        }
        .download-section {
          margin-top: 20px;
          padding: 15px;
          background: #fff;
          border-radius: 5px;
        }
      "))
    ),

    tabItems(
      # Analysis Tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            width = 4,
            title = "Input Parameters",
            status = "primary",
            solidHeader = TRUE,

            fileInput("vcfFile", "Upload VCF File",
                      accept = c(".vcf", ".vcf.gz"),
                      multiple = FALSE),

            checkboxGroupInput("databases",
                               "Select Databases",
                               choices = list(
                                 "GWASdb" = "GWASdb",
                                 "GRASP" = "GRASP",
                                 "GWAS Catalog" = "GWASCat",
                                 "GADCDC" = "GADC",
                                 "Johnson and O'Donnell" = "JohnO",
                                 "ClinVar" = "ClinVar"
                               ),
                               selected = c("GWASdb", "GRASP", "GWASCat", "GADC", "JohnO", "ClinVar")),

            numericInput("pValue",
                         "P-value Threshold",
                         value = 1e-7,
                         min = 0,
                         max = 1,
                         step = 1e-1,
                         width = '100%'
                         ),

            actionButton("runButton",
                         "Run Analysis",
                         icon = icon("play"),
                         class = "btn-primary btn-block")
          ),

          box(
            width = 8,
            title = "File Preview: Head & Tail",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(DTOutput("vcfPreviewHeadTail"))
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "System Status",
            status = "info",
            solidHeader = TRUE,
            uiOutput("systemInfo")
          )
        )
      ),

      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
            title = "Analysis Results",
            status = "primary",
            solidHeader = TRUE,

            fluidRow(
              column(
                width = 12,
                uiOutput("resultSummary")
              )
            ),

            fluidRow(
              column(
                width = 6,
                div(class = "plot-container",
                    h4("Variants per Database"),
                    withSpinner(plotlyOutput("variants_hits"))
                )
              ),
              column(
                width = 6,
                div(class = "plot-container",
                    h4("Top Diseases/Traits"),
                    withSpinner(plotlyOutput("top_diseases"))
                )
              )
            ),

            fluidRow(
              column(
                width = 12,
                div(class = "download-section",
                    h4("Download Results"),
                    fluidRow(
                      column(
                        width = 6,
                        downloadButton("downloadData", "Download TSV",
                                       class = "btn-primary btn-block")
                      ),
                      column(
                        width = 6,
                        downloadButton("downloadExcel", "Download Excel",
                                       class = "btn-primary btn-block")
                      )
                    )
                )
              )
            ),

            fluidRow(
              column(
                width = 12,
                h4("Results Preview"),
                withSpinner(DTOutput("alignedPreview"))
              )
            )
          )
        )
      ),

      # History Tab
      tabItem(
        tabName = "history",
        fluidRow(
          box(
            width = 12,
            title = "Processing History",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("processingHistory")
          )
        )
      ),

      # Tutorial Tab
      tabItem(
        tabName = "tutorial",
        fluidRow(
          box(
            width = 12,
            title = "Tutorial",
            status = "primary",
            solidHeader = TRUE,

            tabsetPanel(
              tabPanel("Getting Started",
                       h4("Quick Start Guide"),
                       tags$ol(
                         tags$li(
                           strong("Upload VCF File"),
                           p("Click", tags$code("Browse"), "to upload your", tags$code(".vcf"), "or",
                             tags$code(".vcf.gz"), "file."),
                           tags$em(tags$b("Note:"), "Compression (.vcf.gz) is recommended for the large VCF file.")
                         ),
                         tags$br(),
                         tags$li(
                           strong("Select Databases"),
                           p("Choose at least one database for variant analysis.")
                         ),
                         tags$li(
                           strong("Set P-value"),
                           p("Enter your desired significance threshold (e.g., 1e-8).")
                         ),
                         tags$li(
                           strong("Run Analysis"),
                           p("Click the", tags$code("Run Analysis"), "button to start processing.")
                         )
                       ),

                       h4("Sample Data"),
                       downloadButton("downloadTestFile",
                                      "Download Test VCF File",
                                      class = "btn-primary")
              ),

              tabPanel("File Format",
                       h4("VCF File Requirements"),
                       tags$ul(
                         tags$li("Standard VCF format (v4.0 or later)"),
                         tags$li("Must contain standard headers"),
                         tags$li("Can be compressed (.gz) or uncompressed"),
                         tags$li("Maximum file size: 9GB")
                       )
              ),

              tabPanel("Analysis Options",
                       h4("Available Databases"),
                       tags$ul(
                         tags$li(strong("GWASdb:"), "Comprehensive collection of genetic variants"),
                         tags$li(strong("GRASP:"), "Genome-Wide Repository of Associations"),
                         tags$li(strong("GWAS Catalog:"), "Curated resource of SNP-trait associations"),
                         tags$li(strong("GADCDC:"), "Genetic Association Database"),
                         tags$li(strong("Johnson and O'Donnell:"), "A specialized resource for exploring genetic and environmental factors influencing complex traits."),
                         tags$li(strong("ClinVar:"), "Relationships between variation and human health")
                       )
              )
            )
          )
        )
      ),

      # About Tab
      tabItem(
        tabName = "about_shinydisvar",
        fluidRow(
          box(
            width = 12,
            title = "About ShinyDisVar",
            status = "primary",
            solidHeader = TRUE,
            tags$p(
              strong("ShinyDisVar Version:"), "alpha 0.3.0",
              tags$br(),
              strong("Last Updated:"), "2024-03-20",
              tags$br(),
              strong("Developer:"), "Khunanon Chanasongkhram",
              tags$br(),
              tags$br(),
              "ShinyDisVar is a Shiny-based web application for querying genetic variants and their associations with diseases and traits.",
              "The application integrates with DisVar R package and provides a user-friendly interface for analyzing VCF files.",
              tags$br(),
              "The web application was developed as part of the Master's degree thesis in the Molecular Biotechnology and Bioinformatics program, Division of Biological Science, Faculty of Science, Prince of Songkla University, Songkhla, Thailand."
            )
          )
        )
      ),
      tabItem(
        tabName = "about_Contact",
        fluidRow(
          box(
            width = 12,
            title = "Contact Information",
            status = "primary",
            solidHeader = TRUE,
            p("For any inquiries or feedback, please contact the developer at:"),
            p(tags$a(href = "mailto:Chanasongkhram.k@gmail.com", "Email: Chanasongkhram.k@gmail.com")),
            p(tags$a(href = "https://github.com/Khunanon-Chanasongkhram/DisVar",
                     target = "_blank",
                     "GitHub: https://github.com/Khunanon-Chanasongkhram/DisVar"))
          )
        )
      )
    )
  )
)
