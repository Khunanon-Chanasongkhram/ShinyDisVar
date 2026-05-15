# Program: ShinyDisVar
# Developer: Khunanon Chanasongkhram

library(shiny)
library(shinydashboard) # Create the dashboard layout
library(shinyjs) # Show/hide elements
library(waiter) # Show/hide loading spinner
library(shinyalert) # Show alert messages
library(DT) # Render data tables
library(plotly) # Render interactive plots
library(shinycssloaders) # Show/hide loading spinner during rendering
library(wesanderson)

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
      tags$script(HTML("
        document.title = 'ShinyDisVar';
      ")),
      tags$link(rel = "icon",
                type = "image/x-icon",
                href = "favicon.ico"),

      # SEO meta tags
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "description",
                content = "ShinyDisVar is a web application for querying genetic variants and their associations with diseases and traits. Upload a VCF file to analyze variants across multiple GWAS and clinical databases including GWASdb, GRASP, GWAS Catalog, ClinVar, and more."),
      tags$meta(name = "keywords",
                content = "ShinyDisVar, genetic variants, VCF analysis, GWAS, disease associations, bioinformatics, DisVar, SNP, ClinVar, GWAS Catalog, variant annotation, genomics"),
      tags$meta(name = "author", content = "Khunanon Chanasongkhram"),
      tags$meta(name = "robots", content = "index, follow"),
      tags$link(rel = "canonical", href = "https://bioinformatics-shinydisvar.shinyapps.io/ShinyDisVar/"),

      # Open Graph (Facebook, LinkedIn previews)
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = "https://bioinformatics-shinydisvar.shinyapps.io/ShinyDisVar/"),
      tags$meta(property = "og:title", content = "ShinyDisVar — Genetic Variant Disease Association Tool"),
      tags$meta(property = "og:description",
                content = "Upload a VCF file to query genetic variants against GWASdb, GRASP, GWAS Catalog, ClinVar, and more. Visualize top disease associations and download results."),
      tags$meta(property = "og:image", content = "https://bioinformatics-shinydisvar.shinyapps.io/ShinyDisVar/ShinyDisVar_logo.gif"),

      # Twitter Card
      tags$meta(name = "twitter:card", content = "summary"),
      tags$meta(name = "twitter:title", content = "ShinyDisVar — Genetic Variant Disease Association Tool"),
      tags$meta(name = "twitter:description",
                content = "Upload a VCF file to query genetic variants against GWASdb, GRASP, GWAS Catalog, ClinVar, and more. Visualize top disease associations and download results."),

      # Structured data (JSON-LD) for Google
      tags$script(type = "application/ld+json", HTML('
        {
          "@context": "https://schema.org",
          "@type": "WebApplication",
          "name": "ShinyDisVar",
          "url": "https://bioinformatics-shinydisvar.shinyapps.io/ShinyDisVar/",
          "description": "A Shiny-based web application for querying genetic variants and their associations with diseases and traits. Integrates with the DisVar R package and provides a user-friendly interface for analyzing VCF files.",
          "applicationCategory": "Bioinformatics",
          "operatingSystem": "Web",
          "author": {
            "@type": "Person",
            "name": "Khunanon Chanasongkhram"
          },
          "keywords": "genetic variants, VCF, GWAS, disease associations, bioinformatics, SNP, ClinVar, genomics"
        }
      ')),
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; } # Change the background color
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
        #runButton:disabled, #downloadData:disabled, #downloadExcel:disabled {
          opacity: 0.45;
          cursor: not-allowed;
          pointer-events: all !important;
        }
        .btn-download {
          display: block;
          width: 100%;
          padding: 10px 16px;
          font-size: 15px;
          font-weight: 600;
          border-radius: 5px;
          text-align: center;
          margin-top: 8px;
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

            fileInput("vcfFile", "Upload VCF File(s)",
                      accept = c(".vcf", ".vcf.gz"),
                      multiple = TRUE),

            uiOutput("uploadedFilesList"),

            checkboxGroupInput("databases",
                               "Select Databases",
                               choices = list(
                                 "GWASdb" = "GWASdb",
                                 "GRASP" = "GRASP",
                                 "GWAS Catalog" = "GWASCat",
                                 "GADCDC" = "GAD",
                                 "Johnson and O'Donnell" = "JohnO",
                                 "ClinVar" = "ClinVar"
                               ),
                               selected = c("GWASdb", "GRASP", "GWASCat", "GAD", "JohnO", "ClinVar")),

            numericInput("pValue",
                         "P-value Threshold",
                         value = 5e-8,
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
                    h4("Top Diseases/Traits"),
                    withSpinner(plotlyOutput("top_diseases")),
                    downloadButton("downloadDiseasesData", "Download Diseases/Traits TSV", class = "btn-primary btn-download")
                )
              ),
              column(
                width = 6,
                div(class = "plot-container",
                    h4("Variants per Database"),
                    withSpinner(plotlyOutput("variants_hits")),
                    downloadButton("downloadVariantsDB", "Download Variants per Database TSV", class = "btn-primary btn-download")
                )
              )
            ),

            fluidRow(
              column(
                width = 12,
                h4("Results Preview"),
                withSpinner(DTOutput("alignedPreview")),
                br(),
                fluidRow(
                  column(
                    width = 6,
                    downloadButton("downloadData", "Download Results TSV",
                                   class = "btn-primary btn-download")
                  ),
                  column(
                    width = 6,
                    downloadButton("downloadExcel", "Download Results Excel",
                                   class = "btn-primary btn-download")
                  )
                )
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
              strong("ShinyDisVar Version:"), "1.1.0",
              tags$br(),
              strong("Last Updated:"), "2026-05-15",
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
            p(tags$a(href = "https://github.com/Khunanon-Chanasongkhram/ShinyDisVar",
                     target = "_blank",
                     "GitHub: https://github.com/Khunanon-Chanasongkhram/ShinyDisVar"))
          )
        )
      )
    )
  )
)
