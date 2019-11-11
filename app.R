#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvent)
library(shinyFiles)
# library(shinyWidgets)

ui <- fluidPage( # Application title
    fluidRow(
        column(3,
               shinyDirButton("dir", "Input directory", "Upload"),
        verbatimTextOutput("import_msg", placeholder = TRUE)
        )
    ),
    fluidRow(
        column(3, 
               selectInput("com_sub", 
                           label = "",
                           choices = "",
                           multiple = TRUE)
               
        )
    )
)
 

server <- function(input, output, session) {
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    dir <- reactive(input$dir)
    
    c_comments <- reactiveValues()
      
    # get the iox files
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~")
                     datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                     
                     all_data <- tryCatch(
                         get_iox(iox_folder = datapath, inter = FALSE, baseline = 30),
                         error = function(c) conditionMessage(c)
                         )
                     if (is.list(all_data)) {
                         output$import_msg <- renderText("Files imported!")
                         choose_comments <- tidyr::unite(all_data$tsd_s, col = "subj_drug_dose_unit", 
                                                         .data$subj, .data$drug, .data$dose, .data$unit, sep = " ")
                         c_comments$tsd_s <- choose_comments$subj_drug_dose_unit
                         c_comments$lab <- "Click the menu to select drug injections"
                     } else {
                         output$import_msg <- renderText(all_data)
                     }
                     
                  }
    )
# https://shiny.rstudio.com/reference/shiny/1.2.0/updateSelectInput.html
      observe({ 
                updateSelectInput(session, "com_sub", label = c_comments$lab, choices = c_comments$tsd_s)
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
