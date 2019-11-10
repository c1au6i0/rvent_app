#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)

ui <- fluidPage( # Application title
    mainPanel(
        shinyDirButton("dir", "Input directory", "Upload"),
        verbatimTextOutput("dir", placeholder = TRUE)  
    ))

server <- function(input, output) {
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    global <- reactiveValues(datapath = getwd())
    
    dir <- reactive(input$dir)
    
    output$dir <- renderText({
        global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                     
                     x <- tryCatch(
                         get_iox(svDialogs::dlg_dir()$res, inter = FALSE),
                         error = function(c) conditionMessage(c)
                         )
                     
                     
                     }
                 )
}

# Run the application
shinyApp(ui = ui, server = server)
