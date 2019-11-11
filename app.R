library(shiny)
library(rvent)
library(shinyFiles)
library(DT)
# library(shinyWidgets)

ui <- fluidPage( # Ciccio Ciccio
    fluidRow(
        column(4,
               shinyDirButton("dir", "Input directory", "Upload"),
               verbatimTextOutput("import_msg", placeholder = TRUE)
        )
    ),
    fluidRow(
        column(4,
               wellPanel(
                    selectInput("com_sub", 
                           label = "",
                           choices = "",
                           multiple = TRUE),
                      actionButton("OK_com", label = "OK")
                    ),
                    conditionalPanel(
                        condition = "output.tsd_s != 0",
                        DTOutput('tsd_s')
                    )
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
    
    vent <- reactiveVal()
      
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
                         
                         vent(all_data$vent)
                         c_comments$tsd_s <- choose_comments
                         c_comments$lab <- "Click the menu to select drug injections and then press OK"
                     } else {
                         output$import_msg <- renderText(all_data)
                     }
                     
                  }
    )
      # select comments with subject and drug
      observe({ 
                updateSelectInput(session, "com_sub", label = c_comments$lab, choices = c_comments$tsd_s$subj_drug_dose_unit)
      }
    )
      
      # filter comments
      observeEvent(ignoreNULL = TRUE,
                   eventExpr = {
                     input$OK_com
                   },
                   handlerExpr = {
                     ## ADD IF NULL
                     tsd_s <- c_comments$tsd_s[c_comments$tsd_s$subj_drug_dose_unit %in% input$com_sub, ]
                     tsd_s <- tidyr::separate(tsd_s, 
                                              .data$subj_drug_dose_unit, 
                                              c("subj", "drug", "dose", "unit"), 
                                              fill = "right", extra = "merge")
                     tsd_s[tsd_s == "NA"] <- NA
                     
                     if (sum(is.na(tsd_s)) > 0) {
                       output$tsd_s <-  renderDT(tsd_s, selection = 'none', server = F, editable = T)
                     }
                   }
      )
}
# https://shiny.rstudio.com/reference/shiny/1.0.0/insertUI.html
# https://stackoverflow.com/questions/41710455/shiny-conditionalpanel-set-condition-as-output-from-server

# Run the application
shinyApp(ui = ui, server = server)
