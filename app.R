library(shiny)
library(rvent)
library(shinyFiles)
library(DT)
# library(shinyWidgets)

ui <- fluidPage( # Ciccio CicciofluidPage(
  # tags$head(tags$style(
  #   HTML('
  #        #sidebar {
  #           background-color: steelblue;
  #       }
  # 
  #       body, label, input, button, select { 
  #         font-family: "Arial";
  #       }')
  # )),
    
  sidebarPanel(width = 5,
              verbatimTextOutput("display_msg", placeholder = TRUE),

               wellPanel(
                    shinyDirButton("dir", "Input directory", "Upload"),
                    selectInput("com_sub", 
                           label = "",
                           choices = "",
                           multiple = TRUE),
                      actionButton("OK_com", label = "OK")
                    ),
                    conditionalPanel(
                        condition = "output.hide == false",
                        DTOutput('tsd_sf')
                    )
              )
)

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c("txt", "tsv", "csv")
    )
    dir <- reactive(input$dir)
    c_comments <- reactiveValues()
    vent <- reactiveVal()

    output$hide <- reactive({
        is.na(c_comments$hide)
    })

    output$display_msg <- renderText("Choose a foder, Francesca!") 
      
    outputOptions(output, "hide", suspendWhenHidden = FALSE)

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
                         output$display_msg <- renderText("Files imported! Now indentify drug injections!")
                         choose_comments <- tidyr::unite(all_data$tsd_s, col = "subj_drug_dose_unit", 
                                                         .data$subj, .data$drug, .data$dose, .data$unit, sep = " ")
                         
                         vent(all_data$vent)
                         c_comments$tsd_s <- choose_comments
                         # c_comments$lab <- "Click the menu to select drug injections and then press OK"
                     } else {
                         output$display_msg <- renderText(all_data)
                     }
                     
                  }
    )
      # select comments with subject and drug
      observe({ 
                updateSelectInput(session, "com_sub", label = c_comments$lab, choices = c_comments$tsd_s$subj_drug_dose_unit)
      }
    )
      
      #  input$com_sub is use to filter tsd_s
      # filter comments: are there NA or not?
      observeEvent(ignoreNULL = TRUE,
                   eventExpr = {
                     input$OK_com
                   },
                   handlerExpr = {
                    if(is.null(input$com_sub)) {
                          output$display_msg <- renderText("Please select something!")
                          c_comments$hide <- NA
                    } else {
                         tsd_sf <- c_comments$tsd_s[c_comments$tsd_s$subj_drug_dose_unit %in% input$com_sub, ]
                         tsd_sf <- tidyr::separate(tsd_sf,
                                                  subj_drug_dose_unit,
                                                  c("subj", "drug", "dose", "unit"),
                                                  fill = "right", extra = "merge")
                         tsd_sf[tsd_sf == "NA"] <- NA
                         c_comments$tsd_sf <- tsd_sf
                         
                         if (sum(is.na(c_comments$tsd_sf)) > 0) {
                         output$tsd_sf <-  renderDT(tsd_sf, selection = 'none', server = F, editable = T)
                         output$display_msg <- renderText("Please, fill up missing values!")
                         c_comments$hide <- 1
                         } else {
                         c_comments$hide <- NA
                         }
                     }
                   }
      )
  
      # https://github.com/rstudio/DT/pull/480
      # observeEvent(input$tsd_sf_cell_edit,
      #              handlerExpr = {
      # 
      #                    }
      # 
      #              
      # 
      # )

}   

# Run the application
shinyApp(ui = ui, server = server)
