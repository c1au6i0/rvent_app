library(shiny)
library(rvent)
library(shinyFiles)
library(DT)
library(shinyWidgets)

ui <- fluidPage( # 
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
# https://stackoverflow.com/questions/36709441/how-to-display-widgets-inline-in-shiny
# https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193
# https://stackoverflow.com/questions/44112000/move-r-shiny-shownotification-to-center-of-screen
  
# https://shiny.rstudio.com/articles/notifications.html
  
  tags$head(tags$style(
    HTML('
    #display_msg {
        background: white;
        }
    #summarized_dat {
        font-size: 12px
    } 
    
         
         ')
    )),
      
  sidebarLayout(
        sidebarPanel(width = 6,
                    verbatimTextOutput("display_msg", placeholder = TRUE),
      
                          shinyDirButton("dir", "Input directory", "Upload"),
                          
                          conditionalPanel(
                              condition = "output.hideokb",
                              selectInput("com_sub", 
                                        label = "",
                                        choices = "",
                                        multiple = TRUE)
                          ),
                          conditionalPanel(
                              condition = "output.hideokb",
                              actionButton("OK_com", label = "OK")
                          ),
                       br(),
                       br(),
                       br(),
                       br(),
                    
                          conditionalPanel(
                              condition = "output.hidedt == false",
                              DTOutput('tsd_sf')
                          ) ,
                        br(),
                        br(),
                        br(),
                        br(),
                          conditionalPanel(
                                condition = "output.hidestat",
                                checkboxGroupButtons(
                                  inputId = "vent_stat",
                                  label = "Stats",
                                  choices = c("mean",
                                            "median",
                                            "n",
                                            "sd"))
                          ),
                          conditionalPanel(
                              condition = "output.hidestat",
                              numericInput("bin", 
                                         label = "bin (min)", value = 1, min = 1, width = '100px'),
                          ),
                         conditionalPanel(
                              condition = "output.hidestat",
                              numericInput("baseline", 
                                         label = "baseline (min)", value = 30, min = 1, width = '100px'),
                          ),
                        conditionalPanel(
                              condition = "output.hidestat",
                              actionButton("summarize", label = "make summary")
                    )
                          
                    
        ),
    mainPanel( width = 6,
      DTOutput('summarized_dat'),
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
    
    output$display_msg <- renderText("Choose a foder that contains iox files, Renata!") 
    
    # reactive val-----------------------
    dpath <- reactiveVal()
    dir <- reactive(input$dir)
    c_comments <- reactiveValues()
    
    # p_hide$okb, p_hide$DT, p_hide$stat
    p_hide <- reactiveValues()
    vent <- reactiveVal()
    
    # ok and choose comments
    output$hideokb <- reactive({
      p_hide$okb
    })
    
    # DT-table
    output$hidedt <- reactive({
        is.na(p_hide$DT)
    })
    
    # get the stats and baseline bin
    output$hidestat <- reactive({
      p_hide$stat
    })

    summarized_dat <- reactiveVal()
  
    outputOptions(output, "hidestat", suspendWhenHidden = FALSE)
    outputOptions(output, "hidedt", suspendWhenHidden = FALSE)
    outputOptions(output, "hideokb", suspendWhenHidden = FALSE)
    
    
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
                     
                     dpath(datapath)
                     
                     all_data <- tryCatch(
                         get_iox(iox_folder = datapath, inter = FALSE, baseline = 30),
                         error = function(c) conditionMessage(c)
                         )
                     if (is.list(all_data)) {
                         output$display_msg <- renderText("Click on the menu and indentify drug injections!")
                         p_hide$okb <- 1
                         choose_comments <- tidyr::unite(all_data$tsd_s, col = "subj_drug_dose_unit", 
                                                         .data$subj, .data$drug, .data$dose, .data$unit, sep = " ")
                         
                         vent(all_data$vent)
                         c_comments$tsd_s <- choose_comments
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
                          p_hide$DT <- NA
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
                         p_hide$DT <- 1
                         } else {
                         p_hide$DT <- NA
                         p_hide$stat <- 1
                         }
                     }
                   }
      )
  
      # edit table with comments
      observeEvent(input$tsd_sf_cell_edit,
                        handlerExpr = {
                          c_comments$tsd_sf[input$tsd_sf_cell_edit$row,input$tsd_sf_cell_edit$col] <<- input$tsd_sf_cell_edit$value
                          if(sum(is.na(c_comments$tsd_sf)) == 0){
                            p_hide$stat <- 1
                          }
                         }

       )
      
      # table with comments edited: slide and checkbox
      observeEvent(
        eventExpr = {
          input$summarize
        },
        handlerExpr = {
          if(is.null(input$vent_stat)) {
            output$display_msg <- renderText("Choose a stat")
          } else {
            vent_jn <- normalizetime_vent(dat = vent(),
                                          tsd_s = c_comments$tsd_sf, 
                                          tofill = NULL,
                                          baseline = input$baseline)

            vent_all <- summarize_vent(vent_jn, inter = FALSE, baseline = input$baseline, bin = input$bin, form = input$vent_stat)
            
            summarized_dat(vent_all)

            file_name <- paste0("summary_", as.character(vent_all$dat_vent$cpu_date[1]), ".xlsx")
            file_path <- paste(dpath(), file_name, sep = .Platform$file.sep)
            writexl::write_xlsx(vent_all$dat_fs, file_path)
            output$summarized_dat <-  renderDT(vent_all$dat_sml, 
                                               selection = 'none', 
                                               server = F, 
                                               editable = F,
                                               fillContainer = FALSE,
                                               options = list(
                                                   pageLength = 17,
                                                   autoWidth = TRUE
                                               )
            )
          }
        }

      )

}   

# Run the application
shinyApp(ui = ui, server = server)
