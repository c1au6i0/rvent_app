library(shiny)
library(rvent)
library(shinyFiles)
library(DT)
library(shinyWidgets)



# UI-------------
ui <- fluidPage(  

  
  tags$head(tags$style(
    HTML('

    #OK_com{
        border-color: green
    }
    
    #summarized_dat {
        font-size: 12px;
    } 
    
    #summarize {
        font-weight: bold;
        background: LightGreen;
        border-color: green;
    }
    
    #bin_d {
        text-align: center;
    }
    
    .shiny-notification {
             position:fixed;
             top: calc(0%);
             left: calc(50%);
             width: 400px;
             }
         
         ')
    )),
  useSweetAlert(),
  tabsetPanel(
    # SUMMARY STATS--------------------------------------------------------------------
    tabPanel("summary stats",

            sidebarLayout(
            # side panel------------
                  sidebarPanel(width = 6,
                               
                               
                  wellPanel(
                    div(style="display: inline-block;vertical-align:top;",
                        switchInput("tutorial", label = "Tutorial", value = TRUE)),
                    div(style="display: inline-block;vertical-align:top; float:right;",
                          actionButton("license", label = "LICENSE", width = "200px"))
                    ),
                            
                  div(style="display: inline-block;vertical-align:top;",
                                    shinyDirButton("dir", "Input directory", "Upload")),
                                    
                  div(style="display: inline-block;vertical-align:top;",
                                    actionButton("demo", label = "Demo Data")),
                  
                  div(style="display: inline-block;vertical-align:top;",
                                      conditionalPanel(
                                          condition = "output.hideokb",
                                          actionButton("OK_com", label = "OK")
                                      )),
                              
                                    conditionalPanel(
                                        condition = "output.hideokb",
                                        selectInput("com_sub", 
                                                  label = "",
                                                  choices = "",
                                                  multiple = TRUE)
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
                div(style="display: inline-block;vertical-align:top;", 
                                      conditionalPanel(
                                            condition = "output.hidestat",
                                            checkboxGroupButtons(
                                              inputId = "vent_stat",
                                              label = "stats",
                                              choices = c("mean",
                                                      "median",
                                                      "n",
                                                      "sd"))
                                    )),
                div(style="display: inline-block;vertical-align:top;", 
                                      conditionalPanel(
                                          condition = "output.hidestat",
                                          numericInput("bin", 
                                                   label = "bin (min)", value = 1, min = 1, width = '100px'),
                                    )),
               div(style="display: inline-block;vertical-align:top;", 
                                    conditionalPanel(
                                        condition = "output.hidestat",
                                        numericInput("baseline", 
                                                   label = "baseline (min)", value = 30, min = 1, width = '100px'),
                                    )),
                                  conditionalPanel(
                                        condition = "output.hidestat",
                                        actionButton("summarize", label = "visualize and save summary", width = "100%")
                              )
               ),
            # main panel------------
              mainPanel( width = 6,
                DTOutput('summarized_dat')
              )
          )
    ),
    
    # PLOTS--------------------------------------------------------------------
    tabPanel("plots",
          sidebarLayout(
               # side panel-----------------
               sidebarPanel(width = 4,
                    awesomeRadio(
                              inputId = "stat_plot",
                              label = h3("Stat to perform"),
                              choices = c("mean",
                                          "median"
                                          ),
                              inline = TRUE
                              ),
                    br(), 
                    br(),
                    br(),
                    br(),
                    sliderInput("bin_plot", label = h3("Slider: bin duration (min)"), min = 0, 
                           max = 30, value = 1)
                    
              ),
              # main panel-----------------
               mainPanel(width = 8
              )
             
         )
    )
  )
)

# https://shiny.rstudio.com/reference/shiny/0.14/renderUI.html
# https://gist.github.com/wch/5436415/


# SERVER-----------------------------------------------------------------------------------------------
server <- function(input, output, session) {
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c("txt", "tsv", "csv")
    )
    
  

  showNotification("Click on input button to select iox folder or use DemoData!",
                     action = "You can turn off these notifications with the Tutorial-switch button", 
                     duration = 300)

  
  
  
  
    
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
    p_hide$stat <- 1
    output$hidestat <- reactive({
      is.na(p_hide$stat)
    })

    summarized_dat <- reactiveVal()
  
    outputOptions(output, "hidestat", suspendWhenHidden = FALSE)
    outputOptions(output, "hidedt", suspendWhenHidden = FALSE)
    outputOptions(output, "hideokb", suspendWhenHidden = FALSE)
    
    
      # license --------
      observeEvent(ignoreNULL = TRUE,
                   eventExpr = {
                     input$license
                   },
                   handlerExpr = {
                     sendSweetAlert(session = session,
                                   title = "MIT License",
                                   html = TRUE,
                                   text = 
                                   HTML("Copyright (c) 2019 Claudio Zanettini <br> 
                                   <a href=\"https://github.com/c1au6i0/rvent_app/blob/master/LICENSE.md\"> LICENSE </a>"),
                                   width = "200px"
                    )
                   }
  
    )
    
     
    
      # get the iox files-----
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
                          
                          if(input$tutorial == TRUE) {
                                showNotification("Click on the menu, indentify drug injections and press OK!", duration = 5)
                          } 
                           
                           p_hide$okb <- 1
                           choose_comments <- tidyr::unite(all_data$tsd_s, col = "subj_drug_dose_unit", 
                                                           .data$subj, .data$drug, .data$dose, .data$unit, sep = " ")
                           
                           vent(all_data$vent)
                           c_comments$tsd_s <- choose_comments
                       } else {
                           sendSweetAlert(session = session, title = "Error!", text = all_data, type = "error" )
                       }
                  }
    )
    
      # load demo data---------
      observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$demo
                 },
                 handlerExpr = {
                   url <- "https://lynu3a.bn.files.1drv.com/y4mW2_-dc2Q-qPDhsY1sAc8qgDUnh0NGGY42BQCqULVT6rY8gUxRI5C__-l0UkUblNAxw_tyrlNbZGoUExhNjlaLq2gvT-VfWhBHe7fnvJ1d4ZP2IaUSjqnFDtMbk-EN_UI_fIl3rN6MqvY5fEAwgdWiu_M9EaB3taT8qNdMjd_3fncGXKpuDBdJsB_WqTV08XgkrgFMt9bVwfGIDRxNZ3Eeq_SBTaipvDnM8u4YDSW240/all_data.RDA?download&psid=1"
                   tmpFile <- tempfile()
                   download.file(url, destfile = tmpFile)
                   load(tmpFile)
                   if(input$tutorial == TRUE) {
                     showNotification("Click on the menu, indentify drug injections and press OK!", duration = 5)
                   } 
                   p_hide$okb <- 1
                   choose_comments <- tidyr::unite(all_data$tsd_s, col = "subj_drug_dose_unit", 
                                                   .data$subj, .data$drug, .data$dose, .data$unit, sep = " ")
                   
                   vent(all_data$vent)
                   c_comments$tsd_s <- choose_comments
                   
                 }
                 )
      
      # select comments with subject and drug ------
      observe({ 
                updateSelectInput(session, "com_sub", label = c_comments$lab, choices = c_comments$tsd_s$subj_drug_dose_unit)
      }
    )
      
      #  input$com_sub is use to filter tsd_s
      # filter comments: are there NA or not? -----------
      observeEvent(ignoreNULL = TRUE,
                   eventExpr = {
                     input$OK_com
                   },
                   handlerExpr = {
                    if(is.null(input$com_sub)) {
                          if(input$tutorial == TRUE) {
                              showNotification("Please select something!", duration = 5)
                          } 

                          p_hide$DT <- NA
                    } else {
                         p_hide$stat <- 1
                         tsd_sf <- c_comments$tsd_s[c_comments$tsd_s$subj_drug_dose_unit %in% input$com_sub, ]
                         tsd_sf <- tidyr::separate(tsd_sf,
                                                  subj_drug_dose_unit,
                                                  c("subj", "drug", "dose", "unit"),
                                                  fill = "right", extra = "merge")
                         tsd_sf[tsd_sf == "NA"] <- NA
                         c_comments$tsd_sf <- tsd_sf
                         if (sum(is.na(c_comments$tsd_sf)) > 0) {
                         output$tsd_sf <-  renderDT(tsd_sf, selection = 'none', server = F, editable = T)
                         
                         
                         if(input$tutorial == TRUE) {
                           showNotification("Please, fill up missing values!", duration = 5)
                         } 
                     
                         p_hide$DT <- 1
                         } else {
                         p_hide$DT <- NA
                         p_hide$stat <- NA
                         }
                     }
                   }
      )
  
      # edit table with comments -----------
      observeEvent(input$tsd_sf_cell_edit,
                        handlerExpr = {
                          c_comments$tsd_sf[input$tsd_sf_cell_edit$row,input$tsd_sf_cell_edit$col] <<- input$tsd_sf_cell_edit$value
                          if(sum(is.na(c_comments$tsd_sf)) == 0){
                            p_hide$stat <- NA
                            
                            if(input$tutorial == TRUE) {
                              showNotification("Now select stats bin and baseline!", duration = 5)
                            } 
                            
                          }
                         }

       )
      
      # table with comments edited: slide and checkbox -----------
      observeEvent(
        eventExpr = {
          input$summarize
        },
        handlerExpr = {
          if(is.null(input$vent_stat)) {
            
            if(input$tutorial == TRUE) {
              showNotification("Choose at least one stat!", duration = 5)
            } 
            
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
            sendSweetAlert(
              session = session,
              title = "Success!",
              text = "Excel file in iox folder",
              type = "success",
              width = "200px"
            )
            output$summarized_dat <-  renderDT(vent_all$dat_sml, 
                                               selection = 'none', 
                                               server = F, 
                                               editable = F,
                                               fillContainer = FALSE,
                                               options = list(
                                                   pageLength = 12,
                                                   autoWidth = TRUE
                                               )
            )
          }
        }

      )

}   

# Run the application
shinyApp(ui = ui, server = server)
