library(shiny)
library(rvent)
library(shinyFiles)
library(DT)
library(shinyWidgets)

ui <- fluidPage( # 

# https://stackoverflow.com/questions/36709441/how-to-display-widgets-inline-in-shiny
# https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193
# https://stackoverflow.com/questions/44112000/move-r-shiny-shownotification-to-center-of-screen
  
# https://shiny.rstudio.com/articles/notifications.html
  
  tags$head(tags$style(
    HTML('
    #display_msg {
        background: white;
        font-size: 14px;
        font-family: "Helvetica Neue",Helvetica,sans-serif;
    }
    
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

         
         ')
    )),
  useSweetAlert(),
  tabsetPanel(
    # SUMMARY STATS--------------------------------------------------------------------
    tabPanel("summary stats",

            sidebarLayout(
            # side panel------------
                  sidebarPanel(width = 6,
                              verbatimTextOutput("display_msg", placeholder = TRUE),
          
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
    
    output$display_msg <- renderText("Choose a foder that contains iox files or the DemoData!") 
    
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
    
      # load demo data---------
      observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$demo
                 },
                 handlerExpr = {
                   url <- "https://zdifdq.bn.files.1drv.com/y4mok_3m1Uq3K_1809wW-uDtSpQ0qkSZI5hi3nPtKZa8hY33P4j0l1SLjD5qFWHLA3uQzXr3T0gS-5BDT3Xh3IlrU9TpICyeCTcXOg9n3g48yTmthp1Q5jY9etGRJLyxZvhH84AZA1C0LUNxIn2AbFaED7lH1Hkpcf9aPUBLuavVdb5ppEFCNNgMWKzMOTnzr-iYzmtXcMsc0PiblMbYAvvArnX_wQNucA_9asXfZQvQmI/all_data.RDA?download&psid=1"
                   tmpFile <- tempfile()
                   download.file(url, destfile = tmpFile)
                   load(tmpFile)
                   output$display_msg <- renderText("Click on the menu and indentify drug injections!")
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
                          output$display_msg <- renderText("Please select something!")
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
                         output$display_msg <- renderText("Please, fill up missing values!")
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
                            output$display_msg <- renderText("Now select stats bin and baseline!")
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
            output$display_msg <- renderText("Choose at least one stat!")
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
