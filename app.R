library(shiny)
library(rvent)
library(shinyFiles)
library(DT)
library(shinyWidgets)
library(RCurl)
library(shinythemes)
library(ggplot2)
library(shinycssloaders)




  

# if("rvent" %in% installed.packages()[,"Package"] == FALSE) 


# NOTE DEMO NEED A PATH!!!!!!!!!
# #summarize {
#     font-weight: bold;
#     background: LightGreen;
#     border-color: green;
# }
# .modal-dialog {
#          position:fixed;
#          top: calc(0%);
#          left: calc(50%);
#          width: 400px;
# }


# UI-------------
ui <- fluidPage(
  theme = shinytheme("paper"),
  tags$head(tags$style(
    HTML("

    #OK_com{
        border-color: green
    }
    
    #summarized_dat {
        font-size: 12px;
    } 
    

    .form-control.shiny-bound-input {
        text-align: center;
    }
    

    .progress {
            width: 400px;

    }
    
    
    #bin {
            text-align: center;
    }
    
         
         ")
  )),
  useSweetAlert(),
  tabsetPanel(
    id = "all",
    # types = "pills",
    # SUMMARY STATS--------------------------------------------------------------------
    tabPanel(
      title = "summary stats",
      sidebarLayout(
        # side panel------------
        sidebarPanel(
          width = 6,


          wellPanel(
            div(
              style = "display: inline-block;vertical-align:top;",
              switchInput("tutorial", label = "Tutorial", value = TRUE)
            ),
            div(
              style = "display: inline-block;vertical-align:top; float:right;",
              actionButton("license", label = "LICENSE", width = "200px")
            )
          ),

          div(
            style = "display: inline-block;vertical-align:top;",
            shinyDirButton("dir", "Input directory", "Upload")
          ),

          div(
            style = "display: inline-block;vertical-align:top;",
            actionButton("demo", label = "Demo Data")
          ),

          div(
            style = "display: inline-block;vertical-align:top;",
            conditionalPanel(
              condition = "output.hideokb",
              actionButton("OK_com", label = "OK")
            )
          ),

          conditionalPanel(
            condition = "output.hideokb",
            selectInput("com_sub",
              label = "",
              choices = "",
              multiple = TRUE
            )
          ),
          br(),
          br(),
          br(),
          br(),

          conditionalPanel(
            condition = "output.hidedt == false",
            DTOutput("tsd_sf")
          ),
          br(),
          br(),
          br(),
          br(),
          div(
            style = "display: inline-block;vertical-align:top;",
            conditionalPanel(
              condition = "output.hidestat",
              checkboxGroupButtons(
                inputId = "vent_stat",
                label = "stats",
                choices = c(
                  "mean",
                  "median",
                  "n",
                  "sd"
                )
              )
            )
          ),

          div(
            style = "display: inline-block;vertical-align:top;",
            conditionalPanel(
              condition = "output.hidestat",
              numericInput("bin",
                label = "bin (min)", value = 1, min = 1, width = "100px"
              ),
            )),
          div(
            style = "display: inline-block;vertical-align:top;",
            conditionalPanel(
              condition = "output.hidestat",
              numericInput("baseline",
                label = "baseline (min)", value = 30, min = 1, width = "100px"
              ),
            ),),
          conditionalPanel(
            condition = "output.hidestat",
            actionButton("summarize", label = "visualize and save summary", width = "100%")
          )
        ),
        # main panel------------
        mainPanel(
          width = 6,
          DTOutput("summarized_dat")
        )
      )
    ),

    # PLOTS--------------------------------------------------------------------
      tabPanel(
        title = "plots", 
           sidebarLayout(
             # side panel-----------------
                sidebarPanel(
                  width = 4,
                  div(
                    style = "display: inline-block;vertical-align:top;",
                  radioGroupButtons(
                    inputId = "stat_plot",
                    label = "stat",
                    choices = c(
                      "mean",
                      "median")
                  )),
                  div(
                    style = "display: inline-block;vertical-align:top;",
                      numericInput("baseline2",
                                   label = "baseline (min)", value = 30, min = 1, width = "100px"
                      )
                  ),

                  br(),
                  br(),
                  sliderInput("bin_plot",
                    label = "Slider: bin duration (min)", min = 0,
                    max = 30, value = 1
                  ),

                  pickerInput(
                    inputId = "measures",
                    label = "Metric", 
                    choices = "",
                    multiple = TRUE
                  ),
                  wellPanel(
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      actionButton("show_plots", label = "Show Plots")
                    ),
                    
                    div(
                      style = "display: inline-block;vertical-align:top; float:right;",
                      conditionalPanel(
                          condition = "output.saveplots",
                          actionButton("save_plots", label = "Save Last Plots"))
                    )
                  ),
                ),
                # main panel-----------------
                mainPanel(
                  width = 8,
                  tabsetPanel(id = "plots_in_plots"

                
  
              )

          )
        )
      )
    )
  )




# SERVER-----------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # initialize -------
  shinyDirChoose(
    input,
    "dir",
    roots = c(home = "~"),
    filetypes = c("txt", "tsv", "csv")
  )

  hideTab("all", "plots")
  
  showModal(modalDialog(
    title = "Tutorial",
    "Click on input button to select iox folder or use DemoData!",
    "You can turn off these notifications with the Tutorial-switch button",
    footer = modalButton("OK"),
    size = "m",
    easyClose = TRUE))


  # reactive val-----------------------
  dpath <- reactiveVal()
  dir <- reactive(input$dir)
  c_comments <- reactiveValues()
  vent <- reactiveVal()
  rc_ses <- reactiveVal()
  
  rc_plots  <- reactiveVal()


  # hide --------------------------
  p_hide <- reactiveValues()

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
  
  
  output$saveplots <- reactive({
    p_hide$saveplots
  })

  # outputOptions --------------MAKE A LIST
  outputOptions(output, "hidestat", suspendWhenHidden = FALSE)
  outputOptions(output, "hidedt", suspendWhenHidden = FALSE)
  outputOptions(output, "hideokb", suspendWhenHidden = FALSE)
  outputOptions(output, "saveplots", suspendWhenHidden = FALSE)



  summarized_dat <- reactiveVal()

  # license --------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$license
    },
    handlerExpr = {
      sendSweetAlert(
        session = session,
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
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$dir
    },
    handlerExpr = {
      if (!"path" %in% names(dir())) {
        return()
      }
      home <- normalizePath("~")
      datapath <-
        file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))

      dpath(datapath)

      all_data <- tryCatch(
        get_iox(iox_folder = datapath, inter = FALSE, baseline = 30),
        error = function(c) conditionMessage(c)
      )
      if (is.list(all_data)) {
        if (input$tutorial == TRUE) {
          showModal(modalDialog(
            title = "Tutorial",
            "Click on the menu, indentify drug injections and press OK!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE))
        }

        p_hide$okb <- 1
        choose_comments <- tidyr::unite(all_data$tsd_s,
          col = "subj_drug_dose_unit",
          .data$subj, .data$drug, .data$dose, .data$unit, sep = " "
        )

        vent(all_data$vent)
        c_comments$tsd_s <- choose_comments
      } else {
        sendSweetAlert(session = session, title = "Error!", text = all_data, type = "error")
      }
    }
  )

  # load demo data---------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$demo
    },
    handlerExpr = {
     
    
      url <- "https://poaf2q.bn.files.1drv.com/y4mM0GOMscIPKhDpSnn_Kt1LcuNWWD0jpMKF6iZn0wt3M5hgYsgqaiqHXk-QQNGbEQBO8q2Q4OW3Jl6sz95MY-GV8CBUrNYNuayofF7UskjA1Tln2OWCfpyfFy13nGkqvn4M4E4o_ogqo5fcd1CG9n8Cx3RiIev73cnCOLM5uRYivDZ_b2fyGb7tIx-qO515kg7IEDP4qTiduQDFjEi1JNl2Lgs6qP0WqmFQ3_QNXt-WU4/all_data.rda?download&psid=1"
   
      tempos <- tempfile()
      download.file(url, destfile = tempos, mode = "wb")
      load(tempos)
      if (input$tutorial == TRUE) {
        
        showModal(modalDialog(
          title = "Tutorial",
          "Click on the menu, indentify drug injections and press OK!",
          footer = modalButton("OK"),
          size = "m",
          easyClose = TRUE))
      }
      p_hide$okb <- 1
      choose_comments <- tidyr::unite(all_data$tsd_s,
        col = "subj_drug_dose_unit",
        .data$subj, .data$drug, .data$dose, .data$unit, sep = " "
      )

      vent(all_data$vent)
      c_comments$tsd_s <- choose_comments
    }
  )

  # select comments with subject and drug ------
  observe({
    updateSelectInput(session, "com_sub", label = c_comments$lab, choices = c_comments$tsd_s$subj_drug_dose_unit)
  })

  #  input$com_sub is use to filter tsd_s
  # filter comments: are there NA or not? -----------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$OK_com
    },
    handlerExpr = {
      if (is.null(input$com_sub)) {
        if (input$tutorial == TRUE) {
          
          showModal(modalDialog(
            title = "Tutorial",
            "Please select something!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE))
        }

        p_hide$DT <- NA
      } else {
        p_hide$stat <- 1
        tsd_sf <- c_comments$tsd_s[c_comments$tsd_s$subj_drug_dose_unit %in% input$com_sub, ]
        tsd_sf <- tidyr::separate(tsd_sf,
          subj_drug_dose_unit,
          c("subj", "drug", "dose", "unit"),
          fill = "right", extra = "merge"
        )
        tsd_sf[tsd_sf == "NA"] <- NA
        c_comments$tsd_sf <- tsd_sf
        if (sum(is.na(c_comments$tsd_sf)) > 0) {
          output$tsd_sf <- renderDT(tsd_sf, selection = "none", server = F, editable = T)


          if (input$tutorial == TRUE) {
            showModal(modalDialog(
              title = "Tutorial",
              "Please, fill up missing values!",
              footer = modalButton("OK"),
              size = "m",
              easyClose = TRUE))
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
      c_comments$tsd_sf[input$tsd_sf_cell_edit$row, input$tsd_sf_cell_edit$col] <<- input$tsd_sf_cell_edit$value
      if (sum(is.na(c_comments$tsd_sf)) == 0) {
        p_hide$stat <- NA

        if (input$tutorial == TRUE) {

          showModal(modalDialog(
            title = "Tutorial",
            "Now select stats bin and baseline!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE))
        }
      }
    }
  )

  # table with comments edited SAVE -----------
  observeEvent(
    eventExpr = {
      input$summarize
    },
    handlerExpr = {
      if (is.null(input$vent_stat)) {
        if (input$tutorial == TRUE) {
          showModal(modalDialog(
            title = "Tutorial",
            "Choose at least one stat!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE))
        }
      } else {
        sess1 <- normalizetime_vent(
          dat = vent(),
          tsd_s = c_comments$tsd_sf,
          tofill = NULL,
          baseline = input$baseline
        )

        rc_ses(sess1)
        showTab("all", "plots")
        
        vent_all <- summarize_vent(sess1, inter = FALSE, baseline = input$baseline, bin = input$bin, form = input$vent_stat)

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
        output$summarized_dat <- renderDT(vent_all$dat_sml,
          selection = "none",
          server = F,
          editable = F,
          fillContainer = FALSE,
          options = list(
            pageLength = 12,
            autoWidth = TRUE
          )
        )
        # choices tab plots
        measure_choices <- c("ALL", levels(vent_all$dat_vent$measure))
        updateSelectInput(session, "measures",
                          choices = measure_choices)
        
      }
    }
  )

  
  # show figs-------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$show_plots
    },
    handlerExpr = {

      measure <- input$measures
      if(length(measure) > 1 & "ALL" %in% measure) {
        measure <- "ALL"
      }
      
      p_hide$saveplots <- 1
      plots <- session_plots(rc_ses(), path = dpath(), inter = FALSE, 
                             vent_stat = input$stat_plot, baseline = input$baseline2, bin = input$bin_plot,
                             measure = measure,
                             
                             
                             fsave = FALSE)
      rc_plots(plots)

     
        # thanks mr flick

           Map(function(x){
             
  
              title_t  <- paste(x$data$subj[1], input$stat_plot, sep = " ")
              plot_subj <- paste(x$data$subj[1], 
                                 input$stat_plot, 
                                 input$baseline2, 
                                 input$bin_plot,
                                 paste(measure, collapse = "_"),
                                 sep = "_")
              output[[plot_subj]] <- renderPlot({x})
            
             appendTab(inputId = "plots_in_plots",
             tabPanel(title = title_t, plotOutput(plot_subj)))
             }, plots)
   
      }
    )
  # save figs------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$save_plots
    },
    handlerExpr = {
      
      # save data
      withProgress(
        expr = {
          plots <- rc_plots()
          lapply(plots, function(dat){
            file_name <- paste(as.character(dat$data$cpu_date[1]), dat$data$subj[1], dat$data$drug[1], dat$data$dose[1], sep = "_")
            file_path <- paste(dpath(), file_name, sep = .Platform$file.sep)
            ggsave(paste0(file_path, ".pdf"), dat, device = "pdf", width = 30, height = 30, units = "cm")
          })}, message = "Computing...please wait")
      
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "Plots in iox folder",
        type = "success",
        width = "200px"
      )
      
    })
  
}
# https://gist.github.com/wch/5436415/
# https://stackoverflow.com/questions/35737029/how-to-generate-output-for-multi-plots-within-a-loop-in-shiny-app
# Run the application
shinyApp(ui = ui, server = server)

# prova = "ciao"
# assign(prova, "OK")
# title_t <- "subj1"
# 
# assign(paste0("output$",title_t), "ciao")
