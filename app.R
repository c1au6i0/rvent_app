# rvent_app v0.2.1.100
# Baltimore (MD), November 2019
# Claudio Zanettini
library(devtools)
library(DT)
library(httr)
library(ggplot2)
library(gmailr)
library(googledrive)
library(shiny)
library(shinyalert)
library(shinyFiles)
library(shinyjs)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(vroom)
library(V8)
library(rvent)

if("rvent" %in% installed.packages()[,"Package"] == FALSE){
  devtools::install_github("c1au6i0/rvent")
}

# authenticate---
drive_auth(path = "rden-259921-43bdd6f37aac.json")
# https://github.com/r-lib/gmailr/issues/115
gm_auth_configure(path = "client_secret.json")
gm_auth("cshinyapp@gmail.com", cache = ".secrets")

# now 
#-----------

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# UI-------------
ui <- fluidPage(


  useShinyjs(),
  useShinyalert(),
  extendShinyjs(text = jsResetCode),
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
    
    input::placeholder {
    text-align: center; 
    }
    
    input { 
        text-align: center; 
    }
    
    input:-moz-placeholder { 
        text-align:right; 
    }

    .progress {
            width: 400px;

    }
    
    
    #bin {
        text-align: center;
    }
    
    #save_summary {
        float:center !important;
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

 
          div(
              style = "display: inline-block;vertical-align: center;",
              switchInput("tutorial", label = "Tutorial", value = TRUE)),
          div(
               style = "display: inline-block;vertical-align: center; float: right;",
              actionButton("license", label = "about", style = "text-transform: lowercase; font-style: italic;")
          ),
          
          fileInput("iox_files",
            label = NULL,
            buttonLabel = "Import Files",
            multiple = TRUE,
            accept = c("txt", "tsv", "csv"),
            placeholder = "0 upload",
            width = "70%"
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

          conditionalPanel(
            condition = "output.hidedt == false",
            DTOutput("tsd_sf")
          ),

          wellPanel(
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
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top;",
              conditionalPanel(
                condition = "output.hidestat",
                numericInput("baseline",
                  label = "baseline (min)", value = 30, min = 1, width = "100px"
                ),
              ),
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top;",
            conditionalPanel(
              condition = "output.hidestat",
              actionButton("summarize", label = "visualize summary")
            ), style = "float:rcenter"
          ),
          div(
            style = "display: inline-block;vertical-align:top;",
            conditionalPanel(
              condition = "output.save_b",
              downloadButton("save_summary", "save"), style = "float:center"
            ),
          ),
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

          switchInput("tutorial2", label = "Tutorial", value = TRUE),


          radioGroupButtons(
            inputId = "stat_plot",
            label = "stat",
            choices = c(
              "mean",
              "median"
            )
          ),
          br(),
          sliderInput("bin_plot",
            label = "Slider: bin duration (min)", min = 0,
            max = 30, value = 1
          ),

          selectInput(
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
                downloadButton("save_plots", "save")
              )
            )
          ),
        ),
        # main panel-----------------
        mainPanel(
          width = 8,
          tabsetPanel(id = "plots_in_plots")
        )
      )
    )
  )
)




# SERVER-----------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  source("helpers.R", local = TRUE)

  # session$onSessionEnded(stopApp)
 
  # this if for RInno
   if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
   }
  
  hideTab("all", "plots")
  
  showModal(modalDialog(
    title = "Tutorial",
    "Click on the input button to select iox files or use DemoData!",
    "If you select by mistake a file that is not an iox.txt, don't worry, it will be filtered out!",
    "You can turn off these notifications with the Tutorial-switch button.",
    footer = modalButton("OK"),
    size = "m",
    easyClose = TRUE
  ))


  # update tutorial in sync-----------------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$tutorial
    },
    handlerExpr = {
      updateSwitchInput(session, "tutorial2", value = input$tutorial)
    }
  )

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$tutorial2
    },
    handlerExpr = {
      updateSwitchInput(session, "tutorial", value = input$tutorial2)
    }
  )



  # reactive val-----------------------
  dpath <- reactiveVal()
  c_comments <- reactiveValues()
  vent <- reactiveVal()
  rc_ses <- reactiveVal()
  rc_plots <- reactiveVal()
  summarized_dat <- reactiveVal()
  # this is to create unique output number
  # for plots that will be rendered
  rc_tabs <- reactiveVal(0)

  demo_imp <- reactiveVal() # has 2 possible value: demo or imp (demo or imported)


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

  # save summary button
  output$save_b <- reactive({
    p_hide$save_summary
  })


  output$saveplots <- reactive({
    p_hide$saveplots
  })

  # outputOptions --------------MAKE A LIST
  outputOptions(output, "hidestat", suspendWhenHidden = FALSE)
  outputOptions(output, "hidedt", suspendWhenHidden = FALSE)
  outputOptions(output, "hideokb", suspendWhenHidden = FALSE)
  outputOptions(output, "saveplots", suspendWhenHidden = FALSE)
  outputOptions(output, "save_b", suspendWhenHidden = FALSE)


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
          HTML("Copyright (c) 2019 Claudio Zanettini (v.0.2.1.1000)<br> 
              <a href=\"https://github.com/c1au6i0/rvent_app/blob/master/LICENSE.md\"> LICENSE </a>"),
        width = "200px"
      )
    }
  )

  # imported data or demo?----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$iox_files
    },
    handlerExpr = {
      demo_imp("imp")
    }
  )

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$demo
    },
    handlerExpr = {
      demo_imp("demo")
    }
  )

  # get the iox files-----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$iox_files
    },
    handlerExpr = {
      all_data <- tryCatch(
        get_iox(iox_data = input$iox_files, shiny_f = TRUE, inter = FALSE),
        error = function(c) conditionMessage(c)
      )

      # if error--

      if (is.character(all_data)) {
        shinyalert("Error!",
          text = HTML(paste0(
            "<i> Error: ", all_data, "</i>", "<br>",
            "<b>Do you want to send an error report to the developers?</b>"
          )),
          type = "error",
          html = TRUE,
          confirmButtonText = "Yes", showCancelButton = TRUE,
          cancelButtonText = "No", callbackR = modalCallback
        )
      } else {
        if (input$tutorial == TRUE) {
          showModal(modalDialog(
            title = "Tutorial",
            "Click on the menu, indentify drug treatments (first injection) and press OK!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE
          ))
        }

        p_hide$okb <- 1
        choose_comments <- tidyr::unite(all_data$tsd_s,
          col = "subj_drug_dose_unit",
          .data$subj, .data$drug, .data$dose, .data$unit, sep = " "
        )

        vent(all_data$vent)
        c_comments$tsd_s <- choose_comments
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

      id_data <- "15Y2hJuczpqF4q2Op0swfMbqBwPDH418G"

      withProgress(
        drive_download(file = as_id(id_data), "all_data.rds", overwrite = TRUE),
        message = "Loading the data...please wait"
      )
      id_data <- "15Y2hJuczpqF4q2Op0swfMbqBwPDH418G"

      all_data <- readRDS("all_data.rds")
      

      if (input$tutorial == TRUE) {
        showModal(modalDialog(
          title = "Tutorial",
          "Click on the blank menu below, indentify drug injections and press OK!",
          footer = modalButton("OK"),
          size = "m",
          easyClose = TRUE
        ))
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
            easyClose = TRUE
          ))
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

        tsd_sf$cpu_date <- as.character(tsd_sf$cpu_date)

        tsd_sf[tsd_sf == "NA"] <- NA

        c_comments$tsd_sf <- tsd_sf
        if (sum(is.na(c_comments$tsd_sf)) > 0) {
          output$tsd_sf <- renderDT(tsd_sf, selection = "none", server = T, editable = T)


          if (input$tutorial == TRUE) {
            showModal(modalDialog(
              title = "Tutorial",
              "Please, fill up any missing value in the table!",
              footer = modalButton("OK"),
              size = "m",
              easyClose = TRUE
            ))
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
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$tsd_sf_cell_edit
    },
    handlerExpr = {
      c_comments$tsd_sf[input$tsd_sf_cell_edit$row, input$tsd_sf_cell_edit$col] <<- input$tsd_sf_cell_edit$value
      if (sum(is.na(c_comments$tsd_sf)) == 0) {
        p_hide$stat <- NA

        if (input$tutorial == TRUE) {
          showModal(modalDialog(
            title = "Tutorial",
            "Now scroll down and select stats, bin and baseline. 
             You can select multiple stats!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE
          ))
        }
      }
    }
  )

  # calculate summary -----------
  observeEvent(
    eventExpr = {
      input$summarize
    },
    handlerExpr = {
      if (is.null(input$vent_stat)) {
        showModal(modalDialog(
          title = "Tutorial",
          "Choose at least one stat!",
          footer = modalButton("OK"),
          size = "m",
          easyClose = TRUE
        ))
      } else {
        sess1 <- tryCatch(
          normalizetime_vent(
            dat = vent(),
            tsd_s = c_comments$tsd_sf,
            tofill = NULL,
            baseline = input$baseline
          ),
          error = function(c) conditionMessage(c)
        )

        rc_ses(sess1)
        showTab("all", "plots")

        data_origin <- demo_imp()
        if (data_origin == "demo") {
          vent_all <- summarize_vent(sess1,
            inter = FALSE,
            baseline =
              input$baseline,
            bin = input$bin,
            form = input$vent_stat,
            filter_val = FALSE
          )
        } else {
          vent_all <-
            tryCatch(
              summarize_vent(sess1,
                inter = FALSE,
                baseline =
                  input$baseline,
                bin = input$bin,
                form = input$vent_stat
              ),
              error = function(c) conditionMessage(c)
            )
        }

        vent_all$dat_sml <- vent_all$dat_sml %>% 
          mutate_at(vars(-value), as.factor)

        summarized_dat(vent_all)

        # if error send error -----
        if (is.character(sess1) | is.character(summarize_vent)) {
          shinyalert("Error!",
            text = HTML(paste0(
              "<i> Error: ", vent_all, "</i>", "<br>",
              "<b>Do you want to send an error report to the developers?</b>"
            )),
            type = "error",
            html = TRUE,
            confirmButtonText = "Yes", showCancelButton = TRUE,
            cancelButtonText = "No", callbackR = modalCallback
          )
        } else {
          output$summarized_dat <- renderDT(vent_all$dat_sml,
            filter = "top",
            selection = "none",
            server = F,
            editable = F,
            # fillContainer = FALSE,
            options = list(
              pageLength = 12,
              autoWidth = TRUE
            )
          )

          # show save summary button
          p_hide$save_summary <- 1

          if (input$tutorial == TRUE) {
            showModal(modalDialog(
              title = "Tutorial",
              "Now you can save the summary by pressing the button below or also,
            plot the data by selecting the 'plots' tab on the top left of the screen.",
              footer = modalButton("OK"),
              size = "m",
              easyClose = TRUE
            ))
          }

          # choices tab plots
          measure_choices <- c("ALL", as.character(levels(vent_all$dat_vent$measure)))
          updateSelectInput(session, "measures",
            choices = measure_choices
          )
        }
      }
    }
  )

  # save summary ---------------------------
  output$save_summary <- downloadHandler(
    filename = function() {
      paste0("summary", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(summarized_dat()$dat_fs, file)
    }
  )


  # tutorial tab plot selection --------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$all
    },
    handlerExpr = {
      if (input$tutorial2 == TRUE) {
        if (input$all == "plots") {
          showModal(modalDialog(
            title = "Tutorial",
            "Select the stat that you want to use to summarize the bins, the duration of the bins,
              the metric that you want to use and then, press visualize. You can use 'ALL' metrics or select just some of them.
              You are free to play with the parameters and make as many plots as you want!",
            footer = modalButton("OK"),
            size = "m",
            easyClose = TRUE
          ))
        }
      }
    }
  )

  # calculate and show figs-------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$show_plots
    },
    handlerExpr = {
      if (is.null(input$measures)) {
        showModal(modalDialog(
          title = "Tutorial",
          "Please select a Metric!",
          footer = modalButton("OK"),
          size = "m",
          easyClose = TRUE
        ))
      } else {
        measure <- input$measures
        if (length(measure) > 1 & "ALL" %in% measure) {
          measure <- "ALL"
        }

        p_hide$saveplots <- 1
        sess1 <- na.omit(rc_ses())
        data_origin <- demo_imp()
        if (data_origin == "demo") {
          plots <- session_plots(sess1,
            path = NULL, inter = FALSE,
            vent_stat = input$stat_plot, bin = input$bin_plot,
            measure = measure,
            fsave = FALSE,
            filter_vals = FALSE
          )
        } else {
          plots <-
            tryCatch(session_plots(sess1,
              path = NULL, inter = FALSE,
              vent_stat = input$stat_plot, bin = input$bin_plot,
              measure = measure,
              fsave = FALSE),
              error = function(c) conditionMessage(c)
            )
        }

        # if error---
        if (is.character(session_plots)) {
          shinyalert("Error!",
            text = HTML(paste0(
              "<i> Error: ", session_plots, "</i>", "<br>",
              "<b>Do you want to send an error report to the developers?</b>"
            )),
            type = "error",
            html = TRUE,
            confirmButtonText = "Yes", showCancelButton = TRUE,
            cancelButtonText = "No", callbackR = modalCallback
          )
        } else {



          # thanks mr flick
          Map(function(x) {
            tab_n <- rc_tabs()
            tab_n <- tab_n + 1
            rc_tabs(tab_n)
            title_t <- paste(x$data$subj[1], input$stat_plot, sep = " ")
            plot_subj <- paste(x$data$subj[1],
              input$stat_plot,
              input$bin_plot,
              input$baseline,
              paste(measure, collapse = "_"),
              tab_n,
              sep = "_"
            )
            output[[plot_subj]] <- renderPlot({
              x
            })

            appendTab(
              inputId = "plots_in_plots",
              tabPanel(
                title = title_t,
                plotOutput(plot_subj) %>% withSpinner()
              )
            )
          }, plots)
          rc_plots(plots)

          if (input$tutorial2 == TRUE) {
            if (demo_imp() == "imp") {
              showModal(modalDialog(
                title = "Tutorial",
                "Click on the tabs with the subject name to see the graphs.
                  You can save the last plots rendered by clicking on the save button!",
                footer = modalButton("OK"),
                size = "m",
                easyClose = TRUE
              ))
            } else {
              showModal(modalDialog(
                title = "Tutorial",
                "Click on the tabs with the subject name to see the graphs.
                       The data might look a little bit flat...that is because they are indeed random-normal values
                       created with the function 'runif'. If you want real data, you got to do the experiment yourself!",
                footer = modalButton("OK"),
                size = "m",
                easyClose = TRUE
              ))
            }
          }
        }
      }
    }
  )
  # save figs------
  output$save_plots <- downloadHandler(
    file = function() {
      paste0("plot_", as.character(summarized_dat()$dat_vent$cpu_date[1]), "_", rc_tabs(), ".pdf")
    },
    content = function(file) {
      pdf(file)
      print(rc_plots())
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
