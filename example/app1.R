library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            switchInput("sw", label = "Switch", value = TRUE),
            actionButton("but", "see all")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("all_inp")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent(
        input$but,
         handlerExpr = {
             sess_inputs <- lapply(names(input), function(x) {
                 input[[x]]
             })
             output$all_inp <- renderText({paste0(sess_inputs,  collapse = " ")})
         }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
