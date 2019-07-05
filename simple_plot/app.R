
library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple plot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("color", label = h3("Choose color"), 
                        choices = list("Red" = 'red', "Green" = 'green', "Blue" = 'blue'), 
                        selected = 1),
            
            hr(),
            fluidRow(column(3, verbatimTextOutput("value")))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        x <- 1:10
        plot(x, col=input$color, type='l')

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
