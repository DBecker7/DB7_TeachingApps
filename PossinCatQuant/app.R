#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Is Poisson Discrete or Continuous?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("n",
                        "n",
                        min = 10,
                        max = 500,
                        value = 30),
            sliderInput("lam",
                        "lambda",
                        min = 1,
                        max = 500,
                        value = 30),
            sliderInput("alpha",
                        "Overdispersion",
                        min = 0,
                        max = 2,
                        value = 0.1,step = 0.1),
            actionButton("doit", "Click me for new data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    xdata <- reactive({
        input$doit
        rnbinom(input$n, mu = input$lam, size = 1/input$alpha+0.00001)
    })

    output$distPlot <- renderPlot({
        x <- xdata()
        
        gghist <- ggplot() + geom_histogram(aes(x = x), 
            bins = input$bins, fill = "lightgrey", colour = 1) +
            theme_bw() +
            labs(x = x, y = "Count", title = "Histogram")
        ggbar <- ggplot() + geom_bar(aes(x = x), 
            fill = "lightgrey", colour = 1) +
            theme_bw() +
            labs(x = x, y = "Count", title = "Bar plot")
        
        gghist / ggbar
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
