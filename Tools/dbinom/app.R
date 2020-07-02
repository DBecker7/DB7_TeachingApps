#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "n", 1, 40, 20, 1, 
                animate = list(interval = 700)),
            sliderInput("p", "p", 0, 1, 0.5, 0.01, 
                animate = list(interval = 700)),
            sliderInput("x", "x", 0, 40, 10, 1, 
                animate = list(interval = 700))
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
        x <- 0:input$n
        y <- dbinom(x, input$n, input$p)
        cols <- rep("grey", 70)
        cols[1:(input$x + 1)] <- 2
        cols[input$x+1] <- 4
        cols <- cols[1:input$n]
        
        mytitle <- paste0("P(X<=", input$x, ")=pbinom(", 
                input$x, ",", input$n, ",",input$p,")=" ,
            round(pbinom(input$x, input$n, input$p), 2),
            ",      P(X=", input$x, ")=dbinom(", 
                input$x, ",", input$n, ",",input$p,")=" ,
            round(dbinom(input$x, input$n, input$p), 2))
        
        
        barplot(y, names = x, col = cols, main = mytitle)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
