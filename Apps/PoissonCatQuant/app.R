# PoissonCatQuant

library(MASS)
library(fitdistrplus)
library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)

ui <- fluidPage(
    
    # Application title
    titlePanel("Is Poisson Discrete or Continuous?"),
     
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                "Number of bins (doesn't change data):",
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
                max = 2000,
                value = 30),
            sliderInput("alpha",
                "Overdispersion",
                min = 0,
                max = 1,
                value = 0,step = 0.1),
            actionButton("doit", "Click me for new data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("Some questions:
                <ol>
                    <li>Find a parameter combination where the barplot looks better. Change the number of bins to give the histogram the best chance.</li>
                    <li>Set the overdispersion to 0 and change the mean parameter (lambda) so that the histogram looks better.</li>
                    <li>Set the mean to 5 and change the mean parameter so that the histogram looks better.</li>
                    <li>Which display would be better for the number of children in families?</li>
                    <li>Which display would be better for the number of cents in bank accounts?</li>
                </ol>
                "))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    xdata <- reactive({
        input$doit
        x <- rnegbin(n = input$n, mu = input$lam,
            theta = 1/(input$alpha+0.00001))
        xest <- mledist(x, "nbinom")$estimate
        list(x = x, xest = xest)
    })
    
    output$distPlot <- renderPlot({
        x <- xdata()
        
        gghist <- ggplot(mapping = aes(x = x$x)) + 
            geom_histogram(aes(x = x$x, y = ..density..), 
                bins = input$bins, fill = "lightgrey", colour = 1) +
            theme_bw() +
            labs(x = "x", y = "Count", title = "Histogram") +
            stat_function(fun = function(y) {
                dnbinom(floor(y), size = x$xest['size'], mu = x$xest['mu'])
            }, col = 2, n = 1000)
        
        ggbar <- ggplot(mapping = aes(x = x$x)) + 
            geom_bar(aes(x = x$x, y = ..prop..), 
                fill = "lightgrey", colour = 1) +
            theme_bw() +
            labs(x = "x", y = "Count", title = "Bar plot",
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
            stat_function(fun = function(y) {
                dnbinom(floor(y), size = x$xest['size'], mu = x$xest['mu'])
            }, col = 2, n = 1000)
        
        gghist / ggbar
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
