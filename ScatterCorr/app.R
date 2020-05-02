#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(MASS)
library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("yeti"),
    
    # Application title
    titlePanel("Scatterplots with fixed correlation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "Number of samples",
                min = 5,
                max = 500,
                value = 50),
            sliderInput("r",
                "Squared Correlation (r^2)",
                min = 0,
                max = 1,
                value = 0,
                step = 0.05),
            sliderInput("a",
                "Intercept",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1),
            sliderInput("b",
                "Slope",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1),
            actionButton(inputId = "doit", label = "Click me for new data")
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
        dummy <- input$doit
        n <- input$n
        r <- input$r
        # Shamelessly stolen from https://stats.stackexchange.com/questions/83172/generate-two-variables-with-precise-pre-specified-correlation
        xy = mvrnorm(n = n, mu = c(0, 0), 
            Sigma = matrix(c(1, r, r, 1), nrow = 2), 
            empirical=TRUE)
        x = xy[, 1] 
        y = input$a + (input$b + 0.001)*xy[, 2] 
        ggplot(mapping = aes(x = x, y = y)) + geom_point() + 
            geom_smooth(method = "lm", formula = y ~ x) +
            theme_bw() + 
            labs(title = paste0("Scatterplot with n = ", n, 
                " and r = ", round(cor(x,y), 3)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
