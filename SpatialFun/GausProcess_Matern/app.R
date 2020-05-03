#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

myseed <- 2112
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Exploration of the parameters in a Gaussian Process"), 
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("range",
                "Range of Correlation:",
                min = 0.1,
                max = 5,
                value = 1,
                step = 0.1, animate = list(interval = 900)),
            sliderInput("smooth",
                "Smoothness:",
                min = 0.1,
                max = 5,
                value = 1, step = 0.1, animate = list(interval = 900)),
            sliderInput("phi",
                "phi:",
                min = 0.1,
                max = 5,
                value = 1, step = 0.1, animate = list(interval = 900)),
            sliderInput("max",
                "Max x value:",
                min = 0.1,
                max = 5,
                value = 1, step = 0.1),
            sliderInput("nx",
                "Number of x points:",
                min = 20,
                max = 300,
                value = 100, step = 5),
            checkboxInput("equalspace", "Equally spaced x values?", 
                value = TRUE),
            actionButton("newseed", "Generate new data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            "The same random seed is used when generating processes until you generate new data, so changing parameters shouldn't change the shape too much. However, GPs are not concerned with what they should do, so sometimes this happens."
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    simGP <- reactive({
        dummy <- input$newseed
        set.seed(myseed)
        if(input$equalspace){
            x <<- input$max*(1:input$nx)/input$nx
        } else {
            x <<- sort(runif(input$nx, 0, input$max))
        }
        covx <- outer(x, x, 
            function (x, y) fields::Matern(abs(x-y), 
                range = input$range, smoothness = input$smooth, phi = input$phi))
        MASS::mvrnorm(n = 1, mu = rep(0, length(x)), covx)
    })
    
    newseed <- reactive({
        dummy <- input$newseed
        myseed <<- round(runif(1, 0, 10000))
    })
    
    output$distPlot <- renderPlot({
        #x <- input$max*(1:100)/100
        y <- simGP()
        dummy <- newseed()
        lowlim <- floor(min(y))
        highlim <- ceiling(max(y))
        ggplot() + geom_point(aes(x = x, y = y)) + 
            geom_line(aes(x = x, y = y)) +
            theme_bw() +
            labs(title = "Realization of a Gaussian Process") +
            coord_cartesian(ylim = c(lowlim, highlim))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
