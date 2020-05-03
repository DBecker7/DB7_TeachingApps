# GausField_Matern

library(RandomFields)
library(ggplot2)
library(shiny)
myseed <- 2112
RFoptions(seed = myseed)

ui <- fluidPage(
    
    titlePanel("Parameters in a Gaussian Field"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("nu",
                "nu",
                min = 0.1,
                max = 5,
                value = 1,
                step = 0.1, animate = list(interval = 900)),
            sliderInput("var",
                "var",
                min = 0.1,
                max = 5,
                value = 1,
                step = 0.1, animate = list(interval = 900)),
            sliderInput("scale",
                "scale",
                min = 0.1,
                max = 5,
                value = 1,
                step = 0.1, animate = list(interval = 900)),
            actionButton("doit", "Click me for new data")
        ),
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    
    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
        RFoptions(seed = myseed)
    })
    
    output$distPlot <- renderPlot({
        newseed() #input$doit
        x <- y <- seq(-10, 10, 0.5)
        
        model <- RMmatern(nu = input$nu, var = input$var, scale = input$scale)
        simu <- RFsimulate(model, x, y, grid=TRUE)
        
        xy <- expand.grid(x = x, y = y)
        xy$field <- as.vector(simu)
        
        lims <- c(floor(min(xy$field, -8)), ceiling(max(xy$field, 8)))
        
        ggplot(xy, aes(x = x, y = y, fill = field)) + 
            geom_tile() +
            scale_fill_viridis_c(limits = lims) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
