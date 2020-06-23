# robustness

library(ggplot2)
library(ggtext)
library(shiny)


ui <- fluidPage(
    
    
    titlePanel("Robustness of Median and IQR"),
    
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("outlier",
                "Outlier Value",
                min = 1,
                max = 60,
                value = 30),
            sliderInput("n",
                "Sample size",
                min = 5,
                max = 60,
                value = 20),
            actionButton("doit", "New Data")
        ),
        
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {
    
    newdata <- reactive({
        input$doit
        round(runif(input$n - 1, 0, 20))
    })
    
    output$distPlot <- renderPlot({
        x <- newdata()
        mycolour <- rep("B", length(x))
        x <- c(x, input$outlier)
        mycolour <- c(mycolour, "A")
        
        mytitle <- paste0("<span style = 'color: red'>Mean = ", round(mean(x), 2), 
            "</span>, <span style = 'color: blue'>Median = ", median(x), "</span>, Var = ", round(var(x), 2),
            ", IQR = ", IQR(x))
        
        ggplot() + 
            geom_histogram(aes(x = x, fill = mycolour), 
                binwidth = 1, boundary = -0.5, colour = "#807F83") +
            theme_bw() +
            scale_fill_manual(values = c(2, "lightgrey")) +
            theme(legend.position = "none", plot.title = element_textbox()) +
            labs(x = "x", y = "Count", title = mytitle) +
            geom_vline(xintercept = mean(x), colour = "red", size = 1)+
            geom_vline(xintercept = median(x), colour = "blue", size = 1)
    })
}


shinyApp(ui = ui, server = server)
