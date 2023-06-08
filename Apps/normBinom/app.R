# normBinom

library(shiny)
library(ggplot2)

ui <- fluidPage(

    

    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of trials",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("p",
                        "Probability of success",
                        min = 0,
                        max = 1,
                        value = 0.5)
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
        xseq <- 0:input$n
        yseq <- dbinom(xseq, input$n, input$p)
        names(yseq) <- xseq
        mu <- input$n*input$p
        sigma <- input$n*input$p*(1 - input$p)
        xseq2 <- seq(min(0, mu - 2.5*sigma), max(input$n, mu + 2.5*sigma), 
            length.out = 100*input$n)
        yseq2 <- dnorm(xseq2, input$n*input$p, 
            sqrt(input$n*input$p*(1-input$p)))
        
        
        
        ggplot() +
            geom_col(aes(x = xseq, y = yseq), 
                col = 1, fill = "lightgrey") +
            geom_line(aes(x = xseq2, y = yseq2), col = 3, size = 1) +
            theme_bw() + 
            scale_x_continuous(breaks = 0:input$n) +
            labs(x = "x", y = "Probability",
                title = "Normal Approximation to Binomial",
                subtitle = paste0("n*p=", input$n*input$p, 
                    ", n*p*(1-p)=", input$n*(1-input$p)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
