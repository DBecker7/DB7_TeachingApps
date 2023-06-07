library(shiny)

ui <- fluidPage(
    sidebarPanel(
        sliderInput("n", "Number of trials", 1, 100, 5,
            animate = list(interval = 500)),
        sliderInput("p", "Prob. of success", 0, 1, 0.5,
            animate = list(interval = 500)),
        checkboxInput("norm", "Draw Normal Approximation?", FALSE)
    ),
    mainPanel(plotOutput("plot"), )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        mu <- input$n * input$p
        sigma <- sqrt(mu * (1 - input$p))
        dseq <- seq(0, input$n, 1)
        cseq <- seq(mu - 3*sigma, mu + 3*sigma, 0.001)
        normseq <- dnorm(cseq, mu, sigma)
        binomseq <- dbinom(dseq, size = input$n, prob = input$p)
        plot(dseq - 0.5, binomseq, type = "h")
        lines(dseq - 0.5, binomseq, type = "s")
        if (input$norm)
            lines(cseq, normseq, col = 2)
    })
}

shinyApp(ui = ui, server = server)
