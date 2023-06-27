library(shiny)

beta <- runif(15, -5, 5)

ui <- fluidPage(
    sidebarPanel(
        sliderInput("s", "Sample Size", 10, 150, 100, 1),
        sliderInput("n_true", "True polynomial order", 0, 15, 2, 1),
        sliderInput("n", "Estimated polynomial order", 0, 15, 2, 1),
        sliderInput("sigma", "Variance", 0.5, 30, 2, 0.5),
        actionButton("newfit", "New Parameters"),
        actionButton("newdata", "New Data, Same Fit"),
        actionButton("newmodel", "Fit to current data")
    ),
    mainPanel(plotOutput("plot"), )
)

server <- function(input, output) {
    new_data <- reactive({
        input$newdata
        new_fit()
        x <<- runif(input$s, -7, 7)
        X <- x
        for(i in 2:15) X <- cbind(X, x^i)
        y <<- X[, 1:input$n_true, drop = FALSE] %*% beta[1:input$n_true] +
            rnorm(input$s, 0, input$sigma)
    })

    new_model <- reactive({
        input$newmodel
        mylm <<- lm(y ~ poly(x, input$n))
    })

    new_fit <- reactive({
        input$newfit
        beta <<- runif(15, -5, 5) / ((1:15)^(1:15 / 2))
    })

    output$plot <- renderPlot({
        new_data()
        new_model()
        new_fit()
        plot(y ~ x)

        xseq <- seq(-10, 10, 0.2)
        yseq <- predict(mylm, newdata = list(x = xseq))
        lines(xseq, yseq)

        X2 <- xseq
        for (i in 2:15) X2 <- cbind(X2, xseq^i)
        yseq2 <<- X2[, 1:input$n_true, drop = FALSE] %*% beta[1:input$n_true]
        lines(xseq, yseq2, col = "grey", lty = 2)
    })
}

shinyApp(ui = ui, server = server)
