library(shiny)
library(TSA)

input <- list("r1" = 0.5, "r2" = 0, "r3" = 0, "r4" = 0,
    "r5" = 0, "r6" = 0, n = 50, s = 1)

ui <- fluidPage(
    sidebarPanel(
        actionButton("new_data", "New Data"),
        sliderInput("n", "Number of Obs", 20, 300, 50, 5),
        sliderInput("s", "Residual Variance", 0.1, 5, 1, 0.1),
        sliderInput("r1", "Lag 1 correlation", -1, 1, 0.5, 0.1),
        sliderInput("r2", "Lag 2 correlation", -1, 1, 0, 0.1),
        sliderInput("r3", "Lag 3 correlation", -1, 1, 0, 0.1),
        sliderInput("r4", "Lag 4 correlation", -1, 1, 0, 0.1),
        sliderInput("r5", "Lag 5 correlation", -1, 1, 0, 0.1),
        sliderInput("r6", "Lag 6 correlation", -1, 1, 0, 0.1),
    ),
    mainPanel(plotOutput("plot"), )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        dummy <- input$new_data
        es <- rep(0, 6)
        rho <- c(input$r1, input$r2, input$r3,
            input$r4, input$r5, input$r6)
        for (i in 7:(input$n + 6)) {
            es <- c(es,
                es[(i - 6):(i - 1)] %*% rev(rho) +
                    rnorm(1, 0, input$s))
        }
        es <- es[7:(input$n + 6)]
        par(mfrow = c(2, 2))
        plot(es, main = "Residuals",
            xlab = "Equally spaced time point",
            ylab = "Residual")
        acf(es, main = "ACF")
        pacf(es, main = "PACF")

        runtest <- runs(es)
        n1 <- runtest$n1
        n2 <- runtest$n2
        mu <- 2 * n1 * n2/(n1 + n2) + 1
        s2 <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) /
            ((n1 + n2)^2 * (n1 + n2 - 1))
        xseq <- round(seq(mu - 3 * sqrt(s2), mu + 3 * sqrt(s2),
            length.out = 200), 0)
        yseq <- dnorm(xseq, mu - 0.5, sqrt(s2))
        plot(xseq, yseq, type = "s",
            xlim = range(c(xseq, runtest$observed.runs)),
            main = "Runs Test (Normal Approx. with 95% CI)",
            xlab = "Number of runs", ylab = "Density")
        abline(v = mu, col = "forestgreen", lwd = 3)
        abline(v = runtest$observed.runs, col = "firebrick", lwd = 3)
        crit_vals <- qnorm(c(0.025, 0.975), mu, sqrt(s2))
        crit_heights <- dnorm(crit_vals, mu, sqrt(s2))
        lines(x = rep(crit_vals[1], 2),
            y = c(0, crit_heights[1]),
            col = "firebrick")
        lines(x = rep(crit_vals[2], 2),
            y = c(0, crit_heights[2]),
            col = "firebrick")
    })
}

shinyApp(ui = ui, server = server)
