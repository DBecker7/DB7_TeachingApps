# PoisBinApprox
# Approximations to the Binomial Distribution


# Make sure these packages are installed first.
library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(

    # Application title
    titlePanel("Poisson Approximation to the Binomial Distribution"),
 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Sample Size",
                        min = 5,
                        max = 5000,
                        step = 5,
                        value = 30, animate = list(interval = 900)),
            "The probability slider is on the log scale so it's easier to select small probabilities. The actual probability is shown in the plot title.",
            sliderInput("p",
                        "log10(Probability)",
                        min = -5,
                        max = -0.05,
                        step = 0.05,
                        value = -1, animate = list(interval = 900)),
            "Pro Tip: There's a 'play' button on the slider!",
            checkboxInput("addnorm", "Add the Normal approximation?")
        ),

        
        mainPanel(
           plotOutput("distPlot", height = "500px"),
            tags$div(HTML("Some questions:<br><ul>
	<li>A common rule of thumb is that you can use the Poisson approximation for p < 0.05. Does this seem reasonable?</li>
	<li>A common rule of thumb is that you can use the Normal approximation when both np > 10 and n(1-p) > 10. Does this seem reasonable? </li>
	<li>At what value of p is the Normal distribution a better approximation than the Poisson?</li>
	<li>Why is it easier to use the Poisson approximation when n is large? Make reference to the pdf of both distributions.</li>
</ul>"))
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        n <- input$n
        p <- 10^(input$p)
        xseq <- seq(0, n, 1)
        plotlims <- c(max(0, floor(n*p - 4*sqrt(n*p*(1-p)))), 
            min(n, ceiling(n*p + 4*sqrt(n*p*(1-p)))))
        bindf <- data.frame(x = xseq, y = dbinom(xseq, n, p), 
            Model = "Binomial", 
            stringsAsFactors = FALSE)
        poisdf <- data.frame(x = xseq, y = dpois(xseq, n*p), 
            Model = "Poisson", 
            stringsAsFactors = FALSE)
        normdf <- data.frame(x = xseq, y = dnorm(xseq, n*p, sqrt(n*p*(1-p))), 
            Model = "Normal", 
            stringsAsFactors = FALSE)
        ggdf <- bind_rows(bindf, poisdf)
        
        normmse <- sum((bindf$y - normdf$y)^2)
        poismse <- sum((bindf$y - poisdf$y)^2)
        mytitle <- paste0("Sum of Squared Error (Poisson) = ", 
            round(poismse, 7))
        if(input$addnorm) mytitle <- paste0(mytitle, 
            "; SSE(Normal) = ", round(normmse, 7))
        
        ggplot(data = ggdf, mapping = aes(x = x, y = y, fill = Model, colour = Model)) + 
            geom_col(position = "identity", alpha = 0.5) +
            scale_fill_manual(values = c("#7570b3", "#66a61e")) +
            scale_colour_manual(values = c("#7570b3", "#66a61e")) +
            theme_bw() +
            coord_cartesian(xlim = plotlims) + 
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14), 
                legend.text = element_text(size = 14))  +
            labs(x = "x", y = "f(x)", 
                title = mytitle,
                subtitle = paste0("n = ", n, ", p = ", round(p, 7)),
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
            theme(legend.position = "bottom") +
            if(input$addnorm) stat_function(
                fun = function(x) dnorm(x, n*p, sqrt(n*p*(1-p))), 
                n = 1000, xlim = plotlims, colour = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
