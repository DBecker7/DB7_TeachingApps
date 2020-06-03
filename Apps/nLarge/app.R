# nLarge

library(shiny)
library(ggplot2)
library(patchwork)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How large must n be?"),
    tags$style(
        ".irs-bar {",
        "  border-color: transparent;",
        "  background-color: transparent;",
        "}",
        ".irs-bar-edge {",
        "  border-color: transparent;",
        "  background-color: transparent;",
        "}"
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("lam",
                "lambda:",
                min = 0.01,
                max = 5,
                value = 1,
                step = 0.01),
            sliderInput("n",
                "n:",
                min = 1,
                max = 100,
                value = 5,
                step = 1,
                animate = list(interval = 500)),
            checkboxInput("equalx", "Same x values?", value = TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML('Some questions:
<ol><li>Why does this use the exponential distribution, rather than something closer to the normal distribution?</li>
    <li>How large must n be before the normal approximation seems "reasonable"?</li>
    <li>Comment on the bias of the normal approximation.</li>
</ol>'))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        lam <- input$lam
        n <- input$n
        
        # Find reasonable x values
        maxx <- qexp(0.99, lam)
        
        xseq <- seq(0.01, maxx, length.out = 200)
        expseq <- dexp(xseq, lam)
        
        if(input$equalx){
            igseq <- dgamma(xseq, shape = n, rate = n*lam)
            nseq <- dnorm(xseq, 1/lam, 1/sqrt(n*lam^2))
            distdf <- data.frame(x = c(xseq, xseq), y = c(igseq, nseq), 
                Distr. = rep(c("Actual Sampling Distr.", 
                    "Normal Approximation"), 
                    each = length(xseq)))
        } else {
            minx <- min(qgamma(0.01, shape = n, rate = n*lam), 
                qnorm(0.01, 1/lam, 1/sqrt(n*lam^2)))
            maxx <- max(qgamma(0.99, shape = n, rate = n*lam), 
                qnorm(0.99, 1/lam, 1/sqrt(n*lam^2)))
            
            xseq2 <- seq(minx, maxx, length.out = 200)
            igseq <- dgamma(xseq2, shape = n, rate = n*lam)
            nseq <- dnorm(xseq2, 1/lam, 1/sqrt(n*lam^2))
            distdf <- data.frame(x = c(xseq2, xseq2), y = c(igseq, nseq), 
                Distr. = rep(c("Actual Sampling Distr.", 
                    "Normal Approximation"), 
                    each = length(xseq2)))
        }
        
        pval_gamma <- pgamma((1/lam - 1.96*1/sqrt(n*lam^2)), n, n*lam) +
            pgamma((1/lam + 1.96*1/sqrt(n*lam^2)), n, n*lam, 
                lower.tail = FALSE)
        
        subtit <- paste0("p_val from Normal approx = 0.05 \n", 
            "p-val from actual sampling distr. = ", round(pval_gamma, 4))
        
        popdist <- ggplot() + 
            geom_line(aes(x = xseq, y = expseq)) +
            theme_bw() +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            labs(x = "Population Values", y = "Pop. Distribution")
        samplingdist <- ggplot(distdf) + 
            geom_line(aes(x = x, y = y, colour = Distr.), size = 1) +
            theme_bw() +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14), 
                legend.text = element_text(size = 12), 
                legend.position = "right") +
            labs(x = "All Possible Means", y = "Sampling Distribution",
                subtitle = subtit,
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps")
        
        popdist / samplingdist
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
