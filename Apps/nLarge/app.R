#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(patchwork)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How large must n be?"),
    
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
            plotOutput("distPlot")
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
                Distr. = rep(c("Actual Sampling Distribution", "Normal Approximation"), each = length(xseq)))
        } else {
            minx <- min(qgamma(0.01, shape = n, rate = n*lam), qnorm(0.01, 1/lam, 1/sqrt(n*lam^2)))
            maxx <- max(qgamma(0.99, shape = n, rate = n*lam), qnorm(0.99, 1/lam, 1/sqrt(n*lam^2)))
            
            xseq2 <- seq(minx, maxx, length.out = 200)
            igseq <- dgamma(xseq2, shape = n, rate = n*lam)
            nseq <- dnorm(xseq2, 1/lam, 1/sqrt(n*lam^2))
            distdf <- data.frame(x = c(xseq2, xseq2), y = c(igseq, nseq), 
                Distr. = rep(c("Actual Sampling Distribution", "Normal Approximation"), each = length(xseq2)))
        }
        
        popdist <- ggplot() + 
            geom_line(aes(x = xseq, y = expseq)) +
            theme_bw() +
            theme(title = element_text(size = 16), axis.text = element_text(size = 14)) +
            labs(x = "Population Values", y = "Pop. Distribution")
        samplingdist <- ggplot(distdf) + 
            geom_line(aes(x = x, y = y, colour = Distr.), size = 1) +
            theme_bw() +
            theme(title = element_text(size = 16), axis.text = element_text(size = 14), 
                legend.text = element_text(size = 12)) +
            labs(x = "All Possible Means", y = "Sampling Distribution",
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps")
        
        popdist / samplingdist
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
