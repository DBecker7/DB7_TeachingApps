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
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("lam",
                "lambda:",
                min = 0.01,
                max = 3,
                value = 1,
                step = 0.01),
            sliderInput("n",
                "n:",
                min = 1,
                max = 100,
                value = 5,
                step = 1,
                animate = list(interval = 500))
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
        
        bigxseq <- seq(0,20,0.1)
        bigyseq <- dexp(bigxseq, lam)
        maxx <- bigxseq[max(which(bigyseq/max(bigyseq) > 0.01))]
        
        xseq <- seq(0.01,maxx,0.01)
        expseq <- dexp(xseq, lam)
        igseq <- dgamma(xseq, shape = n, rate = n*lam)
        nseq <- dnorm(xseq, 1/lam, 1/sqrt(n*lam^2))
        
        distdf <- data.frame(x = c(xseq, xseq), y = c(igseq, nseq), 
            Distr. = rep(c("Actual Sampling Distr.", "Normal Approx."), each = length(xseq)))
        
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
