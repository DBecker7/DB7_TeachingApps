#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

xlims <- c(-3, 3)
ylims <- c(0, dnorm(0, 0, 1))
lastones <- list()
greycols <- rev(rgb(0,0,0,((0:5)/8)^2))

# Define UI for application that draws a histogram
ui <- fluidPage(


    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mu", "Mean μ", -5, 5, 0, 0.1),
            sliderInput("sig", "Standard Deviation σ", 0.1, 5, 1, 0.1),
            actionButton("doit", "Reset Plots"),
            checkboxInput("stick", "Sticky Axes?", TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    resetLims <- reactive({
        input$diot
        xlims <<- c(-3, 3)
        ylims <<- c(0, dnorm(0,0,1))
    })

    output$distPlot <- renderPlot({
        mu <- input$mu
        sig <- input$sig
        resetLims()
        
        xseq <- seq(mu-3*sig, mu + 3*sig, length.out = 300)
        yseq <- dnorm(xseq, mu, sig) # similar to how we used dbinom
        
        lastones <<- c(lastones, list(data.frame(x = xseq, y = yseq)))
        nl <- length(lastones)
        if(nl > 6) lastones <- lastones[(nl-6):nl]
        
        
        xlims <<- c(min(mu - 3*sig, xlims), max(mu + 3*sig, xlims))
        ylims <<- c(0, max(ylims, dnorm(mu, mu, sig)))
        
        if(input$stick){
            xlim2 <- xlims
            ylim2 <- ylims
        } else {
            xlim2 <- c(mu-3*sig, mu + 3*sig)
            ylim2 <- c(0, dnorm(mu,mu,sig))
        }
        
        plot(xseq, yseq, type = "l", col = 4,
            xlim = xlim2, ylim = ylim2, lwd = 2,
            xlab = "x", ylab = "Probability", 
            main = bquote("X~N("*mu*","*sigma*")"))
        for(i in 1:length(lastones)){
            lines(lastones[[i]]$x, lastones[[i]]$y, col = greycols[i])
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
