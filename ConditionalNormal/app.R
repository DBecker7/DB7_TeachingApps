#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(mvtnorm)
library(shiny)
library(plot3D)

myx <- seq(-4,4,0.2)
mymat <- expand.grid(x = myx, y = myx)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Conditional Distributions from Multivariate Normal"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                        "x",
                        min = -4,
                        max = 4,
                        value = 2, step = 0.5),
            sliderInput("y",
                        "y",
                        min = -4,
                        max = 4,
                        value = -2, step = 0.5),
            sliderInput("rho",
                        "correlation",
                        min = 0,
                        max = 1,
                        value = 0.6, step = 0.05),
            sliderInput("phi",
                        "Rotate vertically",
                        min = 0,
                        max = 90,
                        value = 45, step = 5),
            sliderInput("theta",
                        "Rotate laterally",
                        min = 0,
                        max = 360,
                        value = 45, step = 7.5, animate = list(interval = 500, loop = TRUE))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    z2 <- reactive({
        
        sigma <- matrix(c(2, input$rho*2, input$rho*2, 2), ncol = 2)
        z <- dmvnorm(mymat, sigma = sigma)
        matrix(z, ncol = length(myx), byrow = FALSE)
    })

    output$distPlot <- renderPlot({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        
        
        
        z2 <- z2()
        
        layout(mat = matrix(c(1,1,2,3), ncol = 2))
        par(mar = c(5.1,2,2,1), cex.axis = 1.5, cex.lab=1.5, cex.main=1.5)
        persp3D(myx, myx, z2, col = terrain.colors(30), 
            phi = input$phi, theta = input$theta, colkey = FALSE, border = NA)
        points3D(x = myx[ind] + myx*0.001, y = myx, z2[ind,]+0.001, 
            type = "l", add = TRUE, col = "darkorchid", lwd = 4)
        points3D(y = myx[indy] + myx*0.001, x = myx, z2[indy,]+0.001, 
            type = "l", add = TRUE, col = "blue", lwd = 4)
        
        plot(myx, z2[ind,], type = 'l', lwd = 2, col = "darkorchid", ylim = c(0, max(z)),
            xlab = "y", main = paste0("y | x=", myx[ind]))
        plot(myx, z2[,indy], type = 'l', lwd = 2, col = "blue", ylim = c(0, max(z)),
            xlab = "x", main = paste0("x | y=", myx[indy]))
        mtext("Created by Devan Becker", 
            side = 1, line = 3, adj = 1, cex = 0.8)
        mtext("Github: DBecker7/DB7_TeachingApps", 
            side = 1, line = 4, adj = 1, cex = 0.8)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
