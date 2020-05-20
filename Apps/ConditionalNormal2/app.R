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
library(ggplot2)
library(plot3D)
library(rgl)
library(shinyRGL)
library(MASS)

library(mvtnorm)


myx <- seq(-4,4,0.2)
mymat <- expand.grid(x = myx, y = myx)
sigma <- matrix(c(2,1,1,2), ncol = 2)
z <- dmvnorm(mymat, sigma = sigma)
z2 <- matrix(z, ncol = length(myx), byrow = FALSE)
cols <- cut(z2, breaks = 20, labels = FALSE)

myx <- seq(-4,4,0.2)
mymat <- expand.grid(x = myx, y = myx)

userMatrix <- matrix(c(1,0,0,0,0,0.34,-0.94,0,0,0.94,0.34,0,0,0,0,1), ncol = 4)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
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
                value = 0.6, step = 0.05)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            rglwidgetOutput("distPlot", height = "400px"),
            plotOutput("plot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    
    z2 <- reactive({
        sigma <- matrix(c(2, input$rho*2, input$rho*2, 2), ncol = 2)
        z <- dmvnorm(mymat, sigma = sigma)
        z2 <- matrix(z, ncol = length(myx), byrow = FALSE)
    })
    
    scene1 <- reactive({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        z2 <- z2()
        
        open3d(userMatrix = userMatrix)
        par3d(userMatrix = userMatrix)
        persp3d(myx, myx, z2, col = terrain.colors(max(cols))[cols])
        lines3d(x = myx[ind], y = myx, z = z2[ind,], lwd = 3, col = "blue")
        lines3d(x = myx, y = myx[indy], z = z2[,indy], lwd = 3, col = "darkorchid")
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        par3d(userMatrix = userMatrix)
        scene3d()
    })
    
    output$distPlot <- renderRglwidget({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        z2 <- z2()
        par3d(userMatrix = userMatrix)
        scene1 <- scene1()
        par3d(userMatrix = userMatrix)
        userMatrix <<- par3d()$userMatrix
        rgl.close()
        rglwidget(scene1)
    })
    
    output$plot2 <- renderPlot({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        z2 <- z2()
        
        par(mfrow = c(1,2))
        plot(myx, z2[ind,], type = 'l', lwd = 2, col = "darkorchid", ylim = c(0, max(z)),
            xlab = "y", main = paste0("y | x=", myx[ind]))
        plot(myx, z2[,indy], type = 'l', lwd = 2, col = "blue", ylim = c(0, max(z)),
            xlab = "x", main = paste0("x | y=", myx[indy]))
        mtext("Created by Devan Becker", 
            side = 1, line = 3, adj = 1, cex = 0.8)
        mtext("Github: DBecker7/DB7_TeachingApps", 
            side = 1, line = 4, adj = 1, cex = 0.8)
        # TODO: Correct axes labels, maybe qplot instead?
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
