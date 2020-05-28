#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Finding Independence"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("PA",
                "P(A)",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.05),
            sliderInput("PB",
                "P(B)",
                min = 0,
                max = 1,
                value = 0.3,
                step = 0.05),
            sliderInput("PAB",
                "P(A and B)",
                min = 0,
                max = 1,
                value = 0.15,
                step = 0.0001),
            actionButton("force", "Force Independence")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        intermax <- min(input$PA, input$PB)
        intermin <- max(0, -1 + input$PA + input$PB)
        updateSliderInput(session, "PAB", min = intermin, max = intermax)
    })
    
    observe({
        input$force
        updateSliderInput(session, "PAB", value = input$PA*input$PB)
    })
    
    output$distPlot <- renderPlot({
        PA <- input$PA
        PB <- input$PB
        PAB <- input$PAB
        
        if(PA > 0 & PB > 0){
            # Area of the intersection of two circles
            # Given two circles C1 and C2 of radii r1 and r2 respectively (with r1≥r2) 
            # whose center points are at a distance d from each other, the intersection 
            # area of the circles is: 
            # A_intersection = r_1^2*acos(d_1/r_1) - d_1*sqrt(r_1^2 - d_1^2) + 
            #                  r_2^2*acos(d_2/r_2) - d_2*sqrt(r_2^2 - d_2^2)
            # where d_1 = (r_1^2 - r_2^2 + d^2)/(2*d) and d_2 = d - d_1
            
            # PA = pi*r_1^2 \implies r_1 = sqrt(PA/pi)
            r_1 <- sqrt(PA/pi)
            r_2 <- sqrt(PB/pi)
            
            calc_area <- function(r_1, r_2, d){
                d_1a <- (min(r_1, r_2)^2 - max(r_1,r_2)^2 + d^2)/(2*d)
                d_2a <- d - d_1a
                d_1 <- ifelse(r_1 < r_2, d_1a, d_2a)
                d_2 <- ifelse(r_1 < r_2, d_2a, d_1a)
                
                r_1^2*acos(d_1/r_1) - d_1*sqrt(r_1^2 - d_1^2) + 
                    r_2^2*acos(d_2/r_2) - d_2*sqrt(r_2^2 - d_2^2)
            }
            
            dseq <- seq(0, 1, 0.005)
            calcabunch <- suppressWarnings(
                sapply(dseq, function(x) calc_area(r_1, r_2, x))
            )
            c1 <- which.min(abs(PAB - calcabunch))
            d <- dseq[c1]
            
            x1a <- seq(-r_1, r_1, length.out = 100)
            y1a <-  sqrt(round(r_1^2 - x1a^2, 4))
            y1b <- -y1a
            
            x2a <- seq(d - r_2, d + r_2, length.out = 100)
            y2a <-  sqrt(round(r_2^2 - (x2a - d)^2, 4))
            y2b <- -y2a
            
            par(mar = c(0,0,3,0))
            layout(mat = matrix(c(1,2), ncol = 1), heights = c(2,1.5))
            if(PA == 0 | PB == 0){
                indtitle <- "Undefined"
            } else if(PAB == 0) {
                indtitle <- "Disjoint"
            } else if(round(PAB, 4) == round(PA*PB, 4)) {
                indtitle <- "INDEPENDENT!!!"
            } else {
                indtitle <- "Not Independent"
            }
            plot(x1a, y1a, type = "l", 
                xlim = c(min(-r_1, -r_2), max(r_1, r_2) + d), 
                ylim = c(min(-r_1, -r_2), max(r_1, r_2)), 
                asp = 1, xaxt = "n", yaxt = "n", bty = "n",
                xlab = "", ylab = "",
                main = indtitle)
            
            polygon(x = c(x1a, rev(x1a)), y = c(y1a, rev(y1b)), 
                col = rgb(1,0,0,0.2))
            polygon(x = c(x1a, rev(x1a)), y = c(y1a, rev(y1b)), 
                col = 2, density = 10, angle = 45, lty = 1)
            polygon(x = c(x2a, rev(x2a)), y = c(y2a, rev(y2b)), 
                col = rgb(0,0,1,0.2))
            polygon(x = c(x2a, rev(x2a)), y = c(y2a, rev(y2b)), 
                col = 4, density = 10, angle = -45)
            
            lines(x1a, y1a)
            lines(x1a, y1b)
            lines(x2a, y2a)
            lines(x2a, y2b)
            
            text(x = -r_1, y = r_1, labels = "P(A)", cex = 1.25, col = "red")
            text(x = d + r_2, y = r_2, labels = "P(B)", cex = 1.25, col = "blue")
            if(PAB != 0) {
                text(x = mean(c(r_1, d - r_2)), y = 0, labels = "P(A and B)", 
                    cex = 1.25, col = 1)
            }
            
            
            # Part two: slice over whole
            
            d1 <- (min(r_1, r_2)^2 - max(r_1,r_2)^2 + d^2)/(2*d)
            d2 <- d - d1
            
            x3a <- seq(d - r_2, ifelse(r_1 < r_2, d1, d2), length.out = 100)
            x3b <- seq(ifelse(r_1 < r_2, d1, d2), r_1, length.out = 100)
            y3a <-  sqrt(r_1^2 - x3b^2)
            y3b <-  sqrt(r_2^2 - (x3a - d)^2)
            
            
            # Cleaning up
            
            c1x <- c(x1a, rev(x1a))
            c1y <- c(y1a, rev(y1b))
            c2x <- c(x2a, rev(x2a))
            c2y <- c(y2a, rev(y2b))
            
            slicex <- c(x3a, x3b,  rev(x3b),  rev(x3a))
            slicey <- c(y3b, y3a, -rev(y3a), -rev(y3b))
            #plot(NULL, xlim = range(slicex), ylim = range(slicey))
            #polygon(x = slicex, y = slicey)
            
            x1just <- -3
            par(mar = c(0,0,0,0))
            plot(NULL, 
                xlim = c(-3,3), 
                ylim = c(-1,1), 
                asp = 1, xaxt = "n", yaxt = "n", bty = "n",
                xlab = "", ylab = "")
            # First intersection repositioning
            slicex2 <- slicex - median(slicex) + x1just - 0.2
            slicey2 <- slicey - min(slicey)
            polygon(x = slicex2, y = slicey2, 
                col = rgb(1,0,0,0.2), border = NA)
            polygon(x = slicex2, y = slicey2, 
                col = rgb(0,0,1,0.2), border = NA)
            polygon(x = slicex2, y = slicey2, 
                col = 2, density = 10, angle = 45, border = NA)
            polygon(x = slicex2, y = slicey2, 
                col = 4, density = 10, angle = -45, border = NA)
            lines(x = slicex2, y = slicey2)
            
            # first circle repositioning
            c1x2 <- c1x - median(c1x) + x1just + 0.1
            c1y2 <- c1y + min(c1y) + 0.05
            polygon(x = c1x2, y = c1y2, 
                col = rgb(1,0,0,0.2), border = NA)
            polygon(x = c1x2, y = c1y2, 
                col = 2, density = 10, angle = 45, border = NA)
            lines(x = c1x2, y = c1y2)
            
            # division line
            lines(x = x1just + c(-0.4,0.2), y = c(-0.15, 0.2), lwd = 2)
            # label
            lab1 <- paste0("= ", round(PAB/PA, 3), 
                ifelse(PAB == round(PA*PB, 4), " = ", " != "), "P(B)")
            text(x = x1just + 0.5, y = 0, labels = lab1, cex = 1.85, adj = 0)
            
            x2just <- 0
            # Second intersection repositioning
            slicex3 <- slicex - median(slicex) + x2just + 0.2
            slicey3 <- slicey - min(slicey)
            polygon(x = slicex3, y = slicey3, col = rgb(1,0,0,0.2), 
                border = NA)
            polygon(x = slicex3, y = slicey3, col = rgb(0,0,1,0.2), 
                border = NA)
            polygon(x = slicex3, y = slicey3, col = 2, 
                density = 10, angle = 45, border = NA)
            polygon(x = slicex3, y = slicey3, col = 4, 
                density = 10, angle = -45, border = NA)
            lines(x = slicex3, y = slicey3)
            
            c2x2 <- c2x - median(c2x) + x2just + 0.6
            c2y2 <- c2y + min(c2y) + 0.05
            polygon(x = c2x2, y = c2y2, col = rgb(0,0,1,0.2), 
                border = NA)
            polygon(x = c2x2, y = c2y2, col = 4, 
                density = 10, angle = -45, border = NA)
            lines(x = c2x2, y = c2y2)
            
            # division line
            lines(x = x2just + c(0,0.6), y = c(-0.15, 0.2), lwd = 2)
            # label
            lab1 <- paste0("= ", round(PAB/PB, 3), 
                ifelse(PAB == round(PA*PB, 4), " = ", " != "), "P(A)")
            text(x = x2just + 1, y = 0, labels = lab1, cex = 1.85, adj = 0)
            
            
            text(x = x1just+0.1, y = -0.25, labels = "P(A)", cex = 1.25, col = "red")
            text(x = x2just+0.6, y = -0.25, labels = "P(B)", cex = 1.25, col = "blue")
            text(x = x1just-0.2, y = 0.4, labels = "P(A and B)", cex = 1.25, col = 1)
            text(x = x2just+0.1, y = 0.4, labels = "P(A and B)", cex = 1.25, col = 1)
            
        } else if(PA == 0 & PB ==0){
            
            plot(NULL, ylim = c(-0.2, 0.2),
                xlim = c(-1,1), 
                asp = 1, xaxt = "n", yaxt = "n", bty = "n",
                xlab = "", ylab = "")
            text(0, 0, labels = "P(A) * P(B) = 0 = P(A and B), but\nP(A|B) = P(A and B) / P(B) is undefined.\nIndependence is defined as P(A|B) = P(A), so\nindependence is not defined here.", cex = 1.5)
            
        } else {
            
            r_1 <- sqrt(PA/pi)
            r_2 <- sqrt(PB/pi)
            lab2 <- c("P(A)", "P(B)")[which.max(c(r_1, r_2))]
            col1 <- c(2,4)[which.max(c(r_1, r_2))]
            col2 <- c(rgb(1,0,0,0.2),rgb(0,0,1,0.2))[which.max(c(r_1, r_2))]
            
            r_1 <- max(sqrt(PA/pi), sqrt(PB/pi))
            #r_2 <- min(sqrt(PA/pi), sqrt(PB/pi))
            
            x1a <- seq(-r_1, r_1, length.out = 100)
            y1a <-  sqrt(round(r_1^2 - x1a^2, 4))
            y1b <- -y1a
            c1x <- c(x1a, rev(x1a))
            c1y <- c(y1a, rev(y1b))
            
            par(mar = c(0,0,3,0))
            plot(c1x, c1y, type = 'l', ylim = range(c1y) + c(-0.5, 0),
                xlim = range(c1x), 
                asp = 1, xaxt = "n", yaxt = "n", bty = "n",
                xlab = "", ylab = "")
            polygon(c1x,c1y, col = col2)
            polygon(c1x,c1y, col = col1, density = 10, angle = 45)
            polygon(c1x,c1y)
            text(-r_1, r_1, labels = lab2, col = col1, cex = 1.25)
            
            if(r_2 == 0) {
            text(x = 0, y = -r_1 - 0.15,  cex = 1.5,
                labels = "P(A and B) / P(A) = 0") 
            text(x = 0, y = -r_1 - 0.25, cex = 1.5,
                labels = "P(A and B) / P(B) is undefined")
            } else {
                text(x = 0, y = -r_1 - 0.15, cex = 1.5, 
                    labels = "P(A and B) / P(A) is undefined") 
                text(x = 0, y = -r_1 - 0.25,  cex = 1.5,
                    labels = "P(A and B) / P(B) = 0")
            }
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
