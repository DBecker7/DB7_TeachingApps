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
    
    # Application title
    titlePanel("Power for Simple Null and Simple Alternative"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha",
                "Significance Level:",
                min = 0.001,
                max = 0.1,
                value = 0.05,
                step = 0.001),
            sliderInput("mu1",
                "Alternate hypothesis:",
                min = 0,
                max = 5,
                value = 4,
                step = 0.1),
            sliderInput("sd0",
                "Standard deviation of population:",
                min = 0.5,
                max = 50,
                value = 6,
                step = 0.5),
            sliderInput("n",
                "Sample size:",
                min = 1,
                max = 50,
                value = 36,
                step = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("Some questions:
<ol><li>Briefly describe how each slider affects the power.</li>
</ol>"))
            )
        )
    )
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
        
        output$distPlot <- renderPlot({
            alpha <- input$alpha
            
            
            mu0 <- 0
            mu1 <- input$mu1
            sd0 <- round(input$sd0/sqrt(input$n), 2)
            
            zobs <- round(qnorm(1 - alpha, mu0, sd0), 3)
            
            xtotal <- seq(
                from = min(mu0 - pi*sd0, mu1 - pi*sd0),
                to = max(mu0 + pi*sd0, mu1 + pi*sd0),
                by = 0.05
            )
            
            y1 <- dnorm(xtotal, mu0, sd0)
            y2 <- dnorm(xtotal, mu1, sd0)
            
            xrib <- xtotal[xtotal >= zobs]
            yrib <- dnorm(xrib, mu1, sd0)
            yrib2 <- dnorm(xrib, mu0, sd0)
            
            mytitle <- paste0("Power = 1 - pnorm(", zobs, 
                ", ", mu1, ", ", sd0, ") = ", 
                round(1 - pnorm(zobs, mu1, sd0), 3))
            mysub <- paste0("Critical value is qnorm(1-", 
                alpha, ", ", mu0, ", ", sd0, ") = ", zobs)
            
            ggplot() + 
                geom_ribbon(aes(x = xrib, ymin = 0, ymax = yrib),
                    fill = "forestgreen", alpha = 0.3, colour = 1) +
                geom_ribbon(aes(x = xrib, ymin = 0, ymax = yrib2),
                    fill = "darkorchid", alpha = 0.3, colour = 1) +
                geom_line(aes(x = xtotal, y = y1),
                    colour = "darkorchid", size = 1) +
                geom_line(aes(x = xtotal, y = y2),
                    colour = "forestgreen", size = 1) +
                theme_bw() +
                labs(x = "Mean", y = "Sampling Distr. of Means",
                    title = mytitle, subtitle = mysub,
                    caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
                annotate("text", x = mu0 - 2.25*sd0, y = dnorm(mu0 - 1*sd0, mu0, sd0), 
                    label = "Sampling \ndistribution\nunder H0", hjust = 0, vjust = 0, 
                    size = 5, colour = "darkorchid") + 
                annotate("text", x = mu1 + 1.25*sd0, y = dnorm(mu1 + 1*sd0, mu1, sd0), 
                    label = "Sampling \ndistribution\nunder H1", hjust = 0, vjust = 0, 
                    size = 5, colour = "forestgreen") +
                annotate("text", x = zobs, y = 0, label = zobs, 
                    vjust = 1, size = 4) + 
                theme(title = element_text(size = 16), 
                    axis.text = element_text(size = 14)) 
        })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    