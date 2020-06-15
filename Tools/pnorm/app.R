# pnorm

library(shiny)
library(ggplot2)

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
    titlePanel("Normal Probabilities"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("lohi",
                "Z values (-4 for -Inf)",
                min = -4,
                max = 4,
                value = c(-4, 1.96),
                step = 0.01)
        ),
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        lo <- input$lohi[1]
        hi <- input$lohi[2]
        
        xseq <- seq(-4,4,0.01)
        yseq <- dnorm(xseq)
        
        xrib <- seq(lo, hi, 0.01)
        yrib <- dnorm(xrib)
        
        if(lo > -4 & hi < 4){
            mytitle <- paste0("pnorm(", hi, ") - pnorm(",lo,") = ", 
                round(pnorm(hi) - pnorm(lo), 4))
            addtox <- c(lo, hi)
            arrows <- list(
                annotate("segment", x = c(lo, hi), y = c(-0.05, -0.1),  size = 0.8, 
                    colour = "darkorchid",
                    xend = c(-4,-4), yend = c(-0.05, -0.1), arrow = arrow()),
                annotate("text", x = mean(c(lo, -4)), y = -0.05, vjust = -0.5, 
                    label = paste0("pnorm(", lo, ")=", round(pnorm(lo), 3))),
                annotate("text", x = mean(c(hi, -4)), y = -0.1, vjust = -0.5, 
                    label = paste0("pnorm(", hi, ")=", round(pnorm(hi), 3)))
            )
        } else if(hi < 4){
            mytitle <- paste0("pnorm(", hi, ") = ", round(pnorm(hi), 4))
            addtox <- hi
            ydouble <- c(0,0)
            arrows <- list(
                annotate("segment", x = hi, y = -0.05,  size = 0.8, 
                    colour = "darkorchid",
                    xend = -4, yend = -0.05, arrow = arrow()),
                annotate("text", x = mean(c(hi, -4)), y = -0.05, vjust = -0.5, 
                    label = paste0("pnorm(", hi, ")=", round(pnorm(hi), 3)))
            )
        } else {
            mytitle <- paste0("1 - pnorm(", lo, ") = ", 
                round(1 - pnorm(lo), 4))
            addtox <- lo
            ydouble <- c(0,0)
            arrows <- list(
                annotate("segment", x = lo, y = -0.05, size = 0.8, 
                    colour = "darkorchid",
                    xend = -4, yend = -0.05, arrow = arrow()),
                annotate("text", x = mean(c(lo, -4)), y = -0.05, vjust = -0.5, 
                    label = paste0("pnorm(", lo, ")=", round(pnorm(lo), 3)))
            )
        }
        
        ggplot() + 
            geom_ribbon(aes(xmin = xrib, x=xrib, ymin = 0, ymax = yrib),
                fill = "darkorchid", alpha = 0.4) +
            geom_line(aes(x = xseq, y = yseq), size = 1) +
            scale_x_continuous(breaks = c(seq(-4,4,2))) +
            theme_bw() + 
            annotate("text", x = addtox, y = rep(-0.01, length(addtox)), 
                label = addtox) + 
            labs(x = "x", y = "dnorm(x)", title = mytitle,
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") + 
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            arrows
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
