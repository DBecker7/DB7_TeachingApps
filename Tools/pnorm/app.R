# pnorm

library(shiny)
library(ggplot2)

ui <- fluidPage(
    
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
        
        mytitle <- if(lo > -4 & hi < 4){
            paste0("pnorm(", hi, ") - pnorm(",lo,") = ", round(pnorm(hi) - pnorm(lo), 4))
        } else if(hi < 4){
            paste0("pnorm(", hi, ") = ", round(pnorm(hi), 4))
        } else {
            paste0("1 - pnorm(", lo, ") = ", round(1 - pnorm(lo), 4))
        }
        
        ggplot() + 
            geom_ribbon(aes(xmin = xrib, x=xrib, ymin = 0, ymax = yrib),
                fill = "darkorchid", alpha = 0.4) +
            geom_line(aes(x = xseq, y = yseq), size = 1) +
            scale_x_continuous(breaks = c(seq(-4,4,2), lo, hi)) +
            theme_bw() + 
            labs(x = "x", y = "dnorm(x)", title = mytitle,
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") + 
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
