# pairedt

library(shiny)
library(ggplot2)
library(patchwork)

ui <- fluidPage(
    
    titlePanel("Paired versus unpaired t-tests"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "Sample size (same for both)",
                min = 2,
                max = 100,
                value = 30),
            sliderInput("mu1",
                "Mean of group 1",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1),
            sliderInput("sd1",
                "SD of group 1",
                min = 0.05,
                max = 5,
                value = 1,
                step = 0.05),
            sliderInput("mu2",
                "Mean of group 2",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1),
            sliderInput("sd2",
                "SD of group 2",
                min = 0.05,
                max = 5,
                value = 1,
                step = 0.05),
            actionButton("doit", "Click me for new data")
        ),
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        input$doit
        n <- input$n
        mu1 <- input$mu1
        mu2 <- input$mu2
        sd1 <- input$sd1
        sd2 <- input$sd2
        poolsd <- sqrt((sd1^2 + sd2^2)/n)
        
        #samp1 <- rnorm(n, mu1, sd1)
        #diffs <- rnorm(n, mu1 - mu2, poolsd)
        #samp2 <- (samp1 - diffs)*sd2
        
        samp1 <- rnorm(n)
        diffs <- rnorm(n)
        samp2 <- samp1 - diffs
        samp1 <- sd1*samp1 + mu1
        samp2 <- sd2*samp2 + mu2
        
        ue <- t.test(samp1, samp2, paired = FALSE, var.equal = TRUE)
        un <- t.test(samp1, samp2, paired = FALSE, var.equal = FALSE)
        pn <- t.test(samp1, samp2, paired = TRUE)
        
        cidf <- data.frame(
            loci = c(ue$conf[1], un$conf[1], pn$conf[1]),
            hici = c(ue$conf[2], un$conf[2], pn$conf[2]),
            pval = c(ue$p.val[1], un$p.val[1], pn$p.val[1]),
            model = c("Unpaired_VarEqual", "Unpaired_VarUnequal", "Paired"),
            paired = c(FALSE, FALSE, TRUE),
            var.equal = c(TRUE, FALSE, "NA"))
        
        p1 <- ggplot(cidf, aes(x = model, ymin = loci, ymax = hici,
            colour = paired, linetype = var.equal)) + 
            geom_errorbar() +
            coord_flip() +
            theme_bw() +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14), 
                legend.position = "none") +
            labs(y = "CI for difference", x = NULL)
        
        p2 <- ggplot(cidf, aes(x = pval, xend = pval, y = -Inf, yend = Inf,
            colour = paired, linetype = var.equal)) + 
            geom_segment() +
            coord_cartesian(ylim = c(0,1)) +
            theme_bw() +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14), 
                legend.position = "none") +
            labs(x = "p-value", y = NULL) + 
            scale_y_continuous(breaks = NULL)
        
        p1 / p2 + plot_layout(guides = "collect")
    })
}


shinyApp(ui = ui, server = server)
