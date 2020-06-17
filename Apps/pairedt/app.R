# pairedt

# TODO: var.equals = FALSE makes no sense. Simulate better, Devan!

library(shiny)
library(ggplot2)
library(patchwork)
library(dplyr)

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
            tabsetPanel(
                tabPanel("One at a time", plotOutput("distPlot")),
                tabPanel("Type 2 error", plotOutput("type2Plot"))
            )
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
    
    output$type2Plot <- renderPlot({
        input$doit
        n <- input$n
        mu1 <- input$mu1
        mu2 <- input$mu2
        sd1 <- input$sd1
        sd2 <- input$sd2
        poolsd <- sqrt((sd1^2 + sd2^2)/n)
        
        N <- 1000
        
        cil <- vector("list", N)
        
        for(i in 1:N){
            samp1 <- rnorm(n)
            diffs <- rnorm(n)
            samp2 <- samp1 - diffs
            samp1 <- sd1*samp1 + mu1
            samp2 <- sd2*samp2 + mu2
            
            ue <- t.test(samp1, samp2, paired = FALSE, var.equal = TRUE)
            un <- t.test(samp1, samp2, paired = FALSE, var.equal = FALSE)
            pn <- t.test(samp1, samp2, paired = TRUE)
            
            cil[[i]] <- data.frame(
                loci = c(ue$conf[1], un$conf[1], pn$conf[1]),
                hici = c(ue$conf[2], un$conf[2], pn$conf[2]),
                pval = c(ue$p.val[1], un$p.val[1], pn$p.val[1]),
                model = c("Unpaired_VarEqual", "Unpaired_VarUnequal", "Paired"),
                paired = c(FALSE, FALSE, TRUE),
                var.equal = c(TRUE, FALSE, "NA"))
        }
        
        tdiff <- input$mu1 - input$mu2
        cidf <- bind_rows(cil) %>% 
            mutate(coverage = loci < tdiff & hici > tdiff)
        cis <- group_by(cidf, model) %>% 
            summarise(type1 = 1 - mean(coverage))
        
        ggplot(cis, aes(x = model, y = type1)) + 
            theme_bw() + 
            theme(legend.position = "none") + 
            geom_col(colour = 1, 
                mapping = aes(fill = ifelse(substr(model, 1, 1) != "U", 
                    "A", 2))) +
            geom_text(aes(label = round(type1, 4)), vjust = -0.1, size = 7)
    })
}


shinyApp(ui = ui, server = server)
