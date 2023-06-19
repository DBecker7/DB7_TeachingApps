# ci

library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)

mymat <- matrix(integer(0), ncol = 2)
colnames(mymat) <- c("lo", "hi")
resetter5 <- 0
resetter25 <- 0

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
    
    
    titlePanel("Confidence Intervals"),
    
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "n",
                min = 3,
                max = 100,
                value = 3,
                step = 1),
            sliderInput("mu",
                "mu",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1),
            sliderInput("sigma",
                "sigma",
                min = 0.01,
                max = 5,
                value = 1,
                step = 0.01),
            sliderInput("alpha",
                "Significance level alpha",
                min = 0.01,
                max = 0.99,
                value = 0.05,
                step = 0.01),
            actionButton("doit", "Add 1"),
            actionButton("doit5", "Add 5"),
            actionButton("doit25", "Add 25")
        ),
        
        
        mainPanel(
            plotOutput("distPlot", height = "600px")
        )
    )
)


server <- function(input, output) {
    
    adddata <- reactive({
        input$doit
        x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
        x2 <- matrix(c(mean(x), sd(x)), ncol = 2)
        colnames(x2) <- c("hi", "lo")
        mymat <<- rbind(mymat, x2)
    })
    
    addfive <- reactive({
        input$doit5
        for(i in 1:5){
            x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
            x2 <- matrix(c(mean(x), sd(x)), ncol = 2)
            colnames(x2) <- c("hi", "lo")
            mymat <<- rbind(mymat, x2)}
    })
    
    add25 <- reactive({
        input$doit25
        for(i in 1:25){
            x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
            x2 <- matrix(c(mean(x), sd(x)), ncol = 2)
            colnames(x2) <- c("hi", "lo")
            mymat <<- rbind(mymat, x2)}
    })
    
    newdata <- reactive({
        input$n
        input$sigma
        input$mu
        resetter5 <<- isolate(input$doit5)
        resetter25 <<- isolate(input$doit25)
        mymat <<- matrix(integer(0), ncol = 2)
        colnames(mymat) <<- c("lo", "hi")
    })
    
    output$distPlot <- renderPlot({
        newdata()
        adddata()
        if(input$doit5 > resetter5) addfive()
        if(input$doit25 > resetter25) add25()
        
        mu <- input$mu
        sd <- input$sigma
        n <- input$n
        
        mydf <- apply(mymat, 1, function(x) {
            x[1] + c(1,-1) * qnorm(input$alpha/2) * x[2] / sqrt(input$n)
        }) %>% 
            t() %>% 
            magrittr::set_colnames(c("lo", "hi")) %>% 
            as.data.frame() %>% 
            mutate(sample = factor(1:n(), levels = n():1),
                sig = lo > mu | hi < mu) %>% 
            mutate(truemean = factor(!sig, levels = c("TRUE", "FALSE")))
        
        xlims <- c(min(input$mu - 1.5*input$sigma, mydf$lo),
            max(input$mu + 1.5*input$sigma, mydf$hi))
        
        xseq <- seq(xlims[1], xlims[2], length.out = 200)
        yseq <- dnorm(xseq, input$mu, input$sigma)
        
        pop <- ggplot() + 
            geom_line(aes(x = xseq, y = yseq), size = 1) +
            theme_void() +
            labs(title = "Population Distribution") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            annotate("segment", x = mu, xend = mu, y = 0, 
                yend = dnorm(mu, mu, sd), colour = 3) +
            annotate("text", x = mu, y = -0.05, label = "mu", 
                parse = TRUE) +
            xlim(xlims)
        
        samps <- ggplot(mydf, aes(ymin = lo, x = sample, 
                ymax = hi, col = truemean)) + 
            geom_errorbar() + 
            coord_flip(ylim = xlims) + 
            theme_minimal() +
            labs(x = "Sample Number", y = "CI", 
                colour = "Contains true mean?",
                title = paste0("Sample CIs: ", 
                    round(mean(1 - mydf$sig), 4)*100, "% contain mu"),
                caption = paste0("Created by Devan Becker\n", 
                    "Github: DBecker7/DB7_TeachingApps")) +
            theme(plot.title = element_text(hjust = 0.5),
                title = element_text(size = 16), 
                axis.text = element_text(size = 14),
                legend.position = "nont") +
            scale_colour_manual(values = c(1,2), drop = F) +
            annotate("segment", x = -Inf, xend = Inf, 
                y = mu, yend = mu, col = 3)
        
        pop / samps + plot_layout(heights = c(1,2))
    })
}


shinyApp(ui = ui, server = server)
