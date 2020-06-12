# 

library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)


mymat <- matrix(integer(0), ncol = 2)
colnames(mymat) <- c("lo", "hi")
myvec <- c()
resetter5 <- 0
resetter25 <- 0

ui <- fluidPage(
    
    
    #titlePanel("Old Faithful Geyser Data"),
    
    
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
            sliderInput("bins",
                "Number of bins",
                min = 5,
                max = 50,
                value = 20,
                step = 1),
            actionButton("doit", "+1"),
            actionButton("doit5", "+5"),
            actionButton("doit25", "+25"),
            actionButton("doit100", "+100"),
            checkboxInput("addNorm", "Add True Sampling Distr.", 
                value = FALSE),
            checkboxInput("equalAx", "Keep same axes across plots", 
                value = TRUE)
        ),
        
        
        mainPanel(
            plotOutput("distPlot", height = "400px"),
            tableOutput("stats")
        )
    )
)


server <- function(input, output) {
    
    adddata <- reactive({
        input$doit
        x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
        myvec <<- c(myvec, mean(x))
    })
    
    addfive <- reactive({
        input$doit5
        for(i in 1:5){
            x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
            myvec <<- c(myvec, mean(x))
        }
    })
    
    add25 <- reactive({
        input$doit25
        for(i in 1:25){
            x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
            myvec <<- c(myvec, mean(x))
        }
    })
    
    add100 <- reactive({
        input$doit100
        for(i in 1:100){
            x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
            myvec <<- c(myvec, mean(x))
        }
    })
    
    newdata <- reactive({
        input$n
        input$sigma
        input$mu
        resetter5 <<- isolate(input$doit5)
        resetter25 <<- isolate(input$doit25)
        resetter100 <<- isolate(input$doit100)
        myvec <<- c()
    })
    
    output$distPlot <- renderPlot({
        newdata()
        adddata()
        if(input$doit5 > resetter5) addfive()
        if(input$doit25 > resetter25) add25()
        if(input$doit100 > resetter100) add100()
        
        mu <- input$mu
        sd <- input$sigma
        #n <- input$n
        
        
        xlims <- c(min(input$mu - 1.5*input$sigma, myvec),
            max(input$mu + 1.5*input$sigma, myvec))
        if(input$equalAx){
            xlims2 <- xlims
        } else {
            xlims2 <- range(myvec)
        }
        
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
            annotate("text", x = mu, y = 0, label = "mu", 
                parse = TRUE, vjust = 1) +
            xlim(xlims) +
            coord_cartesian(ylim = c(0, dnorm(mu,mu,sd)))
        
        samps <- ggplot(mapping = aes(x = myvec)) + 
            geom_histogram(aes(y = ..density..), bins = input$bins,
                colour = 1, fill = "lightgrey") +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            xlim(xlims2) +
            annotate("segment", x = mu, xend = mu, y = -Inf, 
                yend = Inf, colour = 3) +
            labs(title = paste0("Observed Means from ", 
                length(myvec), " Samples")) +
            stat_function(fun = function(x) {
                    dnorm(x, input$mu, input$sigma/sqrt(input$n))
                }, col = rgb(0.37,0.81,0.31,input$addNorm))
            
        
        pop / samps
    })
    
    output$stats <- renderTable({
        input$doit
        input$doit5
        input$doit25
        input$doit100
        data.frame("Mean of Pop." = input$mu,
            "Mean of means" = mean(myvec),
            "Pop. SD / sqrt(n)" = input$sigma/sqrt(input$n), 
            "SD of means" = sd(myvec))
        
        data.frame(Name = c("Mean of Pop.", "Mean of Sample Means",
            "Pop. SD / sqrt(n)", "SD of means"),
            Value = c(input$mu, mean(myvec), input$sigma/sqrt(input$n), sd(myvec)))
    })
}


shinyApp(ui = ui, server = server)
