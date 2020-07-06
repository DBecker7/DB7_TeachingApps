# samplingDist

library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)


mymat <- matrix(integer(0), ncol = 2)
colnames(mymat) <- c("lo", "hi")
myvec <- c()
resetter5 <- 0
resetter25 <- 0
colourlast <<- 0

ui <- fluidPage(
    
    
    titlePanel("Sampling Distributions"),
    
    
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
            checkboxInput("addRug", "Add most recent sample?", 
                value = TRUE),
            checkboxInput("addNorm", "Add True Sampling Distr?", 
                value = FALSE),
            checkboxInput("equalAx", "Keep same axes across plots?", 
                value = TRUE)
        ),
        
        
        mainPanel(
            plotOutput("distPlot", height = "400px"),
            column(width = 6, 
                tableOutput("stats")
            ), 
            column(width = 6, 
                tags$div(HTML("Some questions:
<ol><li>Set n=3. How many samples do you need before the histogram matches the density plot?</li>
    <li>Set n=100. How many samples do you need now?</li>
    <li>The table shows the sample sd for the most recent sample. Is this closer to the population sd or the sd of the sampling distribution?</li>
</ol>"))
            )
        )
    )
)


server <- function(input, output) {
    
    adddata <- reactive({
        input$doit
        x <<- rnorm(input$n, mean = input$mu, sd = input$sigma)
        myvec <<- c(myvec, mean(x))
        
        colourlast <<- 1
    })
    
    addfive <- reactive({
        input$doit5
        for(i in 1:5){
            x <<- rnorm(input$n, mean = input$mu, sd = input$sigma)
            myvec <<- c(myvec, mean(x))
        }
        colourlast <<- 5
    })
    
    add25 <- reactive({
        input$doit25
        for(i in 1:25){
            x <<- rnorm(input$n, mean = input$mu, sd = input$sigma)
            myvec <<- c(myvec, mean(x))
        }
        colourlast <<- 25
    })
    
    add100 <- reactive({
        input$doit100
        for(i in 1:100){
            x <<- rnorm(input$n, mean = input$mu, sd = input$sigma)
            myvec <<- c(myvec, mean(x))
        }
        colourlast <<- 100
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
        
        
        xlims <- c(min(input$mu - 2*input$sigma, myvec),
            max(input$mu + 2*input$sigma, myvec))
        if(input$equalAx){
            xlims2 <- xlims
        } else {
            minmax <- max(abs(min(myvec - input$mu)), 
                max(myvec - input$mu))
            xlims2 <- c(-minmax, minmax) + input$mu
            xlims <- input$mu + c(-2,2)*input$sigma
        }
        
        xseq <- seq(xlims[1], xlims[2], length.out = 200)
        yseq <- dnorm(xseq, input$mu, input$sigma)
        
        if(input$addRug){
            myRug <- list(geom_rug(mapping = aes(x = x), 
                colour = "red"),
                annotate("text", x = mean(x), y = 0.1*dnorm(mu,mu,sd), 
                    vjust = 1, colour = "red", 
                    label = "bar(x)", parse = TRUE))
        } else {
            myRug <- NULL
        }
        
        histcols <- rep("Old Samples", length(myvec))
        histcols[1:colourlast] <- "Newly Added Samples"
        histcols <- factor(rev(histcols), 
            levels = c("Newly Added Samples", "Old Samples"))
        
        pop <- ggplot() + 
            geom_line(aes(x = xseq, y = yseq), size = 1) +
            theme_void() +
            labs(title = "Population Distribution",
                subtitle = "Red ticks are the most recent sample") +
            theme(plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5),
                title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            annotate("segment", x = mu, xend = mu, 
                y = 0.1*dnorm(mu, mu, sd), 
                yend = dnorm(mu, mu, sd), colour = 3) +
            annotate("text", x = mu, y = 0, label = "mu", 
                parse = TRUE, vjust = 0) +
            xlim(xlims) +
            #coord_cartesian(ylim = c(0, dnorm(mu,mu,sd))) +
            myRug
        
        samps <- ggplot(mapping = aes(x = myvec)) + 
            geom_histogram(aes(y = stat(count / sum(count) / width), 
                fill = histcols),
                bins = input$bins, 
                colour = 1) +
            scale_fill_manual(values = c("red", "grey40"), 
                drop = FALSE) +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(title = element_text(size = 16), 
                plot.subtitle = element_text(hjust = 0.5),
                axis.text = element_text(size = 14), 
                legend.position = "none") +
            xlim(xlims2) +
            annotate("segment", x = mu, xend = mu, y = -Inf, 
                yend = Inf, colour = 3) +
            labs(title = paste0("Observed Means from ", 
                length(myvec), " Samples"),
                fill = NULL,
                subtitle = "Red bars are most recently added sample(s)") +
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
        data.frame("Mean of Population" = input$mu,
            "Mean of means" = mean(myvec),
            "Population SD / sqrt(n)" = input$sigma/sqrt(input$n), 
            "SD of means" = sd(myvec))
        
        data.frame(Name = c("Mean of Population", 
            "Mean of Sample Means",
            "Mean of last sample",
            "Population SD/sqrt(n)", 
            "SD of Sample Means",
            "SD of last sample",
            "SD of last sample/sqrt(n)"),
            Value = c(input$mu, mean(myvec), mean(x), input$sigma/sqrt(input$n), 
                sd(myvec), sd(x), sd(x)/sqrt(input$n)))
    })
}


shinyApp(ui = ui, server = server)
