# MeanLessMeansLeft
# if the MEAN is LESS than the median, that MEANS it's LEFT skewed

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
    
    # Application title
    titlePanel("Exploration of Skewed Data"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("mean",
                "Mean:",
                min = 1,
                max = 50,
                value = 14,
                step = 0.25,
                animate = list(interval = 500)),
            sliderInput("median",
                "Median:",
                min = 5,
                max = 50,
                value = 15,
                step = 0.25,
                animate = list(interval = 500))
        ),
        
        mainPanel(
            plotOutput("distPlot"),
            "To quote a journal that Wikipedia quotes: Many textbooks teach a rule of thumb stating that the mean is right of the median under right skew, and left of the median under left skew. This rule fails with surprising frequency. It can fail in multimodal distributions, or in distributions where one tail is long but the other is heavy. Most commonly, though, the rule fails in discrete distributions where the areas to the left and right of the median are not equal. Such distributions not only contradict the textbook relationship between mean, median, and skew, they also contradict the textbook interpretation of the median."
        )
    )
)

server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        setmean <- input$mean
        setmedian <- input$median
        
        # gamma is right skewed only, so if mean < median, flip it.
        flipit <- FALSE
        if(setmedian > setmean){
            setmedian <- setmean - abs(setmean - setmedian)
            flipit <- TRUE
        }
        
        # A list of parameter combos that have the correct mean
        beta <- seq(0,100,1) + 0.001
        alpha <- beta*setmean
        
        
        # Iterate to find the parameter combo with the correct median
        difff <- 10
        for(i in 1:10){
            aub <- max(2, which.min(setmedian - qgamma(0.5, alpha, beta) > 0))
            alb <- max(1, aub - 1)
            alpha <- seq(alpha[alb], alpha[aub], length.out = 100)
            beta <- alpha/setmean
            difff <- abs(setmedian - qgamma(0.5, alpha[aub], beta[aub]))
            if(difff < 1e-4) break
        }
        
        mydf <- data.frame(x = seq(0, 100, 0.01)) %>% 
            mutate(y = dgamma(x, alpha[aub], beta[aub])) %>% 
            filter((y > 0.001 | x < setmean) & y < 3)
        
        medheight <- dgamma(setmedian, alpha[aub], beta[aub])
        meanheight <- dgamma(setmean, alpha[aub], beta[aub])
        
        skewness <- 2/sqrt(alpha[aub])
        
        if(flipit){
            mydf <- mutate(mydf, x = - x + 2*setmean)
            setmedian <- input$median
            skewness <- -skewness
        }
        
        if(setmean == setmedian){
            mydf <- data.frame(x = seq(0, 100, 0.01)) %>% 
                mutate(y = dnorm(x, mean = setmean, 
                    sd = 2)) %>% 
                filter((y > 0.001 | x < setmean) & y < 3)
            
            medheight <- dgamma(setmedian, setmean, 2)
            meanheight <- medheight
            skewness = 0
        }
        
        ggplot(mydf, aes(x = x, y = y)) + 
            geom_line(size = 1) +
            theme_bw() +
            labs(title = paste0("Skewness = ", round(skewness, 2)),
                subtitle = "Mean Less Means Left: If the MEAN is LESS than the median, that MEANS it is LEFT skewed.") +
            annotate("segment", x = c(setmean, setmedian), y = c(0,0),
                xend = c(setmean, setmedian), yend = c(meanheight, medheight),
                size = 1, colour = c(4,2)) +
            annotate("text", x = setmedian, y = medheight, label = "Median", 
                hjust = 0, colour = 2) + 
            annotate("text", x = setmean, y = meanheight, label = "Mean", 
                hjust = 1, colour = 4) + 
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
