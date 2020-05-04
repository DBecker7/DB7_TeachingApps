# MeanLessMeansLeft
# if the MEAN is LESS than the median, that MEANS it's LEFT skewed

library(shiny)

ui <- fluidPage(
    
    # Application title
    titlePanel("Skewed Data"),
    
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
            plotOutput("distPlot")
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
        
        if(flipit){
            mydf <- mutate(mydf, x = - x + 2*setmedian)
        }
        
        if(setmean == setmedian){
            mydf <- data.frame(x = seq(0, 100, 0.01)) %>% 
                mutate(y = dnorm(x, mean = setmean, 
                    sd = 2)) %>% 
                filter((y > 0.001 | x < setmean) & y < 3)
        }
        
        
        ggplot(mydf, aes(x = x, y = y)) + 
            geom_line(size = 1) +
            theme_bw() +
            labs(title = "Mean Less Means Left",
                subtitle = "If the MEAN is LESS than the median, that MEANS it is LEFT skewed.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
