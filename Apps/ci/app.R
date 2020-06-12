# 

library(shiny)


mymat <- matrix(integer(0), ncol = 2)
colnames(mymat) <- c("lo", "hi")

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
            sliderInput("alpha",
                "Significance level alpha",
                min = 0.01,
                max = 0.99,
                value = 0.05,
                step = 0.01),
            actionButton("doit", "Add 1"),
            actionButton("doit5", "Add 5")
        ),
        
        
        mainPanel(
            plotOutput("distPlot")
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
    
    newdata <- reactive({
        input$n
        input$sigma
        input$mu
        mymat <<- matrix(integer(0), ncol = 2)
        colnames(mymat) <<- c("lo", "hi")
    })
    
    output$distPlot <- renderPlot({
        newdata()
        adddata()
        addfive()
        
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
        
        xseq <- seq(min(mydf$lo), max(mydf$hi), length.out = 200)
        yseq <- dnorm(xseq, input$mu, input$sigma)
        
        pop <- ggplot() + 
            geom_line(aes(x = xseq, y = yseq), size = 1) +
            theme_void() +
            labs(title = "Population Distribution",
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            annotate("segment", x = mu, xend = mu, y = 0, 
                yend = dnorm(mu, mu, sd)) +
            annotate("text", x = mu, y = -0.05, label = "mu", 
                parse = TRUE)
        
        samps <- ggplot(mydf, aes(ymin = lo, x = sample, 
                ymax = hi, col = truemean)) + 
            geom_errorbar() + 
            coord_flip() + 
            theme_minimal() +
            labs(x = "Sample Number", y = "CI", 
                colour = "Contains true mean?",
                title = paste0("Sample CIs - ", 
                    round(mean(1 - mydf$sig), 4)*100, "% contain mu"),
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            scale_colour_manual(values = c(1,2), drop = F) +
            annotate("segment", x = mu, xend = mu, 
                y = -Inf, yend = Inf, col = "grey")
        
        pop / samps
    })
}


shinyApp(ui = ui, server = server)
