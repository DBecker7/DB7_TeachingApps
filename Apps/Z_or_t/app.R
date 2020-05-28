#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Z or t?"),
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
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "Sample size:",
                min = 2,
                max = 60,
                value = 8,
                step = 1),
            sliderInput("mu",
                "Population mean:",
                min = -5,
                max = 5,
                value = 0,
                step = 0.25),
            sliderInput("sigma",
                "Population sd:",
                min = 0.05,
                max = 5,
                value = 1,
                step = 0.05),
            actionButton("doit", "Click for new Sampling Distribution")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput("vtout")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$vtout <- renderPrint({
        cat("# How the sampling distribution is created:",
            "N <- 5000 # number of samples to take (NOT sample size)",
            "zobs <- rep(NA, times = N) # empty vector",
            "for(i in 1:N) {",
            "    xsamp <- rnorm(n = n, mean = mu, sd = sd)",
            "    # Note: dividing by sample sd, NOT population sd",
            "    zobs[i] <- (mean(xsamp) - mu) / (sd(xsamp) / sqrt(n))",
            "}", 
            sep = "\n")
    })
    
    output$distPlot <- renderPlot({
        input$doit
        
        sampling_dist <- replicate(1000, {
            xsamp <- rnorm(input$n, input$mu, input$sigma)
            c(mean(xsamp), sd(xsamp))
        }) # row 1 is mean, row 2 is sd
        
        zobs <- apply(sampling_dist, 2, 
            function(x) (x[1] - input$mu)/(x[2]/sqrt(input$n)))
        zcut <- zobs[zobs >= -4 & zobs <= 4]
        
        histheights <- hist(zcut, 
            breaks = seq(min(floor(zcut)), max(ceiling(zcut)), 0.2), 
            plot = FALSE)
        
        ggplot(mapping = aes(x = histheights$breaks[-1] -0.1, 
            y = histheights$density)) + 
            geom_col(width = 0.2, colour = 1, fill = "lightgrey") +
            stat_function(fun = dnorm, 
                colour = 3, n = 3000, size = 1) +
            stat_function(fun = dt, args = list(df = input$n), 
                colour = 6, n = 3000, size = 1) +
            coord_cartesian(xlim = c(-3.5, 3.75)) + 
            theme_bw() +
            annotate("text", x = 0.65, y = dnorm(0.65), 
                label = "frac(bar(X)-mu,s/sqrt(n)) %~% N(0,1)", 
                hjust = 0, vjust = 0, colour = 3, parse = TRUE, size = 10) +
            annotate("text", x = 1.960, y = dt(1.960, n - 1), 
                label = paste("frac(bar(X)-mu,s/sqrt(n)) %~% t[", input$n - 1, "]", sep = ""), 
                hjust = 0, vjust = 0, colour = 6, parse = TRUE, size = 10) +
            labs(x = "All Possible Sample Means", y = "Density",
                title = "Sampling Distribution: Z or t?",
                subtitle = paste0("P(|Z| > 1.96)=", round(2*pnorm(-1.96), 2),
                    ", P(|t_", input$n-1, "|>1.96)=", 
                    round(2*pt(-1.96, input$n-1), 3), sep = ""),
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
