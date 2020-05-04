# ScatterCorr
# A tool for students to explore correlation.


library(MASS)
library(shiny)
library(ggplot2)

myseed <- 2112
set.seed(myseed)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Scatterplots with fixed correlation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "Number of samples",
                min = 5,
                max = 500,
                value = 50),
            sliderInput("r",
                "Squared Correlation (r^2)",
                min = 0,
                max = 1,
                value = 0,
                step = 0.05,
                animate = list(interval = 700)),
            sliderInput("a",
                "Intercept",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1),
            sliderInput("b",
                "Slope",
                min = -5,
                max = 5,
                value = 0,
                step = 0.1,
                animate = list(interval = 700)),
            actionButton(inputId = "doit", label = "Click me for new data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("Some questions:<br><ul>
	<li>If the correlation is 0, what values can the slope take?</li>
	<li>If the correlation is 0.5, what values can the slope take?</li>
	<li>If the correlation is -0.5, what values can the slope take?</li>
</ul>"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
    })
    
    makedata <- reactive({
        set.seed(myseed)
        input$doit
        n <- input$n
        r <- input$r
        # Shamelessly stolen from https://stats.stackexchange.com/questions/83172/generate-two-variables-with-precise-pre-specified-correlation
        xy = mvrnorm(n = n, mu = c(0, 0), 
            Sigma = matrix(c(1, r^2, r^2, 1), nrow = 2), 
            empirical=TRUE)
        x = xy[, 1] 
        y = input$a + input$b*xy[, 2] 
        data.frame(x = x, y = y)
    })
    
    output$distPlot <- renderPlot({
        newseed()
        xy <- makedata()
        
        x <- xy[,1]
        y <- xy[,2]
        
        if(length(unique(y)) > 1){
            mycor <- round(cor(x,y), 3)
        } else {
            mycor <- "undefined"
        }
        
        lims <- c(floor(min(y)), ceiling(max(y)))
        roundto <- 1
        lims <- c(floor(lims[1]/roundto)*roundto, 
            ceiling(lims[2]/roundto)*roundto)
        lmax <- max(abs(lims))
        lims <- c(-lmax, lmax)
        if(max(abs(y - mean(y))) < 1) lims <- mean(y) + c(-1,1)
        if(max(abs(y - mean(y))) < 0.5) lims <- mean(y) + c(-0.5,0.5)
        if(max(abs(y - mean(y))) < 0.1) lims <- mean(y) + c(-0.1,0.1)
        
        ggplot(mapping = aes(x = x, y = y)) + geom_point() + 
            geom_smooth(method = "lm", formula = y ~ x) +
            theme_bw() + 
            coord_cartesian(ylim = lims) + 
            labs(title = paste0("Scatterplot with n = ", nrow(xy), 
                " and r = ", mycor)) +
            scale_y_continuous(breaks = seq(-20,20, 2), 
                minor_breaks = seq(-20,20,1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
