# DensHist
# The relation between density plots and histograms, 
# And how bin width affects the shape

library(shiny)

myseed <- round(runif(1, 0, 1000))
softmax <- 0.1

ui <- fluidPage(
    
    titlePanel("Relationship between histograms and density lines"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "Sample Size",
                min = 5,
                max = 500,
                step = 5,
                value = 100),
            sliderInput("bin",
                "Histogram Bin",
                min = 0,
                max = 20,
                step = 0.25,
                value = c(10,20),
                animate = list(interval = 200, loop = TRUE)),
            sliderInput("bw",
                "Density Bandwidth",
                min = 0,
                max = 2,
                step = 0.05,
                value = 1,
                animate = list(interval = 200)),
            actionButton("doit", "Click me for new data")
        ),
        
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("Some questions:<br><ul>
    <li>Changing the bins does not change the data, even though it might look that way. For various bin widths, how does the shape of the histogram change?</li>
    <li>How does the shape of the density curve change with bandwidth?</li>
    <li>How does the wiggliness of the density curve change with sample size, and why?</li>
	<li>Set the bin width to 0.75 and bandwidth to 0.2, then hit play on the Bin slider. Is the selected bin close to the density line?</li>
</ul>"))
        )
    )
)

server <- function(input, output) {
    
    newdata <- reactive({
        set.seed(myseed)
        input$doit
        x <- rnorm(input$n, 10, 4)
        x <- x[x > 0 & x < 20]
        x
    })
    
    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
        set.seed(myseed)
        softmax <<- 0.1
    })
    
    # Using base R graphics for speed
    output$distPlot <- renderPlot({
        x <- newdata()
        
        newseed()
        
        xcutlen <- input$bin[2] - input$bin[1]
        toomanycuts <- input$bin[1] + c(-100:100)*xcutlen
        firstcut <- toomanycuts[max(which(toomanycuts < 0))]
        lastcut <- toomanycuts[min(which(toomanycuts > 20))]
        mycuts <- seq(firstcut, lastcut, by = xcutlen)
        
        myhist <- hist(x, breaks = mycuts, plot = FALSE)
        histy <- myhist$density
        
        xpos <- which(mycuts == input$bin[1])
        xheight <- histy[xpos]
        
        xdens <- density(x, bw = input$bw)
        #dx <- xdens$x
        dy <- xdens$y
        
        softmax <<- max(softmax, histy, dy)
        
        hist(x, freq = FALSE, col = rgb(0,0,0,0.02), 
            border = rgb(0,0,0,0.2), 
            breaks = mycuts, 
            xlim = c(0,20),
            ylim = c(0, softmax),
            xlab = "x", ylab = "Density", 
            main = NULL)
        
        polygon(x = c(0, rep(input$bin, each = 2), 20), 
            y = c(0,0,rep(xheight, 2), 0,0),
            col = "#d9b3ff")
        lines(density(x, bw = input$bw), lwd = 2, col = "#009933")
        
        mtext("Created by Devan Becker", 
            side = 1, line = 3, adj = 1, cex = 0.75)
        mtext("Github: DBecker7/DB7_TeachingApps", 
            side = 1, line = 4, adj = 1, cex = 0.75)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
