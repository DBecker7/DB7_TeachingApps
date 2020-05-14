parameter_tabs <- tagList(
    tags$style("#params { display:none; }"),
    tabsetPanel(id = "params",
        tabPanel("normal",
            sliderInput("mean", "mean", min = -5, max = 5, 
                value = 1, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("sd", "standard deviation", min = 0.01, max = 10, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("uniform", 
            sliderInput("min", "min", min = -5, max = 5, 
                value = 1, step = 0.1,
                animate = list(interval = 600)),
            sliderInput("max", "max", min = -5, max = 5, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("exponential",
            sliderInput("rate", "rate", min = 0, max = 20, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("gamma",
            sliderInput("gshape", "shape", min = 0, max = 20, 
                value = 1, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("grate", "rate",  min = 0, max = 20, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        )
    )
)

myseed <- 1

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("dist", "Distribution", 
                choices = c("normal", "uniform", "exponential", "gamma")
            ),
            numericInput("n", "Number of samples", value = 100),
            sliderInput("bins", "binwidth", min = 0.1, max = 5, value = 1, step = 0.1),
            parameter_tabs,
            actionButton("doit", "Click me for new data"),
            actionButton("axes", "Reset axes")
        ),
        mainPanel(
            plotOutput("hist")
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$dist, {
        updateTabsetPanel(session, "params", selected = input$dist)
    }) 
    
    axes <- reactive({
        input$axes
        input$dist
        
        xmin <<- 0
        xmax <<- 0
        ymax <<- 0
    })
    
    sample <- reactive({
        set.seed(myseed)
        input$doit
        switch(input$dist,
            normal = rnorm(input$n, input$mean, input$sd),
            uniform = runif(input$n, input$min, input$max),
            exponential = rexp(input$n, input$rate),
            gamma = rgamma(input$n, shape = input$gshape, rate = input$grate)
        )
    })
    
    distfun <- reactive({
        
        unifs <- c(input$min-0.001, input$max)
        
        lohi <- switch(input$dist,
            normal = c(input$mean - 3*input$sd, input$mean + 3*input$sd),
            uniform = c(min(unifs), max(unifs)),
            exponential = c(0, 5/input$rate),
            #gamma = c(0,forgamma[min(which(mydgamma < 0.005))])
            gamma = c(0, ifelse(input$grate < 0.2, 600, 100))
        )
        
        xseq <- seq(lohi[1], lohi[2], length.out = 500)
        
        
        yseq <- switch(input$dist,
            normal = dnorm(xseq, input$mean, input$sd),
            uniform = dunif(xseq, min(unifs), max(unifs)),
            exponential = dexp(xseq, input$rate),
            gamma = dgamma(xseq, shape = input$gshape, rate = input$grate)
        )
        data.frame(x = xseq, y = yseq)
    })
    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
    })
    
    output$hist <- renderPlot({
        x <- sample()
        d <- distfun()
        
        newseed()
        axes()
        
        histbreaks <- seq(min(floor(x)), max(ceiling(x)) + input$bins, by = input$bins)
        
        histvals <- hist(x, plot = FALSE, breaks = histbreaks)
        xmin <<- min(xmin, min(x))
        xmax <<- max(xmax, max(x))
        ymax <<- max(ymax, max(d$yseq, histvals$density))
        
        mytitle <- paste0("Histogram and PDF of ", input$dist)
        
        hist(x, xlim = c(xmin, xmax), ylim = c(0, ymax), 
            freq = FALSE, breaks = histbreaks,
            main = mytitle, xlab = "x", ylab = "Density")
        
        legend("topleft", 
            legend = switch(input$dist,
                normal = bquote("f(x)=("*2*pi*sigma^2*")"^{n/2}*exp(-(x*"-"*mu)^2/(2*sigma^2))),
                uniform = bquote("f(x)="*1/(b-a)),
                exponential = bquote("f(x)="*lambda*exp(-lambda*x)),
                gamma = bquote("f(x)="*beta^alpha*x^{alpha-1}*exp(-beta*x)/Gamma(alpha))
            ),
            bty = "n", text.col = 4, cex = 1)
        
        lines(d, col = 4,lwd = 2)
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
