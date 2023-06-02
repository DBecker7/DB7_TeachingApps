# distrshapes

library(shiny)

parameter_tabs <- tagList(
    tags$style("#params { display:none; }"),
    tabsetPanel(id = "params",
        tabPanel("normal",
            sliderInput("mean", "mean  μ", min = -5, max = 5, 
                value = 1, step = 0.05,
                animate = list(interval = 600)),
            sliderInput("sd", "standard deviation  σ", min = 0.01, max = 10, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("uniform", 
            sliderInput("min", "min a", min = -5, max = 5, 
                value = 0, step = 0.1,
                animate = list(interval = 600)),
            sliderInput("max", "max b", min = -5, max = 5, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("exponential",
            sliderInput("rate", "rate λ", min = 0.01, max = 20, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("gamma",
            sliderInput("gshape", "shape α", min = 0.05, max = 20, 
                value = 1, step = 0.05,
                animate = list(interval = 600)),
            sliderInput("grate", "rate β",  min = 0.05, max = 20, 
                value = 1, step = 0.05,
                animate = list(interval = 600))
        ),
        tabPanel("beta",
            sliderInput("bshape1", "shape 1 α", min = 0.01, max = 20, 
                value = 1, step = 0.05,
                animate = list(interval = 600)),
            sliderInput("bshape2", "shape 2 β",  min = 0.05, max = 20, 
                value = 1, step = 0.05,
                animate = list(interval = 600))
        ),
        tabPanel("weibull",
            sliderInput("wshape", "shape k", min = 0.05, max = 20, 
                value = 1, step = 0.05,
                animate = list(interval = 600)),
            sliderInput("wscale", "scale λ",  min = 0.05, max = 20, 
                value = 1, step = 0.05,
                animate = list(interval = 600))
        )
    )
)

myseed <- 1
setlist <- list(data.frame(x=NA, y = NA))

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
    sidebarLayout(
        sidebarPanel(
            selectInput("dist", "Distribution", 
                choices = c("normal", "uniform", "exponential", "gamma", "beta", "weibull")
            ),
            numericInput("n", "Number of samples", value = 100),
            sliderInput("bins", "binwidth", min = 0.05, max = 5, value = 1, step = 0.05),
            parameter_tabs,
            actionButton("doit", "Click me for new data"),
            actionButton("axes", "Reset axes and ghosts")
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
        
        setlist <<- list(data.frame(x=NA, y = NA))
    })
    
    sample <- reactive({
        set.seed(myseed)
        input$doit
        switch(input$dist,
            normal = rnorm(input$n, input$mean, input$sd),
            uniform = runif(input$n, input$min, input$max),
            exponential = rexp(input$n, input$rate),
            gamma = rgamma(input$n, shape = input$gshape, rate = input$grate),
            beta = rbeta(input$n, shape1 = input$bshape1, shape2 = input$bshape2),
            weibull = rweibull(input$n, shape = input$wshape, scale = input$wscale)
        )
    })
    
    distfun <- reactive({
        
        # Allows for switching order if b < a
        unifs <- c(input$min-0.001, input$max)
        
        lohi <- switch(input$dist,
            normal = c(input$mean - 3*input$sd, input$mean + 3*input$sd),
            # Switch order:
            uniform = c(min(unifs), max(unifs)),
            exponential = c(0, qexp(0.999, input$rate)),
            #gamma = c(0,forgamma[min(which(mydgamma < 0.005))])
            gamma = c(0, qgamma(0.999, input$gshape, input$grate)),
            beta = c(0, 1),
            weibull = c(0, qweibull(0.999, input$wshape, input$wscale))
        )
        
        xseq <- seq(lohi[1], lohi[2], length.out = 500)
        
        
        yseq <- switch(input$dist,
            normal = dnorm(xseq, input$mean, input$sd),
            uniform = dunif(xseq, min(unifs), max(unifs)),
            exponential = dexp(xseq, input$rate),
            gamma = dgamma(xseq, shape = input$gshape, rate = input$grate),
            beta = dbeta(xseq, shape1 = input$bshape1, shape2 = input$bshape2),
            weibull = dweibull(xseq, shape = input$wshape, scale = input$wscale)
        )
        
        
        setlist <<- c(setlist, list(data.frame(x = xseq, y = yseq)))
        
        data.frame(x = xseq, y = yseq)
    })
    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
    })
    
    output$hist <- renderPlot({
        axes()
        
        x <- sample()
        d <- distfun()
        
        newseed()
        
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
                normal = bquote("f(x)=("*2*pi*sigma^2*")"^{-n/2}*exp(-(x*"-"*mu)^2/(2*sigma^2))),
                uniform = bquote("f(x)="*1/(b-a)),
                exponential = bquote("f(x)="*lambda*exp(-lambda*x)),
                gamma = bquote("f(x)="*beta^alpha*x^{alpha-1}*exp(-beta*x)/Gamma(alpha)),
                beta = bquote("f(x)="*Gamma(alpha*"+"*beta)*x^{alpha-1}*
                        (1-x)^{beta-1}/(Gamma(alpha)*Gamma(beta))),
                weibull = bquote("f(x)="*(k/lambda)*(x/lambda)^{k-1}*exp(-(x/lambda)^k))
            ),
            bty = "n", text.col = 4, cex = 1)
        
        for(i in 1:length(setlist)){
            lines(setlist[[i]], col = rgb(0,0,0,0.1))
        }
        
        lines(d, col = 4,lwd = 2)
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
