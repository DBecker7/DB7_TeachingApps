# MeasureSpread
# Data can be generated with (almost) any IQR and variance
# But when they're "similar", the data looks normal



library(shiny)


q1 <- function(x) median(x[x < median(x)])
q2 <- function(x) median(x)
q3 <- function(x) median(x[x > median(x)])

dblmed <- function(x){
    q1 <- median(x[x < median(x)])
    q3 <- median(x[x > median(x)])
    q3 - q1
}

# as a function
perturb <- function(x, myiqr, mysd){
    x <- x - median(x)
    n <- length(x)
    
    lo <- x[x < median(x)]
    hi <- x[x > median(x)]
    
    lo2 <- -lo*myiqr/(median(lo)*2)
    hi2 <- hi*myiqr/(median(hi)*2)
    
    x2 <- c(lo2, ifelse(n %% 2 == 0, 0, NA), hi2)
    x2 <- x2[!is.na(x2)]
    
    q <- c(q1(x2), q2(x2), q3(x2))
    xcut <- data.frame(x = x2, 
        cut = cut(x2, 
            breaks = c(-Inf, q, Inf), 
            labels = FALSE),
        id = 1:length(x2))
    
    x3 <- xcut$x
    x4 <- xcut$x
    sdhist <- sd(x3)
    
    # if sd is too high, shrink towards median
    up <- 1
    if(sdhist < mysd) up <- -1
    
    for(i in 1:400){
        x4 <- x3
        ind1 <- sample(xcut$id, 1)
        mx <- mean(x3)
        myx <- x3[ind1]
        
        if(xcut$cut[ind1] == 1){ # below first quantile
            myx <- myx + up*abs(myx - q[1])/2
        } else if(xcut$cut[ind1] == 2) { # below second quartile
            if(up == 1){ # if sd too high, shink towards either mean or median
                shrinkto <- min(abs(myx - q[2]), abs(myx - mx))
            } else {
                shrinkto <- abs(myx - q[1])
            }
            myx <- myx + up*shrinkto/2
        } else if(xcut$cut[ind1] == 3){
            if(up == 1){ # if sd too high, shink towards either mean or median
                shrinkto <- min(abs(myx - q[2]), abs(myx - mx))
            } else {
                shrinkto <- abs(myx - q[3])
            }
            myx <- myx - up*shrinkto/2
        } else {
            myx <- myx - up*abs(myx - q[3])/2
        }
        x4[ind1] <- myx
        if(dblmed(x3) == dblmed(x4)) x3 <- x4
        sdhist[i] <- sd(x3)
        
        # if sd is too high, shrink towards median
        up <- 1
        if(sdhist[i] < mysd) up <- -1
        
        if(abs(sdhist[i] - mysd) < 0.01) break
    }
    
    x3
}

# Define UI for application that draws a histogram
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
    
    # Application title
    titlePanel("Measures of Spread"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("IQR",
                "IQR:",
                min = 0.25,
                max = 10,
                value = 3,
                step = 0.25,
                animate = list(interval = 700)),
            sliderInput("sd",
                "sd:",
                min = 0.25,
                max = 10,
                value = 3,
                step = 0.25,
                animate = list(interval = 700)),
            sliderInput("n",
                "n:",
                min = 30,
                max = 300,
                value = 50,
                step = 5),
            actionButton("doit", "Click me for new data"),
            checkboxInput("dens", "Add a density curve?"),
            checkboxInput("norm", "Add a normal curve?")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("Some questions:
            <ol>
                <li>Set sd = 1. What value of the IQR makes this look the most 'Normal'?</li>
                <li>Calculate the IQR of a theoretical normal distribution.</li>
                <li>For these simulations, I made Q2 = 0, Q1 = Q2 - IQR/2, and Q3 = Q2 + IQR/2. Is the mean restricted to be 0?</li>
                <li>What's the theoretical lower limit on the variance for a given IQR, such that Q1 = -Q3 and Q2 = 0? Hint: imagine you have 10 values below Q1, 10 between Q1 and Q2, etc., then rearrange them to be closest to 0.</li>
            </ol>"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        input$doit
        x <- rnorm(input$n, sd = input$sd)
        x <- perturb(x, input$IQR, input$sd)
        
        bw <- ifelse(input$n > 100, 0.25, 0.75)
        breakseq <- seq(floor(min(x)) - bw, ceiling(max(x)) + bw, bw)
        hist(x, breaks = breakseq, freq = !(input$dens | input$norm), 
            main = paste0("sd = ", round(sd(x), 4), 
                ", IQR = ", round(dblmed(x), 4)))
        if(input$dens) lines(density(x), col = 2, lwd = 2)
        mx <- mean(x); sx <- sd(x)
        if(input$norm) curve(dnorm(x, mean = mx, sd = sx), 
            add = TRUE, col = 3, lwd = 2)
        q <- c(q1(x), q2(x), q3(x))
        abline(v = q, col = 4, lwd = 2)
        axis(1, at = q, labels = c("Q1", "Q2", "Q3"), col.axis = 4)
        mtext("Created by Devan Becker", 
            side = 1, line = 3, adj = 1, cex = 0.75)
        mtext("Github: DBecker7/DB7_TeachingApps", 
            side = 1, line = 4, adj = 1, cex = 0.75)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
