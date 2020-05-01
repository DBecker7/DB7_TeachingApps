#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(patchwork)

Distributions <- c("Normal(mu, sigma)", "Gamma(alpha, beta)", "Binomial(n,p)", 
    "Poisson(lambda)", "Exponential(lambda)", "Beta(alpha, beta)", 
    "Lognormal(mu, sigma)")
Theos <- c("Normal", "Gamma", "Binomial", "Exponential")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("QQ-plots for testing distributional assumptions."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "rdist", 
                label = "Distribution to Sample From", 
                choices = Distributions, 
                selected = Distributions[1]),
            selectInput(inputId = "qdist", 
                label = "Theoretical Distr. to Compare To", 
                choices = Theos, 
                selected = Theos[1]),
            "Note: Parameters may be truncated to appropriate parameter space.",
            sliderInput(inputId = "mu", label = "First Parameter", 
                min = -2, max = 20, value = 0.5, step = 0.05),
            sliderInput(inputId = "sigma", label = "Second Parameter", 
                min = -2, max = 20, value = 1.5, step = 0.05),
            "Changing the number of bins does not generate new data.",
            sliderInput(inputId = "bins", label = "Histogram Bins", 
                min = 5, max = 40, value = 10, step = 1),
            sliderInput(inputId = "n", label = "Sample Size", 
                min = 5, max = 500, value = 50, step = 1),
            actionButton("doit", "Click Me for New Data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot", height = "700px"),
            tags$div(HTML("Some questions:<br><ul>
	<li>Which distribution(s) can NEVER be approximated by the normal distribition?</li>
	<li>What do discrete variables look like in the QQ plot?</li>
	<li>Do all normal samples look normal in a QQ plot? How much deviation should we tolerate in the QQ plot before we say that data isn't normal?</li>
</ul>"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rdistr <- reactive({
        dummy <- input$doit
        n <- input$n
        if(input$rdist == "Normal(mu, sigma)"){
            sigma <- ifelse(input$sigma <= 0, 0.001, input$sigma)
            mu <- input$mu
            ysamp <- sort(rnorm(n, mu, sigma))
            subtit <- bquote(mu*" = "*.(mu)*", "*sigma^2*"="*.(sigma^2)*
                    ", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Gamma(alpha, beta)"){
            shape <- input$mu
            if(shape <= 0) shape <- 0.001
            rate <- input$sigma
            if(rate <= 0) rate <- 0.001
            ysamp <- sort(rgamma(n, shape, rate))
            subtit <- bquote(alpha*"="*.(round(shape, 3))*", "*beta*
                    "="*.(round(rate, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Binomial(n,p)"){
            sigma <- input$sigma
            mu <- input$mu
            if(mu <= 0) mu <- 0.001
            p <- input$mu
            if(p <= 0) p <- 0.001
            if(p >= 1) p <- 0.999
            m <- round(sigma)
            if(m <= 0) m <- 1
            ysamp <- sort(rbinom(n, m, p))
            subtit <- bquote(p*"="*.(round(p, 3))*", ntrials="*.(round(m, 3))*
                    ", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Poisson(lambda)"){
            mu <- input$mu
            if(mu <= 0) mu <- 0.001
            ysamp <- sort(rpois(n, mu))
            subtit <- bquote(lambda*"="*.(round(mu, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Exponential(lambda)"){
            mu <- input$mu
            if(mu <= 0) mu <- 0.001
            lambda <- mu
            ysamp <- sort(rgamma(n, lambda))
            subtit <- bquote(lambda*"="*.(round(lambda, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Beta(alpha, beta)"){
            shape1 <- mu
            shape2 <- sigma
            if(shape1 <= 0) shape1 <- 0.001
            if(shape2 <= 0) shape2 <- 0.001
            ysamp <- sort(rbeta(n, shape1, shape2))
            subtit <- bquote(alpha*"="*.(round(shape1, 3))*", "*beta*"="
                *.(round(shape2, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Lognormal(mu, sigma)"){
            sigma <- ifelse(input$sigma <= 0, 0.001, input$sigma)
            mu <- input$mu
            ysamp <- sort(rlnorm(n, mu, sqrt(sigma)))
            subtit <- bquote(mu*"="*.(round(mu, 3))*", "*sigma^2*"="
                *.(round(sigma^2, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        }
    })
    
    
    output$distPlot <- renderPlot({
        
        sampsub <- rdistr()
        ysamp <- sampsub$ysamp
        subtit <- sampsub$subtit
        xbar <- mean(ysamp, na.rm = TRUE)
        sd2 <- sd(ysamp, na.rm = TRUE)
        
        if(input$qdist == "Normal"){
            xquant <- qnorm(ppoints(ysamp), xbar, sd2)
            x1 <- qnorm(c(0.25,0.75), xbar, sd2)
            xtheo <- seq(from = min(ysamp, na.rm = TRUE), to = max(ysamp, na.rm = TRUE), 
                length.out = 200)
            dtheo <- dnorm(xtheo, xbar, sd2)
            theotit <- paste0("Histogram of ", input$rdist, " sample with N(", 
                round(xbar, 3), ",", round(sd2^2, 3), ") Density Curve")
        } else if(input$qdist == "Gamma"){
            alphabar <- xbar^2/sd2^2
            betabar <- xbar/sd2^2
            xquant <- qgamma(ppoints(ysamp), shape = alphabar, rate = betabar)
            x1 <- qgamma(c(0.25,0.75), shape = alphabar, rate = betabar)
            xtheo <- seq(from = min(ysamp, na.rm = TRUE), to = max(ysamp, na.rm = TRUE), 
                length.out = 200)
            dtheo <- dgamma(xtheo, shape = alphabar, rate = betabar)
            theotit <- paste0("Histogram of ", input$rdist, " sample with Gamma(", 
                round(alphabar, 3), ",", round(betabar^2, 3), ") Density Curve")
        } else if(input$qdist == "Exponential"){
            lambdabar <- 1/xbar
            xquant <- qexp(ppoints(ysamp), lambdabar)
            x1 <- qexp(c(0.25,0.75), lambdabar)
            xtheo <- seq(from = min(ysamp, na.rm = TRUE), to = max(ysamp, na.rm = TRUE), 
                length.out = 200)
            dtheo <- dexp(xtheo, lambdabar)
            theotit <- paste0("Histogram of ", input$rdist, " sample with Exp(", 
                round(lambdabar, 3), ") Density Curve")
        } else if(input$qdist == "Binomial"){
            pbar <- 1 - ((input$n - 1)/input$n)*sd2^2/xbar
            mbar <- max(1, round(xbar^2/(pbar), 0))
            if(pbar <= 0) pbar <- 0.001
            if(pbar >= 1) pbar <- 0.999
            xquant <- qbinom(ppoints(ysamp), mbar, pbar)
            x1 <- qbinom(c(0.25,0.75), mbar, pbar)
            xtheo1 <- seq(from = 0, to = ceiling(max(ysamp)), 
                by = 1)
            dtheo1 <- dbinom(xtheo1, mbar, pbar)
            dtheo2 <- matrix(c(rep(0, length(dtheo1)), dtheo1, dtheo1), 
                nrow = 3, byrow = TRUE)
            dtheo <- c(as.vector(dtheo2), 0)
            xtheo <- c(rep(xtheo1, each = 3)[-1], max(xtheo1), max(xtheo1))
            theotit <- paste0("Histogram of ", input$rdist, " sample with Binom(", 
                round(pbar, 3), ",", round(mbar, 2), ") Density Curve")
        }
        
        y1 <- quantile(ysamp, probs = c(0.25,0.75), names = FALSE)
        slope <- diff(y1)/diff(x1)
        int <- y1[1L] - slope*x1[1L]
        
        title <- paste0("QQ-Plot for Sample of ", input$rdist, 
            " versus Theoretical ", input$qdist)
        
        ghist <- ggplot() + 
            geom_histogram(aes(x = ysamp, y = ..density..), 
                bins = input$bins, fill = "lightgrey", colour = 1) +
            geom_line(aes(x = xtheo, y = dtheo), colour = "red", size = 1) +
            theme_bw() +
            labs(title = theotit, 
                subtitle = "Parameters of red curve estimated by Method of Moments (Covered in SS3858)",
                y = "Density", x = "Sample")
        
        gqq <- ggplot() + geom_point(aes(x = xquant, y = ysamp)) +
            geom_abline(aes(intercept = int, slope = slope), colour = 3, size = 1) +
            labs(title = title, subtitle = subtit, 
                x = paste0("Theoretical ", input$qdist, " Quantiles"),
                y = "Sample Quantiles") +
            theme_bw()
        
        ghist / gqq
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
