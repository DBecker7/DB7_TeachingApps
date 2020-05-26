# QQDistrFitting
# How QQ plots can evaluate distribution assumptions.

library(shiny)
library(ggplot2)
library(patchwork)

Distributions <- c("Normal(mu, sigma)", "Gamma(alpha, beta)", "Binomial(n,p)", 
    "Poisson(lambda)", "Exponential(lambda)", "Beta(alpha, beta)", 
    "Lognormal(mu, sigma)")
Theos <- c("Normal", "Gamma", "Binomial", "Exponential", "Beta")


parameter_tabs <- tagList(
    tags$style("#params { display:none; }"),
    tabsetPanel(id = "params",
        tabPanel("Normal",
            sliderInput("mean", "mean", min = -5, max = 5, 
                value = 1, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("sd", "standard deviation", min = 0.01, max = 10, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("Lognormal",
            sliderInput("lmean", "mean", min = -5, max = 5, 
                value = 1, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("lsd", "standard deviation", min = 0.01, max = 10, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("Uniform", 
            sliderInput("min", "min", min = -5, max = 5, 
                value = 0, step = 0.1,
                animate = list(interval = 600)),
            sliderInput("max", "max", min = -5, max = 5, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("Exponential",
            sliderInput("rate", "rate", min = 0, max = 20, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("Poisson",
            sliderInput("prate", "rate", min = 0, max = 30, 
                value = 1, step = 0.1,
                animate = list(interval = 600))
        ),
        tabPanel("Binomial",
            sliderInput("p", "p", min = 0, max = 1, 
                value = 0.5, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("nbin", "n", min = 1, max = 200, 
                value = 20, step = 1,
                animate = list(interval = 600))
        ),
        tabPanel("Gamma",
            sliderInput("gshape", "shape", min = 0, max = 20, 
                value = 1, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("grate", "rate",  min = 0, max = 20, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("Beta",
            sliderInput("bshape1", "shape 1", min = 0, max = 20, 
                value = 1, step = 0.01,
                animate = list(interval = 600)),
            sliderInput("bshape2", "shape 2", min = 0, max = 20, 
                value = 1, step = 0.01,
                animate = list(interval = 600))
        )
    )
)

ui <- fluidPage(
    
    # Application title
    titlePanel("QQ-Plots for testing distributional assumptions"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("rdist", "Distribution", 
                choices = c("Normal", "Uniform", "Exponential", "Gamma", 
                    "Beta", "Binomial", "Poisson")
            ),
            parameter_tabs,
            selectInput(inputId = "qdist", 
                label = "Theoretical Distr. to Compare To", 
                choices = Theos, 
                selected = Theos[1]),
            "Changing the number of bins does not generate new data.",
            sliderInput(inputId = "bins", label = "Histogram Bins", 
                min = 5, max = 40, value = 10, step = 1),
            sliderInput(inputId = "n", label = "Sample Size", 
                min = 5, max = 500, value = 50, step = 1),
            actionButton("doit", "Click Me for New Data")
        ),
        
        mainPanel(
            plotOutput("distPlot", height = "600px"),
            tags$div(HTML("Some questions:<br><ul>
	<li>Which distribution(s) can NEVER be approximated by the normal distribition?</li>
	<li>What do discrete variables look like in the QQ plot?</li>
	<li>Do all normal samples look normal in a QQ plot? How much deviation should we tolerate in the QQ plot before we say that data isn't normal?</li>
	<li>For each distribution, find a parameter combination where the resulting sample is well approximated by the normal distribution. Try the same with the exponential distribution.</li>
	<li>Set the theortical distribution to Beta, then try and find a parameter combination for each other distribution such that the Beta distribution approximates it well.</li>
</ul>"))
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$rdist, {
        updateTabsetPanel(session, "params", selected = input$rdist)
    }) 
    
    rdistr <- reactive({
        dummy <- input$doit
        n <- input$n
        if(input$rdist == "Normal"){
            ysamp <- sort(rnorm(n, input$mean, input$sd))
            subtit <- bquote(input$mean*" = "*.(input$mean)*", "*input$sd^2*"="*.(input$sd^2)*
                    ", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Gamma"){
            shape <- input$gshape
            if(shape <= 0) shape <- 0.001
            rate <- input$grate
            if(rate <= 0) rate <- 0.001
            ysamp <- sort(rgamma(n, shape, rate))
            subtit <- bquote(alpha*"="*.(round(shape, 3))*", "*beta*
                    "="*.(round(rate, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Binomial"){
            ysamp <- sort(rbinom(input$n, input$nbin, input$p))
            subtit <- bquote(input$p*"="*.(round(input$p, 3))*", ntrials="*.(round(input$nbin, 3))*
                    ", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Poisson"){
            ysamp <- sort(rpois(n, input$prate))
            subtit <- bquote(lambda*"="*.(round(input$prate, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Exponential"){
            lambda <- input$rate
            ysamp <- sort(rexp(n, lambda))
            subtit <- bquote(lambda*"="*.(round(lambda, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Beta"){
            shape1 <- input$bshape1
            shape2 <- input$bshape2
            ysamp <- sort(rbeta(n, shape1, shape2))
            subtit <- bquote(alpha*"="*.(round(shape1, 3))*", "*beta*"="
                *.(round(shape2, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Lognormal"){
            sigma <- input$lsd
            mu <- input$lmean
            ysamp <- sort(rlnorm(n, mu, sqrt(sigma)))
            subtit <- bquote(mu*"="*.(round(mu, 3))*", "*sigma^2*"="
                *.(round(sigma^2, 3))*", n="*.(n))
            list(ysamp = ysamp, subtit = subtit)
        } else if(input$rdist == "Uniform"){
            ysamp <- sort(runif(n, input$min, input$max))
            subtit <- bquote("min="*.(round(input$min, 3))*", max="
                *.(round(input$max, 3))*", n="*.(n))
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
        } else if(input$qdist == "Beta"){
            s1bar <- xbar*(xbar*(1-xbar)/sd2^2 - 1)
            s2bar <- (1 - xbar)*(xbar*(1-xbar)/sd2^2 - 1)
            if(s1bar < 0) s1bar <- 0.0001
            if(s2bar < 0) s2bar <- 0.0001
            suppressWarnings({
            xquant <- qbeta(ppoints(ysamp), shape1 = s1bar, shape2 = s2bar)
            x1 <- qbeta(c(0.25,0.75), shape1 = s1bar, shape2 = s2bar)
            xtheo <- seq(from = min(ysamp, na.rm = TRUE), to = max(ysamp, na.rm = TRUE), 
                length.out = 200)
                dtheo <- dbeta(xtheo, shape1 = s1bar, shape2 = s2bar)
            })
            theotit <- paste0("Histogram of ", input$rdist, " sample with Beta(", 
                round(s1bar, 2), ", ", round(s2bar, 2), ") Density Curve")
        }
        
        y1 <- quantile(ysamp, probs = c(0.25,0.75), names = FALSE)
        slope <- diff(y1)/diff(x1)
        int <- y1[1L] - slope*x1[1L]
        
        title <- paste0("QQ-Plot for Sample of ", input$rdist, 
            " versus Theoretical ", input$qdist)
        
        ghist <- ggplot() + 
            geom_histogram(aes(x = ysamp, y = ..density..), 
                bins = input$bins, fill = "lightgrey", colour = 1, boundary = 0) +
            geom_line(aes(x = xtheo, y = dtheo), colour = "red", size = 1) +
            theme_bw() + 
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14)) +
            labs(title = theotit, 
                subtitle = "Parameters of red curve estimated by Method of Moments (Covered in SS3858)",
                y = "Density", x = "Sample")
        
        gqq <- ggplot() + geom_point(aes(x = xquant, y = ysamp)) +
            geom_abline(aes(intercept = int, slope = slope), colour = 3, size = 1) +
            labs(title = title, subtitle = subtit, 
                x = paste0("Theoretical ", input$qdist, " Quantiles"),
                y = "Sample Quantiles") +
            theme_bw() + 
            theme(title = element_text(size = 14), 
                axis.text = element_text(size = 12)) 
        
        ghist / gqq +
            plot_annotation(caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
