library(ggplot2)
library(patchwork)

Distribution <- c("Normal", "Gamma", "Binomial", "Poisson", "Exponential", 
    "Beta", "Lognormal")[7]
Theo <- c("Normal", "Gamma")[2]
mu <- 0.4
sigma <- 0.2
n <- 50
bins <- 15

if(Distribution == "Normal"){
    ysamp <- sort(rnorm(n, mu, sigma))
    subtit <- bquote(mu*" = "*.(mu)*", "*sigma^2*"="*.(sigma^2)*", n="*.(n))
} else if(Distribution == "Gamma"){
    if(mu <= 0){
        mu <- 0.001
    }
    shape <- mu^2/sigma^2
    rate <- mu/sigma^2
    ysamp <- sort(rgamma(n, shape, rate))
    subtit <- bquote(alpha*"="*.(round(shape, 3))*", "*beta*"="*.(round(rate, 3))*", n="*.(n))
} else if(Distribution == "Binomial"){
    if(mu <= 0) mu <- 0.001
    p <- 1 - sigma^2/mu
    if(p <= 0) p <- 0.001
    if(p >= 1) p <- 0.999
    m <- round(mu^2/(mu - sigma^2))
    if(m <= 0) m <- 1
    ysamp <- sort(rbinom(n, m, p))
    subtit <- bquote(p*"="*.(round(p, 3))*", ntrials="*.(round(m, 3))*", n="*.(n))
} else if(Distribution == "Poisson"){
    if(mu <= 0) mu <- 0.001
    ysamp <- sort(rpois(n, mu))
    subtit <- bquote(lambda*"="*.(round(mu, 3))*", n="*.(n))
} else if(Distribution == "Exponential"){
    if(mu <= 0) mu <- 0.001
    lambda <- 1/mu
    ysamp <- sort(rgamma(n, lambda))
    subtit <- bquote(lambda*"="*.(round(lambda, 3))*", n="*.(n))
} else if(Distribution == "Beta"){
    if(mu <= 0) mu <- 0.001
    if(mu >= 1) mu <- 0.999
    shape1 <- mu*(mu*(1-mu)/sigma^2 - 1)
    shape2 <- shape1*(1-mu)/mu
    if(shape1 <= 0) shape1 <- 0.001
    if(shape2 <= 0) shape2 <- 0.001
    ysamp <- sort(rbeta(n, shape1, shape2))
    subtit <- bquote(alpha*"="*.(round(shape1, 3))*", "*beta*"="*.(round(shape2, 3))*", n="*.(n))
} else if(Distribution == "Lognormal"){
    if(mu <= 0) mu <- 0.001
    newmu <- log(mu/sqrt(sigma^2/mu^2 + 1))
    newsig2 <- log(sigma^2/mu^2 + 1)
    ysamp <- sort(rlnorm(n, newmu, sqrt(newsig2)))
}

xbar <- mean(ysamp, na.rm = TRUE)
sd2 <- sd(ysamp, na.rm = TRUE)

if(Theo == "Normal"){
    xquant <- qnorm(ppoints(ysamp), xbar, sd2)
    x1 <- qnorm(c(0.25,0.75), xbar, sd2)
    xtheo <- seq(from = min(ysamp, na.rm = TRUE), to = max(ysamp, na.rm = TRUE), 
        length.out = 200)
    dtheo <- dnorm(xtheo, xbar, sd2)
    theotit <- paste0("Histogram of ", Distribution, " sample with N(", 
        round(xbar, 3), ",", round(sd2^2, 3), ") Density Curve")
} else if(Theo == "Gamma"){
    alphabar <- xbar^2/sd2^2
    betabar <- xbar/sd2^2
    xquant <- qgamma(ppoints(ysamp), shape = alphabar, rate = betabar)
    xtheo <- seq(from = min(ysamp, na.rm = TRUE), to = max(ysamp, na.rm = TRUE), 
        length.out = 200)
    dtheo <- dgamma(xtheo, shape = alphabar, rate = betabar)
    theotit <- paste0("Histogram of ", Distribution, " sample with Gamma(", 
        round(alphabar, 3), ",", round(betabar^2, 3), ") Density Curve")
}

y1 <- quantile(ysamp, probs = c(0.25,0.75), names = FALSE)
slope <- diff(y1)/diff(x1)
int <- y1[1L] - slope*x1[1L]

title <- paste0("QQ-Plot for Sample of ", Distribution, " versus Theoretical ", Theo)

ghist <- ggplot() + 
    geom_histogram(aes(x = ysamp, y = ..density..), 
        bins = bins, fill = "lightgrey", colour = 1) +
    geom_line(aes(x = xtheo, y = dtheo), colour = "red", size = 1) +
    theme_bw() +
    labs(title = theotit, y = "Density", x = "Sample")

gqq <- ggplot() + geom_point(aes(x = xquant, y = ysamp)) +
    geom_abline(aes(intercept = int, slope = slope), colour = 3, size = 1) +
    labs(title = title, subtitle = subtit, 
        x = paste0("Theoretical ", Theo, " Quantiles"),
        y = "Sample Quantiles") +
    theme_bw()

ghist / gqq

