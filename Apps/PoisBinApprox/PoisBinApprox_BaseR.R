# Poisson approximation to the binomial

# Set n and p ----

# Should be moderately large
n <- 400
# Rule of thumb: p < 0.05 means Poisson works
# Poisson is much easier to calculate
p <- 0.01


x <- 0:n

ybin <- dbinom(x = x, size = n, prob = p)
ypois <- dpois(x = x, lambda = n * p)

xnorm <- seq(0, n, length.out = n * 10)
ynorm <- dnorm(x = xnorm, 
    mean = n * p, 
    sd = sqrt(n * p * (1 - p)))

# Since p is small, we don't really need to cover the whole range
# I'm using qbinom to find the 99.9th percentile. This way,
# only the relevant regions will be plotted.
xlims <- c(0, qbinom(0.999, size = n, prob = p))

# y limits should cover all of the distributions
ylims <- c(0, max(ybin, ypois, ynorm))

# Subtract 0.5 so that the bars are centered at the whole number.
# If I didn't do this, the first bar would go from 0 to 1 rather than 
# -0.5 to 0.5.
plot(x = x - 0.5, y = ybin, 
    type = "s", # step function
    xlim = xlims, ylim = ylims,
    lwd = 2) # lwd = line width
# add vertical lines
lines(x = x - 0.5, y = ybin, type = "h",
    lwd = 2) 

# Add poisson distribution
# I'm offsetting it slightly so it's easier to tell apart
lines(x = x - 0.5 + 0.03, y = ypois, type = "s", col = 2, lwd = 2)
lines(x = x - 0.5 + 0.03, y = ypois, type = "h", col = 2, lwd = 2)

# Add normal distribution
lines(x = xnorm, y = ynorm, col = 4, lwd = 2)

# Manually add legend
legend("topright",
    legend = c("Binomial", "Poisson", "Normal"),
    col = c(1,2,4),
    lwd = 2)
