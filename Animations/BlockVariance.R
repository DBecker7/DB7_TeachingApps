set.seed(2112)
library(gganimate)
library(dplyr)
g1 <- rnorm(400, 0, 1.5)
g2 <- rnorm(400, 4, 1.5)

# Attempt 1: Inverse
ggplot() + 
    geom_histogram(aes(g1), fill = 2, alpha = 0.5, colour = NA, binwidth = 0.25, boundary = 0) + 
    geom_histogram(aes(g2), fill = 4, alpha = 0.5, colour = NA, binwidth = 0.25, boundary = 0) +
    geom_histogram(aes(x = c(g1,g2), y = -..count..), fill = NA, binwidth = 0.25, boundary = 0)



g1dens <- density(g1, from = min(g1, g2), to = max(g1,g2), n = 400)
g2dens <- density(g2, from = min(g1, g2), to = max(g1,g2), n = 400, bw = g1dens$bw)
g3dens <- density(c(g1,g2), from = min(g1, g2), to = max(g1,g2), n = 400, bw = g1dens$bw)
all.equal(g1dens$x, g2dens$x)

gnames <- c(paste0("Group 1: Var=", round(var(g1), 3)), 
    paste0("Group 2: Var=", round(var(g2), 3)), 
    paste0("Group 3 (Combined): Var=", round(var(c(g1, g2)), 3)))
allg <- data.frame(x = rep(g1dens$x, 3), y = c(g1dens$y, g2dens$y, g3dens$y),
    group = rep(gnames, each = length(g1dens$x)))

ggplot(allg, aes(x = x, y = y, colour = group)) + geom_line(size = 1) 


allg$frame <- 1

allg2 <- allg
allg2$x <- c(g1dens$x - mean(g1), g2dens$x - mean(g2), g3dens$x - mean(c(g1,g2)))
allg2$frame <- 2
ggplot(allg2, aes(x = x, y = y, colour = group)) + geom_line()

allt <- dbind_rows(allg, allg2)

library(gganimate)
ggplot(allt, aes(x = x, y = y, colour = group)) + 
    geom_line() #+
    #transition_states(frame)



allg3 <- data.frame(x = rep(g1dens$x, 3), y = rep(g3dens$y, 3), 
    group = rep(gnames, each = length(g1dens$x)), frame = 0)

all3 <- bind_rows(allg, allg2, allg3)
ggplot(all3, aes(x = x, y = y, colour = group)) + 
    geom_line(size = 1.5) +
    scale_colour_manual(values = c(2, 4, 1)) + 
    transition_states(frame, wrap = FALSE) +
    theme_bw() +
    theme(legend.position = "bottom", 
        axis.title = element_text(size = 14), 
        title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11)) + 
    labs(x = "x", y = "Density", colour = NULL,
        title = "Blocking reduces variance",
        subtitle = "Individual densities have smaller variance than combined.") 


anim_save("Animations/BlockVariance.gif")


