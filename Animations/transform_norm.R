library(dplyr)
library(gganimate)

x1 <- seq(-3,3,0.1)
y1 <- dnorm(x1)
x2 <- exp(x1)
y2 <- y1/exp(x1)

# Testing
#plot(x2, y2)
#points(x1, y1)
#curve(dlnorm(x), add = TRUE, col = 2)

mydf <- bind_rows(
    data.frame(x = x1, y = y1, trans = "norm", 
        col = case_when(x1 == -1 ~ 1,
            x1 == 0 ~ 2, 
            x1 == 1 ~ 3, 
            x1 == 2 ~ 4, TRUE ~ 0)),
    data.frame(x = x2, y = y2, trans = "lnorm", 
        col = case_when(x2 == exp(-1) ~ 1,
            x2 == exp(0) ~ 2, 
            x2 == exp(1) ~ 3, 
            x2 == exp(2) ~ 4, TRUE ~ 0))
)

ggplot(mydf, aes(x = x, y = y,
        colour = factor(col), size = col > 0)) + 
    theme_minimal() + 
    scale_colour_manual(values = c(1,2,4,6,7)) + 
    scale_x_continuous(breaks = c(exp(-1), 0, exp(0), 
            2, exp(2), seq(-3,25,1)[-5]), 
        labels = c("e^-1", "0", "e^0", "2", "e^2", 
            seq(-3,25,1)[-5])) +
    transition_states(states = trans, transition_length = 1/2,
        state_length = 1/2) +
    stat_function(fun = dnorm,  
        colour = 4, n = 500, size = 1) +
    stat_function(fun = dlnorm, 
        colour = 2, n = 500, size = 1) +
    geom_point() +
    coord_cartesian(xlim = c(-3,7)) +
    theme(legend.position = "none", 
        title = element_text(size = 14)) +
    annotate(geom = "text", x = c(0, exp(-1)), 
        y = c(0.4,0.66), 
        label = c("y1 = dnorm(x)", "y2 = y1/exp(x)"), 
        hjust = c(1.1,-0.1), size = 6, colour = c(4,2)) +
    labs(y = "Density Function", 
        title = "Transformation to Lognormal",
        subtitle = paste0("The red curve is dlnorm(x1),",
            "the points are transformed",
            "\nas x2 = exp(x1); y2 = dnorm(x1)/exp(x1)."))

anim_save("Animations/transform_norm.gif")