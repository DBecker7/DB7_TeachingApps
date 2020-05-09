# Shiny Teaching Apps

Some applications for self-discovery of statistical concepts and rules-of-thumb. If you use these, let me know! Any suggestions for improvement can be raised in the Issues tab on GiHub. 

To run these, you may need to install `dplyr`, `ggplot2`, and `patchwork` packages. If you get a `... not found` message, you're probably missing a package.

You can copy and paste the code from here, or you can save <a href="https://github.com/DBecker7/DB7_TeachingApps/blob/master/ScriptToRunApps.R">`ScriptToRunApps.R`</a> to your computer and run them from there.

----

### PoisBinApprox

<img src="ReadmeFigs/PoisBinApprox.jpg" width="400" align="right">

- The Poisson (and Normal) approximations to the binomial distributions. 
- Includes sliders for sample size and probability of success. 
- The Poisson distribution is useful when *p* is small, while the normal distribution is useful when both *np* and *n(1-p)* are larger than, say, 10 or so (this is a rule-of-thumb, not some magical value).


```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "PoisBinApprox")
```

### QQDistrFitting


<img src="ReadmeFigs/QQDistrFitting.jpg" width="400" align="right">

- Demonstration of the usefulness of QQ plots in assessing distributional assumptions. 
- The app shows the histogram (with estimated density overlaid) and the qq-plot (which does *not* need an estimate of the parameters).
- The theoretical distribution can be changed to something other than Normal (currently just Gamma).

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "QQDistrFitting")
```

### ScatterCorr

<img src="ReadmeFigs/ScatterCorr2.jpg" width="400" align="right">

- Shows what different correlations look like. 
- Allows you to change the slope *independently* of the correlation, demonstrating that they're not the same thing. 
- Allows for animation of the slope and correlation.
- Doesn't generate new data until specified, so animations allow the student to watch the correlation change.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "ScatterCorr")
```



### MeanLessMeansLeft

<img src="ReadmeFigs/MeanLessMeansLeft.jpg" width="400" align="right">

- How the mean and median affect the skew. 
- Uses a Gamma distribution, so some parameter combos lead to a singularity at 0. 
- Please note that it took me a while to figure out how to (efficiently) generate a Gamma distribution with a pre-specified mean and median.


```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "MeanLessMeansLeft")
```



### DensHist

<img src="ReadmeFigs/DensHist.jpg" width="400" align="right">

- Exploration of the connection between binwidth and bandwidth.
- A density plot can be found as the limit as n approaches infinity and te binwidth approaches 0. 
- This tool lets students explore that while also exploring how the histogram changes with bin width and the density plot changes with bandwidth.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "DensHist")
```

### MeasureSpread

<img src="ReadmeFigs/MeasureSpread.jpg" width="400" align="right">

- Explore the relationship between IQR and standard deviation.
- For the normal distribution, the IQR and sd have a consistent relationship. In particular, the sd is constant factor times the IQR, regardless of what the sd is!
- For real data, almost any (IQR, sd) pair is possible. 
    - I wrote a function to fix the IQR and perturb the data until I get the sd that I want.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "MeasureSpread")
```

### transform_norm

<img src="Animations/transform_norm.gif" align="right" width="400">

```r
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
    transition_states(states = trans, 
        transition_length = 1/2, state_length = 1/2) +
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
```

### Credit where credit is due

**CLT:** This app is a classic, and there's no reason for me to top it. <a href="http://onlinestatbook.com/stat_sim/sampling_dist/">http://onlinestatbook.com/stat_sim/sampling_dist/</a>

**Importance of visualizations:** Ya can't beat the datasauRus dozen from https://www.autodeskresearch.com/publications/samestats. It's an update of Anscombe's quartet with even more interesting features. It's also a great way to demonstrate some tidyverse/ggplot2 functions!

The following code chunks are both standalone scripts.


<img src="ReadmeFigs/datasauRus3.png" align="right" width="350">

```r
# Load some packages
library(datasauRus)
library(ggplot2)
# as always
theme_set(theme_bw()) 
library(dplyr)

# All of these plots have the same summary statistics,
    # including xbar, ybar, sd_x, sd_y, and correlation
data("datasaurus_dozen")
# remove a dataset for 3x4 plot
filter(datasaurus_dozen,
    dataset != "slant_up") %>% 
    ggplot(aes(x = x, y = y)) + 
        geom_point() + 
        facet_wrap(~ dataset, ncol = 3) +
        labs(title = "All have same summary statistics")

datasaurus_dozen %>% 
    group_by(dataset) %>% 
    summarise(m_x = mean(x), m_y = mean(y),
        s_x = sd(x), s_y = sd(y), r = cor(x,y)) %>% 
    knitr::kable(digits = 3)
```

|dataset    |    m_x|    m_y|    s_x|    s_y|      r|
|:----------|------:|------:|------:|------:|------:|
|away       | 54.266| 47.835| 16.770| 26.940| -0.064|
|bullseye   | 54.269| 47.831| 16.769| 26.936| -0.069|
|circle     | 54.267| 47.838| 16.760| 26.930| -0.068|
|dino       | 54.263| 47.832| 16.765| 26.935| -0.064|
|dots       | 54.260| 47.840| 16.768| 26.930| -0.060|
|h_lines    | 54.261| 47.830| 16.766| 26.940| -0.062|
|high_lines | 54.269| 47.835| 16.767| 26.940| -0.069|
|slant_down | 54.268| 47.836| 16.767| 26.936| -0.069|
|slant_up   | 54.266| 47.831| 16.769| 26.939| -0.069|
|star       | 54.267| 47.840| 16.769| 26.930| -0.063|
|v_lines    | 54.270| 47.837| 16.770| 26.938| -0.069|
|wide_lines | 54.267| 47.832| 16.770| 26.938| -0.067|
|x_shape    | 54.260| 47.840| 16.770| 26.930| -0.066|

**Boxplots hide shapes:** From the same people who brought you the datasaurus dozen!

<img src="ReadmeFigs/box_plots.png" align="right" width="350">

```r
# I need a surprising amount of packages for this
library(datasauRus)
library(ggplot2)
theme_set(theme_bw()) # as always
library(dplyr)
library(patchwork)
library(tidyr)

data("box_plots")
# to make my code more compact (faceting)
box_plots_long <- pivot_longer(data = box_plots, cols = 1:5,
    names_to = "dataset", values_to = "x")

boxes <- ggplot(box_plots_long, aes(x = x)) + 
    geom_boxplot() + 
    facet_wrap(~ dataset, ncol = 1)
histos <- ggplot(box_plots_long, aes(x = x)) + 
    geom_histogram(colour = 1, fill = "lightgrey", bins = 30) + 
    facet_wrap(~ dataset, ncol = 1)
    
# patchwork is a magical package
boxes + histos +
    plot_annotation(
        title = "Boxplots hide more complicated shapes"
    )
```


----


## TODO

- Two-Way Tables (might just be teaching materials).
- Transformations of Random Variables
    - Demonstrate transformation of at least two x-values
- Marginal and conditional distributions (might just be teaching materials)


# Spatial Stats Apps

The following apps are for my own exploration of spatial statistics. Simulating the data and exploring the parameters is my favourite way to comprehend the underlying concepts.

### Gaussian Processes

GPs are vital to any spatial processes with a Gaussian term, so this app helps to understand how the parameters affect the process.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "SpatialFun/GausProcess_Matern")
```

### Gaussian Fields

Like a Gaussian Process, but a field instead. Still based on Matern covariance.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "SpatialFun/GausField_Matern")
```


### TODO

- Exploration of INLA parameters (esp. penalized complexity).
- Description of INLA methods.
- Multiple realizations of a single gaussian field (both in terms of Geostatistics and Spatial Point Processes, maybe with Areal data).
- Generate random tesselations, show realizations of a continuous spatial process within those regions.
- K-function: slider for r with a circle of radius r around each point, updating the number of points as the circle expands.
    - Inhomogenous version: #points/integral over circle






