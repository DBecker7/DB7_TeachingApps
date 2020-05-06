# DB7_TeachingApps

Some applications for self-discovery of statistical concepts and rules-of-thumb.

To run these, you may need to install `dplyr` and `ggplot2` packages. If you get a `... not found` message, you're probably missing a package.

You can copy and paste the code from here, or you can save <a href="https://github.com/DBecker7/DB7_TeachingApps/blob/master/ScriptToRunApps.R">`ScriptToRunApps.R`</a> to your computer and run them from there.

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

### Other

**CLT:** This app is a classic, and there's no reason for me to top it. <a href="http://onlinestatbook.com/stat_sim/sampling_dist/">http://onlinestatbook.com/stat_sim/sampling_dist/</a>

**Importance of visualizations:** Ya can't beat the datasauRus from https://www.autodeskresearch.com/publications/samestats. It's an update of Anscombe's quartet with some even more interesting features.


<img src="Readmefigs/datasauRus.png" align="right" width="400">

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
        facet_wrap(~ dataset, ncol = 4) +
        labs(title = "All have same summary statistics")
```


**Box plots hide shapes:** From the same people who brought you the datasaurus dozen!

<img src="Readmefigs/box_plots.png" align="right" width="300">

```r
library(datasauRus)
library(ggplot2)
theme_set(theme_bw()) # as always
library(dplyr)
library(patchwork)
library(tidyr)

# change defaults to something I like
myhist <- function(colour = 1, fill = "lightgrey", bins = 30, ...){ 
    geom_histogram(colour = colour, fill = fill, bins = bins, ...)
}

data("box_plots")
# to make my code smaller
box_plots_long <- pivot_longer(data = box_plots, cols = 1:5,
    names_to = "dataset", values_to = "x")

boxes <- ggplot(box_plots_long, aes(x = x)) + 
    geom_boxplot() + facet_wrap(~ dataset, ncol = 1)
histos <- ggplot(box_plots_long, aes(x = x)) + 
    myhist() + facet_wrap(~ dataset, ncol = 1)
boxes + histos + # patchwork magic
    plot_annotation(
        title = "Boxplots hide more complicated shapes"
    )
```


## TODO

- Two-Way Tables (might just be teaching materials).
- Transformations of Random Variables
    - Demonstrate at least two x-values, show that the y-values stay the same
- Marginal and conditional distributions (might just be teaching materials)
- Measures of spread
    - same idea as above, but generate data to have a pre-specified IQR and variance.
    - Method: generate data within quartiles according to a transformed beta distribition within Q2:Q3 and transformed exponential for the upper tail (mirrored across median). As variance slider changes, change parameters to select exact variance (maybe alpha = 1, beta increases by a fixed amount for Beta, lambda increases for exponential to make variance increase match slider).


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






