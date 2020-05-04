# DB7_TeachingApps

Some applications for self-discovery of statistical concepts and rules-of-thumb.

To run these, you may need to install `dplyr` and `ggplot2` packages. If you get a `... not found` message, you're probably missing a package.

### PoisBinApprox

The Poisson (and Normal) approximations to the binomial distributions. Includes sliders for sample size and probability of success. The Poisson distribution is useful when *p* is small, while the normal distribution is useful when both *np* and *n(1-p)* are larger than, say, 10 or so (this is a rule-of-thumb, not some magical value).


```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "PoisBinApprox")
```

<img src="ReadmeFigs/PoisBinApprox.jpg" width="400">

### QQDistrFitting

Demonstration of the usefulness of QQ plots in assessing distributional assumptions. The app shows the histogram (with estimated density overlaid) and the qq-plot (which does *not* need an estimate of the parameters).

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "QQDistrFitting")
```

<img src="ReadmeFigs/QQDistrFitting.jpg" width="400">

### ScatterCorr

Shows what different correlations look like. Allows you to change the slope *independently* of the correlation, demonstrating that they're not the same thing.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "ScatterCorr")
```

<img src="ReadmeFigs/ScatterCorr.jpg" width="400">


### CLT

Potentially under construction. In the mean time, <a href="http://onlinestatbook.com/stat_sim/sampling_dist/">http://onlinestatbook.com/stat_sim/sampling_dist/</a> is the gold standard. (I doubt I'll be able to make a better one, so I might not try.)



# Spatial Stats Apps

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




