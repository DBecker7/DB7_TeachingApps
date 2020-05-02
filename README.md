# DB7_TeachingApps
Some applications for self-discovery of statistical concepts and rules-of-thumb.

**PoisBinApprox**: The Poisson (and Normal) approximations to the binomial distributions. Includes sliders for sample size and probability of success. The Poisson distribution is useful when *p* is small, while the normal distribution is useful when both *np* and *n(1-p)* are larger than, say, 10 or so.

To run without downloading: 

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "PoisBinApprox")
```


**QQDistrFitting**: Demonstration of the usefulness of QQ plots in assessing distributional assumptions. The app shows the histogram (with estimated density overlaid) and the qq-plot (which does *not* need an estimate of the parameters).

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "QQDistrFitting")
```

**ScatterCorr**: Shows what different correlations look like. Allows you to change the slope *independently* of the correlation, demonstrating that they're not the same thing.

```r
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "ScatterCorr")
```











