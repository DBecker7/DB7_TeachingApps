# DB7_TeachingApps
Some applications for self-discovery of statistical concepts and rules-of-thumb.

**PoisBinApprox**: The Poisson (and Normal) approximations to the binomial distributions. Includes sliders for sample size and probability of success. The Poisson distribution is useful when *p* is small, while the normal distribution is useful when both *np* and *n(1-p)* are larger than, say, 10 or so.

To run without downloading: 

```
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "PoisBinApprox")
```
