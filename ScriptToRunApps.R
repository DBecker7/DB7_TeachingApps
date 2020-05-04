# Running apps from DB7_TeachingApps

# These are loaded by the apps, but I put them here so that
# you get a meaningful error if these packages are not installed.
library(ggplot2)
library(dplyr)
library(shiny)

# Poisson Approx to Binomial ----
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "PoisBinApprox")


# Quantile-Quantile Plots ----
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "QQDistrFitting")


# Correlation in Scatterplots ----
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "ScatterCorr")


# Relationship of Mean and Median - Skewness ----
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "MeanLessMeansLeft")




# Spatial Passion Projects ----
# Gaussian Process Parameters
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "SpatialFun/GausProcess_Matern")
# Gaussian Field Parameters
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", subdir = "SpatialFun/GausField_Matern")






