# Running apps from DB7_TeachingApps

# These are loaded by the apps, but I put them here so that
# you get a meaningful error if these packages are not installed.
library(ggplot2)
library(dplyr)
library(shiny)
library(patchwork)

# To run locally (i.e. if you cloned the repo): shiny::runApp("PoisBinApprox")

# Tools ----
# Normal Probabilities
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/pnorm")
# p-values
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/pvalues")
# Distribution shapes
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/distrshapes")



# Demonstrations ----
# Poisson Approximation to Binomial
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/PoisBinApprox")

# Quantile-Quantile Plots 
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/QQDistrFitting")

# Correlation in Scatterplots 
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/ScatterCorr")

# Relationship of Mean and Median - Skewness 
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/MeanLessMeansLeft")

# Relationship between histograms and density plots 
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps",
    subdir = "Apps/DensHist")

# Measures of Spread
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/MeasureSpread")

# Should I use a bar plot or a histogram?
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/PoissonCatQuant")

# Conditional Distributions
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/ConditionalNormal2")

# Power
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/SimplePower")



# Spatial Passion Projects ----
# Gaussian Process Parameters
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "SpatialFun/GausProcess_Matern")

# Gaussian Field Parameters
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "SpatialFun/GausField_Matern")






