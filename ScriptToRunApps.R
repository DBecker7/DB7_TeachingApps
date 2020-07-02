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
# Normal Shape
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/normShape")
# Binomial Probabilities
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/dbinom")
# p-values
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/pvalues")
# Distribution shapes
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/distrshapes")
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/distrshapes_disc")
# Power
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Tools/SimplePower")



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
# How large must n be?
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/nLarge")
# Z or t??
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/Z_or_t")
# Confidence Intervals
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/ci")
# Sampling Distributions
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/samplingDist")
# Sampling Designs: Gettysburg Address
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "Apps/gettysburg")


# Spatial Passion Projects ----
# Gaussian Process Parameters
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "SpatialFun/GausProcess_Matern")
# Gaussian Field Parameters
shiny::runGitHub(repo = "DBecker7/DB7_TeachingApps", 
    subdir = "SpatialFun/GausField_Matern")






