# Accounting for spatio-temporal and sampling dependence in relative biomass and CPUE indices: simulation and modeling framework
<p align="justify">
In this repository we present the code to reproduce our manuscript "Accounting for spatio-temporal and sampling dependence in relative biomass and CPUE indices*: simulation and modeling framework". The goal of this study is to simulate from an underlying spatio-temporal structure and then to use this simulation to determine which modeling of the relative biomass and CPUE data achieves a better estimation and prediction of the “real” biomass of a fish stock. Our hypothesis is that ignoring the underlying spatio-temporal process and the sampling dependence in the modeling approach can worsen the accuracy of the assessment. Whereas the application of preferential and/or spatio-temporal models, such as geostatistical or marked point process, may reduce the uncertainty of relative biomass or CPUE indices, explaining part of the variability that otherwise would not be accounted for.

The available code allows the simulation of a biomass scenario in space-time, assuming an underlying spatio-temporal effect, a temporal trend, and a non-linear relationship with the covariate bathymetry. Furthermore, we have fitted several regression models, such as generalized linear models (GLM), generalized additive models (GAM) and Bayesian hierarchical models (geostatistical models and marked point processes). 

Folders: 

1. A-Simulation: contains two scripts to reproduce the biomass simulation and the relative biomass and CPUE data. 
2. B-Models: contains another two folders CPUE and relative_biomass, where you can find the code for the reproduction of the proposed models. 
4. C-Estimated_series: contains a script to obtain the standardized series of relative biomass and CPUE indices. 
5. D-Error_measures: contains one script for calculating the RMSE and MAPE. 
6. E-SPiCT: contains a script for the reproduction of the SPiCT models implemented in this work. 

## Requirements  

<p align="justify">
For the correct operation of the repository it is necessary to have R-INLA, inlabru, SPiCT and the following packages installed.

1. Install R-INLA (in case of issues with the installation see [r-inla.org](https://www.r-inla.org/)):

```
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
```
  
2. Install inlabru (in case of issues with the installation see [inlabru](https://sites.google.com/inlabru.org/inlabru)):

```
remotes::install_github("inlabru-org/inlabru")
```
  
3. Install SPiCT (in case of issues with the installation see [SPiCT](https://github.com/DTUAqua/spict)):

```
install.packages("TMB", type="source")

library(remotes)
install_github("DTUAqua/spict/spict")  
```

4. Install paquetes:

```  
install.packages(c("lattice","gridExtra", "RColorBrewer", "raster", "fields", "reshape", "ggplot2", "mgcv", "R2BayesX", "tidyverse", "INLAutils", "devtools", "rgdal", "Metrics", "MLmetrics", "knitr", "formatR", "ellipse", "corrplot2", "icesAdvice")) 
```
  
5. Set parameters

```
coord1 <- 0 # coordinates
coord2 <- 10 # coordinates
coord3 <- 0 # coordinates
coord4 <- 10 # coordinates
variance <- 1 # spatial effect variance
kappa <- 0.5 # range = sqrt(8)/kappa
rho <- 0.9 # Rho (temporal correlation)
k <- 15 # k indica el numero de anyos
beta_0 <- 3 # intercept
beta_1 <- -2.5 # coefficient (degree 1) for the covariate (bathymetry)
beta_2 <- -1.5 # coefficient (degree 2) for the covariate (bathymetry)
vector_tiempo <- c(1, # year and alpha for the temporal trend
                   1.5,
                   1.6,
                   1.8,
                   2.2,
                   2.4,
                   2.8,
                   2.5,
                   2.2,
                   2,
                   1.5,
                   1,
                   0.5,
                   1.6,
                   2) 
bg <- 1 # phi (dispersion for the Gamma distribution) 
m <- 50 # Number of sampling points
q_random <- 0.3 # Catchability coefficient for random sampling
q_pref <- 0.6 #  Catchability coefficient for preferential sampling
```
6. Session information
 ```
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)
 ```
