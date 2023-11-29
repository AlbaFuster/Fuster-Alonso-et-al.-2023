# Accounting for spatio-temporal and sampling dependence in relative biomass and CPUE indices: simulation and modeling framework
<p align="justify">
In this repository we present the code to reproduce our manuscript "Accounting for spatio-temporal and sampling dependence in relative biomass and CPUE indices: simulation and modeling framework". The goal of this study is to simulate from an underlying spatio-temporal structure and then to use this simulation to determine which modeling of the relative biomass and CPUE data achieves a better estimation and prediction of the “real” biomass of a fish stock. Our hypothesis is that ignoring the underlying spatio-temporal process and the sampling dependence in the modeling approach can worsen the accuracy of the assessment. Whereas the application of preferential and/or spatio-temporal models, such as geostatistical or marked point process, may reduce the uncertainty of relative biomass or CPUE indices, explaining part of the variability that otherwise would not be accounted for.
<p align="justify">
The available code allows the simulation of a biomass scenario in space-time, assuming an underlying spatio-temporal effect, a temporal trend, and a non-linear relationship with the covariate bathymetry. Furthermore, we have fitted several regression models, such as generalized linear models (GLM), generalized additive models (GAM) and Bayesian hierarchical models (geostatistical models and marked point processes). 

## Requirements  

<p align="justify">
For the correct operation of the repository it is necessary to have R-INLA, inlabru and the following packages installed.

1. Install R-INLA (in case of issues with the installation see [r-inla.org](https://www.r-inla.org/)):

```
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
```
  
2. Install inlabru (in case of issues with the installation see [inlabru](https://sites.google.com/inlabru.org/inlabru)):

```
remotes::install_github("inlabru-org/inlabru")
```

3. Install paquetes:

```  
install.packages(c("lattice","gridExtra", "RColorBrewer", "raster", "fields", "reshape", "ggplot2", "mgcv", "R2BayesX", "tidyverse", "INLAutils", "devtools", "rgdal", "Metrics", "MLmetrics", "knitr", "formatR", "ellipse", "corrplot2")) 
```

6. Session information
 ```
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)
 ```
