# Packages ---------------------------
library(INLA)
library(fields)
library(lattice)
library(reshape)
library(raster)
library(R2BayesX)
library(tidyverse)
library(ggplot2)
library(inlabru)
library(devtools)
library(INLAutils)
library(rgdal)

############################
###     SIMULATION       ###
############################
q_seed <- 852345
# Parameters ---------------------------
coord1 <- 0 # coordinates
coord2 <- 10 # coordinates
coord3 <- 0 # coordinates
coord4 <- 10 # coordinates
variance <- 1 # spatial effect variance
kappa <- 0.5 # range = sqrt(8)/kappa
rho_1 <- 0.9 # Rho (temporal correlation)
rho_2 <- 0.9 # Rho (temporal correlation) # ESTO NO LO HE IMPLEMENTADO ASÍ PERO DEBERÍA, Y DEBERÍAMOS HABERLE CAMBIADO EL VALOR
k <- 15 # k indica el numero de anyos
beta_0 <- 3 # intercept
beta_1 <- -2.5 # coefficient (degree 1) for the covariate (bathymetry)
beta_2 <- -1.5 # coefficient (degree 2) for the covariate (bathymetry)
set.seed(q_seed)
vector_tiempo <- as.vector(arima.sim(list(order=c(1,0,0), ar=rho), n=k))
bg <- 1 # phi (dispersion for the Gamma distribution) 
m <- 50 # Number of sampling points
q_random <- 0.3 # Catchability coefficient for random sampling
q_pref <- 0.6 #  Catchability coefficient for preferential sampling


# Simulated study area ---------------------------
campo_estudio <- function(coord1, coord2, coord3, coord4) {
  xy <- expand.grid(
    seq(coord1, coord2, length.out = (100)),
    seq(coord3, coord4, length.out = (100))
  )
  x <- as.numeric(xy[, 1])
  y <- as.numeric(xy[, 2])
  loc_xy <- cbind(x, y)
  
  return(loc_xy)
}

set.seed(q_seed)
loc_xy <- campo_estudio(coord1, coord2, coord3, coord4)

# Simulation of the predictor terms ---------------------------
book.rspde <- function(coords, sigma=1, range, variance=sigma^2, alpha=2,
                       kappa = sqrt(8*(alpha-1))/range, n=1, mesh, 
                       verbose=FALSE, seed, return.attributes=FALSE) {
  t0 <- Sys.time()
  theta <- c(-0.5*log(4*pi*variance*kappa^2), log(kappa))
  if (verbose) cat('theta =', theta, '\n')
  if (missing(mesh)) {
    mesh.pars <- c(0.5, 1, 0.1, 0.5, 1)*sqrt(alpha-ncol(coords)/2)/kappa 
    if (verbose) cat('mesh.pars =', mesh.pars, '\n')
    attributes <- list(
      mesh=inla.mesh.2d(,
                        coords[chull(coords), ], max.edge=mesh.pars[1:2], 
                        cutoff=mesh.pars[3], offset=mesh.pars[4:5]))
    if (verbose) cat('n.mesh =', attributes$mesh$n, '\n')
  }
  else attributes <- list(mesh=mesh)
  attributes$spde <- inla.spde2.matern(attributes$mesh, alpha=alpha)
  attributes$Q <- inla.spde2.precision(attributes$spde, theta=theta)
  attributes$A <- inla.mesh.project(mesh=attributes$mesh, loc=coords)$A
  if (n==1) 
    result <- drop(attributes$A%*%inla.qsample(
      Q=attributes$Q,
      constr=attributes$spde$f$extraconstr))
  t1 <- Sys.time() 
  result <- inla.qsample(n, attributes$Q, 
                         seed=ifelse(missing(seed), 0, seed), 
                         constr=attributes$spde$f$extraconstr) 
  if (nrow(result)<nrow(attributes$A)) {
    result <- rbind(result, matrix(
      NA, nrow(attributes$A)-nrow(result), ncol(result)))
    dimnames(result)[[1]] <- paste('x', 1:nrow(result), sep='')
    for (j in 1:ncol(result)) 
      result[, j] <- drop(attributes$A%*%
                            result[1:ncol(attributes$A),j])
  }
  else {
    for (j in 1:ncol(result)) 
      result[1:nrow(attributes$A), j] <-
        drop(attributes$A%*%result[,j]) 
    result <- result[1:nrow(attributes$A), ]
  }
  t2 <- Sys.time()
  attributes$cpu <- c(prep=t1-t0, sample=t2-t1, total=t2-t0)
  if (return.attributes) 
    attributes(result) <- c(attributes(result), attributes)
  return(drop(result))
}

simulacion_variables <- function(variance, kappa, rho, k) {
  # Mesh
  prmesh1 <- inla.mesh.2d(loc = loc_xy, max.edge = c(0.4, 1))
  
  # Parameters for spatial effect
  params <- c(variance = variance, kappa = kappa)
  
  # INLA function
  set.seed(q_seed)
  x_k <- book.rspde(loc_xy,
                    range = sqrt(8) / params[2],
                    sigma = sqrt(params[1]), n = k, mesh = prmesh1,
                    return.attributes = TRUE,
                    seed = 12346
  )
  # Add temporal correlation
  x <- x_k
  for (j in 2:k) {
    x[, j] <- rho * x[, j - 1] + sqrt(1 - rho^2) * x_k[, j]
  }
  
  # Function for bathymetry
  batimetria <- function(x, y) {
    x_bat1 <- log(x * y + 1) * 100
    return(x_bat1)
  }
  
  # Function for bathymetry
  x_bat1 <- batimetria(loc_xy[, 1], loc_xy[, 2])
  
  # Scale bathymetry
  x_bat <- scale(x_bat1)
  
  # Data.frame with predictor's terms
  variables <- data.frame(x = x, x_bat1 = x_bat1, x_bat = x_bat)
  
  # Change names of the data.frame
  names(variables) <- c(
    "x1", "x2", "x3", "x4","x5",
    "x6", "x7", "x8", "x9", "x10",
    "x11","x12","x13","x14","x15",
    "x_bat1", "x_bat"
  ) # This has to be change according to k (number of years)
  
  return(variables)
}

set.seed(q_seed)
variables <- simulacion_variables(variance, kappa, rho, k)

# Variable response simulation ---------------------------
simulacion_respuesta <- function(beta_0, beta_1, beta_2, bg, n) {
  
  # Linear predictor of the mean of the distribution
  lin_pred_mu <- list()
  for (i in 1:k) {
    lin_pred_mu[[i]] <- exp(beta_0 + beta_1 * variables$x_bat + beta_2 * (variables$x_bat)^2 + variables[, i] + vector_tiempo[i])
  }
  
  # Parameter alpha for the gamma distribution
  alpha <- list()
  for (i in 1:k) {
    alpha[[i]] <- (lin_pred_mu[[i]]) * bg
  }
  
  
  # Simulation of the biomass with a Gamma distribution
  biomasa_real <- list()
  for (i in 1:k) {
    biomasa_real[[i]] <- rgamma(n, alpha[[i]], bg)
  }
  
  # Datas with biomass, bathymetry and locations
  data_variables <- list()
  for (i in 1:k) {
    data_variables[[i]] <- data.frame(
      biomasa = biomasa_real[[i]], batimetria = variables$x_bat1, xcoord = loc_xy[, 1],
      ycoord = loc_xy[, 2]
    )
  }
  
  return(data_variables)
}

n <- as.numeric(dim(variables)[1]) # number of rows of variables's data frame

set.seed(q_seed)
data_variables <- simulacion_respuesta(beta_0, beta_1, beta_2, bg, n)

# Sum 0.1 to the total biomass
for (i in 1:k) {
  data_variables[[i]]$biomasa <- data_variables[[i]]$biomasa + 0.1
}

# Sampling ---------------------------
## Random sampling ---------------------------
muestreo_random <- function(m, q_random) {
  
  # Number of points selected
  data_random <- list()
  for (i in 1:k) {
    isel <- sample(1:n, m)
    data_random[[i]] <- data_variables[[i]][isel, ]
  }
  
  # Relative biomass data
  for (i in 1:k) {
    data_random[[i]]$bio_relativa <- data_random[[i]]$biomasa * q_random
  }
  
  # Final data frame
  data_random_final <- data.frame()
  
  for (i in 1:k) {
    data_random_final <- rbind(data_random_final, data_random[[i]])
  }
  
  # Add time 
  data_random_final$tiempo <- rep(1:k, each = m)
  
  return(data_random_final)
}

set.seed(q_seed)
data_random_final <- muestreo_random(m, q_random)

## Preferential sampling ---------------------------
muestreo_preferencial <- function(m, q_pref) {
  
  # Transform biomass to a range from 0 to 1
  biomasa_trans <- list()
  for (i in 1:k) {
    biomasa_trans[[i]] <- (data_variables[[i]]$biomasa - min(data_variables[[i]]$biomasa)) / (max(data_variables[[i]]$biomasa) - min(data_variables[[i]]$biomasa))
  }
  
  # Select point depeding on a probability (prob)
  isel_pre <- list()
  for (i in 1:k) {
    isel_pre[[i]] <- sample(1:n, m, prob = biomasa_trans[[i]])
  }
  
  data_pref <- list()
  for (i in 1:k) {
    data_pref[[i]] <- data_variables[[i]][isel_pre[[i]], ]
  }
  
  # Final data frame for preferential biomass 
  data_pref_final <- data.frame()
  
  for (i in 1:k) {
    data_pref_final <- rbind(data_pref_final, data_pref[[i]])
  }
  
  # Variable effort (time the gear remains active)
  esfuerzo <- rnorm(m * k, 20, 3)
  
  # Id 
  id <- 1:length(data_pref_final$biomasa)
  
  data_pref_final$id <- id
  
  # Order values of biomass (increasing)
  data_pref_final1 <- data_pref_final[order(data_pref_final$biomasa), ]
  
  # Order values of effert (increasing)
  data_pref_final1$esfuerzo <- sort(esfuerzo)
  
  # Order according to id
  data_pref_final <- data_pref_final1[order(data_pref_final1$id),]
  
  # Remove id
  data_pref_final$id <- NULL
  
  # Catch data
  data_pref_final$biomasa_r <- data_pref_final$biomasa * q_pref
  
  # CPUE = biomass * q / effort
  data_pref_final$CPUE <- data_pref_final$biomasa_r / data_pref_final$esfuerzo
  
  # Add time
  data_pref_final$tiempo <- rep(1:k, each = m)
  
  return(data_pref_final)
  return(biomasa_trans)
}

set.seed(q_seed)
data_pref_final <- muestreo_preferencial(m, q_pref)

# Save all 
save.image(paste0("./",q_seed,"data.RData"))
