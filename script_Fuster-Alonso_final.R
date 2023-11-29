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

# Seed ---------------------------
q_seed <- sample(1:100000, 1)

############################
###     SAMPLING         ###
############################
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
  esfuerzo <- rnorm(m * k, 30, 3)
  
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

############################
###     MODELS CPUE      ###
############################
# Modify data ---------------------------
data_pref_final$tiempo <- as.factor(data_pref_final$tiempo)
data_pref_final$batimetria_2 <- as.vector(I(data_pref_final$batimetria^2))

# Bayesian ---------------------------
## GLM ---------------------------
set.seed(q_seed)
modelo_bglm_r <- inla(CPUE ~ batimetria + batimetria_2 + tiempo,
                      family = "Gamma",
                      control.predictor = list(compute = TRUE),
                      control.compute = list(
                        dic = TRUE,
                        waic = TRUE,
                        cpo = TRUE,
                        config = TRUE
                      ),
                      data = data_pref_final
)

## Prediction ---------------------------
r <- modelo_bglm_r
r.samples <- inla.posterior.sample(1000, r)
psam <- sapply(r.samples, function(x) {
  intercept <- x$latent %>% rownames(.) %>% stringr::str_detect("^\\(Intercept\\)") %>% x$latent[.,]
  beta_y <- x$latent %>% rownames(. ) %>% stringr::str_detect("^tiempo") %>% x$latent[.,]
  beta_2_pred <- x$latent %>% rownames(. ) %>% stringr::str_detect("^batimetria") %>% x$latent[.,]
  beta_2_pred <- beta_2_pred[1]
  beta_3_pred <- x$latent %>% rownames(. ) %>% stringr::str_detect("^batimetria_2") %>% x$latent[.,]
  
  
  predictor1 <- intercept + 
    beta_2_pred*data_pref_final$batimetria[data_pref_final$tiempo == 1] +
    beta_3_pred*data_pref_final$batimetria_2[data_pref_final$tiempo == 1]
  
  pre=list();l=length(beta_y)
  for (i in 1:l){
    pre[[i]]=intercept +
      beta_y[i] + 
      beta_2_pred*data_pref_final$batimetria[data_pref_final$tiempo == (i+1)] +
      beta_3_pred*data_pref_final$batimetria_2[data_pref_final$tiempo == (i+1)]
  }
  
  predictor=predictor1
  
  for (i in 1:l){
    predictor <- c(predictor, pre[[i]])
  }
  
  exp(predictor)
})


q.sam_al_a <- apply(psam, 1, quantile,
                    c(.025, 0.05, 0.5, 0.95, .975), na.rm =TRUE)

serie_CPUE_glm <- data.frame(t(q.sam_al_a))
serie_CPUE_glm$tiempo <- rep(1:k, each = m)

## GAM ---------------------------
## R2BayesX ---------------------------
set.seed(q_seed)
modelo_gam_CPUE <- bayesx(CPUE ~ sx(batimetria) + sx(xcoord, ycoord, bs = "te") + tiempo,
                          family = "Gamma",
                          method = "MCMC",
                          data = data_pref_final
)

### Prediction ---------------------------
modelo_gam_CPUE_pred <- predict(modelo_gam_CPUE,
                                type = "response"
)
data_gam_CPUE_pred <- data.frame(media_bio = as.vector(modelo_gam_CPUE_pred),
                                 tiempo = rep(1:k, each = m)
)

## GEOSTATISTICAL ---------------------------
# Modify data ---------------------------
data_pref_final <- data_pref_final[,-c(1,5,6,9)]
data_pref_final$batimetria <- as.vector(scale(data_pref_final$batimetria))
data_pref_final$tiempo <- as.numeric(data_pref_final$tiempo)
names(data_pref_final) <- c("batimetria",
                            "xcoord",
                            "ycoord",
                            "CPUE",
                            "tiempo"
)

# Mesh ---------------------------
loc <- cbind(data_pref_final$xcoord, data_pref_final$ycoord) # Locations
mesh <- inla.mesh.2d(
  loc.domain =  cbind(c(0,10,10,0,0),c(0,0,10,10,0)),
  max.edge = c(0.63, 2.5), # mesh parameters
  cutoff = 0.05
)

# Data for the prediction ---------------------------
data_bat <- data.frame(x = loc_xy[, 1],
                       y = loc_xy[, 2],
                       z = as.vector(scale(variables$x_bat1))
)

ras_bat <- rasterFromXYZ(data_bat) # Transform bathymetry into a raster
bat_pred <- raster::extract(ras_bat, mesh$loc[, c(1, 2)]) # Extract values of the mesh

data_pred <- data.frame(batimetria = bat_pred, xcoord = mesh$loc[, 1], ycoord = mesh$loc[, 2]) # Data
data_pred_final <- rbind(
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred
) # repeat the data k (years) times

data_pred_final$tiempo <- rep(1:k, each = length(data_pred$batimetria)) # add years
data_pred_final$CPUE <- rep(NA, length(data_pred_final$batimetria)) # add CPUE

# INLA.GROUP ---------------------------
data_pred_final$batimetria[which(data_pred_final$batimetria > max(data_pref_final$batimetria))] <- NA # Same limits pred and est
data_pred_final$batimetria[which(data_pred_final$batimetria < min(data_pref_final$batimetria))] <- NA

all <- rbind(data_pref_final, data_pred_final) # Joint pred y est
igroupbath <- inla.group(all[, 1], n = 25, idx.only = TRUE) # Function inla.group
groupbath <- inla.group(all[, 1], n = 25) # Function inla.group

allin <- cbind(all, igroupbath, groupbath) # Joint

data_pref_final <- cbind(data_pref_final,
                         allin$groupbath[1:length(data_pref_final$batimetria)],
                         allin$igroupbath[1:length(data_pref_final$batimetria)]
) # Add columns inla.group

data_pred_final <- cbind(data_pred_final,
                         allin$groupbath[(length(data_pref_final$batimetria) + 1):length(allin$batimetria)],
                         allin$igroupbath[(length(data_pref_final$batimetria) + 1):length(allin$batimetria)]
) # Add columns inla.group

names(data_pref_final) <- c("batimetria",
                            "xcoord",
                            "ycoord",
                            "CPUE",
                            "tiempo",
                            "IgBath",
                            "Igroup")

names(data_pred_final) <- c("batimetria",
                            "xcoord",
                            "ycoord",
                            "tiempo",
                            "CPUE",
                            "IgBath",
                            "Igroup")

# SPDE ---------------------------
spde <- inla.spde2.pcmatern(mesh = mesh,
                            prior.range = c(3, 0.5), # P(range < 3) = 0.5
                            prior.sigma = c(1, 0.01) # P(sigma > 1) = 0.01
)

# Matrix of weights (spatial field) ---------------------------
iset <- inla.spde.make.index("i",
                             n.spde = spde$n.spde,
                             n.group = k
)

# Projection matrix ---------------------------
A_est <- inla.spde.make.A(
  mesh = mesh,
  loc = cbind(data_pref_final$xcoord, data_pref_final$ycoord), group = data_pref_final$tiempo
) # Projection matrix for estimation

A_pred <- inla.spde.make.A(mesh = mesh,
                           loc = cbind(data_pred_final$xcoord, data_pred_final$ycoo),
                           group = data_pred_final$tiempo
) # Projection matrix for prediction

# Stack ---------------------------
## Estimation ---------------------------
stack_est <- inla.stack(data = list(y = data_pref_final$CPUE, link = 1),
                        A = list(A_est, 1, 1, 1), 
                        effects = list(iset,
                                       batimetria = data_pref_final$IgBath,
                                       tiempo = data_pref_final$tiempo,
                                       intercept = rep(1, length(data_pref_final$CPUE))
                        ),
                        tag = "est"
)

## Prediction ---------------------------
stack_pred <- inla.stack(data = list(y = data_pred_final$CPUE, link = 1), # Response variable NA
                         A = list(A_pred, 1, 1, 1), 
                         effects = list(iset,
                                        batimetria = data_pred_final$IgBath,
                                        tiempo = data_pred_final$tiempo,
                                        intercept = rep(1, length(data_pred_final$CPUE))
                         ),
                         tag = "pred"
)

## Joint both stacks ---------------------------
stack <- inla.stack(stack_est, stack_pred)

# PC-prior rho ---------------------------
h_spec <- list(rho = list(prior = "pc.cor1", param = c(0, 0.9))) # P(cor > 0 = 0.9)
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

# Formula ---------------------------
formula <- y ~ -1 +
  intercept +
  f(batimetria, model = "rw2") +
  f(i, model = spde, group = i.group,
    control.group = list(model = "ar1", hyper = h_spec)) +
  f(tiempo, model = "rw1", hyper = prec.prior)

# Fit model ---------------------------
set.seed(q_seed)
modelo_CPUE <- inla(formula,
                    data = inla.stack.data(stack),
                    family = "gamma",
                    control.predictor = list(
                      compute = TRUE,
                      A = inla.stack.A(stack), link = 1
                    ),
                    verbose = TRUE,
                    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                    num.threads = 2
)

## MARKED POINT PROCESS ---------------------------
# Modify data ---------------------------
data <- data_pref_final
spdf <- SpatialPointsDataFrame(cbind(data$xcoord,data$ycoord),
                               data[,-c(1,2,3,6,7)]
)

# Limit ---------------------------
poly1 <- Polygon(cbind(c(0,10,10,0,0),c(0,0,10,10,0)))
primer_poly <- Polygons(list(poly1), ID = "A")
boundary <- SpatialPolygons(list(primer_poly))

# Mesh ---------------------------
mesh <- inla.mesh.2d(loc.domain =  cbind(c(0,10,10,0,0),c(0,0,10,10,0)),
                     max.edge = c(0.63, 2.5), 
                     cutoff = 0.05
)

# Integration points ---------------------------
ips <- ipoints(mesh, group = "tiempo")

# SPDE ---------------------------
spde <- inla.spde2.pcmatern(mesh,
                            prior.range = c(3, 0.5), # P(range < 3) = 0.5
                            prior.sigma = c(1, 0.01) # P(sigma > 1) = 0.01
)

# PC-prior rho ---------------------------
prec.prior <- list(prec = list(param = c(0.001, 0.001)))
h_spec <- list(rho = list(prior = "pc.cor1", param = c(0, 0.9))) # P(cor > 0 = 0.9)

# Components ---------------------------
cmp = ~ 
  # Correlated spatial effecto
  spatial(coordinates,model = spde,
          group = tiempo,
          ngroup = k,
          control.group = list(model = "ar1", hyper = h_spec)) +
  # Copy the correlated spatial effect in the Point pattern with a sclae parameter (fixed = FALSE)
  spatialCopy(coordinates, 
              copy = "spatial",
              group = tiempo,
              ngroup = k,
              control.group = list(model = "ar1", hyper = h_spec),
              fixed = FALSE) +
  # Intercept for the point pattern
  lgcpIntercept(1) +
  # Intercept for the CPUE
  Intercept(1) +
  # Temporal trend 
  temporal(tiempo, model = "rw1", hyper = prec.prior) +
  # Bathymetry effect 
  bat(batimetria, model = "linear")

lik1 <- like(data = spdf, # likelihood for the CPUE
             family = "gamma",
             formula = CPUE ~ spatial + Intercept + temporal)
lik2 <- like(data = spdf, # likelihood for the point pattern
             family = "cp",
             ips = ips,
             domain = list(coordinates = mesh),
             formula = coordinates + tiempo ~ spatialCopy + lgcpIntercept + bat)

# Fit model ---------------------------
set.seed(q_seed)
fit_mpp <- bru(components = cmp,
               lik1,
               lik2,
               options = list(verbose = TRUE))

# Prediction ---------------------------
ppxl <- pixels(mesh, mask = boundary)
ppxl_all <- cprod(ppxl, data.frame(tiempo = seq_len(k)))
mark <- predict(fit_mpp,
                ppxl_all,
                ~ data.frame(tiempo = tiempo,
                             mark = exp(spatial + Intercept + temporal + bat)
                )
)

############################
###   MODELS SURVEY      ###
############################
# Modify data ---------------------------
data_random_final$batimetria_2 <- as.vector(I(data_random_final$batimetria ^ 2))
data_random_final$tiempo <- as.factor(data_random_final$tiempo)

## GLM ---------------------------
set.seed(q_seed)
modelo_glm_survey <- inla(bio_relativa ~ batimetria + batimetria_2 + tiempo,
                          family = "Gamma",
                          control.predictor = list(compute = TRUE),
                          control.compute = list(
                            dic = TRUE,
                            waic = TRUE,
                            cpo = TRUE,
                            config = TRUE
                          ),
                          data = data_random_final
)

### Prediction ---------------------------
r <- modelo_glm_survey
r.samples <- inla.posterior.sample(1000, r)
psam <- sapply(r.samples, function(x) {
  intercept <- x$latent %>% rownames(.) %>% stringr::str_detect("^\\(Intercept\\)") %>% x$latent[., ]
  beta_y <- x$latent %>% rownames(.) %>% stringr::str_detect("^tiempo") %>% x$latent[., ]
  beta_2_pred <- x$latent %>% rownames(.) %>% stringr::str_detect("^batimetria") %>% x$latent[., ]
  beta_2_pred <- beta_2_pred[1]
  beta_3_pred <- x$latent %>% rownames(.) %>% stringr::str_detect("^batimetria_2") %>% x$latent[., ]
  
  predictor1 <- intercept +
    beta_2_pred * data_random_final$batimetria[data_random_final$tiempo == 1] +
    beta_3_pred * data_random_final$batimetria_2[data_random_final$tiempo == 1]
  
  pre = list()
  l = length(beta_y)
  for (i in 1:l) {
    pre[[i]] = intercept +
      beta_y[i] +
      beta_2_pred * data_random_final$batimetria[data_random_final$tiempo == (i + 1)] +
      beta_3_pred * data_random_final$batimetria_2[data_random_final$tiempo == (i + 1)]
  }
  
  predictor = predictor1
  
  for (i in 1:l) {
    predictor <- c(predictor, pre[[i]])
  }
  
  exp(predictor)
}
)

q.sam_al_a <- apply(psam, 1, quantile,
                    c(.025, 0.05, 0.5, 0.95, .975), na.rm = TRUE
)

serie_indice_bio_relativa <- data.frame(t(q.sam_al_a))
serie_indice_bio_relativa$tiempo <- rep(1:k, each = m)

## GAM ---------------------------
### R2BayesX ---------------------------
set.seed(q_seed)
modelo_gam_survey <- bayesx(bio_relativa ~ sx(batimetria) + sx(xcoord, ycoord, bs = "te") + tiempo,
                            family = "Gamma",
                            method = "MCMC",
                            data = data_random_final
)

### Prediction ---------------------------
modelo_gam_survey_pred <- predict(modelo_gam_survey, type = "response")
modelo_gam_survey_pred$tiempo <- rep(1:k, each = m)

# Modify data ---------------------------
data_random_final <- data_random_final[,-c(1,7)]
data_random_final$batimetria <- as.vector(scale(data_random_final$batimetria))
data_random_final$tiempo <- as.numeric(data_random_final$tiempo)
names(data_random_final) <- c("batimetria",
                              "xcoord",
                              "ycoord",
                              "bio_relativa",
                              "tiempo"
)

# Mesh ---------------------------
loc <- cbind(data_random_final$xcoord,
             data_random_final$ycoord
) # Locations

mesh <- inla.mesh.2d(loc.domain = cbind(c(0,10,10,0,0),c(0,0,10,10,0)), # Limit
                     max.edge = c(0.63, 2.5), # Mesh parameters
                     cutoff = 0.05
)

# Data for the prediction ---------------------------
data_bat <- data.frame(x = loc_xy[, 1],
                       y = loc_xy[, 2],
                       z = as.vector(scale(variables$x_bat1))
)
ras_bat <- rasterFromXYZ(data_bat) # Transform bathymetry into a raster
bat_pred <- raster::extract(ras_bat,
                            mesh$loc[, c(1, 2)]
) # Extract values of the mesh

data_pred <- data.frame(batimetria = bat_pred,
                        xcoord = mesh$loc[, 1],
                        ycoord = mesh$loc[, 2]) # Data

data_pred_final <- rbind(
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred
) # repeat the data k (years) times

data_pred_final$tiempo <- rep(1:k, each = length(data_pred$batimetria)) # add years
data_pred_final$bio_relativa <- rep(NA, length(data_pred_final$batimetria)) # add relative biomass

# INLA.GROUP ---------------------------
data_pred_final$batimetria[which(data_pred_final$batimetria > max(data_random_final$batimetria))] <- NA # Same limits for pred and est
data_pred_final$batimetria[which(data_pred_final$batimetria < min(data_random_final$batimetria))] <- NA

all <- rbind(data_random_final, data_pred_final) # Joint pred and est
igroupbath <- inla.group(all[, 1], n = 25, idx.only = TRUE) # Funcion inla.group
groupbath <- inla.group(all[, 1], n = 25) # Funcion inla.group

allin <- cbind(all, igroupbath, groupbath) # Joint

data_random_final <- cbind(data_random_final,
                           allin$groupbath[1:length(data_random_final$batimetria)],
                           allin$igroupbath[1:length(data_random_final$batimetria)]
) # Add columns inla.group

data_pred_final <- cbind(data_pred_final,
                         allin$groupbath[(length(data_random_final$batimetria) + 1):length(allin$batimetria)],
                         allin$igroupbath[(length(data_random_final$batimetria) + 1):length(allin$batimetria)]
) # Add columns inla.group

names(data_random_final) <- c("batimetria",
                              "xcoord",
                              "ycoord",
                              "bio_relativa",
                              "tiempo",
                              "IgBath",
                              "Igroup"
)
names(data_pred_final) <- c("batimetria",
                            "xcoord", 
                            "ycoord", 
                            "tiempo", 
                            "bio_relativa", 
                            "IgBath", 
                            "Igroup"
)

# SPDE ---------------------------
spde <- inla.spde2.pcmatern(mesh = mesh,
                            prior.range = c(3, 0.5), # P(range < 3) = 0.5
                            prior.sigma = c(1, 0.01) # P(sigma > 1) = 0.01
) 

# Matrix of weights (spatial field) ---------------------------
iset <- inla.spde.make.index("i",
                             n.spde = spde$n.spde,
                             n.group = k
)

# Projection matrix ---------------------------
A_est <- inla.spde.make.A(mesh = mesh,
                          loc = cbind(data_random_final$xcoord, data_random_final$ycoord),
                          group = data_random_final$tiempo
) # estimation

A_pred <- inla.spde.make.A(mesh = mesh,
                           loc = cbind(data_pred_final$xcoord, data_pred_final$ycoord),
                           group = data_pred_final$tiempo
) # prediction

# Stack ---------------------------
## Estimation ---------------------------
stack_est <- inla.stack(data = list(y = data_random_final$bio_relativa, link = 1), 
                        A = list(A_est, 1, 1, 1), 
                        effects = list(iset,
                                       batimetria = data_random_final$IgBath,
                                       tiempo = data_random_final$tiempo,
                                       intercept = rep(1, length(data_random_final$bio_relativa))
                        ),
                        tag = "est"
)

## Prediction ---------------------------
stack_pred <- inla.stack(data = list(y = data_pred_final$bio_relativa, link = 1), # response variable NA
                         A = list(A_pred, 1, 1, 1), 
                         effects = list(iset,
                                        batimetria = data_pred_final$IgBath,
                                        tiempo = data_pred_final$tiempo,
                                        intercept = rep(1, length(data_pred_final$bio_relativa))
                         ),
                         tag = "pred"
)

## Joint both stacks ---------------------------
stack <- inla.stack(stack_est, stack_pred)

# PC-prior rho ---------------------------
h_spec <- list(rho = list(prior = "pc.cor1", param = c(0, 0.9))) # P(cor > 0 = 0.9)
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

# Formula ---------------------------
formula <- y ~ -1 +
  intercept +
  f(batimetria, model = "rw2") + # smooth function for the bathyemtry
  f(i, model = spde, group = i.group, # spatio-temporal effect
    control.group = list(model = "ar1", hyper = h_spec)) +
  f(tiempo, model = "rw1", hyper = prec.prior) # temporal effect

# Fit the model ---------------------------
set.seed(q_seed)
modelo_survey <- inla(formula,
                      data = inla.stack.data(stack),
                      family = "gamma",
                      control.predictor = list(
                        compute = TRUE,
                        A = inla.stack.A(stack), link = 1),
                      verbose = TRUE,
                      control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                      num.threads = 2
)

# Save all 
save.image(paste0("./",q_seed,"_models.RData"))
