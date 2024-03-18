# PLOT Y DATA
# tienes que cargar los datos simulados y cada data_1,...,data_30
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)
library(ggpubr)
library(grid)
library(openair)
library(Metrics)
library(stats)

elementos <- list()
for(j in 1:7) {
  print(j)
  elemento <- c()
  for (i in 1:30) {
   print(i)
    elemento <- c(elemento, get(paste0("data_", i))[[j]])
  }
  elementos[[j]] <- elemento
  rm(elemento)
}

elementos_data <- as.data.frame(elementos)
names(elementos_data) <- c("GLM","GAM","GEO", "GLM_CPUE",
                           "GAM_CPUE", "GEO_CPUE","PREF")

elementos_data$GLM_trans <- elementos_data$GLM/q_random
elementos_data$GAM_trans <- elementos_data$GAM/q_random
elementos_data$GEO_trans <- elementos_data$GEO/q_random

elementos_data$GLM_CPUE_trans <- (elementos_data$GLM_CPUE * esfuerzo_por_anyo)/q_pref
elementos_data$GAM_CPUE_trans <- (elementos_data$GAM_CPUE * esfuerzo_por_anyo)/q_pref
elementos_data$GEO_CPUE_trans <- (elementos_data$GEO_CPUE * esfuerzo_por_anyo)/q_pref
elementos_data$PREF_CPUE_trans <- (elementos_data$PREF * esfuerzo_por_anyo)/q_pref

elementos_data$SIM <- rep(1:30, each = 15)
elementos_data$YEAR <- rep(1:15, times = 30)
elementos_data$BIO_SIM <- rep(biomasa_simulada, times = 30)

biomasa_simulada <- list()
for (i in 1:k) {
  biomasa_simulada[[i]] <- median(data_variables[[i]]$biomasa)
} 

biomasa_simulada <- unlist(biomasa_simulada)

esfuerzo_por_anyo <- list()
for (i in 1:k) {
  esfuerzo_por_anyo[[i]] <- median(data_pref_final$esfuerzo[data_pref_final$tiempo == i])
}
esfuerzo_por_anyo <- unlist(esfuerzo_por_anyo)

# GRAFICO ALEATORIO
media_bio_sim <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(mean_bio_sim = mean(BIO_SIM, na.rm = TRUE))

p_GLM <- ggplot(elementos_data, aes(x = factor(YEAR), y = GLM_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3) +
  ylim(c(0,95)) + xlab("") + ylab("")

p_GAM <- ggplot(elementos_data, aes(x = factor(YEAR), y = GAM_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3) +
  ylim(c(0,95)) + xlab("") + ylab("")

p_GEO <- ggplot(elementos_data, aes(x = factor(YEAR), y = GEO_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3) +
  ylim(c(0,95)) + xlab("year") + ylab("")

b <- arrangeGrob(
  arrangeGrob(p_GLM, p_GAM, ncol = 2), 
  p_GEO, 
  nrow = 2  
)

plot(b)

# GRAFICO PREFERENTIAL
p_GLM_CPUE <- ggplot(elementos_data, aes(x = factor(YEAR), y = GLM_CPUE_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3)  +
  xlab("") + ylab("") +
  ylim(c(0,313))

p_GAM_CPUE <- ggplot(elementos_data, aes(x = factor(YEAR), y = GAM_CPUE_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3) +
  xlab("") + ylab("") +
  ylim(c(0,313))

p_GEO_CPUE <- ggplot(elementos_data, aes(x = factor(YEAR), y = GEO_CPUE_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3) +
  xlab("year") + ylab("") +
  ylim(c(0,313))

p_PREF_CPUE <- ggplot(elementos_data, aes(x = factor(YEAR), y = PREF_CPUE_trans, group = YEAR)) + 
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() +
  geom_point(data = media_bio_sim, aes(x = factor(YEAR), y = mean_bio_sim), color = "blue", size = 3) +
  xlab("year") + ylab("") +
  ylim(c(0,313))

b_pref <- grid.arrange(p_GLM_CPUE,
                       p_GAM_CPUE,
                       p_GEO_CPUE,
                       p_PREF_CPUE)
plot(b_pref)

# Scatterplot random 
median_GLM_r <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(GLM_trans, na.rm = TRUE))

median_GAM_r <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(GAM_trans, na.rm = TRUE))

median_GEO_r <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(GEO_trans, na.rm = TRUE))

sd_GLM_r <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(GLM_trans, na.rm = TRUE))

sd_GAM_r <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(GAM_trans, na.rm = TRUE))

sd_GEO_r <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(GEO_trans, na.rm = TRUE))

sd_bio_sim <- rep(0,15)

median_random <- c(as.numeric(median_GLM_r$median),
                   as.numeric(median_GAM_r$median),
                  as.numeric(median_GEO_r$median),
                  as.numeric(media_bio_sim$mean_bio_sim))

sd_random <- c(as.numeric(sd_GLM_r$sd),
                   as.numeric(sd_GAM_r$sd),
                   as.numeric(sd_GEO_r$sd),
                   as.numeric(sd_bio_sim))
  
data_random_comp <- data.frame(models = rep(c("GLM","GAM","GEO","BIO"), each = 15),
                               values = as.vector(median_random),
                               sd = as.vector(sd_random),
                               values_bio = rep(media_bio_sim$mean_bio_sim,4))  

scatter_random <- ggplot(data_random_comp, aes(x=values,
                             y=values_bio,
                             color=models)) + 
  geom_point(size=4) +
  theme_classic() + 
  geom_abline(intercept=0, slope=1, linetype = "dashed") +
  scale_color_manual(values = c(
    "gray17",
    "firebrick2",
    "dodgerblue3",
    "mediumseagreen"
  )) +
  geom_errorbar(aes(xmin=values-sd, xmax=values+sd), width=.3, alpha = 0.6) +
  ggtitle("Survey biomass indices") +
  guides(color = "none")


# Scatterplot pref 
median_GLM_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(GLM_CPUE_trans, na.rm = TRUE))

median_GAM_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(GAM_CPUE_trans, na.rm = TRUE))

median_GEO_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(GEO_CPUE_trans, na.rm = TRUE))

median_MPP_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(median = median(PREF_CPUE_trans, na.rm = TRUE))

sd_GLM_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(GLM_CPUE_trans, na.rm = TRUE))

sd_GAM_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(GAM_CPUE_trans, na.rm = TRUE))

sd_GEO_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(GEO_CPUE_trans, na.rm = TRUE))

sd_MPP_pref <- elementos_data %>%
  group_by(YEAR) %>%
  summarize(sd = sd(PREF_CPUE_trans, na.rm = TRUE))

sd_bio_sim <- rep(0,15)

median_random <- c(as.numeric(median_GLM_pref$median),
                   as.numeric(median_GAM_pref$median),
                   as.numeric(median_GEO_pref$median),
                   as.numeric(median_MPP_pref$median),
                   as.numeric(media_bio_sim$mean_bio_sim))

sd_random <- c(as.numeric(sd_GLM_pref$sd),
               as.numeric(sd_GAM_pref$sd),
               as.numeric(sd_GEO_pref$sd),
               as.numeric(sd_MPP_pref$sd),
               as.numeric(sd_bio_sim))

data_pref_comp <- data.frame(models = rep(c("GLM","GAM","GEO","MPP","BIO"), each = 15),
                               values = as.vector(median_random),
                               sd = as.vector(sd_random),
                               values_bio = rep(media_bio_sim$mean_bio_sim,5))  

scatter_pref <- ggplot(data_pref_comp, aes(x=values,
                             y=values_bio,
                             color=models)) + 
  geom_point(size=4) +
  theme_classic() + 
  geom_abline(intercept=0, slope=1, linetype = "dashed") +
  scale_color_manual(values = c(
    "gray17",
    "firebrick2",
    "dodgerblue3",
    "mediumseagreen",
    "plum4"
  )) +
  geom_errorbar(aes(xmin=values-sd, xmax=values+sd), width=.3, alpha = 0.6) +
  ggtitle("CPUE biomass indices") +
  guides(color = "none")



figure <- ggarrange(scatter_random + rremove("ylab") + rremove("xlab"),
                  scatter_pref + rremove("ylab") + rremove("xlab"),
                  nrow = 2,
                  ncol = 1,
                  legend = "right")

annotate_figure(figure, left = textGrob("Predicted biomass", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Simulated biomass", gp = gpar(cex = 1)))

# Plot RMSE and cor 
# Random
rmse_GLM_r <- list()
rmse_GAM_r <- list()
rmse_GEO_r <- list()

for(i in 1:30) {
  print(i)
  rmse_GLM_r[[i]] <- rmse(elementos_data$GLM_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_GLM_r <- unlist(rmse_GLM_r)
  rmse_GAM_r[[i]] <- rmse(elementos_data$GAM_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_GAM_r <- unlist(rmse_GAM_r)
  rmse_GEO_r[[i]] <- rmse(elementos_data$GEO_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_GEO_r <- unlist(rmse_GEO_r)
}

cor_GLM_r <- list()
cor_GAM_r <- list()
cor_GEO_r <- list()

for(i in 1:30) {
  print(i)
  cor_GLM_r[[i]] <- cor(elementos_data$GLM_trans[elementos_data$SIM == i],
                     elementos_data$BIO_SIM[elementos_data$SIM == i],
                     method = c("spearman"))
  cor_GLM_r <- unlist(cor_GLM_r)
  cor_GAM_r[[i]] <- cor(elementos_data$GAM_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i],
                      method = c("spearman"))
  cor_GAM_r <- unlist(cor_GAM_r)
  cor_GEO_r[[i]] <- cor(elementos_data$GEO_trans[elementos_data$SIM == i],
                    elementos_data$BIO_SIM[elementos_data$SIM == i],
                    method = c("spearman"))
  cor_GEO_r <- unlist(cor_GEO_r)
}

cor_random = c(as.numeric(cor_GLM_r),
               as.numeric(cor_GAM_r),
               as.numeric(cor_GEO_r))

rmse_random = c(as.numeric(rmse_GLM_r),
                as.numeric(rmse_GAM_r),
                as.numeric(rmse_GEO_r))

data_random_measures <- data.frame(
  models = rep(c("GLM","GAM","GEO"), each = 30),
  cor = cor_random,
  rmse = rmse_random
)
  
cor_random_p <- ggplot(data_random_measures, aes(x = factor(models), y = cor, group = models)) + 
  geom_boxplot(fill = "#69b3a2", alpha = 0.5) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() + xlab("") + ylab("Spearman correlation coefficient")

rmse_random_p <- ggplot(data_random_measures, aes(x = factor(models), y = rmse, group = models)) + 
  geom_boxplot(fill = "#69b3a2", alpha = 0.5) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() + xlab("") + ylab("Root Mean Square Error (RMSE)")

# Pref
rmse_GLM_pref <- list()
rmse_GAM_pref <- list()
rmse_GEO_pref <- list()
rmse_MPP_pref <- list()

for(i in 1:30) {
  print(i)
  rmse_GLM_pref[[i]] <- rmse(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_GLM_pref <- unlist(rmse_GLM_pref)
  rmse_GAM_pref[[i]] <- rmse(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_GAM_pref <- unlist(rmse_GAM_pref)
  rmse_GEO_pref[[i]] <- rmse(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_GEO_pref <- unlist(rmse_GEO_pref)
  rmse_MPP_pref[[i]] <- rmse(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i],
                             elementos_data$BIO_SIM[elementos_data$SIM == i])
  rmse_MPP_pref <- unlist(rmse_MPP_pref)
}

cor_GLM_pref <- list()
cor_GAM_pref <- list()
cor_GEO_pref <- list()
cor_MPP_pref <- list()

for(i in 1:30) {
  print(i)
  cor_GLM_pref[[i]] <- cor(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i],
                        elementos_data$BIO_SIM[elementos_data$SIM == i],
                        method = c("spearman"))
  cor_GLM_pref <- unlist(cor_GLM_pref)
  cor_GAM_pref[[i]] <- cor(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i],
                        elementos_data$BIO_SIM[elementos_data$SIM == i],
                        method = c("spearman"))
  cor_GAM_pref <- unlist(cor_GAM_pref)
  cor_GEO_pref[[i]] <- cor(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i],
                        elementos_data$BIO_SIM[elementos_data$SIM == i],
                        method = c("spearman"))
  cor_GEO_pref <- unlist(cor_GEO_pref)
  cor_MPP_pref[[i]] <- cor(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i],
                           elementos_data$BIO_SIM[elementos_data$SIM == i],
                           method = c("spearman"))
  cor_MPP_pref <- unlist(cor_MPP_pref)
}

cor_pref = c(as.numeric(cor_GLM_pref),
               as.numeric(cor_GAM_pref),
               as.numeric(cor_GEO_pref),
               as.numeric(cor_MPP_pref))

rmse_pref = c(as.numeric(rmse_GLM_pref),
                as.numeric(rmse_GAM_pref),
                as.numeric(rmse_GEO_pref),
                as.numeric(rmse_MPP_pref))

data_pref_measures <- data.frame(
  models = rep(c("GLM","GAM","GEO","MPP"), each = 30),
  cor = cor_pref,
  rmse = rmse_pref
)

cor_pref_p <- ggplot(data_pref_measures, aes(x = factor(models), y = cor, group = models)) + 
  geom_boxplot(fill = "#69b3a2", alpha = 0.5) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() + xlab("") + ylab("Spearman correlation coefficient")

rmse_pref_p <- ggplot(data_pref_measures, aes(x = factor(models), y = rmse, group = models)) + 
  geom_boxplot(fill = "#69b3a2", alpha = 0.5) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_classic() + xlab("") + ylab("Root Mean Square Error (RMSE)")


figure <- ggarrange(scatter_random + rremove("ylab") + rremove("xlab"),
                    cor_random_p + rremove("ylab") + rremove("xlab"),
                    rmse_random_p + rremove("ylab") + rremove("xlab"),
                    scatter_pref + rremove("ylab") + rremove("xlab"),
                    cor_pref_p + rremove("ylab") + rremove("xlab"),
                    rmse_pref_p + rremove("ylab") + rremove("xlab"),
                    nrow = 2,
                    ncol = 3,
                    legend = "right",
                    widths = c(1, 0.5,0.5))

annotate_figure(figure, left = textGrob("Predicted biomass", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Simulated biomass", gp = gpar(cex = 1)))

# Diagrama de taylor
taylor <- function(observed, predicted, by = NULL, by.class = NULL, panel=TRUE) {
  
  if(is.null(by)){
    
    data <- as.data.frame(cbind(observed, predicted))
    data <- gather(data, key="Model", value="predicted", 2:ncol(data))
    return(TaylorDiagram(data, obs = "observed", mod = "predicted", group = "Model"))
    
  } else {
    
    data <- as.data.frame(cbind(by, observed, predicted))
    data <- gather(data, key="Model", value="predicted", 3:ncol(data))
    
    if(by.class == "numeric"){
      data$by <- as.numeric(data$by)
    } else{
      data$by <- as.factor(data$by)
    }
    
    if(panel){
      return(TaylorDiagram(data, obs = "observed", mod = "predicted", group = "Model", type = "by"))
    } else{
      return(TaylorDiagram(data, obs = "observed", mod = "predicted", group = c("Model", "by"))) 
    }
    
  }
  
}

## Examples ----
### One model
data_models_random <- data.frame(GLM_random = median_GLM_r$median,
                          GAM_random = median_GAM_r$median,
                          Geo_random = median_GEO_r$median)

data_models_pref <- data.frame(GLM_CPUE = median_GLM_pref$median,
                                 GAM_CPUE = median_GAM_pref$median,
                                 GEO_CPUE = median_GEO_pref$median,
                                 PREF_CPUE = median_MPP_pref$median)

taylor_random_median <- taylor(observed = media_bio_sim$mean_bio_sim,
       predicted = data_models_random,
       panel=T)

taylor_random_median$plot$legend <- NULL

taylor_pref_median <- taylor(observed = media_bio_sim$mean_bio_sim,
       predicted = data_models_pref,
       panel=T)

taylor_pref_median$plot$legend <- NULL

# Crea las filas 2 y 4 con grid.arrange
fila_2 <- grid.arrange(cor_random_p, rmse_random_p, taylor_random_median$plot,
                       ncol = 3)
fila_4 <- grid.arrange(cor_pref_p, rmse_pref_p, taylor_pref_median$plot,
                       ncol = 3)

# Crea la figura final con grid.arrange
figure_1 <- grid.arrange(scatter_random, fila_2,
                         nrow = 2, ncol = 1)

ggsave(filename = "F1.svg", plot = figure_1, height = 12, width = 16)

figure_2 <- grid.arrange(scatter_pref, fila_4,
                         nrow = 2, ncol = 1)

ggsave(filename = "F2.svg", plot = figure_2, height = 12, width = 16)

F_paper <- ggarrange(figure_1, figure_2, ncol = 1, nrow = 2)

ggsave(filename = "Figure_measures.svg", plot = F_paper, height = 20, width = 12)


# Table measures error and uncertainty
# Random
MAPE_GLM_r <- list()
MAPE_GAM_r <- list()
MAPE_GEO_r <- list()

for(i in 1:30) {
  print(i)
  MAPE_GLM_r[[i]] <- mape(elementos_data$GLM_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_GLM_r <- unlist(MAPE_GLM_r)
  MAPE_GAM_r[[i]] <- mape(elementos_data$GAM_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_GAM_r <- unlist(MAPE_GAM_r)
  MAPE_GEO_r[[i]] <- mape(elementos_data$GEO_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_GEO_r <- unlist(MAPE_GEO_r)
}

sd_GLM_r <- list()
sd_GAM_r <- list()
sd_GEO_r <- list()

for(i in 1:30) {
  print(i)
  sd_GLM_r[[i]] <- sd(elementos_data$GLM_trans[elementos_data$SIM == i])
  sd_GLM_r <- unlist(sd_GLM_r)
  sd_GAM_r[[i]] <- sd(elementos_data$GAM_trans[elementos_data$SIM == i])
  sd_GAM_r <- unlist(sd_GAM_r)
  sd_GEO_r[[i]] <- sd(elementos_data$GEO_trans[elementos_data$SIM == i])
  sd_GEO_r <- unlist(sd_GEO_r)
}

IQR_GLM_r <- list()
IQR_GAM_r <- list()
IQR_GEO_r <- list()

for(i in 1:30) {
  print(i)
  IQR_GLM_r[[i]] <- IQR(elementos_data$GLM_trans[elementos_data$SIM == i])
  IQR_GLM_r <- unlist(IQR_GLM_r)
  IQR_GAM_r[[i]] <- IQR(elementos_data$GAM_trans[elementos_data$SIM == i])
  IQR_GAM_r <- unlist(IQR_GAM_r)
  IQR_GEO_r[[i]] <- IQR(elementos_data$GEO_trans[elementos_data$SIM == i])
  IQR_GEO_r <- unlist(IQR_GEO_r)
}

MSA <- function(x,y) {
  
  ratio <- function(x,y) {ifelse(x < y, y,x) / ifelse(x < y, x, y)}
  MSA <- 100*(exp(median(log(ratio(x,y))))-1)
  
  return(MSA)
}

Q_GLM_r <- list()
Q_GAM_r <- list()
Q_GEO_r <- list()
for(i in 1:30) {
  print(i)
  Q_GLM_r[[i]] <- MSA(elementos_data$GLM_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_GLM_r <- unlist(Q_GLM_r)
  Q_GAM_r[[i]] <- MSA(elementos_data$GAM_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_GAM_r <- unlist(Q_GAM_r)
  Q_GEO_r[[i]] <- MSA(elementos_data$GEO_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_GEO_r <- unlist(Q_GEO_r)
}

SSPB <- function(y,x) {
  Q <- y/x
  SSPB <- 100*sign(median(log(Q)))*(exp(abs(median(log(Q))))-1)
  
  return(SSPB)
}

SSPB_GLM_r <- list()
SSPB_GAM_r <- list()
SSPB_GEO_r <- list()
for(i in 1:30) {
  print(i)
  SSPB_GLM_r[[i]] <- SSPB(elementos_data$GLM_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_GLM_r <- unlist(SSPB_GLM_r)
  SSPB_GAM_r[[i]] <- SSPB(elementos_data$GAM_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_GAM_r <- unlist(SSPB_GAM_r)
  SSPB_GEO_r[[i]] <- SSPB(elementos_data$GEO_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_GEO_r <- unlist(SSPB_GEO_r)
}

MSA_r <- c(as.numeric(Q_GLM_r),
           as.numeric(Q_GAM_r),
           as.numeric(Q_GEO_r))

SSPB_r <- c(as.numeric(SSPB_GLM_r),
           as.numeric(SSPB_GAM_r),
           as.numeric(SSPB_GEO_r))

MAPE_r = c(as.numeric(MAPE_GLM_r),
             as.numeric(MAPE_GAM_r),
             as.numeric(MAPE_GEO_r))

sd_r = c(as.numeric(sd_GLM_r),
              as.numeric(sd_GAM_r),
              as.numeric(sd_GEO_r))

IQR_r = c(as.numeric(IQR_GLM_r),
         as.numeric(IQR_GAM_r),
         as.numeric(IQR_GEO_r))

data_random_table <- data.frame(
  models = rep(c("GLM","GAM","GEO"), each = 30),
  cor = cor_random,
  rmse = rmse_random,
  mape = MAPE_r,
  msa = MSA_r,
  sspb = SSPB_r,
  sd = sd_r,
  iqr = IQR_r
)

mean_data_random_table <- aggregate(. ~ models, data_random_table, median)
mean_data_random_table$cor <- round(mean_data_random_table$cor,2)
mean_data_random_table$rmse <- round(mean_data_random_table$rmse,2)
mean_data_random_table$mape <- round(mean_data_random_table$mape,2)
mean_data_random_table$msa <- round(mean_data_random_table$msa,2)
mean_data_random_table$sspb <- round(mean_data_random_table$sspb,2)
mean_data_random_table$sd <- round(mean_data_random_table$sd,2)
mean_data_random_table$iqr <- round(mean_data_random_table$iqr,2)

# PREF
MAPE_GLM_pref <- list()
MAPE_GAM_pref <- list()
MAPE_GEO_pref <- list()
MAPE_MPP_pref <- list()

for(i in 1:30) {
  print(i)
  MAPE_GLM_pref[[i]] <- mape(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_GLM_pref <- unlist(MAPE_GLM_pref)
  MAPE_GAM_pref[[i]] <- mape(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_GAM_pref <- unlist(MAPE_GAM_pref)
  MAPE_GEO_pref[[i]] <- mape(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_GEO_pref <- unlist(MAPE_GEO_pref)
  MAPE_MPP_pref[[i]] <- mape(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  MAPE_MPP_pref <- unlist(MAPE_MPP_pref)
}

sd_GLM_pref <- list()
sd_GAM_pref <- list()
sd_GEO_pref <- list()
sd_MPP_pref <- list()
sd(elementos_data$BIO_SIM[elementos_data$SIM == 1])
IQR(elementos_data$BIO_SIM[elementos_data$SIM == 1])
for(i in 1:30) {
  print(i)
  sd_GLM_pref[[i]] <- sd(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i])
  sd_GLM_pref <- unlist(sd_GLM_pref)
  sd_GAM_pref[[i]] <- sd(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i])
  sd_GAM_pref <- unlist(sd_GAM_pref)
  sd_GEO_pref[[i]] <- sd(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i])
  sd_GEO_pref <- unlist(sd_GEO_pref)
  sd_MPP_pref[[i]] <- sd(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i])
  sd_MPP_pref <- unlist(sd_MPP_pref)
}

IQR_GLM_pref <- list()
IQR_GAM_pref <- list()
IQR_GEO_pref <- list()
IQR_MPP_pref <- list()

for(i in 1:30) {
  print(i)
  IQR_GLM_pref[[i]] <- IQR(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i])
  IQR_GLM_pref <- unlist(IQR_GLM_pref)
  IQR_GAM_pref[[i]] <- IQR(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i])
  IQR_GAM_pref <- unlist(IQR_GAM_pref)
  IQR_GEO_pref[[i]] <- IQR(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i])
  IQR_GEO_pref <- unlist(IQR_GEO_pref)
  IQR_MPP_pref[[i]] <- IQR(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i])
  IQR_MPP_pref <- unlist(IQR_MPP_pref)
}

Q_GLM_pref <- list()
Q_GAM_pref <- list()
Q_GEO_pref <- list()
Q_MPP_pref <- list()
for(i in 1:30) {
  print(i)
  Q_GLM_pref[[i]] <- MSA(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_GLM_pref <- unlist(Q_GLM_pref)
  Q_GAM_pref[[i]] <- MSA(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_GAM_pref <- unlist(Q_GAM_pref)
  Q_GEO_pref[[i]] <- MSA(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i],
                      elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_GEO_pref <- unlist(Q_GEO_pref)
  Q_MPP_pref[[i]] <- MSA(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i],
                         elementos_data$BIO_SIM[elementos_data$SIM == i])
  Q_MPP_pref <- unlist(Q_MPP_pref)
}

SSPB_GLM_pref <- list()
SSPB_GAM_pref <- list()
SSPB_GEO_pref <- list()
SSPB_MPP_pref <- list()
for(i in 1:30) {
  print(i)
  SSPB_GLM_pref[[i]] <- SSPB(elementos_data$GLM_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_GLM_pref <- unlist(SSPB_GLM_pref)
  SSPB_GAM_pref[[i]] <- SSPB(elementos_data$GAM_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_GAM_pref <- unlist(SSPB_GAM_pref)
  SSPB_GEO_pref[[i]] <- SSPB(elementos_data$GEO_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_GEO_pref <- unlist(SSPB_GEO_pref)
  SSPB_MPP_pref[[i]] <- SSPB(elementos_data$PREF_CPUE_trans[elementos_data$SIM == i],
                          elementos_data$BIO_SIM[elementos_data$SIM == i])
  SSPB_MPP_pref <- unlist(SSPB_MPP_pref)
}

MSA_pref <- c(as.numeric(Q_GLM_pref),
           as.numeric(Q_GAM_pref),
           as.numeric(Q_GEO_pref),
           as.numeric(Q_MPP_pref)
           )

SSPB_pref <- c(as.numeric(SSPB_GLM_pref),
              as.numeric(SSPB_GAM_pref),
              as.numeric(SSPB_GEO_pref),
              as.numeric(SSPB_MPP_pref)
)

MAPE_pref = c(as.numeric(MAPE_GLM_pref),
           as.numeric(MAPE_GAM_pref),
           as.numeric(MAPE_GEO_pref),
           as.numeric(MAPE_MPP_pref)
           )

sd_pref = c(as.numeric(sd_GLM_pref),
         as.numeric(sd_GAM_pref),
         as.numeric(sd_GEO_pref),
         as.numeric(sd_MPP_pref))

IQR_pref = c(as.numeric(IQR_GLM_pref),
          as.numeric(IQR_GAM_pref),
          as.numeric(IQR_GEO_pref),
          as.numeric(IQR_MPP_pref))

data_pref_table <- data.frame(
  models = rep(c("GLM","GAM","GEO","MPP"), each = 30),
  cor = cor_pref,
  rmse = rmse_pref,
  mape = MAPE_pref,
  msa = MSA_pref,
  sspb = SSPB_pref,
  sd = sd_pref,
  iqr = IQR_pref
)

mean_data_pref_table <- aggregate(. ~ models, data_pref_table, median)
mean_data_pref_table$cor <- round(mean_data_pref_table$cor,2)
mean_data_pref_table$rmse <- round(mean_data_pref_table$rmse,2)
mean_data_pref_table$mape <- round(mean_data_pref_table$mape,2)
mean_data_pref_table$msa <- round(mean_data_pref_table$msa,2)
mean_data_pref_table$sspb <- round(mean_data_pref_table$sspb,2)
mean_data_pref_table$sd <- round(mean_data_pref_table$sd,2)
mean_data_pref_table$iqr <- round(mean_data_pref_table$iqr,2)

## ggplot SSPB

sspb_r_p <- ggplot(data_random_table, aes(x = models, y = sspb, fill = models)) +
  geom_boxplot() + geom_point() +
  scale_fill_manual(values = c("firebrick2",
                               "dodgerblue3",
                               "mediumseagreen")) +
  xlab(label = "") +
  ylab(label = "SSPB") +
  theme_classic() +
  guides(fill = "none") + ggtitle(label = "Survey biomass indices")

sspb_pref_p <- ggplot(data_pref_table, aes(x = models, y = sspb, fill = models)) +
  geom_boxplot() + geom_point() +
  scale_fill_manual(values = c("firebrick2",
                               "dodgerblue3",
                               "mediumseagreen",
                               "plum4")) +
  xlab(label = "") +
  ylab(label = "") +
  theme_classic() + 
  labs(fill = "") + ggtitle(label = "CPUE biomass indices")

sspb_figure <- ggpubr::ggarrange(sspb_r_p,sspb_pref_p)

ggsave(filename = "SSPB_figure.pdf", plot = sspb_figure, height = 10, width = 16)

write.csv(mean_data_pref_table, "./data_measures_pref.csv", row.names = FALSE)
write.csv(mean_data_random_table, "./data_measures_random.csv", row.names = FALSE)
