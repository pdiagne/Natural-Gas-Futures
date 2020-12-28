rm(list=ls())
shell("cls")
start_time = Sys.time()

library(readxl)
library(writexl)
library(Quandl)
library(zoo)
library(plyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(stringr)
library(dplyr)
library(tidyr)
# remove.packages('xlsx')

dir <- ''
setwd(dir)
my_api <- ''
api <- Quandl.api_key(my_api)

# *******
# Load and Clean Quandl Data
# *******

# U.S. Energy Information Administration Data
  # https://www.quandl.com/data/EIA-U-S-Energy-Information-Administration-Data/documentation

# Natural Gas Indiactor Codes (IC)  
  # https://s3.amazonaws.com/quandl-production-static/EIA+codes/EIA_NG_codes.txt
ic <- read_xlsx('Nat_Gas_Indicator_Codes.xlsx') # Load Natural Gas Indicator Codes
ic <- ic[,c(1,3,5)]
names(ic)[1] <- 'Code'
names(ic)[2] <-'Description'
names(ic)[3] <- 'Units'
u <- data.frame(do.call('rbind', strsplit(as.character(ic$Units),'. ',fixed=TRUE))) # separate out units
ic$Units <- u[,1]
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
ic <- ic[substrRight(ic$Code,1) == "M",] # Filter for monthly datasets
ic <- ic[grep('California',ic$Description),] # Filter for CA datasets
ic$Code_long <- paste0('EIA/',ic$Code)

# Load and Clean IC data
i = 1:nrow(ic)
d <- Quandl(ic$Code_long[i])
names(d)[i+1] <- paste(ic$Code[i],ic$Units[i], sep = '_')
d_e <- d[,-which(apply(d == 0,2,any))]

# Wiki Continuous Futures
  # https://www.quandl.com/data/CHRIS-Wiki-Continuous-Futures/documentation
FC <- read_xlsx('Nat_Gas_Futures.xlsx') # Load Natural Gas Indicator Codes
headers <-c("Ticker","Exchange","Name","Quandl Code")
fc <- as.data.frame(matrix(,ncol=4,nrow=sum(FC$`Number of Contracts`)))
names(fc)<-headers
s = 1 
for (i in 1:nrow(FC)) {
  num <- FC$`Number of Contracts`[i]
  for (j in 1:num) {
    fc$Ticker[s] <- FC$Ticker[i]
    fc$Exchange[s] <- FC$Exchange[i]
    fc$Name[s] <- FC$Name[i]
    fc$`Quandl Code`[s] <- paste0(FC$`Quandl Code`[i],j)
    s <- s + 1  
  }
}
i = 1:nrow(fc)
d <- Quandl(paste0(fc$`Quandl Code`[i],".6")) #column 6 is the Settle price
d_f <- d[,-grep('ERROR',colnames(d))]
T_1 <- "CHRIS.CME_NS1 - Settle"
T_2 <- 'CHRIS.CME_XN1 - Settle'
ind_max <- max(min(which(!is.na(d_f[,names(d_f) == T_1]))),min(which(!is.na(d_f[,names(d_f) == T_2]))))
ind_min <- min(max(which(!is.na(d_f[,names(d_f) == T_1]))),max(which(!is.na(d_f[,names(d_f) == T_2]))))
date_min <- d_f$Date[ind_max]
date_max <- d_f$Date[ind_min]

# Combine data sets
d <- join(d_f,d_e,by = 'Date')
d <- na.locf(d, na.rm = FALSE, fromLast = TRUE) 
d <- na.locf(d, na.rm = FALSE, fromLast = FALSE)
is.na(d) <- d == 0
d <- na.locf(d,na.rm=FALSE)  # Replace zeros with previous value
ind <- which(which(d$Date <= date_max) %in% which(d$Date >= date_min))
d <- d[ind,]

# *******
# Auto-ML Data Setup
# *******

# Target(s)
# CHRIS.CME_NS1 : SoCal Natural Gas (Platts IFERC) Basis
# CHRIS.CME_NS2 : SoCal Natural Gas (Platts IFERC) Basis
# CHRIS.CME_XN1 : SoCal Natural Gas (Platts IFERC) Fixed Price
# CHRIS.CME_XN2 : SoCal Natural Gas (Platts IFERC) Fixed Price

# CHRIS/CME_NS1 target
t_1 <- "CHRIS.CME_NS1 - Settle"
ind_1 <- names(d) %in% c('CHRIS.CME_NS2 - Settle','CHRIS.CME_XN1 - Settle','CHRIS.CME_XN2 - Settle') 
D_1 <- d[,-which(ind_1)]
# CHRIS.CME_NS2 target
t_2 <- 'CHRIS.CME_NS2 - Settle'
ind_2 <- names(d) %in% c('CHRIS.CME_NS1 - Settle','CHRIS.CME_XN1 - Settle','CHRIS.CME_XN2 - Settle') 
D_2 <- d[,-which(ind_2)]
# CHRIS.CME_XN1 targt
t_3 <- 'CHRIS.CME_XN1 - Settle'
ind_3 <- names(d) %in% c('CHRIS.CME_NS1 - Settle','CHRIS.CME_NS2 - Settle','CHRIS.CME_XN2 - Settle')
D_3 <- d[,-which(ind_3)]
# CHRIS.CME_XN2 target
t_4 <- 'CHRIS.CME_XN2 - Settle'
ind_4 <- names(d) %in% c('CHRIS.CME_NS1 - Settle','CHRIS.CME_NS2 - Settle','CHRIS.CME_XN1 - Settle') 
D_4 <- d[,-which(ind_4)]

t <- t_4 # select target feature
D <- D_4 
rtime <- 60
nmodels <- 20

# *******
# Method 1: GLM p-value
# *******

# H2O - https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
library(h2o)
h2o.init()
  # After starting H2O, you can use the Web UI at http://localhost:54321

# aside - the other p-value calc option is glm. But must define smoothing features
  # Generalized Additive Models (GAM)
  # https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gam.html
  # http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/compute_p_values.html

# Feature Importance with Generalized Linear Model
  # https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html
  # http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/compute_p_values.html

Dd <- as.h2o(D[,-1])
y <- t # column name
x <- names(Dd)[names(Dd) != y]

glm <- h2o.glm(family = "gaussian",
                        x = x,
                        y = y,
                        training_frame = Dd,
                        lambda = 0,
                        remove_collinear_columns = TRUE,
                        compute_p_values = TRUE)

coeff <- as.data.frame(h2o.coef(glm))
coeff_n <- as.data.frame(h2o.coef_norm(glm))
c_table <- as.data.frame(glm@model$coefficients_table)
c_table <- arrange(c_table,p_value)
f <- c_table$names[1:25] # select top features 
write_xlsx(c_table[1:25,], paste0(dir,'//Results//','features_m1_',t,'_',rtime,'.xlsx'))# export features table

# Train Test split
date_c <- D$Date
Dl <- as.h2o(D[which(date_c <= '2016-01-01'),-1])
splits <- h2o.splitFrame(Dl, ratios = 0.8, seed = 1)
train <- splits[[1]]
test <- splits[[2]]

# Run AutoML for X base models (limited to 1 hour max runtime by default)
aml <- h2o.automl(y = y, x = f,
                  training_frame = train, max_runtime_secs = rtime,
                  max_models = nmodels,
                  seed = 1)

# Get leaderboard with 'extra_columns = 'ALL'
lb <- h2o.get_leaderboard(object = aml, extra_columns = 'ALL')
write_xlsx(as.data.frame(lb), paste0(dir,'//Results//','lb_m1_',t,'_',rtime,'.xlsx'))# export features table

# Out of sample prediction
dates <- d$Date[which(date_c > '2016-01-01')]
newd <- as.h2o(d[which(date_c > '2016-01-01'),])
pred <- as.matrix(h2o.predict(object = aml@leader, newdata = newd))
y_t <- as.matrix(newd[,which(names(newd) == t)])
mae <- sum(abs(pred - y_t))/length(pred)
rmse <- sqrt(mean((pred - y_t)**2)) # RMSE
sse <- sum((pred - y_t)**2) # SSE

# Pred vs target
jpeg(paste0(dir,'//Results//','figure_m1_',t,'_',rtime,'.jpg'))
plot(dates, y_t, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Days", ylab = "$")
# Add a second line
lines(dates, pred, pch = 18, col = "blue", type = "b", lty = 2)
lines(dates, matrix(rmse, nrow = length(dates), ncol = 1), pch = 18, col = "green", type = "b", lty = 2)
# Add a legend to the plot
legend("topleft", legend=c(paste0("target = ",t), "pred", paste0("MAE = ",round(mae,5))),
       col=c("red", "blue", 'green'), lty = 1:2, cex=0.8)
title('Method 1 - GLM p-value')
dev.off()

# write_xlsx(, paste0(dir,'//Results//','plot_m1_',t,'_','mae_',mae,'_',rtime,'.xlsx'))# export features table

# *******
# Method 2 : Dynamic Time Warping Average Cost
# *******

# Feature importance with Dynamic Time Warping
  # https://github.com/maduprey/DynamicTimeWarping
  # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.465.4905&rep=rep1&type=pdf

library(plotly)
library(purrr)
source("./dtwCostMatrices.R")
source("./optimalWarpPath.R")

# Define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max normalization
D_n <- d[,-1]
#D_n <- as.data.frame(lapply(D_n, min_max_norm))

# Dynamic time warping feature selection
a <- D_n[,names(D_n) == t_1]
b <- D_n[,names(D_n) != t_1]
asd <- as.data.frame(matrix(,ncol=2,nrow=ncol(b)))
names(asd)<- c('Name','Distance')
for (i in 1:ncol(b)) {
  # Apply DTW over s- and t-sequences
  dtw = dtwCostMatrices(a, b[,i])
  
  # Extract cost matrices
  acc.cost.m = dtw$acc.cost.m
  local.cost.m = dtw$local.cost.m
  
  # Calculate optimal warp path
  path = optimalWarpPath(acc.cost.m)
  #asd$Distance[i] = Reduce('+',path)[3]/length(path)
  asd$Distance[i] = mean(path[[3]])
}
asd$Name <- names(b)
asd <- arrange(asd,Distance)
f <- asd$Name[1:25] # select top features
asd_f <- asd[1:25,]
write_xlsx(asd_f, paste0(dir,'//Results//','features_m2_',t,'_',rtime,'.xlsx'))# export features table

# Train Test split
date_c <- D$Date
Dl <- as.h2o(D_n[which(date_c <= '2016-01-01'),])
splits <- h2o.splitFrame(Dl, ratios = 0.8, seed = 1)
train <- splits[[1]]
test <- splits[[2]]

# Run AutoML for X base models (limited to 1 hour max runtime by default)
aml <- h2o.automl(y = y, x = f,
                  training_frame = train, max_runtime_secs = rtime,
                  max_models = nmodels,
                  seed = 1)

# Get leaderboard with 'extra_columns = 'ALL'
lb <- h2o.get_leaderboard(object = aml, extra_columns = 'ALL')
write_xlsx(as.data.frame(lb), paste0(dir,'//Results//','lb_m2_',t,'_',rtime,'.xlsx'))# export features table

# Out of sample prediction
dates <- d$Date[which(date_c > '2016-01-01')]
newd <- as.h2o(d[which(date_c > '2016-01-01'),])
pred <- as.matrix(h2o.predict(object = aml@leader, newdata = newd))
y_t <- as.matrix(newd[,which(names(newd) == t)])
mae <- sum(abs(pred - y_t))/length(pred)
rmse <- sqrt(mean((pred - y_t)**2)) # RMSE
sse <- sum((pred - y_t)**2) # SSE

# Pred vs target
jpeg(paste0(dir,'//Results//','figure_m2_',t,'_',rtime,'.jpg'))
plot(dates, y_t, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Days", ylab = "$")
# Add a second line
lines(dates, pred, pch = 18, col = "blue", type = "b", lty = 2)
lines(dates, matrix(rmse, nrow = length(dates), ncol = 1), pch = 18, col = "green", type = "b", lty = 2)
# Add a legend to the plot
legend("topleft", legend=c(paste0("target = ",t), "pred", paste0("MAE = ",round(mae,5))),
       col=c("red", "blue", 'green'), lty = 1:2, cex=0.8)
title('Method 2 - Dynamic Time Warping Average Cost')
dev.off()

# *******
# Popup Window when done
# *******
end_time <- Sys.time()
winDialog(type = c("ok"), paste0('Runtime = ',(round(end_time - start_time)), ' min'))

# # max models + 2 possible plots
# m_1 <- lb[1,1]
# m_2 <- lb[2,1]
# m_3 <- lb[3,1]
# m_4 <- lb[4,1]
# m_5 <- lb[5,1]
# m_6 <- lb[6,1]
# m_7 <- lb[7,1]
# model = m_4
# 
# 
# # Methods for an AutoML object
# h2o.varimp_heatmap(aml)
# h2o.model_correlation_heatmap(aml,newd)
#   # h2o.pd_multi_plot()
# 
# # Methods for an H2O model
# h2o.residual_analysis_plot(model,newd)
#   # h2o.varimp_plot(model)
#   # h2o.pd_plot(model,newd, column = 'CHRIS/CME_7Q1')
#   # h2o.ice_plot(model)
# 
# # Residual Analysis
# ra_plot <- h2o.residual_analysis_plot(model, newd)
# ra_plot
# 
# # Variable Importance
# va_plot <- h2o.varimp_plot(model, newd)
# va_plot
# 
# #Variable Importance Heatmap
# va_plot <- h2o.varimp_heatmap(aml)
# va_plot
# 
# # Model Correlation Heatmap
# mc_plot <- h2o.model_correlation_heatmap(aml, newd)
# mc_plot
# 
# # Partial Dependence (PD) Plots
# pd_plot <- h2o.pd_multi_plot(aml, newd)
# pd_plot
# 
# # Partial Dependence Single-model Plot
# pd_plot <- h2o.pd_plot(model, newd)
# pd_plot
# 
# # Individual Conditional Expectiation (ICE) Plots
# ice_plot <- h2o.ice_plot(model, newd)
# ice_plot