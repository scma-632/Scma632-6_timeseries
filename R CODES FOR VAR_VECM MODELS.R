#https://www.r-bloggers.com/2021/11/vector-autoregressive-model-var-using-r/
setwd('D:\\SCMA 2024\\Data')
getwd()
library(readxl)
sheet = excel_sheets('macronew.xlsx')
sheet

df1= read_excel('macronew.xlsx', sheet='deficit')
df2= read_excel('macronew.xlsx', sheet='CREDIT')
df3= read_excel('macronew.xlsx', sheet='GDP')
head(df3)


# Load necessary libraries
library(dplyr)

# Specify the file path
file <- "macronew.xlsx"

# List of sheet names
sheets <- c("deficit", "CREDIT", "GDP", "EXIM", "WPI", "IIP", "IAP", "FDI", "EXRATE", "BOP", "EMPLOY", "M", "RAIN", "oil","GDP_W")

# Initialize an empty list to store dataframes
data_frames <- list()

# Loop through each sheet name and read the data into a dataframe
for (sheet in sheets) {
  data_frames[[sheet]] <- read_excel(file, sheet = sheet)
}

# Merge all dataframes using "deficit" as the base for merging by "year"
combined_data <- data_frames[["deficit"]]

for (sheet in sheets[-1]) {
  combined_data <- combined_data %>%
    left_join(data_frames[[sheet]], by = "Year")
}

# View the combined data
print(combined_data)

dim(combined_data)
names(combined_data)

macro = combined_data[,c(1,4,13,14,15,24,36,37,63,65,73,83,84,85,86)]
head(macro)
tail(macro)
dput(names(macro))
pairs(df1)

library(janitor)
macro = clean_names(macro)
dput(names(macro))
tail(macro)
df1 = macro[,c(5,2,6,7)]
names(df1)
View(df1)
plot(df1)
library(tseries)
any(is.na(df1))
df1 = na.omit(df1)
any(is.na(df1))

adf.test(df1$per_cap_gdp)
adf.test(df1$gross_fiscal_deficit_3_2)
adf.test(df1$cad_tot)
adf.test(df1$iip)

adf.test(diff(df1$per_cap_gdp))#I(1)
adf.test(diff(df1$gross_fiscal_deficit_3_2))
adf.test(diff(df1$cad_tot))
adf.test(diff(df1$iip))


adf.test(diff(diff(df1$gross_fiscal_deficit_3_2)))#I(2)
adf.test(diff(diff(df1$cad_tot)))#I(2)
adf.test(diff(diff(df1$iip)))#I(2)

# Vector Auto Regression Models
library(vars) 
#install.packages('tsDyn')
library(tsDyn)
dim(df1)


VARselect(df1, lag.max = 4,type = 'const')
plot(df1)

library(urca) # ca.jo, denmark
library(vars) # vec2var

var.model_lev <- VAR(df1, p = 4, type = 'const') 
var.model_lev

var.pred <- predict(var.model_lev, n.ahead = 5)
x11(); par(mai=rep(0.4, 4)); plot(var.pred)
x11(); par(mai=rep(0.4, 4)); fanchart(var.pred)
var.pred


# 1st differenced data
df.diff <- diff(as.matrix(df1, lag = 1))
m.diff <-as.matrix(df.diff)

# lag length
VARselect(df.diff, lag.max = 4,type = 'const')
 
# estimation
vare_diff <- VAR(df.diff, p = 4, 
                 type = 'const')
vare_diff 
# forecast of differenced data
varf_diff <- predict(vare_diff, n.ahead = 5)
x11(); par(mai=rep(0.4,4)); plot(varf_diff)
x11(); par(mai=rep(0.4,4)); fanchart(varf_diff)
 
 

 
 
#========================================================
# VAR model in difference using tsDyn
#========================================================

linevare_diff <- lineVar(data = df1, lag = 1, include = 'const',
        model = 'VAR', I = 'diff', beta = NULL, exogen = NULL)
 
# check if both models (vars & tsDyn) yield same coefficients
linevare_diff 
do.call(rbind,lapply(vare_diff$varresult, 
                     function(x) x$coefficients))
 
# forecast
linevarf_diff <- predict(linevare_diff, n.ahead = 5, 
                         exoPred = NULL) 
# Draw Graph
x11(width=8, height = 8); 
par(mfrow=c(4,1), mar=c(2,2,2,2))
 
df <- rbind(df.lev, linevarf_diff)
for(i in 1:4) {
    matplot(df[,i], type=c('l'), col = c('blue'), 
            main = str.main[i]) 
    abline(v=nr_lev, col='blue')
}



# Fit the VAR model
linevare_diff <- VAR(df1, p = 4, type = 'const')

# Calculate the impulse response functions
irf_results <- irf(linevare_diff, response = c("per_cap_gdp"))

# Plot the impulse response functions
plot(irf_results)

***********************************************************************************
library(readxl)
library(vars)
#install.packages('mFilter')
library(mFilter)
library(tseries)
#nstall.packages('TSstudio')
library(TSstudio)
library(forecast)
library(tidyverse)
library(tsDyn)
library(urca)
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#TEST OF STATIONARITY
PP.test(df1$per_cap_gdp)
PP.test(df1$cad_tot)# I(0)
PP.test(df1$iip)
PP.test(df1$gross_fiscal_deficit_3_2)


PP.test(diff(df1$per_cap_gdp))# I(1)
PP.test(diff(df1$iip))# I(1)
PP.test(diff(df1$gross_fiscal_deficit_3_2))# I(1)


v1 <- df1
#######################lag selection##################
lagselect <- VARselect(v1, lag.max = 4, type = "const")

lagselect$selection
###################model fitting########################
Model1 <- VAR(v1, p = 4, type = "const", season = NULL, exog = NULL) 
summary(Model1)
#install.packages('stargazer')
library(stargazer)
eee<-stargazer(Model1[["varresult"]],type = 'text')

#########################Model Diagnostics###############
Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1

#########################heteroscedasticity check###################
Arch1 <- arch.test(Model1, lags.multi = 5, multivariate.only = TRUE)
Arch1
#########################normality of the residuals################
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1

#####################graphics########################
dev.off()
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))

##############################stability of the model#####################
Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

###########################################################################
##############causality############
Granger_pci<- causality(Model1, cause = "per_cap_gdp")
Granger_pci
Granger_trade<- causality(Model1, cause = "cad_tot")
Granger_trade
Granger_IIP <- causality(Model1, cause = "iip")
Granger_IIP
Granger_deficit <- causality(Model1, cause = "gross_fiscal_deficit_3_2")
Granger_deficit

###########################impulse response functions.######################
pci_trade_irf <- irf(Model1, impulse = "cad_tot", response = "per_cap_gdp", n.ahead = 5, boot = TRUE)
plot(pci_trade_irf, ylab = "PerCapGDP", main = "trade shock to pci")

pci_IIP_irf <- irf(Model1, impulse = "iip", response = "per_cap_gdp", n.ahead = 5, boot = TRUE)
plot(pci_IIP_irf, ylab = "PerCapGDP", main = "IIP's shock to PCI")

pci_deficit_irf <- irf(Model1, impulse = "gross_fiscal_deficit_3_2", response = "per_cap_gdp", n.ahead = 5, boot = TRUE)
plot(pci_deficit_irf, ylab = "PerCapGDP", main = "Deficit's shock to PCI")

################forecast error variance decomposition.#################
FEVD1 <- fevd(Model1, n.ahead = 5)
FEVD1
plot(FEVD1)
#######################Forecasting using VAR######################
forecast <- predict(Model1, n.ahead = 5, ci = 0.95)
forecast
plot(forecast)

# CODES TO RUN VECTOR ERROR CORRECTION(VECM) MODELS
#the cointegration test
vecm.model <- ca.jo(
    v1, ecdet = 'none', 
    type  = 'eigen', K = 4, spec = 'transitory')
 
summary(vecm.model)
coint_ca.jo <- ca.jo(
      v1, ecdet = 'none', type  = 'eigen', K = 4, 
      spec = 'transitory')
  summary(coint_ca.jo)
 
 
#========================================================
# VECM model estimation
#========================================================
 
  #————————————————
  # VECM estimation
  #————————————————
  # VECM(data, lag, r = 2, 
  #      include = c('const'),
  #      beta = NULL, estim = c(“ML”), 
  #      LRinclude = c('none'), 
  #      exogen = NULL)
  #————————————————
  
  VECM_tsDyn <- VECM(v1, lag=4, r=2,
                     estim = 'ML',
                     LRinclude = 'none',
                     exogen = NULL)


  #————————————————
  # restricted VECM -> input for r
  #————————————————
  cajorls_ca.jo <- cajorls(coint_ca.jo, r=2)
  #————————————————
  # the VAR representation of a VECM from ca.jo
  #————————————————
  # vec2var: Transform a VECM to VAR in levels
  # ca.jo is transformed to a VAR in level
  # r : The cointegration rank 
  #————————————————
  vec2var_ca.jo <- vec2var(coint_ca.jo, r=2)
 
 
#========================================================
# Estimation Results
#========================================================
 
  #———————————————-
  # parameter estimates from each model
  #———————————————-
  VECM_tsDyn
  cajorls_ca.jo
  vec2var_ca.jo
 
 
#========================================================
# Forecast
#========================================================
 
  # forecasting horizon
  nhor <- 5
  
  #———————————————-
  # Forecast from VECM() in tsDyn
  #———————————————-
  
   
  VECM_pred_tsDyn <- predict(VECM_tsDyn, 
       exoPred = NULL, n.ahead=nhor)
  
  # Draw Graph
  x11(width=8, height = 8); 
  par(mfrow=c(4,1), mar=c(2,2,2,2))
  dim(v1)
  dim(VECM_pred_tsDyn)
dim(df)
  # historical data + forecast data
  df <- rbind(v1, VECM_pred_tsDyn)
  
  for(i in 1:4) {
      matplot(df[,i], type=c('l'), col = c('blue'), 
              main = str.main[i]) 
      abline(v=nr_lev, col='blue')
  }
  
  VECM_pred_tsDyn

nr_lev <- c(10, 20, 30)
# Define str.main with the appropriate titles
str.main <- c("Title 1", "Title 2", "Title 3", "Title 4")

# Combine v1 and VECM_pred_tsDyn into a single data frame
df <- rbind(v1, VECM_pred_tsDyn)

# Loop through the first 4 columns of df and plot them
for(i in 1:4) {
    matplot(df[,i], type='l', col='blue', 
            main = str.main[i]) 
    abline(v=nr_lev, col='blue')
}




  
  #———————————————-
  # Forecast from ca.jo() using vec2var()
  #———————————————-
  
  pred_vec2var_ca.jo <- predict(vec2var_ca.jo, n.ahead=nhor)
  
  x11(); par(mai=rep(0.4, 4)); plot(pred_vec2var_ca.jo)
  x11(); par(mai=rep(0.4, 4)); fanchart(pred_vec2var_ca.jo)
  
  m.pred_vec2var_ca.jo <- cbind(
    pred_vec2var_ca.jo$fcst$lrm1[,1], 
    pred_vec2var_ca.jo$fcst$lny[,1],
    pred_vec2var_ca.jo$fcst$lnmr[,1], 
    pred_vec2var_ca.jo$fcst$difp[,1])
  
  colnames(m.pred_vec2var_ca.jo) <- colnames(v1)
  tail(v1)
  m.pred_vec2var_ca.jo
  
  #———————————————-
  # Comparison of two sets of forecast
  #———————————————-
  
  VECM_pred_tsDyn – m.pred_vec2var_ca.jo
