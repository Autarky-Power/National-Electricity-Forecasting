library(MuMIn)


## Set working directory and check it
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


## load data
STmodel_data <- read.csv('./data_for_R_code/STmodel_data.csv')
STmodel_data$Consumption <- STmodel_data$dif



##### Deterministic short-term models for with-in sample prediction ----
### Calculating different models per day per month  --> 7*12 = 84 models  


# Prepare dataframes, where model results are stored

model_st <- data.frame(matrix(ncol = 14, nrow = 168))
colnames(model_st) <- c("wday","hour","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
model_st[,1] <- c(rep("Mo",24),rep("Di",24),rep("Mi",24),rep("Do",24),rep("Fr",24),rep("Sa",24),rep("So",24))
model_st[,2] <- 1:24

model.st <- data.frame(matrix(ncol = 4, nrow = 2016)) # 2016 = 24*7*12
colnames(model.st) <- c("month","wday","hour","fitted")
model.st[,1] <- c(rep(1,168), rep(2,168),rep(3,168),rep(4,168),rep(5,168),rep(6,168),
                  rep(7,168),rep(8,168),rep(9,168),rep(10,168),rep(11,168),rep(12,168))
model.st[,2] <- c(rep("Mo",24),rep("Di",24),rep("Mi",24),rep("Do",24),rep("Fr",24),rep("Sa",24),rep("So",24))
model.st[,3] <- 1:24


# Compute models and store results

wday <- c("Mo","Di","Mi","Do","Fr","Sa","So")

k = 1
l = 1
for (i in 1:12){
  for (j in 1:7){
    
    x <- STmodel_data[which(STmodel_data$month == i & STmodel_data$wday == wday[j] & STmodel_data$Holiday != "HD"),]
    xreg <- as.matrix(x[,10:33])
    fit1 <- lm(Consumption ~ xreg[,1:23], data=x)
    AICc(fit1)
    
    name=paste0("month",i,wday[j])
    modelname=paste0("./data_for_R_code/shortterm_models/",name,".Rdata")
    save(fit1,file=modelname)
    fv <- data.frame(fit1$fitted.values)
    model_st[k:(k+23),(i+2)] <- fv[1:24,]
    model.st[l:(l+23),4] <- fv[1:24,]
    k = k+24
    l = l+24
  }
  
  k = 1
}


# Save the dataframes with model results

write.csv(model.st,"./data_for_R_code/st_det_model_training_set.csv",row.names = FALSE )


# Combine the results

STmodel_data$deterministic_fitted <- vector(length=nrow(STmodel_data))


for (i in 1:12){
  for(j in 1:7){
    
    STmodel_data$deterministic_fitted[which(STmodel_data$month == i & STmodel_data$wday == wday[j])] <- model.st[which(model.st$month == i & model.st$wday == wday[j]),4]
    
  }
}


# Calculate residuals 

STmodel_data$res_actual <- STmodel_data$Consumption - STmodel_data$deterministic_fitted 


# Save the combined dataframe

write.csv(STmodel_data,"./data_for_R_code/STmodel_data.csv",row.names = FALSE )


# plot a sample week
source('./ggplot_theme_Publication/ggplot_theme_Publication-2.R')

STmodel_data$index <- 1:nrow(STmodel_data)
sample=STmodel_data$index[STmodel_data$year==2018 & STmodel_data$month==12 & STmodel_data$wday=='Mo']

weekstart=sample[1]
weekend  =weekstart + 24*7

ggplot() + geom_line(aes(STmodel_data$index[weekstart:weekend], STmodel_data$Consumption[weekstart:weekend], color='actual'), size=1)+
  geom_line(aes(STmodel_data$index[weekstart:weekend], STmodel_data$deterministic_fitted[weekstart:weekend],color='model'), lwd=1, linetype=2) +
  ggtitle(label = "deterministic short term model vs actual")  + 
  theme(legend.position = "right") +
  ylab("MW/h") + xlab("Timestamp") 


##### Deterministic short-term models for out-of sample prediction ----

## Load data 

testset_shortterm<- read.csv("./data_for_R_code/ST_results_test_set.csv")


# Add model results to test set dataframe. Because the predictor variables (hours) are 
# the same for every year the model results are the same for each year. Therefore the training and test set
# predictions are the same.

testset_shortterm$short_det <- 0

wday <- c("Mo","Di","Mi","Do","Fr","Sa","So")
for (i in 1:12){
  for(j in 1:7){
    for(k in 1:24){
      testset_shortterm$short_det[testset_shortterm$month==i & testset_shortterm$wday==wday[j] & testset_shortterm$hour==k] <-
        short_term_results$fitted[short_term_results$month==i & short_term_results$wday==wday[j] & short_term_results$hour==k]
      
    }}}


# Visually inspect if everything worked as intended.

testset_shortterm$index <- 1:nrow(testset_shortterm)
STmodel_data <- read.csv('./data_for_R_code/STmodel_data.csv')

ggplot()+  geom_line(aes(STmodel_data$index[5000:5268], STmodel_data$deterministic_fitted[5000:5268],  colour="modelold"))+
  geom_line(aes(testset_shortterm$index[5000:5180], testset_shortterm$short_det[5000:5180], colour="modelnew"))

# If the graphs are overlapping perfectly and change color everything is correct 

write.csv(testset_shortterm,"./data_for_R_code/ST_results_test_set.csv", row.names = FALSE)



##### Stochastic short-term models  ----
### Calculating different models for each month  --> 12 models
# Note 1:  Calculating the AR models takes a very long time due to the high order and seasonality of p and q.
#          Models have already been calculated and can be found in ./data_for_R_code/shortterm_models/arima
# Note 2:  The following code calculates the AR models for each month separately. This is done, so that the 
#          process is more clear and easy to follow, even though a function could easily be written that takes 
#          the respective month as input and returns the best AR model. 

library(tseries)
library(forecast)

#Load data

STmodel_data <- read.csv('./data_for_R_code/STmodel_data.csv')
testset_shortterm <- read.csv("./data_for_R_code/ST_results_test_set.csv")
testset_shortterm$res    <- testset_shortterm$actual - testset_shortterm$short_det


## Calculate models for each month. Only the last three years of the training set are considered

# Jan ----
res_jan <- STmodel_data$res_actual[STmodel_data$month == 1 & STmodel_data$year > 2015 ]
testset_jan <- testset_shortterm$res[testset_shortterm$month == 1 & testset_shortterm$year > 2018 ]


adf.test(res_jan)
kpss.test(res_jan)
# --> data is stationary
d=0

acf(res_jan)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_jan) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_jan))


for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_jan, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_jan - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_jan))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_jan <- Arima(res_jan, order = c(p_order, d, q_order))

save(best_model_jan,file="./data_for_R_code/shortterm_models/arima/fit.jan.Rdata")


# Feb ----
res_feb <- STmodel_data$res_actual[STmodel_data$month == 2 & STmodel_data$year > 2015 ]
testset_feb <- testset_shortterm$res[testset_shortterm$month == 2 & testset_shortterm$year > 2018 ]


adf.test(res_feb)
kpss.test(res_feb)
# --> data is stationary
d=0

acf(res_feb)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_feb) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_feb))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_feb, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_feb - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_feb))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]
#write.csv(test_df,"./Data/short_term_feb.csv")

best_model_feb <- Arima(res_feb, order = c(p_order, d, q_order))
save(best_model_feb,file="./data_for_R_code/shortterm_models/arima/fit.feb.Rdata")


# Mar ----
res_mar <- STmodel_data$res_actual[STmodel_data$month == 3 & STmodel_data$year > 2015 ]
testset_mar <- testset_shortterm$res[testset_shortterm$month == 3 & testset_shortterm$year > 2018 ]


adf.test(res_mar)
kpss.test(res_mar)
# --> data is stationary
d=0

acf(res_mar)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_mar) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_mar))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_mar, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_mar - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_mar))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]
write.csv(test_df,"./Data/short_term_mar.csv")

best_model_mar <- Arima(res_mar, order = c(p_order, d, q_order))

save(best_model_mar,file="./data_for_R_code/shortterm_models/arima/fit.mar.Rdata")


#Apr ----
res_apr <- STmodel_data$res_actual[STmodel_data$month == 4 & STmodel_data$year > 2015 ]
testset_apr <- testset_shortterm$res[testset_shortterm$month == 4 & testset_shortterm$year > 2018 ]

adf.test(res_apr)
kpss.test(res_apr)
# --> data is stationary
d=0

acf(res_apr)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_apr) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_apr))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_apr, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_apr - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_apr))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_apr <- Arima(res_apr, order = c(p_order, d, q_order))

save(best_model_apr,file="./data_for_R_code/shortterm_models/arima/fit.apr.Rdata")


# May ----
res_may <- STmodel_data$res_actual[STmodel_data$month == 5 & STmodel_data$year > 2015 ]
testset_may <- testset_shortterm$res[testset_shortterm$month == 5 & testset_shortterm$year > 2018 ]

adf.test(res_may)
kpss.test(res_may)
# --> data is stationary
d=0

acf(res_may)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_may) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_may))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_may, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_may - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_may))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

write.csv(test_df,"./data/arima_may.csv",row.names = F )
test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_may <- Arima(res_may, order = c(p_order, d, q_order))

save(best_model_may,file="./data_for_R_code/shortterm_models/arima/fit.may.Rdata")


# Jun ----

res_jun <- STmodel_data$res_actual[STmodel_data$month == 6 & STmodel_data$year > 2015 ]
testset_jun <- testset_shortterm$res[testset_shortterm$month == 6 & testset_shortterm$year > 2018 ]


adf.test(res_jun)
kpss.test(res_jun)
# --> data is stationary
d=0

acf(res_jun)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_jun) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_jun))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_jun, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_jun - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_jun))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_jun <- Arima(res_jun, order = c(p_order, d, q_order))

save(best_model_jun,file="./data_for_R_code/shortterm_models/arima/fit.jun.Rdata")


# Jul ----
res_jul <- STmodel_data$res_actual[STmodel_data$month == 7 & STmodel_data$year > 2015 ]
testset_jul <- testset_shortterm$res[testset_shortterm$month == 7 & testset_shortterm$year > 2018 ]

adf.test(res_jul)
kpss.test(res_jul)
# --> data is stationary
d=0

acf(res_jul)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_jul) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_jul))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_jul, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_jul - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_jul))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_jul <- Arima(res_jul, order = c(p_order, d, q_order))

save(best_model_jul,file=".data_for_R_code/shortterm_models/arima/fit.jul.Rdata")


# Aug ----
res_aug <- STmodel_data$res_actual[STmodel_data$month == 8 & STmodel_data$year > 2015 ]
testset_aug <- testset_shortterm$res[testset_shortterm$month == 8 & testset_shortterm$year > 2018 ]

adf.test(res_aug)
kpss.test(res_aug)
# --> data is stationary
d=0

acf(res_aug)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_aug) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_aug))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_aug, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_aug - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_aug))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_aug <- Arima(res_aug, order = c(p_order, d, q_order))

save(best_model_aug,file="./data_for_R_code/shortterm_models/arima/fit.aug.Rdata")


# Sep ----
res_sep <- STmodel_data$res_actual[STmodel_data$month == 9 & STmodel_data$year > 2015 ]
testset_sep <- testset_shortterm$res[testset_shortterm$month == 9 & testset_shortterm$year > 2018 ]

adf.test(res_sep)
kpss.test(res_sep)
# --> data is stationary
d=0

acf(res_sep)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_sep) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_sep))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_sep, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_sep - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_sep))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_sep <- Arima(res_sep, order = c(p_order, d, q_order))

save(best_model_sep,file="./data_for_R_code/shortterm_models/arima/fit.sep.Rdata")


# Oct ----
res_oct <- STmodel_data$res_actual[STmodel_data$month == 10 & STmodel_data$year > 2015 ]
testset_oct <- testset_shortterm$res[testset_shortterm$month == 10 & testset_shortterm$year > 2018 ]

adf.test(res_oct)
kpss.test(res_oct)
# --> data is stationary
d=0

acf(res_oct)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_oct) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_oct))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_oct, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_oct - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_oct))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_oct <- Arima(res_oct, order = c(p_order, d, q_order))

save(best_model_oct,file="./data_for_R_code/shortterm_models/arima/fit.oct.Rdata")


# Nov ----
res_nov <- STmodel_data$res_actual[STmodel_data$month == 11 & STmodel_data$year > 2015 ]
testset_nov <- testset_shortterm$res[testset_shortterm$month == 11 & testset_shortterm$year > 2018 ]

adf.test(res_nov)
kpss.test(res_nov)
# --> data is stationary
d=0

acf(res_nov)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_nov) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_nov))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_nov, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_nov - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_nov))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_nov <- Arima(res_nov, order = c(p_order, d, q_order))

save(best_model_nov,file="./data_for_R_code/shortterm_models/arima/fit.nov.Rdata")


# Dec ----
res_dec <- STmodel_data$res_actual[STmodel_data$month == 12 & STmodel_data$year > 2015 ]
testset_dec <- testset_shortterm$res[testset_shortterm$month == 12 & testset_shortterm$year > 2018 ]

adf.test(res_dec)
kpss.test(res_dec)
# --> data is stationary
d=0

acf(res_dec)  #  seasonal pattern around 24 lags --> max grid search p-order = 26
pacf(res_dec) # last significant lag at t=25 --> max grid search q-oder = 27

test_df<- data.frame(matrix(nrow=(26*27),ncol = 4))
colnames(test_df)<- c("AICC","p","q","res_ratio")
forecast_vector <- vector(length=length(testset_dec))

for (k in 1:27){
  q=k
  for (i in 1:26){
    tryCatch({
      print(paste('processing ARIMA:',i,d,q))
      grid_model <- Arima(res_dec, order = c(i, d, q))
      test_df$AICC[((k-1)*26+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*26+i)] <- i
      test_df$q[((k-1)*26+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset_dec - forecast_vector
      test_df$res_ratio[((k-1)*26+i)] <- sum(abs(res_predicted)) / sum(abs(testset_dec))
      print(test_df$res_ratio[((k-1)*26+i)])
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}

test_df<- test_df[is.na(test_df$res_ratio)==F,]
p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio)]
q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio)]

best_model_dec <- Arima(res_dec, order = c(p_order, d, q_order))

save(best_model_dec,file="./data_for_R_code/shortterm_models/arima/fit.dec.Rdata")


##### Combine all models  ----

# load AR models and fill the training dataframe (STmodel_data) and testset dataframe (testset_shortterm)
STmodel_data <- read.csv("./data/stmodel_data.csv")

month_list <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
STmodel_data$stoch <- 0
testset_shortterm$stoch <-0
library(forecast)
for (i in 1:12){
  path_to_model=paste0("./data_for_R_code/shortterm_models/arima/fit.",month_list[i],".Rdata")
  load(file=path_to_model)
  a=paste0("best_model_",month_list[i])
  model= get(a)
  print(a)
  STmodel_data$stoch[STmodel_data$month==i] <- model$fitted
  forecasted_short_res <-forecast(model, h=24,biasadj=TRUE,bootstrap = TRUE)  
  testset_shortterm$stoch[testset_shortterm$month==i] <- as.numeric(forecasted_short_res$mean) 
  }


# Create dataframe with all short-term results 

all_results_shortterm <- as.data.frame(matrix(nrow=70080,ncol = 1))
all_results_shortterm[1:52560,1:8] <- STmodel_data[,c(2:7,34,37)]
colnames(all_results_shortterm)<- colnames(STmodel_data)[c(2:7,34,37)] 
all_results_shortterm$year[52561:70080] <- testset_shortterm$year
all_results_shortterm$month[52561:70080] <- testset_shortterm$month
all_results_shortterm$wday[52561:70080] <- testset_shortterm$wday
all_results_shortterm$hour[52561:70080] <- testset_shortterm$hour
all_results_shortterm$Consumption[52561:70080] <- testset_shortterm$actual
all_results_shortterm$deterministic_fitted[52561:70080] <- testset_shortterm$short_det
all_results_shortterm$stoch[1:52560] <- STmodel_data$stoch
all_results_shortterm$stoch[52561:70080] <- testset_shortterm$stoch
all_results_shortterm$full_model <- all_results_shortterm$deterministic_fitted+ all_results_shortterm$stoch


write.csv(all_results_shortterm,"./data_for_R_code/all_results_shortterm.csv",row.names = F)


res <- all_results_shortterm$Consumption[52561:70080]- all_results_shortterm$deterministic_fitted[52561:70080]
sum(abs(res- all_results_shortterm$stoch[52561:70080]))/sum(abs(res))



###### Plot results ######## 
library(patchwork)
library(ggplot2)
# load local library for plotting
source('./ggplot_theme_Publication/ggplot_theme_Publication-2.R')

all_results_shortterm <- read.csv("./data_for_R_code/all_results_shortterm.csv") 
all_results_shortterm$index <- 1:nrow(all_results_shortterm)

### Deterministic plots ----

# September 2016
year=2016
month=9
sample=all_results_shortterm$index[all_results_shortterm$year==year & all_results_shortterm$month==month & all_results_shortterm$wday=='Mo']

weekstart=sample[1]
weekend  =weekstart + 24*7

STplot_sample_week_training <- ggplot(all_results_shortterm[weekstart:weekend,])+geom_line(aes(index,Consumption,color="actual"),lwd=1.3)+
  geom_line(aes(index,deterministic_fitted,color='fitted'),linetype=2,lwd=1.3)+
  xlab("\nSeptember week in 2016") +
  ylab("MW\n")+
  ggtitle('Short-term model deterministic prediction')+  
  theme_Publication()+ theme(legend.title = element_blank())+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=30))+
  scale_x_continuous(breaks=c(weekstart,weekstart+24,weekstart+48 ,weekstart+72,weekstart+96,weekstart+120,weekstart+144), 
                     labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))


STplot_sample_week_training


ggsave(file="./plots/STplot_trainingset_det.png", plot=STplot_sample_week_training, width=12, height=8)


# September 2019
year=2019
month=9
sample=all_results_shortterm$index[all_results_shortterm$year==year & all_results_shortterm$month==month & all_results_shortterm$wday=='Mo']

weekstart=sample[1]
weekend  =weekstart + 24*7

STplot_sample_week_test <- ggplot(all_results_shortterm[weekstart:weekend,])+geom_line(aes(index,Consumption,color="actual"),lwd=1.3)+
  geom_line(aes(index,deterministic_fitted,color='fitted'),linetype=2,lwd=1.3)+
  xlab("\nSeptember week in 2019") +
  ylab("MW\n")+
  ggtitle('Short-term model deterministic prediction')+  
  theme_Publication()+ theme(legend.title = element_blank())+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=30))+
  scale_x_continuous(breaks=c(weekstart,weekstart+24,weekstart+48 ,weekstart+72,weekstart+96,weekstart+120,weekstart+144), 
                     labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))


STplot_sample_week_test

ggsave(file="./plots/STplot_testset_det.png", plot=STplot_sample_week_test, width=12, height=8)


### Combined plots ----

# September 2016
year=2016
month=9
sample=all_results_shortterm$index[all_results_shortterm$year==year & all_results_shortterm$month==month & all_results_shortterm$wday=='Mo']

weekstart=sample[1]
weekend  =weekstart + 24*7

STplot_sample_week_training_full <- ggplot(all_results_shortterm[weekstart:weekend,])+geom_line(aes(index,Consumption,color="actual"),lwd=1.3)+
  geom_line(aes(index,full_model,color='fitted'),linetype=2,lwd=1.3)+
  xlab("\nSeptember week in 2016") +
  ylab("MW\n")+
  ggtitle('Short-term model training set prediction')+  
  theme_Publication()+ theme(legend.title = element_blank())+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=30))+
  scale_x_continuous(breaks=c(weekstart,weekstart+24,weekstart+48 ,weekstart+72,weekstart+96,weekstart+120,weekstart+144), 
                     labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))


STplot_sample_week_training_full

ggsave(file="./plots/STplot_trainingset_combined.png", plot=STplot_sample_week_training_full, width=12, height=8)


# September 2019
year=2019
month=9
sample=all_results_shortterm$index[all_results_shortterm$year==year & all_results_shortterm$month==month & all_results_shortterm$wday=='Mo']

weekstart=sample[1]
weekend  =weekstart + 24*7

STplot_sample_week_test_full <- ggplot(all_results_shortterm[weekstart:weekend,])+geom_line(aes(index,Consumption,color="actual"),lwd=1.3)+
  geom_line(aes(index,full_model,color='fitted'),linetype=2,lwd=1.3)+
  xlab("\nSeptember week in 2019") +
  ylab("MW\n")+
  ggtitle('Short-term model test set prediction')+  
  theme_Publication()+ theme(legend.title = element_blank())+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=30))+
  scale_x_continuous(breaks=c(weekstart,weekstart+24,weekstart+48 ,weekstart+72,weekstart+96,weekstart+120,weekstart+144), 
                     labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))


STplot_sample_week_test_full

ggsave(file="./plots/STplot_testset_full.png", plot=STplot_sample_week_test_full, width=12, height=8)









