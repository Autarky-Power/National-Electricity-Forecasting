library(forecast)
library(MuMIn)
library(caret)
library(performance)



## set working directory and check if it is correct
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# load local library for plotting
source('./ggplot_theme_Publication/ggplot_theme_Publication-2.R')

## load macroeconomic variables and load data ----

LTmodel_data <- read.csv('./Data/LTmodel_starting_data.csv')

## make training and test set
LTmodel_training <- LTmodel_data[1:18,]
LTmodel_test <- LTmodel_data[19:20,]


## create global model with all macroeconomic indicators ----

variables=colnames(LTmodel_data[,2:19])[complete.cases(t(LTmodel_data[,2:19]))]

lm_formula <- as.formula(paste("consumption", paste(variables, collapse = " + "), 
        sep = " ~ "))

globalmodel <- lm(lm_formula, data= LTmodel_training,na.action="na.fail")

## create models with all combinations of all indicators
# will take 10-20 minutes
combinations <- dredge(globalmodel)

# save combinations to not have to calculate again
write.csv(combinations,"./Data/combinations_final.csv",row.names = F)

# load combinations if calculated
combinations <- read.csv("./Data/combinations_final.csv")


## k-fold cross validation ----

# set control function and random seed
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
rdm =123

#number of models to check
testquant = 1500 

# make empty dataframe for results and error metrics and fill it with random numbers
results <- data.frame()[1:testquant, ]
results$RMSE_k_fold <- 100000
results$Rsquare_k_fold <- 100000
results$MAE_k_fold <- 100000
results$index <- 1:nrow(results)

for (i in (1:testquant)){
  
  predictor_names=combinations[i,2:(ncol(combinations)-5)] 
  # last 5 columns contain information about the model but not the predictor names
  # therefore we don't include them
  
  # get model formula
  model_variables=colnames(predictor_names)[complete.cases(t(predictor_names))] 
  lm_formula <- as.formula(paste("consumption", paste(model_variables, collapse = " + "), 
          sep = " ~ "))
  
  set.seed(rdm) 
  
  Kfold <- train(lm_formula, data = LTmodel_data, 
                 trControl = ctrl, method = "lm")
  
  results$RMSE_k_fold[i] <- Kfold$results$RMSE
  results$Rsquare_k_fold[i] <- Kfold$results$Rsquared
  results$MAE_k_fold[i] <- Kfold$results$MAE
  
  # Print processed model number
  print(i)
}

## save results data frame to not have to compute again

write.csv(results,"./Data/kfold_cv_results.csv",row.names = F)

results <- read.csv("./Data/kfold_cv_results.csv")

## Find best models based on RMSE and MAE

best_index_RMSE = results$index[results["RMSE_k_fold"] == min(results["RMSE_k_fold"])]
best_value_RMSE = results[best_index_RMSE, "RMSE_k_fold"]
limit_RMSE = best_value_RMSE * 1.5 
paste("Lowest RMSE of",round(best_value_RMSE),"for model no.",best_index_RMSE,". RMSE limit is set to"
      ,round(limit_RMSE))

best_index_MAE = results$index[results["MAE_k_fold"] == min(results["MAE_k_fold"])]
best_value_MAE = results[best_index_MAE, "MAE_k_fold"]
limit_MAE = best_value_MAE * 1.5 
paste("Lowest MAE of",round(best_value_MAE),"for model no.",best_index_MAE,". MAE limit is set to"
      ,round(limit_MAE))

mask_RMSE = results["RMSE_k_fold"] <= limit_RMSE     
mask_MAE = results["MAE_k_fold"] <= limit_MAE     

# Rsquare value should be above 0.9

mask_Rsquare = results["Rsquare_k_fold"] > 0.9  

candidates = results[mask_Rsquare & mask_RMSE & mask_MAE,]
candidates$max_dist <- 10000
candidates$test_res_sum <- 10000

for (i in 1:nrow(candidates)){
  ind =candidates$index[i]
  x=combinations[ind,2:19]  
  variables=colnames(x)[complete.cases(t(x))]
  f <- as.formula(paste("consumption", paste(variables, collapse = " + "), 
        sep = " ~ "))

  model<- lm(f,data=LTmodel_training)
  LT <- predict(model,LTmodel_data[0:20,], interval = "confidence")

  res <- LTmodel_data$consumption - LT[0:20]
  candidates$test_res_sum[i] <- sum(abs(res[c(19,20)]))
  candidates$max_dist[i] <- max(abs(res))
  print(i)
}

candidates$index[candidates$max_dist==min(candidates$max_dist)]
print(paste("Lowest max distance is", round(min(candidates$max_dist))))
candidates$index[candidates$test_res_sum==min(candidates$test_res_sum)]
print(paste("Lowest residual sum over test period is", round(min(candidates$test_res_sum))))


## Filter best candidates and test model for normality of residuals, homogeneity, correlation, ... ----

#  if no suitable model is found relax the limits of max_distance and test_res_sum.

best_candidates <- candidates[candidates$max_dist < (round(min(candidates$max_dist))*1.5)&
                                candidates$test_res_sum<  (round(min(candidates$test_res_sum))*1.5) ,]

print('Possible candidates are model no:')
print(best_candidates$index)
cat("Possible candidates are models no:", best_candidates$index, "\n")

## Check possible candidates manually
# Model number 350 is considered the best and used in the publication

model_no=350

ind=combinations[model_no,2:19]  
variables=colnames(ind)[complete.cases(t(ind))]
f <- as.formula(paste("consumption", paste(variables, collapse = " + "), 
                      sep = " ~ "))

model<- lm(f,data=LTmodel_training)
LT <- predict(model,LTmodel_data[0:20,], interval = "confidence")


# Plot model ---- 

LTmodel_data$fitted <- LT[0:20] 
LTmodel_data$lwr <- LT[,2]
LTmodel_data$upr <- LT[,3]
LTmodel_data$res<- LTmodel_data$consumption-LTmodel_data$fitted


LTplot_overview <- ggplot(LTmodel_data)+geom_line(aes(year,consumption,color="actual"),lwd=1.3)+
  geom_line(aes(year,fitted,color='fitted'),linetype=2,lwd=1.3)+
  geom_vline(xintercept=2018,linetype = 2)+
  geom_ribbon(aes(x=year,ymin=lwr,ymax=upr),alpha=0.25)+
  xlab("\nYear") + ylab("Yearly average load in MW/h\n")+
  ggtitle('Long-term model results')+  
  theme_Publication()+ theme(legend.title = element_blank())+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=34))

LTplot_overview


# check model

model_sanity_check<-check_model(model,panel=FALSE)
print(model_sanity_check)


# save model
save(model,file = "./Models/longterm/LTmodel_final.Rdata")


### Residual Plots ----


LTres1<-ggplot(LTmodel_data[1:18,]) + geom_line(aes(x = year, y =res ),lwd=1.3)+
  ggtitle(label = "Residual Plots\n") +theme_Publication()+
  ylab("MW")+xlab("Year")+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=34))

LTres1


LTres2<-ggAcf(LTmodel_data$res[1:18], lag.max = 6)+ theme_Publication() +ggtitle(element_blank())+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=34))

LTres2

LTres3<-ggplot(LTmodel_data[1:18,], aes(x = res))+geom_histogram(aes(y = ..density..),binwidth = 100) +
  geom_density(color="red",lwd=1.3)+
  theme_Publication()+
  ylab("Density")+
  theme(axis.title=element_text(size=26))+
  theme(legend.text=element_text(size=26))+
  theme(axis.text=element_text(size=23))+
  theme(plot.title = element_text(size=34))
LTres3


## Arrange residual plots

LTresiduals<-LTres1 /(LTres2+LTres3)
LTresiduals



# Save plots and data ----
ggsave(file="./Plots/LTplot_overview_final.png", plot=LTplot_overview, width=12, height=8)
ggsave(file="./Plots/LTplot_residuals_final.png", plot=LTresiduals, width=12, height=8)

write.csv(LTmodel_data,"./data/longterm_model_results.csv",row.names = F)













