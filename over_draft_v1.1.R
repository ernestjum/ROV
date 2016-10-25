###########################################################
###########################################################
## Retail Overdraft Model

###########################################################
###########################################################

## Author: Ernest Jum
## MRMD @ BB&T
## Date: 09/28/2016

###########################################################
###########################################################
## set working directory
setwd("/Users/ernestjum/Documents/overdraft")


###########################################################
###########################################################
## Required Libraries

install.packages("ggplot2")
install.packages("mgcv")
library(mgcv)
library(ggplot2)
library(gridExtra)
library(grid)
library(astsa)
library(kernlab)
library(neuralnet)
require(gdata)

###########################################################
###########################################################
## read in data set it is xlsx file: this was a tough one :)

mydata1<-read.xls ("overdraft_data.xlsx", sheet = 1 ,header = TRUE)

mydata1$data_date <-as.Date(mydata1$data_date, format = "%m/%d/%Y")

## select data from first quarter of 2006
mydata2<-with(mydata1, mydata1[mydata1$data_date >= "2006-01-31", ])
mydata22<-as.data.frame(mydata2[c("data_date", "Loss_Per_Account", "UR_s_Diff",
                              "UR_Lt_Diff", "CPI_RT", "QTR_3_FLG", "QTR_4_FLG")])

## Use this data for training the model Q1 2006--Q4 2015
mydata3<-with(mydata22, mydata22[mydata22$data_date <= "2015-12-31", ])

## Use this data for testing Q1 2016--Q2 2016
mydata_test<-with(mydata22, mydata22[mydata22$data_date > "2015-12-31", ])
mydata_test1<-mydata_test[c("UR_s_Diff", "UR_Lt_Diff", "CPI_RT", "QTR_3_FLG", "QTR_4_FLG")]


## developeres forecast
dev_fcst<-read.xls("forecast_from_dev.xlsx", sheet = 1, header=TRUE)
dev_fcst<-as.data.frame(dev_fcst)
dev_fcst$data_date <- as.Date(dev_fcst$data_date, format = "%m/%d/%Y")


dev_fcst_train<-dev_fcst[which(dev_fcst$data_date <= "2015-12-31"), ]
dev_fcst_test<-dev_fcst[which(dev_fcst$data_date > "2015-12-31"), ]


###########################################################
###########################################################
## Fit models
set.seed(12345)
## model with logit transformation
model_Lt<-lm(Loss_Per_Account ~ UR_Lt_Diff + CPI_RT+ QTR_3_FLG + QTR_4_FLG, data=mydata3)

## model with simple transformation
model_s<-lm(Loss_Per_Account ~ UR_s_Diff + CPI_RT+ QTR_3_FLG + QTR_4_FLG, data=mydata3)

## Fit benchmark model GAM Model 

## fit a GAM model
model_GAM<-gam(Loss_Per_Account ~ s(UR_Lt_Diff) + s(CPI_RT) +QTR_3_FLG + QTR_4_FLG,
                data=mydata3)

pdf("GAM_Overdraft.pdf")
par(mfrow=c(1,2))
plot(model_GAM, residuals = T, pch=19, cex=0.25,
     scheme=1, col= '#FF8000' , shade=T, shade.col='gray90')
dev.off()


## fit a support vector machine
model_svm<-ksvm(Loss_Per_Account ~ UR_Lt_Diff + CPI_RT+ QTR_3_FLG + QTR_4_FLG, 
                               epsilon=0.05, data=mydata3, kernel="vanilladot")

###########################################################
###########################################################
## Data for prediction
score_data_Lt<-as.data.frame(mydata3[c("UR_Lt_Diff", "CPI_RT", "QTR_3_FLG", "QTR_4_FLG")])

score_data_s<-as.data.frame(mydata3[c("UR_s_Diff", "CPI_RT", "QTR_3_FLG", "QTR_4_FLG")])

###########################################################
###########################################################
## Predicted values with logit tranformation
pred_Lt<-as.data.frame(predict(model_Lt, score_data_Lt))
colnames(pred_Lt)<-c("pred_loss_per_act_Lt")
pred<-pred_Lt$pred_loss_per_act_Lt
pred_Lt<-as.data.frame(as.matrix(pred_Lt))
colnames(pred_Lt)<-c("pred_loss_per_act_Lt")

## Predicted values for simple difference
pred_s<-as.data.frame(predict(model_s, score_data_s))
colnames(pred_s)<-c("pred_loss_per_act_s")
pred_s<-pred_s$pred_loss_per_act_s
pred_s<-as.data.frame(as.matrix(pred_s))
colnames(pred_s)<-c("pred_loss_per_act_s")


pred_GAM<-as.data.frame(model_GAM$fitted.values)
colnames(pred_GAM)<-c("pred_loss_per_act_GAM")


pred_svm<-predict(model_svm, mydata3)
pred_svm<-as.data.frame(pred_svm)
colnames(pred_svm)<-c("pred_loss_per_act_svm")

###########################################################
###########################################################
## use test data to predict losses per account

pred_Lt_test<-as.data.frame(predict(model_Lt, mydata_test))
colnames(pred_Lt_test)<-c("pred_loss_per_act_Lt_test")

pred_s_test<-as.data.frame(predict(model_s, mydata_test))
colnames(pred_s_test)<-c("pred_loss_per_act_s_test")

pred_GAM_test<-as.data.frame(predict(model_GAM, mydata_test))
colnames(pred_GAM_test)<-c("pred_loss_per_act_GAM_test")


pred_svm_test<-as.data.frame(predict(model_svm, mydata_test1))
colnames(pred_svm_test)<-c("pred_loss_per_act_svm_test")


pred_Act_test<-as.data.frame(dev_fcst_test$Loss_Per_Account)
colnames(pred_Act_test)<-c("pred_loss_per_act_ACt_test")



## combine predictions from test data set
pred_test<-cbind(pred_Act_test, pred_Lt_test, pred_s_test, pred_GAM_test, pred_svm_test)
colnames(pred_test)<-c("actual", "Lt_test", "s_test", "GAM_test", "svm_test")

###########################################################
###########################################################
## append date and actual values here

actual_loss_per_act<-as.data.frame(as.matrix(mydata3$Loss_Per_Account))
colnames(actual_loss_per_act)<-c("act_loss_per_act")

date<-as.data.frame(mydata3$data_date)
colnames(date)<-c('data_date')


###########################################################
###########################################################
## Dev forecast
dev_f<-as.data.frame(dev_fcst_train[c("Losshat", "Estimate_Loss", "Quarterly_Loss")])


###########################################################
###########################################################
## actual vs predicted
pred_and_act<-as.data.frame(cbind(date, actual_loss_per_act, pred_Lt, pred_GAM, pred_s, dev_f))
pred_and_act<-pred_and_act[ ,1:6]

###########################################################
###########################################################
dev_loss_qtr<-as.data.frame(dev_f$Losshat)
colnames(dev_loss_qtr)<-c("pred_loss_per_act_dev")

time_series<-as.data.frame(ts(cbind(pred_Lt, pred_s, pred_GAM, pred_svm, dev_loss_qtr, actual_loss_per_act)))
time_series<-cbind(date, time_series)


actual_loss<-as.data.frame(dev_f$Quarterly_Loss)
colnames(actual_loss)<-c("act_loss")

dev_loss<-as.data.frame(dev_f$Estimate_Loss)
colnames(dev_loss)<-c("pred_loss_dev")

time_series_loss<-as.data.frame(ts(cbind(pred_losses_Lt,pred_losses_s, pred_losses_GAM, dev_loss, actual_loss)))
time_series_loss<-cbind(date, time_series_loss)

write.csv(time_series, file = "Actual_and_Predicted_insample.csv")

###########################################################
###########################################################
 p<-ggplot() + 
   geom_line(data = time_series, aes(x = data_date, y = act_loss_per_act,      color = "actual losses")) + 
   geom_line(data = time_series, aes(x = data_date, y = pred_loss_per_act_Lt,  color = "predicted losses MRMD_Lt")) +
   geom_line(data = time_series, aes(x = data_date, y = pred_loss_per_act_s,   color = "predicted losses MRMD_s")) +
   geom_line(data = time_series, aes(x = data_date, y = pred_loss_per_act_dev, color = "predicted losses Dev_Lt.")) +
   geom_line(data = time_series, aes(x = data_date, y = pred_loss_per_act_GAM, color = "predicted losses GAM.")) +
   geom_line(data = time_series, aes(x = data_date, y = pred_loss_per_act_svm, color = "predicted losses SVM.")) +
   xlab('date') +
   ylab('loss per account')
 
 ggsave(p, file="insample_plots.pdf")

###########################################################
###########################################################

## calculate error measures like MAE MFE MSE RMS U_Statistic and percentage diffeerence
 
 ## error from losses per account
t1<-time_series
t1$error_Lt<-t1$act_loss_per_act-t1$pred_loss_per_act_Lt
t1$error_s<-t1$act_loss_per_act - t1$pred_loss_per_act_s
t1$error_GAM<-t1$act_loss_per_act - t1$pred_loss_per_act_GAM
t1$error_svm<-t1$act_loss_per_act - t1$pred_loss_per_act_svm
t1$error_dev<-t1$act_loss_per_act -t1$pred_loss_per_act_dev

#####################################################
#####################################################
## calculate lag of losses here used to calculate MASE
t<-as.vector(t1$act_loss_per_act)
z<-numeric(length(t))

z[1]<-0
for (j in 2: length(z))
  {
  
 z[j]<-t[j-1] 
}
 
diff<-t-z
diff[1]<-0

scale<-(1/(length(t)-1))*sum(abs(diff), na.rm = TRUE)


#####################################################
#####################################################

error_per_act<-t1[c("data_date" ,"error_Lt", "error_s", "error_GAM","error_svm","error_dev")]

write.csv(error_per_act, file="Error_Per_Account.csv")


## calculate the percentage difference
t1$error_Lt_pct<-((t1$act_loss_per_act-t1$pred_loss_per_act_Lt)/t1$act_loss_per_act)*100
t1$error_s_pct<-((t1$act_loss_per_act - t1$pred_loss_per_act_s)/t1$act_loss_per_act )*100
t1$error_GAM_pct<-((t1$act_loss_per_act - t1$pred_loss_per_act_GAM)/t1$act_loss_per_act )*100
t1$error_svm_pct<-((t1$act_loss_per_act - t1$pred_loss_per_act_svm)/t1$act_loss_per_act )*100
t1$error_dev_pct<-((t1$act_loss_per_act -t1$pred_loss_per_act_dev)/t1$act_loss_per_act)*100
percantage_error<-t1[c("data_date","error_Lt_pct", "error_s_pct","error_GAM_pct","error_svm_pct", "error_dev_pct")]

write.csv(percantage_error, file = "percenatge_error.csv")


#####################################################################
#####################################################################

### measures of errors on in-sample data performance

## Mean Forecast Error MFE
MFE_Lt=mean(error_per_act$error_Lt)
MFE_s=mean(error_per_act$error_s)
MFE_dev=mean(error_per_act$error_dev)
MFE_GAM<-mean(error_per_act$error_GAM)
MFE_svm<-mean(error_per_act$error_svm)

MFE<-cbind(MFE_Lt, MFE_s, MFE_dev, MFE_GAM, MFE_svm)


## Mean Absolute Error MAE
MAE_Lt=mean(abs(error_per_act$error_Lt))
MAE_s=mean(abs(error_per_act$error_s))
MAE_dev=mean(abs(error_per_act$error_dev))
MAE_GAM=mean(abs(error_per_act$error_GAM))
MAE_svm=mean(abs(error_per_act$error_svm))

MAE<-cbind(MAE_Lt, MAE_s, MAE_dev, MAE_GAM, MAE_svm)

## Mean Square Error
MSE_Lt<-mean((error_per_act$error_Lt)^2)
MSE_s<-mean((error_per_act$error_s)^2)
MSE_dev<-mean((error_per_act$error_dev)^2)
MSE_GAM<-mean((error_per_act$error_GAM)^2)
MSE_svm<-mean((error_per_act$error_svm)^2)

MSE<-cbind(MSE_Lt, MSE_s, MSE_dev, MSE_GAM, MSE_svm)

## Root Mean Square Error
RMSE_Lt<-sqrt(MSE_Lt)
RMSE_s<-sqrt(MSE_s)
RMSE_dev<-sqrt(MSE_dev)
RMSE_GAM<-sqrt(MSE_GAM)
RMSE_svm<-sqrt(MSE_svm)

RMSE<-cbind(RMSE_Lt, RMSE_s, RMSE_dev, RMSE_GAM, RMSE_svm)

## Signed Mean Squared Error
SMSE_Lt<-mean(t1$pred_loss_per_act_Lt*abs(t1$pred_loss_per_act_Lt))
SMSE_s<-mean(t1$pred_loss_per_act_s*abs(t1$pred_loss_per_act_s))
SMSE_dev<-mean(t1$pred_loss_per_act_dev*abs(t1$pred_loss_per_act_dev))
SMSE_GAM<-mean(t1$pred_loss_per_act_GAM*abs(t1$pred_loss_per_act_GAM))
SMSE_svm<-mean(t1$pred_loss_per_act_svm*abs(t1$pred_loss_per_act_svm))


SMSE<-cbind(SMSE_Lt, SMSE_s, SMSE_dev, SMSE_GAM, SMSE_svm)

## Mean Absolute Percentage Error: MAPE
MAPE_Lt<-mean(abs(t1$error_Lt/t1$act_loss_per_act))
MAPE_s<-mean(abs(t1$error_s/t1$act_loss_per_act))
MAPE_dev<-mean(abs(t1$error_dev/t1$act_loss_per_act))
MAPE_GAM<-mean(abs(t1$error_GAM/t1$act_loss_per_act))
MAPE_svm<-mean(abs(t1$error_svm/t1$act_loss_per_act))

MAPE<-cbind(MAPE_Lt, MAPE_s, MAPE_dev, MAPE_GAM, MAPE_svm)

## Theil's U statistic 
s_Lt<-sqrt(mean((t1$pred_loss_per_act_Lt)^2))
s_s<-sqrt(mean((t1$pred_loss_per_act_s)^2))
s_dev<-sqrt(mean((t1$pred_loss_per_act_dev)^2))
s_GAM<-sqrt(mean((t1$pred_loss_per_act_GAM)^2))
s_svm<-sqrt(mean((t1$pred_loss_per_act_svm)^2))
s_act<-sqrt(mean((t1$act_loss_per_act)^2))


U_Lt<-RMSE_Lt/(s_act*s_Lt)
U_s<-RMSE_s/(s_act*s_s)
U_dev<-RMSE_dev/(s_act*s_dev)
U_GAM<-RMSE_GAM/(s_act*s_GAM)
U_svm<-RMSE_svm/(s_act*s_svm)

U<-cbind(U_Lt, U_s, U_dev, U_GAM, U_svm)

###########################################################
###########################################################
## error measure on test data
t3<-as.data.frame(ts(pred_test))

t3$err_Lt_test<-t3$actual -t3$Lt_test
t3$err_s_test<-t3$actual -t3$s_test
t3$err_GAM_test<-t3$actual -t3$GAM_test
t3$err_svm_test<-t3$actual -t3$svm_test

MFE_test_Lt<-mean(t3$err_Lt_test)
MFE_test_s<-mean(t3$err_s_test)
MFE_test_GAM<-mean(t3$err_GAM_test)
MFE_test_svm<-mean(t3$err_svm_test)

MFE_test<-cbind(MFE_test_Lt, MFE_test_s,MFE_test_GAM, MFE_test_svm )

MAE_test_Lt<-mean(abs(t3$err_Lt_test))
MAE_test_s<-mean(abs(t3$err_s_test))
MAE_test_GAM<-mean(abs(t3$err_GAM_test))
MAE_test_svm<-mean(abs(t3$err_svm_test))

MAE_test<-cbind(MAE_test_Lt, MAE_test_s,MAE_test_GAM, MAE_test_svm )

MSE_test_Lt<-mean((t3$err_Lt_test)^2)
MSE_test_s<-mean((t3$err_s_test)^2)
MSE_test_GAM<-mean((t3$err_GAM_test)^2)
MSE_test_svm<-mean((t3$err_svm_test)^2)

MSE_test<-cbind(MSE_test_Lt, MSE_test_s,MSE_test_GAM, MSE_test_svm )

RMSE_test_Lt<-sqrt(mean((t3$err_Lt_test)^2))
RMSE_test_s<-sqrt(mean((t3$err_s_test)^2))
RMSE_test_GAM<-sqrt(mean((t3$err_GAM_test)^2))
RMSE_test_svm<-sqrt(mean((t3$err_svm_test)^2))

RMSE_test<-cbind(RMSE_test_Lt, RMSE_test_s,RMSE_test_GAM, RMSE_test_svm )

MAPE_test_Lt<-mean(abs(t3$err_Lt_test/t3$actual))
MAPE_test_s<-mean(abs(t3$err_s_test/t3$actual))
MAPE_test_GAM<-mean(abs(t3$err_GAM_test/t3$actual))
MAPE_test_svm<-mean(abs(t3$err_svm_test/t3$actual))

MAPE_test<-cbind(MAPE_test_Lt, MAPE_test_s, MAPE_test_GAM, MAPE_test_svm)

MASE_test_Lt<-MAE_test_Lt/scale
MASE_test_s<-MAE_test_s/scale
MASE_test_GAM<-MAE_test_GAM/scale
MASE_test_svm<-MAE_test_svm/scale

MASE_test<-cbind(MASE_test_Lt, MASE_test_s, MASE_test_GAM, MASE_test_svm)

## calculate Theils statistic
s_Lt1<-sqrt(mean((t3$Lt_test)^2))
s_s1<-sqrt(mean((t3$s_test)^2))
s_GAM1<-sqrt(mean((t3$GAM_test)^2))
s_svm1<-sqrt(mean((t3$svm_test)^2))
s_act1<-sqrt(mean((t3$actual)^2))


U_Lt1<-RMSE_test_Lt/(s_act1*s_Lt1)
U_s1<-RMSE_test_s/(s_act1*s_s1)
U_GAM1<-RMSE_test_GAM/(s_act1*s_GAM1)
U_svm1<-RMSE_test_svm/(s_act1*s_svm)

U_test<-cbind(U_Lt1, U_s1, U_GAM1,U_svm1)

###########################################################
##########################################################
## collect all test measure into a table here

Err_Measures_test<-as.data.frame(rbind(MFE_test, MAE_test, MSE_test, RMSE_test, 
                                          U_test, MAPE_test, MASE_test))
rownames(Err_Measures_test)<-c("MFE_test","MAE_test","MSE_test", "RMSE_test",
                                        "U_test","MAPE_test", "MASE_test")
colnames(Err_Measures_test)<-c("Lt_trans", "Simple_trans", "GAM_Lt_trans", "svm_Lt_trans")

Err_Measures_test<-round(Err_Measures_test, 3)

write.csv(Err_Measures_test, file = "Error_Matrix_test.csv")


###########################################################
###########################################################
## create a matrix of all error measures
Err_Measures<-as.data.frame(rbind(MFE, MAE, MSE,RMSE,  MAPE, SMSE, U ))
colnames(Err_Measures)<-c("Ref. Model(R)", "Orig. Model(simp. diff)",
                          "Orig Model(Logit trans.)", "Gen.Ad.Model", "SVM Model")
rownames(Err_Measures)<-c("MFE", "MAE", "MSE", "RMSE", "MAPE", "SMSE", "U")

Err_Measures<-as.data.frame(round(Err_Measures, 3))
write.csv(Err_Measures, file = "Error_Measures_insample.csv")


write.matrix(Err_Measures, file = "Err_Measure_insample.csv", sep="")

###########################################################
###########################################################

## Forecasting under different scenarios-- b1, b3 and s3
## Read in data for forecasting

F_B1<-read.xls ("F_B1.xlsx", sheet = 1 ,header = TRUE)
F_B3<-read.xls ("F_B3.xlsx", sheet = 1 ,header = TRUE)
F_S3<-read.xls ("F_S3.xlsx", sheet = 1 ,header = TRUE)

data_date<-as.data.frame(F_B1$DATA_DATE)
colnames(data_date)<-c("data_date")

F_B11<-F_B1[, 2:ncol(F_B1)]
F_B33<-F_B3[, 2:ncol(F_B3)]
F_S33<-F_S3[, 2:ncol(F_S3)]


## Do forecasts here 
fct_b1<-as.data.frame(predict(model_Lt, F_B11))
colnames(fct_b1)<-c("Loss_per_act_b1")

fct_b3<-as.data.frame(predict(model_Lt, F_B33))
colnames(fct_b3)<-c("Loss_per_act_b3")
                
fct_s3<-as.data.frame(predict(model_Lt, F_S33))
colnames(fct_s3)<-c("Loss_per_act_s3")

fct_GAM_b1<-as.data.frame(predict(model_GAM, F_B11))
colnames(fct_GAM_b1)<-c("Loss_per_act_GAM_b1")

fct_GAM_b3<-as.data.frame(predict(model_GAM, F_B33))
colnames(fct_GAM_b3)<-c("Loss_per_act_GAM_b3")

fct_GAM_s3<-as.data.frame(predict(model_GAM, F_S33))
colnames(fct_GAM_s3)<-c("Loss_per_act_GAM_s3")


fct_svm_b1<-as.data.frame(predict(model_svm, F_B11))
colnames(fct_svm_b1)<-c("Loss_per_act_svm_b1")

fct_svm_b3<-as.data.frame(predict(model_svm, F_B33))
colnames(fct_svm_b3)<-c("Loss_per_act_svm_b3")

fct_svm_s3<-as.data.frame(predict(model_svm, F_S33))
colnames(fct_svm_s3)<-c("Loss_per_act_svm_s3")

fct<-cbind(data_date, fct_b1, fct_b3, fct_s3, fct_GAM_b1, fct_GAM_b3, 
           fct_GAM_s3, fct_svm_b1, fct_svm_b3, fct_svm_s3)

## write output t a csv file 
write.csv(fct, file = "Forecast_Scenarios.csv")

###########################################################
###########################################################
