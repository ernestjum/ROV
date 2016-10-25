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
## read in data set it is xlsx file: this was a tough one :)
require(gdata)
mydata1<-read.xls ("overdraft_data.xlsx", sheet = 1 ,header = TRUE)

mydata1$data_date <-as.Date(mydata1$data_date, format = "%m/%d/%Y")

mydata2<-with(mydata1, mydata1[mydata1$data_date >= "2006-01-31", ])


mydata3<-as.data.frame(mydata2[c("data_date", "Loss_Per_Account", "UR_s_Diff",
                                 "UR_Lt_Diff", "CPI_RT", "QTR_3_FLG", "QTR_4_FLG")])

## macro data
macro_data<-read.xls("macro_vars.xlsx", sheet = 1, header=TRUE)

## developeres forecast
dev_forecast<-read.xls("forecast_from_dev.xlsx", sheet = 1, header=TRUE)

###########################################################
###########################################################
mydata4<-mydata3[c("UR_s_Diff","UR_Lt_Diff", "CPI_RT", "Loss_Per_Account", 
                   "QTR_3_FLG", "QTR_4_FLG")]


###########################################################
###########################################################
## Fit benchmark model
install.packages("mgcv")
library(mgcv)

## model
model_GAM<-gam(Loss_Per_Account ~ s(UR_Lt_Diff) + s(CPI_RT) +QTR_3_FLG + QTR_4_FLG,
               family=gaussian(link=identity), data=mydata3)

sp<-model_GAM$sp

 tuning.scale<-c(1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5, 1e6)
###tuning.scale<-c(0.00001,0.0001,0.001,0.01,0.1,1,10,100,1000,10000,100000)
scale.exponent<-log10(tuning.scale)
n.tuning<-length(tuning.scale)
edf<-rep(NA,n.tuning)
min2ll<-rep(NA,n.tuning)
aic<-rep(NA,n.tuning)
bic<-rep(NA,n.tuning)

 for (i in 1:n.tuning) 
   {
  model_GAM<-gam(Loss_Per_Account ~ s(UR_Lt_Diff) + s(CPI_RT) +QTR_3_FLG + QTR_4_FLG,
                  family=gaussian(link=identity), data=mydata3,sp=tuning.scale[i]*sp)
  min2ll[i]<--2*logLik(model_GAM)
  edf[i]<-sum(model_GAM$edf)+1
  aic[i]<-AIC(model_GAM)
  bic[i]<-BIC(model_GAM)
   }


pdf("AICBICoverdraft.pdf")
  par(mfrow=c(2,2))
  plot(scale.exponent,min2ll,type="b",main="-2 log likelihood")
  plot(scale.exponent,edf,ylim=c(0,70),type="b",main="effective number")
  plot(scale.exponent,aic,type="b",main="AIC")
  plot(scale.exponent,bic,type="b",main="BIC")
  dev.off()

  ## scaling corresponding to min bic
  min.bic<-1e1000
  opt.tuning1.scale<-NULL
   for (i in 1:n.tuning) {
    if (bic[i]<min.bic) {
       min.bic<-bic[i]
      opt.tuning1.scale<-tuning.scale[i]
      }
   }
  
  
  ## scaling corresponding to min aic
  min.aic<-1e1000
  opt.tuning2.scale<-NULL
  for (i in 1:n.tuning) {
    if (aic[i]<min.aic) {
      min.aic<-aic[i]
      opt.tuning2.scale<-tuning.scale[i]
    }
  }
  

  opt.sp<-opt.tuning2.scale*sp
 

  model_GAM<-gam(Loss_Per_Account ~ s(UR_Lt_Diff) + s(CPI_RT) +QTR_3_FLG + QTR_4_FLG,
                 family=gaussian(link=identity), data=mydata3, sp=opt.sp) 
  
  
pred_GAM<-as.data.frame(model_GAM$fitted.values)
colnames(pred_GAM)<-c("loss_per_act_GAM")

