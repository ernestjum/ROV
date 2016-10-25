###########################################################
###########################################################
## Retail Overdraft Model

###########################################################
###########################################################

## Author: Ernest Jum
## MRMD @ BB&T
## Date: 10/09/2016

###########################################################
###########################################################
## set working directory
setwd("/Users/ernestjum/Documents/overdraft")


#########################################################
########################################################
## required packages

# install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
library(grid)
library(corrplot)
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


###########################################################
###########################################################
mydata4<-mydata3[c("data_date","UR_s_Diff","UR_Lt_Diff", "CPI_RT", "Loss_Per_Account", 
                   "QTR_3_FLG", "QTR_4_FLG")]

## check data types
str(mydata4)

##summary of numerical data
summary(mydata4[c("UR_Lt_Diff","CPI_RT", "Loss_Per_Account" )])

## ranges of numerical variables
R_UR<-range(mydata4$UR_Lt_Diff)
R_CPI<-range(mydata4$CPI_RT)
R_LpAct<-range(mydata4$Loss_Per_Account)

Rg<-as.data.frame(cbind(R_UR, R_CPI, R_LpAct))
rownames(Rg)<-c("Max", "Min")

table(mydata4$QTR_4_FLG)
table(mydata4$QTR_3_FLG)

model_table_Q4 <-table(mydata4$QTR_4_FLG)
model_table_Q3<-table(mydata4$QTR_3_FLG)

prop.table(model_table_Q4)
prop.table(model_table_Q3)

#########################################################
########################################################

p<-ggplot(mydata4, aes(UR_Lt_Diff, Loss_Per_Account)) + geom_point() + geom_smooth(method=lm, se=FALSE)
p11<-ggplot(mydata4, aes(UR_Lt_Diff, Loss_Per_Account)) + geom_point() + geom_smooth(se=FALSE)
grid.arrange(p, p11)


r<-ggplot(mydata4, aes(CPI_RT, Loss_Per_Account)) + geom_point() + geom_smooth(method=lm, se=FALSE)
r11<-ggplot(mydata4, aes(CPI_RT, Loss_Per_Account)) + geom_point()  + geom_smooth(se=FALSE)
grid.arrange(r, r11)

## check correlation between variables

mydata5<-mydata4[c("UR_Lt_Diff", "UR_s_Diff", "CPI_RT", "Loss_Per_Account")]
colnames(mydata5)<-c("Logit Diff. UR", "Simp. Diff. UR", "CPI", "Loss Per Act.")
M<-cor(mydata5)

## format the corr matrix with color intensities
corrplot(M, method="number")
corrplot.mixed(M, lower="color", upper = "number")

#########################################################
########################################################
## do the box plots

boxplot(mydata4$UR_Lt_Diff, main="Boxplot of Unemployment Rate", ylab="UR_Lt")
boxplot(mydata4$CPI_RT, main="Boxplot of CPI", ylab="CPI_RT")
boxplot(mydata4$Loss_Per_Account, main="Boxplot of Loss Per Account", ylab="Loss_Per_Account")

#########################################################
########################################################

hist(mydata4$UR_Lt_Diff, main = "Histogram of Unemployment Rtate", xlab = "UR_Lt")
hist(mydata4$CPI_RT, main="Histogram of CPI", xlab="CPI_RT")
hist(mydata4$Loss_Per_Account, main="Histogram of Loss Per Account", xlab="Loss_Per_Account")

#########################################################
########################################################
## 

png("plot6.png", width=1040, height=480)
g <- ggplot(mydata4, aes(UR_Lt_Diff, Loss_Per_Account ))
g <- g + facet_grid(. ~ QTR_4_FLG)
g <- g + geom_point()  
g <- g + geom_smooth(method=lm, se=FALSE)+
  xlab("QTR_Lt") +
  ylab("Loss Per Account") +
  ggtitle("Histogram")
print(g)
dev.off()


#########################################################
########################################################
##  






