library(InformativeCensoring)
set.seed(421234)
setwd("~/zhangyi/InformativeCensoring")
impute1<-read.csv("impute",sep="\t")
head(impute1)
impute1$Z1[is.na(impute1$Z1)]<-mean(impute1$Z1,na.rm = T)#如果是0,取平均值
impute1$arm <- factor(impute1$arm)#結果是0或者是1
impute1$time <- impute1$time*12#時間成爲月
col.control <- col.headings(has.event="event", time="time",Id="Id",arm="arm",
    DCO.time="DCO.time", to.impute="to.impute")#輸入數據
imputed.data.sets <- ScoreImpute(data=impute1,event.model=~Z1+Z2,
                                 col.control=col.control, m=10,
                                 bootstrap.strata=impute1$arm,
                                 NN.control=NN.options(NN=5,w.censoring = 0.2))
#上面model是Z1+Z2
#for the third data set
imputed.data.set <- ExtractSingle(imputed.data.sets,index=3)
#We use the ExtractSingle function to extract out a single imputed data set. The index 
#argument is an integer between 1 and m allowing the user to specify which
#imputed data set is to be extracted:
head(imputed.data.set$data)
#The method argument must be one of ‘logrank’, ‘Wilcoxon’ 5 or ‘Cox’.
Cox.fits <- ImputeStat(imputed.data.sets,method="Cox",
                       formula=~arm+Z1+Z2)
ExtractSingle(Cox.fits,index=3)$model

library(survival)
#Any additional arguments are passed to the Cox model fit function (survival::coxph). Note, the subset
#and na.action arguments cannot be used (na.fail is used)
cox2 <- coxph(Surv(impute.time, impute.event) ~ arm+Z1+Z2, data = imputed.data.set$data)
summary(cox2)
