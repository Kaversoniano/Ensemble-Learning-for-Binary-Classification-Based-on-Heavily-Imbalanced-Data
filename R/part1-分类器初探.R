#分类器的初始表现
library(DMwR)
#将样本的属性设置为数值型，这样能保证读入的data是符合xgboost的num类型而不是int类型
data=read.csv("bank_full_fs.csv")
library(caret)
library(e1071)
cfinal=22
data[,cfinal]=as.factor(data[,cfinal])
#因为folds是随机切割，因此设置一个随机种子，可以还原数据分析过程
set.seed(1234)
folds<-createFolds(y=data[,cfinal],k=10) #根据data的laber-Species把数据集随机切分成10等份
#10-fold cross-validation  
#就是十折交叉验证，用来测试精度，是常用的精度测试方法。  
#将数据集分成十分，轮流将其中9份做训练1份做测试，10次的结果的均值作为对算法精度的估计
#随机森林  630.765107
# ytest
# pre1      0      1
# 0 3896.4  336.6
# 1   95.8  192.3
# precisions: 0.6674766 recall: 0.3635848 Fscore: 0.4228161 
time1=Sys.time()
timecost=list()
library(randomForest)
p1=matrix(0,2,2);pred1=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];
  #traindata<-SMOTE(违约~.,traindata,perc.over=100,perc.under=150); 
  xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
  testdata<-data[folds[[i]],];xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
  model1=randomForest(xtrain,ytrain)
  pre1=predict(model1,xtest);pred1=c(pred1,pre1)
  p=table(pre1,ytest);p1=p1+p;tabl=p1/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
}  
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#ID3决策树   25.615465
# ytest
# pre2     no    yes
# 0 3884.8  343.1
# 1  107.4  185.8
# precisions: 0.6336971 recall: 0.3512951 Fscore: 0.4071196 
library(rpart)
p2=matrix(0,2,2);pred2=NULL;ytestall=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
  testdata<-data[folds[[i]],];xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
  model2=rpart(y~.,data=traindata)
  #model2=rpart(y~.,data=traindata,parms = list(prior = c(.8,.2), split = "information"))
  pre2=predict(model2,xtest)
  pre2=ifelse(pre2[,1]>0.5,0,1);pred2=c(pred2,pre2)
  p=table(pre2,ytest);p2=p2+p;tabl=p2/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
  ytestall=c(ytestall,ytest)
}  
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#C50 决策树    30.37795
# ytest
# pre3      0      1
# 0 3819.7  269.2
# 1  172.5  259.7
# precisions: 0.6008792 recall: 0.4910191 Fscore: 0.5202885
data[,cfinal]=as.factor(data[,cfinal])
library(C50) 
p3=matrix(0,2,2);pred3=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
  testdata<-data[folds[[i]],];xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
  model3=C5.0(xtrain, ytrain)
  pre3=predict(model3,xtest);pred3=c(pred3,pre3)
  p=table(pre3,ytest);p3=p3+p;tabl=p3/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
##　devtools::install_github('dmlc/xgboost',subdir='R-package')
#xgboost算法    15.641895
# ytest
# pre4      0      1
# 0 3544.0  113.3
# 1  448.2  415.6
# precisions: 0.4811299 recall: 0.7857818 Fscore: 0.657651
cfinal=22
require(xgboost)
p4=matrix(0,2,2);pred4=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  xgmat <- xgb.DMatrix(xtrain,label=ytrain)
  bst <- xgboost(data = xgmat, max.depth = 3, eta = 0.1,
                                nround = 100, objective = "binary:logistic",nthread=16,#min_child_weight=NULL,
                                max_delta_step=100,subsample=1,eval_metric="logloss",verbose=0)
  pre4<- predict(bst,xtest)
  pre4=ifelse(pre4>0.2,1,0);pred4=c(pred4,pre4)
  p=table(pre4,ytest);p4=p4+p;tabl=p4/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#KNN分类器     94.60944
# ytest
# pre5      0      1
# 0 3927.7  409.3
# 1   64.5  119.6
# precisions: 0.6496469 recall: 0.2261297 Fscore: 0.2828709 
library(class);library(gmodels)
p5=matrix(0,2,2);pred5=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  pre5<-knn(train=scale(xtrain),test=scale(xtest),cl=ytrain,k=round(sqrt(nrow(xtrain))));pred5=c(pred5,pre5)
  p=table(pre5,ytest);p5=p5+p;tabl=p5/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#LDA判别分类    4.340248
# ytest
# pre6      0      1
# 0 3861.8  326.2
# 1  130.4  202.7
# precisions: 0.608526 recall: 0.3832483 Fscore: 0.4325154 
library(MASS)  
p6=matrix(0,2,2);pred6=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  ff1=lda(y~.,traindata) 
  pre6=predict(ff1,as.data.frame(xtest))$class;pred6=c(pred6,pre6)
  p=table(pre6,ytest);p6=p6+p;tabl=p6/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#朴素贝叶斯分类器   27.008545
# ytest
# pre7     0     1
# 0 3648.2  292.7
# 1  344  236.2
# precisions: 0.407101 recall: 0.4465873 Fscore: 0.4336454
library(e1071)
p7=matrix(0,2,2);pred7=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  efit <- naiveBayes(y~.,traindata)  
  pre7 <- predict(efit,xtest);pred7=c(pred7,pre7)
  p=table(pre7,ytest);p7=p7+p;tabl=p7/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#logistic回归分类      6.942397
# ytest
# pre8     0     1
# 0 3898.1  356.8
# 1   94.1  172.1
# precisions: 0.6465064 recall: 0.3253923 Fscore: 0.3840924 
p8=matrix(0,2,2);pred8=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  anes1=glm(y~.,family=binomial(link='logit'),data=traindata)
  pre8=predict(anes1,testdata[,-cfinal]);pre8=exp(pre8)/(1+exp(pre8))#计算因变量的值
  pre8=ifelse(pre8>0.5,1,0);pred8=c(pred8,pre8)
  p=table(pre8,ytest);
  if(nrow(p)==1)
    p8[1,]=p8[1,]+p
  else p8=p8+p
  tabl=p8/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()

### BP 神经网络    133.756734
# ytest
# pre9      0      1
# 0 3861.7  330.9
# 1  130.5  198.0
# precisions: 0.6027397 recall: 0.3743619 Fscore: 0.4237665
library(nnet)
p9=matrix(0,2,2);pred9=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  fitBP <- nnet(y~., data = traindata, size = 10, rang = 0.1,decay = 5e-4, maxit = 200);
  pre9<- predict(fitBP, xtest, type = "class");pred9=c(pred9,pre9)
  p=table(pre9,ytest);p9=p9+p;tabl=p9/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#神经网络 多层感知器mlp    83.71578
# predictions
# targets      1      2
# 0 3862.6  320.5
# 1  129.6  208.4
# precisions: 0.616568 recall: 0.3940253 Fscore: 0.4432519
library(RSNNS)
p10=matrix(0,2,2);pred10=NULL
for(i in 1:10){  
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  xtrain=scale(xtrain);xtest=scale(xtest)
  ytrain=decodeClassLabels(ytrain)
  ytest=decodeClassLabels(ytest)
  model <- mlp(xtrain, ytrain, size=5, learnFuncParams=c(0.1), 
               maxit=50, inputsTest=xtest, targetsTest=ytest)
  pre10 = predict(model,xtest)[,2];pre10=ifelse(pre10>0.5,1,0);pred10=c(pred10,pre10)
  p=confusionMatrix(pre10,ytest);p10=p10+p;tabl=p10/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#SVM    6467.24368
# ytest
# pre10    0    1
# 0 3135.1   129
# 1  857.1  399.9
#precisions: 0.3181457 recall: 0.7561437 Fscore: 0.5311465
library(e1071)
p11=matrix(0,2,2);pred11=NULL
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  ytrain=as.factor(ytrain)
  model=svm(xtrain, ytrain, kernel = "radial",  degree = 3, gamma =1/ncol(xtrain),class.weights=20/table(ytrain)) #  
  pre11=predict(model,xtest);pred11=c(pred11,pre11)
  p=table(pre11,ytest);p11=p11+p;tabl=p11
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
}
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#16年12月发布的微软开源包-LightGBM   10.8
# ytest
# pre12      0      1
# 0 3830.8  277.8
# 1  161.4  251.1
# precisions: 0.6087273 recall: 0.4747589 Fscore: 0.5092432 
#library(devtools)
#find_rtools()
#devtools::install_github("Microsoft/LightGBM", subdir = "R-package")
library(lightgbm)
p12=matrix(0,2,2);pred12=NULL
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  dtrain <- lgb.Dataset(xtrain,label = ytrain, free_raw_data=T)
  dtest <- lgb.Dataset.create.valid(dtrain,xtest,label = ytest)
  params <- list(objective = "multiclass", metric = "multi_logloss")
  valids <- list(test=dtest)
  bst <- lgb.train(params,dtrain,nrounds = 500,valids,num_threads = 4,num_class = 2,
                   verbose = 0,record = T,early_stopping_rounds = 5,
                   min_data = 10,learning_rate = 1)
  pre12<- predict(bst, xtest, reshape=T)[,2];pred12=c(pred12,pre12)
  pre12=ifelse(pre12>=0.5,1,0)
  p=table(pre12,ytest);p12=p12+p;tabl=p12/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
}
time2=Sys.time()
timecost=list(timecost,time2-time1)
time1=Sys.time()
#multiSVM 本文改进的SVM算法框架-Time difference of 6.982366 mins
#precisions: 0.316751 recall: 0.757232 Fscore: 0.5303177  
library(e1071)
p11=matrix(0,2,2);pred11=NULL
for(i in 1:10)
{
  traindatatotal<-data[-folds[[i]],];
  yin0=which(traindatatotal[,cfinal]==0);yin1=which(traindatatotal[,cfinal]==1)
  validdata<-data[folds[[i]],];xvalid=as.matrix(validdata[,-cfinal,drop=F]);yvalid=as.matrix(validdata[,cfinal,drop=F])
  preofall=rep(0,length(yvalid))
  for(j in 1:20)
  {
    trin0=sample(yin0,length(yin0)/20);trin1=sample(yin1,length(yin1)/20)
    traindata=traindatatotal[c(trin0,trin1),]
    xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
    ytrain=as.factor(ytrain)
    modelone=svm(xtrain, ytrain, kernel = "radial",  degree = 3, gamma =1/ncol(xtrain),class.weights=20/table(ytrain)) 
    preofone=predict(modelone,xvalid);
    preofall=preofall+(as.numeric(preofone)-1);
  }
  pre11=preofall/20;pred11=c(pred11,pre11)
  p=table(ifelse(pre11>=0.5,1,0),yvalid);p11=p11+p;tabl=p11/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
}
time2=Sys.time()
time2-time1
