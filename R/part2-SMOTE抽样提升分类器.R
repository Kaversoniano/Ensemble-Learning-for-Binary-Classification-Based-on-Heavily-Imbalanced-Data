#SMOTE
library(unbalanced)
data=read.csv("bank_full_fs.csv")
library(caret)
cfinal=22
data[,cfinal]=as.factor(data[,cfinal])
set.seed(1234)
folds<-createFolds(y=data[,cfinal],k=10) #根据data的laber-Species把数据集随机切分成10等份
smdata=vector("list",10)
for(i in 1:10)
{
  traindata<-data[-folds[[i]],]
  t=which(traindata[,cfinal]==1)
  trainnew=ubSmoteExs(data=traindata[t,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍
  smdata[[i]]=rbind(traindata,trainnew)
}
#随机森林  
# ytest
# pre1      0      1
# 0 3748.8  203.8
# 1  243.4  325.1
# precisions: 0.5718558 recall: 0.614672 Fscore: 0.6008302
library(randomForest)
p1=matrix(0,2,2);pred1=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
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
#ID3决策树   
# ytest
# pre2      0      1
# 0 3569.3  189.6
# 1  422.9  339.3
# precisions: 0.4451588 recall: 0.6415201 Fscore: 0.5648555 
library(rpart)
p2=matrix(0,2,2);pred2=NULL;ytestall=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
  testdata<-data[folds[[i]],];xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
  model2=rpart(y~.,data=traindata)
  pre2=predict(model2,xtest)
  pre2=ifelse(pre2[,1]>0.5,0,1);pred2=c(pred2,pre2)
  p=table(pre2,ytest);p2=p2+p;tabl=p2/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
  ytestall=c(ytestall,ytest)
}  
#C50决策树
# ytest
# pre3      0      1
# 0 3658.1  175.8
# 1  334.1  353.1
# precisions: 0.5138242 recall: 0.667612 Fscore: 0.6113146 
data[,cfinal]=as.factor(data[,cfinal])
library(C50) 
p3=matrix(0,2,2);pred3=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
  testdata<-data[folds[[i]],];xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
  model3=C5.0(xtrain, ytrain)
  pre3=predict(model3,xtest);pred3=c(pred3,pre3)
  p=table(pre3,ytest);p3=p3+p;tabl=p3/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 
##　devtools::install_github('dmlc/xgboost',subdir='R-package')
#xgboost算法    
# ytest
# pre4      0      1
# 0 3226.5   56.3
# 1  765.7  472.6
# precisions: 0.3816523 recall: 0.8935527 Fscore: 0.6325142 
cfinal=22
require(xgboost)
p4=matrix(0,2,2);pred4=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
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
#KNN分类器  
# ytest
# pre5      0      1
# 0 3662.6  216.7
# 1  329.6  312.2
# precisions: 0.4864444 recall: 0.5902817 Fscore: 0.5539012 
library(class);library(gmodels)
p5=matrix(0,2,2);pred5=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  pre5<-knn(train=scale(xtrain),test=scale(xtest),cl=ytrain,k=round(sqrt(nrow(xtrain))));pred5=c(pred5,pre5)
  p=table(pre5,ytest);p5=p5+p;tabl=p5/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 

#LDA判别分类    
# ytest
# pre6      0      1
# 0 3755.8  248.5
# 1  236.4  280.4
# precisions: 0.5425697 recall: 0.5301569 Fscore: 0.5339153
library(MASS)  
p6=matrix(0,2,2);pred6=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  ff1=lda(y~.,traindata) 
  pre6=predict(ff1,as.data.frame(xtest))$class;pred6=c(pred6,pre6)
  p=table(pre6,ytest);p6=p6+p;tabl=p6/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 

#朴素贝叶斯分类器   
# ytest
# pre7      0      1
# 0 3567.2  256.0
# 1  425.0  272.9
# precisions: 0.3910302 recall: 0.5159766  Fscore: 0.4697883 
library(e1071)
p7=matrix(0,2,2);pred7=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  efit <- naiveBayes(y~.,traindata)  
  pre7 <- predict(efit,xtest);pred7=c(pred7,pre7)
  p=table(pre7,ytest);p7=p7+p;tabl=p7/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 

#logistic回归分类      
# ytest
# pre8      0      1
# 0 3707.8  221.8
# 1  284.4  307.1
# precisions: 0.5191885 recall: 0.5806391 Fscore: 0.5602363 
p8=matrix(0,2,2);pred8=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
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

### BP 神经网络   
# ytest
# pre9      0      1
# 0 3509.9  158.3
# 1  482.3  370.6
# precisions: 0.4345175 recall: 0.7006996 Fscore: 0.5895713
library(nnet)
p9=matrix(0,2,2);pred9=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  testdata<-data[folds[[i]],];xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
  fitBP <- nnet(y~., data = traindata, size = 5, rang = 0.1,decay = 5e-4, maxit = 100);
  pre9<- predict(fitBP, xtest, type = "class");pred9=c(pred9,pre9)
  p=table(pre9,ytest);p9=p9+p;tabl=p9/10
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  if(i==10) print(tabl);
  cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
} 

#神经网络 多层感知器mlp   
# predictions
# predictions
# targets      1      2
# 0 3187.6   74.4
# 1  804.6  454.5
# precisions: 0.3609721 recall: 0.8593307 Fscore: 0.6031236 
library(RSNNS)
p10=matrix(0,2,2);pred10=NULL
for(i in 1:10){  
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
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

#multi-SVM    
# ytest
# pre10    0    1
# 0 3135.1   129
# 1  857.1  399.9
#precisions: 0.3181457 recall: 0.7561437 Fscore: 0.5311465
library(e1071)
p11=matrix(0,2,2);pred11=NULL
for(i in 1:10)
{
  traindatatotal<-smdata[[i]];
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
#16年12月发布的微软开源包-LightGBM   
# ytest
# pre12      0      1
# 0 3726.8  204.4
# 1  265.4  324.5
# precisions: 0.5500932 recall: 0.6135375 Fscore: 0.5925109 
#library(devtools)
#find_rtools()
#devtools::install_github("Microsoft/LightGBM", subdir = "R-package")
library(lightgbm)
p12=matrix(0,2,2);pred12=NULL
for(i in 1:10)
{
  traindata<-smdata[[i]]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
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


