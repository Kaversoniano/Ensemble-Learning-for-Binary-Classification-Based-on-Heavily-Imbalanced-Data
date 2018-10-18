#SMOTE,NCL,Tomek,Kmeans(SBC)联合抽样多种组合
alldata=read.csv("bank_full_fs.csv")
library(caret)
library(unbalanced)
cfinal=22
alldata[,cfinal]=as.factor(alldata[,cfinal])
set.seed(1234)  
fo<-createFolds(y=alldata[,cfinal],k=10) #把原始数据集随机切分成10等份
testdata=alldata[fo[[10]],]   #把随机的最后一份数据定为为test
data=alldata[-fo[[10]],]      #其他数据用来训练模型
set.seed(2345)  
folds<-createFolds(y=data[,cfinal],k=10) #训练的数据集随机切分成10等份
SNTKdata=vector("list", 10)  #每个字母代表一种抽样方法，多个字母代表联合抽样方法
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];
  yin1=which(traindata[,cfinal]=="1")
  trainnew=ubSmoteExs(data=traindata[yin1,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍，SMOTE方法
  Sdata=rbind(traindata,trainnew)
  tkdata<-ubTomek(X=Sdata[,-cfinal], Y= Sdata[,cfinal])   #按照TK规则，删除多数类
  STdata<-cbind(tkdata$X, y=tkdata$Y)
  ncldata<-ubNCL(X=STdata[,-cfinal], Y= STdata[,cfinal])  #按照NCL规则，删除多数类
  sntdata<-cbind(ncldata$X, y=ncldata$Y)
  yin0=which(sntdata[,cfinal]=="0");lenthof1=nrow(sntdata)-length(yin0)
  kmdata=sntdata[yin0,];
  km=kmeans(scale(kmdata[,-cfinal]),110,iter.max = 100) #按照SBC规则（聚类采用kmeans），删除多数类
  km2data=cbind(kmdata,class=as.numeric(km$cluster))
  deletetotal=NULL
  for(j in 1:110)
  {
    t=which(km2data$class==j)
    deleteratio=(length(yin0)-lenthof1)/length(yin0)
    deletenum=round(length(t)*deleteratio)
    deletetotal=c(deletetotal,sample(t,deletenum))
  }
  majdata=kmdata[-deletetotal,]
  SNTKdata[[i]]=rbind(majdata,sntdata[-yin0,])
}
  #library(apcluster)  确定最佳聚类个数（多数类样本训练个数一般是2.4-2.5万之间，估计聚类数为110到120之间）
  #t=sample(1:nrow(kmdata),10000)
  #kkdata=kmdata[t,-22]
  #d.apclus <- apcluster(negDistMat(r=2), kkdata)
  #length(d.apclus@clusters)  #1000样本时39类，2000样本时73类，5000个样本90类,10000样本104类
  #heatmap(d.apclus)       
#   require(vegan)
#   fit <- cascadeKM(scale(kmdata[,-22]), 110, 120, iter = 100)  #精确迭代，耗时半小时
#   plot(fit, sortg = TRUE, grpmts.plot = TRUE)
SNKdata=vector("list", 10)
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];
  yin1=which(traindata[,cfinal]=="1")
  trainnew=ubSmoteExs(data=traindata[yin1,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍，SMOTE方法
  Sdata=rbind(traindata,trainnew)
  ncldata<-ubNCL(X=Sdata[,-cfinal], Y= Sdata[,cfinal])  #按照NCL规则，删除多数类
  sndata<-cbind(ncldata$X, y=ncldata$Y)
  yin0=which(sndata[,cfinal]=="0");lenthof1=nrow(sndata)-length(yin0)
  kmdata=sndata[yin0,];
  km=kmeans(scale(kmdata[,-cfinal]),110,iter.max = 100) #按照SBC规则（聚类采用kmeans），删除多数类
  km2data=cbind(kmdata,class=as.numeric(km$cluster))
  deletetotal=NULL
  for(j in 1:110)
  {
    t=which(km2data$class==j)
    deleteratio=(length(yin0)-lenthof1)/length(yin0)
    deletenum=round(length(t)*deleteratio)
    deletetotal=c(deletetotal,sample(t,deletenum))
  }
  majdata=kmdata[-deletetotal,]
  SNKdata[[i]]=rbind(majdata,sndata[-yin0,])
}
STKdata=vector("list", 10) 
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];
  yin1=which(traindata[,cfinal]=="1")
  trainnew=ubSmoteExs(data=traindata[yin1,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍，SMOTE方法
  Sdata=rbind(traindata,trainnew)
  tkdata<-ubTomek(X=Sdata[,-cfinal], Y= Sdata[,cfinal])   #按照TK规则，删除多数类
  STdata<-cbind(tkdata$X, y=tkdata$Y)
  yin0=which(STdata[,cfinal]=="0");lenthof1=nrow(STdata)-length(yin0)
  kmdata=STdata[yin0,];
  km=kmeans(scale(kmdata[,-cfinal]),110,iter.max = 100) #按照SBC规则（聚类采用kmeans），删除多数类
  km2data=cbind(kmdata,class=as.numeric(km$cluster))
  deletetotal=NULL
  for(j in 1:110)
  {
    t=which(km2data$class==j)
    deleteratio=(length(yin0)-lenthof1)/length(yin0)
    deletenum=round(length(t)*deleteratio)
    deletetotal=c(deletetotal,sample(t,deletenum))
  }
  majdata=kmdata[-deletetotal,]
  STKdata[[i]]=rbind(majdata,STdata[-yin0,])
}
SNTdata=vector("list", 10)
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];
  yin1=which(traindata[,cfinal]=="1")
  trainnew=ubSmoteExs(data=traindata[yin1,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍，SMOTE方法
  Sdata=rbind(traindata,trainnew)
  tkdata<-ubTomek(X=Sdata[,-cfinal], Y= Sdata[,cfinal])   #按照TK规则，删除多数类
  STdata<-cbind(tkdata$X, y=tkdata$Y)
  ncldata<-ubNCL(X=STdata[,-cfinal], Y= STdata[,cfinal])  #按照NCL规则，删除多数类
  sntdata<-cbind(ncldata$X, y=ncldata$Y)
  SNTdata[[i]]=sntdata
}
SKdata=vector("list", 10)
for(i in 1:10)
{
  traindata<-data[-folds[[i]],];
  yin1=which(traindata[,cfinal]=="1")
  trainnew=ubSmoteExs(data=traindata[yin1,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍，SMOTE方法
  Sdata=rbind(traindata,trainnew)
  yin0=which(Sdata[,cfinal]=="0");lenthof1=nrow(Sdata)-length(yin0)
  kmdata=Sdata[yin0,];
  km=kmeans(scale(kmdata[,-cfinal]),110,iter.max = 100) #按照SBC规则（聚类采用kmeans），删除多数类
  km2data=cbind(kmdata,class=as.numeric(km$cluster))
  deletetotal=NULL
  for(j in 1:110)
  {
    t=which(km2data$class==j)
    deleteratio=(length(yin0)-lenthof1)/length(yin0)
    deletenum=round(length(t)*deleteratio)
    deletetotal=c(deletetotal,sample(t,deletenum))
  }
  majdata=kmdata[-deletetotal,]
  SKdata[[i]]=rbind(majdata,Sdata[-yin0,])
}
#随机森林  
library(randomForest)
p1=matrix(0,2,2);pred1=NULL
for(i in 1:10){  
  traindata<-SKdata[[i]]
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
library(rpart)
p2=matrix(0,2,2);pred2=NULL;ytestall=NULL
for(i in 1:10){  
  traindata<-SNTKdata[[i]]
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
data[,cfinal]=as.factor(data[,cfinal])
library(C50) 
p3=matrix(0,2,2);pred3=NULL
for(i in 1:10){  
  traindata<-SOKdata[[i]]
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
library(class);library(gmodels)
p5=matrix(0,2,2);pred5=NULL
for(i in 1:10){  
  traindata<-SNTKdata[[i]]
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
library(MASS)  
p6=matrix(0,2,2);pred6=NULL
for(i in 1:10){  
  traindata<-SKdata[[i]]
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
library(e1071)
p7=matrix(0,2,2);pred7=NULL
for(i in 1:10){  
  traindata<-SNTKdata[[i]]
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
p8=matrix(0,2,2);pred8=NULL
for(i in 1:10){  
  traindata<-SNTdata[[i]]
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
library(nnet)
p9=matrix(0,2,2);pred9=NULL
for(i in 1:10){  
  traindata<-SNTKdata[[i]]
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
library(RSNNS)
p10=matrix(0,2,2);pred10=NULL
for(i in 1:10){  
  traindata<-SKdata[[i]]
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

#SVM    
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
#library(devtools)
#find_rtools()
#devtools::install_github("Microsoft/LightGBM", subdir = "R-package")
library(lightgbm)
p12=matrix(0,2,2);pred12=NULL
for(i in 1:10)
{
  traindata<-SNTKdata[[i]]
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


