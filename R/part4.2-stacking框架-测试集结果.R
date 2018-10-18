#将样本的属性设置为数值型，这样能保证读入的data是符合xgboost的num类型而不是int类型
alldata=read.csv("bank_full_fs.csv") #可以在R里设置为as.numeric
library(caret)
library(unbalanced)
cfinal=22
alldata[,cfinal]=as.factor(alldata[,cfinal])
set.seed(1234)  
fo<-createFolds(y=alldata[,cfinal],k=10) #把原始数据集随机切分成10等份
test=alldata[fo[[10]],]   #把随机的最后一份数据定为为test
data=alldata[-fo[[10]],]      #其他数据用来训练模型
#2代表所有的训练集用来重抽样
SNTdata2=NULL    #每个字母代表一种抽样方法，多个字母代表联合抽样方法
traindata<-data;
yin1=which(traindata[,cfinal]=="1")
trainnew=ubSmoteExs(data=traindata[yin1,],cfinal, N = 200, k = 5)   #创建少数类，按照2倍，SMOTE方法
Sdata=rbind(traindata,trainnew)
tkdata<-ubTomek(X=Sdata[,-cfinal], Y= Sdata[,cfinal])   #按照TK规则，删除多数类
STdata<-cbind(tkdata$X, y=tkdata$Y)
ncldata<-ubNCL(X=STdata[,-cfinal], Y= STdata[,cfinal])  #按照NCL规则，删除多数类
sntdata<-cbind(ncldata$X, y=ncldata$Y)
SNTdata2=sntdata    
#2代表所有的训练集用来重抽样
SKdata2=NULL
traindata<-data
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
SKdata2=rbind(majdata,Sdata[-yin0,])
#2代表所有的训练集用来重抽样
SNTKdata2=NULL
traindata<-data
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
SNTKdata2=rbind(majdata,sntdata[-yin0,])
#2代表所有的训练集用来重抽样
SNKdata2=NULL
traindata<-data
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
SNKdata2=rbind(majdata,sndata[-yin0,])
#2代表所有的训练集用来重抽样
STKdata2=NULL
traindata<-data
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
STKdata2=rbind(majdata,STdata[-yin0,])
#本文有5种较好的联合抽样方法，依次运行11个分类器，得到训练集自身的分类结果
#每次sampledata赋值以后，就运行11个分类器的封装代码
sampledata=SNTKdata2   
SNTKpre2=cbind(pre1,pre2,pre3,pre5,pre6,pre7,pre8,pre9,pre10,pre11,pre12)
sampledata=SNTdata2   
SNTpre2=cbind(pre1,pre2,pre3,pre5,pre6,pre7,pre8,pre9,pre10,pre11,pre12) 
sampledata=SKdata2
SKpre2=cbind(pre1,pre2,pre3,pre5,pre6,pre7,pre8,pre9,pre10,pre11,pre12) 
sampledata=SNKdata2
SNKpre2=cbind(pre1,pre2,pre3,pre5,pre6,pre7,pre8,pre9,pre10,pre11,pre12)
sampledata=STKdata2
STKpre2=cbind(pre1,pre2,pre3,pre5,pre6,pre7,pre8,pre9,pre10,pre11,pre12)
#################11个分类器封装代码用于测试集############start###########  
#multiSVM 本文改进的SVM算法框架-Time difference of 2.622417 mins
#precisions: 0.3709416 recall: 0.8638941 Fscore: 0.6131696 
library(e1071)
traindatatotal<-sampledata
yin0=which(traindatatotal[,cfinal]==0);yin1=which(traindatatotal[,cfinal]==1)
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
preofall=rep(0,length(ytest))
for(j in 1:20)
{
  trin0=sample(yin0,length(yin0)/20);trin1=sample(yin1,length(yin1)/20)
  traindata=traindatatotal[c(trin0,trin1),]
  xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
  ytrain=as.factor(ytrain)
  modelone=svm(xtrain, ytrain, kernel = "radial",  degree = 3, gamma =1/ncol(xtrain)) 
  preofone=predict(modelone,xtest);
  preofall=preofall+(as.numeric(preofone)-1);
}
pre11=preofall/20;
tabl=table(ifelse(pre11>=0.5,1,0),ytest)
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#朴素贝叶斯分类器   
#precisions: 0.3791667 recall: 0.6880907 Fscore: 0.5501686
library(e1071)
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
efit <- naiveBayes(y~.,traindata)  
pre7 <- predict(efit,xtest);
tabl=table(pre7,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#神经网络 多层感知器mlp   
#precisions: 0.2720867 recall: 0.9489603 Fscore: 0.5375175
library(RSNNS)
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
xtrain=scale(xtrain);xtest=scale(xtest)
ytrain=decodeClassLabels(ytrain)
ytest=decodeClassLabels(ytest)
model <- mlp(xtrain, ytrain, size=5, learnFuncParams=c(0.1), 
             maxit=50, inputstest=xtest, targetstest=ytest)
pre10 = predict(model,xtest)[,2];pre10=ifelse(pre10>0.5,1,0)
tabl=confusionMatrix(pre10,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#KNN分类器  
# precisions: 0.323895 recall: 0.8865784 Fscore: 0.5777504
library(class);library(gmodels)
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
pre5<-knn(train=scale(xtrain),test=scale(xtest),cl=ytrain,k=round(sqrt(nrow(xtrain))));
tabl=table(pre5,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#随机森林  
#precisions: 0.4242144 recall: 0.8676749 Fscore: 0.6465079
library(randomForest)
traindata<-sampledata
xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
testdata<-test;xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
model1=randomForest(xtrain,ytrain)
pre1=predict(model1,xtest);
tabl=table(pre1,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#ID3决策树   
#precisions: 0.3868275 recall: 0.7882798 Fscore: 0.597487
library(rpart)
traindata<-sampledata
xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
testdata<-test;xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
model2=rpart(y~.,data=traindata)
pre2=predict(model2,xtest)
pre2=ifelse(pre2[,1]>0.5,0,1);
tabl=table(pre2,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#C50决策树
# precisions: 0.4205598 recall: 0.8397059 Fscore: 0.6426363
library(C50) 
traindata<-sampledata
xtrain=traindata[,-cfinal];ytrain=traindata[,cfinal]
testdata<-test;xtest=testdata[,-cfinal];ytest=testdata[,cfinal] 
model3=C5.0(xtrain, ytrain)
pre3=predict(model3,xtest);
tabl=table(pre3,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#LDA判别分类    
# precisions: 0.4046042 recall: 0.7569328 Fscore: 0.5969794 
library(MASS)  
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
ff1=lda(y~.,traindata) 
pre6=predict(ff1,as.data.frame(xtest))$class;
tabl=table(pre6,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#logistic回归分类      
# precisions: 0.3906904 recall: 0.7846639 Fscore: 0.598853
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
anes1=glm(y~.,family=binomial(link='logit'),data=traindata)
pre8=predict(anes1,testdata[,-cfinal]);pre8=exp(pre8)/(1+exp(pre8))#计算因变量的值
pre8=ifelse(pre8>0.5,1,0);
tabl=table(pre8,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
### BP 神经网络   
# precisions: 0.3642599 recall: 0.8256303 Fscore: 0.5940974
library(nnet)
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
fitBP <- nnet(y~., data = traindata, size = 10, rang = 0.1,decay = 5e-4, maxit = 200);
pre9<- predict(fitBP, xtest, type = "class");
tabl=table(pre9,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#16年12月发布的微软开源包-LightGBM   
# precisions: 0.4616148 recall: 0.7912649 Fscore: 0.6487212
#library(devtools)
#find_rtools()
#devtools::install_github("Microsoft/LightGBM", subdir = "R-package")
library(lightgbm)
traindata<-sampledata
xtrain=as.matrix(traindata[,-cfinal,drop=F]);ytrain=as.matrix(traindata[,cfinal,drop=F])
testdata<-test;xtest=as.matrix(testdata[,-cfinal,drop=F]);ytest=as.matrix(testdata[,cfinal,drop=F])
dtrain <- lgb.Dataset(xtrain,label = ytrain, free_raw_data=T)
dtest <- lgb.Dataset.create.valid(dtrain,xtest,label = ytest)
params <- list(objective = "multiclass", metric = "multi_logloss")
tests <- list(test=dtest)
bst <- lgb.train(params,dtrain,nrounds = 500,tests,num_threads = 4,num_class = 2,
                 verbose = 0,record = T,early_stopping_rounds = 5,
                 min_data = 10,learning_rate = 1)
pre12<- predict(bst, xtest, reshape=T)[,2];
pre12=ifelse(pre12>=0.5,1,0)
tabl=table(pre12,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#################11个分类器封装代码用于测试集############end########### 
testallpre=cbind(SKpre2,SNTpre2,SNKpre2,STKpre2,SNTKpre2)  #得到训练集的新特征-各分类器预测结果
#xgboost算法    作为stacking框架的第二层再学习模型
#precisions: 0.4702259 recall: 0.8657845 Fscore: 0.6877671 
require(xgboost)
names=c(paste("SKpre",c(1:3,5:12)),paste("SNTpre",c(1:3,5:12)),paste("SNKpre",c(1:3,5:12))
        ,paste("STKpre",c(1:3,5:12)),paste("SNTKpre",c(1:3,5:12)),"y")
colnames(trainallpre)=names;nco=ncol(trainallpre)
colnames(testallpre)=names[1:(length(names)-1)]
traindata<-as.data.frame(trainallpre);
nc=ncol(traindata)
traindata=apply(traindata,2,as.numeric) #所有变量必须是num类型. XGBoost won't work with categorical (factor) variables
xtrain=as.matrix(traindata[,-nc,drop=F]);ytrain=as.matrix(traindata[,nc,drop=F])-1
xgmat <- xgb.DMatrix(xtrain,label=ytrain)
bst <- xgboost(data = xgmat, max.depth = 3, eta = 0.1,
               nround = 100, objective = "binary:logistic",nthread=16,#min_child_weight=NULL,
               max_delta_step=100,subsample=1,eval_metric="logloss",verbose = 0)
xtest=apply(testallpre,2,as.numeric)
pre4<- predict(bst,xtest)
pre4=ifelse(pre4>0.2,1,0);
ytest=test[,cfinal]
tabl=table(pre4,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#选择性集成-分簇法和排序法结合
traindata<-as.data.frame(trainallpre);
nc=ncol(traindata)
traindata=apply(traindata,2,as.numeric) 
fscore=function(pre)
{
  if(mean(pre)>1)
    pre=pre-1
  tabl=table(ifelse(pre>=0.5,1,0),label);
  preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
  beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
  return(Fscore)
}
fscores=apply(traindata[,-nc],2,fscore)
km=kmeans(t(scale(traindata[,-nc])),10,iter.max = 80) #分簇法
km2data=cbind(fscores,class=as.numeric(km$cluster))
deall=NULL
for(j in 1:10)
{
  t=which(km2data[,2]==j)
  deleteratio=0.15
  deletenum=ceiling(length(t)*deleteratio)
  threshold=sort(km2data[t,1])[deletenum]        #排序法基于fscores
  delteteindex=t[which(km2data[t,1]<=threshold)]
  deall=c(deall,delteteindex)
}
traindata1=traindata[,-deall]
nc=ncol(traindata1)
traindata=apply(traindata1,2,as.numeric) #所有变量必须是num类型. XGBoost won't work with categorical (factor) variables
xtrain=as.matrix(traindata1[,-nc,drop=F]);ytrain=as.matrix(traindata1[,nc,drop=F])-1
xgmat <- xgb.DMatrix(xtrain,label=ytrain)
bst <- xgboost(data = xgmat, max.depth = 3, eta = 0.1,
               nround = 100, objective = "binary:logistic",nthread=16,#min_child_weight=NULL,
               max_delta_step=100,subsample=1,eval_metric="logloss",verbose = 0)
xtest1=apply(testallpre[,-deall],2,as.numeric)
pre4<- predict(bst,xtest1)
pre4=ifelse(pre4>0.2,1,0);
ytest=test[,cfinal]
tabl=table(pre4,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
deall=deall1;deall1 #删除的分类器如下
# SNTpre 5  SNTpre 6 SNTKpre 8  SNKpre 3  STKpre 3 SNTKpre 3  SNKpre 2  SNTpre 2 
# 15        16        51        25        36        47        24        13 
# STKpre 10 SNTKpre 9   SKpre 5  SNKpre 5   SKpre 7 SNKpre 10 
# 42        52         4        26         6        31 

#本文最终得到的xgboost-stacking选择集成框架
#选择性集成，除去了冗余的分类器，分类器预测结果作为样本的新特征，并加入原始特征，进行最终训练
#precisions: 0.4885993 recall: 0.8506616 Fscore: 0.6927176 
require(xgboost)
trainx=NULL
for(i in 1:10)
{
  trainx<-rbind(trainx,data[folds[[i]],-cfinal]);
}
traindata<-cbind(trainallpre[,-c(deall,nco)],trainx,trainallpre[,nco])
nc=ncol(traindata)
traindata=apply(traindata,2,as.numeric) #所有变量必须是num类型. XGBoost won't work with categorical (factor) variables
xtrain=as.matrix(traindata[,-nc,drop=F]);ytrain=as.matrix(traindata[,nc,drop=F])-1
xgmat <- xgb.DMatrix(xtrain,label=ytrain)
bst <- xgboost(data = xgmat, max.depth = 3, eta = 0.1,
               nround = 99, objective = "binary:logistic",nthread=16,min_child_weight=0.1,
               max_delta_step=100,subsample=1,eval_metric="logloss",verbose=0)
xtest=cbind(testallpre[,-deall],test[,-cfinal])
xtest=apply(xtest,2,as.numeric)
pre4<- predict(bst,xtest)
pre4=ifelse(pre4>0.2,1,0);
ytest=test[,cfinal]
tabl=table(pre4,ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
# #若采用随机森林作为第二层，耗时约30分钟且效果差于xgboost
# #precisions: 0.4947183 recall: 0.5311909 Fscore: 0.5194085 
#通常其他论文中会采用的投票方法
f=function(x)   #有一些分类器的结果是1和2，需要转换为0和1.
{
  if(mean(x)>1)
    return(x-1)
  else return(ifelse(x>=0.5,1,0))
}
votedata=apply(testallpre,2,as.numeric)
votedata=apply(votedata,2,f)
vote=apply(votedata,1,mean)
tabl=table(ifelse(vote>=0.5,1,0),ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")
#通常其他论文中会采用的加权投票法
weight=fscores
weightsum=function(x)
{
  if(length(x)==55)
    return(sum(x*weight)/sum(weight))
  else return(NULL)
}
voteweight=apply(votedata,1,weightsum)
tabl=table(ifelse(voteweight>=0.5,1,0),ytest);
preci=tabl[2,2]/sum(tabl[2,]);recal=tabl[2,2]/sum(tabl[,2])
beta=1.5;Fscore=(beta^2+1)/(beta^2/recal+1/preci);
print(tabl);
cat("precisions:",preci,"recall:",recal,"Fscore:",Fscore,"\n")



