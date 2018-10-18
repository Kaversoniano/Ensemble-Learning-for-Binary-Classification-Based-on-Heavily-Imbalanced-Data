### Boruta ###
library(Boruta)
boruta.train <- Boruta(y~., data = train, doTrace = 2);
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i]);
names(lz) <- colnames(boruta.train$ImpHistory);
Labels <- sort(sapply(lz,median));
axis(side = 1, las = 2, labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7);
title(main = "Boruta - feature selection")

final.boruta <- TentativeRoughFix(boruta.train);
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta);
print(boruta.df)

bankR <- bankR[,c(boruta.df$decision=="Confirmed",TRUE)]; # 31 + 1 = 32 variables including y
bank_fullR <- bank_fullR[,c(boruta.df$decision=="Confirmed",TRUE)]; # 31 + 1 = 32 variables including y
names_boruta=colnames(bank_fullR);names_boruta


### RFE ###
library(caret)
library(randomForest)
control <- rfeControl(functions=rfFuncs, method="cv", number=10);
rfe.train <- rfe(train[,-41], train$y, sizes = 1:40, rfeControl = control);
print(rfe.train)
plot(rfe.train, type = c("g", "o"), cex = 1, col = 1:40)
title(main = "RFE - feature selection")
predictors(rfe.train)

bankR <- bankR[,c(predictors(rfe.train),"y")]; # 10 + 1 = 11 variables including y
bank_fullR <- bank_fullR[,c(predictors(rfe.train))]; # 10  variables 
names_ref=colnames(bank_fullR);names_ref

#XGBOOST
#在excel将样本的属性数值都改成小数，这样能保证读入的data是符合xgboost的矩阵类型num，而不是int
y=as.numeric(bank_fullR[,41])-1
bank_fullR=cbind(apply(bank_fullR[,-41],2,as.numeric),y)
require(xgboost)
cf=41
xtrain=as.matrix(bank_fullR[,-cf,drop=F]);ytrain=as.matrix(bank_fullR[,cf,drop=F])
xgmat <- xgb.DMatrix(xtrain,label=ytrain)
bst <- xgboost(data = xgmat, max.depth = 3, eta = 0.1,
                 nround = 100, objective = "binary:logistic",nthread=16,min_child_weight=1,
                 max_delta_step=100,subsample=1,eval_metric="logloss",silent=1)
model <- xgb.dump(bst, with_stats = T)
model[1:10] #This statement prints top 10 nodes of the model
names=colnames(bank_fullR)
# 计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = bst)
# 制图
xgb.plot.importance(importance_matrix,cex= 1.2,xlab = "Relative importance")
names_xgboost=importance_matrix$Feature;names_xgboost
#汇总3种特征工程选择的特征集合
fs=c(names_boruta,names_ref,names_xgboost)
#进行table汇总，并且排序，发现最前面的21个特征是2个特征工程以上都认为是重要的
temp=sort(table(fs),decreasing = T);temp
importance=names(temp)[1:21]
bank_fullR=read.csv("bank_fullR.csv")
data=bank_fullR[,c(importance,"y")]
write.csv(data,"bank_full_fs.csv",row.names = F)
