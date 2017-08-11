##########################################################################
############################### 模型检验 #################################
##########################################################################
####### 绘制ROC曲线 ######
library("pROC") 
roc_list <- roc(valid$ , model_pre[,2])
plot(roc_list, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)
roc_list$thresholds[which.max(roc_list$sensitivities+roc_list$specificities)]
roc_list$auc[which.max(roc_list$sensitivities+roc_list$specificities)]

####### Gains chart & Lift chart ######
data<-as.data.frame(model_pre[,2],valid$ )
data <- data[order(data[, 1], decreasing = TRUE), ]
data$rpp <- row(data[, 1, drop = FALSE])/nrow(data)
data$target_cum <- cumsum(data[,2])
data$tpr <- data$target_cum/sum(data[,2])
data$lift <- data$tpr/data$rpp
plot(data$rpp, data$tpr, type = "l", main = "Gain Chart")
plot(data$rpp, data$lift, type = "l", main = "Lift Chart")



############ 计算混淆矩阵 #############
#阈值，可修改
thresh<- roc_list$thresholds[which.max(roc_list$sensitivities+roc_list$specificities)]
pred_class <- as.integer(model_pre> thresh)    
cfmatrix<- table(pred_class,valid$ )
tp <- cfmatrix[2, 2]
tn <- cfmatrix[1, 1]
fp <- cfmatrix[2, 1]
fn <- cfmatrix[1, 2]
accuracy <- (tp + tn)/(tp + tn + fp + fn)
sensitivity <- tp/(tp + fn)   #TPR,也叫召回率(Recall)
precision<- tp/(tp + fp)      #精确率
specificity <- tn/(tn + fp)
print(1-specificity)                
F_measure<-2*precision*sensitivity/(precision+sensitivity) # F测度 = 2 * 精确率 * 召回率/( 精确率 + 召回率 )
sensitivity;precision


############# ks曲线 #############
val_lable<- valid$
  TP_rate<-c()
FP_rate<-c()
for(i in 1:51){
  thrsd_pre<- as.integer(model_pre> 0.02*(i-1))   
  
  Q<-cbind(thrsd_pre,val_lable,model_pre)
  TP<-subset(Q,thrsd_pre==1 & val_lable==1,select = c(1,2))
  FP<-subset(Q,thrsd_pre==1 & val_lable==0,select = c(1,2))
  P<-subset(Q,val_lable==1,select = c(1,2))
  N<-subset(Q,val_lable==0,select = c(1,2))
  
  TP_rate[i]<-nrow(TP)/nrow(P)
  FP_rate[i]<-nrow(FP)/nrow(N)
}

plot(rev(TP_rate), type="l", col="blue",lty=2,xaxt="n",ann=FALSE)
axis(side=1,at=c(1:51),labels=seq(1,0,-0.02))
lines(rev(FP_rate), type="l", col="red",lty=2)
lines(rev(TP_rate) - rev(FP_rate), type="l",lty=1)
title(main="KS曲线", col.main="red", font.main=4) # 增添标题，红色，粗斜体
legend(0,1,c("TPR","FPR","TPR-FPR"), cex=0.75, col=c("blue","red","black"),  lty=c(2,2,1))

ks_value<-max( rev(TP_rate) - rev(FP_rate) )

