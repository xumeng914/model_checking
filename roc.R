library("pROC") 
roc_list <- roc(valid$cc_cst_f ,model_pre)
plot(roc_list, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)

coordinate <- which.max(roc_list$sensitivities+roc_list$specificities)
yuzhi <- roc_list$thresholds[coordinate] 
pre <- c()
pre[model_pre > yuzhi] <- 1
pre[model_pre <= yuzhi] <- 0

compareTable <- table (valid$cc_cst_f, pre)  
TP_rate<-compareTable[2,2]/(compareTable[2,2]+compareTable[2,1])
FP_rate<-compareTable[1,2]/(compareTable[1,2]+compareTable[1,1])
r<-(compareTable[2,2]+compareTable[1,1])/(compareTable[1,1]+compareTable[1,2]+compareTable[2,1]+compareTable[2,2])
precision<-compareTable[2,2]/(compareTable[2,2]+compareTable[1,2])
ks<-TP_rate-(FP_rate)
TP_rate;FP_rate;precision;r;ks