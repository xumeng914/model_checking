###########自写函数###########
cs<-model_pre$v1
b<-testData$ln_f_new
r<-c()
TP_rate<-c()
FP_rate<-c()
for(i in 1:51){
  a <- as.integer(cs> 0.02*(i-1))
  
  Q<-cbind(a,b,cs)
  TP<-subset(Q,a==1 & b==1  ,select = c(1,2))
  TP0<-subset(Q,a==0 & b==0  ,select = c(1,2))
  FP<-subset(Q,a==1 & b==0  ,select = c(1,2))
  y<-subset(Q, b==1  ,select = c(1,2))
  y0<-subset(Q, b==0  ,select = c(1,2))
  
  r[i]<-(nrow(TP)+nrow(TP0))/nrow(Q)
  TP_rate[i]<-nrow(TP)/nrow(y)
  FP_rate[i]<-nrow(FP)/nrow(y0)
}

#画roc曲线
plot(FP_rate, TP_rate, main = "Roc", xlab = "FPR", ylab = "TPR",type="o")
roc<-0
for(i in 1:50){
  roc<-roc+(FP_rate[i]-FP_rate[i+1])*mean(TP_rate[i],TP_rate[i+1])
}
legend(0.6,0.4,  paste("AUC=",round(roc,4),sep=""))

#ks曲线
plot( rev(TP_rate), type="o", col="blue",lty=2,xaxt="n",ann=FALSE)
axis(side=1,at=c(1:51),labels=seq(0,1,0.02))
lines( rev(FP_rate), type="o", col="red",lty=2)
lines( rev(TP_rate)- rev(FP_rate), type="o")
title(main="KS曲线", col.main="red", font.main=4) ## 增添标题，红色，粗斜体
legend(x="topleft",  c("tp","fp","tp-fp"), cex=0.6,
       col=c("blue","red","black"), pch=21, lty=c(2,2,1))

write.table(cbind(rev(TP_rate),rev(FP_rate)), paste("ks.csv",sep=""),
            row.names = FALSE,col.names = TRUE, quote = FALSE,sep=",")

