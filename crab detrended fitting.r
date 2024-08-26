zone17=read.table("D:/papers & projects/snow crabs/TAC decisions/zone17.csv",sep=",",header=T)


plot(zone17$year[-(1:12)],zone17$sur.cpue[-(1:12)]/mean(zone17$sur.cpue[-(1:12)]),xlim=c(1984,2010),ylim=c(0,2),type="l")
lines(zone17$year[-c(1,27)],zone17$com.cpue[-c(1,27)]/mean(zone17$com.cpue[-c(1,27)]))
x=zone17$landings[-c(1,26,27)]
y=diff(zone17$com.cpue[-c(1,27)])
x2=zone17$landings[-c(1:12,27)]
y2=diff(zone17$sur.cpue[-c(1:12)])
years=zone17$year[-c(1,26,27)]
plot(x,y,type="n",xlab="Landings (t)",ylab="Delta CPUE")
text(x,y,years,cex=.8)
abline(h=0)
library(MASS)
contour(kde2d(x,y),add=T,drawlabels=F,col="blue")




#lm fitted sine wave
  x=zone17$year[-c(1,27)]
  y=as.numeric(as.character(zone17$com.cpue[-c(1,27)]))
  tmp2=lm(y~(sin(x)+cos(x)))
  int= b1=coef(tmp2)[1]
  b1=coef(tmp2)[2]
  b2=coef(tmp2)[3]
  b=atan(b2/b1)
  cc=sqrt(b1^2+b2^2)
  a=int
  par(mfcol=c(2,1))
  plot(x,y,type="b",xlab="Year",ylab="CPUE",lwd=2)
  lines(x,predict(tmp2),col="blue",lwd=2)
  legend("topleft",bty="n",legend="Detrending sine wave",col="blue",lty=1,lwd=2)
  plot(x,residuals(tmp2),type="b",xlab="Year", ylab="Detrended CPUE",lwd=2)