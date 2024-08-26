setwd("/home/daniel/papers.and.projects/Snowcrab/TACdecisions")
zone17= read.csv("zone17.csv")
source("crab HCR.f.r")

ff= zone17$com.cpue[13:26]
ss= zone17$sur.cpue[13:26]
ffss= mean(ff/ss)
Is= c(zone17$com.cpue[2:12]/ffss,zone17$sur.cpue[13:27])
If= c(zone17$com.cpue[2:26],zone17$sur.cpue[27]*ffss)

ff= zone17$com.cpue[13:26]
ssj= zone17$sur.juv.cpue[13:26]
ffssj= mean(ff/ssj)
Isj= c(zone17$com.cpue[2:12]/ffssj,zone17$sur.juv.cpue[13:27])
If= c(zone17$com.cpue[2:26],zone17$sur.juv.cpue[27]*ffssj)

# Catch reference points
  Qbase= mean(zone17$landings)
  Qmax= max(zone17$landings) #2200
  Qt= zone17$landings[1]

# reference points for the survey and fishery
  BBADs= quantile(Is,.3) #limit is 10th percentile of all values
  BEXEs= quantile(Is,.8) #usr is 50th percentile of all values
  BBADf= quantile(If,.3) #limit is 10th percentile of all values
  BEXEf= quantile(If,.8) #usr is 50th percentile of all values
  BBADsj= quantile(Isj,.3) #limit is 10th percentile of all values
  BEXEsj= quantile(Isj,.8) #usr is 50th percentile of all values

# targets for the survey and fishery
  #Ts= mean(Is[(length(Is)-5):length(Is)])
  #Tf= mean(If[(length(If)-5):length(If)])
  Ts= quantile(Is,.5) #an alternative target as this last 5 years makes no sense if you are in the hole already
  Tf= quantile(If,.5) #an alternative target as this last 5 years makes no sense if you are in the hole already
  Tsj= quantile(Isj,.5) #an alternative target as this last 5 years makes no sense if you are in the hole already

# most recent values of indices and combined index
  Cs= mean(Is[(length(Is)-2):length(Is)])
  Cf= mean(If[(length(If)-2):length(If)])
  Csj= mean(Isj[(length(Isj)-2):length(Isj)])

# organise reference points used in decision matrix
  refpts=as.data.frame(matrix(c(Ts,BBADs,BEXEs,Tf,BBADf,BEXEf,Tsj,Qbase,Qmax),nrow=1))
  names(refpts)=c("Ts","BBADs","BEXEs","Tf","BBADf","BEXEf","Tsj","Qbase","Qmax")

  out= as.data.frame(matrix(ncol=15,nrow=length(Is)))
  for (i in 1:length(Is)){
    out[i,]=crabHCR.f(Is[i],If[i],Isj[i],Qt=Qt,refpts)
    }
  names(out)= c("Ts","BBADs","BEXEs","Tf","BBADf","BEXEf","Tsj","Qbase","Qmax","Cs","Cf","theta","halfup","lambda","Qt")

  library(akima)
  crap=interp(out$Cs,out$Cf,out$Qt)
  par(mfcol=c(5,1),mar = c(4.2,4.2,1,1))
  years=zone17$year[2:27]
  plot(years,Is,xlab="year",ylab="Survey index",type="l")
  plot(years,If,xlab="year",ylab="Fishery index",type="l")
  #colourscale= colorRampPalette(c("white", "black"))
  #filled.contour(crap,color.palette= colourscale,xlab="Survey index",ylab="Fishery index",
  #                xlim=c(0,100),ylim=c(0,100))
  contour(crap,xlab="Survey index",ylab="Fishery index",labcex=.8,nlevels=10)
  abline(v=refpts$BBADs,lty=2,col="red")
  abline(h=refpts$BBADf,lty=2,col="red")
  abline(v=refpts$BEXEs,lty=2,col="green")
  abline(h=refpts$BEXEf,lty=2,col="green")
  points(refpts$Ts,refpts$Tf,pch=19,col="black",cex=3)
  points(refpts$Ts,refpts$Tf,pch=1,col="red",cex=3)
  points(refpts$Ts,refpts$Tf,pch=19,col="yellow",cex=2)
  points(refpts$Ts,refpts$Tf,pch=19,col="green")
  plot(out$theta,out$Qt,xlab="Combined index",ylab="Quota", type="n")
  text(out$theta,out$Qt,years)
  plot(c(1984,years),c(1391,out$Qt),type="l",xlim=c(1984,2010),ylim=c(0,3000),lwd=2,col="blue",xlab="Year",ylab="Landings (t)")
  lines(zone17$year,zone17$landings,lwd=2,col="red")
  lines(zone17$year,zone17$tac,lwd=2,col="red",lty=2)
  legend("topleft",legend=c("HCR","Actual","TAC"),lwd=2,lty=c(1,1,2),col=c("blue","red","red"),bty="n")

  