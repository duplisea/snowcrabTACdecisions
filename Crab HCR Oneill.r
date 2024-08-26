# generate mock cpue data from sine wave with lognormal error
# set the survey index equal to the sine wave with error. Commerical index
# is the survey index with another layer of lognormal error
  x= 1:4000
  x2= sin(x/4.2) + 1
  x3= 1.3^x2 * 10 * exp(rnorm(length(x2),0,0.1))
  Is= x3
  If= 1.3^(x2 * exp(rnorm(length(x2),0,0.2))) * 10 * exp(rnorm(length(x2),0,0.15))
  plot(x,Is,type="b")
  plot(x,If,type="b")
  cor(Is,If)
# another fake dataset
  #N=3000
  #Is=runif(N,1,100)
  #If=runif(N,1,100)

# set a base quota as a mean value with small lognormal error.
# i.e more stable than cpue index
  Qhist= rlnorm(length(If),log(mean(If)/2),.05)
  Qbase= mean(Qhist[(length(If)-5):length(If)])
  Qmax= max(Qhist)*1.5
  Qt= Qbase/3

# reference points for the survey and fishery
  BBADs= quantile(Is,.3) #limit is 10th percentile of all values
  BEXEs= quantile(Is,.8) #usr is 50th percentile of all values
  BBADf= quantile(If,.3) #limit is 10th percentile of all values
  BEXEf= quantile(If,.8) #usr is 50th percentile of all values

# targets for the survey and fishery = mean of the 5 most recent data years
  #Ts= mean(Is[(length(Is)-5):length(Is)])
  #Tf= mean(If[(length(If)-5):length(If)])
  Ts= quantile(Is,.5) #an alternative target as this last 5 years makes no sense if you are in the hole already
  Tf= quantile(If,.5) #an alternative target as this last 5 years makes no sense if you are in the hole already

# most recent values of indices and combined index
  Cs= mean(Is[(length(Is)-2):length(Is)])
  Cf= mean(If[(length(If)-2):length(If)])

# organise reference points used in decision matrix
  refpts=as.data.frame(matrix(c(Ts,BBADs,BEXEs,Tf,BBADf,BEXEf,Qbase,Qmax),nrow=1))
  names(refpts)=c("Ts","BBADs","BEXEs","Tf","BBADf","BEXEf","Qbase","Qmax")

HCR.f= function(Cs,Cf,Qt,ref.pts=refpts){

  # decision matrix (ONeill 2010, Table 1) for quota multiplier (lambda)
  #! note that the Qt=Qbase needs to be changed for a simulation dependent upon last year's quota
    Ts=ref.pts$Ts; BBADs=ref.pts$BBADs; BEXEs=ref.pts$BEXEs;Tf=ref.pts$Tf
    BBADf=ref.pts$BBADf; BEXEf=ref.pts$BEXEf; Qbase=ref.pts$Qbase
    Qmax=ref.pts$Qmax

  # theta and halfup calculation based on current stock state
    theta= mean(c(Cs/Ts, Cf/Tf))
    halfup= (theta-1)/2 + 1

    lambda=-9.99
    #row 1
    if (Cs>=BEXEs & Cf<=BEXEf ) lambda=1                          #c1
    else if (Cs>=BEXEs & Cf>BBADf & Cf<=BEXEf ) lambda=1               #c2
    else if (Cs>=BEXEs & Cf>=BEXEf  ) lambda=halfup                     #c3
    #row 2
    else if (Cs>BBADs & Cs<BEXEs & Cf<=BEXEf & Cs<Ts ) lambda=theta     #c1a
    else if (Cs>BBADs & Cs<BEXEs & Cf<=BEXEf & Cs>=Ts ) lambda=1      #c1b
    else if (Cs>BBADs & Cs<BEXEs & Cf>BBADf & Cf<=BEXEf ) lambda=1    #c2
    else if (Cs>BBADs & Cs<BEXEs & Cf>=BEXEf ) lambda=1                #c3
    #row 3
    else if (Cs<=BBADs & Cf<=BEXEf & theta<=0.5 ) lambda=10^-8       #c1a
    else if (Cs<=BBADs & Cf<=BEXEf & theta>0.5 ) lambda=theta              #c1b
    else if (Cs<=BBADs & Cf>BBADf & Cf<=BEXEf & Cf<Tf) lambda=theta     #c2a
    else if (Cs<=BBADs & Cf>BBADf & Cf<=BEXEf & Cf>=Tf) lambda=1      #c2b
    else if (Cs<=BBADs & Cf>=BEXEf ) lambda=1                           #c3
    else lambda=-9999

  # new quota determination (ONeill 2010, eq 1)
    Qcalc= lambda * Qbase
    Qt1= min(Qcalc,Qmax)
    if (Qt1/Qt <1.05 & Qt1/Qt >0.95) Qt1=Qt else Qt=Qt1
    Qt<<- Qt

  #output of decisions with record of states
    states= ref.pts
    states$Cs= Cs
    states$Cf= Cf
    states$theta= theta
    states$halfup= halfup
    states$lambda= lambda
    states$Qt= Qt
    round(states,2)
    states
}


##### do a simulation with data generated from a sine wave at the top of the file
  out= as.data.frame(matrix(ncol=14,nrow=length(Is)))
  for (i in 1:length(Is)){
    out[i,]=HCR.f(Is[i],If[i],Qt=Qt,refpts)
    }
  names(out)= c("Ts","BBADs","BEXEs","Tf","BBADf","BEXEf","Qbase","Qmax","Cs","Cf","theta","halfup","lambda","Qt")
  
  library(akima)
  crap=interp(out$Cs,out$Cf,out$Qt)

  par(mfcol=c(4,1),mar = c(4.2,4.2,1,1))
  plot(x,Is,ylim=range(c(Is,If)),xlab="year",ylab="Survey index",type="l")
  plot(x,If,ylim=range(c(Is,If)),xlab="year",ylab="Fishery index",type="l")
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
  plot(out$theta,out$Qt,xlab="Combined index",ylab="Quota")