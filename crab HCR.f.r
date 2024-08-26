crabHCR.f= function(Cs,Cf,Qt,ref.pts=refpts){

  # decision matrix (ONeill 2010, Table 1) for quota multiplier (lambda)
    Ts=ref.pts$Ts; BBADs=ref.pts$BBADs; BEXEs=ref.pts$BEXEs;Tf=ref.pts$Tf
    BBADf=ref.pts$BBADf; BEXEf=ref.pts$BEXEf; Qbase=ref.pts$Qbase
    Qmax=ref.pts$Qmax

  # theta and halfup calculation based on current stock state
    theta= (2/3)*(Cs/Ts) + (1/3)*(Cf/Tf) #survey has two subindices so given 2/3 weight
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
    else if (Cs<=BBADs & Cf<=BEXEf & theta<=0.4 ) lambda=10^-8       #c1a        #! theta logical change from 0.5 to 0.4. As it produced a moratorium in 1989
    else if (Cs<=BBADs & Cf<=BEXEf & theta>0.4 ) lambda=theta              #c1b
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