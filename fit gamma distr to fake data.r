crab.gamma.f= function(cycle.mult=1){
  tmp=c(runif(50,10,25),
  runif(100,20,40),
  runif(150,30,50),
  runif(200,40,60),
  runif(250,50,70),
  runif(300,60,80),
  runif(250,70,90),
  runif(200,80,100)*cycle.mult,
  runif(100,90,110)*cycle.mult,
  runif(50,100,120)*cycle.mult,
  runif(25,110,140)*cycle.mult)

  library(MASS)
  coef(fitdistr(tmp,"gamma"))
  }
  