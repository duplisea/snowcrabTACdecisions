  x=seq(1,40,length=2000)
  a=10;b=0.2;c=10
  y=a+c*sin(x+b)
  range(y)
  y2= y * exp((rnorm(length(y),0,0.3)))
  y2= y * (rbeta(length(y),2,2)+.5)
  y2= y * (rgamma(length(y),11,10))
  plot(x,y2,pch=".")
  lines(x,y,col="blue",lwd=3)

  #NLS fitted sine wave
  tmp1= nls(y2~a+c*sin(x+b),start=list(a=4,b=.10,c=12))
  lines(x,predict(tmp1),col="red")

  #lm fitted sine wave
  tmp2=lm(y2~(sin(x)+cos(x)))
  lines(x,predict(tmp2),col="green")
  int= b1=coef(tmp2)[1]
  b1=coef(tmp2)[2]
  b2=coef(tmp2)[3]
  bb=atan(b2/b1)
  cc=sqrt(b1^2+b2^2)
  aa=int

  #glm fitted sine wave
  tmp3=glm(y2~(sin(x)+cos(x)),family="gaussian")
  lines(x,predict(tmp3),col="purple")
  int= b1=coef(tmp3)[1]
  b1=coef(tmp3)[2]
  b2=coef(tmp3)[3]
  bb=atan(b2/b1)
  cc=sqrt(b1^2+b2^2)
  aa=int