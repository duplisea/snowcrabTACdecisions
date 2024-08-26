x=1:40
x2=sin(x/4.2)+1
plot(x,x2,type="b")
x3=6^x2*10000*exp(rnorm(length(x2),0,0.2))
plot(x,x3,type="l")