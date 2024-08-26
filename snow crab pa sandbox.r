shite= crab.gamma.f(1)
sh=shite[1]
ra=shite[2]
phenotype=95
#crab.dist=rgamma(10000,sh,ra)
#plot(density(crab.dist))
#abline (v=95)

par(mfcol=c(2,1))

curve(dgamma(x,sh,ra),10,140,xlab="Carapace width (mm)",ylab="Probability density")
  abline (v=phenotype,lty=2)
  prob.95= dgamma(95,sh,ra)
  abline (h=prob.95,lty=2)
curve(pgamma(x,sh,ra),10,140,xlab="Carapace width (mm)",ylab="Cumulative proportion",ylim=c(0,1))
  abline (v=phenotype,lty=2)
  prob.95= pgamma(95,sh,ra)
  abline (h=prob.95,lty=2)
  legend("left",legend=paste("prop>95 mm = ",round(1-prob.95,3)),bty="n",cex=.8)



# simulation
# create a two sex size based model
# create a sine wave recruitment signal
# create a random walk temperature signal
# make size at stage a function of temperature via growth rate
# seed the population with recruits, apply a temperature
# exploit those larger than 95 with an exploitation rate
# develop a Brecover and test to see if it is robust to temperature change
# adjust harvest as a function of number of crabs and temperature 