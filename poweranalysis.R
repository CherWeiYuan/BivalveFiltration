install.packages("pwr")
library(pwr)

#Our goal: mass of particles absorbed ~ species * particle_type * dry_mass
numVariables = 3

#Cohen suggests f2 values of 0.02, 0.15, and 0.35 represent small, medium, and large effect sizes.
pwr.f2.test(u=numVariables, v=NULL, f2=0.02, sig.level = 0.05, power = 0.8) #low effect size
pwr.f2.test(u=numVariables, v=NULL, f2=0.15, sig.level = 0.05, power = 0.8) #medium effect size
pwr.f2.test(u=numVariables, v=NULL, f2=0.35, sig.level = 0.05, power = 0.8) #high effect size

#Results:

#If numVariables = 3
#sample size = v+u+1
#low effect size: sample size = 546 + 3 + 1 = 550
#med effect size: sample size = 72 + 3 + 1 = 76
#high effect size: sample size = 31 + 3 + 1 = 35

#i.e. if effect size is high, we need 35 observations, or 35 tanks of 3 bivalves each. Need 105 bivalves.
#if effect size is med, we need 76 * 3 = 228 bivalves in 76 tanks. 

install.packages("WebPower")
library(WebPower)

#Our goal: mass of particles absorbed ~ species * particle_type * dry_mass
numVariables = 3

#For medium effect size:
wp.regression(n = NULL, p1 = numVariables, p2 = 0, f2 = 0.15, alpha = 0.05, power = 0.8) 
#We need sample size of 85

#To generate a power curve given a sequence of sample sizes:
res <- wp.regression(n = seq(10,100,10), p1 = numVariables, f2 = 0.15, alpha = 0.05, power = NULL)
res
plot(res)
