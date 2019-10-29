library(pwr)

#Our goal: mass of particles absorbed ~ species * particle_type * dry_mass
numVariables = 2

#Cohen suggests f2 values of 0.02, 0.15, and 0.35 represent small, medium, and large effect sizes.
pwr.f2.test(u=numVariables, v=NULL, f2=0.02, sig.level = 0.05, power = 0.8) #low effect size
pwr.f2.test(u=numVariables, v=NULL, f2=0.15, sig.level = 0.05, power = 0.8) #medium effect size
pwr.f2.test(u=numVariables, v=NULL, f2=0.35, sig.level = 0.05, power = 0.8) #high effect size

#Results:

#If numVariables = 2
#sample size = v+u+1
#low effect size: sample size = 482 + 2 + 1 = 485
#med effect size: sample size = 64 + 2 + 1 = 67
#high effect size: sample size = 27 + 2 + 1 = 30

#i.e. if effect size is high, we need 35 observations, or 35 tanks of 3 bivalves each. Need 105 bivalves.
#if effect size is med, we need 76 * 3 = 228 bivalves in 76 tanks. 

#Since we have a sample size of 48, the power we have:

library(WebPower)

#Our goal: mass of particles absorbed ~ species * particle_type * dry_mass
numVariables = 2

#For medium effect size:
wp.regression(n = NULL, p1 = numVariables, p2 = 0, f2 = 0.15, alpha = 0.05, power = 0.8) 
#We need sample size of 67

#For large effect size:
wp.regression(n = NULL, p1 = numVariables, p2 = 0, f2 = 0.35, alpha = 0.05, power = 0.8) 
#We need sample size of 30

#To generate a power curve given a sequence of sample sizes:
res <- wp.regression(n = seq(10,100,10), p1 = numVariables, f2 = 0.15, alpha = 0.05, power = NULL)
res
plot(res)

#Since we have a sample size of 48, the power we have:
#For small effect size:
wp.regression(n = 48, p1 = numVariables, p2 = 0, f2 = 0.02, alpha = 0.05, power = NULL) 
#For medium effect size:
wp.regression(n = 48, p1 = numVariables, p2 = 0, f2 = 0.15, alpha = 0.05, power = NULL) 
#For large effect size:
wp.regression(n = 48, p1 = numVariables, p2 = 0, f2 = 0.35, alpha = 0.05, power = NULL) 
#We need sample size of 30

#power for small is 0.12, medium is 0.64 and large is 0.95
