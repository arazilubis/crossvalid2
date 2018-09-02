health=read.csv("http://personal.psu.edu/muh10/380/data/mlr07.csv")
names(health) = c("deathRate","drAvail","hospAvail","income","popDensity")
View(health)
fitLM=lm(deathRate~drAvail+hospAvail+income+popDensity,data=health) #actual data 
fitLM
summary(fitLM)
#cross validaton -- training data by subsetting health 
dim(health)
#genearl rule of thumb is leaving out 10% when subsetting 
trainInd = sample(seq(1,53), 40, replace = FALSE) #creating indexes 
traindat = health[trainInd,]
testDat= health[-trainInd,]
View(trainDat)
testDat
train
predLM = predict(fitLM, testDat) #fitLM is the model with real data 
predLM
plot(testDat$deathRate, predLM)
sum((testDat$deathRate-predLM)^2)
fitLM2 = lm(deathRate~popDensity, data = trainDat)
predLM2 = predict(fitLM2, testDat)
sum((testDat$deathRate-predLM2)^2)
M= 20 

#to store you need to remember to open vector to store it in. below you use RSS1[i] to store different indices 
RSS1 = rep(NA, M)
RSS2 = rep(NA, M)

for(i in 1:M){
  
  trainInd = sample(seq(1,53), 40, replace = FALSE)
  traindat = health[trainInd,]
  testDat= health[-trainInd,]
  
  fitLM=lm(deathRate~drAvail+hospAvail+income+popDensity,data=health)
  predLM = predict(fitLM, testDat)
  RSS1[i] = sum((testDat$deathRate-predLM)^2)
  
  fitLM2 = lm(deathRate~popDensity, data = traindat)
  predLM2 = predict(fitLM2, testDat)
  RSS2[i] = sum((testDat$deathRate-predLM2)^2)
  
}
RSS1
RSS2


hist(RSS1)
hist(RSS1, prob = TRUE)
lines(density(RSS1))
hist(RSS2, prob = TRUE)
lines(density(RSS2))
summary(RSS1)
summary(RSS2)
plot(density(RSS1), col = "red")
lines(density(RSS2), col = "blue") 
##choose lower one? less errors 


#bootstrap regression 


bootInd = sample(seq(1,53),53, replace = TRUE) #see 53 in both 
bootDat = health[bootInd,]

bootDat
fitboot= lm(deathRate~popDensity, data = bootDat)
summary(fitboot)
fitboot$coefficients
#replication function 
singleBS = function(){
  bootInd = sample(seq(1,53),53, replace = TRUE)
  bootDat = health[bootInd,]
  fitboot= lm(deathRate~popDensity, data = bootDat)
  return(fitboot$coefficients[2])
  
}
summary(fitboot)
repBS = replicate(1000, singleBS())
repBS
hist(repBS)
quantile(repBS, probs = c(.25, .75))
fitLM2$coefficients[2] + 1.96*sd(repBS)
