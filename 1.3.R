#   Homework 1.3
#           Antonio Perea
#           Jorge Amoros
#           Jorge Sanz


coches <- read.csv('C:\\Users\\Jorge\\Downloads\\cars-PCA.csv', sep=";")


#1 Choose a quantitative variable and explore its distribution in terms of descriptive
#measures of center, dispersion, skewness and kurtosis. Is a normal model a plausible
#one for its distribution? If the answer is no, can you think of a transformation of 
#the variable that improves normality. Are there any outliers?

library(moments)
library(dplyr)


#1.1 Media (horsepower)
apply(coches[4],2, mean)

#1.2 Dispersión (horsepower)
apply(coches[4],2, sd)

#1.3 Simetria (horsepower)
apply(coches[4],2, skewness)

#1.4 Curtosis (horsepower)
apply(coches[4],2, kurtosis)

library(nortest)
library("car")
#1.5 Normalidad (horsepower). No es normal porque p < 0.05. 
# La transformación mejora la normalidad, pero no lo suficiente.

#summary(powerTransform(coches[4]))
qqPlot(coches[,4],dist="norm")
qqPlot(log(coches[,4]),dist="norm")

lillie.test(coches[4][,1])
lillie.test(log(coches[4][,1]))
#lillie.test(bcPower(coches[4][,1],0))

#1.6 Outliers (horsepower)
library("mvoutlier")
plot(coches$horsepower,pch=16,col=(pcout(coches[4])$wfinal01+2)) 



#2 Choose two quantitative variables and describe its joint bivariate distribution. Does it seem to be Normal? Are there any outliers?
joint = coches[,c(4,6)]

#2.1 Distribucion (horsepower, acceleration)
library(ggplot2)
ggplot(coches, aes(x=horsepower, y=acceleration))+geom_bar(stat="identity")



#2.2 Outliers (horsepower, acceleration)
plot(coches$horsepower,coches$acceleration,pch=16,col=(pcout(joint)$wfinal01+2)) 

#2.3 Normalidad (horsepower, acceleration). La distribución no es normal. 
# La transformación mejora la normalidad, pero no lo suficiente.

library("MVN")
mardiaTest(cbind(coches[,4],coches[,6]), qqplot=T)
mardiaTest(log(cbind(coches[,4],coches[,6])), qqplot=T)

#3 Choose a subset of 4 or 5 quantitative variables and explore linear relationships through:
#       1. R matrix of pairwise correlations.
#       2. Matrix of Partial correlation.
#       3. Coefficient of determination (function r2multv() we define in R).
#       4. The determinant of R (correlation matrix) as an overall measure of linear relationships.
#       5. An eigenanalysis of matrix R, looking for really small eigenvalues.


library(ppcor)
cuant = coches[,c(1,3,4,5,6)]

#3.1
cor(cuant)

#3.2
pcor(cuant)

#3.3
r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}
sort(r2multv(cuant))

#3.4
1-det(cor(cuant))^{1/4}

#3.5
eigen(cor(cuant))

