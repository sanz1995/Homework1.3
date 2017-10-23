#   Homework 1.3
#           Antonio Perea
#           Jorge Amoros
#           Jorge Sanz


coches <- read.csv('C:\\Users\\Jorge\\Downloads\\cars-PCA.csv', sep=";")


#1
library(moments)
library(dplyr)


#1.1 Media
apply(coches[4],2, mean)

#1.2 Dispersión
apply(coches[4],2, sd)

#1.3 Simetria
apply(coches[4],2, skewness)

#1.4 Curtosis (concentraciÃ³n en un area)
apply(coches[4],2, kurtosis)

library(nortest)


#1.5 Normalidad. No es normal porque p < 0.05. 
# La transformación mejora la normalidad, pero no lo suficiente.

summary(powerTransform(coches[4]))
qqPlot(coches[,4])
qqPlot(log(coches[,4]))

lillie.test(coches[4][,1])
lillie.test(log(coches[4][,1]))
lillie.test(bcPower(coches[4][,1],0))

#1.6 Outliers
library("mvoutlier")
plot(coches$horsepower,pch=16,col=(pcout(coches[4])$wfinal01+2)) 



#2 
joint = coches[,c(4,6)]

#2.1 Distribucion
library(ggplot2)
ggplot(coches, aes(x=horsepower, y=acceleration))+geom_bar(stat="identity")



#2.2 Outliers
plot(coches$horsepower,coches$acceleration,pch=16,col=(pcout(joint)$wfinal01+2)) 

#2.3 Normalidad. La distribución no es normal. 
# La transformación mejora la normalidad, pero no lo suficiente.

summary(powerTransform(cbind(coches[,4],coches[,6])~1))

plot(cbind(water[,4],water[,6]))
trans1 = bcPower(cbind(water[,4],water[,6]), c(0,0))
plot(trans1)


library("MVN")
mardiaTest(cbind(coches[,4],coches[,6]), qqplot=T)
mardiaTest(trans1, qqplot=T)

#3
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
