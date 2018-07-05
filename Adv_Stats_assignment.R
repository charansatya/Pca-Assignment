setwd("C:\\BABI\\Statistics\\Advanced Stats\\Assignment")
library("factoextra")
library("psych")
data=read.csv("cereal.csv")
attach(data)
new_data=data[,-1]
new_data[new_data==6] <- 5

cov(new_data)

eigen(cov(new_data))$values

cortest.bartlett(new_data,n=235,diag = TRUE)
KMO(new_data)



#Kaiser Criteria

eigen(cov(new_data))$values

pca=princomp(new_data,cor = TRUE,scores = FALSE)
summary(pca)

fviz_eig(pca,ncp=25) # consider 10 components


fa6=factanal(new_data,factors = 6,rotation = "varimax")
fa6

fa8=factanal(new_data,factors = 8,rotation = "varimax")
fa8

new_data=new_data[-c(5,21,24)]
KMO(new_data)

numFactors <- fa.parallel(new_data, fm="ml", fa="fa")
sum(numFactors$fa.values>1.0) 
pca=princomp(new_data,cor = TRUE,scores = FALSE)
summary(pca)

fviz_eig(pca,ncp=25)

eigen(cov(new_data))$values

fa5=factanal(new_data,factors = 5,rotation = "varimax")
fa5


colnames(fa5$loadings) <- c("Health","Taste","Family","Texture","Experience")
fa5$loadings

print(loadings(fa5), digits = 2, cutoff = 0.3, sort = TRUE)

#reliability of factors

factor1 <- c(1,2,3,6,7,12,17,20,22)
factor2 <- c(4,5,14,18)
factor3 <- c(6,8,9,13,16)
factor4 <- c(8,10,15,16,21)
factor5 <- c(1,6,7)

factor1alpha <- psych::alpha(new_data[,factor1], check.keys = TRUE)
factor2alpha <- psych::alpha(new_data[,factor2], check.keys = TRUE)
factor3alpha <- psych::alpha(new_data[,factor3], check.keys = TRUE)
factor4alpha <- psych::alpha(new_data[,factor4], check.keys = TRUE)
factor5alpha <- psych::alpha(new_data[,factor5], check.keys = TRUE)


factor1alpha$total$raw_alpha
factor2alpha$total$raw_alpha
factor3alpha$total$raw_alpha
factor4alpha$total$raw_alpha
factor5alpha$total$raw_alpha

new_data$Cerals <- c(data[,1])
data$Cereals
cbind(new_data,data$Cereals)
new_data=new_data[,-new_data$Cerals]
new_data$factor1Score <- apply(new_data[,factor1],1,mean)
new_data$factor2Score <- apply(new_data[,factor2],1,mean)
new_data$factor3Score <- apply(new_data[,factor3],1,mean)
new_data$factor4Score <- apply(new_data[,factor4],1,mean)
new_data$factor5Score <- apply(new_data[,factor5],1,mean)
colnames(new_data)[23:27] <- c("health","taste","family","texture","experience")
new_data$Health_f <- NA
new_data$Taste_f <- NA
new_data$Family_f <-NA
new_data$Texture_f <-NA
new_data$Experience_f <-NA
aggregateCereal<-aggregate(new_data[,23:27],  list(new_data[,"Cerals"]), mean)
format(aggregateCereal, digits = 2)
colnames(new_data)[23:27]

