setwd('C:/Users/bryso/OneDrive - North Carolina State University/HWTeam (Fall 1)/LA HW1/')
load('LeukError.rdata') #don't need full path since wd specified
library(NLP)
library(tm)

pca <- prcomp((leuk[,0:5000])) #gets all var except 5001 which is leukemia type
plot(pca$sdev^2)
plot(pca)
summary(pca)

factor(leuk[,5001])
plot(pca$x[,1:2],main='PC1 and PC2 for Leuk Types', col=c("red","blue","green")[leuk[,5001]], pch=10) 
text(pca$x[,1], pca$x[,2],row.names(leuk)) #label observations
legend(x='bottomright', c('ALL-B','ALL-T','AML'), pch='o', col=c('red','blue', 'green'), pt.cex=1.5)
#AML plots some blue + green but not all, ALL-B plots some red, ALL-T plots nothing

#obs 22 is mislabeled as AML, but should be ALL-T

freq <- table(leuk[,5001])
(freq) # ALL-T = blue, AML = green, ALL-B = red
