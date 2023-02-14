# C3EX1 (Bourbonnais)
C3EX1=read.table("https://raw.githubusercontent.com/Hamrita/MQ/main/Data/C3EX1.csv",
               h=T,sep=";")
reg1=lm(Y~., data=C3EX1)
ss1=summary(reg1)
ss1$coefficients
confint(reg1)
##############
# calcul à la main
X=as.matrix(cbind(1,C3EX1[,-1])); Y=C3EX1[,1]
XX=crossprod(X)  # X'X
XX
XX_1=solve(XX)  # (X'X)^(-1)
XX_1

XY=crossprod(X,Y)
beta=XX_1%*%XY
beta

# tableau 2 p 62 (résidus)

ee=ss1$residuals
e2=ee^2
y.hat=reg1$fitted.values
tab1=cbind(y=Y,"Y chapeau"=y.hat, e=ee, e2=e2)
round(tab1,2)

# matrice des var-cov

vcov(reg1)

# ou encore

n=NROW(X); k=NCOL(X)

sig2=sum(e2)/(n-k)
sig2*XX_1

# R2 et R2 ajustée

ss1$r.squared
ss1$adj.r.squared

# Tests statistiques

# a1=1 et a2=-0.5  (p 71)

car::linearHypothesis(reg1, c("X1=1", "X2=-0.5"))

# résidus standardisés

H=X%*%solve(XX)%*%t(X)
hi=diag(H)
es=as.vector(ee/(ss1$sigma*sqrt(1-hi)))

# tableau 3 p 72

n=NROW(X); k=NCOL(X)-1
rstud=es*sqrt((n-k-2)/(n-k-1-es^2))

DIFFUS=rstud*sqrt(hi/(1-hi))

tab3=cbind("Résidus"=ee,h=hi,es=es,RSTUDENT=rstud, DIFFUS=DIFFUS)

round(tab3,3)

# Test de chow p 77

# Estimation du mod1: 1...7
lm1=lm(Y~., data=C3EX1[1:7,])
(ss11=summary(lm1))
SCR1=sum(ss11$residuals^2)
# Estimation du mod2: 8...14
lm2=lm(Y~., data=C3EX1[8:14,])
(ss12=summary(lm2))
SCR2=sum(ss12$residuals^2)

SCR=sum(ss1$residuals^2)

Fob=((SCR-(SCR1+SCR2))/4)/((SCR1+SCR2)/6)
Fob

# ou encore, à l'aide du pkge "strucchange"
# install.packages(strucchange)

library(strucchange)

sctest(Y~. , data=C3EX1, point=7,type="Chow")

# Test: a1=1 et a2=a3  (p 78)

linearHypothesis(reg1, c("X1=1", "X2=X3"))

# Exemple 3.1 (Wooldridge p 73): data gpa1

data(gpa1, package = "wooldridge")
head(gpa1,3)
mod=lm(colGPA ~ hsGPA + ACT, data=gpa1)
summary(mod)

aov(mod)

# Exemple 4.1 (Wooldridge p 120): data wage1

data(gpa1, package = "wooldridge")

mod1=lm(log(wage) ~ educ +  exper + tenure , data=wage1)

summary(mod1)

