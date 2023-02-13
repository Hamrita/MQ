######################################
#    Régression linéaire simple
######################################

## Replications

# Jeffrey M. Wooldridge (2011): Introductory econometrics: 
#             A modern approach - 5ième édition

# Exemple 2.2 (p 24): wage = beta0+beta1*educ +epsilon

# installer la library "wooldridge" contenant les données
# install.packages(wooldridge)
# puis charger la library

library(wooldridge)
data("wage1")

# afficher les nons des colonnes de la base de données "wage1"

colnames(wage1)

# régression linéaire simple: wage=f(educ)

reg1=lm(wage ~ educ, data=wage1)
reg1

# intervalles de confiances des paramètres

confint(reg1)


ss1=summary(reg1)

names(ss1)

ss1$coefficients # estimation

# Graphique

plot(wage1$educ,wage1$wage)
abline(coef(reg1),lwd=2, col=2)

# les résidus
u=ss1$residuals

# Vérification des propriétés algébriques

# sum(u)=0
round(sum(u),10) # arrondissement à 10^-10 prés

# sum(x*u)=0

round(sum(wage1$educ*u),10)

# sum(u*(y.hate-y.bar))=0

round(sum(u*(reg1$fitted.values-mean(wage1$wage)) ),10)

# SCR= sum(u^2)

SCR=sum(u^2)

SCR

# sigma^2= SCR/(n-2)

n= NROW(wage1)

sig2=SCR/(n-2); sig2

ss1$sigma^2

# Tableau d'analyse de la variance

aov(reg1)

# tests statistiques

# installer la librairie "car" puis la charger
# install.packageges(car)

library(car)

# tester: beta_0+beta_1=0.5

linearHypothesis(reg1, "(Intercept)+educ=0.5")

############################################

# Greene, W.H. (2003). Econometric Analysis, 
#  8th edition. Upper Saddle River, NJ: Prentice Hall

# Exemple 2.1 (p 14): fonction de consommation

# installer la librairie "AER" puis la charger
# install.packages(AER)

library(AER)

data("USConsump1950")
US=as.data.frame(USConsump1950)
reg2= lm(expenditure ~ income, data = US)
ss2=summary(reg2)
ss2$coefficients

plot(US$income, US$expenditure)
abline(coef(reg2), lwd=2, col=2)
confint(reg2)
