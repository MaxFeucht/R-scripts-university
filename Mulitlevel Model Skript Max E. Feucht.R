##Setting: Salary in Organizatons in relation to occupation domain (Analytics, Consulting, Marketing, HR), age, education duration, Extraversion score, Organization 

## Variables: Occupation domain, age, education duration, Extraversion score, Organization
## Level 1: Individual Level
## Level 2: Organizational Level (determined by OrgID)
### Fixed effects: Occupation domain, age, education duration, Extraversion score, Domain 
### Random effect: Intercept + Occupation domain | Organization 

## Calculating two models: One with just the intercept as random effect and one with domain as random effect 


library(mvtnorm)
library(lmerTest) 
library(nlme)
library(lme4)
library(lattice)
library(memisc)
library(fastDummies)
library(jtools)
library(sjstats)
library(ggplot2)

set.seed(127)


J <- 17 ##Data from 17 Organizations


#Determining Samplesizes
nj <- sample(c(43, 28, 33, 45, 34, 58, 73, 51, 38, 26), J, replace = TRUE) ##nj ist Zuweisung der Gruppengröße für jedes Element von J (Hier nur Gruppengrößen von 15,8,20 & 17)
nj
n <- matrix(0,sum(nj),J)
n

max.nj <- max(nj) #largest group

# Variance-Covariancematrix for X with S=D2 for all epsilon (--> Homoskedasticity)
rho <- 0 ## no correlation
sigma <- rep(2, max.nj) ## SD = 2
Sigma <- ## Variance-Covariancematrix 
  diag(1- rho, max.nj) + 
  matrix(rho, max.nj, max.nj) 
Sigma <- diag(sigma) %*% Sigma %*% diag(sigma) 
sigma

matrix(rho, max.nj, max.nj)

rm(sigma)

dim.d <- 4 

rho.om <- .445 # rho Omega:Correlation in der # Variance-Covariancematrix of random effects (Correlation between Organizations concerning the salary)
Omega <- diag(1-rho.om, dim.d) + matrix(rho.om, dim.d, dim.d) 

sigma.om <- rep(sqrt(2), dim.d) #Modellingvon Heteroskedasticity
Omega <- diag(sigma.om) %*% Omega %*% diag (sigma.om) ##Creating the Variance-Covariancematrix of random effects


##Erzeugung der Variablen:
Y <- c (); X <- c(); Z <- c();
OrgID <- c()
DomainGlobal = c()

j <- 1 

for(j in seq_len(J)){

  #Erzeugen der Variablen:
  Constant <- as.numeric(1)
  OID <- as.numeric(rep(1:67))
  OrgID <- c(OrgID,rep(OID[j],nj[j]))
  Age <- round(rnorm(nj[j],38,8.78))
  Edu <- round(10+rnorm(nj[j],5,3))
  {Extraversion <- (rnorm(nj[j],2.9))
  Extraversion <- pmax(0, Extraversion)
  Extraversion <- pmin(5, Extraversion)}
  
  ##Categorial Variables as Dummy-Variables

  Domain <- sample(c("Analytics","Consultancy","Marketing","HR"),nj[j],replace=TRUE)
  DomainGlobal <- c(DomainGlobal, Domain)
  
  # Construction of the X matrix including the constant for each group j
  X.j <- data.frame(Constant, Age, Edu, Extraversion, Domain)
  
  #Construction of Dummy-variables for Domain
  X.j <- dummy_cols(X.j, select_columns = c("Domain"), remove_first_dummy = TRUE)
  X.j <- X.j[,-5] ##Dropping Domain & Country Column
  
  X.j <- data.matrix(X.j) #Converting dataframe to numeric
  
  # Z-Matrix including Konstante for each group j
  
  Z.j <- X.j[,c(1,5:7)] #Modelling random variability of the Intercept und the Domains bewteen Organizations

  Z <- rbind(Z, Z.j)   
  X <- rbind(X, X.j) 
  
  head(X)
  eps <- t(rmvnorm(1, rep (0, nj[j]), Sigma[1:nj[j], 1:nj[j]] ))
  delta <- as.vector(rmvnorm(1, rep(0, dim.d), Omega))
  beta <- c (44000, 536, 1521, 365, 14323, 1210, -4399)
  Y <- rbind (Y, X.j %*% beta + Z.j %*% delta + eps)  ##Constructing the DV
} 

## Creating final dataframe
data <- data.frame (OrgID, Domain=DomainGlobal, y = Y, x1 = X[,2], x2 = X[,3], x3 = X[,4], z1 = Z[,2], z2 = Z[,3], z3 = Z[,4])
head(data)
max(data$y)


##descriptive Statistics

mean(data$x1)
sd(data$x1)
mean(data$x2)
sd(data$x2)
mean(data$x3)
sd(data$x3)

sum(data$z1) ## no of consultancy employees
sum(data$z2) ## no of HR employees
sum(data$z3) ## no of marketing employees
nrow(data) - sum(sum(data$z1), sum(data$z2), sum(data$z3)) ##Analytics employees


# Random intercept models + random coefficient model

n1Model <- lmer(y ~ x2 + x3 + z1 + z2 + z3 + (1| OrgID), data = data) #Null Model 1: Age
n2Model <- lmer(y ~ x1 + x3 + z1 + z2 + z3 + (1| OrgID), data = data) #Null Model 2: Education
n3Model <- lmer(y ~ x1 + x2 + z1 + z2 + z3 + (1| OrgID), data = data) #Null Model 3: Extraversion
n4Model <- lmer(y ~ x1 + x2 + x3 + z2 + z3 + (1| OrgID), data = data) #Null Model 4: Consultancy
n5Model <- lmer(y ~ x1 + x2 + x3 + z1 + z3 + (1| OrgID), data = data) #Null Model 5: HR
n6Model <- lmer(y ~ x1 + x2 + x3 + z1 + z2 + (1| OrgID), data = data) #Null Model 6: Marketing
iModel <- lmer(y ~ x1 + x2 + x3 + z1 + z2 + z3 + (1 | OrgID) , data = data) #Full Random intercept Model
f1Model <- lmer(y ~ x1 + x2 + x3 + z1 + z2 + z3 + (1 + z2 + z3 | OrgID) , data = data) #Null random coefficients model 1
f2Model <- lmer(y ~ x1 + x2 + x3 + z1 + z2 + z3 + (1 + z1 + z3 | OrgID) , data = data) #Null random coefficients model 2
f3Model <- lmer(y ~ x1 + x2 + x3 + z1 + z2 + z3 + (1 + z1 + z2 | OrgID) , data = data) #Null random coefficients model 3
fModel <- lmer(y ~ x1 + x2 + x3 + z1 + z2 + z3 + (1 + z1 + z2 + z3 | OrgID) , data = data) #Full random coefficients model

options(scipen = 999)

summary(iModel)
summary(fModel)

summ(iModel)
summ(fModel)

##Likelihood ratio tests 

{
  Comp1 <- anova(n1Model, iModel, refit=FALSE)
  Comp2 <- anova(n2Model, iModel, refit=FALSE)
  Comp3 <- anova(n3Model, iModel, refit=FALSE)
  Comp4 <- anova(n4Model, iModel, refit=FALSE)
  Comp5 <- anova(n5Model, iModel, refit=FALSE)
  Comp6 <- anova(n6Model, iModel, refit=FALSE)
  Comp7 <- anova(f1Model,fModel,refit = FALSE)
  Comp8 <- anova(f2Model,fModel,refit = FALSE)
  Comp9 <- anova(f3Model,fModel,refit = FALSE)
}

Comp1 #age
Comp2 #education
Comp3 #extraversion
Comp4 #consultancy fixed 
Comp5 #HR fixed
Comp6 #marketing fixed
Comp7 #consultancy random
Comp8 #HR random
Comp9 #marketing random

## Checking assumptions

plot(fModel) 
plot(y ~ predict(fModel), data = data)
qqnorm(residuals(fModel)) 
qqline(residuals(fModel), col= "blue")  
anova(lm(residuals(fModel)**2 ~ factor(data$OrgID))) ##Homogenous variance between groups can be assumed


##preparationns for Plotting

data$means = NA
data$sds = NA
data$Orgmeans = NA
data$Orgsds = NA
data$Orgints = NA

for(j in seq_len(J)){
  C <- subset(data,data$OrgID == j & data$Domain == "Consultancy")
  M <- subset(data,data$OrgID == j & data$Domain == "Marketing")
  H <- subset(data,data$OrgID == j & data$Domain == "HR")
  A <- subset(data,data$OrgID == j & data$Domain == "Analytics")
  Org <- subset(data,data$OrgID == j)
  
  data$means[which(data$OrgID == j & data$Domain == "Consultancy")] <- mean(C$y)
  data$means[which(data$OrgID == j & data$Domain == "Marketing")] <- mean(M$y)
  data$means[which(data$OrgID == j & data$Domain == "HR")] <- mean(H$y)
  data$means[which(data$OrgID == j & data$Domain == "Analytics")] <- mean(A$y)
  
  data$sds[which(data$OrgID == j & data$Domain == "Consultancy")] <- sd(C$y)
  data$sds[which(data$OrgID == j & data$Domain == "Marketing")] <- sd(M$y)
  data$sds[which(data$OrgID == j & data$Domain == "HR")] <- sd(H$y)
  data$sds[which(data$OrgID == j & data$Domain == "Analytics")] <- sd(A$y)
  
  data$Orgmeans[which(data$OrgID == j)] <- mean(Org$y)
  data$Orgsds[which(data$OrgID == j)] <- sd(Org$y)
  data$Orgints[which(data$OrgID == j)] <- coef(iModel)$OrgID[j,1]
}

ID = unique(data$OrgID)

## Plotting

GrafikiModel <- ggplot(data=data, aes(x=OrgID, y=Orgints)) + 
  geom_point(size=1, shape = 1, na.rm=TRUE, position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = mean(coef(iModel)$OrgID[,1]), size = 0.3, linetype="dashed") +
  theme_classic() +  xlab("Organization") + ylab("Salary") + 
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  scale_x_continuous("Organization",labels = as.character(ID), breaks = ID) +
  theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"));

GrafikiModel

GrafikfModel <- ggplot(data=data, aes(x=OrgID, y=means, group = Domain, color = Domain)) + 
  geom_point(size=2, aes(shape=Domain), na.rm=TRUE, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(x=OrgID,y = means, ymax=means + sds, ymin= means - sds), na.rm=TRUE, position=position_dodge(width=0.5),width=0.25, size = 0.25) +
  geom_point(y = data$Orgmeans, color = "black", shape = 4, size = 2) +
  geom_hline(yintercept = mean(data$y), size = 0.2, linetype="dashed") +
  theme_classic() +  xlab("Organization") + ylab("Salary") + 
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  scale_x_continuous("Organization",labels = as.character(ID), breaks = ID) +
  theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"));

GrafikfModel

