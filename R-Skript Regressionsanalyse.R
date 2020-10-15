##Skript zur 4. Portfolioaufgabe im Seminar II Forschungsmethoden, D: Regressionsanalyse (SoSe 20)
## von Max E. Feucht


###################################################

## Laden der nötigen Packages

library("ggplot2")
library("lme4")
library("car")
library("emulator")
library("pROC")

rm (list=ls()) ## Zum Bereinigen des Workspace

##Einlesen der Daten

Brunnenwechsel <- read.csv("~/Documents/Uni/Statistik/2. Mastersemester/Regressionsanalyse/Aufgaben/Aufgabe 4/brunnenwechsel.txt", sep = "")


###Überprüfen der Verteilung der UV

hist(Brunnenwechsel$ArsenMuG) ##rechtsschief 
hist(Brunnenwechsel$BildungJahre) ##rechtsschief
hist(Brunnenwechsel$EntfernungFt) ##rechtsschief


## Aufstellen des Modells

logmodell <- glm(Wechsel ~ ArsenMuG * BildungJahre + EntfernungFt, family = binomial (link = "logit"), data = Brunnenwechsel)
summary(logmodell)



###############################
###                         ###
### ÜBERPRÜFEN DER ANNAHMEN ###
###                         ###
###############################

## Linearität des Logit überprüfen

probabilities <- predict(logmodell, type = "response")
logit <-  log(probabilities/(1-probabilities))

par(mfrow=c(2,2))
ggplot(Brunnenwechsel, aes(x=BildungJahre, y=logit)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "loess", se = FALSE)

ggplot(Brunnenwechsel, aes(x=EntfernungFt, y=logit)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "loess", se = FALSE)

ggplot(Brunnenwechsel, aes(x=ArsenMuG, y=logit)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "loess", se = FALSE)


# --> Grafisches Überprüfen bestätigt Linearitätsannahme für alle Variablen


## Ausreißer identifizieren

which(rstandard(logmodell)> 3)  ## keine standardisierten Residuen über + 3 
which(rstandard(logmodell)< -3)  ## keine standardisierten Residuen über - 3 

which(cooks.distance(logmodell) > .025) ## keine Werte mit Cooks D über .5 (konservatives Maß)

plot(logmodell)


##Plausibilitätscheck

boxplot(Brunnenwechsel$ArsenMuG)
min(Brunnenwechsel$ArsenMuG)
max(Brunnenwechsel$ArsenMuG)
boxplot(Brunnenwechsel$EntfernungFt)
min(Brunnenwechsel$EntfernungFt)
max(Brunnenwechsel$EntfernungFt)
boxplot(Brunnenwechsel$BildungJahre)
max(Brunnenwechsel$BildungJahre)
min(Brunnenwechsel$BildungJahre)

##Daten scheinen plausibel zu sein



# --> keine einflussreichen Werte


## Multikollinearität überprüfen

vif(logmodell)

cor.test(Brunnenwechsel$ArsenMuG, Brunnenwechsel$EntfernungFt, method = "pearson")
cor.test(Brunnenwechsel$ArsenMuG, Brunnenwechsel$BildungJahre, method = "pearson")
cor.test(Brunnenwechsel$EntfernungFt, Brunnenwechsel$BildungJahre, method = "pearson")

## signifikante Korrelation von Arsenbelastung und Entfernung zur Wasserstelle


##Koeffizienten isolieren 
Koeffizienten <- glm(Wechsel ~ ArsenMuG * BildungJahre + EntfernungFt, family = binomial (link = "logit"), data = Brunnenwechsel)$coef
Koeffizienten <- as.numeric(Koeffizienten)

## Errechnen der Wahrscheinlichkeiten und Anhängen an Datensatz
XMatrix <- cbind(1, Brunnenwechsel$ArsenMuG, Brunnenwechsel$BildungJahre, Brunnenwechsel$EntfernungFt, Brunnenwechsel$ArsenMuG * Brunnenwechsel$BildungJahre)
Brunnenwechsel$Wkeit <- 1/(1+(exp(-(XMatrix%*%Koeffizienten))))



#################################
###                           ###
### Überprüfen der Modellgüte ###
###                           ###
#################################

##Transformation der Koeffizienten und Variablen Werte zu geschätzten Etas
Etapred <- XMatrix %*% Koeffizienten 

##Transformation der geschätzten Etas zu Pi
Pipred <- exp(Etapred)/(1 + exp(Etapred)) # Transformation zu π

##Wenn unter .5 dann 0 ("Nein"), wenn über .5 dann 1 ("Ja")
Ypred <- ifelse(Pipred < .5, 0, 1) # Transformation zu Punktprognose

##Anteil fehlerhafter Vorhersagen
sum((Ypred - Brunnenwechsel$Wechsel)!= 0)/length(Brunnenwechsel$Wechsel) 

## Ca 38% aller Vorhersagen mithilfe des Modells sind nach dem Vergleich mit den Originaldaten fehlerhaft

##ROC:
par(mfrow=c(2,2))
plot(roc(Brunnenwechsel, response = Wechsel, predictor = ArsenMuG, algorithm= 2))
plot(roc(Brunnenwechsel, response = Wechsel, predictor = EntfernungFt, algorithm= 2))
plot(roc(Brunnenwechsel, response = Wechsel, predictor = BildungJahre, algorithm= 2))


###############################
###                         ###
###  Grafische Darstellung  ###
###                         ###
###############################


Arsenplot <- ggplot(Brunnenwechsel, aes(x=ArsenMuG, y=Wechsel)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "glm",  method.args = list(family = "binomial"), fill= "#117CCC", size = 1, level=0.95, se = TRUE) +
  theme_classic() +  xlab("Arsen-Belastung in muG") + ylab("P (Y=1)") + 
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
  theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"))

Arsenplot


Entfernungsplot <- ggplot(Brunnenwechsel, aes(x=EntfernungFt, y=Wechsel)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "glm",  method.args = list(family = "binomial"), fill= "#117CCC", size = 1, level=0.95, se = TRUE) +
  theme_classic() +  xlab("Entfernung in ft") + ylab("P (Y=1)") + 
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
  theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"))

Entfernungsplot


Bildungsplot <- ggplot(Brunnenwechsel, aes(x=BildungJahre, y=Wechsel)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "glm",  method.args = list(family = "binomial"), fill= "#117CCC", size = 1, level=0.95, se = TRUE) +
  theme_classic() +  xlab("Bildung in Jahren") + ylab("P (Y=1)") + 
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
  theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"))

Bildungsplot


## Zum Visualisieren der Interaktion: Kategorisierung der Bildungsjahre

hist(Brunnenwechsel$BildungJahre)

Brunnenwechsel$Bildungskat[which(Brunnenwechsel$BildungJahre <=4)] <- "0-4 Jahre" 
Brunnenwechsel$Bildungskat[which(Brunnenwechsel$BildungJahre >4 & Brunnenwechsel$BildungJahre <=8)] <- "5-8 Jahre" 
Brunnenwechsel$Bildungskat[which(Brunnenwechsel$BildungJahre >8)] <- ">8 Jahre"

length(which(Brunnenwechsel$Bildungskat == "0-4 Jahre"))
length(which(Brunnenwechsel$Bildungskat == "5-8 Jahre"))
length(which(Brunnenwechsel$Bildungskat == ">8 Jahre"))


Interaktionsplot <- ggplot(Brunnenwechsel, aes(x=ArsenMuG, y=Wechsel, color = Bildungskat, fill= Bildungskat)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "glm",  method.args = list(family = "binomial"), size = 1, level=0.95, se = TRUE) +
  theme_classic() +  xlab("Arsenbelastung in muG") + ylab("P (Y=1)") + 
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
  theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"))

Interaktionsplot


## Ende ##

