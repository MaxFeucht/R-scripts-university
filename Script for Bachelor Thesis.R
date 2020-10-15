## Laden der nötigen Packages

{
  library("car")
  library("Rmisc")
  library("dplyr")
  library("ggplot2")
  library("NCmisc")
  library("SciViews")
  library("lmtest")
  library("userfriendlyscience")
  library("MASS")
  library("exactRankTests")
  library("Rttf2pt1")
  library("extrafontdb")
  library("extrafont")
  library("yacca")
  library("miscTools")
  library("plotrix")
  library("mosaicCore")
  library("ggstance")
  library("ggformula")
  library("httr")
  library("plotly")
  
}

#################
#               #
# AUFARBEITUNG  #
#  DATENSÄTZE   #
#               # 
#################  

{
  AuditSES <- read.csv("~/Documents/Uni/Bachelorarbeit/Datenanalyse/AUDIT/SES.csv",sep=";") #Einlesen des Datensatzes zu Alkoholkonsum und sozialer Erwünschtheit
  
  ##Aufbereitung der Daten zu Alkoholkonsum und sozialer Erwünschtheit
  AuditSES <- AuditSES[-c(58,59),-1] #Entfernen der letzten beiden Zeilen und der 1. Spalte 
  AuditSES <- AuditSES[,-(32:46)] #Entfernen der Spalten 32 - 46, da sie keine relevanten Daten enthalten
  
  #Umwandeln der Einträge von faktoriellen zu numerischen Werten
  AuditSES <- AuditSES %>% mutate_if(is.factor, as.character)
  AuditSES <- AuditSES %>% mutate_if(is.character, as.numeric)
  
  ## Einfügen von AUDIT- und SES-Gesamtscores
  AuditSES$AUDIT <- rowSums(AuditSES[,2:11]) #Aufsummieren der einzelnen Fragescores des AUDIT
  AuditSES$AUDIT <- AuditSES$AUDIT-10 #Aufgrund der Codierung in SoSci Survey (1-5) muss pro Frage ein Punkt abgezogen werden, um die ursprüngliche AUDIT-Codierung (0-4) zu erhalten
  AuditSES <- invertItems(AuditSES,items=c("SE01","SE04","SE06","SE07","SE11","SE15","SE17"))
  AuditSES$SES <- rowSums(AuditSES[,15:31]) #Aufsummieren der einzelnen Fragescores des SES-17
  AuditSES$SES <- AuditSES$SES-17 #Aufgrund der Codierung in SoSci Survey (1/2) muss pro Frage ein Punkt abgezogen werden, um die ursprüngliche SES-Codierung (0/1) zu erhalten
  
  ## Einfügen von 24h und 28d Gesamtscores
  AuditSES$AK28d <- AuditSES$AK28d-1 #-1, um Codierung von SoSci Survey auszugleichen
  AuditSES$AK28d2 <- AuditSES$AK28d2-1 #-1, um Codierung von SoSci Survey auszugleichen
  AuditSES$AK28dGesamt <- AuditSES$AK28d*AuditSES$AK28d2 #Gesamtalkoholkonsum 28 Tage ist die Anzahl der Gelegenheiten, zu denen Alkohol konsumiert wurde mulitipliziert mit der Menge konsumierten Alkohols zu einer solchen Gelegenheit
  AuditSES$AK24h <- AuditSES$AK24h-1 #-1, um Codierung von SoSci Survey auszugleichen
  
  ##Aufteilung in Yohimbin- und Placebo-Gruppe + 1d / 28d - Gruppe       15, 28, 31, 75, 79 
  AuditSES$Gruppe <- c("PLAC","PLAC","PLAC","PLAC","YOH","YOH","YOH","YOH","PLAC","PLAC","PLAC","PLAC","YOH","YOH","YOH","YOH","PLAC","PLAC","PLAC","PLAC","YOH","YOH","YOH","YOH","PLAC","PLAC","PLAC","PLAC","YOH","YOH","PLAC","PLAC","PLAC","PLAC","PLAC","PLAC","YOH","YOH","YOH","PLAC","PLAC","PLAC","PLAC","YOH","YOH","YOH","YOH","PLAC","PLAC","PLAC","PLAC","YOH","YOH","PLAC","PLAC","YOH","YOH")
  AuditSES$Termin2 <- c("1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","28d","1d","1d","1d","1d","28d","28d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","28d","28d","1d","1d","1d","28d","28d","28d")
}

##Einlesen und bereinigen der Gedächtnisdaten (Freier Abruf = Recall, Rekognitionsleistung an Tag 2 = Recognition)
Recall <- read.csv("~/Documents/Uni/Bachelorarbeit/Datenanalyse/FreierAbruf.csv",sep=";") #Einlesen der Daten des ersten Testtages
Recall <- Recall[1:65,] #Extrahieren der relevanten Spalten zum freien Abruf

{
  Recognition <- read.csv("~/Documents/Uni/Bachelorarbeit/Datenanalyse/Rekognition.csv",sep=";",dec=",") #Einlesen der Daten des zweiten Testtages
  
  Recognition$Hits <- rowSums(Recognition[,c(15,16,20,21)]) #Anzahl korrekt erinnerter Bilder
  Recognition$FA <- rowSums(Recognition[,c(5,6,10,11)]) #Anzahl falscher Alarme
  Recognition$Fail <- rowSums(Recognition[,c(12,13,17,18)])/60 #Anteil falsch erinnerter Bilder
  Recognition$CorrRej <- rowSums(Recognition[,c(2,3,7,8)])/60 #Anteil korrekt als neu erkannter Bilder
  Recognition$Misses <- rowSums(Recognition[,c(4,9,14,19)])/120 #Anteil nicht beantworteter Items
  Recognition$KorrRecog <- Recognition$Hits/60 ## Anteile der Hits und False Alarms
  Recognition$FalseAlarm <- Recognition$FA/60
  Recognition$KorrRecog[which(Recognition[,47] == 1)] <- 1-(1/(2*60))  ####NACH MACMILLAN (1985)
  Recognition$FalseAlarm[which(Recognition[,48] == 0)] <- 1/(2*60) ####NACH MACMILLAN (1985)
  Recognition$ZFA <- qnorm(Recognition$FalseAlarm)
  Recognition$ZKR <- qnorm(Recognition$KorrRecog)
  Recognition$Sensitivity <- Recognition$ZKR - Recognition$ZFA
  Recognition$Sensitivity <- round(Recognition$Sensitivity, digits=3)
  Recognition$KorrRecog <- round(Recognition$KorrRecog, digits=3)
  Recognition$FalseAlarm <- round(Recognition$FalseAlarm, digits=3)
}

##Demografie
{
  Demografie <- read.csv("~/Documents/Uni/Bachelorarbeit/Datenanalyse/Demografie.csv",sep=";") 
  Demografik <- Demografie[1:64,1:4]
  
}
## Erstellen eines Datensatzes mit allen für die Analyse wichtigen Daten
{
  Calc1 <- AuditSES[,c(1,14,32:36)]
  Calc2 <- Recall[,c(1,62)]
  Calc3 <- Recognition[,c(1,44:48,51)]
  Calc4 <- merge(Calc1, Calc2, by = "VP", all = TRUE, sort = TRUE,) #Zusammenführen der Datensätze, sortiert nach VP
  Calc5 <- merge(Calc3, Calc4, by = "VP", all = TRUE, sort = TRUE,) #Zusammenführen der Datensätze, sortiert nach VP
  Calculation <- merge(Calc5, Demografik, by = "VP", all = TRUE, sort = TRUE,) #Zusammenführen der Datensätze, sortiert nach VP
  Calculation <- na.omit(Calculation) #Auswerfen aller Fälle (VPs), bei denen Werte fehlen
}

#################
#               #
#  DESKRIPTIVE  #
#   STATISTIK   #
#               # 
#################  


##Ausreißer entfernen: Ab 3 SD über / unter Mittelwert
{
  #AUDIT:
  which(Calculation$AUDIT > (median(Calculation$AUDIT) + 3.5*mad(Calculation$AUDIT))) #keine Ausreißer
  which(Calculation$AUDIT < (median(Calculation$AUDIT) - 3.5*mad(Calculation$AUDIT))) #keine Ausreißer
  ##SES
  ##SES Score kann beliebgíg hoch sein (da nur sozial erwünschtes Verhalten, also niedriger SES interessiert), deshalb keine Überprüfung
  which(Calculation$SES < (median(Calculation$SES) - 3.5*mad(Calculation$SES))) #keine Ausreißer
  #FreeRecall
  which(Calculation$FreeRecall_Score > (median(Calculation$FreeRecall_Score) + 3.5*mad(Calculation$FreeRecall_Score))) #keine Ausreißer
  which(Calculation$FreeRecall_Score < (median(Calculation$FreeRecall_Score) - 3.5*mad(Calculation$FreeRecall_Score))) #keine Ausreißer
  
  D1 <- subset(Calculation,Calculation$Termin2=="1d")
  D28 <- subset(Calculation,Calculation$Termin2=="28d")
  #KorrRecog 24h
  which(D1$KorrRecog > (median(D1$KorrRecog) + 3.5*mad(D1$KorrRecog))) ##keine Ausreißer
  which(D1$KorrRecog < (median(D1$KorrRecog) - 3.5*mad(D1$KorrRecog))) ##keine Ausreißer
  #KorrRecog 28d
  which(D28$KorrRecog > (median(D28$KorrRecog) + 3.5*mad(D28$KorrRecog))) ##keine Ausreißer
  which(D28$KorrRecog < (median(D28$KorrRecog) - 3.5*mad(D28$KorrRecog))) ## 4 und 28
  median(D28$KorrRecog)
  mad(D28$KorrRecog)
  #False Alarms 24h
  which(D1$FalseAlarm > (median(D1$FalseAlarm) + 3.5*mad(D1$FalseAlarm))) ####Ausreißer entdeckt, Werte allerdings so gering, dass eine sachgemäße Bearbeitung der Aufgabe anzunehmen ist 
  which(D1$FalseAlarm < (median(D1$FalseAlarm) - 3.5*mad(D1$FalseAlarm))) ## keine Ausreißer
  #False Alarms 28d
  which(D28$FalseAlarm > (median(D28$FalseAlarm) + 3.5*mad(D28$FalseAlarm))) ##Ausreißer entdeckt, Werte allerdings so gering, dass eine sachgemäße Bearbeitung der Aufgabe anzunehmen ist 
  which(D28$FalseAlarm < (median(D28$FalseAlarm) - 3.5*mad(D28$FalseAlarm))) ## keine Ausreißer
  #Misses 24h
  which(D1$Misses > (median(D1$Misses) + 3.5*mad(D1$Misses))) ####Ausreißer entdeckt, Werte allerdings so gering, dass eine sachgemäße Bearbeitung der Aufgabe anzunehmen ist 
  which(D1$Misses < (median(D1$Misses) - 3.5*mad(D1$Misses))) ## keine Ausreißer
  #Misses 28d
  which(D28$Skips > (median(D28$Skips) + 3.5*mad(D28$Skips))) ####Ausreißer entdeckt, Werte allerdings so gering, dass eine sachgemäße Bearbeitung der Aufgabe anzunehmen ist 
  which(D28$Skips < (median(D28$Skips) - 3.5*mad(D28$Skips))) ## keine Ausreißer
  #Fail 24h
  which(D1$Fail > (median(D1$Fail) + 3.5*mad(D1$Fail))) ##keine Ausreißer
  which(D1$Fail < (median(D1$Fail) - 3.5*mad(D1$Fail))) ##keine Ausreißer
  #Fail 28d
  which(D28$Fail > (median(D28$Fail) + 3.5*mad(D28$Fail))) ## ## 4 und 28, Ausschluss beider VPs, da anzunehmen ist, dass Aufgabe nicht richtig bearbeitet wurde, Daten daher möglicherweise fehlerbehaftet
  which(D28$Fail < (median(D28$Fail) - 3.5*mad(D28$Fail))) ## keine Ausreißer
  median(D28$Fail)
  mad(D28$Fail)
  #Sensiitivity 24h
  which(D1$Sensitivity > (median(D1$Sensitivity) + 3.5*mad(D1$Sensitivity))) ##keine Ausreißer
  which(D1$Sensitivity < (median(D1$Sensitivity) - 3.5*mad(D1$Sensitivity))) ##keine Ausreißer
  ##Sensitivity 28 d
  which(D28$Sensitivity > (median(D28$Sensitivity) + 3.5*mad(D28$Sensitivity))) ##keine Ausreißer
  which(D28$Sensitivity < (median(D28$Sensitivity) - 3.5*mad(D28$Sensitivity))) ##keine Ausreißer
  median(D28$Sensitivity)
  mad(D28$Sensitivity)
  
  ########################################################################
  
  Calculation <- Calculation[-c(4,27),]  ##Entfernen der Ausreißer (VP 4 und 28)
}

##Entfernte VPs: 4, 28 Ausreißer; 29, 37, 50, 53 fehlen im Auditdatensatz

### Erstellen von Untergruppierungen

{
  Placebo <- subset(Calculation,Calculation$Gruppe=="PLAC") #Untergruppe, die nur Placebo-Fälle enthält
  Yohimbin <- subset(Calculation,Calculation$Gruppe=="YOH") #Untergruppe, die nur Yohimbin-Fälle enthält
  D1 <- subset(Calculation,Calculation$Termin2=="1d")
  D28 <- subset(Calculation,Calculation$Termin2=="28d")
  M <- subset(Calculation,Calculation$Geschlecht=="m")
  W <- subset(Calculation,Calculation$Geschlecht=="w")
  PlacD1 <- subset(Placebo,Placebo$Termin2=="1d")
  PlacD28 <- subset(Placebo,Placebo$Termin2=="28d")
  YohD1 <- subset(Yohimbin,Yohimbin$Termin2=="1d")
  YohD28 <- subset(Yohimbin,Yohimbin$Termin2=="28d")
  MD1 <- subset(D1,D1$Geschlecht=="m")
  WD1 <- subset(D1,D1$Geschlecht=="w")
  MD28 <- subset(D28,D28$Geschlecht=="m")
  WD28 <- subset(D28,D28$Geschlecht=="w")
}

##Gruppenunterschiede:
###Beschreibung der Stichprobe: sind die interessierenden Variablen in allen Gruppen gleich verteilt, hat Randomisierung gewirkt?

##Gruppenunterschide bezüglich AUDIT
leveneTest(AUDIT~Gruppe,Calculation) ##Varianzhomogenität
shapiro.test(Placebo$AUDIT)
shapiro.test(Yohimbin$AUDIT) ## Nicht Normalverteilt --> Kein normaler T-Test möglich
wilcox.exact(Calculation$AUDIT ~ Calculation$Gruppe) ## Mann-Whitney-U-Test nicht signifikant

##Gruppenunterschide bezüglich freiem Abruf
leveneTest(FreeRecall_Score~Gruppe,Calculation) ##Varianzhomogenität
shapiro.test(Placebo$FreeRecall_Score) ## Nicht Normalverteilt --> Kein normaler T-Test möglich
shapiro.test(Yohimbin$FreeRecall_Score) ##Normalverteilt
wilcox.exact(Calculation$FreeRecall_Score~ Calculation$Gruppe) ## Mann-Whitney-U-Test Nicht signifikant

##Gruppenunterschide bezüglich der Sensitivtät in 24h Gruppe
leveneTest(Sensitivity~Gruppe,D1) ##Varianzhomogenität
shapiro.test(PlacD1$Sensitivity) ##Normalverteilt
shapiro.test(YohD1$Sensitivity) ##Normalverteilt
t.test(D1$Sensitivity ~ D1$Gruppe,var.equal=T) ## Normaler T-Test Nicht signifikant 

##Gruppenunterschide bezüglich der Sensitivität in 28d Gruppe
leveneTest(Sensitivity~Gruppe,D28) ##Varianzhomogenität
shapiro.test(PlacD28$Sensitivity) ##Normalverteilt
shapiro.test(YohD28$Sensitivity) ##Normalverteilt
t.test(D28$Sensitivity ~ D28$Gruppe,var.equal=T) ## Normaler T-Test Nicht signifikant 

##Gruppenunterschide bezüglich SES
leveneTest(SES~Gruppe,Calculation) ##Varianzhomogenität
shapiro.test(Placebo$SES) ##Normalverteilt
shapiro.test(Yohimbin$SES) ##Normalverteilt
t.test(Calculation$SES~ Calculation$Gruppe, var.equal = T) ## Normaler T-Test Nicht signifikant

##########################################

##Geschlechterunterschide bezüglich des AUDIT 
leveneTest(AUDIT~Geschlecht,Calculation) ##Varianzhomogenität
shapiro.test(W$AUDIT) ## Normalverteilt
shapiro.test(M$AUDIT) ## Normalveteilt
t.test(Calculation$AUDIT ~ Calculation$Geschlecht,var.equal=TRUE) ## Mann-Whitney-U-Test Nicht signifikant 

##Geschlechterunterschiede bezüglich freiem Abruf
leveneTest(FreeRecall_Score~Geschlecht,Calculation) ##Varianzhomogenität
shapiro.test(W$FreeRecall_Score) ## Normalverteilt
shapiro.test(M$FreeRecall_Score) ## Normalverteilt
t.test(Calculation$FreeRecall_Score~ Calculation$Geschlecht, var.equal = T) ## Normaler T-Test Nicht signifikant

##Geschlechterunterschiede bezüglich der Sensitivtät in 24h Gruppe
leveneTest(Sensitivity~Geschlecht,D1) ##Varianzhomogenität
shapiro.test(W$Sensitivity) ##Normalverteilt
shapiro.test(M$Sensitivity) ##Normalverteilt
t.test(D1$Sensitivity ~ D1$Geschlecht,var.equal=T) ## Normaler T-Test Nicht signifikant 

##Geschlechterunterschiede bezüglich der Sensitivität in 28d Gruppe
leveneTest(Sensitivity~Geschlecht,D28) ##Varianzhomogenität
shapiro.test(W$Sensitivity) ##Normalverteilt
shapiro.test(M$Sensitivity) ##Normalverteilt
t.test(D28$Sensitivity ~ D28$Geschlecht,var.equal=T) ## Normaler T-Test Nicht signifikant 

##Gruppenunterschide bezüglich SES
leveneTest(SES~Geschlecht,Calculation) ##Varianzhomogenität
shapiro.test(W$SES) ## Normalverteilt
shapiro.test(M$SES) ##Nicht Normalverteilt
wilcox.exact(Calculation$SES ~ Calculation$Geschlecht) ## Mann-Whitney-U-Test Nicht signifikant 

###################

##Terminunterschiede AUDIT
leveneTest(AUDIT~Termin2,Calculation) ##Varianzhomogenität
shapiro.test(D1$AUDIT) ##Normalverteilt
shapiro.test(D28$AUDIT) ##Normalverteilt
t.test(Calculation$AUDIT ~ Calculation$Termin2,var.equal = TRUE) ## T-Test marignal signifikant

##Terminunterschiede Free Recall
leveneTest(FreeRecall_Score~Termin2,Calculation) ##Varianzhomogenität
shapiro.test(D1$FreeRecall_Score) ##Nicht Normalverteilt
shapiro.test(D28$FreeRecall_Score) ##Normalverteilt
wilcox.exact(Calculation$FreeRecall_Score ~ Calculation$Termin2) ## Mann-U-T nicht signifikant

##Terminunterschiede Sensitivity
leveneTest(Sensitivity~Termin2,Calculation) ##Varianzhomogenität
shapiro.test(D1$Sensitivity) ##Normalverteilt
shapiro.test(D28$Sensitivity) ##Normalverteilt
t.test(Calculation$Sensitivity ~ Calculation$Termin2,var.equal = TRUE) ## T-Test marignal signifikant

##Terminunterschiede SES
leveneTest(SES~Termin2,Calculation) ##Varianzhomogenität
shapiro.test(D1$SES) ##Normalverteilt
shapiro.test(D28$SES) ##Normalverteilt
t.test(Calculation$SES ~ Calculation$Termin2,var.equal = TRUE) ## T-Test nicht signifikant

######################################

##Deskriptive Demografie:
{
  a = c(length(Calculation$VP),  mean(Calculation$AUDIT), sd(Calculation$AUDIT), mean(Calculation$AK28dGesamt), sd(Calculation$AK28dGesamt), mean(Calculation$FreeRecall_Score), sd(Calculation$FreeRecall_Score), mean(Calculation$Sensitivity), sd(Calculation$Sensitivity), mean(Calculation$KorrRecog), sd(Calculation$KorrRecog),mean(Calculation$SES),sd(Calculation$SES))
  d = c(length(D1$VP), mean(D1$AUDIT), sd(D1$AUDIT), mean(D1$AK28dGesamt), sd(D1$AK28dGesamt), mean(D1$FreeRecall_Score), sd(D1$FreeRecall_Score), mean(D1$Sensitivity), sd(D1$Sensitivity), mean(D1$KorrRecog), sd(D1$KorrRecog),mean(D1$SES),sd(D1$SES))
  e = c(length(D28$VP), mean(D28$AUDIT), sd(D28$AUDIT), mean(D28$AK28dGesamt), sd(D28$AK28dGesamt), mean(D28$FreeRecall_Score), sd(D28$FreeRecall_Score), mean(D28$Sensitivity), sd(D28$Sensitivity), mean(D28$KorrRecog), sd(D28$KorrRecog),mean(D28$SES),sd(D28$SES))
  f = c(length(Placebo$VP), mean(Placebo$AUDIT), sd(Placebo$AUDIT), mean(Placebo$AK28dGesamt), sd(Placebo$AK28dGesamt), mean(Placebo$FreeRecall_Score), sd(Placebo$FreeRecall_Score), mean(Placebo$Sensitivity), sd(Placebo$Sensitivity), mean(Placebo$KorrRecog), sd(Placebo$KorrRecog),mean(Placebo$SES),sd(Placebo$SES))
  g = c(length(Yohimbin$VP), mean(Yohimbin$AUDIT), sd(Yohimbin$AUDIT), mean(Yohimbin$AK28dGesamt), sd(Yohimbin$AK28dGesamt), mean(Yohimbin$FreeRecall_Score), sd(Yohimbin$FreeRecall_Score), mean(Yohimbin$Sensitivity), sd(Yohimbin$Sensitivity), mean(Yohimbin$KorrRecog), sd(Yohimbin$KorrRecog),mean(Yohimbin$SES),sd(Yohimbin$SES))
  h = c(length(W$VP), mean(W$AUDIT), sd(W$AUDIT), mean(W$AK28dGesamt), sd(W$AK28dGesamt), mean(W$FreeRecall_Score), sd(W$FreeRecall_Score), mean(W$Sensitivity), sd(W$Sensitivity), mean(W$KorrRecog), sd(W$KorrRecog),mean(W$SES),sd(W$SES))
  i = c(length(M$VP), mean(M$AUDIT), sd(M$AUDIT), mean(M$AK28dGesamt), sd(M$AK28dGesamt), mean(M$FreeRecall_Score), sd(M$FreeRecall_Score), mean(M$Sensitivity), sd(M$Sensitivity), mean(M$KorrRecog), sd(M$KorrRecog),mean(M$SES),sd(M$SES))
  x = c("Total Members","AUDIT Mean","AUDIT SD", "AK 28 Tage Mean", "AK 28 Tage SD","Free Recall Mean","Free Recall SD","Sensitivity Mean","Sensitivity SD", "Hits Mean", "Hits SD","SES Mean","SES SD")
  y = c("Gesamt","24h","28d","Placebo","Yohimbin","Frauen","Männer")
  DemografieTable <- data.frame(a,d,e,f,g,h,i)
  row.names(DemografieTable) <- x
  colnames(DemografieTable) <- y
  DemografieTable <- round(DemografieTable,digits=3)

  mean(PlacD1$Sensitivity)
  mean(PlacD28$Sensitivity)
  mean(YohD1$Sensitivity)
  mean(YohD28$Sensitivity)
  
  sd(PlacD1$Sensitivity)
  sd(PlacD28$Sensitivity)
  sd(YohD1$Sensitivity)
  sd(YohD28$Sensitivity)
  
  mean(MD1$Sensitivity)
  mean(MD28$Sensitivity)
  mean(WD1$Sensitivity)
  mean(WD28$Sensitivity)
  
  sd(MD1$Sensitivity)
  sd(MD28$Sensitivity)
  sd(WD1$Sensitivity)
  sd(WD28$Sensitivity)
  
  mean(Calculation$Alter)
  sd(Calculation$Alter)
  
  range(Calculation$AUDIT)
  range(Calculation$FreeRecall_Score)
  range(D1$Sensitivity)
  range(D28$Sensitivity)
  range(Calculation$SES)
  range(D1$AUDIT)
  range(D28$AUDIT)
  }

##Zusammenhang von AUDIT-Werten und SES-17?
cor.test(Calculation$AUDIT,Calculation$SES,method='pearson')
plot(Calculation$AUDIT,Calculation$SES)

## Signifikanter Zusammenhang von AUDIT und SES-17!
which(Calculation$SES<1) ##Entfernen der Probanden mit SES = 0
Calculation1 <- Calculation[-8,]
cor.test(Calculation1$AUDIT,Calculation1$SES,method='pearson') ##Nicht mehr singifikant, aber fast
plot(Calculation1$AUDIT,Calculation1$SES)

##Zusammenhang nun nicht mehr signifikant




#################
#               #
#  KALKULATION  # 
#               # 
#################      

par(mfrow=c(2,2))


#################
##HAUPTANALYSEN##
#################


##FREE RECALL##

###############################################

L1 <- lm(FreeRecall_Score ~ AUDIT*Gruppe*Geschlecht,Calculation) # Regression: x1 = AUDIT, x2 = Gruppe , y = Anzahl erinnerter Bilder beim freien Abruf (RM1 = Regressionsmodell 1)
summary(L1) 
bptest(L1) ##Homoskedastizität gegeben
which(cooks.distance(L1)>.7) ##Keine Ausreißer

##############################################
Calculation$AUDITQ <- Calculation$AUDIT^2

Q1 <- lm(FreeRecall_Score ~ (AUDIT+AUDITQ)*Gruppe*Geschlecht,Calculation) # Regression: x1 = AUDIT, x2 = Gruppe , y = Anzahl erinnerter Bilder beim freien Abruf (RM1 = Regressionsmodell 1)
summary(Q1) 
bptest(Q1) ##Homoskedastizität gegeben
which(cooks.distance(Q1)>.7) ##Keine Ausreißer

## Keine Interaktionseffekte in der multiplen Regression

##Modellvergleich:

VGL1 <- anova(L1,Q1)
VGL1



#####REKOGNITION 24H#####

L2 <- lm(Sensitivity ~ AUDIT*Geschlecht*Gruppe, D1) # Regression: x1 = AUDIT, x2 = Gruppenzugehörigkeit , y = Sensitivity an Tag 2 (24h Gruppe)
summary(L2)
bptest(L2) ##Homoskedastizität
which(cooks.distance(L2)>.7) ##Keine Ausreißer

###############################################

D1$AUDITQ <- D1$AUDIT^2

Q2 <- lm(Sensitivity ~ (AUDIT+AUDITQ)*Gruppe*Geschlecht,D1) # Regression: x1 = AUDIT, x2 = Gruppe , y = Anzahl erinnerter Bilder beim freien Abruf (RM1 = Regressionsmodell 1)
summary(Q2) 
bptest(Q2) ##Homoskedastizität gegeben
which(cooks.distance(Q2)>.7) ##Keine Ausreißer

#Modellvergleich:

VGL2 <- anova(L2,Q2)
VGL2


#####RECOGNITION 28D####


L3 <- lm(Sensitivity ~ AUDIT*Gruppe*Geschlecht, D28) # Regression: x1 = AUDIT, x2 = Gruppenzugehörigkeit , y = Sensitivity an Tag 2 (28d Gruppe)
summary(L3) ##Gruppe marginal signifikanter Haupteffekt auf Sensitivity
bptest(L3) ##Homoskedastizität gegeben
which(cooks.distance(L3)>.7) ##Keine Ausreißer

###############################################
D28$AUDITQ <- D28$AUDIT^2

Q3 <- lm(Sensitivity ~ (AUDIT+AUDITQ)*Gruppe*Geschlecht,D28) # Regression: x1 = AUDIT, x2 = Gruppe , y = Anzahl erinnerter Bilder beim freien Abruf (RM1 = Regressionsmodell 1)
summary(Q3) 
bptest(Q3) ##Homoskedastizität gegeben
which(cooks.distance(Q3)>.7) ##Keine Ausreißer


#Modellvergleich:

VGL3 <- anova(L3,Q3)
VGL3


### Überprüfen der Interaktionseffekte von Modell L3 und Q3

MD1 <- subset(Calculation,D1$Geschlecht=="m")
WD1 <- subset(Calculation,D1$Geschlecht=="w")
WD1PLAC <- subset(WD1,WD1$Gruppe=="PLAC")
MD1PLAC <- subset(MD1,MD1$Gruppe=="PLAC")
WD1YOH <- subset(WD1,WD1$Gruppe=="YOH")
MD1YOH <- subset(MD1,MD1$Gruppe=="YOH")

mean(WD1PLAC$Sensitivity)
mean(MD1PLAC$Sensitivity)
mean(WD1YOH$Sensitivity)
mean(MD1YOH$Sensitivity)

MD28 <- subset(Calculation,D28$Geschlecht=="m")
WD28 <- subset(Calculation,D28$Geschlecht=="w")
WD28PLAC <- subset(WD28,WD28$Gruppe=="PLAC")
MD28PLAC <- subset(MD28,MD28$Gruppe=="PLAC")
WD28YOH <- subset(WD28,WD28$Gruppe=="YOH")
MD28YOH <- subset(MD28,MD28$Gruppe=="YOH")

mean(PlacD28$Sensitivity)
mean(YohD28$Sensitivity)
mean(WD28PLAC$Sensitivity)
mean(MD28PLAC$Sensitivity)
mean(WD28YOH$Sensitivity)
mean(MD28YOH$Sensitivity)
mean(MD28$Sensitivity)
mean(WD28$Sensitivity)


#################
#               #
#   GRAFIK      # 
#               # 
#################  
  
  GrafikL3Q3 <- ggplot(data=D28, aes(x=AUDIT, y=Sensitivity)) + 
                geom_point(size=1,shape=1) + 
                geom_smooth(method=lm, formula = y ~ I(0.17*x), aes(colour="#FF7777"), fill="#FF7777", se=TRUE, fullrange=TRUE, level=0.95) + 
                stat_smooth(method =lm, formula = y ~ I(0.36*x) + I(-.013*I(x^2)), aes(colour="#00CCCC"), fill= "#00CCCC", size = 1,se=TRUE, fullrange=TRUE, level=0.95)  + 
                scale_color_identity(name = "Modell",labels = c("Quadratisch", "Linear"), guide = "legend") + theme_classic() +  xlab("AUDIT-Score") + ylab("Sensitivitätsindex d'") + 
                theme(axis.title.y = element_text(size = rel(1.4), angle = 90,family="Times New Roman"), axis.title.x = element_text(size = rel(1.4), angle = 0, family="Times New Roman")) + 
                theme(plot.title = element_text(size = 14, face = "bold",family="Times New Roman"))

  GrafikL3Q3

  

