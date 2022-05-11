library(randomForest)
library(ggplot2)

# TP ACT6100
# Travail de :
#   Laurence Beaudet  BEAL25589600
#   Caleb Miller      MILC15089801
#   Edgar Lanoue      LANE03059909
#   
# Version du 10 mai 2022





#setwd("https://raw.githubusercontent.com/calmiller43/NHL-Salary/main/salaryNHL.csv")

#data <- read.csv("https://raw.githubusercontent.com/calmiller43/NHL-Salary/main/salaryNHL.csv", header = TRUE, sep = ",")

# setwd("C:/Users/edgar/OneDrive/Bureau/Ecole/UQAM/H22/ACT6100")
# data <- read.csv("salaryNHL201617.csv",  header = TRUE, sep = ",")

data <- read.csv("https://raw.githubusercontent.com//calmiller43//NHL-Salary//main//salaryNHL201617.csv", header = TRUE, sep = ",")
head(data)
str(data)
dim(data)

# Nettoyage des donnees

# Diviser le salaire par 1 000 000

str(data$Salary)

data$Salary <- gsub("\\$","", as.character(data$Salary))
data$Salary <- gsub(",","", as.character(data$Salary))

data$Salary <- as.numeric(data$Salary)
data$Salary <- data$Salary/1000000

# Creation de la variable age

dateMoitieSaison2016 <- as.Date("01/01/2017", "%m/%d/%Y") 
Born <- as.Date(data$Born, "%m/%d/%Y") 

age <- floor(difftime(dateMoitieSaison2016, Born, units= "weeks")/52.25)
data <- cbind(data, age)

# data$age <- ifelse(is.na(data$age), 2017 - data$DftYr + 18 , data$age)


# Positions

data$Position[which(data$Position != "D" & data$Position != "G"  )] <- "ATT"

# Si le joueur n'a pas ete repeche (NA), on le classe comme etant dans la ronde 8 (DftRd) et en 293 pour son rang de repechage

data$DftRd[is.na(data$DftRd)] <- 8

data$Ovrl[is.na(data$Ovrl)] <- max(data$Ovrl, na.rm = TRUE)+1


# Variable a retirer

str(data,list.len=ncol(data))


data <- subset(data, select = -c(Born, City, Pr.St, Cntry, Nat, DftYr,
                                 Cap.Hit, Status, NMC, CHIP, Injuries,
                                 MGL, X3rd, X2nd, X1st, First.Name, Last.Name,
                                 NHLid, Team))

# Retirer toutes les "#VALUE!" des donnees en les remplacant par NA (qu'on peut imputer)

data[ data == "#VALUE!"] <- NA

# Retirer les % des donnees caracteres

data$TOI. <- as.numeric(gsub("%","", as.character(data$TOI.)))/100
data$IPP. <- as.numeric(gsub("%","", as.character(data$IPP.)))/100
data$SH. <- as.numeric(gsub("%","", as.character(data$SH.)))/100
data$Pct. <- as.numeric(gsub("%","", as.character(data$Pct.)))/100
data$FO. <- as.numeric(gsub("%","", as.character(data$FO.)))/100
data$X.FOT <- as.numeric(gsub("%","", as.character(data$X.FOT)))/100
data$BLK. <- as.numeric(gsub("%","", as.character(data$BLK.)))/100


# Ne garder que les observations ou le salaire est connu
data <- data[!is.na(data$Salary),]

# Remplacer les NA dans quelques variables par des 0

data[is.na(data$SOS),]$SOS <- 0
data[is.na(data$SOG),]$SOG <- 0
data[is.na(data$SOGDG),]$SOGDG <- 0


# id des joueurs

id <- data$NHLid

# Mettre en factors et en numerics

Hand <- data$Hand
Position <- data$Position

data <- sapply(data, as.numeric)
data <- as.data.frame(data)

data$Hand <- as.factor(Hand)
data$Position <- as.factor(Position)

# id en tant que row name

row.names(data) <- id

# Statistiques sur le salaire

meanSalary <- mean(data$Salary)
medianSalary <- median(data$Salary)

#Figure 1
hist(data$Salary)

#Figure 2
ggplot(data=data,aes(x=PTS, y=Salary)) + geom_point(aes(x=PTS, y=Salary)) + geom_smooth(method='lm', formula= y~x)

#Figure 3
ggplot(data=data,aes(x=age, y=Salary)) + geom_point(aes(x=age, y=Salary)) + geom_smooth(method='lm', formula= y~x)



# Création d'ensembles de données d'entraînements et de test (aléatoire)

set.seed(123)
train <- sample(dim(data)[1], dim(data)[1] - 178)

dataTrain <- data[train,]
dataTest <- data[-train,]


# Modèle Random Forest

# Imputation des valeurs manquantes
dataTestImpute <- rfImpute(Salary ~ ., data = dataTest, iter=6)
dataImpute <- rfImpute(Salary ~ ., data = dataTrain, iter=6)
str(dataImpute)

# Modèle Random Forest
salary.rf <- randomForest(Salary ~ ., data = dataImpute, ntree = 1000)


mseData <- data.frame(
  Trees = rep(1:length(salary.rf$mse),times=2),
  Type=rep(c("mse", "% variance explained"), each=length(salary.rf$mse)),
  Error = c(mse = salary.rf$mse, rsq = salary.rf$rsq))

#Figure 4
ggplot(data=mseData, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# Prenons ntree = 300

mse <- vector(length = 31)

for(i in 33:63) {
  temp.model <- randomForest(Salary ~ ., data=dataImpute, mtry=i, ntree=300)
  mse[i-32] <- temp.model$mse[length(temp.model$mse)]
  print(i)
}
mseValues <- data.frame(c(33:63), mse)
min(mseValues) 

for(i in 41:71) {
  temp.model <- randomForest(Salary ~ ., data=dataImpute, mtry=i, ntree=300)
  mse[i-40] <- temp.model$mse[length(temp.model$mse)]
  print(i)
}

mseValues <- data.frame(c(41:71), mse)
min(mseValues) 

#Gardons un mtry de 56 et un ntree de 300
salary.rf <- randomForest(Salary ~ ., data = dataImpute, ntree = 300, mtry = 56)

#Évaluation du modèle

  # Imputation de l'ensemble test

salaryPred <- predict(salary.rf, dataTestImpute)

error <- dataTest$Salary - salaryPred
mean(abs(error))

#prediction <- data.frame(salaryPred, dataTest$Salary, abs(error))
#write.csv(prediction, "predRF.csv", row.names = TRUE)






##########Modele 2
#commencons par regarder un histogramme

P=dataTrain$Salary

hist(P,probability=TRUE, main="Salaire",xlab="")
#La grande majorit? des salaires sont sous la barre du million
#commencons pas faire une r?gression lin?aire avec toutes les variables.

reg1 = lm(dataTrain$Salary~ . , data=dataTrain)
summary(reg1)

# Le modele est beaucoup trop complexe, essayons donc de reduire le plus possible
#possible le nb de varaiables tout en preservant le meilleur adjusted r squared. 

#premi?rement, enlevons les variables avec NA (Game,iPenDf,Wide,iDS,iHDf,GA,DSF,DSA,Grit,)

reg2 = lm(dataTrain$Salary~ .-Game-iPenDf-Wide-iDS-iHDf-GA-DSF-DSA-Grit , data=dataTrain, na.action=na.omit)
summary(reg2)
colnames(data)[apply(data, 2, anyNA)] #retirons ces colonnes

reg2 = lm(dataTrain$Salary~ .-Wide-iPenDf-Game-Grit-TOI.1-TOI.-IPP.-SH.-SV.-PDO-F.60-A.60-Diff.60-iCF-iFF-iSF-ixG-iSCF-iRB-iRS-iDS-sDist.1-Pass-iHF.1-iHA-iHDf-iGVA.1-iTKA.1-iBLK.1-BLK.-iFOW.1-iFOL.1-X.FOT-iPENT-iPEND-CF-CA-FF-FA-SF-SA-xGF-xGA-SCF-SCA-GF-GA-RBF-RBA-RSF-RSA-FOW-FOL-HF-HA-GVA-TKA-PENT-PEND-SOS-SOG-SOGDG-OTOI-Pace-GS-GS.G, data=dataTrain, na.action=na.omit)
summary(reg2)
AIC(reg2)
BIC(reg2)
summary(reg2)$adj.r.squared

#on remarque qu'on omet beaucoup trop de donn?es. Nous devons donc remplacer les NA par des 0.

dataTrain[is.na(dataTrain)] = 0
Model2 = lm(dataTrain$Salary~ .-Wide-iPenDf-Game-Grit-TOI.1-TOI.-IPP.-SH.-SV.-PDO-F.60-A.60-Diff.60-iCF-iFF-iSF-ixG-iSCF-iRB-iRS-iDS-sDist.1-Pass-iHF.1-iHA-iHDf-iGVA.1-iTKA.1-iBLK.1-BLK.-iFOW.1-iFOL.1-X.FOT-iPENT-iPEND-CF-CA-FF-FA-SF-SA-xGF-xGA-SCF-SCA-GF-GA-RBF-RBA-RSF-RSA-FOW-FOL-HF-HA-GVA-TKA-PENT-PEND-SOS-SOG-SOGDG-OTOI-Pace-GS-GS.G, data=dataTrain, na.action=na.omit)
summary(Model2)
AIC(Model2)
BIC(Model2)
summary(Model2)$adj.r.squared

with(dataTrain,plot(Salary,predict(Model2)))
abline(a=0,b=1,lty=2,col="yellow")
prevision = data.frame(yhat_1 = predict(reg2, newdata = dataTest),yhat_2 = dataTest$Salary)
colnames(prevision) = c("Modele 2","Vraies valeurs")
prevision$Diff=abs(prevision$`Modele 2`-prevision$`Vraies valeurs`)
mean(prevision$Diff)
str(prevision)

#write.csv(prevision, "predLM.csv", row.names = TRUE)

#Ce n'est pas le meilleur mod?le. Quand on regarde les pr?dictions on remarque que certaines valeurs pr?dites sont n?gatives,


# Comparaison des modeles

prediction <- data.frame(salaryPred, prevision$`Modele 2`, dataTest$Salary)

#write.csv(prediction, "predictions.csv", row.names = TRUE)





















