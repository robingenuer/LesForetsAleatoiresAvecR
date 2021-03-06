#' # 3 Forêts aléatoires
#' 
#' ## 3.1 Principe général
#' 
#' ### 3.1.1 Instabilité d’un arbre
#' 
## ----rfTreeBoot1, fig.cap="Arbre de classification obtenu avec les valeurs par défaut de `rpart()` sur l'échantillon bootstrap `spamBoot1`", fig.width=8.5, fig.height=6----
set.seed(368910)
spamBoot1 <- spamApp[sample(1:nrow(spamApp), nrow(spamApp), replace = TRUE), ]
treeBoot1 <- rpart(type ~ ., data = spamBoot1)
plot(treeBoot1)
text(treeBoot1, xpd = TRUE)

#' 
## ----rfTreeBoot2, fig.cap="Arbre de classification obtenu avec les valeurs par défaut de `rpart()` sur l'échantillon bootstrap `spamBoot2`.", fig.width=8.5, fig.height=6----
set.seed(368915)
spamBoot2 <- spamApp[sample(1:nrow(spamApp), nrow(spamApp), replace = TRUE), ]
treeBoot2 <- rpart(type ~ ., data = spamBoot2)
plot(treeBoot2)
text(treeBoot2, xpd = TRUE)

#' 
## ----rfCompTreeBoot------------------------------------
mean(predict(treeBoot1, spamTest, type = "class") !=
     predict(treeBoot2, spamTest, type = "class"))

#' 
#' ### 3.1.2 D’un arbre à un ensemble : le Bagging
#' 
## ----rfLibrary-----------------------------------------
library(randomForest)

#' 
## ----rfBagging-----------------------------------------
bagging <- randomForest(type ~ ., data = spamApp, mtry = ncol(spamApp) - 1)
bagging

#' 
## ----rfBaggingErrors-----------------------------------
errTestBagging <- mean(predict(bagging, spamTest) != spamTest$type)
errEmpBagging <- mean(predict(bagging, spamApp) != spamApp$type)

#' 
#' ## 3.3 Le package `randomForest`
#' 
## ----rfRFDef-------------------------------------------
RFDef <- randomForest(type ~ ., data = spamApp)
RFDef

#' 
## ----rfRFDefAlter--------------------------------------
RFDef <- randomForest(spamApp[, -58], spamApp[, 58])

#' 
## ----rfRFRIErrorsPrint---------------------------------
errTestRFDef <- mean(predict(RFDef, spamTest) != spamTest$type)
errEmpRFDef <- mean(predict(RFDef, spamApp) != spamApp$type)

#' 
#' ## 3.5 Réglage des paramètres pour la prédiction
#' 
#' ### 3.5.1 Le nombre d'arbres `ntree`
#' 
## ----rfPlotRFDefEcho, fig.cap="Évolution de l’erreur OOB globale et pour chacune des classes, en fonction du nombre d’arbres, données `spam.`"----
plot(RFDef)

#' 
## ----rfRFDoTrace---------------------------------------
RFDoTrace <- randomForest(type~., data=spamApp, ntree = 250, do.trace=25)

#' 
#' ### 3.5.2 Le nombre de variables choisies à chaque noeud `mtry`
#' 
## ----rfTuneMtryEcho------------------------------------
nbvars <- 1:(ncol(spamApp) - 1)
oobsMtry <- sapply(nbvars, function(nbv) {
  RF <- randomForest(type~., spamApp, ntree = 250, mtry = nbv)
  return(RF$err.rate[RF$ntree, "OOB"])})

#' 
## ----rf10runs------------------------------------------
mean(replicate(n = 25,
  randomForest(type ~ ., spamApp, ntree = 250)$err.rate[250, "OOB"]))

#' 
## ----rfBagStump----------------------------------------
bagStump <- randomForest(type~., spamApp, ntree = 100,
                         mtry = ncol(spamApp) - 1, maxnodes = 2)

#' 
## ----rfBagStumpRes-------------------------------------
bagStumpbestvar <- table(bagStump$forest$bestvar[1,])
names(bagStumpbestvar) <- colnames(spamApp)[as.numeric(names(bagStumpbestvar))]
sort(bagStumpbestvar, decreasing = TRUE)

#' 
## ----rfRFStump-----------------------------------------
RFStump <- randomForest(type~., spamApp, ntree = 100, maxnodes = 2)
RFStumpbestvar <- table(RFStump$forest$bestvar[1,])
names(RFStumpbestvar) <- colnames(spamApp)[as.numeric(names(RFStumpbestvar))]
sort(RFStumpbestvar, decreasing = TRUE)

#' 
#' ## 3.6 Exemples
#' 
#' ### 3.6.1 Prédire la concentration d’ozone
#' 
## ----rfOzoneLoad---------------------------------------
library("randomForest")
data("Ozone", package = "mlbench")

#' 
## ----rfOzoneRFDef--------------------------------------
OzRFDef <- randomForest(V4 ~ ., Ozone, na.action = na.omit)

#' 
## ----rfOzoneRFDefRes, fig.cap="Evolution de l'erreur OOB en fonction du nombre d'abres, données `Ozone`."----
OzRFDef
plot(OzRFDef)

#' 
## ----rfOzoneMtryPlot, fig.cap="Evolution de l'erreur OOB en fonction de la valeur du paramètre `mtry`, données `Ozone`."----
plot(nbvars, oobsMtrys, type ="l", xlab = "mtry", ylab = "Erreur OOB")

#' 
## ----rfOzoneRFDefStrat---------------------------------
bins <- c(0, 10, 20, 40)
V4bin <- cut(Ozone$V4, bins, include.lowest = TRUE, right = FALSE)
OzoneBin <- data.frame(Ozone, V4bin)
OzRFDefStrat <- randomForest(V4 ~ . - V9 - V4bin, OzoneBin, strata = V4bin,
                             sampsize = 200, na.action = na.omit)
OzRFDefStrat

#' 
#' ### 3.6.2 Analyser des données génomiques
#' 
## ----rfVac18Load---------------------------------------
library(randomForest)
data("vac18", package = "mixOmics")

#' 
## ----rfVac18DataManage---------------------------------
geneExpr <- vac18$genes
stimu <- vac18$stimulation

#' 
## ----rfVac18RFpsur3------------------------------------
VacRFpsur3 <- randomForest(x = geneExpr, y = stimu, mtry = ncol(geneExpr)/3)
VacRFpsur3
plot(VacRFpsur3)

#' 
## ----rfVac18RFCompMtry---------------------------------
nFor <- 25
VacOOBsqrtp <- replicate(nFor,
  randomForest(geneExpr, stimu)$err.rate[500, "OOB"])
VacOOBpsur3 <- replicate(nFor,
  randomForest(geneExpr, stimu, mtry = ncol(geneExpr)/3)$err.rate[500, "OOB"])

#' 
#' ### 3.6.3 Analyser la pollution par les poussières
#' 
## ----rfPM10load----------------------------------------
library(randomForest)
data("jus", package = "VSURF")
jusComp <- na.omit(jus)

#' 
## ----rfPM10jusRF---------------------------------------
jusRF <- randomForest(PM10 ~ ., data = jusComp)

#' 
## ----rfPM10partialNO, fig.cap="Effet marginal de la variable NO, associé à une forêt aléatoire entraînée en la station JUS"----
partialPlot(jusRF, pred.data = jusComp, x.var = "NO",
            main = "Effet marginal - NO")

