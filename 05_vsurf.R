
#' # 5 Sélection de variables
#' 
#' ## 5.4 Le package VSURF

## ----vsurfLoad-----------------------------------------------------------
library(VSURF)
data("toys")

## ----vsurfToysglob-------------------------------------------
set.seed(3101318)
vsurfToys <- VSURF(toys$x, toys$y, mtry = 100)

## ----vsurfToysSumm-------------------------------------------------------
summary(vsurfToys)

## ----vsurfToysPlot, fig.cap="Illustration of the results of VSURF applied on toys data"----
plot(vsurfToys)

## ----vsurfThres----------------------------------------------
set.seed(3101318)
vsurfThresToys <- VSURF_thres(toys$x, toys$y, mtry = 100)

## ----vsurfThresVarsel----------------------------------------------------
vsurfThresToys$varselect.thres

## ----vsurfToysPlotThres, fig.cap="(ref:titrevsurfToysPlotThres)"---------
plot(vsurfToys, step = "thres", imp.mean = FALSE, ylim = c(0, 2e-4))

## ----vsurfInterp---------------------------------------------
vsurfInterpToys <- VSURF_interp(toys$x, toys$y, vars = vsurfThresToys$varselect.thres)

## ----vsurfInterpRes------------------------------------------------------
vsurfInterpToys$varselect.interp

## ----vsurfPred-----------------------------------------------
vsurfPredToys <- VSURF_pred(toys$x, toys$y, err.interp = vsurfInterpToys$err.interp, varselect.interp = vsurfInterpToys$varselect.interp)

## ----vsurfPredRes--------------------------------------------------------
vsurfPredToys$varselect.pred

## ----vsurfSpamCode-------------------------------------------
set.seed(923321, kind = "L'Ecuyer-CMRG")
vsurfSpam <- VSURF(type~., spamApp, parallel = TRUE, ncores = 3,
                   clusterType = "FORK")

## ----vsurfSpamRes, fig.cap="Résultats de `VSURF`, données `spam`."-------
summary(vsurfSpam)
plot(vsurfSpam)
colnames(spamApp[vsurfSpam$varselect.interp])
colnames(spamApp[vsurfSpam$varselect.pred])

## ----vsurfSpamContinued--------------------------------------------------
vsurfSpam$mean.jump

## ----vsurfSpamTuneCode---------------------------------------
set.seed(945834)
vsurfSpamPred <- VSURF_pred(type~., spamApp, nmj = 15,
  err.interp = vsurfSpam$err.interp,
  varselect.interp = vsurfSpam$varselect.interp)

## ----vsurfSpamTuneRes----------------------------------------------------
colnames(spamApp[vsurfSpamPred$varselect.pred])

#' ## 5.5 Réglage des paramètres pour la sélection

## ----vsurfStump----------------------------------------------
vsurfToysStump <- VSURF(toys$x, toys$y, mtry = 100, maxnodes = 2)
summary(vsurfToysStump)
vsurfToysStump$varselect.interp
vsurfToysStump$varselect.pred

## ----vsurfThresTuned-----------------------------------------------------
vsurfThresToysTuned <- tune(vsurfThresToys, nmin = 3)
vsurfThresToysTuned$varselect.thres

## ----vsurfInterTuned-----------------------------------------------------
vsurfInterpToysTuned <- tune(vsurfInterpToys, nsd = 5)
vsurfInterpToysTuned$varselect.interp

## ----vsurfPredTuned------------------------------------------------------
vsurfPredToysTuned <- VSURF_pred(toys$x, toys$y,
    err.interp = vsurfInterpToys$err.interp, 
    varselect.interp = vsurfInterpToys$varselect.interp,
    nmj = 3)
vsurfPredToysTuned$varselect.pred

#' ## 5.6 Exemples
#' 
#' ### 5.6.1 Prédire la concentration d’ozone

## ----vsurfOzLoad---------------------------------------------------------
library(VSURF)
data("Ozone", package = "mlbench")

## ----vsurfOzCode---------------------------------------------
set.seed(303601)
OzVSURF <- VSURF(V4 ~ ., data = Ozone, na.action = na.omit)

## ----vsurfOzRes, fig.cap="Résultats de `VSURF`, données `Ozone`."--------
summary(OzVSURF)
plot(OzVSURF, var.names = TRUE)

## ----vsurfOzVarThres-----------------------------------------------------
number <- c(1:3, 5:13)
number[OzVSURF$varselect.thres]

## ----vsurfOzVarInterp----------------------------------------------------
number[OzVSURF$varselect.interp]

## ----vsurfOzVarPred------------------------------------------------------
number[OzVSURF$varselect.pred]

#' ### 5.6.2 Analyser des données génomiques

## ----vsurfVac18Load------------------------------------------------------
library(VSURF)
data("vac18", package = "mixOmics")
geneExpr <- vac18$genes
stimu <- vac18$stimulation

## ----vsurfVac18seqCode---------------------------------------
set.seed(481933)
vacVSURF <- VSURF(x = geneExpr, y = stimu)

## ----vsurfVac18seqRes, fig.cap="Graphiques illustrant les résultats de `VSURF()`, données `Vac18`."----
summary(vacVSURF)
plot(vacVSURF)

## ----vsurfVac18probeSelPred----------------------------------------------
probeSelPred <- colnames(geneExpr)[vacVSURF$varselect.pred]
probeSelPred

## ----vsurfVac18Code------------------------------------------
set.seed(627408, kind = "L'Ecuyer-CMRG")
vacVSURFpara <- VSURF(x = geneExpr, y = stimu, parallel = TRUE, ncores = 3,
                  clusterType = "FORK")

## ----vsurfVac18Res-------------------------------------------------------
summary(vacVSURFpara)

