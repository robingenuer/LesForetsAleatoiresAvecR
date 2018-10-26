
#' # 1 Introduction
#' 
#' ## 1.5 Jeux de données
#' 
#' ### 1.5.1 Jeu de données fil rouge : détection de spams

## ----introSpamLoad, tidy=FALSE-------------------------------------------
data("spam", package = "kernlab")
set.seed(9146301)
levels(spam$type) <- c("ok", "spam")
yTable <- table(spam$type)
indApp <- c(sample(1:yTable[2], yTable[2]/2),
            sample((yTable[2] + 1):nrow(spam), yTable[1]/2))
spamApp <- spam[indApp, ]
spamTest <- spam[-indApp, ]

#' ### 1.5.2 Pollution à l'ozone

## ----introOzoneLoad------------------------------------------------------
data("Ozone", package = "mlbench")

#' ### 1.5.3 Analyser des données génomiques pour une étude vaccinale

## ----introVac18Load------------------------------------------------------
data("vac18", package = "mixOmics")

