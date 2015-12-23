#===================================================
#
#   df.duree.pas
#
#===================================================
# Routines pour Durée de passage

# nombre de patients présents à une heure précide. Par exemple combien de patients sont présents à 15 heures?
# Ce sont tous les patients arrivés avant 15 heures et repartis après 15 heures
# On part d'un dataframe formé de deux colonnes (ENTREE et SORIE) où chaque couple est complet => il faut éliminer les couples
# incomplets.
# Nécessite lubridate, Rpu2

# usage:
# - créer un dataframe "duree de passage" avec df.duree.pas Ce dataframe est l'objet de base à partir duquel d'autres
#   fonctions vont agir
# - la fonction is.present.at permet de créer un vecteur de présence d'un patient à une heure donnée, et de la le nombre de 
#   patients présents à une heure donné sum(is.present.at), ou le nombre de patients présents à une heure donnée pour 
#   chaque jour de l'année (tapply) puis de tracer le graphe de présence

#' @title Dataframe Durée de passage
#' @description fabrique à partir d'un dataframe de type RPU, un dataframe de type duree_passage comportant les colonnes suivantes:
#' date/heure d'entree, date/heure de sortie, durée de passage (en minutes par défaut), l'heure d'entrée (HMS), l'heure de sortie
#' @usage df.duree.pas(dx, unit = "mins", mintime = 0, maxtime = 3)
#' @param dx un dataframe de type RPU
#' @param unit unité de temps. Défaut = mins
#' @param mintime défaut = 0. Durée de passage minimale
#' @param maxtime défaut = 3 (72 heures). Durée de passage maximale
#' @param orientation boolean. Default = TRUE, la colonne ORIENTATION est prise en compte
#' @param finess boolean. Default = FALSE, la colonne FINESS n'est pas prise en compte
#' @return dataframe de type duree_passage
#' @examples df <- df.duree.pas(dx)

df.duree.pas <- function(dx, unit = "mins", mintime = 0, maxtime = 3, orientation = TRUE, finess = FALSE){
    
    if(orientation == TRUE)
        pas <- dx[, c("ENTREE", "SORTIE", "MODE_SORTIE", "ORIENTATION", "AGE")]
    else if(orientation == FALSE)
        pas <- dx[, c("ENTREE", "SORTIE", "MODE_SORTIE", "AGE")]
    
    if(finess == TRUE){
        FINESS <- dx$FINESS
        pas <- cbind(pas, FINESS)
    }
    
    # on ne conserve que les couples complets
    pas2 <- pas[complete.cases(pas[, c("ENTREE", "SORTIE")]),]
    
    # calcul de la rurée de passage
    e <- ymd_hms(pas2$ENTREE)
    s <- ymd_hms(pas2$SORTIE)
    pas2$duree <- as.numeric(difftime(s, e, units = unit))
    
    # on ne garde que les passages dont la durées > 0 et < ou = 72 heures
    pas3 <- pas2[pas2$duree > mintime & pas2$duree < maxtime * 24 * 60 + 1,]
    
    # mémorise les heures d'entrée et de sortie
    pas3$he <- horaire(pas3$ENTREE)
    pas3$hs <- horaire(pas3$SORTIE)
    
    return(pas3)
    
}

#===================================================
#
#   is.present.at
#
#===================================================
#' @title Un patient est-il présent à une heure donnée ?
#' @description Crée le vecteur des personnes présentes à une heure donnée
#' @usage is.present.at((dp, heure = "15:00:00"))
#' @param dp dataframe de type duree_passage
#' @param heure heure au format HH:MM:SS. C'es l'heure à laquelle on veut mesurer les passages
#' @return np vecteur de boolean: TRUE si présent à l'heure analysee et FALSE sinon. La longueur de np
#' est égale à celle de dx.
#' @examples dp <- df.duree.pas(dx)
#'           dp$present.a.15h <- is.present.at(dp)
#'           # nombre moyen de patients présents à 15h tous les jours
#'           n.p15 <- tapply(dp$present.a.15h, yday(as.Date(dp$ENTREE)), sum)
#'           summary(n.p15)
#'           sd(n.p15)
#'           # transformation en xts
#'           xts.p15 <- xts(n.p15, order.by = unique(as.Date(dp$ENTREE)))
#'           plot(xts.p15, ylab = "Nombre de patients à 15h", main = "Nombre de patients présents à 15 heures")
#'           lines(rollmean(x = xts.p15, k = 7), col = "red", lwd = 2)
#'           
#'           # à 2h du matin
#'           dp$present.a.2h <- is.present.at(dp, "02:00:00")
#'           n.p2 <- tapply(dp$present.a.2h, yday(as.Date(dp$ENTREE)), sum)
#'           summary(n.p2)
#'           xts.p2 <- xts(n.p2, order.by = unique(as.Date(dp$ENTREE)))
#'           plot(xts.p2, ylab = "Nombre de patients présents", main = "Nombre de patients présents à 2 heures du matin")
#'           lines(rollmean(x = xts.p2, k = 7), col = "red", lwd = 2)
#'           # pour les données de 2015, noter le pic à 2 heures du matin
#'           
#'           # à 8 heures
#'           present.a.8h <- is.present.at(dp, "08:00:00")
#'           n.p8 <- tapply(present.a.8h, yday(as.Date(dp$ENTREE)), sum)
#'           summary(n.p8)
#'           xts.p8 <- xts(n.p8, order.by = unique(as.Date(dp$ENTREE)))
#'           plot(xts.p8, ylab = "Nombre de patients présents", main = "Nombre de patients présents à 8 heures du matin")
#'           lines(rollmean(x = xts.p8, k = 7), col = "red", lwd = 2)
#' 
is.present.at <- function(dp, heure = "15:00:00"){
    # présent à 15 heures
    limite <- hms(heure) # pour incrémenter d'une heure: hms("15:00:00") + as.period(dhours(1))
    np <- dp$he < limite & dp$hs > limite
    
    return(np)
}

#' @title
#' @description
#' @usage
#' @param vx un vecteur de boolean (voir is.present.at)
resume.present <- function(vx){
    n.vx <- length(vx)
    n.true <- sum(vx)
    t <- table(vx)
}