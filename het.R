#======================================================================================================
#
#
#
#======================================================================================================
#' @title radar des indicateurs HET
#' @description affichage des indicateurs HET à partir d'une matrice
#' @usage
#' @param vx vecteur HET centré-réduit du jour
#' @param seuil seuil de normalité
#' @param circle nombre de cercles
#' @examples
#' 
radar.het <- function(vx, seuil = 5, circle  = 9, date = ""){
    b <- vx
    m <- max(b)
    col = "green"
    if(m > 6)
        col = "yellow"
    if(m > 7)
        col = "orange"
    if(m > 8)
        col = "red"
    
    het.names<-c("HET1","HET2", "HET3", "HET4", "HET5")
    if(class(vx) == "xts")
        jour <- index(b)[1]
    else
        jour <- date
    
    radial.plot(vx, 
                labels=het.names,
                rp.type="p", 
                main = jour, # paste0(jour, " - Diagramme indicateurs HET"), 
                grid.unit="",
                radial.lim=c(0, circle),
                boxed.radial = FALSE,
                poly.col=col,
                show.grid.labels=1)
                par(cex.axis = 0.6, cex.lab = 0.6)
}

#======================================================================================================
#
#
#
#======================================================================================================
#' @title créer un DF HET
#' @description crée un dataframe de type time serie (Xts). Chaque colonne représente un indicateur HET
#' @usage het.df(dx)
#' @param dx un dataframe de type RPU
#' @return un dataframe de type XTS à 5 colonnes: HET1 à HET5. Dans cette version, HET1 est arbitrairement fixé à 0.
#' Ce dataframe permet de tracer les courbes de variation de chaque indicateur sur la période.
#' @details nécessite Rpu2, lubridate
#' @examples xt <- het.df(dx)
#'           plot(xt[, "HET2"])
#'           lines(rollmean(xt[, "HET2"], 7), col = "red", lwd = 3)
#' 
het.df <- function(dx){
    # création d'un calendrier pour le période (nécessaire pour transformer en time serie xts)
    x <- seq(min(as.Date(dx$ENTREE)), max(as.Date(dx$ENTREE)), 1)
    # HET2
    n.rpu.jour <- tapply(as.Date(dx$ENTREE), day(as.Date(dx$ENTREE)), length)
    ts.het2 <- xts(n.rpu.jour, order.by = x)
    colnames(ts.het2) <- "HET2"
    
    # HET 3
    # sélectionne les enregistrements où le MODE_SORTIE correspond à une hospitalisation 
    hosp <- dx[!is.na(dx$MODE_SORTIE) & dx$MODE_SORTIE %in% c("Mutation", "Transfert"), ]
    # durée de passage si hospitalisation
    dp <- df.duree.pas(hosp, unit = "mins", mintime = 0, maxtime = 3)
    # moyenne quotidienne
    mean.dp <- tapply(dp$duree , day(as.Date(dp$ENTREE)), mean)
    # transformation en time serie
    ts.mean.dp <- xts(mean.dp, x)
    colnames(ts.mean.dp) <- "HET3"
    
    # HET 4
    n.hosp.jour <- tapply(as.Date(hosp$ENTREE), day(as.Date(hosp$ENTREE)), length)
    tx.hosp <- n.hosp.jour / n.rpu.jour
    ts.tx.hosp <- xts(tx.hosp, x)
    colnames(ts.tx.hosp) <- "HET4"
    
    # HET 5
    dp$present.a.15h <- is.present.at(dp)
    # nombre moyen de patients présents à 15h tous les jours
    n.p15 <- tapply(dp$present.a.15h, yday(as.Date(dp$ENTREE)), sum)
    
    # Transformation en TS
    ts.n.p15 <- xts(n.p15, x)
    colnames(ts.n.p15) <- "HET5"
    
    a <- cbind(0, ts.het2, ts.mean.dp, ts.tx.hosp, ts.n.p15)
    return(a)
}

#======================================================================================================
#
#       tx.hosp
#
#======================================================================================================
# taux d'hospitalisation 
#' @param vx est un vecteur des modes de sortie (non nuls) pour un établissement donné.
#' 
tx.hosp <- function(vx){
    s = summary(as.factor(vx))
    n = length(vx)
    hosp = sum(s["Mutation"], s["Transfert"], na.rm = TRUE)
    tx = hosp/n
    return(tx)}

#======================================================================================================
#
#       indicateurs.jour
#
#======================================================================================================
#' 
#' @param dx dataframe des données du jour pour un finess donné
#' @examples data <- dx[as.Date(dx$ENTREE) == "2015-12-01" & dx$FINESS = "Hus",]
#'           indicateurs.jour(data)
#' 
indicateurs.jour <- function(dx){
    het1 <- nrow(dx)
    het2 <- length(dx$AGE[dx$AGE > 74])
    # on crée un dataframe sans la colonne ORIENTATION mais avec la colonne FINESS
    dp <- df.duree.pas(dx, orientation = FALSE, finess = TRUE)
    het3 <- mean(dp$duree, na.rm = TRUE)
    # On forme un Dataframe de RPU dont le mode de sortie n'est pas nul, appemé ms:
    ms <- dx[!is.na(dx$MODE_SORTIE),]
    het4 <- tx.hosp(ms$MODE_SORTIE)
    het5 <- sum(is.present.at(dp))
    a <- c(het1, het2, het3, het4, het5)
    names(a) <- c("HET1","HET2","HET3","HET4","HET5")
    return(a)
}