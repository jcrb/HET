#======================================================================================================
#
# radar.het
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
                radial.lim=c(0, circle), # 
                boxed.radial = FALSE,
                poly.col=col,
                show.grid.labels=1)
                par(cex.axis = 0.6, cex.lab = 0.6)
}

#======================================================================================================
#
#  het.df
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
#' @title Calcule les indicateurs HET
#' @description Callcule les indicateurs HET pour un ou plusieurs FINESS 
#' sur une période de un ou plusieurs jours
#' @details Nécessite lubridate et Rpu2. Calcule les 5 indicateurs de CA.
#' @param dx dataframe des données du jour pour un finess donné
#' @examples data <- dx[as.Date(dx$ENTREE) == "2015-12-01" & dx$FINESS == "Hus",]
#'           indicateurs.jour(data)
#' @usage indicateurs.jour(dx)
#' @return un vecteur des indicateurs
#' @export
#' 
indicateurs.jour <- function(dx){
    library(Rpu2) # horaire
    library(lubridate) # ymd_hms 
    het1 <- 0
    het2 <- nrow(dx)
    
    # on crée un dataframe sans la colonne ORIENTATION mais avec la colonne FINESS
    dp <- df.duree.pas(dx, orientation = FALSE, finess = TRUE)
    het3 <- mean(dp$duree, na.rm = TRUE)
    
    # On forme un Dataframe de RPU dont le mode de sortie n'est pas nul, appelé ms:
    ms <- dx[!is.na(dx$MODE_SORTIE),]
#     s <- summary(as.factor(ms))
    het4 <- tx.hosp(ms$MODE_SORTIE) # taux hospitalisation
    
    # nb de patients présents à 15h
    het5 <- sum(is.present.at(dp))
    
    a <- c(het1, het2, het3, het4, het5)
    names(a) <- c("HET1","HET2","HET3","HET4","HET5")
    
    # indicateurs FEDORU
#     fhet1 <- het2
#     fhet2 <- het3
#     fhet3 <- length(dx$AGE[dx$AGE > 74]) # nb de plus de 74 ans
#     fhet4 <- NA # Durée moyenne de présence aux urgences
#     fhet5 <- NA # nb de transferts par défaut de lits
#     fhet6 <- s["Mutation"] + s["Transfert"] # nb hospitalisation
#     fhet1 <- NA # nb de lits disponibles
    
    return(a)
}

#======================================================================================================
#
#       indicateurs.ca.jour
#
#======================================================================================================
#' @title Calcule les indicateurs HET CA normalisés 
#' @description Calcule les indicateurs HET type CA un jour donné pour un FINESS donné puis les
#' normalise.
#' @usage indicateurs.ca.jour(dx, finess, date, file)
#' @param dx dataframe RPU
#' @param finess code de l'établissement
#' @param date date du jour 
#' @param file adresse du fichier des moyennes et sd de référence
#' @examples finess <- "Sav"
#'           a <- indicateurs.ca.jour(dx, finess, jour, "ref_het_2015.csv")
#'           radar.het(a, date = paste(finess, "-", format(jour, "%d/%m/%Y")))
#' 
indicateurs.ca.jour <- function(dx, finess, date, file){
    data <- dx[as.Date(dx$ENTREE) == jour & dx$FINESS == finess,]
    het.jour <- indicateurs.jour(data)
    
    # lecture du fichier de référence des moyennes et ecarts-type
    ref.df <- read.csv(file)
    # on retire Hus et Mul
    ref.df <- ref.df[-c(9:10),]
    # triage par ordre alphabétique de la 1ere colonne
    ref.df <- ref.df[with(ref.df, order(X)),]
    # on met comme nom de ligne le contenu de la colonne X, ce qui permet de désigner une ligne par le FINESS de l'établissement.
    rownames(ref.df) <- ref.df$X
    
    # Indicateurs normalisés:
    het1 <- 5
    het2 <- 5 + (het.jour["HET2"] - ref.df[finess,"ref.het2.m"]) / ref.df[finess,"ref.het2.sd"]
    het3 <- 5 + (het.jour["HET3"] - ref.df[finess,"ref.het3.m"]) / ref.df[finess,"ref.het3.sd"]
    het4 <- 5 + (het.jour["HET4"] - ref.df[finess,"ref.het4.m"]) / ref.df[finess,"ref.het4.sd"]
    het5 <- 5 + (het.jour["HET5"] - ref.df[finess,"ref.het5.m"]) / ref.df[finess,"ref.het5.sd"]
    a <- cbind(het1, het2, het3, het4, het5)
    
    return(a)
}

#======================================================================================================
#
#       na2zero
#
#======================================================================================================
na2zero <- function(x){x[is.na(x)] <- 0}

#======================================================================================================
#
#       het2
#
#======================================================================================================
#' @title Nombre de passages un jour donné pour un finess donné 
#' @description Nombre de passages un jour donné pour un finess donné 
#' @usage het2(dx, finess, jour)
#' @param dx un dataframe RPU
#' @param finess un identifiant hôpital (ex. "Sel")
#' @param jour une date (par ex. "2016-01-10")
#' 
het2 <- function(dx, finess, jour){
    d <- dx[dx$FINESS == finess & as.Date(dx$ENTREE) == jour, "ENTREE"]
    return(length(d))
}

#======================================================================================================
#
#       het3
#
#======================================================================================================
#' @title moyenne des durées de passage
#' @description moyenne des durées de passage des patients hospitalisés à partir des urgences 
#' pour un jour et finess donnés.
#' @details moyenne de la différence SORTIE - ENTREE pour les patients dont le MODE_SORTIE
#' correspond à 'Mutation' ou 'Transfert'
#' @param dx un dataframe RPU
#' @param finess un identifiant hôpital (ex. "Sel")
#' @param jour une date (par ex. "2016-01-10")
#' @usage het3(dx, finess, jour)
#' 
het3 <- function(dx, finess, jour){
    # RPU du jour
    d <- dx[dx$FINESS == finess & as.Date(dx$ENTREE) == jour,]
    if(nrow(d) > 0){
        # sélectionne les enregistrements où le MODE_SORTIE correspond à une hospitalisation 
        # hosp <- d[!is.na(d$MODE_SORTIE) & d$MODE_SORTIE %in% c("Mutation", "Transfert"), ]
        
        # sélectionne les enregistrements où le MODE_SORTIE est renseigné quelquesoit le devenir du patient
        hosp <- d[!is.na(d$MODE_SORTIE), ]
        if(nrow(hosp) > 0){
            # durée de passage si hospitalisation
            dp <- df.duree.pas(hosp, unit = "mins", mintime = 0, maxtime = 3)
            # moyenne quotidienne
            mean.dp <- mean(dp$duree, na.rm = TRUE)
            return(mean.dp)
        }
        return(0)
    }
    else
        return(0)
}

#======================================================================================================
#
#       het4
#
#======================================================================================================
#' @title taux d'hospitalisation pour un jour et un finess donnés
#' @description taux d'hospitalisation après passage aux urgences (nb d'hospitalisation / nb de passages)
#' @details none
#' @param dx un dataframe RPU
#' @param finess un identifiant hôpital (ex. "Sel")
#' @param jour une date (par ex. "2016-01-10")
#' @usage het4(dx, finess, jour)
#' 
het4 <- function(dx, finess, jour){
    # RPU du jour où MODE_SORTIE est renseigné
    d <- dx[dx$FINESS == finess & as.Date(dx$ENTREE) == jour & !is.na(dx$MODE_SORTIE),]
    n.rpu.jour <- nrow(d)
    
    # sélectionne les enregistrements où le MODE_SORTIE correspond à une hospitalisation 
    hosp <- d[!is.na(d$MODE_SORTIE) & d$MODE_SORTIE %in% c("Mutation", "Transfert"), ]
    n.hosp.jour <- nrow(hosp)
    
    tx.hosp <- n.hosp.jour / n.rpu.jour
    return(tx.hosp)
}

#======================================================================================================
#
#       het5
#
#======================================================================================================
#' @title Charge de soins pour un jour et un finess donnés
#' @description Charge de soins pour un jour et un finess donnés
#' @details none
#' @param dx un dataframe RPU
#' @param finess un identifiant hôpital (ex. "Sel")
#' @param jour une date (par ex. "2016-01-10")
#' @usage het5(dx, finess, jour)
#' 
het5 <- function(dx, finess, jour){
    # RPU du jour
    d <- dx[dx$FINESS == finess & as.Date(dx$ENTREE) == jour,]
    if(nrow(d) > 0){
        # Calcul des heures de présence
        dp <- df.duree.pas(d, unit = "mins", mintime = 0, maxtime = 3)
    
        dp$present.a.15h <- is.present.at(dp) # vecteur de booleen
        return(sum(dp$present.a.15h))
    }
    else
        return(0)
}


#======================================================================================================
#
#       ahet2
#
#======================================================================================================
#' @title Nombre de RPU sup.ou égal à 75 ans 
#' @description Nombre de RPU sup.ou égal à 75 ans pour un jour et FINESS donnés
#' @details none
#' @param dx un dataframe RPU
#' @param finess un identifiant hôpital (ex. "Sel")
#' @param jour une date (par ex. "2016-01-10")
#' @usage ahet2(dx, finess, jour)
#' 
ahet2 <- function(dx, finess, jour){
    # RPU du jour
    d <- dx[dx$FINESS == finess & as.Date(dx$ENTREE) == jour & dx$AGE > 74, "AGE"]
    return(length(d))
}

