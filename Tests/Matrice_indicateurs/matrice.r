# tous les graphes centrés-réduits
all.xts.cr <- function(xts, m, sd, indic){
    n <- names(xts)
    for (i in n) {
        mean <- m[which(rownames(m) == i), indic]
        s <- sd[which(rownames(sd) == i), indic]
        plot((xts[, i] - mean)/s, main = paste0(i, " - HET", indic), ylab = "écarts-type")
        # si moyenne mobile
        lines(rollmean((xts[, i] - m[i, indic])/sd[i, indic], 7), col = "blue", lwd = 3)
        abline(h = 0, col = "red")
    }
}

df.to.xps <- function(het){
    # La première colonne correspond à la date du jour. Il faut transformer au format
    # date et lui donner un nom. La colonne suivante Xfr doit être renommée Tfr
    colnames(het)[1:2] <- c("date", "3Fr")
    het$date <- as.Date(het$date)
    
    # Transformation en xts en excluant la colonne date
    xts <- xts(het[, -1], order.by = het$date)
    xts
}

all.xts <- function(xts){
    # tous les graphes
    n <- names(xts)
    for (i in n) {
        plot(xts[, i], main = paste0(i, " - HET", indic), ylab = "fréquence")
        # si moyenne mobile
        lines(rollmean(xts[,i], 7), col = "blue", lwd = 3)
    }  
}


