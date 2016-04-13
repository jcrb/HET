library(xts)

all.xts <- function(xts){
    # tous les graphes
    n <- names(xts)
    for (i in n) {
        plot(xts[, i], main = paste0(i, " - HET", indic), ylab = "fréquence")
        # si moyenne mobile
        lines(rollmean(xts[,i], 7), col = "blue", lwd = 3)
    }  
}


# données centrées et réduites

# lecture du fichier des moyennes
mean.ref <- read.csv("../../HET Alsace 2015/het_mean.csv")
rownames(mean.ref) <- mean.ref$X
mean.ref <- mean.ref[order(mean.ref$X),]
mean.ref <- mean.ref[, -1]
names(mean.ref) <- c("ahet1", "ahet2", "ahet3", "ahet4", "ahet5")
# suprimer la ligne Ccm
mean.ref[-which(rownames(mean.ref) == "Ccm"),]
mean.ref[-which(rownames(mean.ref) == "Hus"),]
# head(mean.ref)

# lectures des ecarts-type de référence
sd.ref <- read.csv("../../HET Alsace 2015/het_sd.csv")
rownames(sd.ref) <- sd.ref$X
sd.ref <- sd.ref[order(sd.ref$X),]
sd.ref <- sd.ref[, -1]
names(sd.ref) <- c("ahet1", "ahet2", "ahet3", "ahet4", "ahet5")
# suprimer la ligne Ccm
sd.ref[-which(rownames(sd.ref) == "Ccm"),]
sd.ref[-which(rownames(sd.ref) == "Hus"),]

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

# tous les graphes centrés-réduits
all.xts.cr <- function(xts, m, sd, indic){
    n <- names(xts)
    for (i in n) {
        moy <- m[which(rownames(m) == i), indic]
        s <- sd[which(rownames(sd) == i), indic]
        plot((xts[, i] - moy)/s, main = paste0(i, " - HET", indic), ylab = "écarts-type")
        # si moyenne mobile
        lines(rollmean((xts[, i] - m[i, indic])/sd[i, indic], 7), col = "blue", lwd = 3)
        abline(h = 0, col = "red")
    }
}

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

# test

# lit une matrice d'indicateur
indic <- 5
# het <- read.csv(paste0("Tests/Matrice_indicateurs/mat_het", 5, ".csv"))
het <- read.csv(paste0("mat_het", indic, ".csv")) 
colnames(het)[1:2] <- c("date", "3Fr")
het$date <- as.Date(het$date)
head(het)

xps <- df.to.xps(het)
head(xps)

head(mean.ref)
head(sd.ref)

# all.xts.cr(xps, mean.ref, sd.ref, indic)
all.xts(xps)
