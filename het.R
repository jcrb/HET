
#' @title radar des indicateurs HET
#' @description affichage des indicateurs HET à partir d'une matrice
#' @usage
#' @param vx vecteur HET centré-réduit du jour
#' @param seuil seuil de normalité
#' @param circle nombre de cercles
#' @examples
#' 
radar.het <- function(vx, seuil = 5, circle  = 9){
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
    jour <- index(b)[1]
    
    radial.plot(vx, 
                labels=het.names,
                rp.type="p", 
                main= paste0(jour, " - Diagramme indicateurs HET"), 
                grid.unit="",
                radial.lim=c(0, circle),
                poly.col=col,
                show.grid.labels=1)
}