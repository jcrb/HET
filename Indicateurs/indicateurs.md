# Test des indicateurs HET
JcB  
09/11/2015  

Indicateurs Champagne Ardennes
==============================

L'ORUCA a retenu 5 indicateurs:

- HET1: disponibilité en lit médecine + chir (source manuelle ou ROR) [FEDORU 4.1]
- HET2: Le nombre de passages aux urgences à J-1 [FEDORU 2.2]
- HET3: moyenne des durées de passage des patients hospitalisés à partir des urgences J-1
- HET4: taux d'hospitalisation après passage aux urgences (nb d'hospitalisation / nb de passages) J-1
- HET5: charge d'occupation à 15 heures / capacité d'accueil du service des urgences (nb de patients pouvant être installés simultanément dans le service en salle d'examen et en zone d'attente organisée)

La FEDORU propose 7 indicateurs:

- FHET1: Nombre de passage sur les dernières 24 heures [2.2 = HET2]
- FHET2: Nombre de patients présents à un instant donné [2.3 = HET5 ?] par 10 mn
- FHET3: Nombre de patients présents de plus de 75 ans [2.4] par 10 mn
- FHET4: Durée moyenne de présence aux urgences [2.11 = HET3] par 10 mn
- FHET5: Nombre de transferts par défaut de lits sur les 3 derniers jours [3.6]
- FHET6: Nombre d'hospitalisations à partir des urgences [3.7 = HET4]
- FHET7: Nombre de lits disponibles par service [4.1 = HRT1] par 10 mn

Résultats disponibles:

- synthèse régionale du radar de chaque établissement
- détail du radar avec tableau de données chiffrées pour chaque établissement
- détail de chaque indicateur pour chaque établissement avec courbe historique des 30 derniers jours (et moyenne lissée)


```r
fichier <- "../../DATA/data_test.Rda" # supprimer ../ en mode console
load(fichier) # dx

library(lubridate)
library(xts)
```

```
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library(Rpu2)
```

```
## Loading required package: xtable
## Loading required package: openintro
## Please visit openintro.org for free statistics materials
## 
## Attaching package: 'openintro'
## 
## The following object is masked from 'package:datasets':
## 
##     cars
## 
## Loading required package: plotrix
```

```r
source("duree_passage.R") # si console: source("Indicateurs/duree_passage.R")


# masquer cette ligne pour faire le calcul avec tous les établissements
dx <- dx[dx$FINESS == "Wis",]

# création d'un calendrier pour le période (nécessaire pour transformer en time serie xts)
x <- seq(min(as.Date(dx$ENTREE)), max(as.Date(dx$ENTREE)), 1)
```

HET2 - Nombre de passages aux urgences (par jour)
------------------------------------------


```r
n.rpu.jour <- tapply(as.Date(dx$ENTREE), day(as.Date(dx$ENTREE)), length)

# transformation en time serie
x <- seq(min(as.Date(dx$ENTREE)), max(as.Date(dx$ENTREE)), 1)
ts.het2 <- xts(n.rpu.jour, order.by = x)
colnames(ts.het2) <- "HET2"

head(ts.het2)
```

```
##            HET2
## 2015-10-01   31
## 2015-10-02   41
## 2015-10-03   39
## 2015-10-04   46
## 2015-10-05   51
## 2015-10-06   31
```

```r
plot(ts.het2, main = "Nombre quotidien de passages", type = "b", pch = 21, bg = "yellow")

abline(h = mean(ts.het2), lty = 2, col = "red") # moyenne
lines(rollmean(ts.het2, k = 7), col = "green", lwd = 3)
```

![](indicateurs_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Répartition normale ?
summary(n.rpu.jour)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   20.00   28.00   31.00   32.71   38.00   51.00
```

```r
sd(n.rpu.jour)
```

```
## [1] 7.439505
```

```r
hist(n.rpu.jour)
```

![](indicateurs_files/figure-html/unnamed-chunk-2-2.png) 

Graphe avec les WE: on utilise zoo car abline ne fonctionne pas avec xts ?


```r
we <- x[wday(x) %in% c(1,7)]
plot(zoo(ts.het2))
abline(v = as.Date(we), lty = 2, col = "red")
```

![](indicateurs_files/figure-html/unnamed-chunk-3-1.png) 

HET3: moyenne des durées de passage des patients hospitalisés à partir des urgences
-----------------------------------------------------------------------------------


```r
# sélectionne les enregistrements où le MODE_SORTIE correspond à une hospitalisation 
hosp <- dx[!is.na(dx$MODE_SORTIE) & dx$MODE_SORTIE %in% c("Mutation", "Transfert"), ]

# durée de passage si hospitalisation
dp <- df.duree.pas(hosp, unit = "mins", mintime = 0, maxtime = 3)

# moyenne quotidienne
mean.dp <- tapply(dp$duree , day(as.Date(dp$ENTREE)), mean)

# transformation en time serie
ts.mean.dp <- xts(mean.dp, x)
colnames(ts.mean.dp) <- "HET3"

par(mar = c(2,4,2,5))
plot(ts.het2, ylab = "Nombre de passages", main = "Nombre de passages et durée de passage")
par(new=TRUE)
plot(zoo(ts.mean.dp), xaxt="n",xlab="",ylab="", main = "", yaxt="n", lty = 2, col = "blue")
axis(4)
mtext("HET3 - Durée moyenne de passage avant hospitalisation (mn)",side=4,line=3, col = "blue")
```

![](indicateurs_files/figure-html/unnamed-chunk-4-1.png) 

HET4: taux d'hospitalisation après passage aux urgences (nb d'hospitalisation / nb de passages)
-----------------------------------------------------------------------------------------------


```r
n.hosp.jour <- tapply(as.Date(hosp$ENTREE), day(as.Date(hosp$ENTREE)), length)
tx.hosp <- n.hosp.jour / n.rpu.jour
ts.tx.hosp <- xts(tx.hosp, x)
colnames(ts.tx.hosp) <- "HET4"
plot(ts.tx.hosp, main = "HET4 - Taux d'hospitalisation", ylab = "%")
abline(h = mean(ts.tx.hosp), col = "red")
sd1 <- mean(ts.tx.hosp) + sd(ts.tx.hosp)
sd2 <- mean(ts.tx.hosp) + sd(ts.tx.hosp) * 2
sd3 <- mean(ts.tx.hosp) + sd(ts.tx.hosp) * 3
abline(h = sd1, col = "green")
abline(h = sd2, col = "orange")
abline(h = sd3, col = "red")
lines(rollmean(ts.tx.hosp, 7), col = "blue", lwd = 2)
legend("bottomright", legend = c("moyenne générale", "moyenne lissée",  "1 SD", "2 SD", "3 SD"), col = c("red","blue","green","orange", "red"), lty = 1, lwd = 2, cex = 0.8)
```

![](indicateurs_files/figure-html/unnamed-chunk-5-1.png) 

HET5 - charge d'occupation à 15 heures / capacité d'accueil du service des urgences 
----------------------------------------------------------------------------

```r
dp$present.a.15h <- is.present.at(dp)
# nombre moyen de patients présents à 15h tous les jours
n.p15 <- tapply(dp$present.a.15h, yday(as.Date(dp$ENTREE)), sum)

# Transformation en TS
ts.n.p15 <- xts(n.p15, x)
colnames(ts.n.p15) <- "HET5"
plot(ts.n.p15, main = "Nombre de patients présents au SU à 15 heures")
```

![](indicateurs_files/figure-html/unnamed-chunk-6-1.png) 

Synthèse
--------


```r
a <- cbind(ts.het2, ts.mean.dp, ts.tx.hosp, ts.n.p15)
head(a)
```

```
##            HET2     HET3      HET4 HET5
## 2015-10-01   31 164.6000 0.1612903    0
## 2015-10-02   41 226.3750 0.1951220    3
## 2015-10-03   39 116.5000 0.1794872    1
## 2015-10-04   46 149.3750 0.1739130    1
## 2015-10-05   51 188.3636 0.2156863    1
## 2015-10-06   31 257.1000 0.3548387    3
```

```r
a[1, ]
```

```
##            HET2  HET3      HET4 HET5
## 2015-10-01   31 164.6 0.1612903    0
```

```r
#radial.plot(a[1, ], labels=ion.names,rp.type="p",main="Diagramme indicateurs HET", grid.unit="%",radial.lim=c(0, 5),poly.col="yellow",show.grid.labels=1)
```

Corrélation taux hospitalisation et nombre de passages, et durée de passage
---------------------------------------------------------------------------


```r
# corrélation entre la durée moyenne de passage quotidienne et le nombre de présents à 15h
plot(mean.dp, n.p15, main = "Corrélation durée moyenne de passage quotidienne\n et le nombre de présents à 15h", col ="black", pch = 15)
cor(mean.dp, n.p15)
```

```
## [1] 0.2490515
```

```r
y <- lm(mean.dp ~ n.p15)
y
```

```
## 
## Call:
## lm(formula = mean.dp ~ n.p15)
## 
## Coefficients:
## (Intercept)        n.p15  
##      153.24        12.29
```

```r
summary(y)
```

```
## 
## Call:
## lm(formula = mean.dp ~ n.p15)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -74.81 -38.65 -16.15  33.55 194.14 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  153.238     16.865   9.086 5.54e-10 ***
## n.p15         12.287      8.873   1.385    0.177    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.46 on 29 degrees of freedom
## Multiple R-squared:  0.06203,	Adjusted R-squared:  0.02968 
## F-statistic: 1.918 on 1 and 29 DF,  p-value: 0.1767
```

```r
abline(y)
```

![](indicateurs_files/figure-html/unnamed-chunk-8-1.png) 

```r
# corrélation entre la duréee moyenne de passage et le nombre total de passages
cor(mean.dp, n.rpu.jour)
```

```
## [1] -0.02570271
```

```r
# corrélation entre taux hospitalisation et nombre de passages
cor(tx.hosp, n.rpu.jour)
```

```
## [1] 0.1218142
```

```r
y <- lm(n.rpu.jour ~ tx.hosp)
y
```

```
## 
## Call:
## lm(formula = n.rpu.jour ~ tx.hosp)
## 
## Coefficients:
## (Intercept)      tx.hosp  
##       30.49        10.38
```

```r
summary(y)
```

```
## 
## Call:
## lm(formula = n.rpu.jour ~ tx.hosp)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.606  -4.666  -1.166   5.186  18.269 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   30.492      3.616   8.432 2.72e-09 ***
## tx.hosp       10.379     15.705   0.661    0.514    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.51 on 29 degrees of freedom
## Multiple R-squared:  0.01484,	Adjusted R-squared:  -0.01913 
## F-statistic: 0.4368 on 1 and 29 DF,  p-value: 0.5139
```

```r
plot(tx.hosp, n.rpu.jour, col ="black", pch = 15)
abline(y)
```

![](indicateurs_files/figure-html/unnamed-chunk-8-2.png) 

Radar
=====


```r
source("../het.R")

a <- cbind(0, ts.het2, ts.mean.dp, ts.tx.hosp, ts.n.p15)
head(a)
```

```
##            X0 HET2     HET3      HET4 HET5
## 2015-10-01  0   31 164.6000 0.1612903    0
## 2015-10-02  0   41 226.3750 0.1951220    3
## 2015-10-03  0   39 116.5000 0.1794872    1
## 2015-10-04  0   46 149.3750 0.1739130    1
## 2015-10-05  0   51 188.3636 0.2156863    1
## 2015-10-06  0   31 257.1000 0.3548387    3
```

```r
a[1, ]
```

```
##            X0 HET2  HET3      HET4 HET5
## 2015-10-01  0   31 164.6 0.1612903    0
```

```r
# normalisation sous forme de variable centrée et réduite. Par défaut, moyenne et sd sont calculés à partir de l'échantillon de départ.
m <- 5
a[, 1] <- m # arbitraire faute de mieux
a[, 2] <- m + (a[, 2] - mean(n.rpu.jour)) / sd(n.rpu.jour)
a[, 3] <- m +  (a[, 3] - mean(mean.dp)) / sd(mean.dp)
a[, 4] <- m +  (a[, 4] - mean(tx.hosp)) / sd(tx.hosp)
a[, 5] <- m +  (a[, 5] - mean(n.p15)) / sd(n.p15)

# indicateurs pour le mois d'octobre 2015
for(i in 1:30){
    radar.het(a[i,])
}
```

![](indicateurs_files/figure-html/unnamed-chunk-9-1.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-2.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-3.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-4.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-5.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-6.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-7.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-8.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-9.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-10.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-11.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-12.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-13.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-14.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-15.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-16.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-17.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-18.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-19.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-20.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-21.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-22.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-23.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-24.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-25.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-26.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-27.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-28.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-29.png) ![](indicateurs_files/figure-html/unnamed-chunk-9-30.png) 

Test de la fonction het.fr
==========================

Cette fonction crée un dataframe de type Xts à partir d'un dataframe RPU en extrayant les indicateurs HET. Au final on obtient un dataframe dont chaque ligne correspond à une date et 5 colonnes correspondant au 5 indicateurs:


```r
xt <- het.df(dx)
head(xt)
```

```
##            X0 HET2     HET3      HET4 HET5
## 2015-10-01  0   31 164.6000 0.1612903    0
## 2015-10-02  0   41 226.3750 0.1951220    3
## 2015-10-03  0   39 116.5000 0.1794872    1
## 2015-10-04  0   46 149.3750 0.1739130    1
## 2015-10-05  0   51 188.3636 0.2156863    1
## 2015-10-06  0   31 257.1000 0.3548387    3
```

A partir de ce dataframe on peut:

- récupérer la date du jour

```r
index(head(xt))
```

```
## [1] "2015-10-01" "2015-10-02" "2015-10-03" "2015-10-04" "2015-10-05"
## [6] "2015-10-06"
```

- tracer la courbe de tendance pour un indicateur

```r
plot(xt[, "HET2"])
lines(rollmean(xt[, "HET2"], 7), col = "red", lwd = 3)
```

![](indicateurs_files/figure-html/unnamed-chunk-12-1.png) 

- créer le polygone correspondant après avoir réduit-centré la variable: A FAIRE

```r
# normalisation sous forme de variable centrée et réduite. Par défaut, moyenne et sd sont calculés à partir de l'échantillon de départ.
m <- 5
xt[, 1] <- m # arbitraire faute de mieux
xt[, 2] <- m + (xt[, 2] - mean(n.rpu.jour)) / sd(n.rpu.jour)
xt[, 3] <- m +  (xt[, 3] - mean(mean.dp)) / sd(mean.dp)
xt[, 4] <- m +  (xt[, 4] - mean(tx.hosp)) / sd(tx.hosp)
xt[, 5] <- m +  (xt[, 5] - mean(n.p15)) / sd(n.p15)

head(xt)
```

```
##            X0     HET2     HET3     HET4     HET5
## 2015-10-01  5 4.770189 4.861409 4.400482 3.618345
## 2015-10-02  5 6.114365 5.978744 4.787962 6.295302
## 2015-10-03  5 5.845530 3.991417 4.608894 4.510664
## 2015-10-04  5 6.786453 4.586033 4.545052 4.510664
## 2015-10-05  5 7.458540 5.291226 5.023490 4.510664
## 2015-10-06  5 4.770189 6.534471 6.617231 6.295302
```

```r
radar.het(xt[1,])
```

![](indicateurs_files/figure-html/unnamed-chunk-13-1.png) 


Indicateurs FEDORU
======================

FHET2: Nombre de patients présents à un instant donné [2.3] par 10 mn


```r
h1 <- hms("15:00:00")
inc <- as.period(dhours(1/6))
i <- h1
while(i < h1+ as.period(dhours(8))){
    print(c(i, sum(is.present.at(dp, i))))
    i = i + inc
    }
```

```
## [1] "15H 0M 0S" "48S"      
## [1] "15H 10M 0S" "56S"       
## [1] "15H 20M 0S" "55S"       
## [1] "15H 30M 0S" "53S"       
## [1] "15H 40M 0S" "53S"       
## [1] "15H 50M 0S" "52S"       
## [1] "15H 60M 0S" "50S"       
## [1] "15H 70M 0S" "49S"       
## [1] "15H 80M 0S" "49S"       
## [1] "15H 90M 0S" "48S"       
## [1] "15H 100M 0S" "46S"        
## [1] "15H 110M 0S" "44S"        
## [1] "15H 120M 0S" "44S"        
## [1] "15H 130M 0S" "42S"        
## [1] "15H 140M 0S" "42S"        
## [1] "15H 150M 0S" "40S"        
## [1] "15H 160M 0S" "41S"        
## [1] "15H 170M 0S" "37S"        
## [1] "15H 180M 0S" "33S"        
## [1] "15H 190M 0S" "34S"        
## [1] "15H 200M 0S" "33S"        
## [1] "15H 210M 0S" "32S"        
## [1] "15H 220M 0S" "31S"        
## [1] "15H 230M 0S" "31S"        
## [1] "15H 240M 0S" "31S"        
## [1] "15H 250M 0S" "29S"        
## [1] "15H 260M 0S" "29S"        
## [1] "15H 270M 0S" "29S"        
## [1] "15H 280M 0S" "28S"        
## [1] "15H 290M 0S" "27S"        
## [1] "15H 300M 0S" "26S"        
## [1] "15H 310M 0S" "23S"        
## [1] "15H 320M 0S" "26S"        
## [1] "15H 330M 0S" "25S"        
## [1] "15H 340M 0S" "26S"        
## [1] "15H 350M 0S" "27S"        
## [1] "15H 360M 0S" "26S"        
## [1] "15H 370M 0S" "27S"        
## [1] "15H 380M 0S" "27S"        
## [1] "15H 390M 0S" "25S"        
## [1] "15H 400M 0S" "24S"        
## [1] "15H 410M 0S" "20S"        
## [1] "15H 420M 0S" "19S"        
## [1] "15H 430M 0S" "18S"        
## [1] "15H 440M 0S" "16S"        
## [1] "15H 450M 0S" "14S"        
## [1] "15H 460M 0S" "13S"        
## [1] "15H 470M 0S" "9S"
```


- FHET3: Nombre de patients présents de plus de 75 ans [2.4] par 10 mn
- FHET4: Durée moyenne de présence aux urgences [2.11 = HET3] par 10 mn
- FHET5: Nombre de transferts par défaut de lits sur les 3 derniers jours [3.6]
- FHET6: Nombre d'hospitalisations à partir des urgences [3.7 = HET4]
- FHET7: Nombre de lits disponibles par service [4.1] par 10 mn
