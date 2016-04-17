
Hôpital en tension
==================

Analyse des périodes de tensions hospitalières en Alsace en 2015-2016.

HET Alsace 2015
---------------
Cette partie crée les fichiers des moyennes et écart-type de référence pour les 5 indicateurs utilisés.
Les moyennes et écart-type (sd) de référence pour 5 indicateurs sont stockés dans les fichiers __het_mean__ et __het_sd__. Ils sont construits à partir des données de novembre 2014 à octobre 2015 inclu sauf pour HTP et NHC. En effet un bug corrigé à partir de novembre 2015 faisait que les modes de sortie vers le domicile n'étaient pas pris en compte. Il en résulte un taux anormalement élevé d'hospitalisations (> 70%) pour ces deux sites. Pour cette raison, les moyennes de référence pour ces 2 sites et uniquement pour l'indicateur "taux d'hospitalisation" (ahet4) a été corrigé manuellement. Les version de fichier suffixées par _old correspondent à la version non corrigée.

```
m <- read.csv("HET Alsace 2015/het_mean.csv")
rownames(m) <- m$X
m$X <- NULL
round(m,2)
```

Moyennes 2015 des indicateurs:

```
    ahet1.m ahet2.m ahet3.m ahet4.m ahet5.m
3Fr   47.33    5.41  125.37    0.08    4.82
Alk   46.61    6.84  124.05    0.13    2.56
Ane   46.46    4.38  128.00    0.01    6.55
Col  186.25   23.76  179.47    0.25   36.14
Dia   79.85   11.95  165.55    0.23   15.47
Dts   33.03    1.98   63.29    0.08    3.09
Geb   43.68    4.58   84.53    0.09    3.58
Hag  126.42   17.62  324.29    0.23   21.82
Hus  305.26   41.15  202.17    0.75   63.28
Mul  162.20   23.09  188.41    0.18   33.16
Odi   68.70    4.07   80.10    0.09    5.68
Ros   22.27    1.85   96.69    0.17    2.16
Sav   81.50   11.48  156.17    0.21   15.10
Sel   83.70   12.81  154.81    0.19   15.63
Wis   36.21    6.31  120.51    0.22    5.28
HTP  216.31   20.33  162.55    0.72   40.01
NHC   89.09   20.78  298.48    0.75   21.98
Emr  138.15   22.48  214.00    0.20   30.17
Hsr   40.50      NA  135.69    0.15    5.57
Ccm      NA      NA      NA      NA      NA
```

```
s <- read.csv("HET Alsace 2015/het_sd.csv")
rownames(s) <- s$X
s$X <- NULL
round(s, 2)
```

Ecart-types:

```
    ahet1.sd ahet2.sd ahet3.sd ahet4.sd ahet5.sd
3Fr     8.09     2.34    42.50     0.08     2.77
Alk     8.29     2.83    59.65     0.15     2.34
Ane     9.33     2.22    32.62     0.04     3.21
Col    20.61     5.29    24.27     0.04     8.16
Dia     9.14     4.00    35.93     0.07     4.62
Dts     8.65     1.19    24.10     0.12     2.18
Geb     8.24     2.25    40.44     0.07     2.65
Hag    15.94     4.73   197.82     0.05     6.33
Hus    24.26     7.40    28.33     0.07    10.68
Mul    49.37     6.06    35.78     0.04    13.42
Odi    17.56     1.99    25.03     0.06     2.89
Ros     7.35     1.02    43.28     0.11     1.92
Sav    11.62     3.82    28.83     0.05     4.78
Sel    22.33     3.89    29.83     0.07     5.63
Wis     7.95     2.87    29.66     0.08     2.62
HTP    23.02     4.95    20.71     0.18     8.84
NHC    12.80     4.85    53.35     0.15     5.31
Emr    13.70     4.08    36.34     0.04     6.82
Hsr     9.25       NA    29.78     0.07     3.04
Ccm       NA       NA       NA       NA       NA
```

Correlations

```
# test
cor(m$ahet5.m ~ m$ahet1.m)
cor.test(m$ahet5.m, m$ahet1.m)

# en utilisant le modèle linéaire
mod <- lm(m$ahet5.m ~ m$ahet1.m)
mod
summary(mod)

# tracé du graphe
plot(m$ahet5.m ~ m$ahet1.m, xlab = "Nombre de passages", ylab = "Nombre de patients présents à 15h", col = "red", pch = 16)

# tracé de la droite de corrélation
abline(mod, col = "blue")

# intervalle de confiance
a <- predict(mod, interval = "confidence")
lines(m$ahet1.m[-20], a[,2], lty=2, col = "blue")
lines(m$ahet1.m[-20], a[,3], lty=2, col = "blue")
