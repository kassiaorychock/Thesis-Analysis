---
title: "Thesis Analysis"
author: "Kassia Orychock"
date: '2018-09-17'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

This Rmd file will be used as the file for my Thesis Analysis on causal mediation.

# Libraries

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(car)
```

```
## Loading required package: carData
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```r
library(foreign)
```

# Selecting data

```r
setwd("/Users/kassiaorychock/Documents/Thesis/Analysis/Thesis-Analysis")
getwd()
```

```
## [1] "/Users/kassiaorychock/Documents/Thesis/Analysis/Thesis-Analysis"
```


# Reading of Raw Survey Data

```r
Database_recoded_2012_2014_weights_Walkscore_RTA <- read.csv("Database_recoded_2012-2014_weights_Walkscore_RTA.csv")

USDA <- read.csv("USDA_Data_FARA.csv")
```

# Filtering Out USA Data

```r
Boston <- filter(Database_recoded_2012_2014_weights_Walkscore_RTA, ville == "Boston")
BostonUSDA <- filter(USDA, County == "Suffolk")

Chicago <- filter(Database_recoded_2012_2014_weights_Walkscore_RTA, ville == "Chicago")

Detroit <- filter(Database_recoded_2012_2014_weights_Walkscore_RTA, ville == "Détroit")

NewYork <- filter(Database_recoded_2012_2014_weights_Walkscore_RTA, ville == "New-York")

Philadelphie <- filter(Database_recoded_2012_2014_weights_Walkscore_RTA, ville == "Philadelphie")
```

# Filtering Out Variables in USA Data

```r
Boston1 <- select(Boston, X, quest, lang, q54, ville, q1us, q1aut, q46b7, q46b9, q2, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22b, q22c, q22d, q22e, q22f_m1, q23, q24, q25, q26, q27, q28, q40, q41, q42, q44, q45, q46b, q47, q48, q49, q50, q51, Q52_occupational_status_category, q53, bmi, bmi_category, WalkScore, WalkScoreLabel, TransitScore, TransitScoreLabel, BikeScore, BikeScoreLabel, DiningandDrinkingScore, GroceryScore)

Chicago1 <- select(Boston, X, quest, lang, q54, ville, q1us, q1aut, q46b7, q46b9, q2, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22b, q22c, q22d, q22e, q22f_m1, q23, q24, q25, q26, q27, q28, q40, q41, q42, q44, q45, q46b, q47, q48, q49, q50, q51, Q52_occupational_status_category, q53, bmi, bmi_category, WalkScore, WalkScoreLabel, TransitScore, TransitScoreLabel, BikeScore, BikeScoreLabel, DiningandDrinkingScore, GroceryScore)

Detroit1 <- select(Boston, X, quest, lang, q54, ville, q1us, q1aut, q46b7, q46b9, q2, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22b, q22c, q22d, q22e, q22f_m1, q23, q24, q25, q26, q27, q28, q40, q41, q42, q44, q45, q46b, q47, q48, q49, q50, q51, Q52_occupational_status_category, q53, bmi, bmi_category, WalkScore, WalkScoreLabel, TransitScore, TransitScoreLabel, BikeScore, BikeScoreLabel, DiningandDrinkingScore, GroceryScore)

NewYork1 <- select(Boston, X, quest, lang, q54, ville, q1us, q1aut, q46b7, q46b9, q2, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22b, q22c, q22d, q22e, q22f_m1, q23, q24, q25, q26, q27, q28, q40, q41, q42, q44, q45, q46b, q47, q48, q49, q50, q51, Q52_occupational_status_category, q53, bmi, bmi_category, WalkScore, WalkScoreLabel, TransitScore, TransitScoreLabel, BikeScore, BikeScoreLabel, DiningandDrinkingScore, GroceryScore)

Philadelphie1 <- select(Boston, X, quest, lang, q54, ville, q1us, q1aut, q46b7, q46b9, q2, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22b, q22c, q22d, q22e, q22f_m1, q23, q24, q25, q26, q27, q28, q40, q41, q42, q44, q45, q46b, q47, q48, q49, q50, q51, Q52_occupational_status_category, q53, bmi, bmi_category, WalkScore, WalkScoreLabel, TransitScore, TransitScoreLabel, BikeScore, BikeScoreLabel, DiningandDrinkingScore, GroceryScore)
```


# Descriptive Stats for Boston

```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
Hmisc::describe(Boston1)
```

```
## Boston1 
## 
##  53  Variables      1977  Observations
## ---------------------------------------------------------------------------
## X 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1977        1     1011    703.9     99.8    198.6 
##      .25      .50      .75      .90      .95 
##    495.0    989.0   1483.0   1779.4   1878.2 
##                                                                       
## Value          0   200   400   600   800  1000  1200  1400  1600  1800
## Frequency    100   199   201   199   201   199   201   199   201   199
## Proportion 0.051 0.101 0.102 0.101 0.102 0.101 0.102 0.101 0.102 0.101
##                             
## Value       2000  7200 21400
## Frequency     75     1     2
## Proportion 0.038 0.001 0.001
## ---------------------------------------------------------------------------
## quest 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1975        1    20811    16141    610.8    962.6 
##      .25      .50      .75      .90      .95 
##   7057.0  20211.0  30188.0  33256.4  34680.0 
## 
## lowest :    10    11    13    32    38, highest: 73667 73668 73670 73675 73677
## ---------------------------------------------------------------------------
## lang 
##        n  missing distinct 
##     1977        0        2 
##                             
## Value       Anglais Espagnol
## Frequency      1974        3
## Proportion    0.998    0.002
## ---------------------------------------------------------------------------
## q54 
##        n  missing distinct 
##     1977        0        2 
##                       
## Value      Femme Homme
## Frequency   1283   694
## Proportion 0.649 0.351
## ---------------------------------------------------------------------------
## ville 
##        n  missing distinct    value 
##     1977        0        1   Boston 
##                  
## Value      Boston
## Frequency    1977
## Proportion      1
## ---------------------------------------------------------------------------
## q1us 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1970        7      183    0.999     2167    161.7     1910     2113 
##      .25      .50      .75      .90      .95 
##     2119     2134     2144     2445     2446 
##                                                                       
## Value       1400  1600  1800  2000  2200  2400  2600  3000  3200  3800
## Frequency      7    33    55    37  1600   210     2    15     6     4
## Proportion 0.004 0.017 0.028 0.019 0.812 0.107 0.001 0.008 0.003 0.002
##                 
## Value      20200
## Frequency      1
## Proportion 0.001
## ---------------------------------------------------------------------------
## q1aut 
##        n  missing distinct 
##     1977        0        9 
##                                                                          
## Value              01570  01581  01826  02138  02139  02446 N3C1G3 N3C3K9
## Frequency    1969      1      1      1      1      1      1      1      1
## Proportion  0.996  0.001  0.001  0.001  0.001  0.001  0.001  0.001  0.001
## ---------------------------------------------------------------------------
## q46b7 
##        n  missing distinct 
##     1969        8       17 
## 
## lowest : Allston-Brighton              Autre                         Back Bay/Beacon Hill/West End Cambridgeport                 Charlestown                  
## highest: Roxbury                       South Boston                  South Dorchester              South End/Chinatown           Wellington-Harrington        
## ---------------------------------------------------------------------------
## q46b9 
##        n  missing distinct 
##     1977        0       32 
## 
## lowest :    00 1A 1F 1M, highest: R1 R2 R3 S2 T7
## ---------------------------------------------------------------------------
## q2 
##        n  missing distinct 
##     1977        0        6 
## 
## Bon (521, 0.264), Excellent (467, 0.236), Mauvais (24, 0.012), Moyen (142,
## 0.072), Ne sais pas/Refuse de répondre (5, 0.003), Très bon (818, 0.414)
## ---------------------------------------------------------------------------
## q13 
##        n  missing distinct 
##     1977        0       10 
## 
## Autre (précisez:) (3, 0.002), Marche (500, 0.253), Ne s'applique pas (3,
## 0.002), Scooter (1, 0.001), Taxi (12, 0.006), Transport en commun (669,
## 0.338), Véhicule motorisé (loué, emprunté, covoiturage) (675, 0.341), Vélo
## en libre-service (15, 0.008), Vélo personnel (89, 0.045), Voiture
## personnelle (10, 0.005)
## ---------------------------------------------------------------------------
## q14 
##        n  missing distinct 
##     1977        0        4 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             18                           255
## Proportion                         0.009                         0.129
##                                                                       
## Value                                Oui            Refuse de répondre
## Frequency                           1703                             1
## Proportion                         0.861                         0.001
## ---------------------------------------------------------------------------
## q15 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.962    44.15    26.73        5        7 
##      .25      .50      .75      .90      .95 
##       20       57       64       66       68 
## 
## lowest :  1  2  3  4  5, highest: 70 71 96 98 99
## ---------------------------------------------------------------------------
## q18 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.992    50.68    35.54      5.1      7.0 
##      .25      .50      .75      .90      .95 
##     19.0     61.0     68.0     97.0     97.0 
## 
## lowest :  1  2  3  4  5, highest: 71 96 97 98 99
## ---------------------------------------------------------------------------
## q21 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.984    10.46    15.02        0        0 
##      .25      .50      .75      .90      .95 
##        2        4        7        7       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency    231   261   222   163   133   277   186   362   130    12
## Proportion 0.117 0.132 0.112 0.082 0.067 0.140 0.094 0.183 0.066 0.006
## ---------------------------------------------------------------------------
## q22b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      754     1223       14    0.862     2.09    1.577        1        1 
##      .25      .50      .75      .90      .95 
##        1        2        2        4        5 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8    10
## Frequency      9   361   227    69    37    14    14     2     3     8
## Proportion 0.012 0.479 0.301 0.092 0.049 0.019 0.019 0.003 0.004 0.011
##                                   
## Value         12    14    15    16
## Frequency      5     2     2     1
## Proportion 0.007 0.003 0.003 0.001
## ---------------------------------------------------------------------------
## q22c 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      476     1501       27    0.964    30.61    19.42        7       10 
##      .25      .50      .75      .90      .95 
##       20       30       40       50       70 
## 
## lowest :   0   1   2   3   4, highest:  75  80  90 120 150
## ---------------------------------------------------------------------------
## q22d 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      250     1727       19    0.957    4.216    4.405     1.00     1.00 
##      .25      .50      .75      .90      .95 
##     1.00     2.00     4.75    10.00    14.55 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency      1    73    62    33    18    13     9     5     3     2
## Proportion 0.004 0.292 0.248 0.132 0.072 0.052 0.036 0.020 0.012 0.008
##                                                                 
## Value         10    12    14    15    18    20    25    28    30
## Frequency     10     4     4     2     1     5     2     1     2
## Proportion 0.040 0.016 0.016 0.008 0.004 0.020 0.008 0.004 0.008
## ---------------------------------------------------------------------------
## q22e 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##       90     1887       18    0.916    34.57    23.74     7.25    14.50 
##      .25      .50      .75      .90      .95 
##    20.00    30.00    38.75    60.00    90.00 
##                                                                       
## Value          0     1     5    10    15    20    25    30    35    40
## Frequency      3     1     1     4     8     8     1    39     2     5
## Proportion 0.033 0.011 0.011 0.044 0.089 0.089 0.011 0.433 0.022 0.056
##                                                           
## Value         45    60    70    90   100   120   150   160
## Frequency      8     3     1     2     1     1     1     1
## Proportion 0.089 0.033 0.011 0.022 0.011 0.011 0.011 0.011
## ---------------------------------------------------------------------------
## q22f_m1 
##        n  missing distinct 
##       53     1924        2 
##                                                   
## Value      Ne sais pas/Pas sûr  Refuse de répondre
## Frequency                   49                   4
## Proportion               0.925               0.075
## ---------------------------------------------------------------------------
## q23 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.981    3.425    3.072        0        0 
##      .25      .50      .75      .90      .95 
##        1        3        5        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    344   354   210   130   120   336   134   165   167    17
## Proportion 0.174 0.179 0.106 0.066 0.061 0.170 0.068 0.083 0.084 0.009
## ---------------------------------------------------------------------------
## q25 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.962    4.845     2.61        0        1 
##      .25      .50      .75      .90      .95 
##        3        5        7        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    103   127   185   158   163   365   141   609   112    14
## Proportion 0.052 0.064 0.094 0.080 0.082 0.185 0.071 0.308 0.057 0.007
## ---------------------------------------------------------------------------
## q27 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.828    24.71    36.81        0        0 
##      .25      .50      .75      .90      .95 
##        0        0        7       98       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency   1069   141    70    47    39    69    25    32   469    16
## Proportion 0.541 0.071 0.035 0.024 0.020 0.035 0.013 0.016 0.237 0.008
## ---------------------------------------------------------------------------
## q42 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       67    0.999    37.21    15.17       21       23 
##      .25      .50      .75      .90      .95 
##       26       33       46       58       66 
## 
## lowest : 18 19 20 21 22, highest: 82 83 84 86 90
## ---------------------------------------------------------------------------
## q44 
##        n  missing distinct 
##     1977        0        7 
## 
## Célibatiare (1025, 0.518), Divorcé (e) (130, 0.066), En couple (8, 0.004),
## Marié(e)/Conjoint de fait (739, 0.374), Refuse de répondre (23, 0.012),
## Séparé (e) (28, 0.014), Veuf (ve) (24, 0.012)
## ---------------------------------------------------------------------------
## q45 
##        n  missing distinct     Info     Mean      Gmd 
##     1977        0        7    0.441    6.923     1.85 
##                                                     
## Value          1     2     3     4     5     8     9
## Frequency    209    88    23    10     1  1627    19
## Proportion 0.106 0.045 0.012 0.005 0.001 0.823 0.010
## ---------------------------------------------------------------------------
## q46b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      892     1085       23    0.975    4.834    3.407        1        2 
##      .25      .50      .75      .90      .95 
##        3        4        6        9       11 
## 
## lowest :  0  1  2  3  4, highest: 28 30 32 39 42
## ---------------------------------------------------------------------------
## q47 
##        n  missing distinct 
##     1977        0       11 
## 
## lowest : Amérindien des États-Unis / Autochtone d'Amérique Arabe (Moyen-Orient, Afrique du Nord)             Asiatique / insulaire du Pacifique                Autre                                             Blanc(che) / Caucasien                           
## highest: Indien / Pakistanais                              Je préfère ne pas répondre                        Jewish                                            Mixed / Mixed race / Bi-racial                    Noir(e) / Africain(e) / Afro-Américain(e)        
## ---------------------------------------------------------------------------
## q48 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       55    0.323    6.219    7.977        2        2 
##      .25      .50      .75      .90      .95 
##        2        2        2        3       35 
## 
## lowest :  1  2  3  4  5, highest: 73 74 80 96 99
## ---------------------------------------------------------------------------
## q49 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                              5                           166
## Proportion                         0.003                         0.084
##                                         
## Value                                Oui
## Frequency                           1806
## Proportion                         0.914
## ---------------------------------------------------------------------------
## q50 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             16                           519
## Proportion                         0.008                         0.263
##                                         
## Value                                Oui
## Frequency                           1442
## Proportion                         0.729
## ---------------------------------------------------------------------------
## q51 
##        n  missing distinct 
##     1977        0        8 
## 
## lowest : Autre (précisez):                                   Baccalauréat                                        Cégep                                               Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent        
## highest: Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent         Diplôme universitaire supérieur au baccalauréat     École primaire                                      Refuse de répondre                                 
## ---------------------------------------------------------------------------
## Q52_occupational_status_category 
##        n  missing distinct 
##     1963       14        7 
## 
## Disability (17, 0.009), Full time or Self-employed (1307, 0.666),
## Homemaker or parental leave (55, 0.028), Part time (135, 0.069), Retired
## (112, 0.057), Student (272, 0.139), Unemployed seeking work (65, 0.033)
## ---------------------------------------------------------------------------
## q53 
##        n  missing distinct 
##     1977        0       10 
## 
## lowest : Entre 10000 $ and 19999 $ par année   Entre 100000 $ and 149999 $ par année Entre 150000 $ and 199999 $ par année Entre 20000 $ and 34999 $ par année   Entre 35000 $ and 49999 $ par année  
## highest: Entre 50000 $ and 74999 $ par année   Entre 75000 $ and 99999 $ par année   Moins de 10000 $ par année            Plus de 200000 $ par année            Refuse de répondre                   
## ---------------------------------------------------------------------------
## bmi 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1785      192      617        1    25.06    5.006    19.37    20.12 
##      .25      .50      .75      .90      .95 
##    21.63    24.21    27.32    31.69    34.97 
## 
## lowest : 17.57 17.58 17.64 17.71 17.72, highest: 39.68 39.75 39.94 40.14 40.35
## ---------------------------------------------------------------------------
## bmi_category 
##        n  missing distinct 
##     1785      192        4 
##                                                           
## Value       embonpoint insuffisant      normal     obesite
## Frequency          511          44         982         248
## Proportion       0.286       0.025       0.550       0.139
## ---------------------------------------------------------------------------
## WalkScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38       76    0.993    86.19    14.02       53       71 
##      .25      .50      .75      .90      .95 
##       86       90       96       97       99 
## 
## lowest :  0  1  3  4  5, highest: 95 96 97 98 99
## ---------------------------------------------------------------------------
## WalkScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                            Car-Dependent Somewhat Walkable
## Frequency                 38                96                85
## Proportion             0.019             0.049             0.043
##                                               
## Value          Very Walkable Walker's Paradise
## Frequency                598              1160
## Proportion             0.302             0.587
## ---------------------------------------------------------------------------
## TransitScore 
##        n  missing distinct 
##     1977        0        7 
##                                                                       
## Value                          100          40          50          71
## Frequency           38          16           2          64         106
## Proportion       0.019       0.008       0.001       0.032       0.054
##                                   
## Value               79 Unavailable
## Frequency           95        1656
## Proportion       0.048       0.838
## ---------------------------------------------------------------------------
## TransitScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                        Excellent Transit      Good Transit
## Frequency               1694               201                64
## Proportion             0.857             0.102             0.032
##                                               
## Value       Rider's Paradise      Some Transit
## Frequency                 16                 2
## Proportion             0.008             0.001
## ---------------------------------------------------------------------------
## BikeScore 
##        n  missing distinct 
##     1977        0       31 
## 
## lowest :                        46                     54                     55                     58                    
## highest: 94                     97.741638389225798     98.650123169158107     99.406202578305795     Unavailable           
## ---------------------------------------------------------------------------
## BikeScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                                 Bikeable  Biker's Paradise
## Frequency                598               475               323
## Proportion             0.302             0.240             0.163
##                                               
## Value      Somewhat Bikeable     Very Bikeable
## Frequency                 26               555
## Proportion             0.013             0.281
## ---------------------------------------------------------------------------
## DiningandDrinkingScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      157    0.998    86.12    15.23    52.92    67.97 
##      .25      .50      .75      .90      .95 
##    83.20    89.31    96.09    99.31    99.44 
## 
## lowest :  0.0000000  0.9592180  0.9743672  1.8423924  2.2943511
## highest: 99.4366379 99.7015610 99.8348999 99.9046631 99.9225693
## ---------------------------------------------------------------------------
## GroceryScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      139    0.996    88.94    16.42    46.68    66.03 
##      .25      .50      .75      .90      .95 
##    88.13    97.15    99.37   100.00   100.00 
## 
## lowest :   0.000000   1.710128   1.956163   2.363304   2.792868
## highest:  99.455818  99.624977  99.752014  99.923592 100.000000
## ---------------------------------------------------------------------------
## 
## Variables with all observations missing:
## 
## [1] q16 q17 q19 q20 q24 q26 q28 q40 q41
```

```r
Hmisc::describe(Chicago1)
```

```
## Chicago1 
## 
##  53  Variables      1977  Observations
## ---------------------------------------------------------------------------
## X 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1977        1     1011    703.9     99.8    198.6 
##      .25      .50      .75      .90      .95 
##    495.0    989.0   1483.0   1779.4   1878.2 
##                                                                       
## Value          0   200   400   600   800  1000  1200  1400  1600  1800
## Frequency    100   199   201   199   201   199   201   199   201   199
## Proportion 0.051 0.101 0.102 0.101 0.102 0.101 0.102 0.101 0.102 0.101
##                             
## Value       2000  7200 21400
## Frequency     75     1     2
## Proportion 0.038 0.001 0.001
## ---------------------------------------------------------------------------
## quest 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1975        1    20811    16141    610.8    962.6 
##      .25      .50      .75      .90      .95 
##   7057.0  20211.0  30188.0  33256.4  34680.0 
## 
## lowest :    10    11    13    32    38, highest: 73667 73668 73670 73675 73677
## ---------------------------------------------------------------------------
## lang 
##        n  missing distinct 
##     1977        0        2 
##                             
## Value       Anglais Espagnol
## Frequency      1974        3
## Proportion    0.998    0.002
## ---------------------------------------------------------------------------
## q54 
##        n  missing distinct 
##     1977        0        2 
##                       
## Value      Femme Homme
## Frequency   1283   694
## Proportion 0.649 0.351
## ---------------------------------------------------------------------------
## ville 
##        n  missing distinct    value 
##     1977        0        1   Boston 
##                  
## Value      Boston
## Frequency    1977
## Proportion      1
## ---------------------------------------------------------------------------
## q1us 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1970        7      183    0.999     2167    161.7     1910     2113 
##      .25      .50      .75      .90      .95 
##     2119     2134     2144     2445     2446 
##                                                                       
## Value       1400  1600  1800  2000  2200  2400  2600  3000  3200  3800
## Frequency      7    33    55    37  1600   210     2    15     6     4
## Proportion 0.004 0.017 0.028 0.019 0.812 0.107 0.001 0.008 0.003 0.002
##                 
## Value      20200
## Frequency      1
## Proportion 0.001
## ---------------------------------------------------------------------------
## q1aut 
##        n  missing distinct 
##     1977        0        9 
##                                                                          
## Value              01570  01581  01826  02138  02139  02446 N3C1G3 N3C3K9
## Frequency    1969      1      1      1      1      1      1      1      1
## Proportion  0.996  0.001  0.001  0.001  0.001  0.001  0.001  0.001  0.001
## ---------------------------------------------------------------------------
## q46b7 
##        n  missing distinct 
##     1969        8       17 
## 
## lowest : Allston-Brighton              Autre                         Back Bay/Beacon Hill/West End Cambridgeport                 Charlestown                  
## highest: Roxbury                       South Boston                  South Dorchester              South End/Chinatown           Wellington-Harrington        
## ---------------------------------------------------------------------------
## q46b9 
##        n  missing distinct 
##     1977        0       32 
## 
## lowest :    00 1A 1F 1M, highest: R1 R2 R3 S2 T7
## ---------------------------------------------------------------------------
## q2 
##        n  missing distinct 
##     1977        0        6 
## 
## Bon (521, 0.264), Excellent (467, 0.236), Mauvais (24, 0.012), Moyen (142,
## 0.072), Ne sais pas/Refuse de répondre (5, 0.003), Très bon (818, 0.414)
## ---------------------------------------------------------------------------
## q13 
##        n  missing distinct 
##     1977        0       10 
## 
## Autre (précisez:) (3, 0.002), Marche (500, 0.253), Ne s'applique pas (3,
## 0.002), Scooter (1, 0.001), Taxi (12, 0.006), Transport en commun (669,
## 0.338), Véhicule motorisé (loué, emprunté, covoiturage) (675, 0.341), Vélo
## en libre-service (15, 0.008), Vélo personnel (89, 0.045), Voiture
## personnelle (10, 0.005)
## ---------------------------------------------------------------------------
## q14 
##        n  missing distinct 
##     1977        0        4 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             18                           255
## Proportion                         0.009                         0.129
##                                                                       
## Value                                Oui            Refuse de répondre
## Frequency                           1703                             1
## Proportion                         0.861                         0.001
## ---------------------------------------------------------------------------
## q15 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.962    44.15    26.73        5        7 
##      .25      .50      .75      .90      .95 
##       20       57       64       66       68 
## 
## lowest :  1  2  3  4  5, highest: 70 71 96 98 99
## ---------------------------------------------------------------------------
## q18 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.992    50.68    35.54      5.1      7.0 
##      .25      .50      .75      .90      .95 
##     19.0     61.0     68.0     97.0     97.0 
## 
## lowest :  1  2  3  4  5, highest: 71 96 97 98 99
## ---------------------------------------------------------------------------
## q21 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.984    10.46    15.02        0        0 
##      .25      .50      .75      .90      .95 
##        2        4        7        7       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency    231   261   222   163   133   277   186   362   130    12
## Proportion 0.117 0.132 0.112 0.082 0.067 0.140 0.094 0.183 0.066 0.006
## ---------------------------------------------------------------------------
## q22b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      754     1223       14    0.862     2.09    1.577        1        1 
##      .25      .50      .75      .90      .95 
##        1        2        2        4        5 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8    10
## Frequency      9   361   227    69    37    14    14     2     3     8
## Proportion 0.012 0.479 0.301 0.092 0.049 0.019 0.019 0.003 0.004 0.011
##                                   
## Value         12    14    15    16
## Frequency      5     2     2     1
## Proportion 0.007 0.003 0.003 0.001
## ---------------------------------------------------------------------------
## q22c 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      476     1501       27    0.964    30.61    19.42        7       10 
##      .25      .50      .75      .90      .95 
##       20       30       40       50       70 
## 
## lowest :   0   1   2   3   4, highest:  75  80  90 120 150
## ---------------------------------------------------------------------------
## q22d 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      250     1727       19    0.957    4.216    4.405     1.00     1.00 
##      .25      .50      .75      .90      .95 
##     1.00     2.00     4.75    10.00    14.55 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency      1    73    62    33    18    13     9     5     3     2
## Proportion 0.004 0.292 0.248 0.132 0.072 0.052 0.036 0.020 0.012 0.008
##                                                                 
## Value         10    12    14    15    18    20    25    28    30
## Frequency     10     4     4     2     1     5     2     1     2
## Proportion 0.040 0.016 0.016 0.008 0.004 0.020 0.008 0.004 0.008
## ---------------------------------------------------------------------------
## q22e 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##       90     1887       18    0.916    34.57    23.74     7.25    14.50 
##      .25      .50      .75      .90      .95 
##    20.00    30.00    38.75    60.00    90.00 
##                                                                       
## Value          0     1     5    10    15    20    25    30    35    40
## Frequency      3     1     1     4     8     8     1    39     2     5
## Proportion 0.033 0.011 0.011 0.044 0.089 0.089 0.011 0.433 0.022 0.056
##                                                           
## Value         45    60    70    90   100   120   150   160
## Frequency      8     3     1     2     1     1     1     1
## Proportion 0.089 0.033 0.011 0.022 0.011 0.011 0.011 0.011
## ---------------------------------------------------------------------------
## q22f_m1 
##        n  missing distinct 
##       53     1924        2 
##                                                   
## Value      Ne sais pas/Pas sûr  Refuse de répondre
## Frequency                   49                   4
## Proportion               0.925               0.075
## ---------------------------------------------------------------------------
## q23 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.981    3.425    3.072        0        0 
##      .25      .50      .75      .90      .95 
##        1        3        5        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    344   354   210   130   120   336   134   165   167    17
## Proportion 0.174 0.179 0.106 0.066 0.061 0.170 0.068 0.083 0.084 0.009
## ---------------------------------------------------------------------------
## q25 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.962    4.845     2.61        0        1 
##      .25      .50      .75      .90      .95 
##        3        5        7        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    103   127   185   158   163   365   141   609   112    14
## Proportion 0.052 0.064 0.094 0.080 0.082 0.185 0.071 0.308 0.057 0.007
## ---------------------------------------------------------------------------
## q27 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.828    24.71    36.81        0        0 
##      .25      .50      .75      .90      .95 
##        0        0        7       98       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency   1069   141    70    47    39    69    25    32   469    16
## Proportion 0.541 0.071 0.035 0.024 0.020 0.035 0.013 0.016 0.237 0.008
## ---------------------------------------------------------------------------
## q42 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       67    0.999    37.21    15.17       21       23 
##      .25      .50      .75      .90      .95 
##       26       33       46       58       66 
## 
## lowest : 18 19 20 21 22, highest: 82 83 84 86 90
## ---------------------------------------------------------------------------
## q44 
##        n  missing distinct 
##     1977        0        7 
## 
## Célibatiare (1025, 0.518), Divorcé (e) (130, 0.066), En couple (8, 0.004),
## Marié(e)/Conjoint de fait (739, 0.374), Refuse de répondre (23, 0.012),
## Séparé (e) (28, 0.014), Veuf (ve) (24, 0.012)
## ---------------------------------------------------------------------------
## q45 
##        n  missing distinct     Info     Mean      Gmd 
##     1977        0        7    0.441    6.923     1.85 
##                                                     
## Value          1     2     3     4     5     8     9
## Frequency    209    88    23    10     1  1627    19
## Proportion 0.106 0.045 0.012 0.005 0.001 0.823 0.010
## ---------------------------------------------------------------------------
## q46b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      892     1085       23    0.975    4.834    3.407        1        2 
##      .25      .50      .75      .90      .95 
##        3        4        6        9       11 
## 
## lowest :  0  1  2  3  4, highest: 28 30 32 39 42
## ---------------------------------------------------------------------------
## q47 
##        n  missing distinct 
##     1977        0       11 
## 
## lowest : Amérindien des États-Unis / Autochtone d'Amérique Arabe (Moyen-Orient, Afrique du Nord)             Asiatique / insulaire du Pacifique                Autre                                             Blanc(che) / Caucasien                           
## highest: Indien / Pakistanais                              Je préfère ne pas répondre                        Jewish                                            Mixed / Mixed race / Bi-racial                    Noir(e) / Africain(e) / Afro-Américain(e)        
## ---------------------------------------------------------------------------
## q48 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       55    0.323    6.219    7.977        2        2 
##      .25      .50      .75      .90      .95 
##        2        2        2        3       35 
## 
## lowest :  1  2  3  4  5, highest: 73 74 80 96 99
## ---------------------------------------------------------------------------
## q49 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                              5                           166
## Proportion                         0.003                         0.084
##                                         
## Value                                Oui
## Frequency                           1806
## Proportion                         0.914
## ---------------------------------------------------------------------------
## q50 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             16                           519
## Proportion                         0.008                         0.263
##                                         
## Value                                Oui
## Frequency                           1442
## Proportion                         0.729
## ---------------------------------------------------------------------------
## q51 
##        n  missing distinct 
##     1977        0        8 
## 
## lowest : Autre (précisez):                                   Baccalauréat                                        Cégep                                               Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent        
## highest: Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent         Diplôme universitaire supérieur au baccalauréat     École primaire                                      Refuse de répondre                                 
## ---------------------------------------------------------------------------
## Q52_occupational_status_category 
##        n  missing distinct 
##     1963       14        7 
## 
## Disability (17, 0.009), Full time or Self-employed (1307, 0.666),
## Homemaker or parental leave (55, 0.028), Part time (135, 0.069), Retired
## (112, 0.057), Student (272, 0.139), Unemployed seeking work (65, 0.033)
## ---------------------------------------------------------------------------
## q53 
##        n  missing distinct 
##     1977        0       10 
## 
## lowest : Entre 10000 $ and 19999 $ par année   Entre 100000 $ and 149999 $ par année Entre 150000 $ and 199999 $ par année Entre 20000 $ and 34999 $ par année   Entre 35000 $ and 49999 $ par année  
## highest: Entre 50000 $ and 74999 $ par année   Entre 75000 $ and 99999 $ par année   Moins de 10000 $ par année            Plus de 200000 $ par année            Refuse de répondre                   
## ---------------------------------------------------------------------------
## bmi 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1785      192      617        1    25.06    5.006    19.37    20.12 
##      .25      .50      .75      .90      .95 
##    21.63    24.21    27.32    31.69    34.97 
## 
## lowest : 17.57 17.58 17.64 17.71 17.72, highest: 39.68 39.75 39.94 40.14 40.35
## ---------------------------------------------------------------------------
## bmi_category 
##        n  missing distinct 
##     1785      192        4 
##                                                           
## Value       embonpoint insuffisant      normal     obesite
## Frequency          511          44         982         248
## Proportion       0.286       0.025       0.550       0.139
## ---------------------------------------------------------------------------
## WalkScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38       76    0.993    86.19    14.02       53       71 
##      .25      .50      .75      .90      .95 
##       86       90       96       97       99 
## 
## lowest :  0  1  3  4  5, highest: 95 96 97 98 99
## ---------------------------------------------------------------------------
## WalkScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                            Car-Dependent Somewhat Walkable
## Frequency                 38                96                85
## Proportion             0.019             0.049             0.043
##                                               
## Value          Very Walkable Walker's Paradise
## Frequency                598              1160
## Proportion             0.302             0.587
## ---------------------------------------------------------------------------
## TransitScore 
##        n  missing distinct 
##     1977        0        7 
##                                                                       
## Value                          100          40          50          71
## Frequency           38          16           2          64         106
## Proportion       0.019       0.008       0.001       0.032       0.054
##                                   
## Value               79 Unavailable
## Frequency           95        1656
## Proportion       0.048       0.838
## ---------------------------------------------------------------------------
## TransitScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                        Excellent Transit      Good Transit
## Frequency               1694               201                64
## Proportion             0.857             0.102             0.032
##                                               
## Value       Rider's Paradise      Some Transit
## Frequency                 16                 2
## Proportion             0.008             0.001
## ---------------------------------------------------------------------------
## BikeScore 
##        n  missing distinct 
##     1977        0       31 
## 
## lowest :                        46                     54                     55                     58                    
## highest: 94                     97.741638389225798     98.650123169158107     99.406202578305795     Unavailable           
## ---------------------------------------------------------------------------
## BikeScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                                 Bikeable  Biker's Paradise
## Frequency                598               475               323
## Proportion             0.302             0.240             0.163
##                                               
## Value      Somewhat Bikeable     Very Bikeable
## Frequency                 26               555
## Proportion             0.013             0.281
## ---------------------------------------------------------------------------
## DiningandDrinkingScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      157    0.998    86.12    15.23    52.92    67.97 
##      .25      .50      .75      .90      .95 
##    83.20    89.31    96.09    99.31    99.44 
## 
## lowest :  0.0000000  0.9592180  0.9743672  1.8423924  2.2943511
## highest: 99.4366379 99.7015610 99.8348999 99.9046631 99.9225693
## ---------------------------------------------------------------------------
## GroceryScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      139    0.996    88.94    16.42    46.68    66.03 
##      .25      .50      .75      .90      .95 
##    88.13    97.15    99.37   100.00   100.00 
## 
## lowest :   0.000000   1.710128   1.956163   2.363304   2.792868
## highest:  99.455818  99.624977  99.752014  99.923592 100.000000
## ---------------------------------------------------------------------------
## 
## Variables with all observations missing:
## 
## [1] q16 q17 q19 q20 q24 q26 q28 q40 q41
```

```r
Hmisc::describe(Detroit1)
```

```
## Detroit1 
## 
##  53  Variables      1977  Observations
## ---------------------------------------------------------------------------
## X 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1977        1     1011    703.9     99.8    198.6 
##      .25      .50      .75      .90      .95 
##    495.0    989.0   1483.0   1779.4   1878.2 
##                                                                       
## Value          0   200   400   600   800  1000  1200  1400  1600  1800
## Frequency    100   199   201   199   201   199   201   199   201   199
## Proportion 0.051 0.101 0.102 0.101 0.102 0.101 0.102 0.101 0.102 0.101
##                             
## Value       2000  7200 21400
## Frequency     75     1     2
## Proportion 0.038 0.001 0.001
## ---------------------------------------------------------------------------
## quest 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1975        1    20811    16141    610.8    962.6 
##      .25      .50      .75      .90      .95 
##   7057.0  20211.0  30188.0  33256.4  34680.0 
## 
## lowest :    10    11    13    32    38, highest: 73667 73668 73670 73675 73677
## ---------------------------------------------------------------------------
## lang 
##        n  missing distinct 
##     1977        0        2 
##                             
## Value       Anglais Espagnol
## Frequency      1974        3
## Proportion    0.998    0.002
## ---------------------------------------------------------------------------
## q54 
##        n  missing distinct 
##     1977        0        2 
##                       
## Value      Femme Homme
## Frequency   1283   694
## Proportion 0.649 0.351
## ---------------------------------------------------------------------------
## ville 
##        n  missing distinct    value 
##     1977        0        1   Boston 
##                  
## Value      Boston
## Frequency    1977
## Proportion      1
## ---------------------------------------------------------------------------
## q1us 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1970        7      183    0.999     2167    161.7     1910     2113 
##      .25      .50      .75      .90      .95 
##     2119     2134     2144     2445     2446 
##                                                                       
## Value       1400  1600  1800  2000  2200  2400  2600  3000  3200  3800
## Frequency      7    33    55    37  1600   210     2    15     6     4
## Proportion 0.004 0.017 0.028 0.019 0.812 0.107 0.001 0.008 0.003 0.002
##                 
## Value      20200
## Frequency      1
## Proportion 0.001
## ---------------------------------------------------------------------------
## q1aut 
##        n  missing distinct 
##     1977        0        9 
##                                                                          
## Value              01570  01581  01826  02138  02139  02446 N3C1G3 N3C3K9
## Frequency    1969      1      1      1      1      1      1      1      1
## Proportion  0.996  0.001  0.001  0.001  0.001  0.001  0.001  0.001  0.001
## ---------------------------------------------------------------------------
## q46b7 
##        n  missing distinct 
##     1969        8       17 
## 
## lowest : Allston-Brighton              Autre                         Back Bay/Beacon Hill/West End Cambridgeport                 Charlestown                  
## highest: Roxbury                       South Boston                  South Dorchester              South End/Chinatown           Wellington-Harrington        
## ---------------------------------------------------------------------------
## q46b9 
##        n  missing distinct 
##     1977        0       32 
## 
## lowest :    00 1A 1F 1M, highest: R1 R2 R3 S2 T7
## ---------------------------------------------------------------------------
## q2 
##        n  missing distinct 
##     1977        0        6 
## 
## Bon (521, 0.264), Excellent (467, 0.236), Mauvais (24, 0.012), Moyen (142,
## 0.072), Ne sais pas/Refuse de répondre (5, 0.003), Très bon (818, 0.414)
## ---------------------------------------------------------------------------
## q13 
##        n  missing distinct 
##     1977        0       10 
## 
## Autre (précisez:) (3, 0.002), Marche (500, 0.253), Ne s'applique pas (3,
## 0.002), Scooter (1, 0.001), Taxi (12, 0.006), Transport en commun (669,
## 0.338), Véhicule motorisé (loué, emprunté, covoiturage) (675, 0.341), Vélo
## en libre-service (15, 0.008), Vélo personnel (89, 0.045), Voiture
## personnelle (10, 0.005)
## ---------------------------------------------------------------------------
## q14 
##        n  missing distinct 
##     1977        0        4 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             18                           255
## Proportion                         0.009                         0.129
##                                                                       
## Value                                Oui            Refuse de répondre
## Frequency                           1703                             1
## Proportion                         0.861                         0.001
## ---------------------------------------------------------------------------
## q15 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.962    44.15    26.73        5        7 
##      .25      .50      .75      .90      .95 
##       20       57       64       66       68 
## 
## lowest :  1  2  3  4  5, highest: 70 71 96 98 99
## ---------------------------------------------------------------------------
## q18 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.992    50.68    35.54      5.1      7.0 
##      .25      .50      .75      .90      .95 
##     19.0     61.0     68.0     97.0     97.0 
## 
## lowest :  1  2  3  4  5, highest: 71 96 97 98 99
## ---------------------------------------------------------------------------
## q21 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.984    10.46    15.02        0        0 
##      .25      .50      .75      .90      .95 
##        2        4        7        7       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency    231   261   222   163   133   277   186   362   130    12
## Proportion 0.117 0.132 0.112 0.082 0.067 0.140 0.094 0.183 0.066 0.006
## ---------------------------------------------------------------------------
## q22b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      754     1223       14    0.862     2.09    1.577        1        1 
##      .25      .50      .75      .90      .95 
##        1        2        2        4        5 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8    10
## Frequency      9   361   227    69    37    14    14     2     3     8
## Proportion 0.012 0.479 0.301 0.092 0.049 0.019 0.019 0.003 0.004 0.011
##                                   
## Value         12    14    15    16
## Frequency      5     2     2     1
## Proportion 0.007 0.003 0.003 0.001
## ---------------------------------------------------------------------------
## q22c 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      476     1501       27    0.964    30.61    19.42        7       10 
##      .25      .50      .75      .90      .95 
##       20       30       40       50       70 
## 
## lowest :   0   1   2   3   4, highest:  75  80  90 120 150
## ---------------------------------------------------------------------------
## q22d 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      250     1727       19    0.957    4.216    4.405     1.00     1.00 
##      .25      .50      .75      .90      .95 
##     1.00     2.00     4.75    10.00    14.55 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency      1    73    62    33    18    13     9     5     3     2
## Proportion 0.004 0.292 0.248 0.132 0.072 0.052 0.036 0.020 0.012 0.008
##                                                                 
## Value         10    12    14    15    18    20    25    28    30
## Frequency     10     4     4     2     1     5     2     1     2
## Proportion 0.040 0.016 0.016 0.008 0.004 0.020 0.008 0.004 0.008
## ---------------------------------------------------------------------------
## q22e 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##       90     1887       18    0.916    34.57    23.74     7.25    14.50 
##      .25      .50      .75      .90      .95 
##    20.00    30.00    38.75    60.00    90.00 
##                                                                       
## Value          0     1     5    10    15    20    25    30    35    40
## Frequency      3     1     1     4     8     8     1    39     2     5
## Proportion 0.033 0.011 0.011 0.044 0.089 0.089 0.011 0.433 0.022 0.056
##                                                           
## Value         45    60    70    90   100   120   150   160
## Frequency      8     3     1     2     1     1     1     1
## Proportion 0.089 0.033 0.011 0.022 0.011 0.011 0.011 0.011
## ---------------------------------------------------------------------------
## q22f_m1 
##        n  missing distinct 
##       53     1924        2 
##                                                   
## Value      Ne sais pas/Pas sûr  Refuse de répondre
## Frequency                   49                   4
## Proportion               0.925               0.075
## ---------------------------------------------------------------------------
## q23 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.981    3.425    3.072        0        0 
##      .25      .50      .75      .90      .95 
##        1        3        5        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    344   354   210   130   120   336   134   165   167    17
## Proportion 0.174 0.179 0.106 0.066 0.061 0.170 0.068 0.083 0.084 0.009
## ---------------------------------------------------------------------------
## q25 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.962    4.845     2.61        0        1 
##      .25      .50      .75      .90      .95 
##        3        5        7        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    103   127   185   158   163   365   141   609   112    14
## Proportion 0.052 0.064 0.094 0.080 0.082 0.185 0.071 0.308 0.057 0.007
## ---------------------------------------------------------------------------
## q27 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.828    24.71    36.81        0        0 
##      .25      .50      .75      .90      .95 
##        0        0        7       98       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency   1069   141    70    47    39    69    25    32   469    16
## Proportion 0.541 0.071 0.035 0.024 0.020 0.035 0.013 0.016 0.237 0.008
## ---------------------------------------------------------------------------
## q42 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       67    0.999    37.21    15.17       21       23 
##      .25      .50      .75      .90      .95 
##       26       33       46       58       66 
## 
## lowest : 18 19 20 21 22, highest: 82 83 84 86 90
## ---------------------------------------------------------------------------
## q44 
##        n  missing distinct 
##     1977        0        7 
## 
## Célibatiare (1025, 0.518), Divorcé (e) (130, 0.066), En couple (8, 0.004),
## Marié(e)/Conjoint de fait (739, 0.374), Refuse de répondre (23, 0.012),
## Séparé (e) (28, 0.014), Veuf (ve) (24, 0.012)
## ---------------------------------------------------------------------------
## q45 
##        n  missing distinct     Info     Mean      Gmd 
##     1977        0        7    0.441    6.923     1.85 
##                                                     
## Value          1     2     3     4     5     8     9
## Frequency    209    88    23    10     1  1627    19
## Proportion 0.106 0.045 0.012 0.005 0.001 0.823 0.010
## ---------------------------------------------------------------------------
## q46b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      892     1085       23    0.975    4.834    3.407        1        2 
##      .25      .50      .75      .90      .95 
##        3        4        6        9       11 
## 
## lowest :  0  1  2  3  4, highest: 28 30 32 39 42
## ---------------------------------------------------------------------------
## q47 
##        n  missing distinct 
##     1977        0       11 
## 
## lowest : Amérindien des États-Unis / Autochtone d'Amérique Arabe (Moyen-Orient, Afrique du Nord)             Asiatique / insulaire du Pacifique                Autre                                             Blanc(che) / Caucasien                           
## highest: Indien / Pakistanais                              Je préfère ne pas répondre                        Jewish                                            Mixed / Mixed race / Bi-racial                    Noir(e) / Africain(e) / Afro-Américain(e)        
## ---------------------------------------------------------------------------
## q48 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       55    0.323    6.219    7.977        2        2 
##      .25      .50      .75      .90      .95 
##        2        2        2        3       35 
## 
## lowest :  1  2  3  4  5, highest: 73 74 80 96 99
## ---------------------------------------------------------------------------
## q49 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                              5                           166
## Proportion                         0.003                         0.084
##                                         
## Value                                Oui
## Frequency                           1806
## Proportion                         0.914
## ---------------------------------------------------------------------------
## q50 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             16                           519
## Proportion                         0.008                         0.263
##                                         
## Value                                Oui
## Frequency                           1442
## Proportion                         0.729
## ---------------------------------------------------------------------------
## q51 
##        n  missing distinct 
##     1977        0        8 
## 
## lowest : Autre (précisez):                                   Baccalauréat                                        Cégep                                               Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent        
## highest: Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent         Diplôme universitaire supérieur au baccalauréat     École primaire                                      Refuse de répondre                                 
## ---------------------------------------------------------------------------
## Q52_occupational_status_category 
##        n  missing distinct 
##     1963       14        7 
## 
## Disability (17, 0.009), Full time or Self-employed (1307, 0.666),
## Homemaker or parental leave (55, 0.028), Part time (135, 0.069), Retired
## (112, 0.057), Student (272, 0.139), Unemployed seeking work (65, 0.033)
## ---------------------------------------------------------------------------
## q53 
##        n  missing distinct 
##     1977        0       10 
## 
## lowest : Entre 10000 $ and 19999 $ par année   Entre 100000 $ and 149999 $ par année Entre 150000 $ and 199999 $ par année Entre 20000 $ and 34999 $ par année   Entre 35000 $ and 49999 $ par année  
## highest: Entre 50000 $ and 74999 $ par année   Entre 75000 $ and 99999 $ par année   Moins de 10000 $ par année            Plus de 200000 $ par année            Refuse de répondre                   
## ---------------------------------------------------------------------------
## bmi 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1785      192      617        1    25.06    5.006    19.37    20.12 
##      .25      .50      .75      .90      .95 
##    21.63    24.21    27.32    31.69    34.97 
## 
## lowest : 17.57 17.58 17.64 17.71 17.72, highest: 39.68 39.75 39.94 40.14 40.35
## ---------------------------------------------------------------------------
## bmi_category 
##        n  missing distinct 
##     1785      192        4 
##                                                           
## Value       embonpoint insuffisant      normal     obesite
## Frequency          511          44         982         248
## Proportion       0.286       0.025       0.550       0.139
## ---------------------------------------------------------------------------
## WalkScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38       76    0.993    86.19    14.02       53       71 
##      .25      .50      .75      .90      .95 
##       86       90       96       97       99 
## 
## lowest :  0  1  3  4  5, highest: 95 96 97 98 99
## ---------------------------------------------------------------------------
## WalkScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                            Car-Dependent Somewhat Walkable
## Frequency                 38                96                85
## Proportion             0.019             0.049             0.043
##                                               
## Value          Very Walkable Walker's Paradise
## Frequency                598              1160
## Proportion             0.302             0.587
## ---------------------------------------------------------------------------
## TransitScore 
##        n  missing distinct 
##     1977        0        7 
##                                                                       
## Value                          100          40          50          71
## Frequency           38          16           2          64         106
## Proportion       0.019       0.008       0.001       0.032       0.054
##                                   
## Value               79 Unavailable
## Frequency           95        1656
## Proportion       0.048       0.838
## ---------------------------------------------------------------------------
## TransitScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                        Excellent Transit      Good Transit
## Frequency               1694               201                64
## Proportion             0.857             0.102             0.032
##                                               
## Value       Rider's Paradise      Some Transit
## Frequency                 16                 2
## Proportion             0.008             0.001
## ---------------------------------------------------------------------------
## BikeScore 
##        n  missing distinct 
##     1977        0       31 
## 
## lowest :                        46                     54                     55                     58                    
## highest: 94                     97.741638389225798     98.650123169158107     99.406202578305795     Unavailable           
## ---------------------------------------------------------------------------
## BikeScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                                 Bikeable  Biker's Paradise
## Frequency                598               475               323
## Proportion             0.302             0.240             0.163
##                                               
## Value      Somewhat Bikeable     Very Bikeable
## Frequency                 26               555
## Proportion             0.013             0.281
## ---------------------------------------------------------------------------
## DiningandDrinkingScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      157    0.998    86.12    15.23    52.92    67.97 
##      .25      .50      .75      .90      .95 
##    83.20    89.31    96.09    99.31    99.44 
## 
## lowest :  0.0000000  0.9592180  0.9743672  1.8423924  2.2943511
## highest: 99.4366379 99.7015610 99.8348999 99.9046631 99.9225693
## ---------------------------------------------------------------------------
## GroceryScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      139    0.996    88.94    16.42    46.68    66.03 
##      .25      .50      .75      .90      .95 
##    88.13    97.15    99.37   100.00   100.00 
## 
## lowest :   0.000000   1.710128   1.956163   2.363304   2.792868
## highest:  99.455818  99.624977  99.752014  99.923592 100.000000
## ---------------------------------------------------------------------------
## 
## Variables with all observations missing:
## 
## [1] q16 q17 q19 q20 q24 q26 q28 q40 q41
```

```r
Hmisc::describe(NewYork1)
```

```
## NewYork1 
## 
##  53  Variables      1977  Observations
## ---------------------------------------------------------------------------
## X 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1977        1     1011    703.9     99.8    198.6 
##      .25      .50      .75      .90      .95 
##    495.0    989.0   1483.0   1779.4   1878.2 
##                                                                       
## Value          0   200   400   600   800  1000  1200  1400  1600  1800
## Frequency    100   199   201   199   201   199   201   199   201   199
## Proportion 0.051 0.101 0.102 0.101 0.102 0.101 0.102 0.101 0.102 0.101
##                             
## Value       2000  7200 21400
## Frequency     75     1     2
## Proportion 0.038 0.001 0.001
## ---------------------------------------------------------------------------
## quest 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1975        1    20811    16141    610.8    962.6 
##      .25      .50      .75      .90      .95 
##   7057.0  20211.0  30188.0  33256.4  34680.0 
## 
## lowest :    10    11    13    32    38, highest: 73667 73668 73670 73675 73677
## ---------------------------------------------------------------------------
## lang 
##        n  missing distinct 
##     1977        0        2 
##                             
## Value       Anglais Espagnol
## Frequency      1974        3
## Proportion    0.998    0.002
## ---------------------------------------------------------------------------
## q54 
##        n  missing distinct 
##     1977        0        2 
##                       
## Value      Femme Homme
## Frequency   1283   694
## Proportion 0.649 0.351
## ---------------------------------------------------------------------------
## ville 
##        n  missing distinct    value 
##     1977        0        1   Boston 
##                  
## Value      Boston
## Frequency    1977
## Proportion      1
## ---------------------------------------------------------------------------
## q1us 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1970        7      183    0.999     2167    161.7     1910     2113 
##      .25      .50      .75      .90      .95 
##     2119     2134     2144     2445     2446 
##                                                                       
## Value       1400  1600  1800  2000  2200  2400  2600  3000  3200  3800
## Frequency      7    33    55    37  1600   210     2    15     6     4
## Proportion 0.004 0.017 0.028 0.019 0.812 0.107 0.001 0.008 0.003 0.002
##                 
## Value      20200
## Frequency      1
## Proportion 0.001
## ---------------------------------------------------------------------------
## q1aut 
##        n  missing distinct 
##     1977        0        9 
##                                                                          
## Value              01570  01581  01826  02138  02139  02446 N3C1G3 N3C3K9
## Frequency    1969      1      1      1      1      1      1      1      1
## Proportion  0.996  0.001  0.001  0.001  0.001  0.001  0.001  0.001  0.001
## ---------------------------------------------------------------------------
## q46b7 
##        n  missing distinct 
##     1969        8       17 
## 
## lowest : Allston-Brighton              Autre                         Back Bay/Beacon Hill/West End Cambridgeport                 Charlestown                  
## highest: Roxbury                       South Boston                  South Dorchester              South End/Chinatown           Wellington-Harrington        
## ---------------------------------------------------------------------------
## q46b9 
##        n  missing distinct 
##     1977        0       32 
## 
## lowest :    00 1A 1F 1M, highest: R1 R2 R3 S2 T7
## ---------------------------------------------------------------------------
## q2 
##        n  missing distinct 
##     1977        0        6 
## 
## Bon (521, 0.264), Excellent (467, 0.236), Mauvais (24, 0.012), Moyen (142,
## 0.072), Ne sais pas/Refuse de répondre (5, 0.003), Très bon (818, 0.414)
## ---------------------------------------------------------------------------
## q13 
##        n  missing distinct 
##     1977        0       10 
## 
## Autre (précisez:) (3, 0.002), Marche (500, 0.253), Ne s'applique pas (3,
## 0.002), Scooter (1, 0.001), Taxi (12, 0.006), Transport en commun (669,
## 0.338), Véhicule motorisé (loué, emprunté, covoiturage) (675, 0.341), Vélo
## en libre-service (15, 0.008), Vélo personnel (89, 0.045), Voiture
## personnelle (10, 0.005)
## ---------------------------------------------------------------------------
## q14 
##        n  missing distinct 
##     1977        0        4 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             18                           255
## Proportion                         0.009                         0.129
##                                                                       
## Value                                Oui            Refuse de répondre
## Frequency                           1703                             1
## Proportion                         0.861                         0.001
## ---------------------------------------------------------------------------
## q15 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.962    44.15    26.73        5        7 
##      .25      .50      .75      .90      .95 
##       20       57       64       66       68 
## 
## lowest :  1  2  3  4  5, highest: 70 71 96 98 99
## ---------------------------------------------------------------------------
## q18 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.992    50.68    35.54      5.1      7.0 
##      .25      .50      .75      .90      .95 
##     19.0     61.0     68.0     97.0     97.0 
## 
## lowest :  1  2  3  4  5, highest: 71 96 97 98 99
## ---------------------------------------------------------------------------
## q21 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.984    10.46    15.02        0        0 
##      .25      .50      .75      .90      .95 
##        2        4        7        7       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency    231   261   222   163   133   277   186   362   130    12
## Proportion 0.117 0.132 0.112 0.082 0.067 0.140 0.094 0.183 0.066 0.006
## ---------------------------------------------------------------------------
## q22b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      754     1223       14    0.862     2.09    1.577        1        1 
##      .25      .50      .75      .90      .95 
##        1        2        2        4        5 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8    10
## Frequency      9   361   227    69    37    14    14     2     3     8
## Proportion 0.012 0.479 0.301 0.092 0.049 0.019 0.019 0.003 0.004 0.011
##                                   
## Value         12    14    15    16
## Frequency      5     2     2     1
## Proportion 0.007 0.003 0.003 0.001
## ---------------------------------------------------------------------------
## q22c 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      476     1501       27    0.964    30.61    19.42        7       10 
##      .25      .50      .75      .90      .95 
##       20       30       40       50       70 
## 
## lowest :   0   1   2   3   4, highest:  75  80  90 120 150
## ---------------------------------------------------------------------------
## q22d 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      250     1727       19    0.957    4.216    4.405     1.00     1.00 
##      .25      .50      .75      .90      .95 
##     1.00     2.00     4.75    10.00    14.55 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency      1    73    62    33    18    13     9     5     3     2
## Proportion 0.004 0.292 0.248 0.132 0.072 0.052 0.036 0.020 0.012 0.008
##                                                                 
## Value         10    12    14    15    18    20    25    28    30
## Frequency     10     4     4     2     1     5     2     1     2
## Proportion 0.040 0.016 0.016 0.008 0.004 0.020 0.008 0.004 0.008
## ---------------------------------------------------------------------------
## q22e 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##       90     1887       18    0.916    34.57    23.74     7.25    14.50 
##      .25      .50      .75      .90      .95 
##    20.00    30.00    38.75    60.00    90.00 
##                                                                       
## Value          0     1     5    10    15    20    25    30    35    40
## Frequency      3     1     1     4     8     8     1    39     2     5
## Proportion 0.033 0.011 0.011 0.044 0.089 0.089 0.011 0.433 0.022 0.056
##                                                           
## Value         45    60    70    90   100   120   150   160
## Frequency      8     3     1     2     1     1     1     1
## Proportion 0.089 0.033 0.011 0.022 0.011 0.011 0.011 0.011
## ---------------------------------------------------------------------------
## q22f_m1 
##        n  missing distinct 
##       53     1924        2 
##                                                   
## Value      Ne sais pas/Pas sûr  Refuse de répondre
## Frequency                   49                   4
## Proportion               0.925               0.075
## ---------------------------------------------------------------------------
## q23 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.981    3.425    3.072        0        0 
##      .25      .50      .75      .90      .95 
##        1        3        5        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    344   354   210   130   120   336   134   165   167    17
## Proportion 0.174 0.179 0.106 0.066 0.061 0.170 0.068 0.083 0.084 0.009
## ---------------------------------------------------------------------------
## q25 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.962    4.845     2.61        0        1 
##      .25      .50      .75      .90      .95 
##        3        5        7        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    103   127   185   158   163   365   141   609   112    14
## Proportion 0.052 0.064 0.094 0.080 0.082 0.185 0.071 0.308 0.057 0.007
## ---------------------------------------------------------------------------
## q27 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.828    24.71    36.81        0        0 
##      .25      .50      .75      .90      .95 
##        0        0        7       98       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency   1069   141    70    47    39    69    25    32   469    16
## Proportion 0.541 0.071 0.035 0.024 0.020 0.035 0.013 0.016 0.237 0.008
## ---------------------------------------------------------------------------
## q42 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       67    0.999    37.21    15.17       21       23 
##      .25      .50      .75      .90      .95 
##       26       33       46       58       66 
## 
## lowest : 18 19 20 21 22, highest: 82 83 84 86 90
## ---------------------------------------------------------------------------
## q44 
##        n  missing distinct 
##     1977        0        7 
## 
## Célibatiare (1025, 0.518), Divorcé (e) (130, 0.066), En couple (8, 0.004),
## Marié(e)/Conjoint de fait (739, 0.374), Refuse de répondre (23, 0.012),
## Séparé (e) (28, 0.014), Veuf (ve) (24, 0.012)
## ---------------------------------------------------------------------------
## q45 
##        n  missing distinct     Info     Mean      Gmd 
##     1977        0        7    0.441    6.923     1.85 
##                                                     
## Value          1     2     3     4     5     8     9
## Frequency    209    88    23    10     1  1627    19
## Proportion 0.106 0.045 0.012 0.005 0.001 0.823 0.010
## ---------------------------------------------------------------------------
## q46b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      892     1085       23    0.975    4.834    3.407        1        2 
##      .25      .50      .75      .90      .95 
##        3        4        6        9       11 
## 
## lowest :  0  1  2  3  4, highest: 28 30 32 39 42
## ---------------------------------------------------------------------------
## q47 
##        n  missing distinct 
##     1977        0       11 
## 
## lowest : Amérindien des États-Unis / Autochtone d'Amérique Arabe (Moyen-Orient, Afrique du Nord)             Asiatique / insulaire du Pacifique                Autre                                             Blanc(che) / Caucasien                           
## highest: Indien / Pakistanais                              Je préfère ne pas répondre                        Jewish                                            Mixed / Mixed race / Bi-racial                    Noir(e) / Africain(e) / Afro-Américain(e)        
## ---------------------------------------------------------------------------
## q48 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       55    0.323    6.219    7.977        2        2 
##      .25      .50      .75      .90      .95 
##        2        2        2        3       35 
## 
## lowest :  1  2  3  4  5, highest: 73 74 80 96 99
## ---------------------------------------------------------------------------
## q49 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                              5                           166
## Proportion                         0.003                         0.084
##                                         
## Value                                Oui
## Frequency                           1806
## Proportion                         0.914
## ---------------------------------------------------------------------------
## q50 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             16                           519
## Proportion                         0.008                         0.263
##                                         
## Value                                Oui
## Frequency                           1442
## Proportion                         0.729
## ---------------------------------------------------------------------------
## q51 
##        n  missing distinct 
##     1977        0        8 
## 
## lowest : Autre (précisez):                                   Baccalauréat                                        Cégep                                               Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent        
## highest: Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent         Diplôme universitaire supérieur au baccalauréat     École primaire                                      Refuse de répondre                                 
## ---------------------------------------------------------------------------
## Q52_occupational_status_category 
##        n  missing distinct 
##     1963       14        7 
## 
## Disability (17, 0.009), Full time or Self-employed (1307, 0.666),
## Homemaker or parental leave (55, 0.028), Part time (135, 0.069), Retired
## (112, 0.057), Student (272, 0.139), Unemployed seeking work (65, 0.033)
## ---------------------------------------------------------------------------
## q53 
##        n  missing distinct 
##     1977        0       10 
## 
## lowest : Entre 10000 $ and 19999 $ par année   Entre 100000 $ and 149999 $ par année Entre 150000 $ and 199999 $ par année Entre 20000 $ and 34999 $ par année   Entre 35000 $ and 49999 $ par année  
## highest: Entre 50000 $ and 74999 $ par année   Entre 75000 $ and 99999 $ par année   Moins de 10000 $ par année            Plus de 200000 $ par année            Refuse de répondre                   
## ---------------------------------------------------------------------------
## bmi 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1785      192      617        1    25.06    5.006    19.37    20.12 
##      .25      .50      .75      .90      .95 
##    21.63    24.21    27.32    31.69    34.97 
## 
## lowest : 17.57 17.58 17.64 17.71 17.72, highest: 39.68 39.75 39.94 40.14 40.35
## ---------------------------------------------------------------------------
## bmi_category 
##        n  missing distinct 
##     1785      192        4 
##                                                           
## Value       embonpoint insuffisant      normal     obesite
## Frequency          511          44         982         248
## Proportion       0.286       0.025       0.550       0.139
## ---------------------------------------------------------------------------
## WalkScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38       76    0.993    86.19    14.02       53       71 
##      .25      .50      .75      .90      .95 
##       86       90       96       97       99 
## 
## lowest :  0  1  3  4  5, highest: 95 96 97 98 99
## ---------------------------------------------------------------------------
## WalkScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                            Car-Dependent Somewhat Walkable
## Frequency                 38                96                85
## Proportion             0.019             0.049             0.043
##                                               
## Value          Very Walkable Walker's Paradise
## Frequency                598              1160
## Proportion             0.302             0.587
## ---------------------------------------------------------------------------
## TransitScore 
##        n  missing distinct 
##     1977        0        7 
##                                                                       
## Value                          100          40          50          71
## Frequency           38          16           2          64         106
## Proportion       0.019       0.008       0.001       0.032       0.054
##                                   
## Value               79 Unavailable
## Frequency           95        1656
## Proportion       0.048       0.838
## ---------------------------------------------------------------------------
## TransitScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                        Excellent Transit      Good Transit
## Frequency               1694               201                64
## Proportion             0.857             0.102             0.032
##                                               
## Value       Rider's Paradise      Some Transit
## Frequency                 16                 2
## Proportion             0.008             0.001
## ---------------------------------------------------------------------------
## BikeScore 
##        n  missing distinct 
##     1977        0       31 
## 
## lowest :                        46                     54                     55                     58                    
## highest: 94                     97.741638389225798     98.650123169158107     99.406202578305795     Unavailable           
## ---------------------------------------------------------------------------
## BikeScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                                 Bikeable  Biker's Paradise
## Frequency                598               475               323
## Proportion             0.302             0.240             0.163
##                                               
## Value      Somewhat Bikeable     Very Bikeable
## Frequency                 26               555
## Proportion             0.013             0.281
## ---------------------------------------------------------------------------
## DiningandDrinkingScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      157    0.998    86.12    15.23    52.92    67.97 
##      .25      .50      .75      .90      .95 
##    83.20    89.31    96.09    99.31    99.44 
## 
## lowest :  0.0000000  0.9592180  0.9743672  1.8423924  2.2943511
## highest: 99.4366379 99.7015610 99.8348999 99.9046631 99.9225693
## ---------------------------------------------------------------------------
## GroceryScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      139    0.996    88.94    16.42    46.68    66.03 
##      .25      .50      .75      .90      .95 
##    88.13    97.15    99.37   100.00   100.00 
## 
## lowest :   0.000000   1.710128   1.956163   2.363304   2.792868
## highest:  99.455818  99.624977  99.752014  99.923592 100.000000
## ---------------------------------------------------------------------------
## 
## Variables with all observations missing:
## 
## [1] q16 q17 q19 q20 q24 q26 q28 q40 q41
```

```r
Hmisc::describe(Philadelphie1)
```

```
## Philadelphie1 
## 
##  53  Variables      1977  Observations
## ---------------------------------------------------------------------------
## X 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1977        1     1011    703.9     99.8    198.6 
##      .25      .50      .75      .90      .95 
##    495.0    989.0   1483.0   1779.4   1878.2 
##                                                                       
## Value          0   200   400   600   800  1000  1200  1400  1600  1800
## Frequency    100   199   201   199   201   199   201   199   201   199
## Proportion 0.051 0.101 0.102 0.101 0.102 0.101 0.102 0.101 0.102 0.101
##                             
## Value       2000  7200 21400
## Frequency     75     1     2
## Proportion 0.038 0.001 0.001
## ---------------------------------------------------------------------------
## quest 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0     1975        1    20811    16141    610.8    962.6 
##      .25      .50      .75      .90      .95 
##   7057.0  20211.0  30188.0  33256.4  34680.0 
## 
## lowest :    10    11    13    32    38, highest: 73667 73668 73670 73675 73677
## ---------------------------------------------------------------------------
## lang 
##        n  missing distinct 
##     1977        0        2 
##                             
## Value       Anglais Espagnol
## Frequency      1974        3
## Proportion    0.998    0.002
## ---------------------------------------------------------------------------
## q54 
##        n  missing distinct 
##     1977        0        2 
##                       
## Value      Femme Homme
## Frequency   1283   694
## Proportion 0.649 0.351
## ---------------------------------------------------------------------------
## ville 
##        n  missing distinct    value 
##     1977        0        1   Boston 
##                  
## Value      Boston
## Frequency    1977
## Proportion      1
## ---------------------------------------------------------------------------
## q1us 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1970        7      183    0.999     2167    161.7     1910     2113 
##      .25      .50      .75      .90      .95 
##     2119     2134     2144     2445     2446 
##                                                                       
## Value       1400  1600  1800  2000  2200  2400  2600  3000  3200  3800
## Frequency      7    33    55    37  1600   210     2    15     6     4
## Proportion 0.004 0.017 0.028 0.019 0.812 0.107 0.001 0.008 0.003 0.002
##                 
## Value      20200
## Frequency      1
## Proportion 0.001
## ---------------------------------------------------------------------------
## q1aut 
##        n  missing distinct 
##     1977        0        9 
##                                                                          
## Value              01570  01581  01826  02138  02139  02446 N3C1G3 N3C3K9
## Frequency    1969      1      1      1      1      1      1      1      1
## Proportion  0.996  0.001  0.001  0.001  0.001  0.001  0.001  0.001  0.001
## ---------------------------------------------------------------------------
## q46b7 
##        n  missing distinct 
##     1969        8       17 
## 
## lowest : Allston-Brighton              Autre                         Back Bay/Beacon Hill/West End Cambridgeport                 Charlestown                  
## highest: Roxbury                       South Boston                  South Dorchester              South End/Chinatown           Wellington-Harrington        
## ---------------------------------------------------------------------------
## q46b9 
##        n  missing distinct 
##     1977        0       32 
## 
## lowest :    00 1A 1F 1M, highest: R1 R2 R3 S2 T7
## ---------------------------------------------------------------------------
## q2 
##        n  missing distinct 
##     1977        0        6 
## 
## Bon (521, 0.264), Excellent (467, 0.236), Mauvais (24, 0.012), Moyen (142,
## 0.072), Ne sais pas/Refuse de répondre (5, 0.003), Très bon (818, 0.414)
## ---------------------------------------------------------------------------
## q13 
##        n  missing distinct 
##     1977        0       10 
## 
## Autre (précisez:) (3, 0.002), Marche (500, 0.253), Ne s'applique pas (3,
## 0.002), Scooter (1, 0.001), Taxi (12, 0.006), Transport en commun (669,
## 0.338), Véhicule motorisé (loué, emprunté, covoiturage) (675, 0.341), Vélo
## en libre-service (15, 0.008), Vélo personnel (89, 0.045), Voiture
## personnelle (10, 0.005)
## ---------------------------------------------------------------------------
## q14 
##        n  missing distinct 
##     1977        0        4 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             18                           255
## Proportion                         0.009                         0.129
##                                                                       
## Value                                Oui            Refuse de répondre
## Frequency                           1703                             1
## Proportion                         0.861                         0.001
## ---------------------------------------------------------------------------
## q15 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.962    44.15    26.73        5        7 
##      .25      .50      .75      .90      .95 
##       20       57       64       66       68 
## 
## lowest :  1  2  3  4  5, highest: 70 71 96 98 99
## ---------------------------------------------------------------------------
## q18 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1703      274       62    0.992    50.68    35.54      5.1      7.0 
##      .25      .50      .75      .90      .95 
##     19.0     61.0     68.0     97.0     97.0 
## 
## lowest :  1  2  3  4  5, highest: 71 96 97 98 99
## ---------------------------------------------------------------------------
## q21 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.984    10.46    15.02        0        0 
##      .25      .50      .75      .90      .95 
##        2        4        7        7       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency    231   261   222   163   133   277   186   362   130    12
## Proportion 0.117 0.132 0.112 0.082 0.067 0.140 0.094 0.183 0.066 0.006
## ---------------------------------------------------------------------------
## q22b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      754     1223       14    0.862     2.09    1.577        1        1 
##      .25      .50      .75      .90      .95 
##        1        2        2        4        5 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8    10
## Frequency      9   361   227    69    37    14    14     2     3     8
## Proportion 0.012 0.479 0.301 0.092 0.049 0.019 0.019 0.003 0.004 0.011
##                                   
## Value         12    14    15    16
## Frequency      5     2     2     1
## Proportion 0.007 0.003 0.003 0.001
## ---------------------------------------------------------------------------
## q22c 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      476     1501       27    0.964    30.61    19.42        7       10 
##      .25      .50      .75      .90      .95 
##       20       30       40       50       70 
## 
## lowest :   0   1   2   3   4, highest:  75  80  90 120 150
## ---------------------------------------------------------------------------
## q22d 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      250     1727       19    0.957    4.216    4.405     1.00     1.00 
##      .25      .50      .75      .90      .95 
##     1.00     2.00     4.75    10.00    14.55 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency      1    73    62    33    18    13     9     5     3     2
## Proportion 0.004 0.292 0.248 0.132 0.072 0.052 0.036 0.020 0.012 0.008
##                                                                 
## Value         10    12    14    15    18    20    25    28    30
## Frequency     10     4     4     2     1     5     2     1     2
## Proportion 0.040 0.016 0.016 0.008 0.004 0.020 0.008 0.004 0.008
## ---------------------------------------------------------------------------
## q22e 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##       90     1887       18    0.916    34.57    23.74     7.25    14.50 
##      .25      .50      .75      .90      .95 
##    20.00    30.00    38.75    60.00    90.00 
##                                                                       
## Value          0     1     5    10    15    20    25    30    35    40
## Frequency      3     1     1     4     8     8     1    39     2     5
## Proportion 0.033 0.011 0.011 0.044 0.089 0.089 0.011 0.433 0.022 0.056
##                                                           
## Value         45    60    70    90   100   120   150   160
## Frequency      8     3     1     2     1     1     1     1
## Proportion 0.089 0.033 0.011 0.022 0.011 0.011 0.011 0.011
## ---------------------------------------------------------------------------
## q22f_m1 
##        n  missing distinct 
##       53     1924        2 
##                                                   
## Value      Ne sais pas/Pas sûr  Refuse de répondre
## Frequency                   49                   4
## Proportion               0.925               0.075
## ---------------------------------------------------------------------------
## q23 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.981    3.425    3.072        0        0 
##      .25      .50      .75      .90      .95 
##        1        3        5        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    344   354   210   130   120   336   134   165   167    17
## Proportion 0.174 0.179 0.106 0.066 0.061 0.170 0.068 0.083 0.084 0.009
## ---------------------------------------------------------------------------
## q25 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.962    4.845     2.61        0        1 
##      .25      .50      .75      .90      .95 
##        3        5        7        7        8 
##                                                                       
## Value          0     1     2     3     4     5     6     7     8     9
## Frequency    103   127   185   158   163   365   141   609   112    14
## Proportion 0.052 0.064 0.094 0.080 0.082 0.185 0.071 0.308 0.057 0.007
## ---------------------------------------------------------------------------
## q27 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       10    0.828    24.71    36.81        0        0 
##      .25      .50      .75      .90      .95 
##        0        0        7       98       98 
##                                                                       
## Value          0     1     2     3     4     5     6     7    98    99
## Frequency   1069   141    70    47    39    69    25    32   469    16
## Proportion 0.541 0.071 0.035 0.024 0.020 0.035 0.013 0.016 0.237 0.008
## ---------------------------------------------------------------------------
## q42 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       67    0.999    37.21    15.17       21       23 
##      .25      .50      .75      .90      .95 
##       26       33       46       58       66 
## 
## lowest : 18 19 20 21 22, highest: 82 83 84 86 90
## ---------------------------------------------------------------------------
## q44 
##        n  missing distinct 
##     1977        0        7 
## 
## Célibatiare (1025, 0.518), Divorcé (e) (130, 0.066), En couple (8, 0.004),
## Marié(e)/Conjoint de fait (739, 0.374), Refuse de répondre (23, 0.012),
## Séparé (e) (28, 0.014), Veuf (ve) (24, 0.012)
## ---------------------------------------------------------------------------
## q45 
##        n  missing distinct     Info     Mean      Gmd 
##     1977        0        7    0.441    6.923     1.85 
##                                                     
## Value          1     2     3     4     5     8     9
## Frequency    209    88    23    10     1  1627    19
## Proportion 0.106 0.045 0.012 0.005 0.001 0.823 0.010
## ---------------------------------------------------------------------------
## q46b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      892     1085       23    0.975    4.834    3.407        1        2 
##      .25      .50      .75      .90      .95 
##        3        4        6        9       11 
## 
## lowest :  0  1  2  3  4, highest: 28 30 32 39 42
## ---------------------------------------------------------------------------
## q47 
##        n  missing distinct 
##     1977        0       11 
## 
## lowest : Amérindien des États-Unis / Autochtone d'Amérique Arabe (Moyen-Orient, Afrique du Nord)             Asiatique / insulaire du Pacifique                Autre                                             Blanc(che) / Caucasien                           
## highest: Indien / Pakistanais                              Je préfère ne pas répondre                        Jewish                                            Mixed / Mixed race / Bi-racial                    Noir(e) / Africain(e) / Afro-Américain(e)        
## ---------------------------------------------------------------------------
## q48 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1977        0       55    0.323    6.219    7.977        2        2 
##      .25      .50      .75      .90      .95 
##        2        2        2        3       35 
## 
## lowest :  1  2  3  4  5, highest: 73 74 80 96 99
## ---------------------------------------------------------------------------
## q49 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                              5                           166
## Proportion                         0.003                         0.084
##                                         
## Value                                Oui
## Frequency                           1806
## Proportion                         0.914
## ---------------------------------------------------------------------------
## q50 
##        n  missing distinct 
##     1977        0        3 
##                                                                       
## Value      Ne sais pas/Ne s'applique pas                           Non
## Frequency                             16                           519
## Proportion                         0.008                         0.263
##                                         
## Value                                Oui
## Frequency                           1442
## Proportion                         0.729
## ---------------------------------------------------------------------------
## q51 
##        n  missing distinct 
##     1977        0        8 
## 
## lowest : Autre (précisez):                                   Baccalauréat                                        Cégep                                               Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent        
## highest: Certificat d'école de métier, certificat ou diplôme Diplôme d'études secondaire ou l'équivalent         Diplôme universitaire supérieur au baccalauréat     École primaire                                      Refuse de répondre                                 
## ---------------------------------------------------------------------------
## Q52_occupational_status_category 
##        n  missing distinct 
##     1963       14        7 
## 
## Disability (17, 0.009), Full time or Self-employed (1307, 0.666),
## Homemaker or parental leave (55, 0.028), Part time (135, 0.069), Retired
## (112, 0.057), Student (272, 0.139), Unemployed seeking work (65, 0.033)
## ---------------------------------------------------------------------------
## q53 
##        n  missing distinct 
##     1977        0       10 
## 
## lowest : Entre 10000 $ and 19999 $ par année   Entre 100000 $ and 149999 $ par année Entre 150000 $ and 199999 $ par année Entre 20000 $ and 34999 $ par année   Entre 35000 $ and 49999 $ par année  
## highest: Entre 50000 $ and 74999 $ par année   Entre 75000 $ and 99999 $ par année   Moins de 10000 $ par année            Plus de 200000 $ par année            Refuse de répondre                   
## ---------------------------------------------------------------------------
## bmi 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1785      192      617        1    25.06    5.006    19.37    20.12 
##      .25      .50      .75      .90      .95 
##    21.63    24.21    27.32    31.69    34.97 
## 
## lowest : 17.57 17.58 17.64 17.71 17.72, highest: 39.68 39.75 39.94 40.14 40.35
## ---------------------------------------------------------------------------
## bmi_category 
##        n  missing distinct 
##     1785      192        4 
##                                                           
## Value       embonpoint insuffisant      normal     obesite
## Frequency          511          44         982         248
## Proportion       0.286       0.025       0.550       0.139
## ---------------------------------------------------------------------------
## WalkScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38       76    0.993    86.19    14.02       53       71 
##      .25      .50      .75      .90      .95 
##       86       90       96       97       99 
## 
## lowest :  0  1  3  4  5, highest: 95 96 97 98 99
## ---------------------------------------------------------------------------
## WalkScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                            Car-Dependent Somewhat Walkable
## Frequency                 38                96                85
## Proportion             0.019             0.049             0.043
##                                               
## Value          Very Walkable Walker's Paradise
## Frequency                598              1160
## Proportion             0.302             0.587
## ---------------------------------------------------------------------------
## TransitScore 
##        n  missing distinct 
##     1977        0        7 
##                                                                       
## Value                          100          40          50          71
## Frequency           38          16           2          64         106
## Proportion       0.019       0.008       0.001       0.032       0.054
##                                   
## Value               79 Unavailable
## Frequency           95        1656
## Proportion       0.048       0.838
## ---------------------------------------------------------------------------
## TransitScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                        Excellent Transit      Good Transit
## Frequency               1694               201                64
## Proportion             0.857             0.102             0.032
##                                               
## Value       Rider's Paradise      Some Transit
## Frequency                 16                 2
## Proportion             0.008             0.001
## ---------------------------------------------------------------------------
## BikeScore 
##        n  missing distinct 
##     1977        0       31 
## 
## lowest :                        46                     54                     55                     58                    
## highest: 94                     97.741638389225798     98.650123169158107     99.406202578305795     Unavailable           
## ---------------------------------------------------------------------------
## BikeScoreLabel 
##        n  missing distinct 
##     1977        0        5 
##                                                                 
## Value                                 Bikeable  Biker's Paradise
## Frequency                598               475               323
## Proportion             0.302             0.240             0.163
##                                               
## Value      Somewhat Bikeable     Very Bikeable
## Frequency                 26               555
## Proportion             0.013             0.281
## ---------------------------------------------------------------------------
## DiningandDrinkingScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      157    0.998    86.12    15.23    52.92    67.97 
##      .25      .50      .75      .90      .95 
##    83.20    89.31    96.09    99.31    99.44 
## 
## lowest :  0.0000000  0.9592180  0.9743672  1.8423924  2.2943511
## highest: 99.4366379 99.7015610 99.8348999 99.9046631 99.9225693
## ---------------------------------------------------------------------------
## GroceryScore 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1939       38      139    0.996    88.94    16.42    46.68    66.03 
##      .25      .50      .75      .90      .95 
##    88.13    97.15    99.37   100.00   100.00 
## 
## lowest :   0.000000   1.710128   1.956163   2.363304   2.792868
## highest:  99.455818  99.624977  99.752014  99.923592 100.000000
## ---------------------------------------------------------------------------
## 
## Variables with all observations missing:
## 
## [1] q16 q17 q19 q20 q24 q26 q28 q40 q41
```

  
