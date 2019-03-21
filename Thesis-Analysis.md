---
title: "ThesisAnalysis"
author: "Kassia Orychock"
date: '2019-01-31'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---



This Rmd file will be used as the file for my Thesis Analysis on causal mediation.

# Libraries

```r
library(tidyverse)
library(car)
library(foreign)
library(Hmisc)
library(survival)
library(finalfit)
library(tableone)
```

# Reading of Raw Survey Data

```r
ibiccs_readin <- read.csv("Database_recoded_2012-2014_weights_Walkscore_RTA.csv")
ibiccs_readin$TransitScore <- as.numeric(ibiccs_readin$TransitScore)
ibiccs_readin$BikeScore <- as.numeric(ibiccs_readin$BikeScore)
```

# Recode BMI

```r
## BMI
ibiccs_clean <- ibiccs_readin %>%
	mutate(bmi_category = case_when(
		bmi < 18.5 ~ "underweight",
		bmi >=30 & bmi <999 ~ "obese",
		bmi >=25 & bmi <30 ~ "overweight",
		bmi >=18.5 & bmi <25 ~ "normal weight",
		TRUE ~ "other"
	))
```

# Recode Language

```r
table(ibiccs_clean$lang)
```

```
## 
##  Anglais Espagnol Français 
##    22236       44     1621
```

```r
## Language
ibiccs_clean <- ibiccs_clean %>%
	mutate(language = case_when(
		lang == "Anglais" ~ "English",
		lang == "Espagnol" ~ "Fren/Span",
		lang == "Français" ~ "Fren/Span"
	))
table(ibiccs_clean$lang, ibiccs_clean$language)
```

```
##           
##            English Fren/Span
##   Anglais    22236         0
##   Espagnol       0        44
##   Français       0      1621
```

# Recode Gender

```r
table(ibiccs_clean$q54)
```

```
## 
## Femme Homme 
## 14042  9859
```

```r
#Gender
ibiccs_clean <- ibiccs_clean %>%
  mutate(gender = case_when(
    q54 == "Femme" ~ "Female",
    q54 == "Homme" ~ "Male"
  ))
table(ibiccs_clean$q54, ibiccs_clean$gender)
```

```
##        
##         Female  Male
##   Femme  14042     0
##   Homme      0  9859
```

# Recode Self-Rated Health

```r
table(ibiccs_clean$q2)
```

```
## 
##                            Bon                      Excellent 
##                           6725                           4803 
##                        Mauvais                          Moyen 
##                            489                           2104 
## Ne sais pas/Refuse de répondre                       Très bon 
##                             39                           9741
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(health = case_when(
    q2 == "Excellent" ~ "Excellent",
    q2 == "Très bon" ~ "Excellent",
    q2 == "Bon" ~ "Good",
    q2 == "Moyen" ~ "Poor",
    q2 == "Mauvais" ~ "Poor",
    q2 == "Ne sais pas/Refuse de répondre" ~ "NA"
  ))
table(ibiccs_clean$q2, ibiccs_clean$health)
```

```
##                                 
##                                  Excellent Good   NA Poor
##   Bon                                    0 6725    0    0
##   Excellent                           4803    0    0    0
##   Mauvais                                0    0    0  489
##   Moyen                                  0    0    0 2104
##   Ne sais pas/Refuse de répondre         0    0   39    0
##   Très bon                            9741    0    0    0
```

# Recode Transportation

```r
table(ibiccs_clean$q13)
```

```
## 
##                               Autre (précisez:) 
##                                              35 
##                                          Marche 
##                                            4484 
##                               Ne s'applique pas 
##                                              61 
##                                     Ne sais pas 
##                                              36 
##                                         Scooter 
##                                              21 
##                                            Taxi 
##                                             205 
##                             Transport en commun 
##                                            7886 
## Véhicule motorisé (loué, emprunté, covoiturage) 
##                                            9768 
##                           Vélo en libre-service 
##                                             243 
##                                  Vélo personnel 
##                                             934 
##                             Voiture personnelle 
##                                             228
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(common_transportation = case_when(
    q13 == "Marche" ~ "Walking",
    q13 == "Véhicule motorisé (loué, emprunté, covoiturage)" ~ "Car",
    q13 == "Voiture personnelle" ~ "Car",
    q13 == " Ne s'applique pas" ~ "Other",
    q13 == "Ne sais pas" ~ "Other",
    q13 == "Scooter" ~ "Other",
    q13 == "Taxi" ~ "Other",
    q13 == "Transport en commun" ~ "Public Transportation",
    q13 == "Vélo en libre-service" ~ "Bicycle",
    q13 == "Vélo personnel" ~ "Bicycle"
  ))
table(ibiccs_clean$q13, ibiccs_clean$common_transportation)
```

```
##                                                  
##                                                   Bicycle  Car Other
##   Autre (précisez:)                                     0    0     0
##   Marche                                                0    0     0
##   Ne s'applique pas                                     0    0     0
##   Ne sais pas                                           0    0    36
##   Scooter                                               0    0    21
##   Taxi                                                  0    0   205
##   Transport en commun                                   0    0     0
##   Véhicule motorisé (loué, emprunté, covoiturage)       0 9768     0
##   Vélo en libre-service                               243    0     0
##   Vélo personnel                                      934    0     0
##   Voiture personnelle                                   0  228     0
##                                                  
##                                                   Public Transportation
##   Autre (précisez:)                                                   0
##   Marche                                                              0
##   Ne s'applique pas                                                   0
##   Ne sais pas                                                         0
##   Scooter                                                             0
##   Taxi                                                                0
##   Transport en commun                                              7886
##   Véhicule motorisé (loué, emprunté, covoiturage)                     0
##   Vélo en libre-service                                               0
##   Vélo personnel                                                      0
##   Voiture personnelle                                                 0
##                                                  
##                                                   Walking
##   Autre (précisez:)                                     0
##   Marche                                             4484
##   Ne s'applique pas                                     0
##   Ne sais pas                                           0
##   Scooter                                               0
##   Taxi                                                  0
##   Transport en commun                                   0
##   Véhicule motorisé (loué, emprunté, covoiturage)       0
##   Vélo en libre-service                                 0
##   Vélo personnel                                        0
##   Voiture personnelle                                   0
```

# Recode Physically Active (Y/N)

```r
table(ibiccs_clean$q14)
```

```
## 
## Ne sais pas/Ne s'applique pas                           Non 
##                           276                          4124 
##                           Oui            Refuse de répondre 
##                         19471                            30
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(physically_active = case_when(
    q14 == "Ne sais pas/Ne s'applique pas" ~ "Other",
    q14 == "Refuse de répondre " ~ "Other",
    q14 == "Oui" ~ "Yes",
    q14 == "Non" ~ "No"
  ))
table(ibiccs_clean$q14, ibiccs_clean$physically_active)
```

```
##                                
##                                    No Other   Yes
##   Ne sais pas/Ne s'applique pas     0   276     0
##   Non                            4124     0     0
##   Oui                               0     0 19471
##   Refuse de répondre                0     0     0
```

# Recode Type of Physical Activity 

```r
#Find IPAQ guidleines with Melissa
table(ibiccs_clean$q15)
```

```
## 
##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
##  157  629   47   81  288  436 1044   26  131   74  179   12   33  316  828 
##   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30 
##   30   27  357  122  174    9  156  182   29   27    8   14  852    2   23 
##   31   32   33   34   35   36   37   38   39   40   41   42   43   44   46 
##   92    3   30  188   15   93 1555   25    8   34   15   18   13   56    1 
##   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61 
##    3   11   11    4  167   56   25  142    8   10  300  199   28   60  179 
##   62   63   64   65   66   67   68   69   70   71   96   98   99 
##   28   98 7090    2 1165   13  840   74   65   21  362   54   17
```

```r
summary(ibiccs_clean$q18)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       1      18      64      52      68      99    4430
```

# Recode Days Per Week Spent Travelling via Car

```r
table(ibiccs_clean$q21)
```

```
## 
##    0    1    2    3    4    5    6    7   98   99 
## 2785 2527 2631 2070 1648 3136 2019 4925 2037  123
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(day_per_week_motor_vehicle = case_when(
    q21 == "0" ~ "0",
    q21 == "1" ~ "1",
    q21 == "2" ~ "2",
    q21 == "3" ~ "3",
    q21 == "4" ~ "4",
    q21 == "5" ~ "5",
    q21 == "6" ~ "6",
    q21 == "7" ~ "7",
    q21 == "98" ~ "NA",
    q21 == "99" ~ "NA"
  ))

table(ibiccs_clean$q21, ibiccs_clean$day_per_week_motor_vehicle)
```

```
##     
##         0    1    2    3    4    5    6    7   NA
##   0  2785    0    0    0    0    0    0    0    0
##   1     0 2527    0    0    0    0    0    0    0
##   2     0    0 2631    0    0    0    0    0    0
##   3     0    0    0 2070    0    0    0    0    0
##   4     0    0    0    0 1648    0    0    0    0
##   5     0    0    0    0    0 3136    0    0    0
##   6     0    0    0    0    0    0 2019    0    0
##   7     0    0    0    0    0    0    0 4925    0
##   98    0    0    0    0    0    0    0    0 2037
##   99    0    0    0    0    0    0    0    0  123
```

# Recode Days Per Week Spent Travelling via Public Transport

```r
table(ibiccs_clean$q23)
```

```
## 
##    0    1    2    3    4    5    6    7    8    9 
## 6169 2993 2167 1598 1339 3617 1482 1583 2726  227
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(day_per_week_public_transit = case_when(
    q23 == "0" ~ "0",
    q23 == "1" ~ "1",
    q23 == "2" ~ "2",
    q23 == "3" ~ "3",
    q23 == "4" ~ "4",
    q23 == "5" ~ "5",
    q23 == "6" ~ "6",
    q23 == "7" ~ "7",
    q23 == "8" ~ "NA",
    q23 == "9" ~ "NA"
  ))

table(ibiccs_clean$q23, ibiccs_clean$day_per_week_public_transit)
```

```
##    
##        0    1    2    3    4    5    6    7   NA
##   0 6169    0    0    0    0    0    0    0    0
##   1    0 2993    0    0    0    0    0    0    0
##   2    0    0 2167    0    0    0    0    0    0
##   3    0    0    0 1598    0    0    0    0    0
##   4    0    0    0    0 1339    0    0    0    0
##   5    0    0    0    0    0 3617    0    0    0
##   6    0    0    0    0    0    0 1482    0    0
##   7    0    0    0    0    0    0    0 1583    0
##   8    0    0    0    0    0    0    0    0 2726
##   9    0    0    0    0    0    0    0    0  227
```

# Recode Days Per Week Spent Travelling via Walking

```r
table(ibiccs_clean$q25)
```

```
## 
##    0    1    2    3    4    5    6    7    8    9 
## 2115 1841 2361 2245 1831 3994 1711 5436 2149  218
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(day_per_week_walking = case_when(
    q25 == "0" ~ "0",
    q25 == "1" ~ "1",
    q25 == "2" ~ "2",
    q25 == "3" ~ "3",
    q25 == "4" ~ "4",
    q25 == "5" ~ "5",
    q25 == "6" ~ "6",
    q25 == "7" ~ "7",
    q25 == "8" ~ "NA",
    q25 == "9" ~ "NA"
  ))

table(ibiccs_clean$q25, ibiccs_clean$day_per_week_walking)
```

```
##    
##        0    1    2    3    4    5    6    7   NA
##   0 2115    0    0    0    0    0    0    0    0
##   1    0 1841    0    0    0    0    0    0    0
##   2    0    0 2361    0    0    0    0    0    0
##   3    0    0    0 2245    0    0    0    0    0
##   4    0    0    0    0 1831    0    0    0    0
##   5    0    0    0    0    0 3994    0    0    0
##   6    0    0    0    0    0    0 1711    0    0
##   7    0    0    0    0    0    0    0 5436    0
##   8    0    0    0    0    0    0    0    0 2149
##   9    0    0    0    0    0    0    0    0  218
```

# Recode Days Per Week Spent Travelling via Bike

```r
table(ibiccs_clean$q27)
```

```
## 
##     0     1     2     3     4     5     6     7    98    99 
## 12010  1793  1185   741   462   632   215   294  6395   174
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(day_per_week_bike = case_when(
    q27 == "0" ~ "0",
    q27 == "1" ~ "1",
    q27 == "2" ~ "2",
    q27 == "3" ~ "3",
    q27 == "4" ~ "4",
    q27 == "5" ~ "5",
    q27 == "6" ~ "6",
    q27 == "7" ~ "7",
    q27 == "98" ~ "NA",
    q27 == "99" ~ "NA"
  ))

table(ibiccs_clean$q27, ibiccs_clean$day_per_week_bike)
```

```
##     
##          0     1     2     3     4     5     6     7    NA
##   0  12010     0     0     0     0     0     0     0     0
##   1      0  1793     0     0     0     0     0     0     0
##   2      0     0  1185     0     0     0     0     0     0
##   3      0     0     0   741     0     0     0     0     0
##   4      0     0     0     0   462     0     0     0     0
##   5      0     0     0     0     0   632     0     0     0
##   6      0     0     0     0     0     0   215     0     0
##   7      0     0     0     0     0     0     0   294     0
##   98     0     0     0     0     0     0     0     0  6395
##   99     0     0     0     0     0     0     0     0   174
```

# Recode Age

```r
summary(ibiccs_clean$q42)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    18.0    31.0    41.0    42.6    53.0    94.0
```

```r
#CCHS/STATCAN or continuous
```

# Recode Marital Status

```r
table(ibiccs_clean$q44)
```

```
## 
##               Célibatiare               Divorcé (e) 
##                      9452                      1854 
##                 En couple Marié(e)/Conjoint de fait 
##                        79                     11287 
##        Refuse de répondre                Séparé (e) 
##                       291                       460 
##                 Veuf (ve) 
##                       478
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(marital_status = case_when(
    q44 == "Célibatiare" ~ "Single",
    q44 == "Divorcé(e)" ~ "Divorced/Separated/Widowed",
    q44 == "En couple" ~ "Relationship/Married/Common-Law",
    q44 == "Marié(e)/Conjoint de fait" ~ "Relationship/Married/Common-Law",
    q44 == "Refuse de répondre" ~ "NA",
    q44 == "Séparé(e)" ~ "Divorced/Separated/Widowed",
    q44 == "Veuf(ve)" ~ "Divorced/Separated/Widowed"
  ))
table(ibiccs_clean$q44, ibiccs_clean$marital_status)
```

```
##                            
##                                NA Relationship/Married/Common-Law Single
##   Célibatiare                   0                               0   9452
##   Divorcé (e)                   0                               0      0
##   En couple                     0                              79      0
##   Marié(e)/Conjoint de fait     0                           11287      0
##   Refuse de répondre          291                               0      0
##   Séparé (e)                    0                               0      0
##   Veuf (ve)                     0                               0      0
```

# Recode Number of Children in Home

```r
table(ibiccs_clean$q45)
```

```
## 
##     1     2     3     4     5     6     7     8     9 
##  2915  1863   471   119    40    18     9 18191   275
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(children_household = case_when(
    q45 == "1" ~ "1",
    q45 == "2" ~ "2-3",
    q45 == "3" ~ "2-3",
    q45 == "4" ~ "4+",
    q45 == "5" ~ "4+",
    q45 == "6" ~ "4+",
    q45 == "7" ~ "4+",
    q45 == "8" ~ "0",
    q45 == "9" ~ "Refuse"
  ))
table(ibiccs_clean$q45, ibiccs_clean$children_household)
```

```
##    
##         0     1   2-3    4+ Refuse
##   1     0  2915     0     0      0
##   2     0     0  1863     0      0
##   3     0     0   471     0      0
##   4     0     0     0   119      0
##   5     0     0     0    40      0
##   6     0     0     0    18      0
##   7     0     0     0     9      0
##   8 18191     0     0     0      0
##   9     0     0     0     0    275
```

# Recode Months in Current Home?

```r
summary(ibiccs_clean$q46b)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0       3       5       5       6      81   15358
```

# Recode Ethnicity

```r
table(ibiccs_clean$q47)
```

```
## 
## Amérindien des États-Unis / Autochtone d'Amérique 
##                                                79 
##             Arabe (Moyen-Orient, Afrique du Nord) 
##                                               135 
##                Asiatique / insulaire du Pacifique 
##                                              2661 
##                                             Autre 
##                                               129 
##                            Blanc(che) / Caucasien 
##                                             17154 
##                 Hispanique / Latino / Espagnol(e) 
##                                               933 
##                              Indien / Pakistanais 
##                                               330 
##                        Je préfère ne pas répondre 
##                                               639 
##                                            Jewish 
##                                                16 
##                    Mixed / Mixed race / Bi-racial 
##                                               171 
##         Noir(e) / Africain(e) / Afro-Américain(e) 
##                                              1654
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(ethnicity = case_when(
    q47 == "Amérindien des États-Unis / Autochtone d'Amérique" ~ "Native American",
    q47 == "Arabe (Moyen-Orient, Afrique du Nord) " ~ "Arab/Indian/Jewish/Mixed/Other",
    q47 == "Asiatique / insulaire du Pacifique " ~ "Asian",
    q47 == "Autre " ~ "Arab/Indian/Jewish/Mixed/Other",
    q47 == "Blanc(che) / Caucasien " ~ "Causcasian",
    q47 == "Hispanique / Latino / Espagnol(e) " ~ "Hispanic",
    q47 == "Indien / Pakistanais" ~ "Arab/Indian/Jewish/Mixed/Other",
    q47 == "Je préfère ne pas répondre " ~ "Refuse",
    q47 == "Jewish " ~ "Arab/Indian/Jewish/Mixed/Other",
    q47 == "Mixed / Mixed race / Bi-racial" ~ "Arab/Indian/Jewish/Mixed/Other",
    q47 == "Noir(e) / Africain(e) / Afro-Américain(e)" ~ "African American"
  ))
table(ibiccs_clean$q47, ibiccs_clean$ethnicity)
```

```
##                                                    
##                                                     African American
##   Amérindien des États-Unis / Autochtone d'Amérique                0
##   Arabe (Moyen-Orient, Afrique du Nord)                            0
##   Asiatique / insulaire du Pacifique                               0
##   Autre                                                            0
##   Blanc(che) / Caucasien                                           0
##   Hispanique / Latino / Espagnol(e)                                0
##   Indien / Pakistanais                                             0
##   Je préfère ne pas répondre                                       0
##   Jewish                                                           0
##   Mixed / Mixed race / Bi-racial                                   0
##   Noir(e) / Africain(e) / Afro-Américain(e)                     1654
##                                                    
##                                                     Arab/Indian/Jewish/Mixed/Other
##   Amérindien des États-Unis / Autochtone d'Amérique                              0
##   Arabe (Moyen-Orient, Afrique du Nord)                                          0
##   Asiatique / insulaire du Pacifique                                             0
##   Autre                                                                          0
##   Blanc(che) / Caucasien                                                         0
##   Hispanique / Latino / Espagnol(e)                                              0
##   Indien / Pakistanais                                                         330
##   Je préfère ne pas répondre                                                     0
##   Jewish                                                                         0
##   Mixed / Mixed race / Bi-racial                                               171
##   Noir(e) / Africain(e) / Afro-Américain(e)                                      0
##                                                    
##                                                     Native American
##   Amérindien des États-Unis / Autochtone d'Amérique              79
##   Arabe (Moyen-Orient, Afrique du Nord)                           0
##   Asiatique / insulaire du Pacifique                              0
##   Autre                                                           0
##   Blanc(che) / Caucasien                                          0
##   Hispanique / Latino / Espagnol(e)                               0
##   Indien / Pakistanais                                            0
##   Je préfère ne pas répondre                                      0
##   Jewish                                                          0
##   Mixed / Mixed race / Bi-racial                                  0
##   Noir(e) / Africain(e) / Afro-Américain(e)                       0
```

```r
Chicago <- filter(ibiccs_clean, ville == "Chicago")

Detroit <- filter(ibiccs_clean, ville == "Détroit")

NewYork <- filter(ibiccs_clean, ville == "New-York")

Philadelphia <- filter(ibiccs_clean, ville == "Philadelphie")

Montreal <- filter(ibiccs_clean, ville == "Montréal")

Toronto <- filter(ibiccs_clean, ville == "Toronto")

Vancouver <- filter(ibiccs_clean, ville == "Vancouver")
```

# Recode Country Born

```r
table(ibiccs_clean$q48)
```

```
## 
##     1     2     3     4     5     6     7     8     9    10    11    12 
##  7171 13175   246    11    11    20    29    49    20    28    14    17 
##    13    14    15    16    17    18    19    20    21    22    23    24 
##    15    12   109   153   104   231   166    39     7    22    77    43 
##    25    26    27    28    29    30    31    32    33    34    35    36 
##    73    28    21    57    27    18    93    63    44    48    66    33 
##    37    38    39    40    41    42    43    44    45    46    47    48 
##    33    23    25    24   159    51    27    43     8    15     4    40 
##    49    50    51    52    53    54    55    56    57    58    59    60 
##     5    26    14    11    20    11    13    34     3    18    13    13 
##    61    62    63    64    65    66    67    68    69    70    71    72 
##    12     9    33    24    13    15    20     9     6    13    10     3 
##    73    74    75    76    77    78    79    80    81    96    99 
##    16     8     4     5    19    12     6     9    13   196   473
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(country_born = case_when(
    q48 == 1 ~ "Canada",
    q48 == 2 ~ "United States",
    q48 >= 3 | q48 <=81 ~ "Other", 
    TRUE ~ "Not stated"
  ))
table(ibiccs_clean$q48, ibiccs_clean$country_born)
```

```
##     
##      Canada Other United States
##   1    7171     0             0
##   2       0     0         13175
##   3       0   246             0
##   4       0    11             0
##   5       0    11             0
##   6       0    20             0
##   7       0    29             0
##   8       0    49             0
##   9       0    20             0
##   10      0    28             0
##   11      0    14             0
##   12      0    17             0
##   13      0    15             0
##   14      0    12             0
##   15      0   109             0
##   16      0   153             0
##   17      0   104             0
##   18      0   231             0
##   19      0   166             0
##   20      0    39             0
##   21      0     7             0
##   22      0    22             0
##   23      0    77             0
##   24      0    43             0
##   25      0    73             0
##   26      0    28             0
##   27      0    21             0
##   28      0    57             0
##   29      0    27             0
##   30      0    18             0
##   31      0    93             0
##   32      0    63             0
##   33      0    44             0
##   34      0    48             0
##   35      0    66             0
##   36      0    33             0
##   37      0    33             0
##   38      0    23             0
##   39      0    25             0
##   40      0    24             0
##   41      0   159             0
##   42      0    51             0
##   43      0    27             0
##   44      0    43             0
##   45      0     8             0
##   46      0    15             0
##   47      0     4             0
##   48      0    40             0
##   49      0     5             0
##   50      0    26             0
##   51      0    14             0
##   52      0    11             0
##   53      0    20             0
##   54      0    11             0
##   55      0    13             0
##   56      0    34             0
##   57      0     3             0
##   58      0    18             0
##   59      0    13             0
##   60      0    13             0
##   61      0    12             0
##   62      0     9             0
##   63      0    33             0
##   64      0    24             0
##   65      0    13             0
##   66      0    15             0
##   67      0    20             0
##   68      0     9             0
##   69      0     6             0
##   70      0    13             0
##   71      0    10             0
##   72      0     3             0
##   73      0    16             0
##   74      0     8             0
##   75      0     4             0
##   76      0     5             0
##   77      0    19             0
##   78      0    12             0
##   79      0     6             0
##   80      0     9             0
##   81      0    13             0
##   96      0   196             0
##   99      0   473             0
```

# Recode Motor Vehicle Access

```r
table(ibiccs_clean$q50)
```

```
## 
## Ne sais pas/Ne s'applique pas                           Non 
##                           230                          6295 
##                           Oui 
##                         17376
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(motor_vehicle_access = case_when(
    q50 == "Ne sais pas/Ne s'applique pas" ~ "NA",
    q50 == "Non" ~ "No",
    q50 == "Oui" ~ "Yes"
  ))
table(ibiccs_clean$q50, ibiccs_clean$motor_vehicle_access)
```

```
##                                
##                                    NA    No   Yes
##   Ne sais pas/Ne s'applique pas   230     0     0
##   Non                               0  6295     0
##   Oui                               0     0 17376
```

# Recode Education Level

```r
table(ibiccs_clean$q51)
```

```
## 
##                  Aucun grade, certificat ou diplôme 
##                                                  32 
##                                   Autre (précisez): 
##                                                  81 
##                                        Baccalauréat 
##                                                8638 
##                                               Cégep 
##                                                3197 
## Certificat d'école de métier, certificat ou diplôme 
##                                                2559 
##         Diplôme d'études secondaire ou l'équivalent 
##                                                2471 
##     Diplôme universitaire supérieur au baccalauréat 
##                                                6655 
##                                      École primaire 
##                                                  88 
##                                  Refuse de répondre 
##                                                 180
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(education = case_when(
    q51 == "Aucun grade, certificat ou diplôme" ~ "Other",
    q51 == "Autre (précisez)" ~ "Other",
    q51 == "Baccalauréat" ~ "Baccalaureate",
    q51 == "Cégep" ~ "Cégep",
    q51 == "Certificat d'école de métier, certificat ou diplôme" ~ "Certificate/Diploma",
    q51 == "Diplôme d'études secondaire ou l'équivalent" ~ "High School/Lower",
    q51 == "Diplôme universitaire supérieur au baccalauréat" ~ "Graduate School",
    q51 == "École primaire" ~ "High School/Lower",
    q51 == "Refuse de répondre" ~ "Other"
  ))
table(ibiccs_clean$q51, ibiccs_clean$education)
```

```
##                                                      
##                                                       Baccalaureate Cégep
##   Aucun grade, certificat ou diplôme                              0     0
##   Autre (précisez):                                               0     0
##   Baccalauréat                                                 8638     0
##   Cégep                                                           0  3197
##   Certificat d'école de métier, certificat ou diplôme             0     0
##   Diplôme d'études secondaire ou l'équivalent                     0     0
##   Diplôme universitaire supérieur au baccalauréat                 0     0
##   École primaire                                                  0     0
##   Refuse de répondre                                              0     0
##                                                      
##                                                       Certificate/Diploma
##   Aucun grade, certificat ou diplôme                                    0
##   Autre (précisez):                                                     0
##   Baccalauréat                                                          0
##   Cégep                                                                 0
##   Certificat d'école de métier, certificat ou diplôme                2559
##   Diplôme d'études secondaire ou l'équivalent                           0
##   Diplôme universitaire supérieur au baccalauréat                       0
##   École primaire                                                        0
##   Refuse de répondre                                                    0
##                                                      
##                                                       Graduate School
##   Aucun grade, certificat ou diplôme                                0
##   Autre (précisez):                                                 0
##   Baccalauréat                                                      0
##   Cégep                                                             0
##   Certificat d'école de métier, certificat ou diplôme               0
##   Diplôme d'études secondaire ou l'équivalent                       0
##   Diplôme universitaire supérieur au baccalauréat                6655
##   École primaire                                                    0
##   Refuse de répondre                                                0
##                                                      
##                                                       High School/Lower
##   Aucun grade, certificat ou diplôme                                  0
##   Autre (précisez):                                                   0
##   Baccalauréat                                                        0
##   Cégep                                                               0
##   Certificat d'école de métier, certificat ou diplôme                 0
##   Diplôme d'études secondaire ou l'équivalent                      2471
##   Diplôme universitaire supérieur au baccalauréat                     0
##   École primaire                                                     88
##   Refuse de répondre                                                  0
##                                                      
##                                                       Other
##   Aucun grade, certificat ou diplôme                     32
##   Autre (précisez):                                       0
##   Baccalauréat                                            0
##   Cégep                                                   0
##   Certificat d'école de métier, certificat ou diplôme     0
##   Diplôme d'études secondaire ou l'équivalent             0
##   Diplôme universitaire supérieur au baccalauréat         0
##   École primaire                                          0
##   Refuse de répondre                                    180
```

# Recode Occupation Status

```r
table(ibiccs_clean$Q52_occupational_status_category)
```

```
## 
##                  Disability  Full time or Self-employed 
##                         453                       15373 
## Homemaker or parental leave                   Part time 
##                         983                        1899 
##                     Retired                     Student 
##                        2434                        1586 
##     Unemployed seeking work 
##                         977
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(occupation_status = case_when(
    Q52_occupational_status_category == "Disability" ~ "Unemployed",
    Q52_occupational_status_category == "Full time or Self-employed" ~ "Employed",
    Q52_occupational_status_category == "Homemaker or parental leave" ~ "Unemployed",
    Q52_occupational_status_category == "Part time" ~ "Employed",
    Q52_occupational_status_category == "Retired" ~ "Unemployed",
    Q52_occupational_status_category == "Student" ~ "Student",
    Q52_occupational_status_category == "Unemployed seeking work" ~ "Unemployed"
  ))
table(ibiccs_clean$Q52_occupational_status_category, ibiccs_clean$occupation_status)
```

```
##                              
##                               Employed Student Unemployed
##   Disability                         0       0        453
##   Full time or Self-employed     15373       0          0
##   Homemaker or parental leave        0       0        983
##   Part time                       1899       0          0
##   Retired                            0       0       2434
##   Student                            0    1586          0
##   Unemployed seeking work            0       0        977
```

# Recode Household Income

```r
table(ibiccs_clean$q53)
```

```
## 
##   Entre 10000 $ and 19999 $ par année 
##                                  1084 
## Entre 100000 $ and 149999 $ par année 
##                                  3490 
## Entre 150000 $ and 199999 $ par année 
##                                  1296 
##   Entre 20000 $ and 34999 $ par année 
##                                  2061 
##   Entre 35000 $ and 49999 $ par année 
##                                  2700 
##   Entre 50000 $ and 74999 $ par année 
##                                  4396 
##   Entre 75000 $ and 99999 $ par année 
##                                  3425 
##            Moins de 10000 $ par année 
##                                  1366 
##            Plus de 200000 $ par année 
##                                  1104 
##                    Refuse de répondre 
##                                  2979
```

```r
ibiccs_clean <- ibiccs_clean %>%
  mutate(household_income = case_when(
    q53 == "Moins de 10000 $ par année" ~ "$0-$19999",
    q53 == "Entre 10000 $ and 19999 $ par année" ~ "$0-$19999",
    q53 == "Entre 20000 $ and 34999 $ par année" ~ "$20000-$49999",
    q53 == "Entre 35000 $ and 49999 $ par année" ~ "$20000-$49999",
    q53 == "Entre 50000 $ and 74999 $ par année" ~ "$50000-$74999",
    q53 == "Entre 75000 $ and 99999 $ par année" ~ "$75000-$99999",
    q53 == "Entre 100000 $ and 149999 $ par année" ~ "$100000-149999",
    q53 == "Entre 150000 $ and 199999 $ par année" ~ "$150000+",
    q53 == "Plus de 200000 $ par année" ~ "$150000+",
    q53 == "Refuse de répondre" ~ "Refuse"
  ))
table(ibiccs_clean$q53, ibiccs_clean$household_income)
```

```
##                                        
##                                         $0-$19999 $100000-149999 $150000+
##   Entre 10000 $ and 19999 $ par année        1084              0        0
##   Entre 100000 $ and 149999 $ par année         0           3490        0
##   Entre 150000 $ and 199999 $ par année         0              0     1296
##   Entre 20000 $ and 34999 $ par année           0              0        0
##   Entre 35000 $ and 49999 $ par année           0              0        0
##   Entre 50000 $ and 74999 $ par année           0              0        0
##   Entre 75000 $ and 99999 $ par année           0              0        0
##   Moins de 10000 $ par année                 1366              0        0
##   Plus de 200000 $ par année                    0              0     1104
##   Refuse de répondre                            0              0        0
##                                        
##                                         $20000-$49999 $50000-$74999
##   Entre 10000 $ and 19999 $ par année               0             0
##   Entre 100000 $ and 149999 $ par année             0             0
##   Entre 150000 $ and 199999 $ par année             0             0
##   Entre 20000 $ and 34999 $ par année            2061             0
##   Entre 35000 $ and 49999 $ par année            2700             0
##   Entre 50000 $ and 74999 $ par année               0          4396
##   Entre 75000 $ and 99999 $ par année               0             0
##   Moins de 10000 $ par année                        0             0
##   Plus de 200000 $ par année                        0             0
##   Refuse de répondre                                0             0
##                                        
##                                         $75000-$99999 Refuse
##   Entre 10000 $ and 19999 $ par année               0      0
##   Entre 100000 $ and 149999 $ par année             0      0
##   Entre 150000 $ and 199999 $ par année             0      0
##   Entre 20000 $ and 34999 $ par année               0      0
##   Entre 35000 $ and 49999 $ par année               0      0
##   Entre 50000 $ and 74999 $ par année               0      0
##   Entre 75000 $ and 99999 $ par année            3425      0
##   Moins de 10000 $ par année                        0      0
##   Plus de 200000 $ par année                        0      0
##   Refuse de répondre                                0   2979
```

# Filtering Out Cities

```r
Boston <- filter(ibiccs_clean, ville == "Boston")

Chicago <- filter(ibiccs_clean, ville == "Chicago")

Detroit <- filter(ibiccs_clean, ville == "Détroit")

NewYork <- filter(ibiccs_clean, ville == "New-York")

Philadelphia <- filter(ibiccs_clean, ville == "Philadelphie")

Montreal <- filter(ibiccs_clean, ville == "Montréal")

Toronto <- filter(ibiccs_clean, ville == "Toronto")

Vancouver <- filter(ibiccs_clean, ville == "Vancouver")
```

# Filtering Out Variables in Clean Data

```r
#Add the prepost variable
vars_clean <- c('language', 'ville', 'gender', 'health', 'common_transportation', 'physically_active', 'day_per_week_motor_vehicle', 'day_per_week_public_transit', 'day_per_week_walking', 'day_per_week_bike', 'q42', 'marital_status', 'children_household', 'ethnicity', 'q48', 'motor_vehicle_access', 'education', 'occupation_status', 'household_income', 'bmi', 'bmi_category', 'WalkScore', 'WalkScoreLabel', 'TransitScore', 'TransitScoreLabel', 'BikeScore', 'BikeScoreLabel', 'DiningandDrinkingScore', 'GroceryScore')

ibiccs <- select(ibiccs_clean, vars_clean)
Boston <- select(Boston, vars_clean)
Chicago <- select(Chicago, vars_clean)
Detroit <- select(Detroit, vars_clean)
NewYork <- select(NewYork, vars_clean)
Philadelphia <- select(Philadelphia, vars_clean)
Montreal <- select(Montreal, vars_clean)
Toronto <- select(Toronto, vars_clean)
Vancouver <- select(Vancouver, vars_clean)
city <- rbind(Philadelphia, NewYork, Detroit, Chicago, Boston, Montreal, Toronto, Vancouver)
```

# Binding by Country

```r
city_USA <- rbind(Philadelphia, NewYork, Detroit, Chicago, Boston)
city_CAN <- rbind(Montreal, Toronto, Vancouver)
```


#Tables for Each City and Combined

```r
vars <- c('language', 'ville', 'gender', 'health', 'common_transportation', 'physically_active', 'day_per_week_motor_vehicle', 'day_per_week_public_transit', 'day_per_week_walking', 'day_per_week_bike', 'q42', 'marital_status', 'children_household', 'ethnicity', 'q48', 'motor_vehicle_access', 'education', 'occupation_status', 'household_income', 'bmi', 'bmi_category', 'WalkScore', 'WalkScoreLabel', 'TransitScore', 'TransitScoreLabel', 'BikeScore', 'BikeScoreLabel', 'DiningandDrinkingScore', 'GroceryScore')
CreateTableOne(vars = vars, strata = "ville", data = ibiccs_clean)
```

```
##                                     Stratified by ville
##                                      Boston          Chicago       
##   n                                    1977           4085         
##   language = Fren/Span (%)                3 (  0.2)     20 (  0.5) 
##   ville (%)                                                        
##      Boston                            1977 (100.0)      0 (  0.0) 
##      Chicago                              0 (  0.0)   4085 (100.0) 
##      Détroit                              0 (  0.0)      0 (  0.0) 
##      Montréal                             0 (  0.0)      0 (  0.0) 
##      New-York                             0 (  0.0)      0 (  0.0) 
##      Philadelphie                         0 (  0.0)      0 (  0.0) 
##      Toronto                              0 (  0.0)      0 (  0.0) 
##      Vancouver                            0 (  0.0)      0 (  0.0) 
##   gender = Male (%)                     694 ( 35.1)   1628 ( 39.9) 
##   health (%)                                                       
##      Excellent                         1285 ( 65.0)   2559 ( 62.6) 
##      Good                               521 ( 26.4)   1105 ( 27.1) 
##      NA                                   5 (  0.3)      9 (  0.2) 
##      Poor                               166 (  8.4)    412 ( 10.1) 
##   common_transportation (%)                                        
##      Bicycle                            104 (  5.3)    170 (  4.2) 
##      Car                                685 ( 34.8)   1803 ( 44.3) 
##      Other                               13 (  0.7)     46 (  1.1) 
##      Public Transportation              669 ( 33.9)   1473 ( 36.2) 
##      Walking                            500 ( 25.4)    576 ( 14.2) 
##   physically_active (%)                                            
##      No                                 255 ( 12.9)    626 ( 15.3) 
##      Other                               18 (  0.9)     35 (  0.9) 
##      Yes                               1703 ( 86.2)   3418 ( 83.8) 
##   day_per_week_motor_vehicle (%)                                   
##      0                                  231 ( 11.7)    311 (  7.6) 
##      1                                  261 ( 13.2)    400 (  9.8) 
##      2                                  222 ( 11.2)    479 ( 11.7) 
##      3                                  163 (  8.2)    416 ( 10.2) 
##      4                                  133 (  6.7)    299 (  7.3) 
##      5                                  277 ( 14.0)    614 ( 15.0) 
##      6                                  186 (  9.4)    412 ( 10.1) 
##      7                                  362 ( 18.3)    897 ( 22.0) 
##      NA                                 142 (  7.2)    257 (  6.3) 
##   day_per_week_public_transit (%)                                  
##      0                                  344 ( 17.4)    936 ( 22.9) 
##      1                                  354 ( 17.9)    564 ( 13.8) 
##      2                                  210 ( 10.6)    380 (  9.3) 
##      3                                  130 (  6.6)    286 (  7.0) 
##      4                                  120 (  6.1)    224 (  5.5) 
##      5                                  336 ( 17.0)    751 ( 18.4) 
##      6                                  134 (  6.8)    276 (  6.8) 
##      7                                  165 (  8.3)    235 (  5.8) 
##      NA                                 184 (  9.3)    433 ( 10.6) 
##   day_per_week_walking (%)                                         
##      0                                  103 (  5.2)    295 (  7.2) 
##      1                                  127 (  6.4)    360 (  8.8) 
##      2                                  185 (  9.4)    430 ( 10.5) 
##      3                                  158 (  8.0)    434 ( 10.6) 
##      4                                  163 (  8.2)    325 (  8.0) 
##      5                                  365 ( 18.5)    731 ( 17.9) 
##      6                                  141 (  7.1)    302 (  7.4) 
##      7                                  609 ( 30.8)    864 ( 21.2) 
##      NA                                 126 (  6.4)    344 (  8.4) 
##   day_per_week_bike (%)                                            
##      0                                 1069 ( 54.1)   2080 ( 50.9) 
##      1                                  141 (  7.1)    341 (  8.3) 
##      2                                   70 (  3.5)    229 (  5.6) 
##      3                                   47 (  2.4)    145 (  3.5) 
##      4                                   39 (  2.0)     69 (  1.7) 
##      5                                   69 (  3.5)     96 (  2.4) 
##      6                                   25 (  1.3)     39 (  1.0) 
##      7                                   32 (  1.6)     47 (  1.2) 
##      NA                                 485 ( 24.5)   1039 ( 25.4) 
##   q42 (mean (SD))                     37.21 (13.86)  39.59 (12.73) 
##   marital_status (%)                                               
##      NA                                  23 (  1.3)     33 (  0.9) 
##      Relationship/Married/Common-Law    747 ( 41.6)   1806 ( 49.0) 
##      Single                            1025 ( 57.1)   1844 ( 50.1) 
##   children_household (%)                                           
##      0                                 1627 ( 82.3)   3150 ( 77.1) 
##      1                                  209 ( 10.6)    446 ( 10.9) 
##      2-3                                111 (  5.6)    417 ( 10.2) 
##      4+                                  11 (  0.6)     35 (  0.9) 
##      Refuse                              19 (  1.0)     37 (  0.9) 
##   ethnicity (%)                                                    
##      African American                   110 ( 71.0)    449 ( 87.5) 
##      Arab/Indian/Jewish/Mixed/Other      38 ( 24.5)     56 ( 10.9) 
##      Native American                      7 (  4.5)      8 (  1.6) 
##   q48 (mean (SD))                      6.22 (16.05)   5.55 (14.90) 
##   motor_vehicle_access (%)                                         
##      NA                                  16 (  0.8)     30 (  0.7) 
##      No                                 519 ( 26.3)    789 ( 19.3) 
##      Yes                               1442 ( 72.9)   3266 ( 80.0) 
##   education (%)                                                    
##      Baccalaureate                      757 ( 38.3)   1624 ( 39.9) 
##      Cégep                              231 ( 11.7)    495 ( 12.2) 
##      Certificate/Diploma                 65 (  3.3)    293 (  7.2) 
##      Graduate School                    774 ( 39.2)   1320 ( 32.4) 
##      High School/Lower                  132 (  6.7)    315 (  7.7) 
##      Other                               16 (  0.8)     23 (  0.6) 
##   occupation_status (%)                                            
##      Employed                          1442 ( 73.5)   3160 ( 78.0) 
##      Student                            272 ( 13.9)    271 (  6.7) 
##      Unemployed                         249 ( 12.7)    622 ( 15.3) 
##   household_income (%)                                             
##      $0-$19999                          210 ( 10.6)    436 ( 10.7) 
##      $100000-149999                     304 ( 15.4)    617 ( 15.1) 
##      $150000+                           232 ( 11.7)    438 ( 10.7) 
##      $20000-$49999                      389 ( 19.7)    812 ( 19.9) 
##      $50000-$74999                      342 ( 17.3)    799 ( 19.6) 
##      $75000-$99999                      275 ( 13.9)    604 ( 14.8) 
##      Refuse                             225 ( 11.4)    379 (  9.3) 
##   bmi (mean (SD))                     25.06 (4.59)   25.89 (4.86)  
##   bmi_category (%)                                                 
##      normal weight                      982 ( 49.7)   1745 ( 42.7) 
##      obese                              248 ( 12.5)    698 ( 17.1) 
##      other                              192 (  9.7)    438 ( 10.7) 
##      overweight                         511 ( 25.8)   1139 ( 27.9) 
##      underweight                         44 (  2.2)     65 (  1.6) 
##   WalkScore (mean (SD))               86.19 (17.12)  80.36 (17.45) 
##   WalkScoreLabel (%)                                               
##                                          38 (  1.9)     51 (  1.2) 
##      Car-Dependent                       96 (  4.9)    274 (  6.7) 
##      Somewhat Walkable                   85 (  4.3)    510 ( 12.5) 
##      Very Walkable                      598 ( 30.2)   1680 ( 41.1) 
##      Walker's Paradise                 1160 ( 58.7)   1570 ( 38.4) 
##   TransitScore (mean (SD))            73.62 (16.68)  77.05 (11.31) 
##   TransitScoreLabel (%)                                            
##                                        1694 ( 85.7)   3713 ( 90.9) 
##      Excellent Transit                  201 ( 10.2)    277 (  6.8) 
##      Good Transit                        64 (  3.2)     82 (  2.0) 
##      Minimal Transit                      0 (  0.0)      0 (  0.0) 
##      Rider's Paradise                    16 (  0.8)     13 (  0.3) 
##      Some Transit                         2 (  0.1)      0 (  0.0) 
##   BikeScore (mean (SD))              106.50 (32.02)  87.29 (29.96) 
##   BikeScoreLabel (%)                                               
##                                         598 ( 30.2)    558 ( 13.7) 
##      Bikeable                           475 ( 24.0)   1113 ( 27.2) 
##      Biker's Paradise                   323 ( 16.3)      0 (  0.0) 
##      Somewhat Bikeable                   26 (  1.3)    264 (  6.5) 
##      Very Bikeable                      555 ( 28.1)   2150 ( 52.6) 
##   DiningandDrinkingScore (mean (SD))  86.12 (17.23)  82.41 (18.59) 
##   GroceryScore (mean (SD))            88.94 (21.26)  78.71 (28.21) 
##                                     Stratified by ville
##                                      Détroit         Montréal      
##   n                                    3077           2678         
##   language = Fren/Span (%)                3 (  0.1)   1617 ( 60.4) 
##   ville (%)                                                        
##      Boston                               0 (  0.0)      0 (  0.0) 
##      Chicago                              0 (  0.0)      0 (  0.0) 
##      Détroit                           3077 (100.0)      0 (  0.0) 
##      Montréal                             0 (  0.0)   2678 (100.0) 
##      New-York                             0 (  0.0)      0 (  0.0) 
##      Philadelphie                         0 (  0.0)      0 (  0.0) 
##      Toronto                              0 (  0.0)      0 (  0.0) 
##      Vancouver                            0 (  0.0)      0 (  0.0) 
##   gender = Male (%)                    1134 ( 36.9)   1159 ( 43.3) 
##   health (%)                                                       
##      Excellent                         1758 ( 57.1)   1501 ( 56.0) 
##      Good                               928 ( 30.2)    791 ( 29.5) 
##      NA                                   1 (  0.0)      3 (  0.1) 
##      Poor                               390 ( 12.7)    383 ( 14.3) 
##   common_transportation (%)                                        
##      Bicycle                             75 (  2.5)    207 (  7.8) 
##      Car                               2718 ( 88.9)    914 ( 34.3) 
##      Other                               21 (  0.7)     30 (  1.1) 
##      Public Transportation              105 (  3.4)   1019 ( 38.3) 
##      Walking                            139 (  4.5)    492 ( 18.5) 
##   physically_active (%)                                            
##      No                                 680 ( 22.1)    528 ( 19.7) 
##      Other                               38 (  1.2)     40 (  1.5) 
##      Yes                               2355 ( 76.6)   2107 ( 78.8) 
##   day_per_week_motor_vehicle (%)                                   
##      0                                   41 (  1.3)    449 ( 16.8) 
##      1                                   67 (  2.2)    298 ( 11.1) 
##      2                                  106 (  3.4)    357 ( 13.3) 
##      3                                  159 (  5.2)    209 (  7.8) 
##      4                                  161 (  5.2)    179 (  6.7) 
##      5                                  433 ( 14.1)    340 ( 12.7) 
##      6                                  392 ( 12.7)    175 (  6.5) 
##      7                                 1529 ( 49.7)    372 ( 13.9) 
##      NA                                 189 (  6.1)    299 ( 11.2) 
##   day_per_week_public_transit (%)                                  
##      0                                 1988 ( 64.6)    723 ( 27.0) 
##      1                                  102 (  3.3)    332 ( 12.4) 
##      2                                   77 (  2.5)    250 (  9.3) 
##      3                                   45 (  1.5)    194 (  7.2) 
##      4                                   33 (  1.1)    155 (  5.8) 
##      5                                   77 (  2.5)    430 ( 16.1) 
##      6                                   26 (  0.8)    159 (  5.9) 
##      7                                   48 (  1.6)    174 (  6.5) 
##      NA                                 681 ( 22.1)    261 (  9.7) 
##   day_per_week_walking (%)                                         
##      0                                  903 ( 29.3)    233 (  8.7) 
##      1                                  328 ( 10.7)    223 (  8.3) 
##      2                                  334 ( 10.9)    286 ( 10.7) 
##      3                                  238 (  7.7)    273 ( 10.2) 
##      4                                  165 (  5.4)    226 (  8.4) 
##      5                                  294 (  9.6)    482 ( 18.0) 
##      6                                   81 (  2.6)    170 (  6.3) 
##      7                                  238 (  7.7)    509 ( 19.0) 
##      NA                                 496 ( 16.1)    276 ( 10.3) 
##   day_per_week_bike (%)                                            
##      0                                 1711 ( 55.6)   1500 ( 56.0) 
##      1                                  191 (  6.2)    183 (  6.8) 
##      2                                  135 (  4.4)    141 (  5.3) 
##      3                                   77 (  2.5)     96 (  3.6) 
##      4                                   47 (  1.5)     72 (  2.7) 
##      5                                   54 (  1.8)     99 (  3.7) 
##      6                                   22 (  0.7)     21 (  0.8) 
##      7                                   26 (  0.8)     34 (  1.3) 
##      NA                                 814 ( 26.5)    532 ( 19.9) 
##   q42 (mean (SD))                     41.11 (12.74)  47.02 (14.17) 
##   marital_status (%)                                               
##      NA                                  24 (  0.9)     29 (  1.3) 
##      Relationship/Married/Common-Law   1710 ( 62.8)   1333 ( 58.8) 
##      Single                             988 ( 36.3)    906 ( 39.9) 
##   children_household (%)                                           
##      0                                 1954 ( 63.5)   2103 ( 78.5) 
##      1                                  523 ( 17.0)    321 ( 12.0) 
##      2-3                                516 ( 16.8)    219 (  8.2) 
##      4+                                  49 (  1.6)     13 (  0.5) 
##      Refuse                              35 (  1.1)     22 (  0.8) 
##   ethnicity (%)                                                    
##      African American                   361 ( 87.2)     52 ( 50.0) 
##      Arab/Indian/Jewish/Mixed/Other      38 (  9.2)     44 ( 42.3) 
##      Native American                     15 (  3.6)      8 (  7.7) 
##   q48 (mean (SD))                      4.51 (12.93)   8.11 (20.21) 
##   motor_vehicle_access (%)                                         
##      NA                                  18 (  0.6)     20 (  0.7) 
##      No                                 123 (  4.0)    837 ( 31.3) 
##      Yes                               2936 ( 95.4)   1821 ( 68.0) 
##   education (%)                                                    
##      Baccalaureate                      951 ( 31.0)    788 ( 29.5) 
##      Cégep                              506 ( 16.5)    528 ( 19.8) 
##      Certificate/Diploma                444 ( 14.5)    330 ( 12.4) 
##      Graduate School                    736 ( 24.0)    581 ( 21.8) 
##      High School/Lower                  410 ( 13.4)    401 ( 15.0) 
##      Other                               18 (  0.6)     42 (  1.6) 
##   occupation_status (%)                                            
##      Employed                          2152 ( 70.4)   1750 ( 65.8) 
##      Student                            243 (  7.9)    141 (  5.3) 
##      Unemployed                         663 ( 21.7)    769 ( 28.9) 
##   household_income (%)                                             
##      $0-$19999                          412 ( 13.4)    286 ( 10.7) 
##      $100000-149999                     443 ( 14.4)    271 ( 10.1) 
##      $150000+                           193 (  6.3)    148 (  5.5) 
##      $20000-$49999                      666 ( 21.6)    731 ( 27.3) 
##      $50000-$74999                      536 ( 17.4)    523 ( 19.5) 
##      $75000-$99999                      461 ( 15.0)    365 ( 13.6) 
##      Refuse                             366 ( 11.9)    354 ( 13.2) 
##   bmi (mean (SD))                     26.75 (5.11)   25.88 (4.65)  
##   bmi_category (%)                                                 
##      normal weight                     1073 ( 34.9)   1122 ( 41.9) 
##      obese                              661 ( 21.5)    454 ( 17.0) 
##      other                              439 ( 14.3)    321 ( 12.0) 
##      overweight                         864 ( 28.1)    746 ( 27.9) 
##      underweight                         40 (  1.3)     35 (  1.3) 
##   WalkScore (mean (SD))               36.05 (24.44)  81.88 (16.05) 
##   WalkScoreLabel (%)                                               
##                                          27 (  0.9)     28 (  1.0) 
##      Car-Dependent                     2094 ( 68.1)    135 (  5.0) 
##      Somewhat Walkable                  733 ( 23.8)    303 ( 11.3) 
##      Very Walkable                      175 (  5.7)   1117 ( 41.7) 
##      Walker's Paradise                   48 (  1.6)   1095 ( 40.9) 
##   TransitScore (mean (SD))            79.31 (7.37)   13.38 (18.57) 
##   TransitScoreLabel (%)                                            
##                                        3077 (100.0)     45 (  1.7) 
##      Excellent Transit                    0 (  0.0)     71 (  2.7) 
##      Good Transit                         0 (  0.0)    485 ( 18.1) 
##      Minimal Transit                      0 (  0.0)     43 (  1.6) 
##      Rider's Paradise                     0 (  0.0)   1911 ( 71.4) 
##      Some Transit                         0 (  0.0)    123 (  4.6) 
##   BikeScore (mean (SD))              133.74 (18.39)  86.99 (43.07) 
##   BikeScoreLabel (%)                                               
##                                        2898 ( 94.2)    624 ( 23.3) 
##      Bikeable                            90 (  2.9)    719 ( 26.8) 
##      Biker's Paradise                    40 (  1.3)    496 ( 18.5) 
##      Somewhat Bikeable                    1 (  0.0)     83 (  3.1) 
##      Very Bikeable                       48 (  1.6)    756 ( 28.2) 
##   DiningandDrinkingScore (mean (SD))  40.56 (26.92)  82.34 (17.58) 
##   GroceryScore (mean (SD))            38.81 (34.49)  89.47 (17.42) 
##                                     Stratified by ville
##                                      New-York       Philadelphie   
##   n                                   3824            1478         
##   language = Fren/Span (%)              16 (  0.4)       3 (  0.2) 
##   ville (%)                                                        
##      Boston                              0 (  0.0)       0 (  0.0) 
##      Chicago                             0 (  0.0)       0 (  0.0) 
##      Détroit                             0 (  0.0)       0 (  0.0) 
##      Montréal                            0 (  0.0)       0 (  0.0) 
##      New-York                         3824 (100.0)       0 (  0.0) 
##      Philadelphie                        0 (  0.0)    1478 (100.0) 
##      Toronto                             0 (  0.0)       0 (  0.0) 
##      Vancouver                           0 (  0.0)       0 (  0.0) 
##   gender = Male (%)                   1593 ( 41.7)     536 ( 36.3) 
##   health (%)                                                       
##      Excellent                        2537 ( 66.3)     928 ( 62.8) 
##      Good                              986 ( 25.8)     404 ( 27.3) 
##      NA                                  6 (  0.2)       2 (  0.1) 
##      Poor                              295 (  7.7)     144 (  9.7) 
##   common_transportation (%)                                        
##      Bicycle                           184 (  4.8)      87 (  5.9) 
##      Car                               360 (  9.4)     606 ( 41.2) 
##      Other                              81 (  2.1)      15 (  1.0) 
##      Public Transportation            2207 ( 57.9)     321 ( 21.8) 
##      Walking                           978 ( 25.7)     443 ( 30.1) 
##   physically_active (%)                                            
##      No                                680 ( 17.8)     270 ( 18.3) 
##      Other                              51 (  1.3)      14 (  0.9) 
##      Yes                              3088 ( 80.9)    1193 ( 80.8) 
##   day_per_week_motor_vehicle (%)                                   
##      0                                 968 ( 25.3)     128 (  8.7) 
##      1                                 594 ( 15.5)     178 ( 12.0) 
##      2                                 487 ( 12.7)     192 ( 13.0) 
##      3                                 323 (  8.4)     128 (  8.7) 
##      4                                 237 (  6.2)     105 (  7.1) 
##      5                                 306 (  8.0)     209 ( 14.1) 
##      6                                 135 (  3.5)     117 (  7.9) 
##      7                                 223 (  5.8)     302 ( 20.4) 
##      NA                                551 ( 14.4)     119 (  8.1) 
##   day_per_week_public_transit (%)                                  
##      0                                 229 (  6.0)     390 ( 26.4) 
##      1                                 320 (  8.4)     234 ( 15.8) 
##      2                                 368 (  9.6)     151 ( 10.2) 
##      3                                 365 (  9.5)      99 (  6.7) 
##      4                                 298 (  7.8)      85 (  5.8) 
##      5                                 844 ( 22.1)     222 ( 15.0) 
##      6                                 477 ( 12.5)      58 (  3.9) 
##      7                                 588 ( 15.4)      53 (  3.6) 
##      NA                                335 (  8.8)     186 ( 12.6) 
##   day_per_week_walking (%)                                         
##      0                                  69 (  1.8)     101 (  6.8) 
##      1                                 160 (  4.2)     100 (  6.8) 
##      2                                 257 (  6.7)     157 ( 10.6) 
##      3                                 287 (  7.5)     137 (  9.3) 
##      4                                 303 (  7.9)      98 (  6.6) 
##      5                                 629 ( 16.4)     282 ( 19.1) 
##      6                                 388 ( 10.1)     100 (  6.8) 
##      7                                1386 ( 36.2)     366 ( 24.8) 
##      NA                                345 (  9.0)     137 (  9.3) 
##   day_per_week_bike (%)                                            
##      0                                1851 ( 48.4)     722 ( 48.8) 
##      1                                 298 (  7.8)     113 (  7.6) 
##      2                                 199 (  5.2)      87 (  5.9) 
##      3                                 131 (  3.4)      48 (  3.2) 
##      4                                  93 (  2.4)      34 (  2.3) 
##      5                                 107 (  2.8)      40 (  2.7) 
##      6                                  24 (  0.6)      19 (  1.3) 
##      7                                  44 (  1.2)      26 (  1.8) 
##      NA                               1077 ( 28.2)     389 ( 26.3) 
##   q42 (mean (SD))                    39.74 (13.55)   39.19 (13.51) 
##   marital_status (%)                                               
##      NA                                 53 (  1.5)      13 (  1.0) 
##      Relationship/Married/Common-Law  1557 ( 44.6)     582 ( 44.0) 
##      Single                           1879 ( 53.9)     729 ( 55.1) 
##   children_household (%)                                           
##      0                                2959 ( 77.4)    1120 ( 75.8) 
##      1                                 480 ( 12.6)     177 ( 12.0) 
##      2-3                               306 (  8.0)     146 (  9.9) 
##      4+                                 31 (  0.8)      20 (  1.4) 
##      Refuse                             48 (  1.3)      15 (  1.0) 
##   ethnicity (%)                                                    
##      African American                  336 ( 81.8)     231 ( 89.2) 
##      Arab/Indian/Jewish/Mixed/Other     66 ( 16.1)      28 ( 10.8) 
##      Native American                     9 (  2.2)       0 (  0.0) 
##   q48 (mean (SD))                     7.41 (17.89)    4.50 (12.55) 
##   motor_vehicle_access (%)                                         
##      NA                                 47 (  1.2)      16 (  1.1) 
##      No                               2106 ( 55.1)     353 ( 23.9) 
##      Yes                              1671 ( 43.7)    1109 ( 75.0) 
##   education (%)                                                    
##      Baccalaureate                    1589 ( 41.6)     509 ( 34.7) 
##      Cégep                             466 ( 12.2)     206 ( 14.0) 
##      Certificate/Diploma               168 (  4.4)      86 (  5.9) 
##      Graduate School                  1284 ( 33.6)     447 ( 30.5) 
##      High School/Lower                 291 (  7.6)     210 ( 14.3) 
##      Other                              22 (  0.6)       9 (  0.6) 
##   occupation_status (%)                                            
##      Employed                         2943 ( 77.7)    1049 ( 71.3) 
##      Student                           270 (  7.1)     172 ( 11.7) 
##      Unemployed                        574 ( 15.2)     251 ( 17.1) 
##   household_income (%)                                             
##      $0-$19999                         397 ( 10.4)     227 ( 15.4) 
##      $100000-149999                    631 ( 16.5)     182 ( 12.3) 
##      $150000+                          598 ( 15.6)     124 (  8.4) 
##      $20000-$49999                     605 ( 15.8)     335 ( 22.7) 
##      $50000-$74999                     611 ( 16.0)     301 ( 20.4) 
##      $75000-$99999                     545 ( 14.3)     182 ( 12.3) 
##      Refuse                            437 ( 11.4)     127 (  8.6) 
##   bmi (mean (SD))                    25.05 (4.52)    26.02 (4.97)  
##   bmi_category (%)                                                 
##      normal weight                    1841 ( 48.1)     618 ( 41.8) 
##      obese                             490 ( 12.8)     273 ( 18.5) 
##      other                             450 ( 11.8)     164 ( 11.1) 
##      overweight                        971 ( 25.4)     404 ( 27.3) 
##      underweight                        72 (  1.9)      19 (  1.3) 
##   WalkScore (mean (SD))              95.88 (11.04)   86.27 (17.72) 
##   WalkScoreLabel (%)                                               
##                                         65 (  1.7)      22 (  1.5) 
##      Car-Dependent                      61 (  1.6)      83 (  5.6) 
##      Somewhat Walkable                  33 (  0.9)      96 (  6.5) 
##      Very Walkable                     169 (  4.4)     244 ( 16.5) 
##      Walker's Paradise                3496 ( 91.4)    1033 ( 69.9) 
##   TransitScore (mean (SD))           71.19 (24.19)   73.31 (21.71) 
##   TransitScoreLabel (%)                                            
##                                       3394 ( 88.8)    1371 ( 92.8) 
##      Excellent Transit                  34 (  0.9)       0 (  0.0) 
##      Good Transit                        7 (  0.2)       2 (  0.1) 
##      Minimal Transit                     0 (  0.0)       0 (  0.0) 
##      Rider's Paradise                  389 ( 10.2)     105 (  7.1) 
##      Some Transit                        0 (  0.0)       0 (  0.0) 
##   BikeScore (mean (SD))              90.15 (24.92)  106.32 (30.52) 
##   BikeScoreLabel (%)                                               
##                                        178 (  4.7)     148 ( 10.0) 
##      Bikeable                         1137 ( 29.7)     236 ( 16.0) 
##      Biker's Paradise                  145 (  3.8)     594 ( 40.2) 
##      Somewhat Bikeable                  71 (  1.9)      38 (  2.6) 
##      Very Bikeable                    2293 ( 60.0)     462 ( 31.3) 
##   DiningandDrinkingScore (mean (SD)) 96.05 (11.20)   87.70 (16.44) 
##   GroceryScore (mean (SD))           97.91 (11.21)   90.43 (22.07) 
##                                     Stratified by ville
##                                      Toronto        Vancouver      p     
##   n                                   4264           2518                
##   language = Fren/Span (%)               3 (  0.1)      0 (  0.0)  <0.001
##   ville (%)                                                        <0.001
##      Boston                              0 (  0.0)      0 (  0.0)        
##      Chicago                             0 (  0.0)      0 (  0.0)        
##      Détroit                             0 (  0.0)      0 (  0.0)        
##      Montréal                            0 (  0.0)      0 (  0.0)        
##      New-York                            0 (  0.0)      0 (  0.0)        
##      Philadelphie                        0 (  0.0)      0 (  0.0)        
##      Toronto                          4264 (100.0)      0 (  0.0)        
##      Vancouver                           0 (  0.0)   2518 (100.0)        
##   gender = Male (%)                   1984 ( 46.5)   1131 ( 44.9)  <0.001
##   health (%)                                                       <0.001
##      Excellent                        2572 ( 60.3)   1404 ( 55.8)        
##      Good                             1220 ( 28.6)    770 ( 30.6)        
##      NA                                  8 (  0.2)      5 (  0.2)        
##      Poor                              464 ( 10.9)    339 ( 13.5)        
##   common_transportation (%)                                        <0.001
##      Bicycle                           231 (  5.4)    119 (  4.7)        
##      Car                              1665 ( 39.2)   1245 ( 49.5)        
##      Other                              43 (  1.0)     13 (  0.5)        
##      Public Transportation            1459 ( 34.3)    633 ( 25.2)        
##      Walking                           853 ( 20.1)    503 ( 20.0)        
##   physically_active (%)                                            <0.001
##      No                                713 ( 16.7)    372 ( 14.8)        
##      Other                              52 (  1.2)     28 (  1.1)        
##      Yes                              3493 ( 82.0)   2114 ( 84.1)        
##   day_per_week_motor_vehicle (%)                                   <0.001
##      0                                 470 ( 11.0)    187 (  7.4)        
##      1                                 484 ( 11.4)    245 (  9.7)        
##      2                                 504 ( 11.8)    284 ( 11.3)        
##      3                                 430 ( 10.1)    242 (  9.6)        
##      4                                 311 (  7.3)    223 (  8.9)        
##      5                                 571 ( 13.4)    386 ( 15.3)        
##      6                                 349 (  8.2)    253 ( 10.0)        
##      7                                 731 ( 17.1)    509 ( 20.2)        
##      NA                                414 (  9.7)    189 (  7.5)        
##   day_per_week_public_transit (%)                                  <0.001
##      0                                 857 ( 20.1)    702 ( 27.9)        
##      1                                 674 ( 15.8)    413 ( 16.4)        
##      2                                 467 ( 11.0)    264 ( 10.5)        
##      3                                 325 (  7.6)    154 (  6.1)        
##      4                                 296 (  6.9)    128 (  5.1)        
##      5                                 634 ( 14.9)    323 ( 12.8)        
##      6                                 245 (  5.7)    107 (  4.2)        
##      7                                 223 (  5.2)     97 (  3.9)        
##      NA                                543 ( 12.7)    330 ( 13.1)        
##   day_per_week_walking (%)                                         <0.001
##      0                                 248 (  5.8)    163 (  6.5)        
##      1                                 348 (  8.2)    195 (  7.7)        
##      2                                 464 ( 10.9)    248 (  9.8)        
##      3                                 449 ( 10.5)    269 ( 10.7)        
##      4                                 341 (  8.0)    210 (  8.3)        
##      5                                 756 ( 17.7)    455 ( 18.1)        
##      6                                 328 (  7.7)    201 (  8.0)        
##      7                                 911 ( 21.4)    553 ( 22.0)        
##      NA                                419 (  9.8)    224 (  8.9)        
##   day_per_week_bike (%)                                            <0.001
##      0                                1844 ( 43.2)   1233 ( 49.0)        
##      1                                 341 (  8.0)    185 (  7.3)        
##      2                                 200 (  4.7)    124 (  4.9)        
##      3                                 110 (  2.6)     87 (  3.5)        
##      4                                  77 (  1.8)     31 (  1.2)        
##      5                                 114 (  2.7)     53 (  2.1)        
##      6                                  45 (  1.1)     20 (  0.8)        
##      7                                  61 (  1.4)     24 (  1.0)        
##      NA                               1472 ( 34.5)    761 ( 30.2)        
##   q42 (mean (SD))                    46.98 (14.52)  47.49 (14.51)  <0.001
##   marital_status (%)                                               <0.001
##      NA                                 73 (  2.0)     43 (  2.0)        
##      Relationship/Married/Common-Law  2228 ( 61.1)   1403 ( 64.3)        
##      Single                           1345 ( 36.9)    736 ( 33.7)        
##   children_household (%)                                           <0.001
##      0                                3375 ( 79.2)   1903 ( 75.6)        
##      1                                 454 ( 10.6)    305 ( 12.1)        
##      2-3                               361 (  8.5)    258 ( 10.2)        
##      4+                                 22 (  0.5)      5 (  0.2)        
##      Refuse                             52 (  1.2)     47 (  1.9)        
##   ethnicity (%)                                                    <0.001
##      African American                   96 ( 35.6)     19 ( 17.6)        
##      Arab/Indian/Jewish/Mixed/Other    160 ( 59.3)     71 ( 65.7)        
##      Native American                    14 (  5.2)     18 ( 16.7)        
##   q48 (mean (SD))                    12.16 (24.93)  10.41 (22.52)  <0.001
##   motor_vehicle_access (%)                                         <0.001
##      NA                                 60 (  1.4)     23 (  0.9)        
##      No                               1147 ( 26.9)    421 ( 16.7)        
##      Yes                              3057 ( 71.7)   2074 ( 82.4)        
##   education (%)                                                    <0.001
##      Baccalaureate                    1543 ( 36.3)    877 ( 35.1)        
##      Cégep                             451 ( 10.6)    314 ( 12.6)        
##      Certificate/Diploma               746 ( 17.5)    427 ( 17.1)        
##      Graduate School                   995 ( 23.4)    518 ( 20.7)        
##      High School/Lower                 478 ( 11.2)    322 ( 12.9)        
##      Other                              42 (  1.0)     40 (  1.6)        
##   occupation_status (%)                                            <0.001
##      Employed                         3051 ( 72.2)   1725 ( 69.4)        
##      Student                           134 (  3.2)     83 (  3.3)        
##      Unemployed                       1040 ( 24.6)    679 ( 27.3)        
##   household_income (%)                                             <0.001
##      $0-$19999                         301 (  7.1)    181 (  7.2)        
##      $100000-149999                    673 ( 15.8)    369 ( 14.7)        
##      $150000+                          463 ( 10.9)    204 (  8.1)        
##      $20000-$49999                     768 ( 18.0)    455 ( 18.1)        
##      $50000-$74999                     780 ( 18.3)    504 ( 20.0)        
##      $75000-$99999                     619 ( 14.5)    374 ( 14.9)        
##      Refuse                            660 ( 15.5)    431 ( 17.1)        
##   bmi (mean (SD))                    25.60 (4.46)   24.87 (4.27)   <0.001
##   bmi_category (%)                                                 <0.001
##      normal weight                    1843 ( 43.2)   1213 ( 48.2)        
##      obese                             586 ( 13.7)    262 ( 10.4)        
##      other                             498 ( 11.7)    239 (  9.5)        
##      overweight                       1276 ( 29.9)    747 ( 29.7)        
##      underweight                        61 (  1.4)     57 (  2.3)        
##   WalkScore (mean (SD))              79.56 (18.77)  78.79 (19.52)  <0.001
##   WalkScoreLabel (%)                                               <0.001
##                                         26 (  0.6)     27 (  1.1)        
##      Car-Dependent                     351 (  8.2)    243 (  9.7)        
##      Somewhat Walkable                 700 ( 16.4)    403 ( 16.0)        
##      Very Walkable                    1467 ( 34.4)    869 ( 34.5)        
##      Walker's Paradise                1720 ( 40.3)    976 ( 38.8)        
##   TransitScore (mean (SD))           48.42 (25.00)  45.29 (19.95)  <0.001
##   TransitScoreLabel (%)                                            <0.001
##                                         33 (  0.8)     88 (  3.5)        
##      Excellent Transit                1547 ( 36.3)   1035 ( 41.1)        
##      Good Transit                      939 ( 22.0)    865 ( 34.4)        
##      Minimal Transit                    34 (  0.8)      5 (  0.2)        
##      Rider's Paradise                 1671 ( 39.2)    371 ( 14.7)        
##      Some Transit                       40 (  0.9)    154 (  6.1)        
##   BikeScore (mean (SD))              70.90 (30.62)  92.61 (36.44)  <0.001
##   BikeScoreLabel (%)                                               <0.001
##                                        120 (  2.8)    512 ( 20.3)        
##      Bikeable                         2214 ( 51.9)    745 ( 29.6)        
##      Biker's Paradise                  431 ( 10.1)    422 ( 16.8)        
##      Somewhat Bikeable                 500 ( 11.7)    179 (  7.1)        
##      Very Bikeable                     999 ( 23.4)    660 ( 26.2)        
##   DiningandDrinkingScore (mean (SD)) 81.64 (19.10)  81.17 (19.59)  <0.001
##   GroceryScore (mean (SD))           82.99 (23.65)  84.45 (22.22)  <0.001
##                                     Stratified by ville
##                                      test
##   n                                      
##   language = Fren/Span (%)               
##   ville (%)                              
##      Boston                              
##      Chicago                             
##      Détroit                             
##      Montréal                            
##      New-York                            
##      Philadelphie                        
##      Toronto                             
##      Vancouver                           
##   gender = Male (%)                      
##   health (%)                             
##      Excellent                           
##      Good                                
##      NA                                  
##      Poor                                
##   common_transportation (%)              
##      Bicycle                             
##      Car                                 
##      Other                               
##      Public Transportation               
##      Walking                             
##   physically_active (%)                  
##      No                                  
##      Other                               
##      Yes                                 
##   day_per_week_motor_vehicle (%)         
##      0                                   
##      1                                   
##      2                                   
##      3                                   
##      4                                   
##      5                                   
##      6                                   
##      7                                   
##      NA                                  
##   day_per_week_public_transit (%)        
##      0                                   
##      1                                   
##      2                                   
##      3                                   
##      4                                   
##      5                                   
##      6                                   
##      7                                   
##      NA                                  
##   day_per_week_walking (%)               
##      0                                   
##      1                                   
##      2                                   
##      3                                   
##      4                                   
##      5                                   
##      6                                   
##      7                                   
##      NA                                  
##   day_per_week_bike (%)                  
##      0                                   
##      1                                   
##      2                                   
##      3                                   
##      4                                   
##      5                                   
##      6                                   
##      7                                   
##      NA                                  
##   q42 (mean (SD))                        
##   marital_status (%)                     
##      NA                                  
##      Relationship/Married/Common-Law     
##      Single                              
##   children_household (%)                 
##      0                                   
##      1                                   
##      2-3                                 
##      4+                                  
##      Refuse                              
##   ethnicity (%)                          
##      African American                    
##      Arab/Indian/Jewish/Mixed/Other      
##      Native American                     
##   q48 (mean (SD))                        
##   motor_vehicle_access (%)               
##      NA                                  
##      No                                  
##      Yes                                 
##   education (%)                          
##      Baccalaureate                       
##      Cégep                               
##      Certificate/Diploma                 
##      Graduate School                     
##      High School/Lower                   
##      Other                               
##   occupation_status (%)                  
##      Employed                            
##      Student                             
##      Unemployed                          
##   household_income (%)                   
##      $0-$19999                           
##      $100000-149999                      
##      $150000+                            
##      $20000-$49999                       
##      $50000-$74999                       
##      $75000-$99999                       
##      Refuse                              
##   bmi (mean (SD))                        
##   bmi_category (%)                       
##      normal weight                       
##      obese                               
##      other                               
##      overweight                          
##      underweight                         
##   WalkScore (mean (SD))                  
##   WalkScoreLabel (%)                     
##                                          
##      Car-Dependent                       
##      Somewhat Walkable                   
##      Very Walkable                       
##      Walker's Paradise                   
##   TransitScore (mean (SD))               
##   TransitScoreLabel (%)                  
##                                          
##      Excellent Transit                   
##      Good Transit                        
##      Minimal Transit                     
##      Rider's Paradise                    
##      Some Transit                        
##   BikeScore (mean (SD))                  
##   BikeScoreLabel (%)                     
##                                          
##      Bikeable                            
##      Biker's Paradise                    
##      Somewhat Bikeable                   
##      Very Bikeable                       
##   DiningandDrinkingScore (mean (SD))     
##   GroceryScore (mean (SD))
```

```r
CreateTableOne(vars = vars, data = Chicago)
```

```
##                                     
##                                      Overall       
##   n                                   4085         
##   language = Fren/Span (%)              20 (  0.5) 
##   ville (%)                                        
##      Boston                              0 (  0.0) 
##      Chicago                          4085 (100.0) 
##      Détroit                             0 (  0.0) 
##      Montréal                            0 (  0.0) 
##      New-York                            0 (  0.0) 
##      Philadelphie                        0 (  0.0) 
##      Toronto                             0 (  0.0) 
##      Vancouver                           0 (  0.0) 
##   gender = Male (%)                   1628 ( 39.9) 
##   health (%)                                       
##      Excellent                        2559 ( 62.6) 
##      Good                             1105 ( 27.1) 
##      NA                                  9 (  0.2) 
##      Poor                              412 ( 10.1) 
##   common_transportation (%)                        
##      Bicycle                           170 (  4.2) 
##      Car                              1803 ( 44.3) 
##      Other                              46 (  1.1) 
##      Public Transportation            1473 ( 36.2) 
##      Walking                           576 ( 14.2) 
##   physically_active (%)                            
##      No                                626 ( 15.3) 
##      Other                              35 (  0.9) 
##      Yes                              3418 ( 83.8) 
##   day_per_week_motor_vehicle (%)                   
##      0                                 311 (  7.6) 
##      1                                 400 (  9.8) 
##      2                                 479 ( 11.7) 
##      3                                 416 ( 10.2) 
##      4                                 299 (  7.3) 
##      5                                 614 ( 15.0) 
##      6                                 412 ( 10.1) 
##      7                                 897 ( 22.0) 
##      NA                                257 (  6.3) 
##   day_per_week_public_transit (%)                  
##      0                                 936 ( 22.9) 
##      1                                 564 ( 13.8) 
##      2                                 380 (  9.3) 
##      3                                 286 (  7.0) 
##      4                                 224 (  5.5) 
##      5                                 751 ( 18.4) 
##      6                                 276 (  6.8) 
##      7                                 235 (  5.8) 
##      NA                                433 ( 10.6) 
##   day_per_week_walking (%)                         
##      0                                 295 (  7.2) 
##      1                                 360 (  8.8) 
##      2                                 430 ( 10.5) 
##      3                                 434 ( 10.6) 
##      4                                 325 (  8.0) 
##      5                                 731 ( 17.9) 
##      6                                 302 (  7.4) 
##      7                                 864 ( 21.2) 
##      NA                                344 (  8.4) 
##   day_per_week_bike (%)                            
##      0                                2080 ( 50.9) 
##      1                                 341 (  8.3) 
##      2                                 229 (  5.6) 
##      3                                 145 (  3.5) 
##      4                                  69 (  1.7) 
##      5                                  96 (  2.4) 
##      6                                  39 (  1.0) 
##      7                                  47 (  1.2) 
##      NA                               1039 ( 25.4) 
##   q42 (mean (SD))                    39.59 (12.73) 
##   marital_status (%)                               
##      NA                                 33 (  0.9) 
##      Relationship/Married/Common-Law  1806 ( 49.0) 
##      Single                           1844 ( 50.1) 
##   children_household (%)                           
##      0                                3150 ( 77.1) 
##      1                                 446 ( 10.9) 
##      2-3                               417 ( 10.2) 
##      4+                                 35 (  0.9) 
##      Refuse                             37 (  0.9) 
##   ethnicity (%)                                    
##      African American                  449 ( 87.5) 
##      Arab/Indian/Jewish/Mixed/Other     56 ( 10.9) 
##      Native American                     8 (  1.6) 
##   q48 (mean (SD))                     5.55 (14.90) 
##   motor_vehicle_access (%)                         
##      NA                                 30 (  0.7) 
##      No                                789 ( 19.3) 
##      Yes                              3266 ( 80.0) 
##   education (%)                                    
##      Baccalaureate                    1624 ( 39.9) 
##      Cégep                             495 ( 12.2) 
##      Certificate/Diploma               293 (  7.2) 
##      Graduate School                  1320 ( 32.4) 
##      High School/Lower                 315 (  7.7) 
##      Other                              23 (  0.6) 
##   occupation_status (%)                            
##      Employed                         3160 ( 78.0) 
##      Student                           271 (  6.7) 
##      Unemployed                        622 ( 15.3) 
##   household_income (%)                             
##      $0-$19999                         436 ( 10.7) 
##      $100000-149999                    617 ( 15.1) 
##      $150000+                          438 ( 10.7) 
##      $20000-$49999                     812 ( 19.9) 
##      $50000-$74999                     799 ( 19.6) 
##      $75000-$99999                     604 ( 14.8) 
##      Refuse                            379 (  9.3) 
##   bmi (mean (SD))                    25.89 (4.86)  
##   bmi_category (%)                                 
##      normal weight                    1745 ( 42.7) 
##      obese                             698 ( 17.1) 
##      other                             438 ( 10.7) 
##      overweight                       1139 ( 27.9) 
##      underweight                        65 (  1.6) 
##   WalkScore (mean (SD))              80.36 (17.45) 
##   WalkScoreLabel (%)                               
##                                         51 (  1.2) 
##      Car-Dependent                     274 (  6.7) 
##      Somewhat Walkable                 510 ( 12.5) 
##      Very Walkable                    1680 ( 41.1) 
##      Walker's Paradise                1570 ( 38.4) 
##   TransitScore (mean (SD))           77.05 (11.31) 
##   TransitScoreLabel (%)                            
##                                       3713 ( 90.9) 
##      Excellent Transit                 277 (  6.8) 
##      Good Transit                       82 (  2.0) 
##      Minimal Transit                     0 (  0.0) 
##      Rider's Paradise                   13 (  0.3) 
##      Some Transit                        0 (  0.0) 
##   BikeScore (mean (SD))              87.29 (29.96) 
##   BikeScoreLabel (%)                               
##                                        558 ( 13.7) 
##      Bikeable                         1113 ( 27.2) 
##      Biker's Paradise                    0 (  0.0) 
##      Somewhat Bikeable                 264 (  6.5) 
##      Very Bikeable                    2150 ( 52.6) 
##   DiningandDrinkingScore (mean (SD)) 82.41 (18.59) 
##   GroceryScore (mean (SD))           78.71 (28.21)
```

```r
CreateTableOne(vars = vars, data = Detroit)
```

```
##                                     
##                                      Overall        
##   n                                    3077         
##   language = Fren/Span (%)                3 (  0.1) 
##   ville (%)                                         
##      Boston                               0 (  0.0) 
##      Chicago                              0 (  0.0) 
##      Détroit                           3077 (100.0) 
##      Montréal                             0 (  0.0) 
##      New-York                             0 (  0.0) 
##      Philadelphie                         0 (  0.0) 
##      Toronto                              0 (  0.0) 
##      Vancouver                            0 (  0.0) 
##   gender = Male (%)                    1134 ( 36.9) 
##   health (%)                                        
##      Excellent                         1758 ( 57.1) 
##      Good                               928 ( 30.2) 
##      NA                                   1 (  0.0) 
##      Poor                               390 ( 12.7) 
##   common_transportation (%)                         
##      Bicycle                             75 (  2.5) 
##      Car                               2718 ( 88.9) 
##      Other                               21 (  0.7) 
##      Public Transportation              105 (  3.4) 
##      Walking                            139 (  4.5) 
##   physically_active (%)                             
##      No                                 680 ( 22.1) 
##      Other                               38 (  1.2) 
##      Yes                               2355 ( 76.6) 
##   day_per_week_motor_vehicle (%)                    
##      0                                   41 (  1.3) 
##      1                                   67 (  2.2) 
##      2                                  106 (  3.4) 
##      3                                  159 (  5.2) 
##      4                                  161 (  5.2) 
##      5                                  433 ( 14.1) 
##      6                                  392 ( 12.7) 
##      7                                 1529 ( 49.7) 
##      NA                                 189 (  6.1) 
##   day_per_week_public_transit (%)                   
##      0                                 1988 ( 64.6) 
##      1                                  102 (  3.3) 
##      2                                   77 (  2.5) 
##      3                                   45 (  1.5) 
##      4                                   33 (  1.1) 
##      5                                   77 (  2.5) 
##      6                                   26 (  0.8) 
##      7                                   48 (  1.6) 
##      NA                                 681 ( 22.1) 
##   day_per_week_walking (%)                          
##      0                                  903 ( 29.3) 
##      1                                  328 ( 10.7) 
##      2                                  334 ( 10.9) 
##      3                                  238 (  7.7) 
##      4                                  165 (  5.4) 
##      5                                  294 (  9.6) 
##      6                                   81 (  2.6) 
##      7                                  238 (  7.7) 
##      NA                                 496 ( 16.1) 
##   day_per_week_bike (%)                             
##      0                                 1711 ( 55.6) 
##      1                                  191 (  6.2) 
##      2                                  135 (  4.4) 
##      3                                   77 (  2.5) 
##      4                                   47 (  1.5) 
##      5                                   54 (  1.8) 
##      6                                   22 (  0.7) 
##      7                                   26 (  0.8) 
##      NA                                 814 ( 26.5) 
##   q42 (mean (SD))                     41.11 (12.74) 
##   marital_status (%)                                
##      NA                                  24 (  0.9) 
##      Relationship/Married/Common-Law   1710 ( 62.8) 
##      Single                             988 ( 36.3) 
##   children_household (%)                            
##      0                                 1954 ( 63.5) 
##      1                                  523 ( 17.0) 
##      2-3                                516 ( 16.8) 
##      4+                                  49 (  1.6) 
##      Refuse                              35 (  1.1) 
##   ethnicity (%)                                     
##      African American                   361 ( 87.2) 
##      Arab/Indian/Jewish/Mixed/Other      38 (  9.2) 
##      Native American                     15 (  3.6) 
##   q48 (mean (SD))                      4.51 (12.93) 
##   motor_vehicle_access (%)                          
##      NA                                  18 (  0.6) 
##      No                                 123 (  4.0) 
##      Yes                               2936 ( 95.4) 
##   education (%)                                     
##      Baccalaureate                      951 ( 31.0) 
##      Cégep                              506 ( 16.5) 
##      Certificate/Diploma                444 ( 14.5) 
##      Graduate School                    736 ( 24.0) 
##      High School/Lower                  410 ( 13.4) 
##      Other                               18 (  0.6) 
##   occupation_status (%)                             
##      Employed                          2152 ( 70.4) 
##      Student                            243 (  7.9) 
##      Unemployed                         663 ( 21.7) 
##   household_income (%)                              
##      $0-$19999                          412 ( 13.4) 
##      $100000-149999                     443 ( 14.4) 
##      $150000+                           193 (  6.3) 
##      $20000-$49999                      666 ( 21.6) 
##      $50000-$74999                      536 ( 17.4) 
##      $75000-$99999                      461 ( 15.0) 
##      Refuse                             366 ( 11.9) 
##   bmi (mean (SD))                     26.75 (5.11)  
##   bmi_category (%)                                  
##      normal weight                     1073 ( 34.9) 
##      obese                              661 ( 21.5) 
##      other                              439 ( 14.3) 
##      overweight                         864 ( 28.1) 
##      underweight                         40 (  1.3) 
##   WalkScore (mean (SD))               36.05 (24.44) 
##   WalkScoreLabel (%)                                
##                                          27 (  0.9) 
##      Car-Dependent                     2094 ( 68.1) 
##      Somewhat Walkable                  733 ( 23.8) 
##      Very Walkable                      175 (  5.7) 
##      Walker's Paradise                   48 (  1.6) 
##   TransitScore (mean (SD))            79.31 (7.37)  
##   TransitScoreLabel (%)                             
##                                        3077 (100.0) 
##      Excellent Transit                    0 (  0.0) 
##      Good Transit                         0 (  0.0) 
##      Minimal Transit                      0 (  0.0) 
##      Rider's Paradise                     0 (  0.0) 
##      Some Transit                         0 (  0.0) 
##   BikeScore (mean (SD))              133.74 (18.39) 
##   BikeScoreLabel (%)                                
##                                        2898 ( 94.2) 
##      Bikeable                            90 (  2.9) 
##      Biker's Paradise                    40 (  1.3) 
##      Somewhat Bikeable                    1 (  0.0) 
##      Very Bikeable                       48 (  1.6) 
##   DiningandDrinkingScore (mean (SD))  40.56 (26.92) 
##   GroceryScore (mean (SD))            38.81 (34.49)
```

```r
CreateTableOne(vars = vars, data = NewYork)
```

```
##                                     
##                                      Overall       
##   n                                   3824         
##   language = Fren/Span (%)              16 (  0.4) 
##   ville (%)                                        
##      Boston                              0 (  0.0) 
##      Chicago                             0 (  0.0) 
##      Détroit                             0 (  0.0) 
##      Montréal                            0 (  0.0) 
##      New-York                         3824 (100.0) 
##      Philadelphie                        0 (  0.0) 
##      Toronto                             0 (  0.0) 
##      Vancouver                           0 (  0.0) 
##   gender = Male (%)                   1593 ( 41.7) 
##   health (%)                                       
##      Excellent                        2537 ( 66.3) 
##      Good                              986 ( 25.8) 
##      NA                                  6 (  0.2) 
##      Poor                              295 (  7.7) 
##   common_transportation (%)                        
##      Bicycle                           184 (  4.8) 
##      Car                               360 (  9.4) 
##      Other                              81 (  2.1) 
##      Public Transportation            2207 ( 57.9) 
##      Walking                           978 ( 25.7) 
##   physically_active (%)                            
##      No                                680 ( 17.8) 
##      Other                              51 (  1.3) 
##      Yes                              3088 ( 80.9) 
##   day_per_week_motor_vehicle (%)                   
##      0                                 968 ( 25.3) 
##      1                                 594 ( 15.5) 
##      2                                 487 ( 12.7) 
##      3                                 323 (  8.4) 
##      4                                 237 (  6.2) 
##      5                                 306 (  8.0) 
##      6                                 135 (  3.5) 
##      7                                 223 (  5.8) 
##      NA                                551 ( 14.4) 
##   day_per_week_public_transit (%)                  
##      0                                 229 (  6.0) 
##      1                                 320 (  8.4) 
##      2                                 368 (  9.6) 
##      3                                 365 (  9.5) 
##      4                                 298 (  7.8) 
##      5                                 844 ( 22.1) 
##      6                                 477 ( 12.5) 
##      7                                 588 ( 15.4) 
##      NA                                335 (  8.8) 
##   day_per_week_walking (%)                         
##      0                                  69 (  1.8) 
##      1                                 160 (  4.2) 
##      2                                 257 (  6.7) 
##      3                                 287 (  7.5) 
##      4                                 303 (  7.9) 
##      5                                 629 ( 16.4) 
##      6                                 388 ( 10.1) 
##      7                                1386 ( 36.2) 
##      NA                                345 (  9.0) 
##   day_per_week_bike (%)                            
##      0                                1851 ( 48.4) 
##      1                                 298 (  7.8) 
##      2                                 199 (  5.2) 
##      3                                 131 (  3.4) 
##      4                                  93 (  2.4) 
##      5                                 107 (  2.8) 
##      6                                  24 (  0.6) 
##      7                                  44 (  1.2) 
##      NA                               1077 ( 28.2) 
##   q42 (mean (SD))                    39.74 (13.55) 
##   marital_status (%)                               
##      NA                                 53 (  1.5) 
##      Relationship/Married/Common-Law  1557 ( 44.6) 
##      Single                           1879 ( 53.9) 
##   children_household (%)                           
##      0                                2959 ( 77.4) 
##      1                                 480 ( 12.6) 
##      2-3                               306 (  8.0) 
##      4+                                 31 (  0.8) 
##      Refuse                             48 (  1.3) 
##   ethnicity (%)                                    
##      African American                  336 ( 81.8) 
##      Arab/Indian/Jewish/Mixed/Other     66 ( 16.1) 
##      Native American                     9 (  2.2) 
##   q48 (mean (SD))                     7.41 (17.89) 
##   motor_vehicle_access (%)                         
##      NA                                 47 (  1.2) 
##      No                               2106 ( 55.1) 
##      Yes                              1671 ( 43.7) 
##   education (%)                                    
##      Baccalaureate                    1589 ( 41.6) 
##      Cégep                             466 ( 12.2) 
##      Certificate/Diploma               168 (  4.4) 
##      Graduate School                  1284 ( 33.6) 
##      High School/Lower                 291 (  7.6) 
##      Other                              22 (  0.6) 
##   occupation_status (%)                            
##      Employed                         2943 ( 77.7) 
##      Student                           270 (  7.1) 
##      Unemployed                        574 ( 15.2) 
##   household_income (%)                             
##      $0-$19999                         397 ( 10.4) 
##      $100000-149999                    631 ( 16.5) 
##      $150000+                          598 ( 15.6) 
##      $20000-$49999                     605 ( 15.8) 
##      $50000-$74999                     611 ( 16.0) 
##      $75000-$99999                     545 ( 14.3) 
##      Refuse                            437 ( 11.4) 
##   bmi (mean (SD))                    25.05 (4.52)  
##   bmi_category (%)                                 
##      normal weight                    1841 ( 48.1) 
##      obese                             490 ( 12.8) 
##      other                             450 ( 11.8) 
##      overweight                        971 ( 25.4) 
##      underweight                        72 (  1.9) 
##   WalkScore (mean (SD))              95.88 (11.04) 
##   WalkScoreLabel (%)                               
##                                         65 (  1.7) 
##      Car-Dependent                      61 (  1.6) 
##      Somewhat Walkable                  33 (  0.9) 
##      Very Walkable                     169 (  4.4) 
##      Walker's Paradise                3496 ( 91.4) 
##   TransitScore (mean (SD))           71.19 (24.19) 
##   TransitScoreLabel (%)                            
##                                       3394 ( 88.8) 
##      Excellent Transit                  34 (  0.9) 
##      Good Transit                        7 (  0.2) 
##      Minimal Transit                     0 (  0.0) 
##      Rider's Paradise                  389 ( 10.2) 
##      Some Transit                        0 (  0.0) 
##   BikeScore (mean (SD))              90.15 (24.92) 
##   BikeScoreLabel (%)                               
##                                        178 (  4.7) 
##      Bikeable                         1137 ( 29.7) 
##      Biker's Paradise                  145 (  3.8) 
##      Somewhat Bikeable                  71 (  1.9) 
##      Very Bikeable                    2293 ( 60.0) 
##   DiningandDrinkingScore (mean (SD)) 96.05 (11.20) 
##   GroceryScore (mean (SD))           97.91 (11.21)
```

```r
CreateTableOne(vars = vars, data = Philadelphia)
```

```
##                                                 
##                                                  Overall        
##   n                                                1478         
##   language = Fren/Span (%)                            3 (  0.2) 
##   ville (%)                                                     
##      Boston                                           0 (  0.0) 
##      Chicago                                          0 (  0.0) 
##      Détroit                                          0 (  0.0) 
##      Montréal                                         0 (  0.0) 
##      New-York                                         0 (  0.0) 
##      Philadelphie                                  1478 (100.0) 
##      Toronto                                          0 (  0.0) 
##      Vancouver                                        0 (  0.0) 
##   gender = Male (%)                                 536 ( 36.3) 
##   health (%)                                                    
##      Excellent                                      928 ( 62.8) 
##      Good                                           404 ( 27.3) 
##      NA                                               2 (  0.1) 
##      Poor                                           144 (  9.7) 
##   common_transportation (%)                                     
##      Bicycle                                         87 (  5.9) 
##      Car                                            606 ( 41.2) 
##      Other                                           15 (  1.0) 
##      Public Transportation                          321 ( 21.8) 
##      Walking                                        443 ( 30.1) 
##   physically_active (%)                                         
##      No                                             270 ( 18.3) 
##      Other                                           14 (  0.9) 
##      Yes                                           1193 ( 80.8) 
##   day_per_week_motor_vehicle (%)                                
##      0                                              128 (  8.7) 
##      1                                              178 ( 12.0) 
##      2                                              192 ( 13.0) 
##      3                                              128 (  8.7) 
##      4                                              105 (  7.1) 
##      5                                              209 ( 14.1) 
##      6                                              117 (  7.9) 
##      7                                              302 ( 20.4) 
##      NA                                             119 (  8.1) 
##   day_per_week_public_transit (%)                               
##      0                                              390 ( 26.4) 
##      1                                              234 ( 15.8) 
##      2                                              151 ( 10.2) 
##      3                                               99 (  6.7) 
##      4                                               85 (  5.8) 
##      5                                              222 ( 15.0) 
##      6                                               58 (  3.9) 
##      7                                               53 (  3.6) 
##      NA                                             186 ( 12.6) 
##   day_per_week_walking (%)                                      
##      0                                              101 (  6.8) 
##      1                                              100 (  6.8) 
##      2                                              157 ( 10.6) 
##      3                                              137 (  9.3) 
##      4                                               98 (  6.6) 
##      5                                              282 ( 19.1) 
##      6                                              100 (  6.8) 
##      7                                              366 ( 24.8) 
##      NA                                             137 (  9.3) 
##   day_per_week_bike (%)                                         
##      0                                              722 ( 48.8) 
##      1                                              113 (  7.6) 
##      2                                               87 (  5.9) 
##      3                                               48 (  3.2) 
##      4                                               34 (  2.3) 
##      5                                               40 (  2.7) 
##      6                                               19 (  1.3) 
##      7                                               26 (  1.8) 
##      NA                                             389 ( 26.3) 
##   q42 (mean (SD))                                 39.19 (13.51) 
##   marital_status (%)                                            
##      NA                                              13 (  1.0) 
##      Relationship/Married/Common-Law                582 ( 44.0) 
##      Single                                         729 ( 55.1) 
##   children_household (%)                                        
##      0                                             1120 ( 75.8) 
##      1                                              177 ( 12.0) 
##      2-3                                            146 (  9.9) 
##      4+                                              20 (  1.4) 
##      Refuse                                          15 (  1.0) 
##   ethnicity = Arab/Indian/Jewish/Mixed/Other (%)     28 ( 10.8) 
##   q48 (mean (SD))                                  4.50 (12.55) 
##   motor_vehicle_access (%)                                      
##      NA                                              16 (  1.1) 
##      No                                             353 ( 23.9) 
##      Yes                                           1109 ( 75.0) 
##   education (%)                                                 
##      Baccalaureate                                  509 ( 34.7) 
##      Cégep                                          206 ( 14.0) 
##      Certificate/Diploma                             86 (  5.9) 
##      Graduate School                                447 ( 30.5) 
##      High School/Lower                              210 ( 14.3) 
##      Other                                            9 (  0.6) 
##   occupation_status (%)                                         
##      Employed                                      1049 ( 71.3) 
##      Student                                        172 ( 11.7) 
##      Unemployed                                     251 ( 17.1) 
##   household_income (%)                                          
##      $0-$19999                                      227 ( 15.4) 
##      $100000-149999                                 182 ( 12.3) 
##      $150000+                                       124 (  8.4) 
##      $20000-$49999                                  335 ( 22.7) 
##      $50000-$74999                                  301 ( 20.4) 
##      $75000-$99999                                  182 ( 12.3) 
##      Refuse                                         127 (  8.6) 
##   bmi (mean (SD))                                 26.02 (4.97)  
##   bmi_category (%)                                              
##      normal weight                                  618 ( 41.8) 
##      obese                                          273 ( 18.5) 
##      other                                          164 ( 11.1) 
##      overweight                                     404 ( 27.3) 
##      underweight                                     19 (  1.3) 
##   WalkScore (mean (SD))                           86.27 (17.72) 
##   WalkScoreLabel (%)                                            
##                                                      22 (  1.5) 
##      Car-Dependent                                   83 (  5.6) 
##      Somewhat Walkable                               96 (  6.5) 
##      Very Walkable                                  244 ( 16.5) 
##      Walker's Paradise                             1033 ( 69.9) 
##   TransitScore (mean (SD))                        73.31 (21.71) 
##   TransitScoreLabel (%)                                         
##                                                    1371 ( 92.8) 
##      Excellent Transit                                0 (  0.0) 
##      Good Transit                                     2 (  0.1) 
##      Minimal Transit                                  0 (  0.0) 
##      Rider's Paradise                               105 (  7.1) 
##      Some Transit                                     0 (  0.0) 
##   BikeScore (mean (SD))                          106.32 (30.52) 
##   BikeScoreLabel (%)                                            
##                                                     148 ( 10.0) 
##      Bikeable                                       236 ( 16.0) 
##      Biker's Paradise                               594 ( 40.2) 
##      Somewhat Bikeable                               38 (  2.6) 
##      Very Bikeable                                  462 ( 31.3) 
##   DiningandDrinkingScore (mean (SD))              87.70 (16.44) 
##   GroceryScore (mean (SD))                        90.43 (22.07)
```

```r
CreateTableOne(vars = vars, data = Montreal)
```

```
##                                     
##                                      Overall       
##   n                                   2678         
##   language = Fren/Span (%)            1617 ( 60.4) 
##   ville (%)                                        
##      Boston                              0 (  0.0) 
##      Chicago                             0 (  0.0) 
##      Détroit                             0 (  0.0) 
##      Montréal                         2678 (100.0) 
##      New-York                            0 (  0.0) 
##      Philadelphie                        0 (  0.0) 
##      Toronto                             0 (  0.0) 
##      Vancouver                           0 (  0.0) 
##   gender = Male (%)                   1159 ( 43.3) 
##   health (%)                                       
##      Excellent                        1501 ( 56.0) 
##      Good                              791 ( 29.5) 
##      NA                                  3 (  0.1) 
##      Poor                              383 ( 14.3) 
##   common_transportation (%)                        
##      Bicycle                           207 (  7.8) 
##      Car                               914 ( 34.3) 
##      Other                              30 (  1.1) 
##      Public Transportation            1019 ( 38.3) 
##      Walking                           492 ( 18.5) 
##   physically_active (%)                            
##      No                                528 ( 19.7) 
##      Other                              40 (  1.5) 
##      Yes                              2107 ( 78.8) 
##   day_per_week_motor_vehicle (%)                   
##      0                                 449 ( 16.8) 
##      1                                 298 ( 11.1) 
##      2                                 357 ( 13.3) 
##      3                                 209 (  7.8) 
##      4                                 179 (  6.7) 
##      5                                 340 ( 12.7) 
##      6                                 175 (  6.5) 
##      7                                 372 ( 13.9) 
##      NA                                299 ( 11.2) 
##   day_per_week_public_transit (%)                  
##      0                                 723 ( 27.0) 
##      1                                 332 ( 12.4) 
##      2                                 250 (  9.3) 
##      3                                 194 (  7.2) 
##      4                                 155 (  5.8) 
##      5                                 430 ( 16.1) 
##      6                                 159 (  5.9) 
##      7                                 174 (  6.5) 
##      NA                                261 (  9.7) 
##   day_per_week_walking (%)                         
##      0                                 233 (  8.7) 
##      1                                 223 (  8.3) 
##      2                                 286 ( 10.7) 
##      3                                 273 ( 10.2) 
##      4                                 226 (  8.4) 
##      5                                 482 ( 18.0) 
##      6                                 170 (  6.3) 
##      7                                 509 ( 19.0) 
##      NA                                276 ( 10.3) 
##   day_per_week_bike (%)                            
##      0                                1500 ( 56.0) 
##      1                                 183 (  6.8) 
##      2                                 141 (  5.3) 
##      3                                  96 (  3.6) 
##      4                                  72 (  2.7) 
##      5                                  99 (  3.7) 
##      6                                  21 (  0.8) 
##      7                                  34 (  1.3) 
##      NA                                532 ( 19.9) 
##   q42 (mean (SD))                    47.02 (14.17) 
##   marital_status (%)                               
##      NA                                 29 (  1.3) 
##      Relationship/Married/Common-Law  1333 ( 58.8) 
##      Single                            906 ( 39.9) 
##   children_household (%)                           
##      0                                2103 ( 78.5) 
##      1                                 321 ( 12.0) 
##      2-3                               219 (  8.2) 
##      4+                                 13 (  0.5) 
##      Refuse                             22 (  0.8) 
##   ethnicity (%)                                    
##      African American                   52 ( 50.0) 
##      Arab/Indian/Jewish/Mixed/Other     44 ( 42.3) 
##      Native American                     8 (  7.7) 
##   q48 (mean (SD))                     8.11 (20.21) 
##   motor_vehicle_access (%)                         
##      NA                                 20 (  0.7) 
##      No                                837 ( 31.3) 
##      Yes                              1821 ( 68.0) 
##   education (%)                                    
##      Baccalaureate                     788 ( 29.5) 
##      Cégep                             528 ( 19.8) 
##      Certificate/Diploma               330 ( 12.4) 
##      Graduate School                   581 ( 21.8) 
##      High School/Lower                 401 ( 15.0) 
##      Other                              42 (  1.6) 
##   occupation_status (%)                            
##      Employed                         1750 ( 65.8) 
##      Student                           141 (  5.3) 
##      Unemployed                        769 ( 28.9) 
##   household_income (%)                             
##      $0-$19999                         286 ( 10.7) 
##      $100000-149999                    271 ( 10.1) 
##      $150000+                          148 (  5.5) 
##      $20000-$49999                     731 ( 27.3) 
##      $50000-$74999                     523 ( 19.5) 
##      $75000-$99999                     365 ( 13.6) 
##      Refuse                            354 ( 13.2) 
##   bmi (mean (SD))                    25.88 (4.65)  
##   bmi_category (%)                                 
##      normal weight                    1122 ( 41.9) 
##      obese                             454 ( 17.0) 
##      other                             321 ( 12.0) 
##      overweight                        746 ( 27.9) 
##      underweight                        35 (  1.3) 
##   WalkScore (mean (SD))              81.88 (16.05) 
##   WalkScoreLabel (%)                               
##                                         28 (  1.0) 
##      Car-Dependent                     135 (  5.0) 
##      Somewhat Walkable                 303 ( 11.3) 
##      Very Walkable                    1117 ( 41.7) 
##      Walker's Paradise                1095 ( 40.9) 
##   TransitScore (mean (SD))           13.38 (18.57) 
##   TransitScoreLabel (%)                            
##                                         45 (  1.7) 
##      Excellent Transit                  71 (  2.7) 
##      Good Transit                      485 ( 18.1) 
##      Minimal Transit                    43 (  1.6) 
##      Rider's Paradise                 1911 ( 71.4) 
##      Some Transit                      123 (  4.6) 
##   BikeScore (mean (SD))              86.99 (43.07) 
##   BikeScoreLabel (%)                               
##                                        624 ( 23.3) 
##      Bikeable                          719 ( 26.8) 
##      Biker's Paradise                  496 ( 18.5) 
##      Somewhat Bikeable                  83 (  3.1) 
##      Very Bikeable                     756 ( 28.2) 
##   DiningandDrinkingScore (mean (SD)) 82.34 (17.58) 
##   GroceryScore (mean (SD))           89.47 (17.42)
```

```r
CreateTableOne(vars = vars, data = Toronto)
```

```
##                                     
##                                      Overall       
##   n                                   4264         
##   language = Fren/Span (%)               3 (  0.1) 
##   ville (%)                                        
##      Boston                              0 (  0.0) 
##      Chicago                             0 (  0.0) 
##      Détroit                             0 (  0.0) 
##      Montréal                            0 (  0.0) 
##      New-York                            0 (  0.0) 
##      Philadelphie                        0 (  0.0) 
##      Toronto                          4264 (100.0) 
##      Vancouver                           0 (  0.0) 
##   gender = Male (%)                   1984 ( 46.5) 
##   health (%)                                       
##      Excellent                        2572 ( 60.3) 
##      Good                             1220 ( 28.6) 
##      NA                                  8 (  0.2) 
##      Poor                              464 ( 10.9) 
##   common_transportation (%)                        
##      Bicycle                           231 (  5.4) 
##      Car                              1665 ( 39.2) 
##      Other                              43 (  1.0) 
##      Public Transportation            1459 ( 34.3) 
##      Walking                           853 ( 20.1) 
##   physically_active (%)                            
##      No                                713 ( 16.7) 
##      Other                              52 (  1.2) 
##      Yes                              3493 ( 82.0) 
##   day_per_week_motor_vehicle (%)                   
##      0                                 470 ( 11.0) 
##      1                                 484 ( 11.4) 
##      2                                 504 ( 11.8) 
##      3                                 430 ( 10.1) 
##      4                                 311 (  7.3) 
##      5                                 571 ( 13.4) 
##      6                                 349 (  8.2) 
##      7                                 731 ( 17.1) 
##      NA                                414 (  9.7) 
##   day_per_week_public_transit (%)                  
##      0                                 857 ( 20.1) 
##      1                                 674 ( 15.8) 
##      2                                 467 ( 11.0) 
##      3                                 325 (  7.6) 
##      4                                 296 (  6.9) 
##      5                                 634 ( 14.9) 
##      6                                 245 (  5.7) 
##      7                                 223 (  5.2) 
##      NA                                543 ( 12.7) 
##   day_per_week_walking (%)                         
##      0                                 248 (  5.8) 
##      1                                 348 (  8.2) 
##      2                                 464 ( 10.9) 
##      3                                 449 ( 10.5) 
##      4                                 341 (  8.0) 
##      5                                 756 ( 17.7) 
##      6                                 328 (  7.7) 
##      7                                 911 ( 21.4) 
##      NA                                419 (  9.8) 
##   day_per_week_bike (%)                            
##      0                                1844 ( 43.2) 
##      1                                 341 (  8.0) 
##      2                                 200 (  4.7) 
##      3                                 110 (  2.6) 
##      4                                  77 (  1.8) 
##      5                                 114 (  2.7) 
##      6                                  45 (  1.1) 
##      7                                  61 (  1.4) 
##      NA                               1472 ( 34.5) 
##   q42 (mean (SD))                    46.98 (14.52) 
##   marital_status (%)                               
##      NA                                 73 (  2.0) 
##      Relationship/Married/Common-Law  2228 ( 61.1) 
##      Single                           1345 ( 36.9) 
##   children_household (%)                           
##      0                                3375 ( 79.2) 
##      1                                 454 ( 10.6) 
##      2-3                               361 (  8.5) 
##      4+                                 22 (  0.5) 
##      Refuse                             52 (  1.2) 
##   ethnicity (%)                                    
##      African American                   96 ( 35.6) 
##      Arab/Indian/Jewish/Mixed/Other    160 ( 59.3) 
##      Native American                    14 (  5.2) 
##   q48 (mean (SD))                    12.16 (24.93) 
##   motor_vehicle_access (%)                         
##      NA                                 60 (  1.4) 
##      No                               1147 ( 26.9) 
##      Yes                              3057 ( 71.7) 
##   education (%)                                    
##      Baccalaureate                    1543 ( 36.3) 
##      Cégep                             451 ( 10.6) 
##      Certificate/Diploma               746 ( 17.5) 
##      Graduate School                   995 ( 23.4) 
##      High School/Lower                 478 ( 11.2) 
##      Other                              42 (  1.0) 
##   occupation_status (%)                            
##      Employed                         3051 ( 72.2) 
##      Student                           134 (  3.2) 
##      Unemployed                       1040 ( 24.6) 
##   household_income (%)                             
##      $0-$19999                         301 (  7.1) 
##      $100000-149999                    673 ( 15.8) 
##      $150000+                          463 ( 10.9) 
##      $20000-$49999                     768 ( 18.0) 
##      $50000-$74999                     780 ( 18.3) 
##      $75000-$99999                     619 ( 14.5) 
##      Refuse                            660 ( 15.5) 
##   bmi (mean (SD))                    25.60 (4.46)  
##   bmi_category (%)                                 
##      normal weight                    1843 ( 43.2) 
##      obese                             586 ( 13.7) 
##      other                             498 ( 11.7) 
##      overweight                       1276 ( 29.9) 
##      underweight                        61 (  1.4) 
##   WalkScore (mean (SD))              79.56 (18.77) 
##   WalkScoreLabel (%)                               
##                                         26 (  0.6) 
##      Car-Dependent                     351 (  8.2) 
##      Somewhat Walkable                 700 ( 16.4) 
##      Very Walkable                    1467 ( 34.4) 
##      Walker's Paradise                1720 ( 40.3) 
##   TransitScore (mean (SD))           48.42 (25.00) 
##   TransitScoreLabel (%)                            
##                                         33 (  0.8) 
##      Excellent Transit                1547 ( 36.3) 
##      Good Transit                      939 ( 22.0) 
##      Minimal Transit                    34 (  0.8) 
##      Rider's Paradise                 1671 ( 39.2) 
##      Some Transit                       40 (  0.9) 
##   BikeScore (mean (SD))              70.90 (30.62) 
##   BikeScoreLabel (%)                               
##                                        120 (  2.8) 
##      Bikeable                         2214 ( 51.9) 
##      Biker's Paradise                  431 ( 10.1) 
##      Somewhat Bikeable                 500 ( 11.7) 
##      Very Bikeable                     999 ( 23.4) 
##   DiningandDrinkingScore (mean (SD)) 81.64 (19.10) 
##   GroceryScore (mean (SD))           82.99 (23.65)
```

```r
CreateTableOne(vars = vars, data = Vancouver)
```

```
##                                     
##                                      Overall       
##   n                                   2518         
##   language = English (%)              2518 (100.0) 
##   ville (%)                                        
##      Boston                              0 (  0.0) 
##      Chicago                             0 (  0.0) 
##      Détroit                             0 (  0.0) 
##      Montréal                            0 (  0.0) 
##      New-York                            0 (  0.0) 
##      Philadelphie                        0 (  0.0) 
##      Toronto                             0 (  0.0) 
##      Vancouver                        2518 (100.0) 
##   gender = Male (%)                   1131 ( 44.9) 
##   health (%)                                       
##      Excellent                        1404 ( 55.8) 
##      Good                              770 ( 30.6) 
##      NA                                  5 (  0.2) 
##      Poor                              339 ( 13.5) 
##   common_transportation (%)                        
##      Bicycle                           119 (  4.7) 
##      Car                              1245 ( 49.5) 
##      Other                              13 (  0.5) 
##      Public Transportation             633 ( 25.2) 
##      Walking                           503 ( 20.0) 
##   physically_active (%)                            
##      No                                372 ( 14.8) 
##      Other                              28 (  1.1) 
##      Yes                              2114 ( 84.1) 
##   day_per_week_motor_vehicle (%)                   
##      0                                 187 (  7.4) 
##      1                                 245 (  9.7) 
##      2                                 284 ( 11.3) 
##      3                                 242 (  9.6) 
##      4                                 223 (  8.9) 
##      5                                 386 ( 15.3) 
##      6                                 253 ( 10.0) 
##      7                                 509 ( 20.2) 
##      NA                                189 (  7.5) 
##   day_per_week_public_transit (%)                  
##      0                                 702 ( 27.9) 
##      1                                 413 ( 16.4) 
##      2                                 264 ( 10.5) 
##      3                                 154 (  6.1) 
##      4                                 128 (  5.1) 
##      5                                 323 ( 12.8) 
##      6                                 107 (  4.2) 
##      7                                  97 (  3.9) 
##      NA                                330 ( 13.1) 
##   day_per_week_walking (%)                         
##      0                                 163 (  6.5) 
##      1                                 195 (  7.7) 
##      2                                 248 (  9.8) 
##      3                                 269 ( 10.7) 
##      4                                 210 (  8.3) 
##      5                                 455 ( 18.1) 
##      6                                 201 (  8.0) 
##      7                                 553 ( 22.0) 
##      NA                                224 (  8.9) 
##   day_per_week_bike (%)                            
##      0                                1233 ( 49.0) 
##      1                                 185 (  7.3) 
##      2                                 124 (  4.9) 
##      3                                  87 (  3.5) 
##      4                                  31 (  1.2) 
##      5                                  53 (  2.1) 
##      6                                  20 (  0.8) 
##      7                                  24 (  1.0) 
##      NA                                761 ( 30.2) 
##   q42 (mean (SD))                    47.49 (14.51) 
##   marital_status (%)                               
##      NA                                 43 (  2.0) 
##      Relationship/Married/Common-Law  1403 ( 64.3) 
##      Single                            736 ( 33.7) 
##   children_household (%)                           
##      0                                1903 ( 75.6) 
##      1                                 305 ( 12.1) 
##      2-3                               258 ( 10.2) 
##      4+                                  5 (  0.2) 
##      Refuse                             47 (  1.9) 
##   ethnicity (%)                                    
##      African American                   19 ( 17.6) 
##      Arab/Indian/Jewish/Mixed/Other     71 ( 65.7) 
##      Native American                    18 ( 16.7) 
##   q48 (mean (SD))                    10.41 (22.52) 
##   motor_vehicle_access (%)                         
##      NA                                 23 (  0.9) 
##      No                                421 ( 16.7) 
##      Yes                              2074 ( 82.4) 
##   education (%)                                    
##      Baccalaureate                     877 ( 35.1) 
##      Cégep                             314 ( 12.6) 
##      Certificate/Diploma               427 ( 17.1) 
##      Graduate School                   518 ( 20.7) 
##      High School/Lower                 322 ( 12.9) 
##      Other                              40 (  1.6) 
##   occupation_status (%)                            
##      Employed                         1725 ( 69.4) 
##      Student                            83 (  3.3) 
##      Unemployed                        679 ( 27.3) 
##   household_income (%)                             
##      $0-$19999                         181 (  7.2) 
##      $100000-149999                    369 ( 14.7) 
##      $150000+                          204 (  8.1) 
##      $20000-$49999                     455 ( 18.1) 
##      $50000-$74999                     504 ( 20.0) 
##      $75000-$99999                     374 ( 14.9) 
##      Refuse                            431 ( 17.1) 
##   bmi (mean (SD))                    24.87 (4.27)  
##   bmi_category (%)                                 
##      normal weight                    1213 ( 48.2) 
##      obese                             262 ( 10.4) 
##      other                             239 (  9.5) 
##      overweight                        747 ( 29.7) 
##      underweight                        57 (  2.3) 
##   WalkScore (mean (SD))              78.79 (19.52) 
##   WalkScoreLabel (%)                               
##                                         27 (  1.1) 
##      Car-Dependent                     243 (  9.7) 
##      Somewhat Walkable                 403 ( 16.0) 
##      Very Walkable                     869 ( 34.5) 
##      Walker's Paradise                 976 ( 38.8) 
##   TransitScore (mean (SD))           45.29 (19.95) 
##   TransitScoreLabel (%)                            
##                                         88 (  3.5) 
##      Excellent Transit                1035 ( 41.1) 
##      Good Transit                      865 ( 34.4) 
##      Minimal Transit                     5 (  0.2) 
##      Rider's Paradise                  371 ( 14.7) 
##      Some Transit                      154 (  6.1) 
##   BikeScore (mean (SD))              92.61 (36.44) 
##   BikeScoreLabel (%)                               
##                                        512 ( 20.3) 
##      Bikeable                          745 ( 29.6) 
##      Biker's Paradise                  422 ( 16.8) 
##      Somewhat Bikeable                 179 (  7.1) 
##      Very Bikeable                     660 ( 26.2) 
##   DiningandDrinkingScore (mean (SD)) 81.17 (19.59) 
##   GroceryScore (mean (SD))           84.45 (22.22)
```

# Fix this Code - Breaks Session
ibiccs_clean %>%
            group_by(ville) %>%
              CreateTableOne(vars = vars)

language_lm <- lm(bmi ~ cbind(lang, q2), Boston)
tidy(language_lm)

# Histogram of BMI

```r
hist_bmi <- ggplot(city, aes(bmi)) + 
  geom_histogram()
plot(hist_bmi)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

```r
hist_bmi_city <- ggplot(city, aes(bmi)) + 
  geom_density(aes(group = ville, colour = ville)) +
  theme_minimal()
plot(hist_bmi_city)
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-29-2.png)<!-- -->

# Histogram of WalkScore

```r
hist_walkscore <- ggplot(city, aes(WalkScore)) + 
  geom_histogram()
plot(hist_walkscore)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

```r
hist_walkscore_city <- ggplot(city, aes(WalkScore)) + 
  geom_density(aes(group = ville, colour = ville)) +
  theme_minimal()
plot(hist_walkscore_city)
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-30-2.png)<!-- -->

# Histogram of TransitScore

```r
hist_trasnsitscore <- ggplot(city, aes(TransitScore)) + 
  geom_histogram()
plot(hist_trasnsitscore)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```r
hist_trasnsitscore_city <- ggplot(city, aes(TransitScore)) + 
  geom_density(aes(group = ville, colour = ville)) +
  theme_minimal()
plot(hist_trasnsitscore_city)
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-31-2.png)<!-- -->

# Histogram of GroceryScore

```r
hist_groceryscore <- ggplot(city, aes(GroceryScore)) + 
  geom_histogram()
plot(hist_groceryscore)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

```r
hist_groceryscore_city <- ggplot(city, aes(GroceryScore)) + 
  geom_density(aes(group = ville, colour = ville)) +
  theme_minimal()
plot(hist_groceryscore_city)
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-32-2.png)<!-- -->

# Histogram of DiningAndDrinkingScore

```r
hist_diningdrinkingscore <- ggplot(city, aes(DiningandDrinkingScore)) + 
  geom_histogram()
plot(hist_diningdrinkingscore)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

```r
hist_diningdrinkingscore_city <- ggplot(city, aes(DiningandDrinkingScore)) + 
  geom_density(aes(group = ville, colour = ville)) +
  theme_minimal()
plot(hist_diningdrinkingscore_city)
```

![](Thesis-Analysis_files/figure-html/unnamed-chunk-33-2.png)<!-- -->

# Linear Regression

```r
explanatory <- c("DiningandDrinkingScore", "WalkScore", "TransitScore")
dependent <- "bmi"
ibiccs_clean %>%
  finalfit.lm(dependent, explanatory)
```

```
##           Dependent: bmi          Mean (sd)
## 1 DiningandDrinkingScore [0,100] 25.6 (4.7)
## 3              WalkScore [0,100] 25.6 (4.7)
## 2           TransitScore  [1,80] 25.6 (4.7)
##         Coefficient (univariable)     Coefficient (multivariable)
## 1 -0.02 (-0.02 to -0.02, p<0.001) -0.02 (-0.03 to -0.01, p<0.001)
## 3 -0.02 (-0.02 to -0.02, p<0.001)   0.00 (-0.01 to 0.01, p=0.796)
## 2    0.01 (0.00 to 0.01, p<0.001)    0.00 (0.00 to 0.01, p=0.003)
```

# Linear Regression

```r
lm1 <- lm(bmi ~ WalkScore + DiningandDrinkingScore + TransitScore + GroceryScore + factor(ville), data = ibiccs_clean)
summary(lm1)
```

```
## 
## Call:
## lm(formula = bmi ~ WalkScore + DiningandDrinkingScore + TransitScore + 
##     GroceryScore + factor(ville), data = ibiccs_clean)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.949 -3.441 -0.781  2.583 15.476 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               25.83536    0.24117  107.13  < 2e-16 ***
## WalkScore                 -0.01524    0.00757   -2.01   0.0441 *  
## DiningandDrinkingScore    -0.01349    0.00580   -2.32   0.0201 *  
## TransitScore               0.00567    0.00180    3.15   0.0016 ** 
## GroceryScore               0.01399    0.00266    5.26  1.4e-07 ***
## factor(ville)Chicago       0.87269    0.13682    6.38  1.8e-10 ***
## factor(ville)Détroit       0.99810    0.17311    5.77  8.2e-09 ***
## factor(ville)Montréal      1.07539    0.18641    5.77  8.1e-09 ***
## factor(ville)New-York      0.17548    0.13876    1.26   0.2060    
## factor(ville)Philadelphie  1.01116    0.17097    5.91  3.4e-09 ***
## factor(ville)Toronto       0.64052    0.14452    4.43  9.4e-06 ***
## factor(ville)Vancouver    -0.09132    0.15988   -0.57   0.5679    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.66 on 20910 degrees of freedom
##   (2979 observations deleted due to missingness)
## Multiple R-squared:  0.0206,	Adjusted R-squared:  0.0201 
## F-statistic:   40 on 11 and 20910 DF,  p-value: <2e-16
```

```r
confint(lm1)
```

```
##                               2.5 %     97.5 %
## (Intercept)               25.362648 26.3080704
## WalkScore                 -0.030076 -0.0004043
## DiningandDrinkingScore    -0.024860 -0.0021113
## TransitScore               0.002140  0.0091972
## GroceryScore               0.008778  0.0191945
## factor(ville)Chicago       0.604508  1.1408655
## factor(ville)Détroit       0.658788  1.3374042
## factor(ville)Montréal      0.710006  1.4407736
## factor(ville)New-York     -0.096492  0.4474543
## factor(ville)Philadelphie  0.676034  1.3462768
## factor(ville)Toronto       0.357243  0.9237921
## factor(ville)Vancouver    -0.404711  0.2220625
```

```r
lm2 <- lm(bmi ~ WalkScore + DiningandDrinkingScore + TransitScore + GroceryScore + factor(ville) + language + marital_status + ethnicity + education + occupation_status + gender, data = ibiccs_clean)
summary(lm2)
```

```
## 
## Call:
## lm(formula = bmi ~ WalkScore + DiningandDrinkingScore + TransitScore + 
##     GroceryScore + factor(ville) + language + marital_status + 
##     ethnicity + education + occupation_status + gender, data = ibiccs_clean)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.769  -3.616  -0.895   3.132  14.545 
## 
## Coefficients:
##                                               Estimate Std. Error t value
## (Intercept)                                   25.49162    1.95832   13.02
## WalkScore                                     -0.01553    0.03085   -0.50
## DiningandDrinkingScore                        -0.01139    0.02331   -0.49
## TransitScore                                   0.01263    0.00873    1.45
## GroceryScore                                   0.01964    0.01110    1.77
## factor(ville)Chicago                           1.68998    0.57356    2.95
## factor(ville)Détroit                           1.84830    0.68474    2.70
## factor(ville)Montréal                          0.96031    1.11891    0.86
## factor(ville)New-York                          1.10837    0.57800    1.92
## factor(ville)Philadelphie                      2.87865    0.62840    4.58
## factor(ville)Toronto                           0.69460    0.68770    1.01
## factor(ville)Vancouver                         0.51557    0.82349    0.63
## languageFren/Span                              1.86792    1.25603    1.49
## marital_statusRelationship/Married/Common-Law -0.18054    1.62593   -0.11
## marital_statusSingle                          -0.62796    1.61938   -0.39
## ethnicityArab/Indian/Jewish/Mixed/Other       -1.62907    0.35541   -4.58
## ethnicityNative American                      -1.34373    0.74560   -1.80
## educationCégep                                 1.09240    0.40103    2.72
## educationCertificate/Diploma                   1.26244    0.43927    2.87
## educationGraduate School                       0.15574    0.35484    0.44
## educationHigh School/Lower                     0.28403    0.45086    0.63
## educationOther                                 2.24247    1.82007    1.23
## occupation_statusStudent                      -1.67497    0.45139   -3.71
## occupation_statusUnemployed                    0.45379    0.35746    1.27
## genderMale                                     0.24375    0.27718    0.88
##                                               Pr(>|t|)    
## (Intercept)                                    < 2e-16 ***
## WalkScore                                      0.61481    
## DiningandDrinkingScore                         0.62512    
## TransitScore                                   0.14838    
## GroceryScore                                   0.07704 .  
## factor(ville)Chicago                           0.00326 ** 
## factor(ville)Détroit                           0.00703 ** 
## factor(ville)Montréal                          0.39089    
## factor(ville)New-York                          0.05535 .  
## factor(ville)Philadelphie                        5e-06 ***
## factor(ville)Toronto                           0.31264    
## factor(ville)Vancouver                         0.53136    
## languageFren/Span                              0.13718    
## marital_statusRelationship/Married/Common-Law  0.91160    
## marital_statusSingle                           0.69823    
## ethnicityArab/Indian/Jewish/Mixed/Other          5e-06 ***
## ethnicityNative American                       0.07172 .  
## educationCégep                                 0.00653 ** 
## educationCertificate/Diploma                   0.00411 ** 
## educationGraduate School                       0.66079    
## educationHigh School/Lower                     0.52881    
## educationOther                                 0.21811    
## occupation_statusStudent                       0.00021 ***
## occupation_statusUnemployed                    0.20447    
## genderMale                                     0.37932    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.01 on 1489 degrees of freedom
##   (22387 observations deleted due to missingness)
## Multiple R-squared:  0.0991,	Adjusted R-squared:  0.0846 
## F-statistic: 6.82 on 24 and 1489 DF,  p-value: <2e-16
```

```r
confint(lm2)
```

```
##                                                   2.5 %   97.5 %
## (Intercept)                                   21.650270 29.33297
## WalkScore                                     -0.076033  0.04498
## DiningandDrinkingScore                        -0.057117  0.03433
## TransitScore                                  -0.004503  0.02976
## GroceryScore                                  -0.002134  0.04142
## factor(ville)Chicago                           0.564906  2.81506
## factor(ville)Détroit                           0.505154  3.19145
## factor(ville)Montréal                         -1.234501  3.15512
## factor(ville)New-York                         -0.025412  2.24215
## factor(ville)Philadelphie                      1.646008  4.11129
## factor(ville)Toronto                          -0.654368  2.04358
## factor(ville)Vancouver                        -1.099758  2.13089
## languageFren/Span                             -0.595845  4.33169
## marital_statusRelationship/Married/Common-Law -3.369899  3.00882
## marital_statusSingle                          -3.804461  2.54854
## ethnicityArab/Indian/Jewish/Mixed/Other       -2.326233 -0.93191
## ethnicityNative American                      -2.806268  0.11882
## educationCégep                                 0.305753  1.87904
## educationCertificate/Diploma                   0.400794  2.12409
## educationGraduate School                      -0.540302  0.85179
## educationHigh School/Lower                    -0.600352  1.16841
## educationOther                                -1.327694  5.81264
## occupation_statusStudent                      -2.560407 -0.78954
## occupation_statusUnemployed                   -0.247396  1.15497
## genderMale                                    -0.299949  0.78746
```

```r
table(ibiccs_clean$education)
```

```
## 
##       Baccalaureate               Cégep Certificate/Diploma 
##                8638                3197                2559 
##     Graduate School   High School/Lower               Other 
##                6655                2559                 212
```

# TO DO: LOOP REGRESSIONS
Select associations we are keeping
Read mediation package information
Figure out how you want to run model for next week
BMI & KEY variable scatterplot and regression line

## Junk Code
```
usda_zip <- dplyr::full_join(USDA, USA_ZIP, by = "CensusTract")

usda_zip_food <- usda_zip %>%
                    group_by(zip) %>%
                        summarize(
                          food = mean(foodvariable)
                        )
```

```
library(plyr)
city$zip <- as.numeric(city$RTA_zip)
USA_ZIP$CensusTract <- as.numeric(USA_ZIP$tract)
```

```
library(plyr)
city$CensusTract <- as.numeric(city$zip)
```


```
city_zip <- dplyr::left_join(city, usda_zip, by = "zip")
table(city_zip$ville)

summary(city$WalkScore)
summary(city$DiningandDrinkingScore)
summary(city$GroceryScore)
summary(city$TransitScore)
```

