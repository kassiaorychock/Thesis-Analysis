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
#Talk to Dr. Fuller about organizing questions 15 and 18
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


# Filtering Out Variables in USA Data

```r
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
CreateTableOne(vars = vars, data = Boston)
```

```
##                                     
##                                      Overall        
##   n                                    1977         
##   language = Fren/Span (%)                3 (  0.2) 
##   ville (%)                                         
##      Boston                            1977 (100.0) 
##      Chicago                              0 (  0.0) 
##      Détroit                              0 (  0.0) 
##      Montréal                             0 (  0.0) 
##      New-York                             0 (  0.0) 
##      Philadelphie                         0 (  0.0) 
##      Toronto                              0 (  0.0) 
##      Vancouver                            0 (  0.0) 
##   gender = Male (%)                     694 ( 35.1) 
##   health (%)                                        
##      Excellent                         1285 ( 65.0) 
##      Good                               521 ( 26.4) 
##      NA                                   5 (  0.3) 
##      Poor                               166 (  8.4) 
##   common_transportation (%)                         
##      Bicycle                            104 (  5.3) 
##      Car                                685 ( 34.8) 
##      Other                               13 (  0.7) 
##      Public Transportation              669 ( 33.9) 
##      Walking                            500 ( 25.4) 
##   physically_active (%)                             
##      No                                 255 ( 12.9) 
##      Other                               18 (  0.9) 
##      Yes                               1703 ( 86.2) 
##   day_per_week_motor_vehicle (%)                    
##      0                                  231 ( 11.7) 
##      1                                  261 ( 13.2) 
##      2                                  222 ( 11.2) 
##      3                                  163 (  8.2) 
##      4                                  133 (  6.7) 
##      5                                  277 ( 14.0) 
##      6                                  186 (  9.4) 
##      7                                  362 ( 18.3) 
##      NA                                 142 (  7.2) 
##   day_per_week_public_transit (%)                   
##      0                                  344 ( 17.4) 
##      1                                  354 ( 17.9) 
##      2                                  210 ( 10.6) 
##      3                                  130 (  6.6) 
##      4                                  120 (  6.1) 
##      5                                  336 ( 17.0) 
##      6                                  134 (  6.8) 
##      7                                  165 (  8.3) 
##      NA                                 184 (  9.3) 
##   day_per_week_walking (%)                          
##      0                                  103 (  5.2) 
##      1                                  127 (  6.4) 
##      2                                  185 (  9.4) 
##      3                                  158 (  8.0) 
##      4                                  163 (  8.2) 
##      5                                  365 ( 18.5) 
##      6                                  141 (  7.1) 
##      7                                  609 ( 30.8) 
##      NA                                 126 (  6.4) 
##   day_per_week_bike (%)                             
##      0                                 1069 ( 54.1) 
##      1                                  141 (  7.1) 
##      2                                   70 (  3.5) 
##      3                                   47 (  2.4) 
##      4                                   39 (  2.0) 
##      5                                   69 (  3.5) 
##      6                                   25 (  1.3) 
##      7                                   32 (  1.6) 
##      NA                                 485 ( 24.5) 
##   q42 (mean (SD))                     37.21 (13.86) 
##   marital_status (%)                                
##      NA                                  23 (  1.3) 
##      Relationship/Married/Common-Law    747 ( 41.6) 
##      Single                            1025 ( 57.1) 
##   children_household (%)                            
##      0                                 1627 ( 82.3) 
##      1                                  209 ( 10.6) 
##      2-3                                111 (  5.6) 
##      4+                                  11 (  0.6) 
##      Refuse                              19 (  1.0) 
##   ethnicity (%)                                     
##      African American                   110 ( 71.0) 
##      Arab/Indian/Jewish/Mixed/Other      38 ( 24.5) 
##      Native American                      7 (  4.5) 
##   q48 (mean (SD))                      6.22 (16.05) 
##   motor_vehicle_access (%)                          
##      NA                                  16 (  0.8) 
##      No                                 519 ( 26.3) 
##      Yes                               1442 ( 72.9) 
##   education (%)                                     
##      Baccalaureate                      757 ( 38.3) 
##      Cégep                              231 ( 11.7) 
##      Certificate/Diploma                 65 (  3.3) 
##      Graduate School                    774 ( 39.2) 
##      High School/Lower                  132 (  6.7) 
##      Other                               16 (  0.8) 
##   occupation_status (%)                             
##      Employed                          1442 ( 73.5) 
##      Student                            272 ( 13.9) 
##      Unemployed                         249 ( 12.7) 
##   household_income (%)                              
##      $0-$19999                          210 ( 10.6) 
##      $100000-149999                     304 ( 15.4) 
##      $150000+                           232 ( 11.7) 
##      $20000-$49999                      389 ( 19.7) 
##      $50000-$74999                      342 ( 17.3) 
##      $75000-$99999                      275 ( 13.9) 
##      Refuse                             225 ( 11.4) 
##   bmi (mean (SD))                     25.06 (4.59)  
##   bmi_category (%)                                  
##      normal weight                      982 ( 49.7) 
##      obese                              248 ( 12.5) 
##      other                              192 (  9.7) 
##      overweight                         511 ( 25.8) 
##      underweight                         44 (  2.2) 
##   WalkScore (mean (SD))               86.19 (17.12) 
##   WalkScoreLabel (%)                                
##                                          38 (  1.9) 
##      Car-Dependent                       96 (  4.9) 
##      Somewhat Walkable                   85 (  4.3) 
##      Very Walkable                      598 ( 30.2) 
##      Walker's Paradise                 1160 ( 58.7) 
##   TransitScore (mean (SD))            73.62 (16.68) 
##   TransitScoreLabel (%)                             
##                                        1694 ( 85.7) 
##      Excellent Transit                  201 ( 10.2) 
##      Good Transit                        64 (  3.2) 
##      Minimal Transit                      0 (  0.0) 
##      Rider's Paradise                    16 (  0.8) 
##      Some Transit                         2 (  0.1) 
##   BikeScore (mean (SD))              106.50 (32.02) 
##   BikeScoreLabel (%)                                
##                                         598 ( 30.2) 
##      Bikeable                           475 ( 24.0) 
##      Biker's Paradise                   323 ( 16.3) 
##      Somewhat Bikeable                   26 (  1.3) 
##      Very Bikeable                      555 ( 28.1) 
##   DiningandDrinkingScore (mean (SD))  86.12 (17.23) 
##   GroceryScore (mean (SD))            88.94 (21.26)
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

```r
CreateTableOne(vars = vars, data = city, includeNA = TRUE)
```

```
##                                     
##                                      Overall      
##   n                                  23901        
##   language = Fren/Span (%)            1665 ( 7.0) 
##   ville (%)                                       
##      Boston                           1977 ( 8.3) 
##      Chicago                          4085 (17.1) 
##      Détroit                          3077 (12.9) 
##      Montréal                         2678 (11.2) 
##      New-York                         3824 (16.0) 
##      Philadelphie                     1478 ( 6.2) 
##      Toronto                          4264 (17.8) 
##      Vancouver                        2518 (10.5) 
##   gender = Male (%)                   9859 (41.2) 
##   health (%)                                      
##      Excellent                       14544 (60.9) 
##      Good                             6725 (28.1) 
##      NA                                 39 ( 0.2) 
##      Poor                             2593 (10.8) 
##   common_transportation (%)                       
##      Bicycle                          1177 ( 4.9) 
##      Car                              9996 (41.8) 
##      Other                             262 ( 1.1) 
##      Public Transportation            7886 (33.0) 
##      Walking                          4484 (18.8) 
##      NA                                 96 ( 0.4) 
##   physically_active (%)                           
##      No                               4124 (17.3) 
##      Other                             276 ( 1.2) 
##      Yes                             19471 (81.5) 
##      NA                                 30 ( 0.1) 
##   day_per_week_motor_vehicle (%)                  
##      0                                2785 (11.7) 
##      1                                2527 (10.6) 
##      2                                2631 (11.0) 
##      3                                2070 ( 8.7) 
##      4                                1648 ( 6.9) 
##      5                                3136 (13.1) 
##      6                                2019 ( 8.4) 
##      7                                4925 (20.6) 
##      NA                               2160 ( 9.0) 
##   day_per_week_public_transit (%)                 
##      0                                6169 (25.8) 
##      1                                2993 (12.5) 
##      2                                2167 ( 9.1) 
##      3                                1598 ( 6.7) 
##      4                                1339 ( 5.6) 
##      5                                3617 (15.1) 
##      6                                1482 ( 6.2) 
##      7                                1583 ( 6.6) 
##      NA                               2953 (12.4) 
##   day_per_week_walking (%)                        
##      0                                2115 ( 8.8) 
##      1                                1841 ( 7.7) 
##      2                                2361 ( 9.9) 
##      3                                2245 ( 9.4) 
##      4                                1831 ( 7.7) 
##      5                                3994 (16.7) 
##      6                                1711 ( 7.2) 
##      7                                5436 (22.7) 
##      NA                               2367 ( 9.9) 
##   day_per_week_bike (%)                           
##      0                               12010 (50.2) 
##      1                                1793 ( 7.5) 
##      2                                1185 ( 5.0) 
##      3                                 741 ( 3.1) 
##      4                                 462 ( 1.9) 
##      5                                 632 ( 2.6) 
##      6                                 215 ( 0.9) 
##      7                                 294 ( 1.2) 
##      NA                               6569 (27.5) 
##   q42 (mean (SD))                    42.57 (14.20)
##   marital_status (%)                              
##      NA                                291 ( 1.2) 
##      Relationship/Married/Common-Law 11366 (47.6) 
##      Single                           9452 (39.5) 
##      NA                               2792 (11.7) 
##   children_household (%)                          
##      0                               18191 (76.1) 
##      1                                2915 (12.2) 
##      2-3                              2334 ( 9.8) 
##      4+                                186 ( 0.8) 
##      Refuse                            275 ( 1.2) 
##   ethnicity (%)                                   
##      African American                 1654 ( 6.9) 
##      Arab/Indian/Jewish/Mixed/Other    501 ( 2.1) 
##      Native American                    79 ( 0.3) 
##      NA                              21667 (90.7) 
##   q48 (mean (SD))                     7.68 (18.95)
##   motor_vehicle_access (%)                        
##      NA                                230 ( 1.0) 
##      No                               6295 (26.3) 
##      Yes                             17376 (72.7) 
##   education (%)                                   
##      Baccalaureate                    8638 (36.1) 
##      Cégep                            3197 (13.4) 
##      Certificate/Diploma              2559 (10.7) 
##      Graduate School                  6655 (27.8) 
##      High School/Lower                2559 (10.7) 
##      Other                             212 ( 0.9) 
##      NA                                 81 ( 0.3) 
##   occupation_status (%)                           
##      Employed                        17272 (72.3) 
##      Student                          1586 ( 6.6) 
##      Unemployed                       4847 (20.3) 
##      NA                                196 ( 0.8) 
##   household_income (%)                            
##      $0-$19999                        2450 (10.3) 
##      $100000-149999                   3490 (14.6) 
##      $150000+                         2400 (10.0) 
##      $20000-$49999                    4761 (19.9) 
##      $50000-$74999                    4396 (18.4) 
##      $75000-$99999                    3425 (14.3) 
##      Refuse                           2979 (12.5) 
##   bmi (mean (SD))                    25.64 (4.70) 
##   bmi_category (%)                                
##      normal weight                   10437 (43.7) 
##      obese                            3672 (15.4) 
##      other                            2741 (11.5) 
##      overweight                       6658 (27.9) 
##      underweight                       393 ( 1.6) 
##   WalkScore (mean (SD))              77.81 (24.80)
##   WalkScoreLabel (%)                              
##                                        284 ( 1.2) 
##      Car-Dependent                    3337 (14.0) 
##      Somewhat Walkable                2863 (12.0) 
##      Very Walkable                    6319 (26.4) 
##      Walker's Paradise               11098 (46.4) 
##   TransitScore (mean (SD))           60.30 (28.30)
##   TransitScoreLabel (%)                           
##                                      13415 (56.1) 
##      Excellent Transit                3165 (13.2) 
##      Good Transit                     2444 (10.2) 
##      Minimal Transit                    82 ( 0.3) 
##      Rider's Paradise                 4476 (18.7) 
##      Some Transit                      319 ( 1.3) 
##   BikeScore (mean (SD))              94.10 (35.92)
##   BikeScoreLabel (%)                              
##                                       5636 (23.6) 
##      Bikeable                         6729 (28.2) 
##      Biker's Paradise                 2451 (10.3) 
##      Somewhat Bikeable                1162 ( 4.9) 
##      Very Bikeable                    7923 (33.1) 
##   DiningandDrinkingScore (mean (SD)) 79.53 (24.59)
##   GroceryScore (mean (SD))           80.76 (29.33)
```

# Fix this Code - Breaks Session
table1 <- ibiccs %>%
            group_by(ville) %>%
              CreateTableOne(vars = vars, data = ibiccs)


lang_lm <- lm(bmi ~ cbind(lang, q2), Boston1)
tidy(lang_lm)

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

To do:
2. Linear Regressions (loop)

### Linear regression




## Junk Code
```
lm1 <- lm(bmi ~ WalkScore + DiningandDrinkingScore + TransitScore + GroceryScore + factor(ville), data = ibiccs)
summary(lm1)
confint(lm1)

lm2 <- lm(bmi ~ WalkScore + DiningandDrinkingScore + + TransitScore + GroceryScore + factor(ville) + lang + q44 + q47 + q51 + Q52_occupational_status_category + q54, data = ibiccs)
summary(lm2)
confint(lm2)
```

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

