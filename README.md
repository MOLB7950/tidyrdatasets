-   [Overview](#overview)
    -   [ability](#ability)
    -   [airmiles](#airmiles)
    -   [AirPassengers](#airpassengers)
    -   [airquality](#airquality)
    -   [austres](#austres)
    -   [attenu](#attenu)
    -   [attitude](#attitude)
    -   [beavers](#beavers)
    -   [BOD](#bod)
    -   [cars](#cars)
    -   [ChickWeight](#chickweight)
    -   [chickwts](#chickwts)
    -   [CO2](#co2)
    -   [DNase](#dnase)
    -   [esoph](#esoph)
    -   [faithful](#faithful)
    -   [Formaldehyde](#formaldehyde)
    -   [freeny](#freeny)
    -   [Indometh](#indometh)
    -   [InsectSprays](#insectsprays)
    -   [iris](#iris)
    -   [LifeCycleSavings](#lifecyclesavings)
    -   [Loblolly](#loblolly)
    -   [longley](#longley)
    -   [morley](#morley)
    -   [mtcars](#mtcars)
    -   [npk](#npk)
    -   [Orange](#orange)
    -   [OrchardSprays](#orchardsprays)
    -   [PlantGrowth](#plantgrowth)
    -   [pressure](#pressure)
    -   [Puromycin](#puromycin)
    -   [quakes](#quakes)
    -   [randu](#randu)
    -   [rock](#rock)
    -   [sleep](#sleep)
    -   [stackloss](#stackloss)
    -   [swiss](#swiss)
    -   [Theoph](#theoph)
    -   [ToothGrowth](#toothgrowth)
    -   [trees](#trees)
    -   [USArrests](#usarrests)
    -   [USJudgeRatings](#usjudgeratings)
    -   [USPersonalExpenditure](#uspersonalexpenditure)
    -   [VADeaths](#vadeaths)
    -   [volcano](#volcano)
    -   [warpbreaks](#warpbreaks)
    -   [women](#women)
    -   [WorldPhones](#worldphones)

Overview
========

Tidy up all of the R builtin datasets.

``` r
library(tidyr)
library(dplyr)

knitr::opts_chunk$set(comment='#>')

format_strs <- function(strings, sep='.', ...) {
  strings %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(' ', sep)
}
```

ability
-------

``` r
ability.cov_tidy <- ability.cov %>%
  as.data.frame() %>%
  select(-n.obs, -center) %>%
  mutate(test = rownames(.)) %>%
  gather(cov, value, -test) %>%
  tbl_df()

ability.cov_tidy 
```

    #> Source: local data frame [36 x 3]
    #> 
    #>       test         cov  value
    #>      (chr)       (chr)  (dbl)
    #> 1  general cov.general 24.641
    #> 2  picture cov.general  5.991
    #> 3   blocks cov.general 33.520
    #> 4     maze cov.general  6.023
    #> 5  reading cov.general 20.755
    #> 6    vocab cov.general 29.701
    #> 7  general cov.picture  5.991
    #> 8  picture cov.picture  6.700
    #> 9   blocks cov.picture 18.137
    #> 10    maze cov.picture  1.782
    #> ..     ...         ...    ...

airmiles
--------

``` r
airmiles_tidy <- airmiles %>%
  as.data.frame() %>%
  setNames('airmiles') %>%
  mutate(year = time(airmiles)) %>%
  tbl_df()

airmiles_tidy
```

    #> Source: local data frame [24 x 2]
    #> 
    #>    airmiles  year
    #>       (dbl) (dbl)
    #> 1       412  1937
    #> 2       480  1938
    #> 3       683  1939
    #> 4      1052  1940
    #> 5      1385  1941
    #> 6      1418  1942
    #> 7      1634  1943
    #> 8      2178  1944
    #> 9      3362  1945
    #> 10     5948  1946
    #> ..      ...   ...

AirPassengers
-------------

``` r
# gawd time series are awful ...
# from http://stackoverflow.com/questions/5331901/transforming-a-ts-in-a-data-frame-and-back

dmn <- list(month.abb, unique(floor(time(AirPassengers))))
AirPassengers_df <- tbl_df(data.frame(matrix(AirPassengers, 12, dimnames = dmn)))

AirPassengers_tidy <- AirPassengers_df %>%
  mutate(month = rownames(.)) %>%
  gather(year, value, -month)

AirPassengers_tidy
```

    #> Source: local data frame [144 x 3]
    #> 
    #>    month  year value
    #>    (chr) (chr) (dbl)
    #> 1      1 X1949   112
    #> 2      2 X1949   118
    #> 3      3 X1949   132
    #> 4      4 X1949   129
    #> 5      5 X1949   121
    #> 6      6 X1949   135
    #> 7      7 X1949   148
    #> 8      8 X1949   148
    #> 9      9 X1949   136
    #> 10    10 X1949   119
    #> ..   ...   ...   ...

airquality
----------

austres
-------

``` r
# Figure out how to get the 'Qtr1' ... colnames
```

attenu
------

``` r
# `attenu` already tidy

#attenu_tidy <- attenu %>%
#  mutate(obs = seq_len(n())) %>%
 # gather(varible, value, -obs) %>%
  #tbl_df
#attenu_tidy
```

attitude
--------

``` r
attitude_tidy <- attitude %>%
  mutate(dept = seq_len(n())) %>%
  gather(varible, value, -dept) %>%
  tbl_df
attitude_tidy
```

    #> Source: local data frame [210 x 3]
    #> 
    #>     dept varible value
    #>    (int)   (chr) (dbl)
    #> 1      1  rating    43
    #> 2      2  rating    63
    #> 3      3  rating    71
    #> 4      4  rating    61
    #> 5      5  rating    81
    #> 6      6  rating    43
    #> 7      7  rating    58
    #> 8      8  rating    71
    #> 9      9  rating    72
    #> 10    10  rating    67
    #> ..   ...     ...   ...

beavers
-------

``` r
# Combine the beaver1 and beaver2 datasets ...
beaver1_tidy <- beaver1 %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  mutate(beaver = '1')

beaver2_tidy <- beaver2 %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  mutate(beaver = '2')

beavers_tidy <- rbind_list(beaver1_tidy, beaver2_tidy)

beavers_tidy
```

    #> Source: local data frame [856 x 4]
    #> 
    #>      obs   key value beaver
    #>    (int) (chr) (dbl)  (chr)
    #> 1      1   day   346      1
    #> 2      2   day   346      1
    #> 3      3   day   346      1
    #> 4      4   day   346      1
    #> 5      5   day   346      1
    #> 6      6   day   346      1
    #> 7      7   day   346      1
    #> 8      8   day   346      1
    #> 9      9   day   346      1
    #> 10    10   day   346      1
    #> ..   ...   ...   ...    ...

BOD
---

``` r
BOD_tidy <- BOD %>% tbl_df %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs)

BOD_tidy
```

    #> Source: local data frame [12 x 3]
    #> 
    #>      obs    key value
    #>    (int)  (chr) (dbl)
    #> 1      1   Time   1.0
    #> 2      2   Time   2.0
    #> 3      3   Time   3.0
    #> 4      4   Time   4.0
    #> 5      5   Time   5.0
    #> 6      6   Time   7.0
    #> 7      1 demand   8.3
    #> 8      2 demand  10.3
    #> 9      3 demand  19.0
    #> 10     4 demand  16.0
    #> 11     5 demand  15.6
    #> 12     6 demand  19.8

cars
----

``` r
cars_tidy <- cars %>% tbl_df %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs)

cars_tidy
```

    #> Source: local data frame [100 x 3]
    #> 
    #>      obs   key value
    #>    (int) (chr) (dbl)
    #> 1      1 speed     4
    #> 2      2 speed     4
    #> 3      3 speed     7
    #> 4      4 speed     7
    #> 5      5 speed     8
    #> 6      6 speed     9
    #> 7      7 speed    10
    #> 8      8 speed    10
    #> 9      9 speed    10
    #> 10    10 speed    11
    #> ..   ...   ...   ...

ChickWeight
-----------

``` r
ChickWeight_tidy <- ChickWeight %>%
  gather(key, value, -Chick, convert = TRUE) %>%
  tbl_df

ChickWeight_tidy
```

    #> Source: local data frame [1,734 x 3]
    #> 
    #>     Chick    key value
    #>    (fctr)  (chr) (chr)
    #> 1       1 weight    42
    #> 2       1 weight    51
    #> 3       1 weight    59
    #> 4       1 weight    64
    #> 5       1 weight    76
    #> 6       1 weight    93
    #> 7       1 weight   106
    #> 8       1 weight   125
    #> 9       1 weight   149
    #> 10      1 weight   171
    #> ..    ...    ...   ...

chickwts
--------

``` r
# `chickwts` already tidy
```

CO2
---

``` r
# `CO2` already tidy
```

DNase
-----

``` r
# `DNase` already tidy
```

esoph
-----

``` r
# `esoph` already tidy
```

faithful
--------

``` r
# `faithful` already tidy
```

Formaldehyde
------------

``` r
# `Formaldehyde` already tidy
```

freeny
------

``` r
freeny_tidy <- freeny %>%
  tbl_df %>%
  mutate(year = seq(1962.25, 1971.75, by = 0.25)) %>%
  gather (key, value, -year)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
freeny_tidy
```

    #> Source: local data frame [195 x 3]
    #> 
    #>       year   key   value
    #>      (dbl) (chr)   (dbl)
    #> 1  1962.25     y 8.79236
    #> 2  1962.50     y 8.79137
    #> 3  1962.75     y 8.81486
    #> 4  1963.00     y 8.81301
    #> 5  1963.25     y 8.90751
    #> 6  1963.50     y 8.93673
    #> 7  1963.75     y 8.96161
    #> 8  1964.00     y 8.96044
    #> 9  1964.25     y 9.00868
    #> 10 1964.50     y 9.03049
    #> ..     ...   ...     ...

Indometh
--------

``` r
Indometh_tidy <- Indometh %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  tbl_df()
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
Indometh_tidy
```

    #> Source: local data frame [198 x 3]
    #> 
    #>      obs     key value
    #>    (int)   (chr) (chr)
    #> 1      1 Subject     1
    #> 2      2 Subject     1
    #> 3      3 Subject     1
    #> 4      4 Subject     1
    #> 5      5 Subject     1
    #> 6      6 Subject     1
    #> 7      7 Subject     1
    #> 8      8 Subject     1
    #> 9      9 Subject     1
    #> 10    10 Subject     1
    #> ..   ...     ...   ...

### infert

``` r
infert_tidy <- infert %>%
  mutate(subject = seq_len(n())) %>%
  gather(key, value, -subject) %>%
  tbl_df()
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
infert_tidy
```

    #> Source: local data frame [1,984 x 3]
    #> 
    #>    subject       key   value
    #>      (int)     (chr)   (chr)
    #> 1        1 education  0-5yrs
    #> 2        2 education  0-5yrs
    #> 3        3 education  0-5yrs
    #> 4        4 education  0-5yrs
    #> 5        5 education 6-11yrs
    #> 6        6 education 6-11yrs
    #> 7        7 education 6-11yrs
    #> 8        8 education 6-11yrs
    #> 9        9 education 6-11yrs
    #> 10      10 education 6-11yrs
    #> ..     ...       ...     ...

InsectSprays
------------

``` r
# `InsectSprays` already tidy
```

iris
----

``` r
iris_tidy <- iris %>%
  mutate(plantnum = seq_len(n())) %>%
  gather(measurement, value, -plantnum) %>%
  tbl_df()
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
iris_tidy
```

    #> Source: local data frame [750 x 3]
    #> 
    #>    plantnum  measurement value
    #>       (int)        (chr) (chr)
    #> 1         1 Sepal.Length   5.1
    #> 2         2 Sepal.Length   4.9
    #> 3         3 Sepal.Length   4.7
    #> 4         4 Sepal.Length   4.6
    #> 5         5 Sepal.Length     5
    #> 6         6 Sepal.Length   5.4
    #> 7         7 Sepal.Length   4.6
    #> 8         8 Sepal.Length     5
    #> 9         9 Sepal.Length   4.4
    #> 10       10 Sepal.Length   4.9
    #> ..      ...          ...   ...

LifeCycleSavings
----------------

``` r
# LifeCycleSavings is already tidy
```

Loblolly
--------

``` r
# Loblolly is already tidy
```

longley
-------

``` r
# longley is already tidy, unless the duplicate column will cause an issue?
```

morley
------

``` r
# morley is already tidy
```

mtcars
------

npk
---

``` r
# npk is already tidy
```

Orange
------

``` r
Orange_tidy <- Orange %>%
  gather(measurement, value, -Tree) %>%
  tbl_df()
Orange_tidy
```

    #> Source: local data frame [70 x 3]
    #> 
    #>      Tree measurement value
    #>    (fctr)       (chr) (dbl)
    #> 1       1         age   118
    #> 2       1         age   484
    #> 3       1         age   664
    #> 4       1         age  1004
    #> 5       1         age  1231
    #> 6       1         age  1372
    #> 7       1         age  1582
    #> 8       2         age   118
    #> 9       2         age   484
    #> 10      2         age   664
    #> ..    ...         ...   ...

OrchardSprays
-------------

PlantGrowth
-----------

``` r
PlantGrowth_tidy <- PlantGrowth %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  tbl_df()
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
PlantGrowth_tidy
```

    #> Source: local data frame [60 x 3]
    #> 
    #>      obs    key value
    #>    (int)  (chr) (chr)
    #> 1      1 weight  4.17
    #> 2      2 weight  5.58
    #> 3      3 weight  5.18
    #> 4      4 weight  6.11
    #> 5      5 weight   4.5
    #> 6      6 weight  4.61
    #> 7      7 weight  5.17
    #> 8      8 weight  4.53
    #> 9      9 weight  5.33
    #> 10    10 weight  5.14
    #> ..   ...    ...   ...

pressure
--------

``` r
pressure_tidy <- pressure %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  tbl_df()
pressure_tidy
```

    #> Source: local data frame [38 x 3]
    #> 
    #>      obs         key value
    #>    (int)       (chr) (dbl)
    #> 1      1 temperature     0
    #> 2      2 temperature    20
    #> 3      3 temperature    40
    #> 4      4 temperature    60
    #> 5      5 temperature    80
    #> 6      6 temperature   100
    #> 7      7 temperature   120
    #> 8      8 temperature   140
    #> 9      9 temperature   160
    #> 10    10 temperature   180
    #> ..   ...         ...   ...

Puromycin
---------

quakes
------

randu
-----

``` r
randu_tidy <- randu %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  tbl_df()
randu_tidy
```

    #> Source: local data frame [1,200 x 3]
    #> 
    #>      obs   key    value
    #>    (int) (chr)    (dbl)
    #> 1      1     x 0.000031
    #> 2      2     x 0.044495
    #> 3      3     x 0.822440
    #> 4      4     x 0.322291
    #> 5      5     x 0.393595
    #> 6      6     x 0.309097
    #> 7      7     x 0.826368
    #> 8      8     x 0.729424
    #> 9      9     x 0.317649
    #> 10    10     x 0.599793
    #> ..   ...   ...      ...

rock
----

``` r
rock_tidy <- rock %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  tbl_df()
rock_tidy
```

    #> Source: local data frame [192 x 3]
    #> 
    #>      obs   key value
    #>    (int) (chr) (dbl)
    #> 1      1  area  4990
    #> 2      2  area  7002
    #> 3      3  area  7558
    #> 4      4  area  7352
    #> 5      5  area  7943
    #> 6      6  area  7979
    #> 7      7  area  9333
    #> 8      8  area  8209
    #> 9      9  area  8393
    #> 10    10  area  6425
    #> ..   ...   ...   ...

sleep
-----

stackloss
---------

``` r
stackloss_tidy <- stackloss %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  tbl_df()
stackloss_tidy
```

    #> Source: local data frame [84 x 3]
    #> 
    #>      obs      key value
    #>    (int)    (chr) (dbl)
    #> 1      1 Air.Flow    80
    #> 2      2 Air.Flow    80
    #> 3      3 Air.Flow    75
    #> 4      4 Air.Flow    62
    #> 5      5 Air.Flow    62
    #> 6      6 Air.Flow    62
    #> 7      7 Air.Flow    62
    #> 8      8 Air.Flow    62
    #> 9      9 Air.Flow    58
    #> 10    10 Air.Flow    58
    #> ..   ...      ...   ...

swiss
-----

``` r
swiss_tidy <- swiss %>%
  mutate(region = rownames(.)) %>%
  gather(key, value, -region) %>%
  tbl_df()
swiss_tidy
```

    #> Source: local data frame [282 x 3]
    #> 
    #>          region       key value
    #>           (chr)     (chr) (dbl)
    #> 1    Courtelary Fertility  80.2
    #> 2      Delemont Fertility  83.1
    #> 3  Franches-Mnt Fertility  92.5
    #> 4       Moutier Fertility  85.8
    #> 5    Neuveville Fertility  76.9
    #> 6    Porrentruy Fertility  76.1
    #> 7         Broye Fertility  83.8
    #> 8         Glane Fertility  92.4
    #> 9       Gruyere Fertility  82.4
    #> 10       Sarine Fertility  82.9
    #> ..          ...       ...   ...

Theoph
------

ToothGrowth
-----------

trees
-----

``` r
trees_tidy <- trees %>%
  mutate(tree = seq_len(n())) %>%
  gather(key, value, -tree) %>%
  tbl_df()
trees_tidy
```

    #> Source: local data frame [93 x 3]
    #> 
    #>     tree   key value
    #>    (int) (chr) (dbl)
    #> 1      1 Girth   8.3
    #> 2      2 Girth   8.6
    #> 3      3 Girth   8.8
    #> 4      4 Girth  10.5
    #> 5      5 Girth  10.7
    #> 6      6 Girth  10.8
    #> 7      7 Girth  11.0
    #> 8      8 Girth  11.0
    #> 9      9 Girth  11.1
    #> 10    10 Girth  11.2
    #> ..   ...   ...   ...

USArrests
---------

``` r
USArrests_tidy <- USArrests %>%
  mutate(state = rownames(.)) %>%
  gather(key, value, -state) %>%
  tbl_df()
USArrests_tidy
```

    #> Source: local data frame [200 x 3]
    #> 
    #>          state    key value
    #>          (chr)  (chr) (dbl)
    #> 1      Alabama Murder  13.2
    #> 2       Alaska Murder  10.0
    #> 3      Arizona Murder   8.1
    #> 4     Arkansas Murder   8.8
    #> 5   California Murder   9.0
    #> 6     Colorado Murder   7.9
    #> 7  Connecticut Murder   3.3
    #> 8     Delaware Murder   5.9
    #> 9      Florida Murder  15.4
    #> 10     Georgia Murder  17.4
    #> ..         ...    ...   ...

USJudgeRatings
--------------

``` r
USJudgeRatings_tidy <- USJudgeRatings %>%
  mutate(judge= rownames(.)) %>%
  gather(key, value, -judge) %>%
  tbl_df()
USJudgeRatings_tidy
```

    #> Source: local data frame [516 x 3]
    #> 
    #>             judge   key value
    #>             (chr) (chr) (dbl)
    #> 1   AARONSON,L.H.  CONT   5.7
    #> 2  ALEXANDER,J.M.  CONT   6.8
    #> 3  ARMENTANO,A.J.  CONT   7.2
    #> 4     BERDON,R.I.  CONT   6.8
    #> 5    BRACKEN,J.J.  CONT   7.3
    #> 6      BURNS,E.B.  CONT   6.2
    #> 7   CALLAHAN,R.J.  CONT  10.6
    #> 8      COHEN,S.S.  CONT   7.0
    #> 9       DALY,J.J.  CONT   7.3
    #> 10   DANNEHY,J.F.  CONT   8.2
    #> ..            ...   ...   ...

USPersonalExpenditure
---------------------

``` r
USPersonalExpenditure_tidy <- USPersonalExpenditure %>%
  as.data.frame() %>%
  mutate(group = rownames(.)) %>%
  gather(year, expense, -group) %>%
  tbl_df()
USPersonalExpenditure_tidy
```

    #> Source: local data frame [25 x 3]
    #> 
    #>                  group  year expense
    #>                  (chr) (chr)   (dbl)
    #> 1     Food and Tobacco  1940  22.200
    #> 2  Household Operation  1940  10.500
    #> 3   Medical and Health  1940   3.530
    #> 4        Personal Care  1940   1.040
    #> 5    Private Education  1940   0.341
    #> 6     Food and Tobacco  1945  44.500
    #> 7  Household Operation  1945  15.500
    #> 8   Medical and Health  1945   5.760
    #> 9        Personal Care  1945   1.980
    #> 10   Private Education  1945   0.974
    #> ..                 ...   ...     ...

VADeaths
--------

``` r
VADeaths_tidy <- VADeaths %>%
  as.data.frame() %>%
  mutate(age = rownames(.)) %>%
  gather(demographic, deaths, -age) %>%
  tbl_df()
VADeaths_tidy
```

    #> Source: local data frame [20 x 3]
    #> 
    #>      age  demographic deaths
    #>    (chr)        (chr)  (dbl)
    #> 1  50-54   Rural Male   11.7
    #> 2  55-59   Rural Male   18.1
    #> 3  60-64   Rural Male   26.9
    #> 4  65-69   Rural Male   41.0
    #> 5  70-74   Rural Male   66.0
    #> 6  50-54 Rural Female    8.7
    #> 7  55-59 Rural Female   11.7
    #> 8  60-64 Rural Female   20.3
    #> 9  65-69 Rural Female   30.9
    #> 10 70-74 Rural Female   54.3
    #> 11 50-54   Urban Male   15.4
    #> 12 55-59   Urban Male   24.3
    #> 13 60-64   Urban Male   37.0
    #> 14 65-69   Urban Male   54.6
    #> 15 70-74   Urban Male   71.1
    #> 16 50-54 Urban Female    8.4
    #> 17 55-59 Urban Female   13.6
    #> 18 60-64 Urban Female   19.3
    #> 19 65-69 Urban Female   35.1
    #> 20 70-74 Urban Female   50.0

volcano
-------

warpbreaks
----------

women
-----

WorldPhones
-----------

``` r
WPs_tidy <- WorldPhones %>% 
  as.data.frame() %>%
  mutate(year = rownames(.)) %>%
  gather(continent, value, -year) %>%
  tbl_df()
WPs_tidy
```

    #> Source: local data frame [49 x 3]
    #> 
    #>     year continent value
    #>    (chr)     (chr) (dbl)
    #> 1   1951    N.Amer 45939
    #> 2   1956    N.Amer 60423
    #> 3   1957    N.Amer 64721
    #> 4   1958    N.Amer 68484
    #> 5   1959    N.Amer 71799
    #> 6   1960    N.Amer 76036
    #> 7   1961    N.Amer 79831
    #> 8   1951    Europe 21574
    #> 9   1956    Europe 29990
    #> 10  1957    Europe 32510
    #> ..   ...       ...   ...
