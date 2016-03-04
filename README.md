-   [Overview](#overview)
    -   [ability](#ability)
    -   [airmiles](#airmiles)
    -   [AirPassengers](#airpassengers)
    -   [airquality](#airquality)
    -   [anscombe](#anscombe)
    -   [austres](#austres)
    -   [attenu](#attenu)
    -   [attitude](#attitude)
    -   [beavers](#beavers)
    -   [BJsales](#bjsales)
    -   [BJsales.lead](#bjsales.lead)
    -   [BOD](#bod)
    -   [cars](#cars)
    -   [ChickWeight](#chickweight)
    -   [chickwts](#chickwts)
    -   [CO2](#co2)
    -   [co2](#co2-1)
    -   [crimtab](#crimtab)
    -   [discoveries](#discoveries)
    -   [DNase](#dnase)
    -   [esoph](#esoph)
    -   [euro](#euro)
    -   [euro.cross](#euro.cross)
    -   [eurodist](#eurodist)
    -   [EuStockMarkets](#eustockmarkets)
    -   [faithful](#faithful)
    -   [fdeaths](#fdeaths)
    -   [Formaldehyde](#formaldehyde)
    -   [freeny](#freeny)
    -   [freeny.x](#freeny.x)
    -   [freeny.y](#freeny.y)
    -   [HairEyeColor](#haireyecolor)
    -   [Harman23.cor](#harman23.cor)
    -   [Harman74.cor](#harman74.cor)
    -   [Indometh](#indometh)
    -   [InsectSprays](#insectsprays)
    -   [iris](#iris)
    -   [iris3](#iris3)
    -   [islands](#islands)
    -   [JohnsonJohnson](#johnsonjohnson)
    -   [LakeHuron](#lakehuron)
    -   [ldeaths](#ldeaths)
    -   [lh](#lh)
    -   [LifeCycleSavings](#lifecyclesavings)
    -   [Loblolly](#loblolly)
    -   [longley](#longley)
    -   [lynx](#lynx)
    -   [mdeaths](#mdeaths)
    -   [morley](#morley)
    -   [mtcars](#mtcars)
    -   [nhtemp](#nhtemp)
    -   [Nile](#nile)
    -   [nottem](#nottem)
    -   [npk](#npk)
    -   [occupationalStatus](#occupationalstatus)
    -   [Orange](#orange)
    -   [OrchardSprays](#orchardsprays)
    -   [PlantGrowth](#plantgrowth)
    -   [precip](#precip)
    -   [presidents](#presidents)
    -   [pressure](#pressure)
    -   [Puromycin](#puromycin)
    -   [quakes](#quakes)
    -   [randu](#randu)
    -   [rivers](#rivers)
    -   [rock](#rock)
    -   [sleep](#sleep)
    -   [stack.loss](#stack.loss)
    -   [stack.x](#stack.x)
    -   [stackloss](#stackloss)
    -   [state.abb](#state.abb)
    -   [state.area](#state.area)
    -   [state.center](#state.center)
    -   [state.division](#state.division)
    -   [state.name](#state.name)
    -   [state.region](#state.region)
    -   [state.x77](#state.x77)
    -   [states](#states)
    -   [sunspot.month](#sunspot.month)
    -   [sunspot.year](#sunspot.year)
    -   [sunspots](#sunspots)
    -   [swiss](#swiss)
    -   [Theoph](#theoph)
    -   [Titanic](#titanic)
    -   [ToothGrowth](#toothgrowth)
    -   [treering](#treering)
    -   [trees](#trees)
    -   [UCBAdmissions](#ucbadmissions)
    -   [UKDriverDeaths](#ukdriverdeaths)
    -   [UKgas](#ukgas)
    -   [USAccDeaths](#usaccdeaths)
    -   [USArrests](#usarrests)
    -   [UScitiesD](#uscitiesd)
    -   [USJudgeRatings](#usjudgeratings)
    -   [USPersonalExpenditure](#uspersonalexpenditure)
    -   [uspop](#uspop)
    -   [VADeaths](#vadeaths)
    -   [volcano](#volcano)
    -   [warpbreaks](#warpbreaks)
    -   [women](#women)
    -   [WorldPhones](#worldphones)
    -   [WWWusage](#wwwusage)

Overview
========

Tidy up all of the R builtin datasets.

``` r
library(tidyr)
library(dplyr)
library(tidytime)

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
  separate(cov, c("cov", "cat")) %>%
  select(-cov) %>%
  tbl_df()

ability.cov_tidy 
```

    #> Source: local data frame [36 x 3]
    #> 
    #>       test     cat  value
    #>      (chr)   (chr)  (dbl)
    #> 1  general general 24.641
    #> 2  picture general  5.991
    #> 3   blocks general 33.520
    #> 4     maze general  6.023
    #> 5  reading general 20.755
    #> 6    vocab general 29.701
    #> 7  general picture  5.991
    #> 8  picture picture  6.700
    #> 9   blocks picture 18.137
    #> 10    maze picture  1.782
    #> ..     ...     ...    ...

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
  gather(year, value, -month) %>%
  separate(year, c("x", "year"), sep = 1) %>%
  select(-x)

AirPassengers_tidy
```

    #> Source: local data frame [144 x 3]
    #> 
    #>    month  year value
    #>    (chr) (chr) (dbl)
    #> 1      1  1949   112
    #> 2      2  1949   118
    #> 3      3  1949   132
    #> 4      4  1949   129
    #> 5      5  1949   121
    #> 6      6  1949   135
    #> 7      7  1949   148
    #> 8      8  1949   148
    #> 9      9  1949   136
    #> 10    10  1949   119
    #> ..   ...   ...   ...

airquality
----------

``` r
# 'airquality' is already tidy
head(airquality)
```

    #>   Ozone Solar.R Wind Temp Month Day
    #> 1    41     190  7.4   67     5   1
    #> 2    36     118  8.0   72     5   2
    #> 3    12     149 12.6   74     5   3
    #> 4    18     313 11.5   62     5   4
    #> 5    NA      NA 14.3   56     5   5
    #> 6    28      NA 14.9   66     5   6

anscombe
--------

``` r
anscombe_tidy <- anscombe %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  separate(key, into = c("var", "set"), sep=1) %>%
  spread(var, value) %>%
  arrange(set, obs)

head(anscombe_tidy)
```

    #>   obs set  x    y
    #> 1   1   1 10 8.04
    #> 2   2   1  8 6.95
    #> 3   3   1 13 7.58
    #> 4   4   1  9 8.81
    #> 5   5   1 11 8.33
    #> 6   6   1 14 9.96

austres
-------

``` r
austres_tidy <- austres %>%
  tidytime() %>%
  setNames(c("year", "series","residents")) %>%
  select(-series)
austres_tidy
```

    #> Source: local data frame [89 x 2]
    #> 
    #>       year residents
    #>      (dbl)     (dbl)
    #> 1  1971.25   13067.3
    #> 2  1971.50   13130.5
    #> 3  1971.75   13198.4
    #> 4  1972.00   13254.2
    #> 5  1972.25   13303.7
    #> 6  1972.50   13353.9
    #> 7  1972.75   13409.3
    #> 8  1973.00   13459.2
    #> 9  1973.25   13504.5
    #> 10 1973.50   13552.6
    #> ..     ...       ...

attenu
------

``` r
# `attenu` already tidy
head(attenu)
```

    #>   event mag station dist accel
    #> 1     1 7.0     117   12 0.359
    #> 2     2 7.4    1083  148 0.014
    #> 3     2 7.4    1095   42 0.196
    #> 4     2 7.4     283   85 0.135
    #> 5     2 7.4     135  107 0.062
    #> 6     2 7.4     475  109 0.054

attitude
--------

``` r
# 'attitude' is already tidy
head(attitude)
```

    #>   rating complaints privileges learning raises critical advance
    #> 1     43         51         30       39     61       92      45
    #> 2     63         64         51       54     63       73      47
    #> 3     71         70         68       69     76       86      48
    #> 4     61         63         45       47     54       84      35
    #> 5     81         78         56       66     71       83      47
    #> 6     43         55         49       44     54       49      34

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

BJsales
-------

``` r
BJsales_tidy <- BJsales %>%
  tidytime() %>%
  select(-series)
BJsales_tidy
```

    #> Source: local data frame [150 x 2]
    #> 
    #>    index value
    #>    (dbl) (dbl)
    #> 1      1 200.1
    #> 2      2 199.5
    #> 3      3 199.4
    #> 4      4 198.9
    #> 5      5 199.0
    #> 6      6 200.2
    #> 7      7 198.6
    #> 8      8 200.0
    #> 9      9 200.3
    #> 10    10 201.2
    #> ..   ...   ...

BJsales.lead
------------

``` r
BJsales.lead_tidy <- BJsales.lead%>%
  tidytime() %>%
  select(-series)
BJsales.lead_tidy
```

    #> Source: local data frame [150 x 2]
    #> 
    #>    index value
    #>    (dbl) (dbl)
    #> 1      1 10.01
    #> 2      2 10.07
    #> 3      3 10.32
    #> 4      4  9.75
    #> 5      5 10.33
    #> 6      6 10.13
    #> 7      7 10.36
    #> 8      8 10.32
    #> 9      9 10.13
    #> 10    10 10.16
    #> ..   ...   ...

BOD
---

``` r
# 'BOD' is already tidy
head(BOD)
```

    #>   Time demand
    #> 1    1    8.3
    #> 2    2   10.3
    #> 3    3   19.0
    #> 4    4   16.0
    #> 5    5   15.6
    #> 6    7   19.8

cars
----

``` r
# 'cars' is already tidy
head(cars)
```

    #>   speed dist
    #> 1     4    2
    #> 2     4   10
    #> 3     7    4
    #> 4     7   22
    #> 5     8   16
    #> 6     9   10

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
head(chickwts)
```

    #>   weight      feed
    #> 1    179 horsebean
    #> 2    160 horsebean
    #> 3    136 horsebean
    #> 4    227 horsebean
    #> 5    217 horsebean
    #> 6    168 horsebean

CO2
---

``` r
# `CO2` already tidy
head(CO2)
```

    #>   Plant   Type  Treatment conc uptake
    #> 1   Qn1 Quebec nonchilled   95   16.0
    #> 2   Qn1 Quebec nonchilled  175   30.4
    #> 3   Qn1 Quebec nonchilled  250   34.8
    #> 4   Qn1 Quebec nonchilled  350   37.2
    #> 5   Qn1 Quebec nonchilled  500   35.3
    #> 6   Qn1 Quebec nonchilled  675   39.2

co2
---

``` r
co2_tidy <- co2 %>%
  tidytime() %>%
  setNames(c("year", "series", "co2")) %>%
  select(-series)
co2_tidy
```

    #> Source: local data frame [468 x 2]
    #> 
    #>        year    co2
    #>       (dbl)  (dbl)
    #> 1  1959.000 315.42
    #> 2  1959.083 316.31
    #> 3  1959.167 316.50
    #> 4  1959.250 317.56
    #> 5  1959.333 318.13
    #> 6  1959.417 318.00
    #> 7  1959.500 316.39
    #> 8  1959.583 314.65
    #> 9  1959.667 313.68
    #> 10 1959.750 313.18
    #> ..      ...    ...

crimtab
-------

``` r
crimtab_tidy <- crimtab %>%
  as.data.frame() %>%
  setNames(c("finger_len", "bod_height","count")) %>%
  tbl_df()
crimtab_tidy
```

    #> Source: local data frame [924 x 3]
    #> 
    #>    finger_len bod_height count
    #>        (fctr)     (fctr) (int)
    #> 1         9.4     142.24     0
    #> 2         9.5     142.24     0
    #> 3         9.6     142.24     0
    #> 4         9.7     142.24     0
    #> 5         9.8     142.24     0
    #> 6         9.9     142.24     0
    #> 7          10     142.24     1
    #> 8        10.1     142.24     0
    #> 9        10.2     142.24     0
    #> 10       10.3     142.24     0
    #> ..        ...        ...   ...

discoveries
-----------

``` r
discoveries_tidy <- discoveries %>%
  tidytime() %>%
  setNames(c("year", "series", "disc")) %>%
  select(-series)
discoveries_tidy
```

    #> Source: local data frame [100 x 2]
    #> 
    #>     year  disc
    #>    (dbl) (dbl)
    #> 1   1860     5
    #> 2   1861     3
    #> 3   1862     0
    #> 4   1863     2
    #> 5   1864     0
    #> 6   1865     3
    #> 7   1866     2
    #> 8   1867     3
    #> 9   1868     6
    #> 10  1869     1
    #> ..   ...   ...

DNase
-----

``` r
# `DNase` already tidy
head(DNase)
```

    #>   Run       conc density
    #> 1   1 0.04882812   0.017
    #> 2   1 0.04882812   0.018
    #> 3   1 0.19531250   0.121
    #> 4   1 0.19531250   0.124
    #> 5   1 0.39062500   0.206
    #> 6   1 0.39062500   0.215

esoph
-----

``` r
# `esoph` already tidy
head(esoph)
```

    #>   agegp     alcgp    tobgp ncases ncontrols
    #> 1 25-34 0-39g/day 0-9g/day      0        40
    #> 2 25-34 0-39g/day    10-19      0        10
    #> 3 25-34 0-39g/day    20-29      0         6
    #> 4 25-34 0-39g/day      30+      0         5
    #> 5 25-34     40-79 0-9g/day      0        27
    #> 6 25-34     40-79    10-19      0         7

euro
----

``` r
euro_tidy<- euro %>%
  data_frame(country = names(euro)) %>%
  setNames(c("rate", "country"))
head(euro_tidy)
```

    #> Source: local data frame [6 x 2]
    #> 
    #>        rate country
    #>       (dbl)   (chr)
    #> 1  13.76030     ATS
    #> 2  40.33990     BEF
    #> 3   1.95583     DEM
    #> 4 166.38600     ESP
    #> 5   5.94573     FIM
    #> 6   6.55957     FRF

euro.cross
----------

``` r
euro.cross_tidy <- euro.cross %>%
  as.data.frame() %>%
  mutate(from = row.names(.)) %>%
  gather(to, value, -from) %>%
  arrange(from, to) 
head(euro.cross_tidy)
```

    #>   from  to      value
    #> 1  ATS ATS  1.0000000
    #> 2  ATS BEF  2.9316149
    #> 3  ATS DEM  0.1421357
    #> 4  ATS ESP 12.0917422
    #> 5  ATS FIM  0.4320931
    #> 6  ATS FRF  0.4767025

eurodist
--------

``` r
#? class dist
head(eurodist)
```

    #> [1] 3313 2963 3175 3339 2762 3276

EuStockMarkets
--------------

``` r
EuStockMarkets_tidy <- EuStockMarkets %>%
  tidytime() %>%
  setNames(c("year", "country", "value"))
EuStockMarkets_tidy
```

    #> Source: local data frame [7,440 x 3]
    #> 
    #>        year country   value
    #>       (dbl)   (chr)   (dbl)
    #> 1  1991.496     DAX 1628.75
    #> 2  1991.500     DAX 1613.63
    #> 3  1991.504     DAX 1606.51
    #> 4  1991.508     DAX 1621.04
    #> 5  1991.512     DAX 1618.16
    #> 6  1991.515     DAX 1610.61
    #> 7  1991.519     DAX 1630.75
    #> 8  1991.523     DAX 1640.17
    #> 9  1991.527     DAX 1635.47
    #> 10 1991.531     DAX 1645.89
    #> ..      ...     ...     ...

faithful
--------

``` r
# `faithful` already tidy
head(faithful)
```

    #>   eruptions waiting
    #> 1     3.600      79
    #> 2     1.800      54
    #> 3     3.333      74
    #> 4     2.283      62
    #> 5     4.533      85
    #> 6     2.883      55

fdeaths
-------

``` r
fdeaths_tidy <- fdeaths %>%
  tidytime() %>%
  select(-series) %>%
  setNames(c("year", "deaths"))
fdeaths_tidy  
```

    #> Source: local data frame [72 x 2]
    #> 
    #>        year deaths
    #>       (dbl)  (dbl)
    #> 1  1974.000    901
    #> 2  1974.083    689
    #> 3  1974.167    827
    #> 4  1974.250    677
    #> 5  1974.333    522
    #> 6  1974.417    406
    #> 7  1974.500    441
    #> 8  1974.583    393
    #> 9  1974.667    387
    #> 10 1974.750    582
    #> ..      ...    ...

Formaldehyde
------------

``` r
# `Formaldehyde` already tidy
head(Formaldehyde)
```

    #>   carb optden
    #> 1  0.1  0.086
    #> 2  0.3  0.269
    #> 3  0.5  0.446
    #> 4  0.6  0.538
    #> 5  0.7  0.626
    #> 6  0.9  0.782

freeny
------

``` r
head(freeny)
```

    #>               y lag.quarterly.revenue price.index income.level
    #> 1962.25 8.79236               8.79636     4.70997      5.82110
    #> 1962.5  8.79137               8.79236     4.70217      5.82558
    #> 1962.75 8.81486               8.79137     4.68944      5.83112
    #> 1963    8.81301               8.81486     4.68558      5.84046
    #> 1963.25 8.90751               8.81301     4.64019      5.85036
    #> 1963.5  8.93673               8.90751     4.62553      5.86464
    #>         market.potential
    #> 1962.25          12.9699
    #> 1962.5           12.9733
    #> 1962.75          12.9774
    #> 1963             12.9806
    #> 1963.25          12.9831
    #> 1963.5           12.9854

freeny.x
--------

``` r
head(freeny.x)
```

    #>      lag quarterly revenue price index income level market potential
    #> [1,]               8.79636     4.70997      5.82110          12.9699
    #> [2,]               8.79236     4.70217      5.82558          12.9733
    #> [3,]               8.79137     4.68944      5.83112          12.9774
    #> [4,]               8.81486     4.68558      5.84046          12.9806
    #> [5,]               8.81301     4.64019      5.85036          12.9831
    #> [6,]               8.90751     4.62553      5.86464          12.9854

freeny.y
--------

``` r
head(freeny.y)
```

    #> [1] 8.79236 8.79137 8.81486 8.81301 8.90751 8.93673

HairEyeColor
------------

``` r
HairEyeColor_tidy <- HairEyeColor %>%
  as.data.frame() %>%
  tbl_df()
HairEyeColor_tidy
```

    #> Source: local data frame [32 x 4]
    #> 
    #>      Hair    Eye    Sex  Freq
    #>    (fctr) (fctr) (fctr) (dbl)
    #> 1   Black  Brown   Male    32
    #> 2   Brown  Brown   Male    53
    #> 3     Red  Brown   Male    10
    #> 4   Blond  Brown   Male     3
    #> 5   Black   Blue   Male    11
    #> 6   Brown   Blue   Male    50
    #> 7     Red   Blue   Male    10
    #> 8   Blond   Blue   Male    30
    #> 9   Black  Hazel   Male    10
    #> 10  Brown  Hazel   Male    25
    #> ..    ...    ...    ...   ...

Harman23.cor
------------

``` r
Harman23_tidy <- Harman23.cor %>%
  as.data.frame() %>%
  mutate(measurement = row.names(.)) %>%
  gather(compare, value, -measurement) %>%
  arrange(measurement, compare)
head(Harman23_tidy)
```

    #>   measurement            compare value
    #> 1    arm.span             center 0.000
    #> 2    arm.span       cov.arm.span 1.000
    #> 3    arm.span cov.bitro.diameter 0.326
    #> 4    arm.span    cov.chest.girth 0.277
    #> 5    arm.span    cov.chest.width 0.415
    #> 6    arm.span        cov.forearm 0.881

Harman74.cor
------------

``` r
Harman74_tidy <- Harman74.cor %>%
  as.data.frame() %>%
  mutate(measurement = row.names(.)) %>%
  gather(compare, value, -measurement) %>%
  arrange(measurement, compare)
head(Harman74_tidy)
```

    #>   measurement                compare value
    #> 1    Addition                 center 0.000
    #> 2    Addition           cov.Addition 1.000
    #> 3    Addition cov.ArithmeticProblems 0.531
    #> 4    Addition               cov.Code 0.484
    #> 5    Addition       cov.CountingDots 0.585
    #> 6    Addition              cov.Cubes 0.057

Indometh
--------

``` r
# 'Indometh' already tidy
head(Indometh)
```

    #>   Subject time conc
    #> 1       1 0.25 1.50
    #> 2       1 0.50 0.94
    #> 3       1 0.75 0.78
    #> 4       1 1.00 0.48
    #> 5       1 1.25 0.37
    #> 6       1 2.00 0.19

### infert

``` r
# 'infert' already tidy
head(infert)
```

    #>   education age parity induced case spontaneous stratum pooled.stratum
    #> 1    0-5yrs  26      6       1    1           2       1              3
    #> 2    0-5yrs  42      1       1    1           0       2              1
    #> 3    0-5yrs  39      6       2    1           0       3              4
    #> 4    0-5yrs  34      4       2    1           0       4              2
    #> 5   6-11yrs  35      3       1    1           1       5             32
    #> 6   6-11yrs  36      4       2    1           1       6             36

InsectSprays
------------

``` r
# `InsectSprays` already tidy
head(InsectSprays)
```

    #>   count spray
    #> 1    10     A
    #> 2     7     A
    #> 3    20     A
    #> 4    14     A
    #> 5    14     A
    #> 6    12     A

iris
----

``` r
# 'iris' is already tidy
head(iris)
```

    #>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    #> 1          5.1         3.5          1.4         0.2  setosa
    #> 2          4.9         3.0          1.4         0.2  setosa
    #> 3          4.7         3.2          1.3         0.2  setosa
    #> 4          4.6         3.1          1.5         0.2  setosa
    #> 5          5.0         3.6          1.4         0.2  setosa
    #> 6          5.4         3.9          1.7         0.4  setosa

iris3
-----

``` r
iris3_tidy <- iris3 %>%
  as.data.frame()
head(iris3_tidy)
```

    #>   Sepal L..Setosa Sepal W..Setosa Petal L..Setosa Petal W..Setosa
    #> 1             5.1             3.5             1.4             0.2
    #> 2             4.9             3.0             1.4             0.2
    #> 3             4.7             3.2             1.3             0.2
    #> 4             4.6             3.1             1.5             0.2
    #> 5             5.0             3.6             1.4             0.2
    #> 6             5.4             3.9             1.7             0.4
    #>   Sepal L..Versicolor Sepal W..Versicolor Petal L..Versicolor
    #> 1                 7.0                 3.2                 4.7
    #> 2                 6.4                 3.2                 4.5
    #> 3                 6.9                 3.1                 4.9
    #> 4                 5.5                 2.3                 4.0
    #> 5                 6.5                 2.8                 4.6
    #> 6                 5.7                 2.8                 4.5
    #>   Petal W..Versicolor Sepal L..Virginica Sepal W..Virginica
    #> 1                 1.4                6.3                3.3
    #> 2                 1.5                5.8                2.7
    #> 3                 1.5                7.1                3.0
    #> 4                 1.3                6.3                2.9
    #> 5                 1.5                6.5                3.0
    #> 6                 1.3                7.6                3.0
    #>   Petal L..Virginica Petal W..Virginica
    #> 1                6.0                2.5
    #> 2                5.1                1.9
    #> 3                5.9                2.1
    #> 4                5.6                1.8
    #> 5                5.8                2.2
    #> 6                6.6                2.1

islands
-------

``` r
islands_tidy <- islands %>%
  as.data.frame() %>%
  setNames("area") %>%
  mutate(island = row.names(.)) %>%
  select(2,1) %>%
  tbl_df()
islands_tidy
```

    #> Source: local data frame [48 x 2]
    #> 
    #>          island  area
    #>           (chr) (dbl)
    #> 1        Africa 11506
    #> 2    Antarctica  5500
    #> 3          Asia 16988
    #> 4     Australia  2968
    #> 5  Axel Heiberg    16
    #> 6        Baffin   184
    #> 7         Banks    23
    #> 8        Borneo   280
    #> 9       Britain    84
    #> 10      Celebes    73
    #> ..          ...   ...

JohnsonJohnson
--------------

``` r
JohnsonJohnson_tidy <- JohnsonJohnson %>%
  tidytime() %>%
  select(-series) %>%
  setNames(c("year", "sales"))
JohnsonJohnson_tidy
```

    #> Source: local data frame [84 x 2]
    #> 
    #>       year sales
    #>      (dbl) (dbl)
    #> 1  1960.00  0.71
    #> 2  1960.25  0.63
    #> 3  1960.50  0.85
    #> 4  1960.75  0.44
    #> 5  1961.00  0.61
    #> 6  1961.25  0.69
    #> 7  1961.50  0.92
    #> 8  1961.75  0.55
    #> 9  1962.00  0.72
    #> 10 1962.25  0.77
    #> ..     ...   ...

LakeHuron
---------

``` r
LakeHuron_tidy <- LakeHuron %>%
  tidytime() %>%
  setNames(c("year", "series", "level_ft")) %>%
  select(-series)
LakeHuron_tidy
```

    #> Source: local data frame [98 x 2]
    #> 
    #>     year level_ft
    #>    (dbl)    (dbl)
    #> 1   1875   580.38
    #> 2   1876   581.86
    #> 3   1877   580.97
    #> 4   1878   580.80
    #> 5   1879   579.79
    #> 6   1880   580.39
    #> 7   1881   580.42
    #> 8   1882   580.82
    #> 9   1883   581.40
    #> 10  1884   581.32
    #> ..   ...      ...

ldeaths
-------

``` r
ldeaths_tidy <- ldeaths %>%
  tidytime() %>%
  setNames(c("year", "series", "deaths"))
ldeaths_tidy
```

    #> Source: local data frame [72 x 3]
    #> 
    #>        year series deaths
    #>       (dbl)  (chr)  (dbl)
    #> 1  1974.000      x   3035
    #> 2  1974.083      x   2552
    #> 3  1974.167      x   2704
    #> 4  1974.250      x   2554
    #> 5  1974.333      x   2014
    #> 6  1974.417      x   1655
    #> 7  1974.500      x   1721
    #> 8  1974.583      x   1524
    #> 9  1974.667      x   1596
    #> 10 1974.750      x   2074
    #> ..      ...    ...    ...

lh
--

``` r
lh_tidy <- lh %>%
  tidytime() %>%
  setNames(c("time_min", "series", "lh")) %>%
  select(-series)
lh_tidy
```

    #> Source: local data frame [48 x 2]
    #> 
    #>    time_min    lh
    #>       (dbl) (dbl)
    #> 1         1   2.4
    #> 2         2   2.4
    #> 3         3   2.4
    #> 4         4   2.2
    #> 5         5   2.1
    #> 6         6   1.5
    #> 7         7   2.3
    #> 8         8   2.3
    #> 9         9   2.5
    #> 10       10   2.0
    #> ..      ...   ...

LifeCycleSavings
----------------

``` r
LifeCycleSavings_tidy <- LifeCycleSavings %>%
  mutate(country = row.names(.)) %>%
  select(6,1,2,3,4,5) %>%
  tbl_df()
LifeCycleSavings_tidy
```

    #> Source: local data frame [50 x 6]
    #> 
    #>       country    sr pop15 pop75     dpi  ddpi
    #>         (chr) (dbl) (dbl) (dbl)   (dbl) (dbl)
    #> 1   Australia 11.43 29.35  2.87 2329.68  2.87
    #> 2     Austria 12.07 23.32  4.41 1507.99  3.93
    #> 3     Belgium 13.17 23.80  4.43 2108.47  3.82
    #> 4     Bolivia  5.75 41.89  1.67  189.13  0.22
    #> 5      Brazil 12.88 42.19  0.83  728.47  4.56
    #> 6      Canada  8.79 31.72  2.85 2982.88  2.43
    #> 7       Chile  0.60 39.74  1.34  662.86  2.67
    #> 8       China 11.90 44.75  0.67  289.52  6.51
    #> 9    Colombia  4.98 46.64  1.06  276.65  3.08
    #> 10 Costa Rica 10.78 47.64  1.14  471.24  2.80
    #> ..        ...   ...   ...   ...     ...   ...

Loblolly
--------

``` r
Loblolly_tidy <- Loblolly %>%
  mutate(tree_num = row.names(.)) %>%
  select(4,1,2,3) %>%
  tbl_df()
Loblolly_tidy
```

    #> Source: local data frame [84 x 4]
    #> 
    #>    tree_num height   age   Seed
    #>       (chr)  (dbl) (dbl) (fctr)
    #> 1         1   4.51     3    301
    #> 2        15  10.89     5    301
    #> 3        29  28.72    10    301
    #> 4        43  41.74    15    301
    #> 5        57  52.70    20    301
    #> 6        71  60.92    25    301
    #> 7         2   4.55     3    303
    #> 8        16  10.92     5    303
    #> 9        30  29.07    10    303
    #> 10       44  42.83    15    303
    #> ..      ...    ...   ...    ...

longley
-------

``` r
# longley is already tidy
head(longley)
```

    #>      GNP.deflator     GNP Unemployed Armed.Forces Population Year Employed
    #> 1947         83.0 234.289      235.6        159.0    107.608 1947   60.323
    #> 1948         88.5 259.426      232.5        145.6    108.632 1948   61.122
    #> 1949         88.2 258.054      368.2        161.6    109.773 1949   60.171
    #> 1950         89.5 284.599      335.1        165.0    110.929 1950   61.187
    #> 1951         96.2 328.975      209.9        309.9    112.075 1951   63.221
    #> 1952         98.1 346.999      193.2        359.4    113.270 1952   63.639

lynx
----

``` r
lynx_tidy <- lynx %>%
  tidytime() %>%
  setNames(c("year", "series", "trappings")) %>%
  select(-series)
lynx_tidy
```

    #> Source: local data frame [114 x 2]
    #> 
    #>     year trappings
    #>    (dbl)     (dbl)
    #> 1   1821       269
    #> 2   1822       321
    #> 3   1823       585
    #> 4   1824       871
    #> 5   1825      1475
    #> 6   1826      2821
    #> 7   1827      3928
    #> 8   1828      5943
    #> 9   1829      4950
    #> 10  1830      2577
    #> ..   ...       ...

mdeaths
-------

``` r
mdeaths_tidy <- mdeaths %>%
  tidytime() %>%
  setNames(c("year", "series", "deaths")) %>%
  select(-series)
mdeaths_tidy
```

    #> Source: local data frame [72 x 2]
    #> 
    #>        year deaths
    #>       (dbl)  (dbl)
    #> 1  1974.000   2134
    #> 2  1974.083   1863
    #> 3  1974.167   1877
    #> 4  1974.250   1877
    #> 5  1974.333   1492
    #> 6  1974.417   1249
    #> 7  1974.500   1280
    #> 8  1974.583   1131
    #> 9  1974.667   1209
    #> 10 1974.750   1492
    #> ..      ...    ...

morley
------

``` r
# morley is already tidy
head(morley)
```

    #>     Expt Run Speed
    #> 001    1   1   850
    #> 002    1   2   740
    #> 003    1   3   900
    #> 004    1   4  1070
    #> 005    1   5   930
    #> 006    1   6   850

mtcars
------

``` r
mtcars_tidy <- mtcars %>%
  mutate(car = row.names(.)) %>%
  select(12,1:11) %>%
  tbl_df()
mtcars_tidy
```

    #> Source: local data frame [32 x 12]
    #> 
    #>                  car   mpg   cyl  disp    hp  drat    wt  qsec    vs    am
    #>                (chr) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl)
    #> 1          Mazda RX4  21.0     6 160.0   110  3.90 2.620 16.46     0     1
    #> 2      Mazda RX4 Wag  21.0     6 160.0   110  3.90 2.875 17.02     0     1
    #> 3         Datsun 710  22.8     4 108.0    93  3.85 2.320 18.61     1     1
    #> 4     Hornet 4 Drive  21.4     6 258.0   110  3.08 3.215 19.44     1     0
    #> 5  Hornet Sportabout  18.7     8 360.0   175  3.15 3.440 17.02     0     0
    #> 6            Valiant  18.1     6 225.0   105  2.76 3.460 20.22     1     0
    #> 7         Duster 360  14.3     8 360.0   245  3.21 3.570 15.84     0     0
    #> 8          Merc 240D  24.4     4 146.7    62  3.69 3.190 20.00     1     0
    #> 9           Merc 230  22.8     4 140.8    95  3.92 3.150 22.90     1     0
    #> 10          Merc 280  19.2     6 167.6   123  3.92 3.440 18.30     1     0
    #> ..               ...   ...   ...   ...   ...   ...   ...   ...   ...   ...
    #> Variables not shown: gear (dbl), carb (dbl)

nhtemp
------

``` r
nhtemp_tidy <- nhtemp %>%
  tidytime() %>%
  setNames(c("year", "series", "temp_F")) %>%
  select(-series)
nhtemp_tidy
```

    #> Source: local data frame [60 x 2]
    #> 
    #>     year temp_F
    #>    (dbl)  (dbl)
    #> 1   1912   49.9
    #> 2   1913   52.3
    #> 3   1914   49.4
    #> 4   1915   51.1
    #> 5   1916   49.4
    #> 6   1917   47.9
    #> 7   1918   49.8
    #> 8   1919   50.9
    #> 9   1920   49.3
    #> 10  1921   51.9
    #> ..   ...    ...

Nile
----

``` r
Nile_tidy <- Nile %>%
  tidytime() %>%
  setNames(c("year", "series", "flow")) %>%
  select(-series)
Nile_tidy
```

    #> Source: local data frame [100 x 2]
    #> 
    #>     year  flow
    #>    (dbl) (dbl)
    #> 1   1871  1120
    #> 2   1872  1160
    #> 3   1873   963
    #> 4   1874  1210
    #> 5   1875  1160
    #> 6   1876  1160
    #> 7   1877   813
    #> 8   1878  1230
    #> 9   1879  1370
    #> 10  1880  1140
    #> ..   ...   ...

nottem
------

``` r
nottem_tidy <- nottem %>%
  tidytime() %>%
  setNames(c("year", "series", "temp_F")) %>%
  select(-series)
nottem_tidy
```

    #> Source: local data frame [240 x 2]
    #> 
    #>        year temp_F
    #>       (dbl)  (dbl)
    #> 1  1920.000   40.6
    #> 2  1920.083   40.8
    #> 3  1920.167   44.4
    #> 4  1920.250   46.7
    #> 5  1920.333   54.1
    #> 6  1920.417   58.5
    #> 7  1920.500   57.7
    #> 8  1920.583   56.4
    #> 9  1920.667   54.3
    #> 10 1920.750   50.5
    #> ..      ...    ...

npk
---

``` r
# npk is already tidy
head(npk)
```

    #>   block N P K yield
    #> 1     1 0 1 1  49.5
    #> 2     1 1 1 0  62.8
    #> 3     1 0 0 0  46.8
    #> 4     1 1 0 1  57.0
    #> 5     2 1 0 0  59.8
    #> 6     2 1 1 1  58.5

occupationalStatus
------------------

``` r
#?
head(occupationalStatus)
```

    #>       destination
    #> origin  1  2   3   4  5   6   7   8
    #>      1 50 19  26   8  7  11   6   2
    #>      2 16 40  34  18 11  20   8   3
    #>      3 12 35  65  66 35  88  23  21
    #>      4 11 20  58 110 40 183  64  32
    #>      5  2  8  12  23 25  46  28  12
    #>      6 12 28 102 162 90 554 230 177

Orange
------

``` r
# 'Orange' already tidy
head(Orange)
```

    #>   Tree  age circumference
    #> 1    1  118            30
    #> 2    1  484            58
    #> 3    1  664            87
    #> 4    1 1004           115
    #> 5    1 1231           120
    #> 6    1 1372           142

OrchardSprays
-------------

``` r
# 'OrchardSprays' alreayd tidy
head(OrchardSprays)
```

    #>   decrease rowpos colpos treatment
    #> 1       57      1      1         D
    #> 2       95      2      1         E
    #> 3        8      3      1         B
    #> 4       69      4      1         H
    #> 5       92      5      1         G
    #> 6       90      6      1         F

PlantGrowth
-----------

``` r
# 'PlantGrowth' already tidy
head(PlantGrowth)
```

    #>   weight group
    #> 1   4.17  ctrl
    #> 2   5.58  ctrl
    #> 3   5.18  ctrl
    #> 4   6.11  ctrl
    #> 5   4.50  ctrl
    #> 6   4.61  ctrl

precip
------

``` r
precip_tidy <- precip %>%
  data.frame("city" = names(precip), "precip" = as.numeric(precip)) %>%
  select(city, precip) %>%
  arrange(city)
head(precip_tidy)
```

    #>            city precip
    #> 1        Albany   33.4
    #> 2   Albuquerque    7.8
    #> 3       Atlanta   48.3
    #> 4 Atlantic City   45.5
    #> 5     Baltimore   41.8
    #> 6       Bismark   16.2

presidents
----------

``` r
presidents_tidy <- presidents %>%
  tidytime() %>%
  setNames(c("year", "series", "rating")) %>%
  select(-series)
presidents_tidy
```

    #> Source: local data frame [120 x 2]
    #> 
    #>       year rating
    #>      (dbl)  (dbl)
    #> 1  1945.00     NA
    #> 2  1945.25     87
    #> 3  1945.50     82
    #> 4  1945.75     75
    #> 5  1946.00     63
    #> 6  1946.25     50
    #> 7  1946.50     43
    #> 8  1946.75     32
    #> 9  1947.00     35
    #> 10 1947.25     60
    #> ..     ...    ...

pressure
--------

``` r
# 'pressure' is already tidy
head(pressure)
```

    #>   temperature pressure
    #> 1           0   0.0002
    #> 2          20   0.0012
    #> 3          40   0.0060
    #> 4          60   0.0300
    #> 5          80   0.0900
    #> 6         100   0.2700

Puromycin
---------

``` r
# 'Puromycin' is already tidy
head(Puromycin)
```

    #>   conc rate   state
    #> 1 0.02   76 treated
    #> 2 0.02   47 treated
    #> 3 0.06   97 treated
    #> 4 0.06  107 treated
    #> 5 0.11  123 treated
    #> 6 0.11  139 treated

quakes
------

``` r
# 'quakes' is already tidy
head(quakes)
```

    #>      lat   long depth mag stations
    #> 1 -20.42 181.62   562 4.8       41
    #> 2 -20.62 181.03   650 4.2       15
    #> 3 -26.00 184.10    42 5.4       43
    #> 4 -17.97 181.66   626 4.1       19
    #> 5 -20.42 181.96   649 4.0       11
    #> 6 -19.68 184.31   195 4.0       12

randu
-----

``` r
# 'randu' is already tidy
head(randu)
```

    #>          x        y        z
    #> 1 0.000031 0.000183 0.000824
    #> 2 0.044495 0.155732 0.533939
    #> 3 0.822440 0.873416 0.838542
    #> 4 0.322291 0.648545 0.990648
    #> 5 0.393595 0.826873 0.418881
    #> 6 0.309097 0.926590 0.777664

rivers
------

``` r
rivers_tidy <- rivers %>%
  as.data.frame() %>%
  setNames("length_mi") %>%
  mutate(river_num = seq_len(n())) %>%
  select(2,1) %>%
  tbl_df()
rivers_tidy
```

    #> Source: local data frame [141 x 2]
    #> 
    #>    river_num length_mi
    #>        (int)     (dbl)
    #> 1          1       735
    #> 2          2       320
    #> 3          3       325
    #> 4          4       392
    #> 5          5       524
    #> 6          6       450
    #> 7          7      1459
    #> 8          8       135
    #> 9          9       465
    #> 10        10       600
    #> ..       ...       ...

rock
----

``` r
rock_tidy <- rock %>%
  mutate(samp_num = seq_len(n())) %>%
  select(5,1:4) %>%
  tbl_df()
rock_tidy
```

    #> Source: local data frame [48 x 5]
    #> 
    #>    samp_num  area    peri     shape  perm
    #>       (int) (int)   (dbl)     (dbl) (dbl)
    #> 1         1  4990 2791.90 0.0903296   6.3
    #> 2         2  7002 3892.60 0.1486220   6.3
    #> 3         3  7558 3930.66 0.1833120   6.3
    #> 4         4  7352 3869.32 0.1170630   6.3
    #> 5         5  7943 3948.54 0.1224170  17.1
    #> 6         6  7979 4010.15 0.1670450  17.1
    #> 7         7  9333 4345.75 0.1896510  17.1
    #> 8         8  8209 4344.75 0.1641270  17.1
    #> 9         9  8393 3682.04 0.2036540 119.0
    #> 10       10  6425 3098.65 0.1623940 119.0
    #> ..      ...   ...     ...       ...   ...

sleep
-----

``` r
# 'sleep' is already tidy
head(sleep)
```

    #>   extra group ID
    #> 1   0.7     1  1
    #> 2  -1.6     1  2
    #> 3  -0.2     1  3
    #> 4  -1.2     1  4
    #> 5  -0.1     1  5
    #> 6   3.4     1  6

stack.loss
----------

``` r
head(stack.loss)
```

    #> [1] 42 37 37 28 18 18

stack.x
-------

``` r
head(stack.x)
```

    #>      Air.Flow Water.Temp Acid.Conc.
    #> [1,]       80         27         89
    #> [2,]       80         27         88
    #> [3,]       75         25         90
    #> [4,]       62         24         87
    #> [5,]       62         22         87
    #> [6,]       62         23         87

stackloss
---------

``` r
# 'stackloss' already tidy
head(stackloss)
```

    #>   Air.Flow Water.Temp Acid.Conc. stack.loss
    #> 1       80         27         89         42
    #> 2       80         27         88         37
    #> 3       75         25         90         37
    #> 4       62         24         87         28
    #> 5       62         22         87         18
    #> 6       62         23         87         18

state.abb
---------

``` r
# see states
```

state.area
----------

``` r
# see states
```

state.center
------------

``` r
# see states
```

state.division
--------------

``` r
# see states
```

state.name
----------

``` r
# see states
```

state.region
------------

``` r
# see states
```

state.x77
---------

``` r
# see states
```

states
------

``` r
state.abb_tidy <- as.data.frame(state.abb) 
state.area_tidy <- as.data.frame(state.area)
state.center_tidy <- as.data.frame(state.center)
state.division_tidy <- as.data.frame(state.division)
state.name_tidy <- as.data.frame(state.name)
state.region_tidy <- as.data.frame(state.region)
state.x77_tidy <- state.x77 %>%
  as.data.frame() %>%
  mutate(state = row.names(.)) %>%
  select(9,1:8)

states_tidy <- cbind(state.abb_tidy, state.area_tidy, state.center_tidy, state.division_tidy, state.region_tidy, state.x77_tidy) %>%
  tbl_df()
states_tidy
```

    #> Source: local data frame [50 x 15]
    #> 
    #>    state.abb state.area         x       y     state.division state.region
    #>       (fctr)      (dbl)     (dbl)   (dbl)             (fctr)       (fctr)
    #> 1         AL      51609  -86.7509 32.5901 East South Central        South
    #> 2         AK     589757 -127.2500 49.2500            Pacific         West
    #> 3         AZ     113909 -111.6250 34.2192           Mountain         West
    #> 4         AR      53104  -92.2992 34.7336 West South Central        South
    #> 5         CA     158693 -119.7730 36.5341            Pacific         West
    #> 6         CO     104247 -105.5130 38.6777           Mountain         West
    #> 7         CT       5009  -72.3573 41.5928        New England    Northeast
    #> 8         DE       2057  -74.9841 38.6777     South Atlantic        South
    #> 9         FL      58560  -81.6850 27.8744     South Atlantic        South
    #> 10        GA      58876  -83.3736 32.3329     South Atlantic        South
    #> ..       ...        ...       ...     ...                ...          ...
    #> Variables not shown: state (chr), Population (dbl), Income (dbl),
    #>   Illiteracy (dbl), Life Exp (dbl), Murder (dbl), HS Grad (dbl), Frost
    #>   (dbl), Area (dbl)

sunspot.month
-------------

``` r
sunspot.month_tidy <- sunspot.month %>%
  tidytime() %>%
  setNames(c("year", "series", "sunspots")) %>%
  select(-series)
sunspot.month_tidy
```

    #> Source: local data frame [3,177 x 2]
    #> 
    #>        year sunspots
    #>       (dbl)    (dbl)
    #> 1  1749.000     58.0
    #> 2  1749.083     62.6
    #> 3  1749.167     70.0
    #> 4  1749.250     55.7
    #> 5  1749.333     85.0
    #> 6  1749.417     83.5
    #> 7  1749.500     94.8
    #> 8  1749.583     66.3
    #> 9  1749.667     75.9
    #> 10 1749.750     75.5
    #> ..      ...      ...

sunspot.year
------------

``` r
sunspot.year_tidy <- sunspot.year %>%
  tidytime() %>%
  setNames(c("year", "series", "sunspots")) %>%
  select(-series)
sunspot.year_tidy
```

    #> Source: local data frame [289 x 2]
    #> 
    #>     year sunspots
    #>    (dbl)    (dbl)
    #> 1   1700        5
    #> 2   1701       11
    #> 3   1702       16
    #> 4   1703       23
    #> 5   1704       36
    #> 6   1705       58
    #> 7   1706       29
    #> 8   1707       20
    #> 9   1708       10
    #> 10  1709        8
    #> ..   ...      ...

sunspots
--------

``` r
sunspots_tidy <- sunspots %>%
  tidytime() %>%
  setNames(c("year", "series", "sunspots")) %>%
  select(-series)
sunspots_tidy
```

    #> Source: local data frame [2,820 x 2]
    #> 
    #>        year sunspots
    #>       (dbl)    (dbl)
    #> 1  1749.000     58.0
    #> 2  1749.083     62.6
    #> 3  1749.167     70.0
    #> 4  1749.250     55.7
    #> 5  1749.333     85.0
    #> 6  1749.417     83.5
    #> 7  1749.500     94.8
    #> 8  1749.583     66.3
    #> 9  1749.667     75.9
    #> 10 1749.750     75.5
    #> ..      ...      ...

swiss
-----

``` r
swiss_tidy <- swiss %>%
  mutate(region = rownames(.)) %>%
  select(7,1:6) %>%
  tbl_df()
swiss_tidy
```

    #> Source: local data frame [47 x 7]
    #> 
    #>          region Fertility Agriculture Examination Education Catholic
    #>           (chr)     (dbl)       (dbl)       (int)     (int)    (dbl)
    #> 1    Courtelary      80.2        17.0          15        12     9.96
    #> 2      Delemont      83.1        45.1           6         9    84.84
    #> 3  Franches-Mnt      92.5        39.7           5         5    93.40
    #> 4       Moutier      85.8        36.5          12         7    33.77
    #> 5    Neuveville      76.9        43.5          17        15     5.16
    #> 6    Porrentruy      76.1        35.3           9         7    90.57
    #> 7         Broye      83.8        70.2          16         7    92.85
    #> 8         Glane      92.4        67.8          14         8    97.16
    #> 9       Gruyere      82.4        53.3          12         7    97.67
    #> 10       Sarine      82.9        45.2          16        13    91.38
    #> ..          ...       ...         ...         ...       ...      ...
    #> Variables not shown: Infant.Mortality (dbl)

Theoph
------

``` r
# 'Theoph' is already tidy
head(Theoph)
```

    #>   Subject   Wt Dose Time  conc
    #> 1       1 79.6 4.02 0.00  0.74
    #> 2       1 79.6 4.02 0.25  2.84
    #> 3       1 79.6 4.02 0.57  6.57
    #> 4       1 79.6 4.02 1.12 10.50
    #> 5       1 79.6 4.02 2.02  9.66
    #> 6       1 79.6 4.02 3.82  8.58

Titanic
-------

``` r
Titanic_tidy <- Titanic %>%
  as.data.frame() %>%
  tbl_df()
Titanic_tidy
```

    #> Source: local data frame [32 x 5]
    #> 
    #>     Class    Sex    Age Survived  Freq
    #>    (fctr) (fctr) (fctr)   (fctr) (dbl)
    #> 1     1st   Male  Child       No     0
    #> 2     2nd   Male  Child       No     0
    #> 3     3rd   Male  Child       No    35
    #> 4    Crew   Male  Child       No     0
    #> 5     1st Female  Child       No     0
    #> 6     2nd Female  Child       No     0
    #> 7     3rd Female  Child       No    17
    #> 8    Crew Female  Child       No     0
    #> 9     1st   Male  Adult       No   118
    #> 10    2nd   Male  Adult       No   154
    #> ..    ...    ...    ...      ...   ...

ToothGrowth
-----------

``` r
# 'ToothGrowth' is already tidy
head(ToothGrowth)
```

    #>    len supp dose
    #> 1  4.2   VC  0.5
    #> 2 11.5   VC  0.5
    #> 3  7.3   VC  0.5
    #> 4  5.8   VC  0.5
    #> 5  6.4   VC  0.5
    #> 6 10.0   VC  0.5

treering
--------

``` r
treering_tidy <- treering %>%
  tidytime() %>%
  setNames(c("year", "series", "width")) %>%
  select(-series)
treering_tidy
```

    #> Source: local data frame [7,980 x 2]
    #> 
    #>     year width
    #>    (dbl) (dbl)
    #> 1  -6000 1.345
    #> 2  -5999 1.077
    #> 3  -5998 1.545
    #> 4  -5997 1.319
    #> 5  -5996 1.413
    #> 6  -5995 1.069
    #> 7  -5994 0.489
    #> 8  -5993 1.171
    #> 9  -5992 0.887
    #> 10 -5991 0.493
    #> ..   ...   ...

trees
-----

``` r
trees_tidy <- trees %>%
  mutate(tree_num = seq_len(n())) %>%
  select(4,1:3) %>%
  tbl_df()
trees_tidy
```

    #> Source: local data frame [31 x 4]
    #> 
    #>    tree_num Girth Height Volume
    #>       (int) (dbl)  (dbl)  (dbl)
    #> 1         1   8.3     70   10.3
    #> 2         2   8.6     65   10.3
    #> 3         3   8.8     63   10.2
    #> 4         4  10.5     72   16.4
    #> 5         5  10.7     81   18.8
    #> 6         6  10.8     83   19.7
    #> 7         7  11.0     66   15.6
    #> 8         8  11.0     75   18.2
    #> 9         9  11.1     80   22.6
    #> 10       10  11.2     75   19.9
    #> ..      ...   ...    ...    ...

UCBAdmissions
-------------

``` r
UCBAdmissions_tidy <- UCBAdmissions %>%
  as.data.frame() %>%
  tbl_df()
UCBAdmissions_tidy
```

    #> Source: local data frame [24 x 4]
    #> 
    #>       Admit Gender   Dept  Freq
    #>      (fctr) (fctr) (fctr) (dbl)
    #> 1  Admitted   Male      A   512
    #> 2  Rejected   Male      A   313
    #> 3  Admitted Female      A    89
    #> 4  Rejected Female      A    19
    #> 5  Admitted   Male      B   353
    #> 6  Rejected   Male      B   207
    #> 7  Admitted Female      B    17
    #> 8  Rejected Female      B     8
    #> 9  Admitted   Male      C   120
    #> 10 Rejected   Male      C   205
    #> ..      ...    ...    ...   ...

UKDriverDeaths
--------------

``` r
UKDriverDeaths_tidy <- UKDriverDeaths %>%
  tidytime() %>%
  setNames(c("year", "series", "deaths")) %>%
  select(-series)
UKDriverDeaths_tidy
```

    #> Source: local data frame [192 x 2]
    #> 
    #>        year deaths
    #>       (dbl)  (dbl)
    #> 1  1969.000   1687
    #> 2  1969.083   1508
    #> 3  1969.167   1507
    #> 4  1969.250   1385
    #> 5  1969.333   1632
    #> 6  1969.417   1511
    #> 7  1969.500   1559
    #> 8  1969.583   1630
    #> 9  1969.667   1579
    #> 10 1969.750   1653
    #> ..      ...    ...

UKgas
-----

``` r
UKgas_tidy <- UKgas %>%
  tidytime() %>%
  setNames(c("year", "series", "gas")) %>%
  select(-series)
UKgas_tidy
```

    #> Source: local data frame [108 x 2]
    #> 
    #>       year   gas
    #>      (dbl) (dbl)
    #> 1  1960.00 160.1
    #> 2  1960.25 129.7
    #> 3  1960.50  84.8
    #> 4  1960.75 120.1
    #> 5  1961.00 160.1
    #> 6  1961.25 124.9
    #> 7  1961.50  84.8
    #> 8  1961.75 116.9
    #> 9  1962.00 169.7
    #> 10 1962.25 140.9
    #> ..     ...   ...

USAccDeaths
-----------

``` r
USAccDeaths_tidy <- USAccDeaths %>%
  tidytime() %>%
  setNames(c("year", "series", "deaths")) %>%
  select(-series)
USAccDeaths_tidy
```

    #> Source: local data frame [72 x 2]
    #> 
    #>        year deaths
    #>       (dbl)  (dbl)
    #> 1  1973.000   9007
    #> 2  1973.083   8106
    #> 3  1973.167   8928
    #> 4  1973.250   9137
    #> 5  1973.333  10017
    #> 6  1973.417  10826
    #> 7  1973.500  11317
    #> 8  1973.583  10744
    #> 9  1973.667   9713
    #> 10 1973.750   9938
    #> ..      ...    ...

USArrests
---------

``` r
USArrests_tidy <- USArrests %>%
  mutate(state = rownames(.)) %>%
  select(5,1:4) %>%
  tbl_df()
USArrests_tidy
```

    #> Source: local data frame [50 x 5]
    #> 
    #>          state Murder Assault UrbanPop  Rape
    #>          (chr)  (dbl)   (int)    (int) (dbl)
    #> 1      Alabama   13.2     236       58  21.2
    #> 2       Alaska   10.0     263       48  44.5
    #> 3      Arizona    8.1     294       80  31.0
    #> 4     Arkansas    8.8     190       50  19.5
    #> 5   California    9.0     276       91  40.6
    #> 6     Colorado    7.9     204       78  38.7
    #> 7  Connecticut    3.3     110       77  11.1
    #> 8     Delaware    5.9     238       72  15.8
    #> 9      Florida   15.4     335       80  31.9
    #> 10     Georgia   17.4     211       60  25.8
    #> ..         ...    ...     ...      ...   ...

UScitiesD
---------

``` r
# class dist
head(UScitiesD)
```

    #> [1]  587 1212  701 1936  604  748

USJudgeRatings
--------------

``` r
USJudgeRatings_tidy <- USJudgeRatings %>%
  mutate(judge= rownames(.)) %>%
  select(13,1:12) %>%
  tbl_df()
USJudgeRatings_tidy
```

    #> Source: local data frame [43 x 13]
    #> 
    #>             judge  CONT  INTG  DMNR  DILG  CFMG  DECI  PREP  FAMI  ORAL
    #>             (chr) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl)
    #> 1   AARONSON,L.H.   5.7   7.9   7.7   7.3   7.1   7.4   7.1   7.1   7.1
    #> 2  ALEXANDER,J.M.   6.8   8.9   8.8   8.5   7.8   8.1   8.0   8.0   7.8
    #> 3  ARMENTANO,A.J.   7.2   8.1   7.8   7.8   7.5   7.6   7.5   7.5   7.3
    #> 4     BERDON,R.I.   6.8   8.8   8.5   8.8   8.3   8.5   8.7   8.7   8.4
    #> 5    BRACKEN,J.J.   7.3   6.4   4.3   6.5   6.0   6.2   5.7   5.7   5.1
    #> 6      BURNS,E.B.   6.2   8.8   8.7   8.5   7.9   8.0   8.1   8.0   8.0
    #> 7   CALLAHAN,R.J.  10.6   9.0   8.9   8.7   8.5   8.5   8.5   8.5   8.6
    #> 8      COHEN,S.S.   7.0   5.9   4.9   5.1   5.4   5.9   4.8   5.1   4.7
    #> 9       DALY,J.J.   7.3   8.9   8.9   8.7   8.6   8.5   8.4   8.4   8.4
    #> 10   DANNEHY,J.F.   8.2   7.9   6.7   8.1   7.9   8.0   7.9   8.1   7.7
    #> ..            ...   ...   ...   ...   ...   ...   ...   ...   ...   ...
    #> Variables not shown: WRIT (dbl), PHYS (dbl), RTEN (dbl)

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

uspop
-----

``` r
uspop_tidy <- uspop %>%
  tidytime() %>%
  setNames(c("year", "series", "pop")) %>%
  select(-series)
uspop_tidy
```

    #> Source: local data frame [19 x 2]
    #> 
    #>     year    pop
    #>    (dbl)  (dbl)
    #> 1   1790   3.93
    #> 2   1800   5.31
    #> 3   1810   7.24
    #> 4   1820   9.64
    #> 5   1830  12.90
    #> 6   1840  17.10
    #> 7   1850  23.20
    #> 8   1860  31.40
    #> 9   1870  39.80
    #> 10  1880  50.20
    #> 11  1890  62.90
    #> 12  1900  76.00
    #> 13  1910  92.00
    #> 14  1920 105.70
    #> 15  1930 122.80
    #> 16  1940 131.70
    #> 17  1950 151.30
    #> 18  1960 179.30
    #> 19  1970 203.20

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

``` r
#matrix of topographic info
head(volcano)
```

    #>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
    #> [1,]  100  100  101  101  101  101  101  100  100   100   101   101   102
    #> [2,]  101  101  102  102  102  102  102  101  101   101   102   102   103
    #> [3,]  102  102  103  103  103  103  103  102  102   102   103   103   104
    #> [4,]  103  103  104  104  104  104  104  103  103   103   103   104   104
    #> [5,]  104  104  105  105  105  105  105  104  104   103   104   104   105
    #> [6,]  105  105  105  106  106  106  106  105  105   104   104   105   105
    #>      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
    #> [1,]   102   102   102   103   104   103   102   101   101   102   103
    #> [2,]   103   103   103   104   105   104   103   102   102   103   105
    #> [3,]   104   104   104   105   106   105   104   104   105   106   107
    #> [4,]   104   105   105   106   107   106   106   106   107   108   110
    #> [5,]   105   105   106   107   108   108   108   109   110   112   114
    #> [6,]   106   106   107   109   110   110   112   113   115   116   118
    #>      [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35]
    #> [1,]   104   104   105   107   107   107   108   108   110   110   110
    #> [2,]   106   106   107   109   110   110   110   110   111   112   113
    #> [3,]   108   110   111   113   114   115   114   115   116   118   119
    #> [4,]   111   114   117   118   117   119   120   121   122   124   125
    #> [5,]   115   118   121   122   121   123   128   131   129   130   131
    #> [6,]   119   121   124   126   126   129   134   137   137   136   136
    #>      [,36] [,37] [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46]
    #> [1,]   110   110   110   110   110   108   108   108   107   107   108
    #> [2,]   114   116   115   114   112   110   110   110   109   108   109
    #> [3,]   119   121   121   120   118   116   114   112   111   110   110
    #> [4,]   126   127   127   126   124   122   120   117   116   113   111
    #> [5,]   131   132   132   131   130   128   126   122   119   115   114
    #> [6,]   135   136   136   136   135   133   129   126   122   118   116
    #>      [,47] [,48] [,49] [,50] [,51] [,52] [,53] [,54] [,55] [,56] [,57]
    #> [1,]   108   108   108   108   107   107   107   107   106   106   105
    #> [2,]   109   109   109   108   108   108   108   107   107   106   106
    #> [3,]   110   110   109   109   109   109   108   108   107   107   106
    #> [4,]   110   110   110   109   109   109   109   108   108   107   107
    #> [5,]   112   110   110   110   110   110   109   109   108   107   107
    #> [6,]   115   113   111   110   110   110   110   109   108   108   108
    #>      [,58] [,59] [,60] [,61]
    #> [1,]   105   104   104   103
    #> [2,]   105   105   104   104
    #> [3,]   106   105   105   104
    #> [4,]   106   106   105   105
    #> [5,]   107   106   106   105
    #> [6,]   107   107   106   106

warpbreaks
----------

``` r
# 'warpbreaks' is already tidy
head(warpbreaks)
```

    #>   breaks wool tension
    #> 1     26    A       L
    #> 2     30    A       L
    #> 3     54    A       L
    #> 4     25    A       L
    #> 5     70    A       L
    #> 6     52    A       L

women
-----

``` r
# 'women' is already tidy
head(women)
```

    #>   height weight
    #> 1     58    115
    #> 2     59    117
    #> 3     60    120
    #> 4     61    123
    #> 5     62    126
    #> 6     63    129

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

WWWusage
--------

``` r
WWWusage_tidy <- WWWusage %>%
  tidytime() %>%
  setNames(c("time", "series", "users")) %>%
  select(-series)
WWWusage_tidy
```

    #> Source: local data frame [100 x 2]
    #> 
    #>     time users
    #>    (dbl) (dbl)
    #> 1      1    88
    #> 2      2    84
    #> 3      3    85
    #> 4      4    85
    #> 5      5    84
    #> 6      6    85
    #> 7      7    83
    #> 8      8    85
    #> 9      9    88
    #> 10    10    89
    #> ..   ...   ...
