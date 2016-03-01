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

``` r
# 'airquality' is already tidy
```

anscombe
--------

``` r
anscombe_tidy <- anscombe %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  separate(key, into = c("var", "set"), sep=1) %>%
  spread(var, value) %>%
  arrange(set, obs)
```

austres
-------

``` r
austres_tidy <- austres %>%
  tidytime() %>%
  setNames(c("year", "series","residents"))
austres_tidy
```

    #> Source: local data frame [89 x 3]
    #> 
    #>       year series residents
    #>      (dbl)  (chr)     (dbl)
    #> 1  1971.25      x   13067.3
    #> 2  1971.50      x   13130.5
    #> 3  1971.75      x   13198.4
    #> 4  1972.00      x   13254.2
    #> 5  1972.25      x   13303.7
    #> 6  1972.50      x   13353.9
    #> 7  1972.75      x   13409.3
    #> 8  1973.00      x   13459.2
    #> 9  1973.25      x   13504.5
    #> 10 1973.50      x   13552.6
    #> ..     ...    ...       ...

attenu
------

``` r
# `attenu` already tidy
```

attitude
--------

``` r
# 'attitude' is already tidy
```

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
  tidytime() 
BJsales_tidy
```

    #> Source: local data frame [150 x 3]
    #> 
    #>    index series value
    #>    (dbl)  (chr) (dbl)
    #> 1      1      x 200.1
    #> 2      2      x 199.5
    #> 3      3      x 199.4
    #> 4      4      x 198.9
    #> 5      5      x 199.0
    #> 6      6      x 200.2
    #> 7      7      x 198.6
    #> 8      8      x 200.0
    #> 9      9      x 200.3
    #> 10    10      x 201.2
    #> ..   ...    ...   ...

BJsales.lead
------------

``` r
BJsales.lead_tidy <- BJsales.lead%>%
  tidytime() 
BJsales.lead_tidy
```

    #> Source: local data frame [150 x 3]
    #> 
    #>    index series value
    #>    (dbl)  (chr) (dbl)
    #> 1      1      x 10.01
    #> 2      2      x 10.07
    #> 3      3      x 10.32
    #> 4      4      x  9.75
    #> 5      5      x 10.33
    #> 6      6      x 10.13
    #> 7      7      x 10.36
    #> 8      8      x 10.32
    #> 9      9      x 10.13
    #> 10    10      x 10.16
    #> ..   ...    ...   ...

BOD
---

``` r
# 'BOD' is already tidy
```

cars
----

``` r
# 'cars' is already tidy
```

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

co2
---

``` r
co2_tidy <- co2 %>%
  tidytime() %>%
  setNames(c("year", "series", "co2"))
co2_tidy
```

    #> Source: local data frame [468 x 3]
    #> 
    #>        year series    co2
    #>       (dbl)  (chr)  (dbl)
    #> 1  1959.000      x 315.42
    #> 2  1959.083      x 316.31
    #> 3  1959.167      x 316.50
    #> 4  1959.250      x 317.56
    #> 5  1959.333      x 318.13
    #> 6  1959.417      x 318.00
    #> 7  1959.500      x 316.39
    #> 8  1959.583      x 314.65
    #> 9  1959.667      x 313.68
    #> 10 1959.750      x 313.18
    #> ..      ...    ...    ...

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
  setNames(c("year", "series", "disc"))
discoveries_tidy
```

    #> Source: local data frame [100 x 3]
    #> 
    #>     year series  disc
    #>    (dbl)  (chr) (dbl)
    #> 1   1860      x     5
    #> 2   1861      x     3
    #> 3   1862      x     0
    #> 4   1863      x     2
    #> 5   1864      x     0
    #> 6   1865      x     3
    #> 7   1866      x     2
    #> 8   1867      x     3
    #> 9   1868      x     6
    #> 10  1869      x     1
    #> ..   ...    ...   ...

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

euro
----

``` r
euro_tidy<- euro %>%
  data_frame(country = names(euro))
head(euro_tidy)
```

    #> Source: local data frame [6 x 2]
    #> 
    #>           . country
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
```

EuStockMarkets
--------------

``` r
EuStockMarkets_tidy <- EuStockMarkets %>%
  tidytime()
EuStockMarkets_tidy
```

    #> Source: local data frame [7,440 x 3]
    #> 
    #>       index series   value
    #>       (dbl)  (chr)   (dbl)
    #> 1  1991.496    DAX 1628.75
    #> 2  1991.500    DAX 1613.63
    #> 3  1991.504    DAX 1606.51
    #> 4  1991.508    DAX 1621.04
    #> 5  1991.512    DAX 1618.16
    #> 6  1991.515    DAX 1610.61
    #> 7  1991.519    DAX 1630.75
    #> 8  1991.523    DAX 1640.17
    #> 9  1991.527    DAX 1635.47
    #> 10 1991.531    DAX 1645.89
    #> ..      ...    ...     ...

faithful
--------

``` r
# `faithful` already tidy
```

fdeaths
-------

``` r
fdeaths_tidy <- fdeaths %>%
  tidytime()
fdeaths_tidy  
```

    #> Source: local data frame [72 x 3]
    #> 
    #>       index series value
    #>       (dbl)  (chr) (dbl)
    #> 1  1974.000      x   901
    #> 2  1974.083      x   689
    #> 3  1974.167      x   827
    #> 4  1974.250      x   677
    #> 5  1974.333      x   522
    #> 6  1974.417      x   406
    #> 7  1974.500      x   441
    #> 8  1974.583      x   393
    #> 9  1974.667      x   387
    #> 10 1974.750      x   582
    #> ..      ...    ...   ...

Formaldehyde
------------

``` r
# `Formaldehyde` already tidy
```

freeny
------

freeny.x
--------

freeny.y
--------

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
```

### infert

``` r
# 'infert' already tidy
```

InsectSprays
------------

``` r
# `InsectSprays` already tidy
```

iris
----

``` r
# 'iris' is already tidy
```

iris3
-----

``` r
# 'iris3' is already tidy
```

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
  tidytime()
JohnsonJohnson_tidy
```

    #> Source: local data frame [84 x 3]
    #> 
    #>      index series value
    #>      (dbl)  (chr) (dbl)
    #> 1  1960.00      x  0.71
    #> 2  1960.25      x  0.63
    #> 3  1960.50      x  0.85
    #> 4  1960.75      x  0.44
    #> 5  1961.00      x  0.61
    #> 6  1961.25      x  0.69
    #> 7  1961.50      x  0.92
    #> 8  1961.75      x  0.55
    #> 9  1962.00      x  0.72
    #> 10 1962.25      x  0.77
    #> ..     ...    ...   ...

LakeHuron
---------

``` r
LakeHuron_tidy <- LakeHuron %>%
  tidytime() %>%
  setNames(c("year", "series", "level_ft"))
LakeHuron_tidy
```

    #> Source: local data frame [98 x 3]
    #> 
    #>     year series level_ft
    #>    (dbl)  (chr)    (dbl)
    #> 1   1875      x   580.38
    #> 2   1876      x   581.86
    #> 3   1877      x   580.97
    #> 4   1878      x   580.80
    #> 5   1879      x   579.79
    #> 6   1880      x   580.39
    #> 7   1881      x   580.42
    #> 8   1882      x   580.82
    #> 9   1883      x   581.40
    #> 10  1884      x   581.32
    #> ..   ...    ...      ...

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
  setNames(c("time_min", "series", "lh"))
lh_tidy
```

    #> Source: local data frame [48 x 3]
    #> 
    #>    time_min series    lh
    #>       (dbl)  (chr) (dbl)
    #> 1         1      x   2.4
    #> 2         2      x   2.4
    #> 3         3      x   2.4
    #> 4         4      x   2.2
    #> 5         5      x   2.1
    #> 6         6      x   1.5
    #> 7         7      x   2.3
    #> 8         8      x   2.3
    #> 9         9      x   2.5
    #> 10       10      x   2.0
    #> ..      ...    ...   ...

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
```

lynx
----

``` r
lynx_tidy <- lynx %>%
  tidytime() %>%
  setNames(c("year", "series", "trappings"))
lynx_tidy
```

    #> Source: local data frame [114 x 3]
    #> 
    #>     year series trappings
    #>    (dbl)  (chr)     (dbl)
    #> 1   1821      x       269
    #> 2   1822      x       321
    #> 3   1823      x       585
    #> 4   1824      x       871
    #> 5   1825      x      1475
    #> 6   1826      x      2821
    #> 7   1827      x      3928
    #> 8   1828      x      5943
    #> 9   1829      x      4950
    #> 10  1830      x      2577
    #> ..   ...    ...       ...

mdeaths
-------

``` r
mdeaths_tidy <- mdeaths %>%
  tidytime() %>%
  setNames(c("year", "series", "deaths"))
mdeaths_tidy
```

    #> Source: local data frame [72 x 3]
    #> 
    #>        year series deaths
    #>       (dbl)  (chr)  (dbl)
    #> 1  1974.000      x   2134
    #> 2  1974.083      x   1863
    #> 3  1974.167      x   1877
    #> 4  1974.250      x   1877
    #> 5  1974.333      x   1492
    #> 6  1974.417      x   1249
    #> 7  1974.500      x   1280
    #> 8  1974.583      x   1131
    #> 9  1974.667      x   1209
    #> 10 1974.750      x   1492
    #> ..      ...    ...    ...

morley
------

``` r
# morley is already tidy
```

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
  setNames(c("year", "series", "temp_F"))
nhtemp_tidy
```

    #> Source: local data frame [60 x 3]
    #> 
    #>     year series temp_F
    #>    (dbl)  (chr)  (dbl)
    #> 1   1912      x   49.9
    #> 2   1913      x   52.3
    #> 3   1914      x   49.4
    #> 4   1915      x   51.1
    #> 5   1916      x   49.4
    #> 6   1917      x   47.9
    #> 7   1918      x   49.8
    #> 8   1919      x   50.9
    #> 9   1920      x   49.3
    #> 10  1921      x   51.9
    #> ..   ...    ...    ...

Nile
----

``` r
Nile_tidy <- Nile %>%
  tidytime() %>%
  setNames(c("year", "series", "flow"))
Nile_tidy
```

    #> Source: local data frame [100 x 3]
    #> 
    #>     year series  flow
    #>    (dbl)  (chr) (dbl)
    #> 1   1871      x  1120
    #> 2   1872      x  1160
    #> 3   1873      x   963
    #> 4   1874      x  1210
    #> 5   1875      x  1160
    #> 6   1876      x  1160
    #> 7   1877      x   813
    #> 8   1878      x  1230
    #> 9   1879      x  1370
    #> 10  1880      x  1140
    #> ..   ...    ...   ...

nottem
------

``` r
nottem_tidy <- nottem %>%
  tidytime() %>%
  setNames(c("year", "series", "temp_F"))
nottem_tidy
```

    #> Source: local data frame [240 x 3]
    #> 
    #>        year series temp_F
    #>       (dbl)  (chr)  (dbl)
    #> 1  1920.000      x   40.6
    #> 2  1920.083      x   40.8
    #> 3  1920.167      x   44.4
    #> 4  1920.250      x   46.7
    #> 5  1920.333      x   54.1
    #> 6  1920.417      x   58.5
    #> 7  1920.500      x   57.7
    #> 8  1920.583      x   56.4
    #> 9  1920.667      x   54.3
    #> 10 1920.750      x   50.5
    #> ..      ...    ...    ...

npk
---

``` r
# npk is already tidy
```

occupationalStatus
------------------

``` r
#?
```

Orange
------

``` r
# 'Orange' already tidy
```

OrchardSprays
-------------

``` r
# 'OrchardSprays' alreayd tidy
```

PlantGrowth
-----------

``` r
# 'PlantGrowth' already tidy
```

precip
------

``` r
# how to get town names?
```

presidents
----------

``` r
presidents_tidy <- presidents %>%
  tidytime() %>%
  setNames(c("year", "series", "rating"))
presidents_tidy
```

    #> Source: local data frame [120 x 3]
    #> 
    #>       year series rating
    #>      (dbl)  (chr)  (dbl)
    #> 1  1945.00      x     NA
    #> 2  1945.25      x     87
    #> 3  1945.50      x     82
    #> 4  1945.75      x     75
    #> 5  1946.00      x     63
    #> 6  1946.25      x     50
    #> 7  1946.50      x     43
    #> 8  1946.75      x     32
    #> 9  1947.00      x     35
    #> 10 1947.25      x     60
    #> ..     ...    ...    ...

pressure
--------

``` r
# 'pressure' is already tidy
```

Puromycin
---------

``` r
# 'Puromycin' is already tidy
```

quakes
------

``` r
# 'quakes' is already tidy
```

randu
-----

``` r
# 'randu' is already tidy
```

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
```

stack.loss
----------

stack.x
-------

stackloss
---------

``` r
# 'stackloss' already tidy
```

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
  setNames(c("year", "series", "sunspots"))
sunspot.month_tidy
```

    #> Source: local data frame [3,177 x 3]
    #> 
    #>        year series sunspots
    #>       (dbl)  (chr)    (dbl)
    #> 1  1749.000      x     58.0
    #> 2  1749.083      x     62.6
    #> 3  1749.167      x     70.0
    #> 4  1749.250      x     55.7
    #> 5  1749.333      x     85.0
    #> 6  1749.417      x     83.5
    #> 7  1749.500      x     94.8
    #> 8  1749.583      x     66.3
    #> 9  1749.667      x     75.9
    #> 10 1749.750      x     75.5
    #> ..      ...    ...      ...

sunspot.year
------------

``` r
sunspot.year_tidy <- sunspot.year %>%
  tidytime() %>%
  setNames(c("year", "series", "sunspots"))
sunspot.year_tidy
```

    #> Source: local data frame [289 x 3]
    #> 
    #>     year series sunspots
    #>    (dbl)  (chr)    (dbl)
    #> 1   1700      x        5
    #> 2   1701      x       11
    #> 3   1702      x       16
    #> 4   1703      x       23
    #> 5   1704      x       36
    #> 6   1705      x       58
    #> 7   1706      x       29
    #> 8   1707      x       20
    #> 9   1708      x       10
    #> 10  1709      x        8
    #> ..   ...    ...      ...

sunspots
--------

``` r
sunspots_tidy <- sunspots %>%
  tidytime() %>%
  setNames(c("year", "series", "sunspots"))
sunspots_tidy
```

    #> Source: local data frame [2,820 x 3]
    #> 
    #>        year series sunspots
    #>       (dbl)  (chr)    (dbl)
    #> 1  1749.000      x     58.0
    #> 2  1749.083      x     62.6
    #> 3  1749.167      x     70.0
    #> 4  1749.250      x     55.7
    #> 5  1749.333      x     85.0
    #> 6  1749.417      x     83.5
    #> 7  1749.500      x     94.8
    #> 8  1749.583      x     66.3
    #> 9  1749.667      x     75.9
    #> 10 1749.750      x     75.5
    #> ..      ...    ...      ...

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
```

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
```

treering
--------

``` r
treering_tidy <- treering %>%
  tidytime() %>%
  setNames(c("year", "series", "width"))
treering_tidy
```

    #> Source: local data frame [7,980 x 3]
    #> 
    #>     year series width
    #>    (dbl)  (chr) (dbl)
    #> 1  -6000      x 1.345
    #> 2  -5999      x 1.077
    #> 3  -5998      x 1.545
    #> 4  -5997      x 1.319
    #> 5  -5996      x 1.413
    #> 6  -5995      x 1.069
    #> 7  -5994      x 0.489
    #> 8  -5993      x 1.171
    #> 9  -5992      x 0.887
    #> 10 -5991      x 0.493
    #> ..   ...    ...   ...

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
  setNames(c("year", "series", "deaths"))
UKDriverDeaths_tidy
```

    #> Source: local data frame [192 x 3]
    #> 
    #>        year series deaths
    #>       (dbl)  (chr)  (dbl)
    #> 1  1969.000      x   1687
    #> 2  1969.083      x   1508
    #> 3  1969.167      x   1507
    #> 4  1969.250      x   1385
    #> 5  1969.333      x   1632
    #> 6  1969.417      x   1511
    #> 7  1969.500      x   1559
    #> 8  1969.583      x   1630
    #> 9  1969.667      x   1579
    #> 10 1969.750      x   1653
    #> ..      ...    ...    ...

UKgas
-----

``` r
UKgas_tidy <- UKgas %>%
  tidytime() %>%
  setNames(c("year", "series", "gas"))
UKgas_tidy
```

    #> Source: local data frame [108 x 3]
    #> 
    #>       year series   gas
    #>      (dbl)  (chr) (dbl)
    #> 1  1960.00      x 160.1
    #> 2  1960.25      x 129.7
    #> 3  1960.50      x  84.8
    #> 4  1960.75      x 120.1
    #> 5  1961.00      x 160.1
    #> 6  1961.25      x 124.9
    #> 7  1961.50      x  84.8
    #> 8  1961.75      x 116.9
    #> 9  1962.00      x 169.7
    #> 10 1962.25      x 140.9
    #> ..     ...    ...   ...

USAccDeaths
-----------

``` r
USAccDeaths_tidy <- USAccDeaths %>%
  tidytime() %>%
  setNames(c("year", "series", "deaths"))
USAccDeaths_tidy
```

    #> Source: local data frame [72 x 3]
    #> 
    #>        year series deaths
    #>       (dbl)  (chr)  (dbl)
    #> 1  1973.000      x   9007
    #> 2  1973.083      x   8106
    #> 3  1973.167      x   8928
    #> 4  1973.250      x   9137
    #> 5  1973.333      x  10017
    #> 6  1973.417      x  10826
    #> 7  1973.500      x  11317
    #> 8  1973.583      x  10744
    #> 9  1973.667      x   9713
    #> 10 1973.750      x   9938
    #> ..      ...    ...    ...

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
```

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
  setNames(c("year", "series", "pop"))
uspop_tidy
```

    #> Source: local data frame [19 x 3]
    #> 
    #>     year series    pop
    #>    (dbl)  (chr)  (dbl)
    #> 1   1790      x   3.93
    #> 2   1800      x   5.31
    #> 3   1810      x   7.24
    #> 4   1820      x   9.64
    #> 5   1830      x  12.90
    #> 6   1840      x  17.10
    #> 7   1850      x  23.20
    #> 8   1860      x  31.40
    #> 9   1870      x  39.80
    #> 10  1880      x  50.20
    #> 11  1890      x  62.90
    #> 12  1900      x  76.00
    #> 13  1910      x  92.00
    #> 14  1920      x 105.70
    #> 15  1930      x 122.80
    #> 16  1940      x 131.70
    #> 17  1950      x 151.30
    #> 18  1960      x 179.30
    #> 19  1970      x 203.20

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
```

warpbreaks
----------

``` r
# 'warpbreaks' is already tidy
```

women
-----

``` r
# 'women' is already tidy
```

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
  setNames(c("time", "series", "users"))
WWWusage_tidy
```

    #> Source: local data frame [100 x 3]
    #> 
    #>     time series users
    #>    (dbl)  (chr) (dbl)
    #> 1      1      x    88
    #> 2      2      x    84
    #> 3      3      x    85
    #> 4      4      x    85
    #> 5      5      x    84
    #> 6      6      x    85
    #> 7      7      x    83
    #> 8      8      x    85
    #> 9      9      x    88
    #> 10    10      x    89
    #> ..   ...    ...   ...
