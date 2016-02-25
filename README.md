-   [Overview](#overview)
    -   [ability](#ability)
    -   [airmiles](#airmiles)
    -   [AirPassengers](#airpassengers)
    -   [airquality](#airquality)
    -   [anscombe](#anscombe)
    -   [attenu](#attenu)
    -   [attitude](#attitude)
    -   [austres](#austres)
    -   [beavers](#beavers)
    -   [BJsales](#bjsales)
    -   [BJsales.lead](#bjsales.lead)
    -   [BOD](#bod)
    -   [cars](#cars)
    -   [ChickWeight](#chickweight)
    -   [chickwts](#chickwts)
    -   [co2](#co2)
    -   [CO2](#co2-1)
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
    -   [freeny freeny.x freeny.y](#freeny-freeny.x-freeny.y)
    -   [HairEyeColor](#haireyecolor)
    -   [Harman23.cor](#harman23.cor)
    -   [Harman 74.cor](#harman-74.cor)
    -   [Indometh](#indometh)
    -   [infert](#infert)
    -   [InsectSprays](#insectsprays)
    -   [iris](#iris)
    -   [iris3](#iris3)
    -   [islands](#islands)
    -   [JohnsonJohnson](#johnsonjohnson)
    -   [LakeHuron](#lakehuron)
    -   [ldeaths (mdeaths, fdeaths)](#ldeaths-mdeaths-fdeaths)
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
    -   [occupationalstatus](#occupationalstatus)
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
    -   [Seatbelts](#seatbelts)
    -   [sleep](#sleep)
    -   [stackloss](#stackloss)
    -   [state](#state)
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
    -   [USArrests](#usarrests)
    -   [UScitiesD](#uscitiesd)
    -   [USJudgeRatings](#usjudgeratings)
    -   [USPersonalExpenditures](#uspersonalexpenditures)
    -   [uspop](#uspop)
    -   [VADeaths](#vadeaths)
    -   [volcano](#volcano)
    -   [warpbreaks](#warpbreaks)
    -   [women](#women)
    -   [World Phones](#world-phones)
    -   [WWWusage](#wwwusage)

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
    #> 1    Jan X1949   112
    #> 2    Feb X1949   118
    #> 3    Mar X1949   132
    #> 4    Apr X1949   129
    #> 5    May X1949   121
    #> 6    Jun X1949   135
    #> 7    Jul X1949   148
    #> 8    Aug X1949   148
    #> 9    Sep X1949   136
    #> 10   Oct X1949   119
    #> ..   ...   ...   ...

airquality
----------

``` r
# `airquality` already tidy
```

anscombe
--------

``` r
anscombe %>% mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) %>%
  separate(key, into = c('var','set'), sep = 1) %>%
  spread(var, value) %>%
  arrange(set) -> anscombe_tidy

head(anscombe_tidy)
```

    #>   obs set  x    y
    #> 1   1   1 10 8.04
    #> 2   2   1  8 6.95
    #> 3   3   1 13 7.58
    #> 4   4   1  9 8.81
    #> 5   5   1 11 8.33
    #> 6   6   1 14 9.96

attenu
------

``` r
# `attenu` already tidy
```

attitude
--------

``` r
# `attitude` already tidy
```

austres
-------

``` r
dmn <- list(
  c("Qtr1","Qtr2","Qtr3","Qtr4"),
  unique(floor(time(austres)))
)

austres_tidy <- as.data.frame(t(matrix(c(NA,austres,NA,NA), 4, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(qtr, value, -year) %>%
  arrange(year)

head(austres_tidy)
```

    #>   year  qtr   value
    #> 1 1971 Qtr1      NA
    #> 2 1971 Qtr2 13067.3
    #> 3 1971 Qtr3 13130.5
    #> 4 1971 Qtr4 13198.4
    #> 5 1972 Qtr1 13254.2
    #> 6 1972 Qtr2 13303.7

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
```

    #> Warning: `rbind_list()` is deprecated. Please use `bind_rows()` instead.

``` r
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
BJsales_tidy <- data.frame(
  time = as.numeric(time(BJsales)),
  val = as.numeric(BJsales)
)

head(BJsales_tidy)
```

    #>   time   val
    #> 1    1 200.1
    #> 2    2 199.5
    #> 3    3 199.4
    #> 4    4 198.9
    #> 5    5 199.0
    #> 6    6 200.2

BJsales.lead
------------

``` r
BJsales.lead_tidy <- data.frame(
  time = as.numeric(time(BJsales.lead)),
  val = as.numeric(BJsales.lead)
)

head(BJsales.lead_tidy)
```

    #>   time   val
    #> 1    1 10.01
    #> 2    2 10.07
    #> 3    3 10.32
    #> 4    4  9.75
    #> 5    5 10.33
    #> 6    6 10.13

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

co2
---

``` r
dmn <- list(month.abb, unique(floor(time(co2))))

co2_tidy <-  data.frame(t(matrix(co2, 12, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, value, -year) %>%
  arrange(year)

head(co2_tidy)
```

    #>   year month  value
    #> 1 1959   Jan 315.42
    #> 2 1959   Feb 316.31
    #> 3 1959   Mar 316.50
    #> 4 1959   Apr 317.56
    #> 5 1959   May 318.13
    #> 6 1959   Jun 318.00

CO2
---

``` r
# `CO2` already tidy
```

crimtab
-------

``` r
crimtab_tidy <- as.data.frame(crimtab)

head(crimtab_tidy)
```

    #>   Var1   Var2 Freq
    #> 1  9.4 142.24    0
    #> 2  9.5 142.24    0
    #> 3  9.6 142.24    0
    #> 4  9.7 142.24    0
    #> 5  9.8 142.24    0
    #> 6  9.9 142.24    0

discoveries
-----------

``` r
discoveries_tidy <- data.frame(
  year = as.numeric(time(discoveries)),
  count = as.numeric(discoveries))

head(discoveries_tidy)
```

    #>   year count
    #> 1 1860     5
    #> 2 1861     3
    #> 3 1862     0
    #> 4 1863     2
    #> 5 1864     0
    #> 6 1865     3

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
euro_tidy <- data.frame(
  Country = names(euro),
  val = as.numeric(euro))

head(euro_tidy)
```

    #>   Country       val
    #> 1     ATS  13.76030
    #> 2     BEF  40.33990
    #> 3     DEM   1.95583
    #> 4     ESP 166.38600
    #> 5     FIM   5.94573
    #> 6     FRF   6.55957

euro.cross
----------

``` r
euro.cross_tidy <- as.data.frame(euro.cross) %>%
  mutate(Euro_curr = rownames(euro.cross)) %>%
  gather(Euro_curr2, value, -Euro_curr) %>%
  arrange(Euro_curr)

head(euro.cross_tidy)
```

    #>   Euro_curr Euro_curr2      value
    #> 1       ATS        ATS  1.0000000
    #> 2       ATS        BEF  2.9316149
    #> 3       ATS        DEM  0.1421357
    #> 4       ATS        ESP 12.0917422
    #> 5       ATS        FIM  0.4320931
    #> 6       ATS        FRF  0.4767025

eurodist
--------

``` r
# `eurodist` is a dist object (and I don't feel like figuring it out)
```

EuStockMarkets
--------------

``` r
dmn <- list(colnames(EuStockMarkets), time(EuStockMarkets))

EuStockMarkets_tidy <- data.frame(t(matrix(EuStockMarkets, 4, dimnames = dmn))) %>%
  mutate(time_ind = rownames(.)) %>%
  gather(key, val, -time_ind) %>%
  separate(time_ind, into = c("year","time"), sep = 4) %>%
  arrange(year)

head(EuStockMarkets_tidy)
```

    #>   year         time key     val
    #> 1 1991 .49615384615 DAX 1628.75
    #> 2 1991           .5 DAX 1618.16
    #> 3 1991 .50384615385 DAX 1635.47
    #> 4 1991 .50769230769 DAX 1629.93
    #> 5 1991 .51153846154 DAX 1631.99
    #> 6 1991 .51538461538 DAX 1605.75

faithful
--------

``` r
# `faithful` already tidy
```

fdeaths
-------

``` r
dmn <- list(month.abb, unique(floor(time(fdeaths))))

fdeaths_tidy <- data.frame(t(matrix(fdeaths, 12, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, deaths, -year) %>%
  arrange(year)

head(fdeaths_tidy)
```

    #>   year month deaths
    #> 1 1974   Jan    901
    #> 2 1974   Feb    689
    #> 3 1974   Mar    827
    #> 4 1974   Apr    677
    #> 5 1974   May    522
    #> 6 1974   Jun    406

Formaldehyde
------------

``` r
# `Formaldehyde` already tidy
```

freeny freeny.x freeny.y
------------------------

``` r
freeny_y_df <- as.data.frame(t(matrix(c(NA,freeny.y), 4)))

colnames(freeny_y_df) <- c("Qtr1","Qtr2","Qtr3","Qtr4")
rownames(freeny_y_df) <- 1962:1971

freeny_y_tmp <- freeny_y_df %>% mutate(year = rownames(freeny_y_df)) %>%
  gather(Qtr, value, -year) %>%
  arrange(year)

freeny_y_tidy <- freeny_y_tmp[2:40,]

freeny_tidy <- freeny %>% mutate(year = freeny_y_tidy$year, Qtr = freeny_y_tidy$Qtr) %>%
  gather(exp_var, value, -year, -Qtr)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
head(freeny_tidy)
```

    #>   year  Qtr exp_var   value
    #> 1 1962 Qtr2       y 8.79236
    #> 2 1962 Qtr3       y 8.79137
    #> 3 1962 Qtr4       y 8.81486
    #> 4 1963 Qtr1       y 8.81301
    #> 5 1963 Qtr2       y 8.90751
    #> 6 1963 Qtr3       y 8.93673

HairEyeColor
------------

``` r
males <- as.data.frame(HairEyeColor[1:4,1:4,1])

males_tidy <- males %>% 
  mutate(hair_color = rownames(males), sex = rep("male",4)) %>%
  gather(eye_color, value, -sex, -hair_color)

females <- as.data.frame(HairEyeColor[1:4,1:4,2])

females_tidy <- females %>% 
  mutate(hair_color = rownames(females), sex = rep("female",4)) %>%
  gather(eye_color, value, -sex, -hair_color)

HairEyeColor_tidy <- rbind(males_tidy,females_tidy)
HairEyeColor_tidy <- HairEyeColor_tidy[,c("sex","eye_color","hair_color","value")]

head(HairEyeColor_tidy)
```

    #>    sex eye_color hair_color value
    #> 1 male     Brown      Black    32
    #> 2 male     Brown      Brown    53
    #> 3 male     Brown        Red    10
    #> 4 male     Brown      Blond     3
    #> 5 male      Blue      Black    11
    #> 6 male      Blue      Brown    50

Harman23.cor
------------

Harman 74.cor
-------------

Indometh
--------

``` r
# `Indometh` already tidy
```

infert
------

``` r
# `infert` already tidy
```

InsectSprays
------------

``` r
# `InsectSprays` already tidy
```

iris
----

``` r
# `iris` already tidy
```

iris3
-----

``` r
iris3_tidy <- as.data.frame(rbind(iris3[,,1],iris3[,,2],iris3[,,3])) %>%
  mutate(species = c(
    rep(dimnames(iris3)[[3]][1], length(iris3[,1,1])),
    rep(dimnames(iris3)[[3]][2], length(iris3[,1,2])),
    rep(dimnames(iris3)[[3]][3], length(iris3[,1,3]))
  ))

head(iris3_tidy)
```

    #>   Sepal L. Sepal W. Petal L. Petal W. species
    #> 1      5.1      3.5      1.4      0.2  Setosa
    #> 2      4.9      3.0      1.4      0.2  Setosa
    #> 3      4.7      3.2      1.3      0.2  Setosa
    #> 4      4.6      3.1      1.5      0.2  Setosa
    #> 5      5.0      3.6      1.4      0.2  Setosa
    #> 6      5.4      3.9      1.7      0.4  Setosa

islands
-------

``` r
islands_tidy <- data.frame(
  land = names(islands),
  area = as.numeric(islands)
)

head(islands_tidy)
```

    #>           land  area
    #> 1       Africa 11506
    #> 2   Antarctica  5500
    #> 3         Asia 16988
    #> 4    Australia  2968
    #> 5 Axel Heiberg    16
    #> 6       Baffin   184

JohnsonJohnson
--------------

``` r
dmn <- list(
  c("Qtr1","Qtr2","Qtr3","Qtr4"),
  unique(floor(time(JohnsonJohnson)))
)

JohnsonJohnson_tidy <- as.data.frame(t(matrix(JohnsonJohnson, 4, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(Qtr, val, -year) %>%
  arrange(year)

head(JohnsonJohnson_tidy)
```

    #>   year  Qtr  val
    #> 1 1960 Qtr1 0.71
    #> 2 1960 Qtr2 0.63
    #> 3 1960 Qtr3 0.85
    #> 4 1960 Qtr4 0.44
    #> 5 1961 Qtr1 0.61
    #> 6 1961 Qtr2 0.69

LakeHuron
---------

``` r
LakeHuron_tidy <- data.frame(
  year = unique(time(LakeHuron)),
  level = as.numeric((LakeHuron))
)

head(LakeHuron_tidy)
```

    #>   year  level
    #> 1 1875 580.38
    #> 2 1876 581.86
    #> 3 1877 580.97
    #> 4 1878 580.80
    #> 5 1879 579.79
    #> 6 1880 580.39

ldeaths (mdeaths, fdeaths)
--------------------------

``` r
dmn <- list(month.abb, unique(floor(time(ldeaths))))

ldeaths_tidy <- data.frame(t(matrix(ldeaths, 12, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, deaths, -year) %>%
  arrange(year)

head(ldeaths_tidy)
```

    #>   year month deaths
    #> 1 1974   Jan   3035
    #> 2 1974   Feb   2552
    #> 3 1974   Mar   2704
    #> 4 1974   Apr   2554
    #> 5 1974   May   2014
    #> 6 1974   Jun   1655

lh
--

``` r
lh_tidy <- data.frame(
  time_int = as.numeric(time(lh)),
  lh = as.numeric(lh)
)

head(lh_tidy)
```

    #>   time_int  lh
    #> 1        1 2.4
    #> 2        2 2.4
    #> 3        3 2.4
    #> 4        4 2.2
    #> 5        5 2.1
    #> 6        6 1.5

LifeCycleSavings
----------------

``` r
LifeCycleSavings_tidy <- LifeCycleSavings %>%
  mutate(country = rownames((LifeCycleSavings))) %>%
  gather(stat, val, -country) %>%
  arrange(country)

head(LifeCycleSavings_tidy)
```

    #>     country  stat     val
    #> 1 Australia    sr   11.43
    #> 2 Australia pop15   29.35
    #> 3 Australia pop75    2.87
    #> 4 Australia   dpi 2329.68
    #> 5 Australia  ddpi    2.87
    #> 6   Austria    sr   12.07

Loblolly
--------

``` r
Loblolly_tidy <- as.data.frame(Loblolly) %>%
  mutate(index = as.numeric(rownames(Loblolly))) %>%
  arrange(index)

head(Loblolly_tidy)
```

    #>   height age Seed index
    #> 1   4.51   3  301     1
    #> 2   4.55   3  303     2
    #> 3   4.79   3  305     3
    #> 4   3.91   3  307     4
    #> 5   4.81   3  309     5
    #> 6   3.88   3  311     6

longley
-------

``` r
# `longley` already tidy
```

lynx
----

``` r
lynx_tidy <- data.frame(
  year = as.numeric(time(lynx)),
  val = as.numeric(lynx)
)

head(lynx_tidy)
```

    #>   year  val
    #> 1 1821  269
    #> 2 1822  321
    #> 3 1823  585
    #> 4 1824  871
    #> 5 1825 1475
    #> 6 1826 2821

mdeaths
-------

``` r
dmn <- list(month.abb, unique(floor(time(mdeaths))))

mdeaths_tidy <- data.frame(t(matrix(mdeaths, 12, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, deaths, -year) %>%
  arrange(year)

head(mdeaths_tidy)
```

    #>   year month deaths
    #> 1 1974   Jan   2134
    #> 2 1974   Feb   1863
    #> 3 1974   Mar   1877
    #> 4 1974   Apr   1877
    #> 5 1974   May   1492
    #> 6 1974   Jun   1249

morley
------

``` r
# `morley` already tidy
```

mtcars
------

``` r
mtcars_tidy <- mtcars %>%
  mutate(car = format_strs(rownames(.))) %>% 
  gather(key, value, -car) %>% tbl_df

mtcars_tidy
```

    #> Source: local data frame [352 x 3]
    #> 
    #>                  car   key value
    #>                (chr) (chr) (dbl)
    #> 1          mazda.rx4   mpg  21.0
    #> 2      mazda.rx4.wag   mpg  21.0
    #> 3         datsun.710   mpg  22.8
    #> 4     hornet.4.drive   mpg  21.4
    #> 5  hornet.sportabout   mpg  18.7
    #> 6            valiant   mpg  18.1
    #> 7         duster.360   mpg  14.3
    #> 8          merc.240d   mpg  24.4
    #> 9           merc.230   mpg  22.8
    #> 10          merc.280   mpg  19.2
    #> ..               ...   ...   ...

nhtemp
------

``` r
nhtemp_tidy <- data.frame(
  year = as.numeric(time(nhtemp)),
  val = as.numeric(nhtemp)
)

head(nhtemp_tidy)
```

    #>   year  val
    #> 1 1912 49.9
    #> 2 1913 52.3
    #> 3 1914 49.4
    #> 4 1915 51.1
    #> 5 1916 49.4
    #> 6 1917 47.9

Nile
----

``` r
Nile_tidy <- data.frame(
  year = as.numeric(time(Nile)),
  val = as.numeric(Nile)
)

head(Nile_tidy)
```

    #>   year  val
    #> 1 1871 1120
    #> 2 1872 1160
    #> 3 1873  963
    #> 4 1874 1210
    #> 5 1875 1160
    #> 6 1876 1160

nottem
------

``` r
dmn <- list(month.abb, unique(floor(time(nottem))))

nottem_tidy <- data.frame(t(matrix(nottem,12,dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, val, -year) %>%
  arrange(year)

head(nottem_tidy)
```

    #>   year month  val
    #> 1 1920   Jan 40.6
    #> 2 1920   Feb 40.8
    #> 3 1920   Mar 44.4
    #> 4 1920   Apr 46.7
    #> 5 1920   May 54.1
    #> 6 1920   Jun 58.5

npk
---

``` r
# `npk` already tidy
```

occupationalstatus
------------------

``` r
occupationalStatus_tidy <- as.data.frame(occupationalStatus) %>%
  arrange(origin)

head(occupationalStatus_tidy)
```

    #>   origin destination Freq
    #> 1      1           1   50
    #> 2      1           2   19
    #> 3      1           3   26
    #> 4      1           4    8
    #> 5      1           5    7
    #> 6      1           6   11

Orange
------

``` r
# `Orange` already tidy
```

OrchardSprays
-------------

``` r
# `OrchardSprays` already tidy
```

PlantGrowth
-----------

``` r
# `PlantGrowth` already tidy
```

precip
------

``` r
precip_tidy <- data.frame(
  City = names(precip),
  val = as.numeric(precip)
) %>% arrange(City)

head(precip_tidy)
```

    #>            City  val
    #> 1        Albany 33.4
    #> 2   Albuquerque  7.8
    #> 3       Atlanta 48.3
    #> 4 Atlantic City 45.5
    #> 5     Baltimore 41.8
    #> 6       Bismark 16.2

presidents
----------

``` r
dmn <- list(
  c("Qtr1","Qtr2","Qtr3","Qtr4"),
  unique(floor(time(presidents)))
)

presidents_tidy <- data.frame(t(matrix(presidents,4,dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(Qtr, val, -year) %>%
  arrange(year)

head(presidents_tidy)
```

    #>   year  Qtr val
    #> 1 1945 Qtr1  NA
    #> 2 1945 Qtr2  87
    #> 3 1945 Qtr3  82
    #> 4 1945 Qtr4  75
    #> 5 1946 Qtr1  63
    #> 6 1946 Qtr2  50

pressure
--------

``` r
# `pressure` already tidy
```

Puromycin
---------

``` r
# `Puromycin` already tidy
```

quakes
------

``` r
quakes_tidy <- quakes %>% mutate(event = rownames(.))

head(quakes_tidy)
```

    #>      lat   long depth mag stations event
    #> 1 -20.42 181.62   562 4.8       41     1
    #> 2 -20.62 181.03   650 4.2       15     2
    #> 3 -26.00 184.10    42 5.4       43     3
    #> 4 -17.97 181.66   626 4.1       19     4
    #> 5 -20.42 181.96   649 4.0       11     5
    #> 6 -19.68 184.31   195 4.0       12     6

randu
-----

``` r
# `randu` already tidy
```

rivers
------

``` r
rivers_tidy <- data.frame(index = 1:length(rivers), length = rivers)

head(rivers_tidy)
```

    #>   index length
    #> 1     1    735
    #> 2     2    320
    #> 3     3    325
    #> 4     4    392
    #> 5     5    524
    #> 6     6    450

rock
----

``` r
# `rock` already tidy
```

Seatbelts
---------

``` r
Seatbelts_tidy <- data.frame(Seatbelts) %>%
  mutate(year = as.numeric(floor(time(Seatbelts))),
         month = rep(month.abb,(length(as.numeric(floor(time(Seatbelts))))/12))) %>%
  gather(key, val, -year,-month) %>%
  arrange(year)

head(Seatbelts_tidy)
```

    #>   year month           key val
    #> 1 1969   Jan DriversKilled 107
    #> 2 1969   Feb DriversKilled  97
    #> 3 1969   Mar DriversKilled 102
    #> 4 1969   Apr DriversKilled  87
    #> 5 1969   May DriversKilled 119
    #> 6 1969   Jun DriversKilled 106

sleep
-----

``` r
# `sleep` already tidy
```

stackloss
---------

``` r
# `stackloss` already tidy
```

state
-----

``` r
state_tidy <- data.frame(state = state.name, state_abb = state.abb, area = state.area,
                         center_x = state.center$x, center_y = state.center$y, division = state.division)

head(state_tidy)
```

    #>        state state_abb   area  center_x center_y           division
    #> 1    Alabama        AL  51609  -86.7509  32.5901 East South Central
    #> 2     Alaska        AK 589757 -127.2500  49.2500            Pacific
    #> 3    Arizona        AZ 113909 -111.6250  34.2192           Mountain
    #> 4   Arkansas        AR  53104  -92.2992  34.7336 West South Central
    #> 5 California        CA 158693 -119.7730  36.5341            Pacific
    #> 6   Colorado        CO 104247 -105.5130  38.6777           Mountain

sunspot.month
-------------

``` r
dmn <- list(month.abb, unique(floor(time(sunspot.month))))

sunspot.month_tidy <- data.frame(t(matrix(c(sunspot.month,NA,NA,NA),12,dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, val, -year) %>%
  arrange(year)

head(sunspot.month_tidy)
```

    #>   year month  val
    #> 1 1749   Jan 58.0
    #> 2 1749   Feb 62.6
    #> 3 1749   Mar 70.0
    #> 4 1749   Apr 55.7
    #> 5 1749   May 85.0
    #> 6 1749   Jun 83.5

sunspot.year
------------

``` r
sunspot.year_tidy <- data.frame(
  year = time(sunspot.year),
  val = as.numeric(sunspot.year)
)

head(sunspot.year_tidy)
```

    #>   year val
    #> 1 1700   5
    #> 2 1701  11
    #> 3 1702  16
    #> 4 1703  23
    #> 5 1704  36
    #> 6 1705  58

sunspots
--------

``` r
dmn <- list(month.abb, unique(floor(time(sunspots))))

sunspots_tidy <- data.frame(t(matrix(sunspots,12,dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month, val, -year) %>%
  arrange(year)

head(sunspots_tidy)
```

    #>   year month  val
    #> 1 1749   Jan 58.0
    #> 2 1749   Feb 62.6
    #> 3 1749   Mar 70.0
    #> 4 1749   Apr 55.7
    #> 5 1749   May 85.0
    #> 6 1749   Jun 83.5

swiss
-----

``` r
swiss_tidy <- swiss %>% mutate(province = rownames(.)) %>%
  gather(key, val, -province) %>%
  arrange(province)

head(swiss_tidy)
```

    #>   province              key   val
    #> 1    Aigle        Fertility 64.10
    #> 2    Aigle      Agriculture 62.00
    #> 3    Aigle      Examination 21.00
    #> 4    Aigle        Education 12.00
    #> 5    Aigle         Catholic  8.52
    #> 6    Aigle Infant.Mortality 16.50

Theoph
------

``` r
# `Theoph` already tidy
```

Titanic
-------

``` r
Titanic_tidy <- data.frame(NULL)

for(i in 1:2){
  for(j in 1:2){
    x <- as.data.frame(Titanic[,,i,j]) %>%
      mutate(class = rownames(.),
             age = rep(dimnames(Titanic)$Age[i],length(Titanic[,1,i,j])),
             survived = rep(dimnames(Titanic)$Survived[j],length(Titanic[,1,i,j])))
    Titanic_tidy <- rbind(Titanic_tidy,x)
  }
}

head(Titanic_tidy)
```

    #>   Male Female class   age survived
    #> 1    0      0   1st Child       No
    #> 2    0      0   2nd Child       No
    #> 3   35     17   3rd Child       No
    #> 4    0      0  Crew Child       No
    #> 5    5      1   1st Child      Yes
    #> 6   11     13   2nd Child      Yes

ToothGrowth
-----------

``` r
ToothGrowth_tidy <- ToothGrowth %>%
  mutate(index = as.numeric(rownames(.))) %>%
  gather(key,val,-index) %>%
  arrange(index)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
head(ToothGrowth_tidy)
```

    #>   index  key  val
    #> 1     1  len  4.2
    #> 2     1 supp   VC
    #> 3     1 dose  0.5
    #> 4     2  len 11.5
    #> 5     2 supp   VC
    #> 6     2 dose  0.5

treering
--------

``` r
treering_tidy <- data.frame(
  time = time(treering),
  val = as.numeric(treering)
)

head(treering_tidy)
```

    #>    time   val
    #> 1 -6000 1.345
    #> 2 -5999 1.077
    #> 3 -5998 1.545
    #> 4 -5997 1.319
    #> 5 -5996 1.413
    #> 6 -5995 1.069

trees
-----

``` r
trees_tidy <- trees %>% mutate(index = as.numeric(rownames(.))) %>%
  gather(key, val, -index) %>%
  arrange(index)

head(trees_tidy)
```

    #>   index    key  val
    #> 1     1  Girth  8.3
    #> 2     1 Height 70.0
    #> 3     1 Volume 10.3
    #> 4     2  Girth  8.6
    #> 5     2 Height 65.0
    #> 6     2 Volume 10.3

UCBAdmissions
-------------

``` r
UCBAdmissions_tidy <- data.frame(NULL)

for(i in 1:length(UCBAdmissions[1,1,])){
  x <- as.data.frame(UCBAdmissions[,,i]) %>%
    mutate(admit = rownames(.),
           dept = rep(dimnames(UCBAdmissions)$Dept[i],length(UCBAdmissions[,1,i]))) %>%
    gather(sex,val,-admit,-dept)
  
  UCBAdmissions_tidy <- rbind(UCBAdmissions_tidy,x)
}

head(UCBAdmissions_tidy)
```

    #>      admit dept    sex val
    #> 1 Admitted    A   Male 512
    #> 2 Rejected    A   Male 313
    #> 3 Admitted    A Female  89
    #> 4 Rejected    A Female  19
    #> 5 Admitted    B   Male 353
    #> 6 Rejected    B   Male 207

UKDriverDeaths
--------------

``` r
dmn <- list(month.abb, unique(floor(time(UKDriverDeaths))))

UKDriverDeaths_tidy <- data.frame(t(matrix(UKDriverDeaths,12,dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(month,val,-year) %>%
  arrange(year)

head(UKDriverDeaths_tidy)
```

    #>   year month  val
    #> 1 1969   Jan 1687
    #> 2 1969   Feb 1508
    #> 3 1969   Mar 1507
    #> 4 1969   Apr 1385
    #> 5 1969   May 1632
    #> 6 1969   Jun 1511

UKgas
-----

``` r
dmn <- list(
  c("Qtr1","Qtr2","Qtr3","Qtr4"),
  unique(floor(time(UKgas)))
)

UKgas_tidy <- data.frame(t(matrix(UKgas, 4, dimnames = dmn))) %>%
  mutate(year = rownames(.)) %>%
  gather(Qtr,val,-year) %>%
  arrange(year)

head(UKgas_tidy)
```

    #>   year  Qtr   val
    #> 1 1960 Qtr1 160.1
    #> 2 1960 Qtr2 129.7
    #> 3 1960 Qtr3  84.8
    #> 4 1960 Qtr4 120.1
    #> 5 1961 Qtr1 160.1
    #> 6 1961 Qtr2 124.9

USArrests
---------

``` r
USArrests_tidy <- as.data.frame(USArrests) %>%
  mutate(state = rownames(.)) %>%
  gather(type, val, -state) %>%
  arrange(state)

head(USArrests_tidy)
```

    #>     state     type   val
    #> 1 Alabama   Murder  13.2
    #> 2 Alabama  Assault 236.0
    #> 3 Alabama UrbanPop  58.0
    #> 4 Alabama     Rape  21.2
    #> 5  Alaska   Murder  10.0
    #> 6  Alaska  Assault 263.0

UScitiesD
---------

``` r
# `UScitiesD` is a dist file
```

USJudgeRatings
--------------

``` r
USJudgeRatings_tidy <- as.data.frame(USJudgeRatings) %>%
  mutate(judge = rownames(.)) %>%
  gather(var, val, -judge) %>%
  arrange(judge)

head(USJudgeRatings_tidy)
```

    #>           judge  var val
    #> 1 AARONSON,L.H. CONT 5.7
    #> 2 AARONSON,L.H. INTG 7.9
    #> 3 AARONSON,L.H. DMNR 7.7
    #> 4 AARONSON,L.H. DILG 7.3
    #> 5 AARONSON,L.H. CFMG 7.1
    #> 6 AARONSON,L.H. DECI 7.4

USPersonalExpenditures
----------------------

``` r
USPersonalExpenditure_tidy <- as.data.frame(USPersonalExpenditure) %>%
  mutate(type = rownames(.)) %>%
  gather(year, val, -type) %>%
  arrange(year)

head(USPersonalExpenditure_tidy)
```

    #>                  type year    val
    #> 1    Food and Tobacco 1940 22.200
    #> 2 Household Operation 1940 10.500
    #> 3  Medical and Health 1940  3.530
    #> 4       Personal Care 1940  1.040
    #> 5   Private Education 1940  0.341
    #> 6    Food and Tobacco 1945 44.500

uspop
-----

``` r
uspop_tidy <- data.frame(
  time = time(uspop),
  val = as.numeric(uspop)
)

head(uspop_tidy)
```

    #>   time   val
    #> 1 1790  3.93
    #> 2 1800  5.31
    #> 3 1810  7.24
    #> 4 1820  9.64
    #> 5 1830 12.90
    #> 6 1840 17.10

VADeaths
--------

``` r
VADeaths_tidy <- as.data.frame(VADeaths) %>%
  mutate(age = rownames(.)) %>%
  gather(key, val, -age) %>%
  arrange(age)

head(VADeaths_tidy)
```

    #>     age          key  val
    #> 1 50-54   Rural Male 11.7
    #> 2 50-54 Rural Female  8.7
    #> 3 50-54   Urban Male 15.4
    #> 4 50-54 Urban Female  8.4
    #> 5 55-59   Rural Male 18.1
    #> 6 55-59 Rural Female 11.7

volcano
-------

``` r
# `volcano` is a topographic map
```

warpbreaks
----------

``` r
# `warpbreaks` already tidy
```

women
-----

``` r
# `women` already tidy
```

World Phones
------------

``` r
WorldPhones_tidy <- WorldPhones %>%
  as.data.frame() %>%
  mutate(year = rownames(.)) %>%
  gather(continent, value, -year) %>% tbl_df

WorldPhones_tidy
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
WWWusage_tidy <- data.frame(
  time = time(WWWusage),
  val = as.numeric(WWWusage)
)

head(WWWusage_tidy)
```

    #>   time val
    #> 1    1  88
    #> 2    2  84
    #> 3    3  85
    #> 4    4  85
    #> 5    5  84
    #> 6    6  85
