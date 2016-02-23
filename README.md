-   [Overview](#overview)
    -   [ability](#ability)
    -   [airmiles](#airmiles)
    -   [AirPassengers](#airpassengers)
    -   [austres](#austres)
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
    -   [mtcars](#mtcars)

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

austres
-------

``` r
# Figure out how to get the 'Qtr1' ... colnames
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
