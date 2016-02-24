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
  gather(key, value, -obs)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
Indometh_tidy
```

    #>     obs     key value
    #> 1     1 Subject     1
    #> 2     2 Subject     1
    #> 3     3 Subject     1
    #> 4     4 Subject     1
    #> 5     5 Subject     1
    #> 6     6 Subject     1
    #> 7     7 Subject     1
    #> 8     8 Subject     1
    #> 9     9 Subject     1
    #> 10   10 Subject     1
    #> 11   11 Subject     1
    #> 12   12 Subject     2
    #> 13   13 Subject     2
    #> 14   14 Subject     2
    #> 15   15 Subject     2
    #> 16   16 Subject     2
    #> 17   17 Subject     2
    #> 18   18 Subject     2
    #> 19   19 Subject     2
    #> 20   20 Subject     2
    #> 21   21 Subject     2
    #> 22   22 Subject     2
    #> 23   23 Subject     3
    #> 24   24 Subject     3
    #> 25   25 Subject     3
    #> 26   26 Subject     3
    #> 27   27 Subject     3
    #> 28   28 Subject     3
    #> 29   29 Subject     3
    #> 30   30 Subject     3
    #> 31   31 Subject     3
    #> 32   32 Subject     3
    #> 33   33 Subject     3
    #> 34   34 Subject     4
    #> 35   35 Subject     4
    #> 36   36 Subject     4
    #> 37   37 Subject     4
    #> 38   38 Subject     4
    #> 39   39 Subject     4
    #> 40   40 Subject     4
    #> 41   41 Subject     4
    #> 42   42 Subject     4
    #> 43   43 Subject     4
    #> 44   44 Subject     4
    #> 45   45 Subject     5
    #> 46   46 Subject     5
    #> 47   47 Subject     5
    #> 48   48 Subject     5
    #> 49   49 Subject     5
    #> 50   50 Subject     5
    #> 51   51 Subject     5
    #> 52   52 Subject     5
    #> 53   53 Subject     5
    #> 54   54 Subject     5
    #> 55   55 Subject     5
    #> 56   56 Subject     6
    #> 57   57 Subject     6
    #> 58   58 Subject     6
    #> 59   59 Subject     6
    #> 60   60 Subject     6
    #> 61   61 Subject     6
    #> 62   62 Subject     6
    #> 63   63 Subject     6
    #> 64   64 Subject     6
    #> 65   65 Subject     6
    #> 66   66 Subject     6
    #> 67    1    time  0.25
    #> 68    2    time   0.5
    #> 69    3    time  0.75
    #> 70    4    time     1
    #> 71    5    time  1.25
    #> 72    6    time     2
    #> 73    7    time     3
    #> 74    8    time     4
    #> 75    9    time     5
    #> 76   10    time     6
    #> 77   11    time     8
    #> 78   12    time  0.25
    #> 79   13    time   0.5
    #> 80   14    time  0.75
    #> 81   15    time     1
    #> 82   16    time  1.25
    #> 83   17    time     2
    #> 84   18    time     3
    #> 85   19    time     4
    #> 86   20    time     5
    #> 87   21    time     6
    #> 88   22    time     8
    #> 89   23    time  0.25
    #> 90   24    time   0.5
    #> 91   25    time  0.75
    #> 92   26    time     1
    #> 93   27    time  1.25
    #> 94   28    time     2
    #> 95   29    time     3
    #> 96   30    time     4
    #> 97   31    time     5
    #> 98   32    time     6
    #> 99   33    time     8
    #> 100  34    time  0.25
    #> 101  35    time   0.5
    #> 102  36    time  0.75
    #> 103  37    time     1
    #> 104  38    time  1.25
    #> 105  39    time     2
    #> 106  40    time     3
    #> 107  41    time     4
    #> 108  42    time     5
    #> 109  43    time     6
    #> 110  44    time     8
    #> 111  45    time  0.25
    #> 112  46    time   0.5
    #> 113  47    time  0.75
    #> 114  48    time     1
    #> 115  49    time  1.25
    #> 116  50    time     2
    #> 117  51    time     3
    #> 118  52    time     4
    #> 119  53    time     5
    #> 120  54    time     6
    #> 121  55    time     8
    #> 122  56    time  0.25
    #> 123  57    time   0.5
    #> 124  58    time  0.75
    #> 125  59    time     1
    #> 126  60    time  1.25
    #> 127  61    time     2
    #> 128  62    time     3
    #> 129  63    time     4
    #> 130  64    time     5
    #> 131  65    time     6
    #> 132  66    time     8
    #> 133   1    conc   1.5
    #> 134   2    conc  0.94
    #> 135   3    conc  0.78
    #> 136   4    conc  0.48
    #> 137   5    conc  0.37
    #> 138   6    conc  0.19
    #> 139   7    conc  0.12
    #> 140   8    conc  0.11
    #> 141   9    conc  0.08
    #> 142  10    conc  0.07
    #> 143  11    conc  0.05
    #> 144  12    conc  2.03
    #> 145  13    conc  1.63
    #> 146  14    conc  0.71
    #> 147  15    conc   0.7
    #> 148  16    conc  0.64
    #> 149  17    conc  0.36
    #> 150  18    conc  0.32
    #> 151  19    conc   0.2
    #> 152  20    conc  0.25
    #> 153  21    conc  0.12
    #> 154  22    conc  0.08
    #> 155  23    conc  2.72
    #> 156  24    conc  1.49
    #> 157  25    conc  1.16
    #> 158  26    conc   0.8
    #> 159  27    conc   0.8
    #> 160  28    conc  0.39
    #> 161  29    conc  0.22
    #> 162  30    conc  0.12
    #> 163  31    conc  0.11
    #> 164  32    conc  0.08
    #> 165  33    conc  0.08
    #> 166  34    conc  1.85
    #> 167  35    conc  1.39
    #> 168  36    conc  1.02
    #> 169  37    conc  0.89
    #> 170  38    conc  0.59
    #> 171  39    conc   0.4
    #> 172  40    conc  0.16
    #> 173  41    conc  0.11
    #> 174  42    conc   0.1
    #> 175  43    conc  0.07
    #> 176  44    conc  0.07
    #> 177  45    conc  2.05
    #> 178  46    conc  1.04
    #> 179  47    conc  0.81
    #> 180  48    conc  0.39
    #> 181  49    conc   0.3
    #> 182  50    conc  0.23
    #> 183  51    conc  0.13
    #> 184  52    conc  0.11
    #> 185  53    conc  0.08
    #> 186  54    conc   0.1
    #> 187  55    conc  0.06
    #> 188  56    conc  2.31
    #> 189  57    conc  1.44
    #> 190  58    conc  1.03
    #> 191  59    conc  0.84
    #> 192  60    conc  0.64
    #> 193  61    conc  0.42
    #> 194  62    conc  0.24
    #> 195  63    conc  0.17
    #> 196  64    conc  0.13
    #> 197  65    conc   0.1
    #> 198  66    conc  0.09

### infert

``` r
infert_tidy <- infert %>%
  mutate(subject = seq_len(n())) %>%
  gather(key, value, -subject)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
infert_tidy
```

    #>      subject            key   value
    #> 1          1      education  0-5yrs
    #> 2          2      education  0-5yrs
    #> 3          3      education  0-5yrs
    #> 4          4      education  0-5yrs
    #> 5          5      education 6-11yrs
    #> 6          6      education 6-11yrs
    #> 7          7      education 6-11yrs
    #> 8          8      education 6-11yrs
    #> 9          9      education 6-11yrs
    #> 10        10      education 6-11yrs
    #> 11        11      education 6-11yrs
    #> 12        12      education 6-11yrs
    #> 13        13      education 6-11yrs
    #> 14        14      education 6-11yrs
    #> 15        15      education 6-11yrs
    #> 16        16      education 6-11yrs
    #> 17        17      education 6-11yrs
    #> 18        18      education 6-11yrs
    #> 19        19      education 6-11yrs
    #> 20        20      education 6-11yrs
    #> 21        21      education 6-11yrs
    #> 22        22      education 6-11yrs
    #> 23        23      education 6-11yrs
    #> 24        24      education 6-11yrs
    #> 25        25      education 6-11yrs
    #> 26        26      education 6-11yrs
    #> 27        27      education 6-11yrs
    #> 28        28      education 6-11yrs
    #> 29        29      education 6-11yrs
    #> 30        30      education 6-11yrs
    #> 31        31      education 6-11yrs
    #> 32        32      education 6-11yrs
    #> 33        33      education 6-11yrs
    #> 34        34      education 6-11yrs
    #> 35        35      education 6-11yrs
    #> 36        36      education 6-11yrs
    #> 37        37      education 6-11yrs
    #> 38        38      education 6-11yrs
    #> 39        39      education 6-11yrs
    #> 40        40      education 6-11yrs
    #> 41        41      education 6-11yrs
    #> 42        42      education 6-11yrs
    #> 43        43      education 6-11yrs
    #> 44        44      education 6-11yrs
    #> 45        45      education 12+ yrs
    #> 46        46      education 12+ yrs
    #> 47        47      education 12+ yrs
    #> 48        48      education 12+ yrs
    #> 49        49      education 12+ yrs
    #> 50        50      education 12+ yrs
    #> 51        51      education 12+ yrs
    #> 52        52      education 12+ yrs
    #> 53        53      education 12+ yrs
    #> 54        54      education 12+ yrs
    #> 55        55      education 12+ yrs
    #> 56        56      education 12+ yrs
    #> 57        57      education 12+ yrs
    #> 58        58      education 12+ yrs
    #> 59        59      education 12+ yrs
    #> 60        60      education 12+ yrs
    #> 61        61      education 12+ yrs
    #> 62        62      education 12+ yrs
    #> 63        63      education 12+ yrs
    #> 64        64      education 12+ yrs
    #> 65        65      education 12+ yrs
    #> 66        66      education 12+ yrs
    #> 67        67      education 12+ yrs
    #> 68        68      education 12+ yrs
    #> 69        69      education 12+ yrs
    #> 70        70      education 12+ yrs
    #> 71        71      education 12+ yrs
    #> 72        72      education 12+ yrs
    #> 73        73      education 12+ yrs
    #> 74        74      education 12+ yrs
    #> 75        75      education 12+ yrs
    #> 76        76      education 12+ yrs
    #> 77        77      education 12+ yrs
    #> 78        78      education 12+ yrs
    #> 79        79      education 12+ yrs
    #> 80        80      education 12+ yrs
    #> 81        81      education 12+ yrs
    #> 82        82      education 12+ yrs
    #> 83        83      education 12+ yrs
    #> 84        84      education  0-5yrs
    #> 85        85      education  0-5yrs
    #> 86        86      education  0-5yrs
    #> 87        87      education  0-5yrs
    #> 88        88      education 6-11yrs
    #> 89        89      education 6-11yrs
    #> 90        90      education 6-11yrs
    #> 91        91      education 6-11yrs
    #> 92        92      education 6-11yrs
    #> 93        93      education 6-11yrs
    #> 94        94      education 6-11yrs
    #> 95        95      education 6-11yrs
    #> 96        96      education 6-11yrs
    #> 97        97      education 6-11yrs
    #> 98        98      education 6-11yrs
    #> 99        99      education 6-11yrs
    #> 100      100      education 6-11yrs
    #> 101      101      education 6-11yrs
    #> 102      102      education 6-11yrs
    #> 103      103      education 6-11yrs
    #> 104      104      education 6-11yrs
    #> 105      105      education 6-11yrs
    #> 106      106      education 6-11yrs
    #> 107      107      education 6-11yrs
    #> 108      108      education 6-11yrs
    #> 109      109      education 6-11yrs
    #> 110      110      education 6-11yrs
    #> 111      111      education 6-11yrs
    #> 112      112      education 6-11yrs
    #> 113      113      education 6-11yrs
    #> 114      114      education 6-11yrs
    #> 115      115      education 6-11yrs
    #> 116      116      education 6-11yrs
    #> 117      117      education 6-11yrs
    #> 118      118      education 6-11yrs
    #> 119      119      education 6-11yrs
    #> 120      120      education 6-11yrs
    #> 121      121      education 6-11yrs
    #> 122      122      education 6-11yrs
    #> 123      123      education 6-11yrs
    #> 124      124      education 6-11yrs
    #> 125      125      education 6-11yrs
    #> 126      126      education 6-11yrs
    #> 127      127      education 6-11yrs
    #> 128      128      education 12+ yrs
    #> 129      129      education 12+ yrs
    #> 130      130      education 12+ yrs
    #> 131      131      education 12+ yrs
    #> 132      132      education 12+ yrs
    #> 133      133      education 12+ yrs
    #> 134      134      education 12+ yrs
    #> 135      135      education 12+ yrs
    #> 136      136      education 12+ yrs
    #> 137      137      education 12+ yrs
    #> 138      138      education 12+ yrs
    #> 139      139      education 12+ yrs
    #> 140      140      education 12+ yrs
    #> 141      141      education 12+ yrs
    #> 142      142      education 12+ yrs
    #> 143      143      education 12+ yrs
    #> 144      144      education 12+ yrs
    #> 145      145      education 12+ yrs
    #> 146      146      education 12+ yrs
    #> 147      147      education 12+ yrs
    #> 148      148      education 12+ yrs
    #> 149      149      education 12+ yrs
    #> 150      150      education 12+ yrs
    #> 151      151      education 12+ yrs
    #> 152      152      education 12+ yrs
    #> 153      153      education 12+ yrs
    #> 154      154      education 12+ yrs
    #> 155      155      education 12+ yrs
    #> 156      156      education 12+ yrs
    #> 157      157      education 12+ yrs
    #> 158      158      education 12+ yrs
    #> 159      159      education 12+ yrs
    #> 160      160      education 12+ yrs
    #> 161      161      education 12+ yrs
    #> 162      162      education 12+ yrs
    #> 163      163      education 12+ yrs
    #> 164      164      education 12+ yrs
    #> 165      165      education 12+ yrs
    #> 166      166      education  0-5yrs
    #> 167      167      education  0-5yrs
    #> 168      168      education  0-5yrs
    #> 169      169      education  0-5yrs
    #> 170      170      education 6-11yrs
    #> 171      171      education 6-11yrs
    #> 172      172      education 6-11yrs
    #> 173      173      education 6-11yrs
    #> 174      174      education 6-11yrs
    #> 175      175      education 6-11yrs
    #> 176      176      education 6-11yrs
    #> 177      177      education 6-11yrs
    #> 178      178      education 6-11yrs
    #> 179      179      education 6-11yrs
    #> 180      180      education 6-11yrs
    #> 181      181      education 6-11yrs
    #> 182      182      education 6-11yrs
    #> 183      183      education 6-11yrs
    #> 184      184      education 6-11yrs
    #> 185      185      education 6-11yrs
    #> 186      186      education 6-11yrs
    #> 187      187      education 6-11yrs
    #> 188      188      education 6-11yrs
    #> 189      189      education 6-11yrs
    #> 190      190      education 6-11yrs
    #> 191      191      education 6-11yrs
    #> 192      192      education 6-11yrs
    #> 193      193      education 6-11yrs
    #> 194      194      education 6-11yrs
    #> 195      195      education 6-11yrs
    #> 196      196      education 6-11yrs
    #> 197      197      education 6-11yrs
    #> 198      198      education 6-11yrs
    #> 199      199      education 6-11yrs
    #> 200      200      education 6-11yrs
    #> 201      201      education 6-11yrs
    #> 202      202      education 6-11yrs
    #> 203      203      education 6-11yrs
    #> 204      204      education 6-11yrs
    #> 205      205      education 6-11yrs
    #> 206      206      education 6-11yrs
    #> 207      207      education 6-11yrs
    #> 208      208      education 6-11yrs
    #> 209      209      education 6-11yrs
    #> 210      210      education 12+ yrs
    #> 211      211      education 12+ yrs
    #> 212      212      education 12+ yrs
    #> 213      213      education 12+ yrs
    #> 214      214      education 12+ yrs
    #> 215      215      education 12+ yrs
    #> 216      216      education 12+ yrs
    #> 217      217      education 12+ yrs
    #> 218      218      education 12+ yrs
    #> 219      219      education 12+ yrs
    #> 220      220      education 12+ yrs
    #> 221      221      education 12+ yrs
    #> 222      222      education 12+ yrs
    #> 223      223      education 12+ yrs
    #> 224      224      education 12+ yrs
    #> 225      225      education 12+ yrs
    #> 226      226      education 12+ yrs
    #> 227      227      education 12+ yrs
    #> 228      228      education 12+ yrs
    #> 229      229      education 12+ yrs
    #> 230      230      education 12+ yrs
    #> 231      231      education 12+ yrs
    #> 232      232      education 12+ yrs
    #> 233      233      education 12+ yrs
    #> 234      234      education 12+ yrs
    #> 235      235      education 12+ yrs
    #> 236      236      education 12+ yrs
    #> 237      237      education 12+ yrs
    #> 238      238      education 12+ yrs
    #> 239      239      education 12+ yrs
    #> 240      240      education 12+ yrs
    #> 241      241      education 12+ yrs
    #> 242      242      education 12+ yrs
    #> 243      243      education 12+ yrs
    #> 244      244      education 12+ yrs
    #> 245      245      education 12+ yrs
    #> 246      246      education 12+ yrs
    #> 247      247      education 12+ yrs
    #> 248      248      education 12+ yrs
    #> 249        1            age      26
    #> 250        2            age      42
    #> 251        3            age      39
    #> 252        4            age      34
    #> 253        5            age      35
    #> 254        6            age      36
    #> 255        7            age      23
    #> 256        8            age      32
    #> 257        9            age      21
    #> 258       10            age      28
    #> 259       11            age      29
    #> 260       12            age      37
    #> 261       13            age      31
    #> 262       14            age      29
    #> 263       15            age      31
    #> 264       16            age      27
    #> 265       17            age      30
    #> 266       18            age      26
    #> 267       19            age      25
    #> 268       20            age      44
    #> 269       21            age      40
    #> 270       22            age      35
    #> 271       23            age      28
    #> 272       24            age      36
    #> 273       25            age      27
    #> 274       26            age      40
    #> 275       27            age      38
    #> 276       28            age      34
    #> 277       29            age      28
    #> 278       30            age      30
    #> 279       31            age      32
    #> 280       32            age      34
    #> 281       33            age      42
    #> 282       34            age      32
    #> 283       35            age      39
    #> 284       36            age      35
    #> 285       37            age      36
    #> 286       38            age      34
    #> 287       39            age      30
    #> 288       40            age      28
    #> 289       41            age      39
    #> 290       42            age      35
    #> 291       43            age      41
    #> 292       44            age      37
    #> 293       45            age      30
    #> 294       46            age      37
    #> 295       47            age      28
    #> 296       48            age      27
    #> 297       49            age      26
    #> 298       50            age      38
    #> 299       51            age      24
    #> 300       52            age      36
    #> 301       53            age      27
    #> 302       54            age      28
    #> 303       55            age      29
    #> 304       56            age      36
    #> 305       57            age      28
    #> 306       58            age      28
    #> 307       59            age      28
    #> 308       60            age      27
    #> 309       61            age      35
    #> 310       62            age      25
    #> 311       63            age      34
    #> 312       64            age      31
    #> 313       65            age      26
    #> 314       66            age      32
    #> 315       67            age      21
    #> 316       68            age      28
    #> 317       69            age      37
    #> 318       70            age      25
    #> 319       71            age      32
    #> 320       72            age      25
    #> 321       73            age      31
    #> 322       74            age      38
    #> 323       75            age      26
    #> 324       76            age      31
    #> 325       77            age      31
    #> 326       78            age      25
    #> 327       79            age      31
    #> 328       80            age      34
    #> 329       81            age      35
    #> 330       82            age      29
    #> 331       83            age      23
    #> 332       84            age      26
    #> 333       85            age      42
    #> 334       86            age      39
    #> 335       87            age      34
    #> 336       88            age      35
    #> 337       89            age      36
    #> 338       90            age      23
    #> 339       91            age      32
    #> 340       92            age      21
    #> 341       93            age      28
    #> 342       94            age      29
    #> 343       95            age      37
    #> 344       96            age      31
    #> 345       97            age      29
    #> 346       98            age      31
    #> 347       99            age      27
    #> 348      100            age      30
    #> 349      101            age      26
    #> 350      102            age      25
    #> 351      103            age      44
    #> 352      104            age      40
    #> 353      105            age      35
    #> 354      106            age      28
    #> 355      107            age      36
    #> 356      108            age      27
    #> 357      109            age      40
    #> 358      110            age      38
    #> 359      111            age      34
    #> 360      112            age      28
    #> 361      113            age      30
    #> 362      114            age      32
    #> 363      115            age      34
    #> 364      116            age      42
    #> 365      117            age      32
    #> 366      118            age      39
    #> 367      119            age      35
    #> 368      120            age      36
    #> 369      121            age      34
    #> 370      122            age      30
    #> 371      123            age      28
    #> 372      124            age      39
    #> 373      125            age      35
    #> 374      126            age      41
    #> 375      127            age      37
    #> 376      128            age      30
    #> 377      129            age      37
    #> 378      130            age      28
    #> 379      131            age      27
    #> 380      132            age      26
    #> 381      133            age      38
    #> 382      134            age      24
    #> 383      135            age      36
    #> 384      136            age      27
    #> 385      137            age      28
    #> 386      138            age      29
    #> 387      139            age      36
    #> 388      140            age      28
    #> 389      141            age      28
    #> 390      142            age      28
    #> 391      143            age      27
    #> 392      144            age      35
    #> 393      145            age      25
    #> 394      146            age      34
    #> 395      147            age      31
    #> 396      148            age      26
    #> 397      149            age      32
    #> 398      150            age      21
    #> 399      151            age      28
    #> 400      152            age      37
    #> 401      153            age      25
    #> 402      154            age      32
    #> 403      155            age      25
    #> 404      156            age      31
    #> 405      157            age      26
    #> 406      158            age      31
    #> 407      159            age      31
    #> 408      160            age      25
    #> 409      161            age      31
    #> 410      162            age      34
    #> 411      163            age      35
    #> 412      164            age      29
    #> 413      165            age      23
    #> 414      166            age      26
    #> 415      167            age      42
    #> 416      168            age      39
    #> 417      169            age      34
    #> 418      170            age      35
    #> 419      171            age      36
    #> 420      172            age      23
    #> 421      173            age      32
    #> 422      174            age      21
    #> 423      175            age      28
    #> 424      176            age      29
    #> 425      177            age      37
    #> 426      178            age      31
    #> 427      179            age      29
    #> 428      180            age      31
    #> 429      181            age      27
    #> 430      182            age      30
    #> 431      183            age      26
    #> 432      184            age      25
    #> 433      185            age      44
    #> 434      186            age      40
    #> 435      187            age      35
    #> 436      188            age      28
    #> 437      189            age      36
    #> 438      190            age      27
    #> 439      191            age      40
    #> 440      192            age      38
    #> 441      193            age      34
    #> 442      194            age      28
    #> 443      195            age      30
    #> 444      196            age      32
    #> 445      197            age      34
    #> 446      198            age      42
    #> 447      199            age      32
    #> 448      200            age      39
    #> 449      201            age      35
    #> 450      202            age      36
    #> 451      203            age      34
    #> 452      204            age      30
    #> 453      205            age      28
    #> 454      206            age      39
    #> 455      207            age      35
    #> 456      208            age      41
    #> 457      209            age      37
    #> 458      210            age      30
    #> 459      211            age      37
    #> 460      212            age      28
    #> 461      213            age      27
    #> 462      214            age      26
    #> 463      215            age      38
    #> 464      216            age      24
    #> 465      217            age      36
    #> 466      218            age      27
    #> 467      219            age      28
    #> 468      220            age      29
    #> 469      221            age      36
    #> 470      222            age      28
    #> 471      223            age      28
    #> 472      224            age      28
    #> 473      225            age      27
    #> 474      226            age      35
    #> 475      227            age      25
    #> 476      228            age      34
    #> 477      229            age      31
    #> 478      230            age      26
    #> 479      231            age      32
    #> 480      232            age      21
    #> 481      233            age      28
    #> 482      234            age      37
    #> 483      235            age      25
    #> 484      236            age      32
    #> 485      237            age      25
    #> 486      238            age      31
    #> 487      239            age      38
    #> 488      240            age      26
    #> 489      241            age      31
    #> 490      242            age      31
    #> 491      243            age      25
    #> 492      244            age      31
    #> 493      245            age      34
    #> 494      246            age      35
    #> 495      247            age      29
    #> 496      248            age      23
    #> 497        1         parity       6
    #> 498        2         parity       1
    #> 499        3         parity       6
    #> 500        4         parity       4
    #> 501        5         parity       3
    #> 502        6         parity       4
    #> 503        7         parity       1
    #> 504        8         parity       2
    #> 505        9         parity       1
    #> 506       10         parity       2
    #> 507       11         parity       2
    #> 508       12         parity       4
    #> 509       13         parity       1
    #> 510       14         parity       3
    #> 511       15         parity       2
    #> 512       16         parity       2
    #> 513       17         parity       5
    #> 514       18         parity       1
    #> 515       19         parity       3
    #> 516       20         parity       1
    #> 517       21         parity       1
    #> 518       22         parity       2
    #> 519       23         parity       2
    #> 520       24         parity       1
    #> 521       25         parity       2
    #> 522       26         parity       2
    #> 523       27         parity       2
    #> 524       28         parity       3
    #> 525       29         parity       4
    #> 526       30         parity       4
    #> 527       31         parity       1
    #> 528       32         parity       2
    #> 529       33         parity       1
    #> 530       34         parity       2
    #> 531       35         parity       1
    #> 532       36         parity       2
    #> 533       37         parity       1
    #> 534       38         parity       3
    #> 535       39         parity       3
    #> 536       40         parity       1
    #> 537       41         parity       3
    #> 538       42         parity       1
    #> 539       43         parity       1
    #> 540       44         parity       2
    #> 541       45         parity       1
    #> 542       46         parity       1
    #> 543       47         parity       2
    #> 544       48         parity       4
    #> 545       49         parity       2
    #> 546       50         parity       3
    #> 547       51         parity       3
    #> 548       52         parity       5
    #> 549       53         parity       3
    #> 550       54         parity       1
    #> 551       55         parity       2
    #> 552       56         parity       2
    #> 553       57         parity       2
    #> 554       58         parity       2
    #> 555       59         parity       1
    #> 556       60         parity       2
    #> 557       61         parity       2
    #> 558       62         parity       1
    #> 559       63         parity       1
    #> 560       64         parity       2
    #> 561       65         parity       2
    #> 562       66         parity       1
    #> 563       67         parity       1
    #> 564       68         parity       3
    #> 565       69         parity       3
    #> 566       70         parity       1
    #> 567       71         parity       1
    #> 568       72         parity       1
    #> 569       73         parity       1
    #> 570       74         parity       6
    #> 571       75         parity       2
    #> 572       76         parity       1
    #> 573       77         parity       2
    #> 574       78         parity       1
    #> 575       79         parity       1
    #> 576       80         parity       1
    #> 577       81         parity       2
    #> 578       82         parity       1
    #> 579       83         parity       1
    #> 580       84         parity       6
    #> 581       85         parity       1
    #> 582       86         parity       6
    #> 583       87         parity       4
    #> 584       88         parity       3
    #> 585       89         parity       4
    #> 586       90         parity       1
    #> 587       91         parity       2
    #> 588       92         parity       1
    #> 589       93         parity       2
    #> 590       94         parity       2
    #> 591       95         parity       4
    #> 592       96         parity       1
    #> 593       97         parity       3
    #> 594       98         parity       2
    #> 595       99         parity       2
    #> 596      100         parity       5
    #> 597      101         parity       1
    #> 598      102         parity       3
    #> 599      103         parity       1
    #> 600      104         parity       1
    #> 601      105         parity       2
    #> 602      106         parity       2
    #> 603      107         parity       1
    #> 604      108         parity       2
    #> 605      109         parity       2
    #> 606      110         parity       2
    #> 607      111         parity       3
    #> 608      112         parity       4
    #> 609      113         parity       4
    #> 610      114         parity       1
    #> 611      115         parity       2
    #> 612      116         parity       1
    #> 613      117         parity       2
    #> 614      118         parity       1
    #> 615      119         parity       2
    #> 616      120         parity       1
    #> 617      121         parity       3
    #> 618      122         parity       3
    #> 619      123         parity       1
    #> 620      124         parity       3
    #> 621      125         parity       1
    #> 622      126         parity       1
    #> 623      127         parity       2
    #> 624      128         parity       1
    #> 625      129         parity       1
    #> 626      130         parity       2
    #> 627      131         parity       4
    #> 628      132         parity       2
    #> 629      133         parity       3
    #> 630      134         parity       3
    #> 631      135         parity       5
    #> 632      136         parity       3
    #> 633      137         parity       1
    #> 634      138         parity       2
    #> 635      139         parity       2
    #> 636      140         parity       2
    #> 637      141         parity       2
    #> 638      142         parity       1
    #> 639      143         parity       2
    #> 640      144         parity       2
    #> 641      145         parity       1
    #> 642      146         parity       1
    #> 643      147         parity       2
    #> 644      148         parity       2
    #> 645      149         parity       1
    #> 646      150         parity       1
    #> 647      151         parity       3
    #> 648      152         parity       3
    #> 649      153         parity       1
    #> 650      154         parity       1
    #> 651      155         parity       1
    #> 652      156         parity       1
    #> 653      157         parity       2
    #> 654      158         parity       1
    #> 655      159         parity       2
    #> 656      160         parity       1
    #> 657      161         parity       1
    #> 658      162         parity       1
    #> 659      163         parity       2
    #> 660      164         parity       1
    #> 661      165         parity       1
    #> 662      166         parity       6
    #> 663      167         parity       1
    #> 664      168         parity       6
    #> 665      169         parity       4
    #> 666      170         parity       3
    #> 667      171         parity       4
    #> 668      172         parity       1
    #> 669      173         parity       2
    #> 670      174         parity       1
    #> 671      175         parity       2
    #> 672      176         parity       2
    #> 673      177         parity       4
    #> 674      178         parity       1
    #> 675      179         parity       3
    #> 676      180         parity       2
    #> 677      181         parity       2
    #> 678      182         parity       5
    #> 679      183         parity       1
    #> 680      184         parity       3
    #> 681      185         parity       1
    #> 682      186         parity       1
    #> 683      187         parity       2
    #> 684      188         parity       2
    #> 685      189         parity       1
    #> 686      190         parity       2
    #> 687      191         parity       2
    #> 688      192         parity       2
    #> 689      193         parity       3
    #> 690      194         parity       4
    #> 691      195         parity       4
    #> 692      196         parity       1
    #> 693      197         parity       2
    #> 694      198         parity       1
    #> 695      199         parity       2
    #> 696      200         parity       1
    #> 697      201         parity       2
    #> 698      202         parity       1
    #> 699      203         parity       3
    #> 700      204         parity       3
    #> 701      205         parity       1
    #> 702      206         parity       3
    #> 703      207         parity       1
    #> 704      208         parity       1
    #> 705      209         parity       2
    #> 706      210         parity       1
    #> 707      211         parity       1
    #> 708      212         parity       2
    #> 709      213         parity       4
    #> 710      214         parity       2
    #> 711      215         parity       3
    #> 712      216         parity       3
    #> 713      217         parity       5
    #> 714      218         parity       3
    #> 715      219         parity       1
    #> 716      220         parity       2
    #> 717      221         parity       2
    #> 718      222         parity       2
    #> 719      223         parity       2
    #> 720      224         parity       1
    #> 721      225         parity       2
    #> 722      226         parity       2
    #> 723      227         parity       1
    #> 724      228         parity       1
    #> 725      229         parity       2
    #> 726      230         parity       2
    #> 727      231         parity       1
    #> 728      232         parity       1
    #> 729      233         parity       3
    #> 730      234         parity       3
    #> 731      235         parity       1
    #> 732      236         parity       1
    #> 733      237         parity       1
    #> 734      238         parity       1
    #> 735      239         parity       6
    #> 736      240         parity       2
    #> 737      241         parity       1
    #> 738      242         parity       2
    #> 739      243         parity       1
    #> 740      244         parity       1
    #> 741      245         parity       1
    #> 742      246         parity       2
    #> 743      247         parity       1
    #> 744      248         parity       1
    #> 745        1        induced       1
    #> 746        2        induced       1
    #> 747        3        induced       2
    #> 748        4        induced       2
    #> 749        5        induced       1
    #> 750        6        induced       2
    #> 751        7        induced       0
    #> 752        8        induced       0
    #> 753        9        induced       0
    #> 754       10        induced       0
    #> 755       11        induced       1
    #> 756       12        induced       2
    #> 757       13        induced       1
    #> 758       14        induced       2
    #> 759       15        induced       1
    #> 760       16        induced       2
    #> 761       17        induced       2
    #> 762       18        induced       0
    #> 763       19        induced       2
    #> 764       20        induced       0
    #> 765       21        induced       0
    #> 766       22        induced       2
    #> 767       23        induced       0
    #> 768       24        induced       0
    #> 769       25        induced       1
    #> 770       26        induced       0
    #> 771       27        induced       0
    #> 772       28        induced       0
    #> 773       29        induced       1
    #> 774       30        induced       2
    #> 775       31        induced       0
    #> 776       32        induced       1
    #> 777       33        induced       1
    #> 778       34        induced       0
    #> 779       35        induced       1
    #> 780       36        induced       0
    #> 781       37        induced       0
    #> 782       38        induced       1
    #> 783       39        induced       0
    #> 784       40        induced       0
    #> 785       41        induced       0
    #> 786       42        induced       0
    #> 787       43        induced       0
    #> 788       44        induced       1
    #> 789       45        induced       0
    #> 790       46        induced       1
    #> 791       47        induced       0
    #> 792       48        induced       2
    #> 793       49        induced       2
    #> 794       50        induced       0
    #> 795       51        induced       1
    #> 796       52        induced       1
    #> 797       53        induced       1
    #> 798       54        induced       0
    #> 799       55        induced       0
    #> 800       56        induced       0
    #> 801       57        induced       1
    #> 802       58        induced       0
    #> 803       59        induced       0
    #> 804       60        induced       0
    #> 805       61        induced       0
    #> 806       62        induced       0
    #> 807       63        induced       0
    #> 808       64        induced       0
    #> 809       65        induced       1
    #> 810       66        induced       0
    #> 811       67        induced       0
    #> 812       68        induced       1
    #> 813       69        induced       0
    #> 814       70        induced       1
    #> 815       71        induced       1
    #> 816       72        induced       0
    #> 817       73        induced       0
    #> 818       74        induced       0
    #> 819       75        induced       0
    #> 820       76        induced       0
    #> 821       77        induced       0
    #> 822       78        induced       1
    #> 823       79        induced       0
    #> 824       80        induced       0
    #> 825       81        induced       2
    #> 826       82        induced       0
    #> 827       83        induced       0
    #> 828       84        induced       2
    #> 829       85        induced       0
    #> 830       86        induced       2
    #> 831       87        induced       0
    #> 832       88        induced       2
    #> 833       89        induced       1
    #> 834       90        induced       0
    #> 835       91        induced       2
    #> 836       92        induced       0
    #> 837       93        induced       0
    #> 838       94        induced       0
    #> 839       95        induced       1
    #> 840       96        induced       0
    #> 841       97        induced       0
    #> 842       98        induced       1
    #> 843       99        induced       1
    #> 844      100        induced       0
    #> 845      101        induced       0
    #> 846      102        induced       0
    #> 847      103        induced       0
    #> 848      104        induced       0
    #> 849      105        induced       0
    #> 850      106        induced       0
    #> 851      107        induced       0
    #> 852      108        induced       0
    #> 853      109        induced       0
    #> 854      110        induced       0
    #> 855      111        induced       0
    #> 856      112        induced       0
    #> 857      113        induced       1
    #> 858      114        induced       0
    #> 859      115        induced       1
    #> 860      116        induced       1
    #> 861      117        induced       0
    #> 862      118        induced       0
    #> 863      119        induced       0
    #> 864      120        induced       0
    #> 865      121        induced       2
    #> 866      122        induced       0
    #> 867      123        induced       1
    #> 868      124        induced       1
    #> 869      125        induced       0
    #> 870      126        induced       0
    #> 871      127        induced       0
    #> 872      128        induced       1
    #> 873      129        induced       0
    #> 874      130        induced       1
    #> 875      131        induced       2
    #> 876      132        induced       1
    #> 877      133        induced       1
    #> 878      134        induced       2
    #> 879      135        induced       1
    #> 880      136        induced       1
    #> 881      137        induced       1
    #> 882      138        induced       1
    #> 883      139        induced       1
    #> 884      140        induced       1
    #> 885      141        induced       2
    #> 886      142        induced       1
    #> 887      143        induced       1
    #> 888      144        induced       2
    #> 889      145        induced       1
    #> 890      146        induced       0
    #> 891      147        induced       0
    #> 892      148        induced       0
    #> 893      149        induced       0
    #> 894      150        induced       0
    #> 895      151        induced       2
    #> 896      152        induced       1
    #> 897      153        induced       0
    #> 898      154        induced       1
    #> 899      155        induced       0
    #> 900      156        induced       0
    #> 901      157        induced       0
    #> 902      158        induced       0
    #> 903      159        induced       2
    #> 904      160        induced       0
    #> 905      161        induced       0
    #> 906      162        induced       0
    #> 907      163        induced       0
    #> 908      164        induced       0
    #> 909      165        induced       0
    #> 910      166        induced       2
    #> 911      167        induced       0
    #> 912      168        induced       2
    #> 913      169        induced       0
    #> 914      170        induced       0
    #> 915      171        induced       0
    #> 916      172        induced       0
    #> 917      173        induced       0
    #> 918      174        induced       1
    #> 919      175        induced       0
    #> 920      176        induced       0
    #> 921      177        induced       0
    #> 922      178        induced       0
    #> 923      179        induced       0
    #> 924      180        induced       1
    #> 925      181        induced       0
    #> 926      182        induced       1
    #> 927      183        induced       1
    #> 928      184        induced       1
    #> 929      185        induced       1
    #> 930      186        induced       0
    #> 931      187        induced       0
    #> 932      188        induced       2
    #> 933      189        induced       0
    #> 934      190        induced       0
    #> 935      191        induced       0
    #> 936      192        induced       0
    #> 937      193        induced       0
    #> 938      194        induced       2
    #> 939      195        induced       1
    #> 940      196        induced       0
    #> 941      197        induced       0
    #> 942      198        induced       0
    #> 943      199        induced       2
    #> 944      200        induced       0
    #> 945      201        induced       0
    #> 946      202        induced       0
    #> 947      203        induced       2
    #> 948      204        induced       0
    #> 949      205        induced       0
    #> 950      206        induced       0
    #> 951      207        induced       0
    #> 952      208        induced       0
    #> 953      209        induced       0
    #> 954      210        induced       0
    #> 955      211        induced       0
    #> 956      212        induced       1
    #> 957      213        induced       2
    #> 958      214        induced       1
    #> 959      215        induced       1
    #> 960      216        induced       2
    #> 961      217        induced       2
    #> 962      218        induced       2
    #> 963      219        induced       0
    #> 964      220        induced       1
    #> 965      221        induced       0
    #> 966      222        induced       2
    #> 967      223        induced       1
    #> 968      224        induced       0
    #> 969      225        induced       1
    #> 970      226        induced       1
    #> 971      227        induced       1
    #> 972      228        induced       0
    #> 973      229        induced       1
    #> 974      230        induced       0
    #> 975      231        induced       1
    #> 976      232        induced       0
    #> 977      233        induced       2
    #> 978      234        induced       0
    #> 979      235        induced       1
    #> 980      236        induced       0
    #> 981      237        induced       1
    #> 982      238        induced       0
    #> 983      239        induced       0
    #> 984      240        induced       1
    #> 985      241        induced       1
    #> 986      242        induced       0
    #> 987      243        induced       0
    #> 988      244        induced       0
    #> 989      245        induced       0
    #> 990      246        induced       2
    #> 991      247        induced       0
    #> 992      248        induced       0
    #> 993        1           case       1
    #> 994        2           case       1
    #> 995        3           case       1
    #> 996        4           case       1
    #> 997        5           case       1
    #> 998        6           case       1
    #> 999        7           case       1
    #> 1000       8           case       1
    #> 1001       9           case       1
    #> 1002      10           case       1
    #> 1003      11           case       1
    #> 1004      12           case       1
    #> 1005      13           case       1
    #> 1006      14           case       1
    #> 1007      15           case       1
    #> 1008      16           case       1
    #> 1009      17           case       1
    #> 1010      18           case       1
    #> 1011      19           case       1
    #> 1012      20           case       1
    #> 1013      21           case       1
    #> 1014      22           case       1
    #> 1015      23           case       1
    #> 1016      24           case       1
    #> 1017      25           case       1
    #> 1018      26           case       1
    #> 1019      27           case       1
    #> 1020      28           case       1
    #> 1021      29           case       1
    #> 1022      30           case       1
    #> 1023      31           case       1
    #> 1024      32           case       1
    #> 1025      33           case       1
    #> 1026      34           case       1
    #> 1027      35           case       1
    #> 1028      36           case       1
    #> 1029      37           case       1
    #> 1030      38           case       1
    #> 1031      39           case       1
    #> 1032      40           case       1
    #> 1033      41           case       1
    #> 1034      42           case       1
    #> 1035      43           case       1
    #> 1036      44           case       1
    #> 1037      45           case       1
    #> 1038      46           case       1
    #> 1039      47           case       1
    #> 1040      48           case       1
    #> 1041      49           case       1
    #> 1042      50           case       1
    #> 1043      51           case       1
    #> 1044      52           case       1
    #> 1045      53           case       1
    #> 1046      54           case       1
    #> 1047      55           case       1
    #> 1048      56           case       1
    #> 1049      57           case       1
    #> 1050      58           case       1
    #> 1051      59           case       1
    #> 1052      60           case       1
    #> 1053      61           case       1
    #> 1054      62           case       1
    #> 1055      63           case       1
    #> 1056      64           case       1
    #> 1057      65           case       1
    #> 1058      66           case       1
    #> 1059      67           case       1
    #> 1060      68           case       1
    #> 1061      69           case       1
    #> 1062      70           case       1
    #> 1063      71           case       1
    #> 1064      72           case       1
    #> 1065      73           case       1
    #> 1066      74           case       1
    #> 1067      75           case       1
    #> 1068      76           case       1
    #> 1069      77           case       1
    #> 1070      78           case       1
    #> 1071      79           case       1
    #> 1072      80           case       1
    #> 1073      81           case       1
    #> 1074      82           case       1
    #> 1075      83           case       1
    #> 1076      84           case       0
    #> 1077      85           case       0
    #> 1078      86           case       0
    #> 1079      87           case       0
    #> 1080      88           case       0
    #> 1081      89           case       0
    #> 1082      90           case       0
    #> 1083      91           case       0
    #> 1084      92           case       0
    #> 1085      93           case       0
    #> 1086      94           case       0
    #> 1087      95           case       0
    #> 1088      96           case       0
    #> 1089      97           case       0
    #> 1090      98           case       0
    #> 1091      99           case       0
    #> 1092     100           case       0
    #> 1093     101           case       0
    #> 1094     102           case       0
    #> 1095     103           case       0
    #> 1096     104           case       0
    #> 1097     105           case       0
    #> 1098     106           case       0
    #> 1099     107           case       0
    #> 1100     108           case       0
    #> 1101     109           case       0
    #> 1102     110           case       0
    #> 1103     111           case       0
    #> 1104     112           case       0
    #> 1105     113           case       0
    #> 1106     114           case       0
    #> 1107     115           case       0
    #> 1108     116           case       0
    #> 1109     117           case       0
    #> 1110     118           case       0
    #> 1111     119           case       0
    #> 1112     120           case       0
    #> 1113     121           case       0
    #> 1114     122           case       0
    #> 1115     123           case       0
    #> 1116     124           case       0
    #> 1117     125           case       0
    #> 1118     126           case       0
    #> 1119     127           case       0
    #> 1120     128           case       0
    #> 1121     129           case       0
    #> 1122     130           case       0
    #> 1123     131           case       0
    #> 1124     132           case       0
    #> 1125     133           case       0
    #> 1126     134           case       0
    #> 1127     135           case       0
    #> 1128     136           case       0
    #> 1129     137           case       0
    #> 1130     138           case       0
    #> 1131     139           case       0
    #> 1132     140           case       0
    #> 1133     141           case       0
    #> 1134     142           case       0
    #> 1135     143           case       0
    #> 1136     144           case       0
    #> 1137     145           case       0
    #> 1138     146           case       0
    #> 1139     147           case       0
    #> 1140     148           case       0
    #> 1141     149           case       0
    #> 1142     150           case       0
    #> 1143     151           case       0
    #> 1144     152           case       0
    #> 1145     153           case       0
    #> 1146     154           case       0
    #> 1147     155           case       0
    #> 1148     156           case       0
    #> 1149     157           case       0
    #> 1150     158           case       0
    #> 1151     159           case       0
    #> 1152     160           case       0
    #> 1153     161           case       0
    #> 1154     162           case       0
    #> 1155     163           case       0
    #> 1156     164           case       0
    #> 1157     165           case       0
    #> 1158     166           case       0
    #> 1159     167           case       0
    #> 1160     168           case       0
    #> 1161     169           case       0
    #> 1162     170           case       0
    #> 1163     171           case       0
    #> 1164     172           case       0
    #> 1165     173           case       0
    #> 1166     174           case       0
    #> 1167     175           case       0
    #> 1168     176           case       0
    #> 1169     177           case       0
    #> 1170     178           case       0
    #> 1171     179           case       0
    #> 1172     180           case       0
    #> 1173     181           case       0
    #> 1174     182           case       0
    #> 1175     183           case       0
    #> 1176     184           case       0
    #> 1177     185           case       0
    #> 1178     186           case       0
    #> 1179     187           case       0
    #> 1180     188           case       0
    #> 1181     189           case       0
    #> 1182     190           case       0
    #> 1183     191           case       0
    #> 1184     192           case       0
    #> 1185     193           case       0
    #> 1186     194           case       0
    #> 1187     195           case       0
    #> 1188     196           case       0
    #> 1189     197           case       0
    #> 1190     198           case       0
    #> 1191     199           case       0
    #> 1192     200           case       0
    #> 1193     201           case       0
    #> 1194     202           case       0
    #> 1195     203           case       0
    #> 1196     204           case       0
    #> 1197     205           case       0
    #> 1198     206           case       0
    #> 1199     207           case       0
    #> 1200     208           case       0
    #> 1201     209           case       0
    #> 1202     210           case       0
    #> 1203     211           case       0
    #> 1204     212           case       0
    #> 1205     213           case       0
    #> 1206     214           case       0
    #> 1207     215           case       0
    #> 1208     216           case       0
    #> 1209     217           case       0
    #> 1210     218           case       0
    #> 1211     219           case       0
    #> 1212     220           case       0
    #> 1213     221           case       0
    #> 1214     222           case       0
    #> 1215     223           case       0
    #> 1216     224           case       0
    #> 1217     225           case       0
    #> 1218     226           case       0
    #> 1219     227           case       0
    #> 1220     228           case       0
    #> 1221     229           case       0
    #> 1222     230           case       0
    #> 1223     231           case       0
    #> 1224     232           case       0
    #> 1225     233           case       0
    #> 1226     234           case       0
    #> 1227     235           case       0
    #> 1228     236           case       0
    #> 1229     237           case       0
    #> 1230     238           case       0
    #> 1231     239           case       0
    #> 1232     240           case       0
    #> 1233     241           case       0
    #> 1234     242           case       0
    #> 1235     243           case       0
    #> 1236     244           case       0
    #> 1237     245           case       0
    #> 1238     246           case       0
    #> 1239     247           case       0
    #> 1240     248           case       0
    #> 1241       1    spontaneous       2
    #> 1242       2    spontaneous       0
    #> 1243       3    spontaneous       0
    #> 1244       4    spontaneous       0
    #> 1245       5    spontaneous       1
    #> 1246       6    spontaneous       1
    #> 1247       7    spontaneous       0
    #> 1248       8    spontaneous       0
    #> 1249       9    spontaneous       1
    #> 1250      10    spontaneous       0
    #> 1251      11    spontaneous       0
    #> 1252      12    spontaneous       1
    #> 1253      13    spontaneous       0
    #> 1254      14    spontaneous       0
    #> 1255      15    spontaneous       1
    #> 1256      16    spontaneous       0
    #> 1257      17    spontaneous       1
    #> 1258      18    spontaneous       1
    #> 1259      19    spontaneous       1
    #> 1260      20    spontaneous       1
    #> 1261      21    spontaneous       1
    #> 1262      22    spontaneous       0
    #> 1263      23    spontaneous       2
    #> 1264      24    spontaneous       1
    #> 1265      25    spontaneous       1
    #> 1266      26    spontaneous       2
    #> 1267      27    spontaneous       2
    #> 1268      28    spontaneous       2
    #> 1269      29    spontaneous       2
    #> 1270      30    spontaneous       0
    #> 1271      31    spontaneous       1
    #> 1272      32    spontaneous       0
    #> 1273      33    spontaneous       0
    #> 1274      34    spontaneous       2
    #> 1275      35    spontaneous       0
    #> 1276      36    spontaneous       2
    #> 1277      37    spontaneous       1
    #> 1278      38    spontaneous       2
    #> 1279      39    spontaneous       0
    #> 1280      40    spontaneous       1
    #> 1281      41    spontaneous       2
    #> 1282      42    spontaneous       0
    #> 1283      43    spontaneous       0
    #> 1284      44    spontaneous       1
    #> 1285      45    spontaneous       0
    #> 1286      46    spontaneous       0
    #> 1287      47    spontaneous       2
    #> 1288      48    spontaneous       0
    #> 1289      49    spontaneous       0
    #> 1290      50    spontaneous       2
    #> 1291      51    spontaneous       2
    #> 1292      52    spontaneous       2
    #> 1293      53    spontaneous       1
    #> 1294      54    spontaneous       1
    #> 1295      55    spontaneous       2
    #> 1296      56    spontaneous       2
    #> 1297      57    spontaneous       0
    #> 1298      58    spontaneous       2
    #> 1299      59    spontaneous       1
    #> 1300      60    spontaneous       2
    #> 1301      61    spontaneous       2
    #> 1302      62    spontaneous       1
    #> 1303      63    spontaneous       1
    #> 1304      64    spontaneous       2
    #> 1305      65    spontaneous       0
    #> 1306      66    spontaneous       1
    #> 1307      67    spontaneous       1
    #> 1308      68    spontaneous       2
    #> 1309      69    spontaneous       2
    #> 1310      70    spontaneous       0
    #> 1311      71    spontaneous       0
    #> 1312      72    spontaneous       1
    #> 1313      73    spontaneous       1
    #> 1314      74    spontaneous       2
    #> 1315      75    spontaneous       2
    #> 1316      76    spontaneous       1
    #> 1317      77    spontaneous       1
    #> 1318      78    spontaneous       0
    #> 1319      79    spontaneous       1
    #> 1320      80    spontaneous       1
    #> 1321      81    spontaneous       0
    #> 1322      82    spontaneous       1
    #> 1323      83    spontaneous       1
    #> 1324      84    spontaneous       0
    #> 1325      85    spontaneous       0
    #> 1326      86    spontaneous       0
    #> 1327      87    spontaneous       1
    #> 1328      88    spontaneous       0
    #> 1329      89    spontaneous       1
    #> 1330      90    spontaneous       0
    #> 1331      91    spontaneous       0
    #> 1332      92    spontaneous       1
    #> 1333      93    spontaneous       1
    #> 1334      94    spontaneous       0
    #> 1335      95    spontaneous       1
    #> 1336      96    spontaneous       0
    #> 1337      97    spontaneous       1
    #> 1338      98    spontaneous       0
    #> 1339      99    spontaneous       0
    #> 1340     100    spontaneous       2
    #> 1341     101    spontaneous       0
    #> 1342     102    spontaneous       1
    #> 1343     103    spontaneous       0
    #> 1344     104    spontaneous       0
    #> 1345     105    spontaneous       0
    #> 1346     106    spontaneous       0
    #> 1347     107    spontaneous       0
    #> 1348     108    spontaneous       1
    #> 1349     109    spontaneous       0
    #> 1350     110    spontaneous       0
    #> 1351     111    spontaneous       0
    #> 1352     112    spontaneous       2
    #> 1353     113    spontaneous       1
    #> 1354     114    spontaneous       0
    #> 1355     115    spontaneous       0
    #> 1356     116    spontaneous       0
    #> 1357     117    spontaneous       0
    #> 1358     118    spontaneous       0
    #> 1359     119    spontaneous       0
    #> 1360     120    spontaneous       0
    #> 1361     121    spontaneous       0
    #> 1362     122    spontaneous       2
    #> 1363     123    spontaneous       0
    #> 1364     124    spontaneous       0
    #> 1365     125    spontaneous       0
    #> 1366     126    spontaneous       0
    #> 1367     127    spontaneous       0
    #> 1368     128    spontaneous       0
    #> 1369     129    spontaneous       0
    #> 1370     130    spontaneous       0
    #> 1371     131    spontaneous       1
    #> 1372     132    spontaneous       0
    #> 1373     133    spontaneous       0
    #> 1374     134    spontaneous       1
    #> 1375     135    spontaneous       1
    #> 1376     136    spontaneous       1
    #> 1377     137    spontaneous       0
    #> 1378     138    spontaneous       0
    #> 1379     139    spontaneous       0
    #> 1380     140    spontaneous       1
    #> 1381     141    spontaneous       0
    #> 1382     142    spontaneous       0
    #> 1383     143    spontaneous       0
    #> 1384     144    spontaneous       0
    #> 1385     145    spontaneous       0
    #> 1386     146    spontaneous       0
    #> 1387     147    spontaneous       0
    #> 1388     148    spontaneous       1
    #> 1389     149    spontaneous       0
    #> 1390     150    spontaneous       1
    #> 1391     151    spontaneous       0
    #> 1392     152    spontaneous       1
    #> 1393     153    spontaneous       0
    #> 1394     154    spontaneous       0
    #> 1395     155    spontaneous       0
    #> 1396     156    spontaneous       1
    #> 1397     157    spontaneous       2
    #> 1398     158    spontaneous       0
    #> 1399     159    spontaneous       0
    #> 1400     160    spontaneous       0
    #> 1401     161    spontaneous       0
    #> 1402     162    spontaneous       0
    #> 1403     163    spontaneous       0
    #> 1404     164    spontaneous       1
    #> 1405     165    spontaneous       1
    #> 1406     166    spontaneous       0
    #> 1407     167    spontaneous       0
    #> 1408     168    spontaneous       0
    #> 1409     169    spontaneous       2
    #> 1410     170    spontaneous       0
    #> 1411     171    spontaneous       2
    #> 1412     172    spontaneous       0
    #> 1413     173    spontaneous       1
    #> 1414     174    spontaneous       0
    #> 1415     175    spontaneous       1
    #> 1416     176    spontaneous       1
    #> 1417     177    spontaneous       1
    #> 1418     178    spontaneous       0
    #> 1419     179    spontaneous       2
    #> 1420     180    spontaneous       0
    #> 1421     181    spontaneous       0
    #> 1422     182    spontaneous       2
    #> 1423     183    spontaneous       0
    #> 1424     184    spontaneous       1
    #> 1425     185    spontaneous       0
    #> 1426     186    spontaneous       0
    #> 1427     187    spontaneous       0
    #> 1428     188    spontaneous       0
    #> 1429     189    spontaneous       1
    #> 1430     190    spontaneous       2
    #> 1431     191    spontaneous       0
    #> 1432     192    spontaneous       0
    #> 1433     193    spontaneous       0
    #> 1434     194    spontaneous       1
    #> 1435     195    spontaneous       1
    #> 1436     196    spontaneous       0
    #> 1437     197    spontaneous       0
    #> 1438     198    spontaneous       0
    #> 1439     199    spontaneous       0
    #> 1440     200    spontaneous       0
    #> 1441     201    spontaneous       0
    #> 1442     202    spontaneous       0
    #> 1443     203    spontaneous       0
    #> 1444     204    spontaneous       1
    #> 1445     205    spontaneous       0
    #> 1446     206    spontaneous       0
    #> 1447     207    spontaneous       0
    #> 1448     208    spontaneous       0
    #> 1449     209    spontaneous       0
    #> 1450     210    spontaneous       0
    #> 1451     211    spontaneous       1
    #> 1452     212    spontaneous       0
    #> 1453     213    spontaneous       0
    #> 1454     214    spontaneous       0
    #> 1455     215    spontaneous       0
    #> 1456     216    spontaneous       0
    #> 1457     217    spontaneous       1
    #> 1458     218    spontaneous       0
    #> 1459     219    spontaneous       1
    #> 1460     220    spontaneous       1
    #> 1461     221    spontaneous       1
    #> 1462     222    spontaneous       0
    #> 1463     223    spontaneous       0
    #> 1464     224    spontaneous       0
    #> 1465     225    spontaneous       0
    #> 1466     226    spontaneous       0
    #> 1467     227    spontaneous       0
    #> 1468     228    spontaneous       0
    #> 1469     229    spontaneous       0
    #> 1470     230    spontaneous       2
    #> 1471     231    spontaneous       0
    #> 1472     232    spontaneous       0
    #> 1473     233    spontaneous       0
    #> 1474     234    spontaneous       2
    #> 1475     235    spontaneous       0
    #> 1476     236    spontaneous       0
    #> 1477     237    spontaneous       0
    #> 1478     238    spontaneous       0
    #> 1479     239    spontaneous       2
    #> 1480     240    spontaneous       1
    #> 1481     241    spontaneous       0
    #> 1482     242    spontaneous       1
    #> 1483     243    spontaneous       1
    #> 1484     244    spontaneous       1
    #> 1485     245    spontaneous       0
    #> 1486     246    spontaneous       0
    #> 1487     247    spontaneous       1
    #> 1488     248    spontaneous       1
    #> 1489       1        stratum       1
    #> 1490       2        stratum       2
    #> 1491       3        stratum       3
    #> 1492       4        stratum       4
    #> 1493       5        stratum       5
    #> 1494       6        stratum       6
    #> 1495       7        stratum       7
    #> 1496       8        stratum       8
    #> 1497       9        stratum       9
    #> 1498      10        stratum      10
    #> 1499      11        stratum      11
    #> 1500      12        stratum      12
    #> 1501      13        stratum      13
    #> 1502      14        stratum      14
    #> 1503      15        stratum      15
    #> 1504      16        stratum      16
    #> 1505      17        stratum      17
    #> 1506      18        stratum      18
    #> 1507      19        stratum      19
    #> 1508      20        stratum      20
    #> 1509      21        stratum      21
    #> 1510      22        stratum      22
    #> 1511      23        stratum      23
    #> 1512      24        stratum      24
    #> 1513      25        stratum      25
    #> 1514      26        stratum      26
    #> 1515      27        stratum      27
    #> 1516      28        stratum      28
    #> 1517      29        stratum      29
    #> 1518      30        stratum      30
    #> 1519      31        stratum      31
    #> 1520      32        stratum      32
    #> 1521      33        stratum      33
    #> 1522      34        stratum      34
    #> 1523      35        stratum      35
    #> 1524      36        stratum      36
    #> 1525      37        stratum      37
    #> 1526      38        stratum      38
    #> 1527      39        stratum      39
    #> 1528      40        stratum      40
    #> 1529      41        stratum      41
    #> 1530      42        stratum      42
    #> 1531      43        stratum      43
    #> 1532      44        stratum      44
    #> 1533      45        stratum      45
    #> 1534      46        stratum      46
    #> 1535      47        stratum      47
    #> 1536      48        stratum      48
    #> 1537      49        stratum      49
    #> 1538      50        stratum      50
    #> 1539      51        stratum      51
    #> 1540      52        stratum      52
    #> 1541      53        stratum      53
    #> 1542      54        stratum      54
    #> 1543      55        stratum      55
    #> 1544      56        stratum      56
    #> 1545      57        stratum      57
    #> 1546      58        stratum      58
    #> 1547      59        stratum      59
    #> 1548      60        stratum      60
    #> 1549      61        stratum      61
    #> 1550      62        stratum      62
    #> 1551      63        stratum      63
    #> 1552      64        stratum      64
    #> 1553      65        stratum      65
    #> 1554      66        stratum      66
    #> 1555      67        stratum      67
    #> 1556      68        stratum      68
    #> 1557      69        stratum      69
    #> 1558      70        stratum      70
    #> 1559      71        stratum      71
    #> 1560      72        stratum      72
    #> 1561      73        stratum      73
    #> 1562      74        stratum      74
    #> 1563      75        stratum      75
    #> 1564      76        stratum      76
    #> 1565      77        stratum      77
    #> 1566      78        stratum      78
    #> 1567      79        stratum      79
    #> 1568      80        stratum      80
    #> 1569      81        stratum      81
    #> 1570      82        stratum      82
    #> 1571      83        stratum      83
    #> 1572      84        stratum       1
    #> 1573      85        stratum       2
    #> 1574      86        stratum       3
    #> 1575      87        stratum       4
    #> 1576      88        stratum       5
    #> 1577      89        stratum       6
    #> 1578      90        stratum       7
    #> 1579      91        stratum       8
    #> 1580      92        stratum       9
    #> 1581      93        stratum      10
    #> 1582      94        stratum      11
    #> 1583      95        stratum      12
    #> 1584      96        stratum      13
    #> 1585      97        stratum      14
    #> 1586      98        stratum      15
    #> 1587      99        stratum      16
    #> 1588     100        stratum      17
    #> 1589     101        stratum      18
    #> 1590     102        stratum      19
    #> 1591     103        stratum      20
    #> 1592     104        stratum      21
    #> 1593     105        stratum      22
    #> 1594     106        stratum      23
    #> 1595     107        stratum      24
    #> 1596     108        stratum      25
    #> 1597     109        stratum      26
    #> 1598     110        stratum      27
    #> 1599     111        stratum      28
    #> 1600     112        stratum      29
    #> 1601     113        stratum      30
    #> 1602     114        stratum      31
    #> 1603     115        stratum      32
    #> 1604     116        stratum      33
    #> 1605     117        stratum      34
    #> 1606     118        stratum      35
    #> 1607     119        stratum      36
    #> 1608     120        stratum      37
    #> 1609     121        stratum      38
    #> 1610     122        stratum      39
    #> 1611     123        stratum      40
    #> 1612     124        stratum      41
    #> 1613     125        stratum      42
    #> 1614     126        stratum      43
    #> 1615     127        stratum      44
    #> 1616     128        stratum      45
    #> 1617     129        stratum      46
    #> 1618     130        stratum      47
    #> 1619     131        stratum      48
    #> 1620     132        stratum      49
    #> 1621     133        stratum      50
    #> 1622     134        stratum      51
    #> 1623     135        stratum      52
    #> 1624     136        stratum      53
    #> 1625     137        stratum      54
    #> 1626     138        stratum      55
    #> 1627     139        stratum      56
    #> 1628     140        stratum      57
    #> 1629     141        stratum      58
    #> 1630     142        stratum      59
    #> 1631     143        stratum      60
    #> 1632     144        stratum      61
    #> 1633     145        stratum      62
    #> 1634     146        stratum      63
    #> 1635     147        stratum      64
    #> 1636     148        stratum      65
    #> 1637     149        stratum      66
    #> 1638     150        stratum      67
    #> 1639     151        stratum      68
    #> 1640     152        stratum      69
    #> 1641     153        stratum      70
    #> 1642     154        stratum      71
    #> 1643     155        stratum      72
    #> 1644     156        stratum      73
    #> 1645     157        stratum      75
    #> 1646     158        stratum      76
    #> 1647     159        stratum      77
    #> 1648     160        stratum      78
    #> 1649     161        stratum      79
    #> 1650     162        stratum      80
    #> 1651     163        stratum      81
    #> 1652     164        stratum      82
    #> 1653     165        stratum      83
    #> 1654     166        stratum       1
    #> 1655     167        stratum       2
    #> 1656     168        stratum       3
    #> 1657     169        stratum       4
    #> 1658     170        stratum       5
    #> 1659     171        stratum       6
    #> 1660     172        stratum       7
    #> 1661     173        stratum       8
    #> 1662     174        stratum       9
    #> 1663     175        stratum      10
    #> 1664     176        stratum      11
    #> 1665     177        stratum      12
    #> 1666     178        stratum      13
    #> 1667     179        stratum      14
    #> 1668     180        stratum      15
    #> 1669     181        stratum      16
    #> 1670     182        stratum      17
    #> 1671     183        stratum      18
    #> 1672     184        stratum      19
    #> 1673     185        stratum      20
    #> 1674     186        stratum      21
    #> 1675     187        stratum      22
    #> 1676     188        stratum      23
    #> 1677     189        stratum      24
    #> 1678     190        stratum      25
    #> 1679     191        stratum      26
    #> 1680     192        stratum      27
    #> 1681     193        stratum      28
    #> 1682     194        stratum      29
    #> 1683     195        stratum      30
    #> 1684     196        stratum      31
    #> 1685     197        stratum      32
    #> 1686     198        stratum      33
    #> 1687     199        stratum      34
    #> 1688     200        stratum      35
    #> 1689     201        stratum      36
    #> 1690     202        stratum      37
    #> 1691     203        stratum      38
    #> 1692     204        stratum      39
    #> 1693     205        stratum      40
    #> 1694     206        stratum      41
    #> 1695     207        stratum      42
    #> 1696     208        stratum      43
    #> 1697     209        stratum      44
    #> 1698     210        stratum      45
    #> 1699     211        stratum      46
    #> 1700     212        stratum      47
    #> 1701     213        stratum      48
    #> 1702     214        stratum      49
    #> 1703     215        stratum      50
    #> 1704     216        stratum      51
    #> 1705     217        stratum      52
    #> 1706     218        stratum      53
    #> 1707     219        stratum      54
    #> 1708     220        stratum      55
    #> 1709     221        stratum      56
    #> 1710     222        stratum      57
    #> 1711     223        stratum      58
    #> 1712     224        stratum      59
    #> 1713     225        stratum      60
    #> 1714     226        stratum      61
    #> 1715     227        stratum      62
    #> 1716     228        stratum      63
    #> 1717     229        stratum      64
    #> 1718     230        stratum      65
    #> 1719     231        stratum      66
    #> 1720     232        stratum      67
    #> 1721     233        stratum      68
    #> 1722     234        stratum      69
    #> 1723     235        stratum      70
    #> 1724     236        stratum      71
    #> 1725     237        stratum      72
    #> 1726     238        stratum      73
    #> 1727     239        stratum      74
    #> 1728     240        stratum      75
    #> 1729     241        stratum      76
    #> 1730     242        stratum      77
    #> 1731     243        stratum      78
    #> 1732     244        stratum      79
    #> 1733     245        stratum      80
    #> 1734     246        stratum      81
    #> 1735     247        stratum      82
    #> 1736     248        stratum      83
    #> 1737       1 pooled.stratum       3
    #> 1738       2 pooled.stratum       1
    #> 1739       3 pooled.stratum       4
    #> 1740       4 pooled.stratum       2
    #> 1741       5 pooled.stratum      32
    #> 1742       6 pooled.stratum      36
    #> 1743       7 pooled.stratum       6
    #> 1744       8 pooled.stratum      22
    #> 1745       9 pooled.stratum       5
    #> 1746      10 pooled.stratum      19
    #> 1747      11 pooled.stratum      20
    #> 1748      12 pooled.stratum      37
    #> 1749      13 pooled.stratum       9
    #> 1750      14 pooled.stratum      29
    #> 1751      15 pooled.stratum      21
    #> 1752      16 pooled.stratum      18
    #> 1753      17 pooled.stratum      38
    #> 1754      18 pooled.stratum       7
    #> 1755      19 pooled.stratum      28
    #> 1756      20 pooled.stratum      17
    #> 1757      21 pooled.stratum      14
    #> 1758      22 pooled.stratum      24
    #> 1759      23 pooled.stratum      19
    #> 1760      24 pooled.stratum      12
    #> 1761      25 pooled.stratum      18
    #> 1762      26 pooled.stratum      27
    #> 1763      27 pooled.stratum      26
    #> 1764      28 pooled.stratum      31
    #> 1765      29 pooled.stratum      34
    #> 1766      30 pooled.stratum      35
    #> 1767      31 pooled.stratum      10
    #> 1768      32 pooled.stratum      23
    #> 1769      33 pooled.stratum      16
    #> 1770      34 pooled.stratum      22
    #> 1771      35 pooled.stratum      13
    #> 1772      36 pooled.stratum      24
    #> 1773      37 pooled.stratum      12
    #> 1774      38 pooled.stratum      31
    #> 1775      39 pooled.stratum      30
    #> 1776      40 pooled.stratum       8
    #> 1777      41 pooled.stratum      33
    #> 1778      42 pooled.stratum      11
    #> 1779      43 pooled.stratum      15
    #> 1780      44 pooled.stratum      25
    #> 1781      45 pooled.stratum      44
    #> 1782      46 pooled.stratum      48
    #> 1783      47 pooled.stratum      51
    #> 1784      48 pooled.stratum      61
    #> 1785      49 pooled.stratum      49
    #> 1786      50 pooled.stratum      60
    #> 1787      51 pooled.stratum      56
    #> 1788      52 pooled.stratum      62
    #> 1789      53 pooled.stratum      57
    #> 1790      54 pooled.stratum      42
    #> 1791      55 pooled.stratum      52
    #> 1792      56 pooled.stratum      55
    #> 1793      57 pooled.stratum      51
    #> 1794      58 pooled.stratum      51
    #> 1795      59 pooled.stratum      42
    #> 1796      60 pooled.stratum      50
    #> 1797      61 pooled.stratum      54
    #> 1798      62 pooled.stratum      41
    #> 1799      63 pooled.stratum      47
    #> 1800      64 pooled.stratum      53
    #> 1801      65 pooled.stratum      49
    #> 1802      66 pooled.stratum      46
    #> 1803      67 pooled.stratum      39
    #> 1804      68 pooled.stratum      58
    #> 1805      69 pooled.stratum      59
    #> 1806      70 pooled.stratum      41
    #> 1807      71 pooled.stratum      46
    #> 1808      72 pooled.stratum      41
    #> 1809      73 pooled.stratum      45
    #> 1810      74 pooled.stratum      63
    #> 1811      75 pooled.stratum      49
    #> 1812      76 pooled.stratum      45
    #> 1813      77 pooled.stratum      53
    #> 1814      78 pooled.stratum      41
    #> 1815      79 pooled.stratum      45
    #> 1816      80 pooled.stratum      47
    #> 1817      81 pooled.stratum      54
    #> 1818      82 pooled.stratum      43
    #> 1819      83 pooled.stratum      40
    #> 1820      84 pooled.stratum       3
    #> 1821      85 pooled.stratum       1
    #> 1822      86 pooled.stratum       4
    #> 1823      87 pooled.stratum       2
    #> 1824      88 pooled.stratum      32
    #> 1825      89 pooled.stratum      36
    #> 1826      90 pooled.stratum       6
    #> 1827      91 pooled.stratum      22
    #> 1828      92 pooled.stratum       5
    #> 1829      93 pooled.stratum      19
    #> 1830      94 pooled.stratum      20
    #> 1831      95 pooled.stratum      37
    #> 1832      96 pooled.stratum       9
    #> 1833      97 pooled.stratum      29
    #> 1834      98 pooled.stratum      21
    #> 1835      99 pooled.stratum      18
    #> 1836     100 pooled.stratum      38
    #> 1837     101 pooled.stratum       7
    #> 1838     102 pooled.stratum      28
    #> 1839     103 pooled.stratum      17
    #> 1840     104 pooled.stratum      14
    #> 1841     105 pooled.stratum      24
    #> 1842     106 pooled.stratum      19
    #> 1843     107 pooled.stratum      12
    #> 1844     108 pooled.stratum      18
    #> 1845     109 pooled.stratum      27
    #> 1846     110 pooled.stratum      26
    #> 1847     111 pooled.stratum      31
    #> 1848     112 pooled.stratum      34
    #> 1849     113 pooled.stratum      35
    #> 1850     114 pooled.stratum      10
    #> 1851     115 pooled.stratum      23
    #> 1852     116 pooled.stratum      16
    #> 1853     117 pooled.stratum      22
    #> 1854     118 pooled.stratum      13
    #> 1855     119 pooled.stratum      24
    #> 1856     120 pooled.stratum      12
    #> 1857     121 pooled.stratum      31
    #> 1858     122 pooled.stratum      30
    #> 1859     123 pooled.stratum       8
    #> 1860     124 pooled.stratum      33
    #> 1861     125 pooled.stratum      11
    #> 1862     126 pooled.stratum      15
    #> 1863     127 pooled.stratum      25
    #> 1864     128 pooled.stratum      44
    #> 1865     129 pooled.stratum      48
    #> 1866     130 pooled.stratum      51
    #> 1867     131 pooled.stratum      61
    #> 1868     132 pooled.stratum      49
    #> 1869     133 pooled.stratum      60
    #> 1870     134 pooled.stratum      56
    #> 1871     135 pooled.stratum      62
    #> 1872     136 pooled.stratum      57
    #> 1873     137 pooled.stratum      42
    #> 1874     138 pooled.stratum      52
    #> 1875     139 pooled.stratum      55
    #> 1876     140 pooled.stratum      51
    #> 1877     141 pooled.stratum      51
    #> 1878     142 pooled.stratum      42
    #> 1879     143 pooled.stratum      50
    #> 1880     144 pooled.stratum      54
    #> 1881     145 pooled.stratum      41
    #> 1882     146 pooled.stratum      47
    #> 1883     147 pooled.stratum      53
    #> 1884     148 pooled.stratum      49
    #> 1885     149 pooled.stratum      46
    #> 1886     150 pooled.stratum      39
    #> 1887     151 pooled.stratum      58
    #> 1888     152 pooled.stratum      59
    #> 1889     153 pooled.stratum      41
    #> 1890     154 pooled.stratum      46
    #> 1891     155 pooled.stratum      41
    #> 1892     156 pooled.stratum      45
    #> 1893     157 pooled.stratum      49
    #> 1894     158 pooled.stratum      45
    #> 1895     159 pooled.stratum      53
    #> 1896     160 pooled.stratum      41
    #> 1897     161 pooled.stratum      45
    #> 1898     162 pooled.stratum      47
    #> 1899     163 pooled.stratum      54
    #> 1900     164 pooled.stratum      43
    #> 1901     165 pooled.stratum      40
    #> 1902     166 pooled.stratum       3
    #> 1903     167 pooled.stratum       1
    #> 1904     168 pooled.stratum       4
    #> 1905     169 pooled.stratum       2
    #> 1906     170 pooled.stratum      32
    #> 1907     171 pooled.stratum      36
    #> 1908     172 pooled.stratum       6
    #> 1909     173 pooled.stratum      22
    #> 1910     174 pooled.stratum       5
    #> 1911     175 pooled.stratum      19
    #> 1912     176 pooled.stratum      20
    #> 1913     177 pooled.stratum      37
    #> 1914     178 pooled.stratum       9
    #> 1915     179 pooled.stratum      29
    #> 1916     180 pooled.stratum      21
    #> 1917     181 pooled.stratum      18
    #> 1918     182 pooled.stratum      38
    #> 1919     183 pooled.stratum       7
    #> 1920     184 pooled.stratum      28
    #> 1921     185 pooled.stratum      17
    #> 1922     186 pooled.stratum      14
    #> 1923     187 pooled.stratum      24
    #> 1924     188 pooled.stratum      19
    #> 1925     189 pooled.stratum      12
    #> 1926     190 pooled.stratum      18
    #> 1927     191 pooled.stratum      27
    #> 1928     192 pooled.stratum      26
    #> 1929     193 pooled.stratum      31
    #> 1930     194 pooled.stratum      34
    #> 1931     195 pooled.stratum      35
    #> 1932     196 pooled.stratum      10
    #> 1933     197 pooled.stratum      23
    #> 1934     198 pooled.stratum      16
    #> 1935     199 pooled.stratum      22
    #> 1936     200 pooled.stratum      13
    #> 1937     201 pooled.stratum      24
    #> 1938     202 pooled.stratum      12
    #> 1939     203 pooled.stratum      31
    #> 1940     204 pooled.stratum      30
    #> 1941     205 pooled.stratum       8
    #> 1942     206 pooled.stratum      33
    #> 1943     207 pooled.stratum      11
    #> 1944     208 pooled.stratum      15
    #> 1945     209 pooled.stratum      25
    #> 1946     210 pooled.stratum      44
    #> 1947     211 pooled.stratum      48
    #> 1948     212 pooled.stratum      51
    #> 1949     213 pooled.stratum      61
    #> 1950     214 pooled.stratum      49
    #> 1951     215 pooled.stratum      60
    #> 1952     216 pooled.stratum      56
    #> 1953     217 pooled.stratum      62
    #> 1954     218 pooled.stratum      57
    #> 1955     219 pooled.stratum      42
    #> 1956     220 pooled.stratum      52
    #> 1957     221 pooled.stratum      55
    #> 1958     222 pooled.stratum      51
    #> 1959     223 pooled.stratum      51
    #> 1960     224 pooled.stratum      42
    #> 1961     225 pooled.stratum      50
    #> 1962     226 pooled.stratum      54
    #> 1963     227 pooled.stratum      41
    #> 1964     228 pooled.stratum      47
    #> 1965     229 pooled.stratum      53
    #> 1966     230 pooled.stratum      49
    #> 1967     231 pooled.stratum      46
    #> 1968     232 pooled.stratum      39
    #> 1969     233 pooled.stratum      58
    #> 1970     234 pooled.stratum      59
    #> 1971     235 pooled.stratum      41
    #> 1972     236 pooled.stratum      46
    #> 1973     237 pooled.stratum      41
    #> 1974     238 pooled.stratum      45
    #> 1975     239 pooled.stratum      63
    #> 1976     240 pooled.stratum      49
    #> 1977     241 pooled.stratum      45
    #> 1978     242 pooled.stratum      53
    #> 1979     243 pooled.stratum      41
    #> 1980     244 pooled.stratum      45
    #> 1981     245 pooled.stratum      47
    #> 1982     246 pooled.stratum      54
    #> 1983     247 pooled.stratum      43
    #> 1984     248 pooled.stratum      40

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
  gather(measurement, value, -plantnum)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
iris_tidy
```

    #>     plantnum  measurement      value
    #> 1          1 Sepal.Length        5.1
    #> 2          2 Sepal.Length        4.9
    #> 3          3 Sepal.Length        4.7
    #> 4          4 Sepal.Length        4.6
    #> 5          5 Sepal.Length          5
    #> 6          6 Sepal.Length        5.4
    #> 7          7 Sepal.Length        4.6
    #> 8          8 Sepal.Length          5
    #> 9          9 Sepal.Length        4.4
    #> 10        10 Sepal.Length        4.9
    #> 11        11 Sepal.Length        5.4
    #> 12        12 Sepal.Length        4.8
    #> 13        13 Sepal.Length        4.8
    #> 14        14 Sepal.Length        4.3
    #> 15        15 Sepal.Length        5.8
    #> 16        16 Sepal.Length        5.7
    #> 17        17 Sepal.Length        5.4
    #> 18        18 Sepal.Length        5.1
    #> 19        19 Sepal.Length        5.7
    #> 20        20 Sepal.Length        5.1
    #> 21        21 Sepal.Length        5.4
    #> 22        22 Sepal.Length        5.1
    #> 23        23 Sepal.Length        4.6
    #> 24        24 Sepal.Length        5.1
    #> 25        25 Sepal.Length        4.8
    #> 26        26 Sepal.Length          5
    #> 27        27 Sepal.Length          5
    #> 28        28 Sepal.Length        5.2
    #> 29        29 Sepal.Length        5.2
    #> 30        30 Sepal.Length        4.7
    #> 31        31 Sepal.Length        4.8
    #> 32        32 Sepal.Length        5.4
    #> 33        33 Sepal.Length        5.2
    #> 34        34 Sepal.Length        5.5
    #> 35        35 Sepal.Length        4.9
    #> 36        36 Sepal.Length          5
    #> 37        37 Sepal.Length        5.5
    #> 38        38 Sepal.Length        4.9
    #> 39        39 Sepal.Length        4.4
    #> 40        40 Sepal.Length        5.1
    #> 41        41 Sepal.Length          5
    #> 42        42 Sepal.Length        4.5
    #> 43        43 Sepal.Length        4.4
    #> 44        44 Sepal.Length          5
    #> 45        45 Sepal.Length        5.1
    #> 46        46 Sepal.Length        4.8
    #> 47        47 Sepal.Length        5.1
    #> 48        48 Sepal.Length        4.6
    #> 49        49 Sepal.Length        5.3
    #> 50        50 Sepal.Length          5
    #> 51        51 Sepal.Length          7
    #> 52        52 Sepal.Length        6.4
    #> 53        53 Sepal.Length        6.9
    #> 54        54 Sepal.Length        5.5
    #> 55        55 Sepal.Length        6.5
    #> 56        56 Sepal.Length        5.7
    #> 57        57 Sepal.Length        6.3
    #> 58        58 Sepal.Length        4.9
    #> 59        59 Sepal.Length        6.6
    #> 60        60 Sepal.Length        5.2
    #> 61        61 Sepal.Length          5
    #> 62        62 Sepal.Length        5.9
    #> 63        63 Sepal.Length          6
    #> 64        64 Sepal.Length        6.1
    #> 65        65 Sepal.Length        5.6
    #> 66        66 Sepal.Length        6.7
    #> 67        67 Sepal.Length        5.6
    #> 68        68 Sepal.Length        5.8
    #> 69        69 Sepal.Length        6.2
    #> 70        70 Sepal.Length        5.6
    #> 71        71 Sepal.Length        5.9
    #> 72        72 Sepal.Length        6.1
    #> 73        73 Sepal.Length        6.3
    #> 74        74 Sepal.Length        6.1
    #> 75        75 Sepal.Length        6.4
    #> 76        76 Sepal.Length        6.6
    #> 77        77 Sepal.Length        6.8
    #> 78        78 Sepal.Length        6.7
    #> 79        79 Sepal.Length          6
    #> 80        80 Sepal.Length        5.7
    #> 81        81 Sepal.Length        5.5
    #> 82        82 Sepal.Length        5.5
    #> 83        83 Sepal.Length        5.8
    #> 84        84 Sepal.Length          6
    #> 85        85 Sepal.Length        5.4
    #> 86        86 Sepal.Length          6
    #> 87        87 Sepal.Length        6.7
    #> 88        88 Sepal.Length        6.3
    #> 89        89 Sepal.Length        5.6
    #> 90        90 Sepal.Length        5.5
    #> 91        91 Sepal.Length        5.5
    #> 92        92 Sepal.Length        6.1
    #> 93        93 Sepal.Length        5.8
    #> 94        94 Sepal.Length          5
    #> 95        95 Sepal.Length        5.6
    #> 96        96 Sepal.Length        5.7
    #> 97        97 Sepal.Length        5.7
    #> 98        98 Sepal.Length        6.2
    #> 99        99 Sepal.Length        5.1
    #> 100      100 Sepal.Length        5.7
    #> 101      101 Sepal.Length        6.3
    #> 102      102 Sepal.Length        5.8
    #> 103      103 Sepal.Length        7.1
    #> 104      104 Sepal.Length        6.3
    #> 105      105 Sepal.Length        6.5
    #> 106      106 Sepal.Length        7.6
    #> 107      107 Sepal.Length        4.9
    #> 108      108 Sepal.Length        7.3
    #> 109      109 Sepal.Length        6.7
    #> 110      110 Sepal.Length        7.2
    #> 111      111 Sepal.Length        6.5
    #> 112      112 Sepal.Length        6.4
    #> 113      113 Sepal.Length        6.8
    #> 114      114 Sepal.Length        5.7
    #> 115      115 Sepal.Length        5.8
    #> 116      116 Sepal.Length        6.4
    #> 117      117 Sepal.Length        6.5
    #> 118      118 Sepal.Length        7.7
    #> 119      119 Sepal.Length        7.7
    #> 120      120 Sepal.Length          6
    #> 121      121 Sepal.Length        6.9
    #> 122      122 Sepal.Length        5.6
    #> 123      123 Sepal.Length        7.7
    #> 124      124 Sepal.Length        6.3
    #> 125      125 Sepal.Length        6.7
    #> 126      126 Sepal.Length        7.2
    #> 127      127 Sepal.Length        6.2
    #> 128      128 Sepal.Length        6.1
    #> 129      129 Sepal.Length        6.4
    #> 130      130 Sepal.Length        7.2
    #> 131      131 Sepal.Length        7.4
    #> 132      132 Sepal.Length        7.9
    #> 133      133 Sepal.Length        6.4
    #> 134      134 Sepal.Length        6.3
    #> 135      135 Sepal.Length        6.1
    #> 136      136 Sepal.Length        7.7
    #> 137      137 Sepal.Length        6.3
    #> 138      138 Sepal.Length        6.4
    #> 139      139 Sepal.Length          6
    #> 140      140 Sepal.Length        6.9
    #> 141      141 Sepal.Length        6.7
    #> 142      142 Sepal.Length        6.9
    #> 143      143 Sepal.Length        5.8
    #> 144      144 Sepal.Length        6.8
    #> 145      145 Sepal.Length        6.7
    #> 146      146 Sepal.Length        6.7
    #> 147      147 Sepal.Length        6.3
    #> 148      148 Sepal.Length        6.5
    #> 149      149 Sepal.Length        6.2
    #> 150      150 Sepal.Length        5.9
    #> 151        1  Sepal.Width        3.5
    #> 152        2  Sepal.Width          3
    #> 153        3  Sepal.Width        3.2
    #> 154        4  Sepal.Width        3.1
    #> 155        5  Sepal.Width        3.6
    #> 156        6  Sepal.Width        3.9
    #> 157        7  Sepal.Width        3.4
    #> 158        8  Sepal.Width        3.4
    #> 159        9  Sepal.Width        2.9
    #> 160       10  Sepal.Width        3.1
    #> 161       11  Sepal.Width        3.7
    #> 162       12  Sepal.Width        3.4
    #> 163       13  Sepal.Width          3
    #> 164       14  Sepal.Width          3
    #> 165       15  Sepal.Width          4
    #> 166       16  Sepal.Width        4.4
    #> 167       17  Sepal.Width        3.9
    #> 168       18  Sepal.Width        3.5
    #> 169       19  Sepal.Width        3.8
    #> 170       20  Sepal.Width        3.8
    #> 171       21  Sepal.Width        3.4
    #> 172       22  Sepal.Width        3.7
    #> 173       23  Sepal.Width        3.6
    #> 174       24  Sepal.Width        3.3
    #> 175       25  Sepal.Width        3.4
    #> 176       26  Sepal.Width          3
    #> 177       27  Sepal.Width        3.4
    #> 178       28  Sepal.Width        3.5
    #> 179       29  Sepal.Width        3.4
    #> 180       30  Sepal.Width        3.2
    #> 181       31  Sepal.Width        3.1
    #> 182       32  Sepal.Width        3.4
    #> 183       33  Sepal.Width        4.1
    #> 184       34  Sepal.Width        4.2
    #> 185       35  Sepal.Width        3.1
    #> 186       36  Sepal.Width        3.2
    #> 187       37  Sepal.Width        3.5
    #> 188       38  Sepal.Width        3.6
    #> 189       39  Sepal.Width          3
    #> 190       40  Sepal.Width        3.4
    #> 191       41  Sepal.Width        3.5
    #> 192       42  Sepal.Width        2.3
    #> 193       43  Sepal.Width        3.2
    #> 194       44  Sepal.Width        3.5
    #> 195       45  Sepal.Width        3.8
    #> 196       46  Sepal.Width          3
    #> 197       47  Sepal.Width        3.8
    #> 198       48  Sepal.Width        3.2
    #> 199       49  Sepal.Width        3.7
    #> 200       50  Sepal.Width        3.3
    #> 201       51  Sepal.Width        3.2
    #> 202       52  Sepal.Width        3.2
    #> 203       53  Sepal.Width        3.1
    #> 204       54  Sepal.Width        2.3
    #> 205       55  Sepal.Width        2.8
    #> 206       56  Sepal.Width        2.8
    #> 207       57  Sepal.Width        3.3
    #> 208       58  Sepal.Width        2.4
    #> 209       59  Sepal.Width        2.9
    #> 210       60  Sepal.Width        2.7
    #> 211       61  Sepal.Width          2
    #> 212       62  Sepal.Width          3
    #> 213       63  Sepal.Width        2.2
    #> 214       64  Sepal.Width        2.9
    #> 215       65  Sepal.Width        2.9
    #> 216       66  Sepal.Width        3.1
    #> 217       67  Sepal.Width          3
    #> 218       68  Sepal.Width        2.7
    #> 219       69  Sepal.Width        2.2
    #> 220       70  Sepal.Width        2.5
    #> 221       71  Sepal.Width        3.2
    #> 222       72  Sepal.Width        2.8
    #> 223       73  Sepal.Width        2.5
    #> 224       74  Sepal.Width        2.8
    #> 225       75  Sepal.Width        2.9
    #> 226       76  Sepal.Width          3
    #> 227       77  Sepal.Width        2.8
    #> 228       78  Sepal.Width          3
    #> 229       79  Sepal.Width        2.9
    #> 230       80  Sepal.Width        2.6
    #> 231       81  Sepal.Width        2.4
    #> 232       82  Sepal.Width        2.4
    #> 233       83  Sepal.Width        2.7
    #> 234       84  Sepal.Width        2.7
    #> 235       85  Sepal.Width          3
    #> 236       86  Sepal.Width        3.4
    #> 237       87  Sepal.Width        3.1
    #> 238       88  Sepal.Width        2.3
    #> 239       89  Sepal.Width          3
    #> 240       90  Sepal.Width        2.5
    #> 241       91  Sepal.Width        2.6
    #> 242       92  Sepal.Width          3
    #> 243       93  Sepal.Width        2.6
    #> 244       94  Sepal.Width        2.3
    #> 245       95  Sepal.Width        2.7
    #> 246       96  Sepal.Width          3
    #> 247       97  Sepal.Width        2.9
    #> 248       98  Sepal.Width        2.9
    #> 249       99  Sepal.Width        2.5
    #> 250      100  Sepal.Width        2.8
    #> 251      101  Sepal.Width        3.3
    #> 252      102  Sepal.Width        2.7
    #> 253      103  Sepal.Width          3
    #> 254      104  Sepal.Width        2.9
    #> 255      105  Sepal.Width          3
    #> 256      106  Sepal.Width          3
    #> 257      107  Sepal.Width        2.5
    #> 258      108  Sepal.Width        2.9
    #> 259      109  Sepal.Width        2.5
    #> 260      110  Sepal.Width        3.6
    #> 261      111  Sepal.Width        3.2
    #> 262      112  Sepal.Width        2.7
    #> 263      113  Sepal.Width          3
    #> 264      114  Sepal.Width        2.5
    #> 265      115  Sepal.Width        2.8
    #> 266      116  Sepal.Width        3.2
    #> 267      117  Sepal.Width          3
    #> 268      118  Sepal.Width        3.8
    #> 269      119  Sepal.Width        2.6
    #> 270      120  Sepal.Width        2.2
    #> 271      121  Sepal.Width        3.2
    #> 272      122  Sepal.Width        2.8
    #> 273      123  Sepal.Width        2.8
    #> 274      124  Sepal.Width        2.7
    #> 275      125  Sepal.Width        3.3
    #> 276      126  Sepal.Width        3.2
    #> 277      127  Sepal.Width        2.8
    #> 278      128  Sepal.Width          3
    #> 279      129  Sepal.Width        2.8
    #> 280      130  Sepal.Width          3
    #> 281      131  Sepal.Width        2.8
    #> 282      132  Sepal.Width        3.8
    #> 283      133  Sepal.Width        2.8
    #> 284      134  Sepal.Width        2.8
    #> 285      135  Sepal.Width        2.6
    #> 286      136  Sepal.Width          3
    #> 287      137  Sepal.Width        3.4
    #> 288      138  Sepal.Width        3.1
    #> 289      139  Sepal.Width          3
    #> 290      140  Sepal.Width        3.1
    #> 291      141  Sepal.Width        3.1
    #> 292      142  Sepal.Width        3.1
    #> 293      143  Sepal.Width        2.7
    #> 294      144  Sepal.Width        3.2
    #> 295      145  Sepal.Width        3.3
    #> 296      146  Sepal.Width          3
    #> 297      147  Sepal.Width        2.5
    #> 298      148  Sepal.Width          3
    #> 299      149  Sepal.Width        3.4
    #> 300      150  Sepal.Width          3
    #> 301        1 Petal.Length        1.4
    #> 302        2 Petal.Length        1.4
    #> 303        3 Petal.Length        1.3
    #> 304        4 Petal.Length        1.5
    #> 305        5 Petal.Length        1.4
    #> 306        6 Petal.Length        1.7
    #> 307        7 Petal.Length        1.4
    #> 308        8 Petal.Length        1.5
    #> 309        9 Petal.Length        1.4
    #> 310       10 Petal.Length        1.5
    #> 311       11 Petal.Length        1.5
    #> 312       12 Petal.Length        1.6
    #> 313       13 Petal.Length        1.4
    #> 314       14 Petal.Length        1.1
    #> 315       15 Petal.Length        1.2
    #> 316       16 Petal.Length        1.5
    #> 317       17 Petal.Length        1.3
    #> 318       18 Petal.Length        1.4
    #> 319       19 Petal.Length        1.7
    #> 320       20 Petal.Length        1.5
    #> 321       21 Petal.Length        1.7
    #> 322       22 Petal.Length        1.5
    #> 323       23 Petal.Length          1
    #> 324       24 Petal.Length        1.7
    #> 325       25 Petal.Length        1.9
    #> 326       26 Petal.Length        1.6
    #> 327       27 Petal.Length        1.6
    #> 328       28 Petal.Length        1.5
    #> 329       29 Petal.Length        1.4
    #> 330       30 Petal.Length        1.6
    #> 331       31 Petal.Length        1.6
    #> 332       32 Petal.Length        1.5
    #> 333       33 Petal.Length        1.5
    #> 334       34 Petal.Length        1.4
    #> 335       35 Petal.Length        1.5
    #> 336       36 Petal.Length        1.2
    #> 337       37 Petal.Length        1.3
    #> 338       38 Petal.Length        1.4
    #> 339       39 Petal.Length        1.3
    #> 340       40 Petal.Length        1.5
    #> 341       41 Petal.Length        1.3
    #> 342       42 Petal.Length        1.3
    #> 343       43 Petal.Length        1.3
    #> 344       44 Petal.Length        1.6
    #> 345       45 Petal.Length        1.9
    #> 346       46 Petal.Length        1.4
    #> 347       47 Petal.Length        1.6
    #> 348       48 Petal.Length        1.4
    #> 349       49 Petal.Length        1.5
    #> 350       50 Petal.Length        1.4
    #> 351       51 Petal.Length        4.7
    #> 352       52 Petal.Length        4.5
    #> 353       53 Petal.Length        4.9
    #> 354       54 Petal.Length          4
    #> 355       55 Petal.Length        4.6
    #> 356       56 Petal.Length        4.5
    #> 357       57 Petal.Length        4.7
    #> 358       58 Petal.Length        3.3
    #> 359       59 Petal.Length        4.6
    #> 360       60 Petal.Length        3.9
    #> 361       61 Petal.Length        3.5
    #> 362       62 Petal.Length        4.2
    #> 363       63 Petal.Length          4
    #> 364       64 Petal.Length        4.7
    #> 365       65 Petal.Length        3.6
    #> 366       66 Petal.Length        4.4
    #> 367       67 Petal.Length        4.5
    #> 368       68 Petal.Length        4.1
    #> 369       69 Petal.Length        4.5
    #> 370       70 Petal.Length        3.9
    #> 371       71 Petal.Length        4.8
    #> 372       72 Petal.Length          4
    #> 373       73 Petal.Length        4.9
    #> 374       74 Petal.Length        4.7
    #> 375       75 Petal.Length        4.3
    #> 376       76 Petal.Length        4.4
    #> 377       77 Petal.Length        4.8
    #> 378       78 Petal.Length          5
    #> 379       79 Petal.Length        4.5
    #> 380       80 Petal.Length        3.5
    #> 381       81 Petal.Length        3.8
    #> 382       82 Petal.Length        3.7
    #> 383       83 Petal.Length        3.9
    #> 384       84 Petal.Length        5.1
    #> 385       85 Petal.Length        4.5
    #> 386       86 Petal.Length        4.5
    #> 387       87 Petal.Length        4.7
    #> 388       88 Petal.Length        4.4
    #> 389       89 Petal.Length        4.1
    #> 390       90 Petal.Length          4
    #> 391       91 Petal.Length        4.4
    #> 392       92 Petal.Length        4.6
    #> 393       93 Petal.Length          4
    #> 394       94 Petal.Length        3.3
    #> 395       95 Petal.Length        4.2
    #> 396       96 Petal.Length        4.2
    #> 397       97 Petal.Length        4.2
    #> 398       98 Petal.Length        4.3
    #> 399       99 Petal.Length          3
    #> 400      100 Petal.Length        4.1
    #> 401      101 Petal.Length          6
    #> 402      102 Petal.Length        5.1
    #> 403      103 Petal.Length        5.9
    #> 404      104 Petal.Length        5.6
    #> 405      105 Petal.Length        5.8
    #> 406      106 Petal.Length        6.6
    #> 407      107 Petal.Length        4.5
    #> 408      108 Petal.Length        6.3
    #> 409      109 Petal.Length        5.8
    #> 410      110 Petal.Length        6.1
    #> 411      111 Petal.Length        5.1
    #> 412      112 Petal.Length        5.3
    #> 413      113 Petal.Length        5.5
    #> 414      114 Petal.Length          5
    #> 415      115 Petal.Length        5.1
    #> 416      116 Petal.Length        5.3
    #> 417      117 Petal.Length        5.5
    #> 418      118 Petal.Length        6.7
    #> 419      119 Petal.Length        6.9
    #> 420      120 Petal.Length          5
    #> 421      121 Petal.Length        5.7
    #> 422      122 Petal.Length        4.9
    #> 423      123 Petal.Length        6.7
    #> 424      124 Petal.Length        4.9
    #> 425      125 Petal.Length        5.7
    #> 426      126 Petal.Length          6
    #> 427      127 Petal.Length        4.8
    #> 428      128 Petal.Length        4.9
    #> 429      129 Petal.Length        5.6
    #> 430      130 Petal.Length        5.8
    #> 431      131 Petal.Length        6.1
    #> 432      132 Petal.Length        6.4
    #> 433      133 Petal.Length        5.6
    #> 434      134 Petal.Length        5.1
    #> 435      135 Petal.Length        5.6
    #> 436      136 Petal.Length        6.1
    #> 437      137 Petal.Length        5.6
    #> 438      138 Petal.Length        5.5
    #> 439      139 Petal.Length        4.8
    #> 440      140 Petal.Length        5.4
    #> 441      141 Petal.Length        5.6
    #> 442      142 Petal.Length        5.1
    #> 443      143 Petal.Length        5.1
    #> 444      144 Petal.Length        5.9
    #> 445      145 Petal.Length        5.7
    #> 446      146 Petal.Length        5.2
    #> 447      147 Petal.Length          5
    #> 448      148 Petal.Length        5.2
    #> 449      149 Petal.Length        5.4
    #> 450      150 Petal.Length        5.1
    #> 451        1  Petal.Width        0.2
    #> 452        2  Petal.Width        0.2
    #> 453        3  Petal.Width        0.2
    #> 454        4  Petal.Width        0.2
    #> 455        5  Petal.Width        0.2
    #> 456        6  Petal.Width        0.4
    #> 457        7  Petal.Width        0.3
    #> 458        8  Petal.Width        0.2
    #> 459        9  Petal.Width        0.2
    #> 460       10  Petal.Width        0.1
    #> 461       11  Petal.Width        0.2
    #> 462       12  Petal.Width        0.2
    #> 463       13  Petal.Width        0.1
    #> 464       14  Petal.Width        0.1
    #> 465       15  Petal.Width        0.2
    #> 466       16  Petal.Width        0.4
    #> 467       17  Petal.Width        0.4
    #> 468       18  Petal.Width        0.3
    #> 469       19  Petal.Width        0.3
    #> 470       20  Petal.Width        0.3
    #> 471       21  Petal.Width        0.2
    #> 472       22  Petal.Width        0.4
    #> 473       23  Petal.Width        0.2
    #> 474       24  Petal.Width        0.5
    #> 475       25  Petal.Width        0.2
    #> 476       26  Petal.Width        0.2
    #> 477       27  Petal.Width        0.4
    #> 478       28  Petal.Width        0.2
    #> 479       29  Petal.Width        0.2
    #> 480       30  Petal.Width        0.2
    #> 481       31  Petal.Width        0.2
    #> 482       32  Petal.Width        0.4
    #> 483       33  Petal.Width        0.1
    #> 484       34  Petal.Width        0.2
    #> 485       35  Petal.Width        0.2
    #> 486       36  Petal.Width        0.2
    #> 487       37  Petal.Width        0.2
    #> 488       38  Petal.Width        0.1
    #> 489       39  Petal.Width        0.2
    #> 490       40  Petal.Width        0.2
    #> 491       41  Petal.Width        0.3
    #> 492       42  Petal.Width        0.3
    #> 493       43  Petal.Width        0.2
    #> 494       44  Petal.Width        0.6
    #> 495       45  Petal.Width        0.4
    #> 496       46  Petal.Width        0.3
    #> 497       47  Petal.Width        0.2
    #> 498       48  Petal.Width        0.2
    #> 499       49  Petal.Width        0.2
    #> 500       50  Petal.Width        0.2
    #> 501       51  Petal.Width        1.4
    #> 502       52  Petal.Width        1.5
    #> 503       53  Petal.Width        1.5
    #> 504       54  Petal.Width        1.3
    #> 505       55  Petal.Width        1.5
    #> 506       56  Petal.Width        1.3
    #> 507       57  Petal.Width        1.6
    #> 508       58  Petal.Width          1
    #> 509       59  Petal.Width        1.3
    #> 510       60  Petal.Width        1.4
    #> 511       61  Petal.Width          1
    #> 512       62  Petal.Width        1.5
    #> 513       63  Petal.Width          1
    #> 514       64  Petal.Width        1.4
    #> 515       65  Petal.Width        1.3
    #> 516       66  Petal.Width        1.4
    #> 517       67  Petal.Width        1.5
    #> 518       68  Petal.Width          1
    #> 519       69  Petal.Width        1.5
    #> 520       70  Petal.Width        1.1
    #> 521       71  Petal.Width        1.8
    #> 522       72  Petal.Width        1.3
    #> 523       73  Petal.Width        1.5
    #> 524       74  Petal.Width        1.2
    #> 525       75  Petal.Width        1.3
    #> 526       76  Petal.Width        1.4
    #> 527       77  Petal.Width        1.4
    #> 528       78  Petal.Width        1.7
    #> 529       79  Petal.Width        1.5
    #> 530       80  Petal.Width          1
    #> 531       81  Petal.Width        1.1
    #> 532       82  Petal.Width          1
    #> 533       83  Petal.Width        1.2
    #> 534       84  Petal.Width        1.6
    #> 535       85  Petal.Width        1.5
    #> 536       86  Petal.Width        1.6
    #> 537       87  Petal.Width        1.5
    #> 538       88  Petal.Width        1.3
    #> 539       89  Petal.Width        1.3
    #> 540       90  Petal.Width        1.3
    #> 541       91  Petal.Width        1.2
    #> 542       92  Petal.Width        1.4
    #> 543       93  Petal.Width        1.2
    #> 544       94  Petal.Width          1
    #> 545       95  Petal.Width        1.3
    #> 546       96  Petal.Width        1.2
    #> 547       97  Petal.Width        1.3
    #> 548       98  Petal.Width        1.3
    #> 549       99  Petal.Width        1.1
    #> 550      100  Petal.Width        1.3
    #> 551      101  Petal.Width        2.5
    #> 552      102  Petal.Width        1.9
    #> 553      103  Petal.Width        2.1
    #> 554      104  Petal.Width        1.8
    #> 555      105  Petal.Width        2.2
    #> 556      106  Petal.Width        2.1
    #> 557      107  Petal.Width        1.7
    #> 558      108  Petal.Width        1.8
    #> 559      109  Petal.Width        1.8
    #> 560      110  Petal.Width        2.5
    #> 561      111  Petal.Width          2
    #> 562      112  Petal.Width        1.9
    #> 563      113  Petal.Width        2.1
    #> 564      114  Petal.Width          2
    #> 565      115  Petal.Width        2.4
    #> 566      116  Petal.Width        2.3
    #> 567      117  Petal.Width        1.8
    #> 568      118  Petal.Width        2.2
    #> 569      119  Petal.Width        2.3
    #> 570      120  Petal.Width        1.5
    #> 571      121  Petal.Width        2.3
    #> 572      122  Petal.Width          2
    #> 573      123  Petal.Width          2
    #> 574      124  Petal.Width        1.8
    #> 575      125  Petal.Width        2.1
    #> 576      126  Petal.Width        1.8
    #> 577      127  Petal.Width        1.8
    #> 578      128  Petal.Width        1.8
    #> 579      129  Petal.Width        2.1
    #> 580      130  Petal.Width        1.6
    #> 581      131  Petal.Width        1.9
    #> 582      132  Petal.Width          2
    #> 583      133  Petal.Width        2.2
    #> 584      134  Petal.Width        1.5
    #> 585      135  Petal.Width        1.4
    #> 586      136  Petal.Width        2.3
    #> 587      137  Petal.Width        2.4
    #> 588      138  Petal.Width        1.8
    #> 589      139  Petal.Width        1.8
    #> 590      140  Petal.Width        2.1
    #> 591      141  Petal.Width        2.4
    #> 592      142  Petal.Width        2.3
    #> 593      143  Petal.Width        1.9
    #> 594      144  Petal.Width        2.3
    #> 595      145  Petal.Width        2.5
    #> 596      146  Petal.Width        2.3
    #> 597      147  Petal.Width        1.9
    #> 598      148  Petal.Width          2
    #> 599      149  Petal.Width        2.3
    #> 600      150  Petal.Width        1.8
    #> 601        1      Species     setosa
    #> 602        2      Species     setosa
    #> 603        3      Species     setosa
    #> 604        4      Species     setosa
    #> 605        5      Species     setosa
    #> 606        6      Species     setosa
    #> 607        7      Species     setosa
    #> 608        8      Species     setosa
    #> 609        9      Species     setosa
    #> 610       10      Species     setosa
    #> 611       11      Species     setosa
    #> 612       12      Species     setosa
    #> 613       13      Species     setosa
    #> 614       14      Species     setosa
    #> 615       15      Species     setosa
    #> 616       16      Species     setosa
    #> 617       17      Species     setosa
    #> 618       18      Species     setosa
    #> 619       19      Species     setosa
    #> 620       20      Species     setosa
    #> 621       21      Species     setosa
    #> 622       22      Species     setosa
    #> 623       23      Species     setosa
    #> 624       24      Species     setosa
    #> 625       25      Species     setosa
    #> 626       26      Species     setosa
    #> 627       27      Species     setosa
    #> 628       28      Species     setosa
    #> 629       29      Species     setosa
    #> 630       30      Species     setosa
    #> 631       31      Species     setosa
    #> 632       32      Species     setosa
    #> 633       33      Species     setosa
    #> 634       34      Species     setosa
    #> 635       35      Species     setosa
    #> 636       36      Species     setosa
    #> 637       37      Species     setosa
    #> 638       38      Species     setosa
    #> 639       39      Species     setosa
    #> 640       40      Species     setosa
    #> 641       41      Species     setosa
    #> 642       42      Species     setosa
    #> 643       43      Species     setosa
    #> 644       44      Species     setosa
    #> 645       45      Species     setosa
    #> 646       46      Species     setosa
    #> 647       47      Species     setosa
    #> 648       48      Species     setosa
    #> 649       49      Species     setosa
    #> 650       50      Species     setosa
    #> 651       51      Species versicolor
    #> 652       52      Species versicolor
    #> 653       53      Species versicolor
    #> 654       54      Species versicolor
    #> 655       55      Species versicolor
    #> 656       56      Species versicolor
    #> 657       57      Species versicolor
    #> 658       58      Species versicolor
    #> 659       59      Species versicolor
    #> 660       60      Species versicolor
    #> 661       61      Species versicolor
    #> 662       62      Species versicolor
    #> 663       63      Species versicolor
    #> 664       64      Species versicolor
    #> 665       65      Species versicolor
    #> 666       66      Species versicolor
    #> 667       67      Species versicolor
    #> 668       68      Species versicolor
    #> 669       69      Species versicolor
    #> 670       70      Species versicolor
    #> 671       71      Species versicolor
    #> 672       72      Species versicolor
    #> 673       73      Species versicolor
    #> 674       74      Species versicolor
    #> 675       75      Species versicolor
    #> 676       76      Species versicolor
    #> 677       77      Species versicolor
    #> 678       78      Species versicolor
    #> 679       79      Species versicolor
    #> 680       80      Species versicolor
    #> 681       81      Species versicolor
    #> 682       82      Species versicolor
    #> 683       83      Species versicolor
    #> 684       84      Species versicolor
    #> 685       85      Species versicolor
    #> 686       86      Species versicolor
    #> 687       87      Species versicolor
    #> 688       88      Species versicolor
    #> 689       89      Species versicolor
    #> 690       90      Species versicolor
    #> 691       91      Species versicolor
    #> 692       92      Species versicolor
    #> 693       93      Species versicolor
    #> 694       94      Species versicolor
    #> 695       95      Species versicolor
    #> 696       96      Species versicolor
    #> 697       97      Species versicolor
    #> 698       98      Species versicolor
    #> 699       99      Species versicolor
    #> 700      100      Species versicolor
    #> 701      101      Species  virginica
    #> 702      102      Species  virginica
    #> 703      103      Species  virginica
    #> 704      104      Species  virginica
    #> 705      105      Species  virginica
    #> 706      106      Species  virginica
    #> 707      107      Species  virginica
    #> 708      108      Species  virginica
    #> 709      109      Species  virginica
    #> 710      110      Species  virginica
    #> 711      111      Species  virginica
    #> 712      112      Species  virginica
    #> 713      113      Species  virginica
    #> 714      114      Species  virginica
    #> 715      115      Species  virginica
    #> 716      116      Species  virginica
    #> 717      117      Species  virginica
    #> 718      118      Species  virginica
    #> 719      119      Species  virginica
    #> 720      120      Species  virginica
    #> 721      121      Species  virginica
    #> 722      122      Species  virginica
    #> 723      123      Species  virginica
    #> 724      124      Species  virginica
    #> 725      125      Species  virginica
    #> 726      126      Species  virginica
    #> 727      127      Species  virginica
    #> 728      128      Species  virginica
    #> 729      129      Species  virginica
    #> 730      130      Species  virginica
    #> 731      131      Species  virginica
    #> 732      132      Species  virginica
    #> 733      133      Species  virginica
    #> 734      134      Species  virginica
    #> 735      135      Species  virginica
    #> 736      136      Species  virginica
    #> 737      137      Species  virginica
    #> 738      138      Species  virginica
    #> 739      139      Species  virginica
    #> 740      140      Species  virginica
    #> 741      141      Species  virginica
    #> 742      142      Species  virginica
    #> 743      143      Species  virginica
    #> 744      144      Species  virginica
    #> 745      145      Species  virginica
    #> 746      146      Species  virginica
    #> 747      147      Species  virginica
    #> 748      148      Species  virginica
    #> 749      149      Species  virginica
    #> 750      150      Species  virginica

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
  gather(measurement, value, -Tree)
Orange_tidy
```

    #>    Tree   measurement value
    #> 1     1           age   118
    #> 2     1           age   484
    #> 3     1           age   664
    #> 4     1           age  1004
    #> 5     1           age  1231
    #> 6     1           age  1372
    #> 7     1           age  1582
    #> 8     2           age   118
    #> 9     2           age   484
    #> 10    2           age   664
    #> 11    2           age  1004
    #> 12    2           age  1231
    #> 13    2           age  1372
    #> 14    2           age  1582
    #> 15    3           age   118
    #> 16    3           age   484
    #> 17    3           age   664
    #> 18    3           age  1004
    #> 19    3           age  1231
    #> 20    3           age  1372
    #> 21    3           age  1582
    #> 22    4           age   118
    #> 23    4           age   484
    #> 24    4           age   664
    #> 25    4           age  1004
    #> 26    4           age  1231
    #> 27    4           age  1372
    #> 28    4           age  1582
    #> 29    5           age   118
    #> 30    5           age   484
    #> 31    5           age   664
    #> 32    5           age  1004
    #> 33    5           age  1231
    #> 34    5           age  1372
    #> 35    5           age  1582
    #> 36    1 circumference    30
    #> 37    1 circumference    58
    #> 38    1 circumference    87
    #> 39    1 circumference   115
    #> 40    1 circumference   120
    #> 41    1 circumference   142
    #> 42    1 circumference   145
    #> 43    2 circumference    33
    #> 44    2 circumference    69
    #> 45    2 circumference   111
    #> 46    2 circumference   156
    #> 47    2 circumference   172
    #> 48    2 circumference   203
    #> 49    2 circumference   203
    #> 50    3 circumference    30
    #> 51    3 circumference    51
    #> 52    3 circumference    75
    #> 53    3 circumference   108
    #> 54    3 circumference   115
    #> 55    3 circumference   139
    #> 56    3 circumference   140
    #> 57    4 circumference    32
    #> 58    4 circumference    62
    #> 59    4 circumference   112
    #> 60    4 circumference   167
    #> 61    4 circumference   179
    #> 62    4 circumference   209
    #> 63    4 circumference   214
    #> 64    5 circumference    30
    #> 65    5 circumference    49
    #> 66    5 circumference    81
    #> 67    5 circumference   125
    #> 68    5 circumference   142
    #> 69    5 circumference   174
    #> 70    5 circumference   177

OrchardSprays
-------------

PlantGrowth
-----------

``` r
PlantGrowth_tidy <- PlantGrowth %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs)
```

    #> Warning: attributes are not identical across measure variables; they will
    #> be dropped

``` r
PlantGrowth_tidy
```

    #>    obs    key value
    #> 1    1 weight  4.17
    #> 2    2 weight  5.58
    #> 3    3 weight  5.18
    #> 4    4 weight  6.11
    #> 5    5 weight   4.5
    #> 6    6 weight  4.61
    #> 7    7 weight  5.17
    #> 8    8 weight  4.53
    #> 9    9 weight  5.33
    #> 10  10 weight  5.14
    #> 11  11 weight  4.81
    #> 12  12 weight  4.17
    #> 13  13 weight  4.41
    #> 14  14 weight  3.59
    #> 15  15 weight  5.87
    #> 16  16 weight  3.83
    #> 17  17 weight  6.03
    #> 18  18 weight  4.89
    #> 19  19 weight  4.32
    #> 20  20 weight  4.69
    #> 21  21 weight  6.31
    #> 22  22 weight  5.12
    #> 23  23 weight  5.54
    #> 24  24 weight   5.5
    #> 25  25 weight  5.37
    #> 26  26 weight  5.29
    #> 27  27 weight  4.92
    #> 28  28 weight  6.15
    #> 29  29 weight   5.8
    #> 30  30 weight  5.26
    #> 31   1  group  ctrl
    #> 32   2  group  ctrl
    #> 33   3  group  ctrl
    #> 34   4  group  ctrl
    #> 35   5  group  ctrl
    #> 36   6  group  ctrl
    #> 37   7  group  ctrl
    #> 38   8  group  ctrl
    #> 39   9  group  ctrl
    #> 40  10  group  ctrl
    #> 41  11  group  trt1
    #> 42  12  group  trt1
    #> 43  13  group  trt1
    #> 44  14  group  trt1
    #> 45  15  group  trt1
    #> 46  16  group  trt1
    #> 47  17  group  trt1
    #> 48  18  group  trt1
    #> 49  19  group  trt1
    #> 50  20  group  trt1
    #> 51  21  group  trt2
    #> 52  22  group  trt2
    #> 53  23  group  trt2
    #> 54  24  group  trt2
    #> 55  25  group  trt2
    #> 56  26  group  trt2
    #> 57  27  group  trt2
    #> 58  28  group  trt2
    #> 59  29  group  trt2
    #> 60  30  group  trt2

pressure
--------

``` r
pressure_tidy <- pressure %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs)
pressure_tidy
```

    #>    obs         key    value
    #> 1    1 temperature   0.0000
    #> 2    2 temperature  20.0000
    #> 3    3 temperature  40.0000
    #> 4    4 temperature  60.0000
    #> 5    5 temperature  80.0000
    #> 6    6 temperature 100.0000
    #> 7    7 temperature 120.0000
    #> 8    8 temperature 140.0000
    #> 9    9 temperature 160.0000
    #> 10  10 temperature 180.0000
    #> 11  11 temperature 200.0000
    #> 12  12 temperature 220.0000
    #> 13  13 temperature 240.0000
    #> 14  14 temperature 260.0000
    #> 15  15 temperature 280.0000
    #> 16  16 temperature 300.0000
    #> 17  17 temperature 320.0000
    #> 18  18 temperature 340.0000
    #> 19  19 temperature 360.0000
    #> 20   1    pressure   0.0002
    #> 21   2    pressure   0.0012
    #> 22   3    pressure   0.0060
    #> 23   4    pressure   0.0300
    #> 24   5    pressure   0.0900
    #> 25   6    pressure   0.2700
    #> 26   7    pressure   0.7500
    #> 27   8    pressure   1.8500
    #> 28   9    pressure   4.2000
    #> 29  10    pressure   8.8000
    #> 30  11    pressure  17.3000
    #> 31  12    pressure  32.1000
    #> 32  13    pressure  57.0000
    #> 33  14    pressure  96.0000
    #> 34  15    pressure 157.0000
    #> 35  16    pressure 247.0000
    #> 36  17    pressure 376.0000
    #> 37  18    pressure 558.0000
    #> 38  19    pressure 806.0000

Puromycin
---------

quakes
------

randu
-----

``` r
randu_tidy <- randu %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) 
randu_tidy
```

    #>      obs key    value
    #> 1      1   x 0.000031
    #> 2      2   x 0.044495
    #> 3      3   x 0.822440
    #> 4      4   x 0.322291
    #> 5      5   x 0.393595
    #> 6      6   x 0.309097
    #> 7      7   x 0.826368
    #> 8      8   x 0.729424
    #> 9      9   x 0.317649
    #> 10    10   x 0.599793
    #> 11    11   x 0.647603
    #> 12    12   x 0.547048
    #> 13    13   x 0.529873
    #> 14    14   x 0.908040
    #> 15    15   x 0.835195
    #> 16    16   x 0.068696
    #> 17    17   x 0.984329
    #> 18    18   x 0.945783
    #> 19    19   x 0.017137
    #> 20    20   x 0.772506
    #> 21    21   x 0.493080
    #> 22    22   x 0.919386
    #> 23    23   x 0.964342
    #> 24    24   x 0.864672
    #> 25    25   x 0.786249
    #> 26    26   x 0.123862
    #> 27    27   x 0.990535
    #> 28    28   x 0.455714
    #> 29    29   x 0.345516
    #> 30    30   x 0.482433
    #> 31    31   x 0.063100
    #> 32    32   x 0.494563
    #> 33    33   x 0.386052
    #> 34    34   x 0.156384
    #> 35    35   x 0.999850
    #> 36    36   x 0.585455
    #> 37    37   x 0.361887
    #> 38    38   x 0.350248
    #> 39    39   x 0.126752
    #> 40    40   x 0.812634
    #> 41    41   x 0.369723
    #> 42    42   x 0.437286
    #> 43    43   x 0.771568
    #> 44    44   x 0.697878
    #> 45    45   x 0.826174
    #> 46    46   x 0.530293
    #> 47    47   x 0.968455
    #> 48    48   x 0.415824
    #> 49    49   x 0.793458
    #> 50    50   x 0.622709
    #> 51    51   x 0.748484
    #> 52    52   x 0.392119
    #> 53    53   x 0.328510
    #> 54    54   x 0.425048
    #> 55    55   x 0.391111
    #> 56    56   x 0.409372
    #> 57    57   x 0.230546
    #> 58    58   x 0.021631
    #> 59    59   x 0.011576
    #> 60    60   x 0.345928
    #> 61    61   x 0.562056
    #> 62    62   x 0.472640
    #> 63    63   x 0.859244
    #> 64    64   x 0.672445
    #> 65    65   x 0.318240
    #> 66    66   x 0.486482
    #> 67    67   x 0.701881
    #> 68    68   x 0.831292
    #> 69    69   x 0.619876
    #> 70    70   x 0.287463
    #> 71    71   x 0.670872
    #> 72    72   x 0.623757
    #> 73    73   x 0.804632
    #> 74    74   x 0.803778
    #> 75    75   x 0.942370
    #> 76    76   x 0.730063
    #> 77    77   x 0.791810
    #> 78    78   x 0.300808
    #> 79    79   x 0.629387
    #> 80    80   x 0.487203
    #> 81    81   x 0.095530
    #> 82    82   x 0.584738
    #> 83    83   x 0.259822
    #> 84    84   x 0.049581
    #> 85    85   x 0.861351
    #> 86    86   x 0.901482
    #> 87    87   x 0.215425
    #> 88    88   x 0.057474
    #> 89    89   x 0.301331
    #> 90    90   x 0.655918
    #> 91    91   x 0.506700
    #> 92    92   x 0.938298
    #> 93    93   x 0.902520
    #> 94    94   x 0.076441
    #> 95    95   x 0.258743
    #> 96    96   x 0.965690
    #> 97    97   x 0.783897
    #> 98    98   x 0.949827
    #> 99    99   x 0.283024
    #> 100  100   x 0.201405
    #> 101  101   x 0.576739
    #> 102  102   x 0.551408
    #> 103  103   x 0.110096
    #> 104  104   x 0.445020
    #> 105  105   x 0.203431
    #> 106  106   x 0.895558
    #> 107  107   x 0.858251
    #> 108  108   x 0.316456
    #> 109  109   x 0.929709
    #> 110  110   x 0.431499
    #> 111  111   x 0.313108
    #> 112  112   x 0.596309
    #> 113  113   x 0.065349
    #> 114  114   x 0.309409
    #> 115  115   x 0.593143
    #> 116  116   x 0.948994
    #> 117  117   x 0.688282
    #> 118  118   x 0.341853
    #> 119  119   x 0.776151
    #> 120  120   x 0.153704
    #> 121  121   x 0.767158
    #> 122  122   x 0.785514
    #> 123  123   x 0.861648
    #> 124  124   x 0.968176
    #> 125  125   x 0.057338
    #> 126  126   x 0.068251
    #> 127  127   x 0.444216
    #> 128  128   x 0.750276
    #> 129  129   x 0.145329
    #> 130  130   x 0.586459
    #> 131  131   x 0.472772
    #> 132  132   x 0.962394
    #> 133  133   x 0.016658
    #> 134  134   x 0.697745
    #> 135  135   x 0.470791
    #> 136  136   x 0.683648
    #> 137  137   x 0.522175
    #> 138  138   x 0.033916
    #> 139  139   x 0.592453
    #> 140  140   x 0.254907
    #> 141  141   x 0.117803
    #> 142  142   x 0.259280
    #> 143  143   x 0.889736
    #> 144  144   x 0.181619
    #> 145  145   x 0.252720
    #> 146  146   x 0.399261
    #> 147  147   x 0.165440
    #> 148  148   x 0.419460
    #> 149  149   x 0.780983
    #> 150  150   x 0.864323
    #> 151  151   x 0.787156
    #> 152  152   x 0.167788
    #> 153  153   x 0.771582
    #> 154  154   x 0.294206
    #> 155  155   x 0.837164
    #> 156  156   x 0.295912
    #> 157  157   x 0.091939
    #> 158  158   x 0.347482
    #> 159  159   x 0.973278
    #> 160  160   x 0.527179
    #> 161  161   x 0.139841
    #> 162  162   x 0.561416
    #> 163  163   x 0.375616
    #> 164  164   x 0.505862
    #> 165  165   x 0.098811
    #> 166  166   x 0.407139
    #> 167  167   x 0.154323
    #> 168  168   x 0.871869
    #> 169  169   x 0.091918
    #> 170  170   x 0.665140
    #> 171  171   x 0.593219
    #> 172  172   x 0.468268
    #> 173  173   x 0.609019
    #> 174  174   x 0.246066
    #> 175  175   x 0.604387
    #> 176  176   x 0.806610
    #> 177  177   x 0.582449
    #> 178  178   x 0.581833
    #> 179  179   x 0.768705
    #> 180  180   x 0.915080
    #> 181  181   x 0.486134
    #> 182  182   x 0.712057
    #> 183  183   x 0.337308
    #> 184  184   x 0.694452
    #> 185  185   x 0.833158
    #> 186  186   x 0.190784
    #> 187  187   x 0.568990
    #> 188  188   x 0.906860
    #> 189  189   x 0.446754
    #> 190  190   x 0.938550
    #> 191  191   x 0.778547
    #> 192  192   x 0.922376
    #> 193  193   x 0.879737
    #> 194  194   x 0.164737
    #> 195  195   x 0.471049
    #> 196  196   x 0.348409
    #> 197  197   x 0.357379
    #> 198  198   x 0.485254
    #> 199  199   x 0.937061
    #> 200  200   x 0.666909
    #> 201  201   x 0.618715
    #> 202  202   x 0.360520
    #> 203  203   x 0.683790
    #> 204  204   x 0.004390
    #> 205  205   x 0.031289
    #> 206  206   x 0.978769
    #> 207  207   x 0.077178
    #> 208  208   x 0.159757
    #> 209  209   x 0.354338
    #> 210  210   x 0.709664
    #> 211  211   x 0.569929
    #> 212  212   x 0.014955
    #> 213  213   x 0.525414
    #> 214  214   x 0.253219
    #> 215  215   x 0.890473
    #> 216  216   x 0.453425
    #> 217  217   x 0.845441
    #> 218  218   x 0.609038
    #> 219  219   x 0.567867
    #> 220  220   x 0.911370
    #> 221  221   x 0.937211
    #> 222  222   x 0.990868
    #> 223  223   x 0.167640
    #> 224  224   x 0.686843
    #> 225  225   x 0.852322
    #> 226  226   x 0.811463
    #> 227  227   x 0.613563
    #> 228  228   x 0.131638
    #> 229  229   x 0.701542
    #> 230  230   x 0.862754
    #> 231  231   x 0.970370
    #> 232  232   x 0.850995
    #> 233  233   x 0.183622
    #> 234  234   x 0.816344
    #> 235  235   x 0.062409
    #> 236  236   x 0.036126
    #> 237  237   x 0.387542
    #> 238  238   x 0.169305
    #> 239  239   x 0.303227
    #> 240  240   x 0.054538
    #> 241  241   x 0.243194
    #> 242  242   x 0.760294
    #> 243  243   x 0.111106
    #> 244  244   x 0.423188
    #> 245  245   x 0.995503
    #> 246  246   x 0.980973
    #> 247  247   x 0.285519
    #> 248  248   x 0.289431
    #> 249  249   x 0.076984
    #> 250  250   x 0.807755
    #> 251  251   x 0.719538
    #> 252  252   x 0.544794
    #> 253  253   x 0.753742
    #> 254  254   x 0.778851
    #> 255  255   x 0.823174
    #> 256  256   x 0.696558
    #> 257  257   x 0.919900
    #> 258  258   x 0.041631
    #> 259  259   x 0.032650
    #> 260  260   x 0.622150
    #> 261  261   x 0.415478
    #> 262  262   x 0.595303
    #> 263  263   x 0.780619
    #> 264  264   x 0.331354
    #> 265  265   x 0.242692
    #> 266  266   x 0.853902
    #> 267  267   x 0.302318
    #> 268  268   x 0.861326
    #> 269  269   x 0.055708
    #> 270  270   x 0.654588
    #> 271  271   x 0.652651
    #> 272  272   x 0.429430
    #> 273  273   x 0.298822
    #> 274  274   x 0.836260
    #> 275  275   x 0.309226
    #> 276  276   x 0.968880
    #> 277  277   x 0.368081
    #> 278  278   x 0.513483
    #> 279  279   x 0.736313
    #> 280  280   x 0.172198
    #> 281  281   x 0.171344
    #> 282  282   x 0.170726
    #> 283  283   x 0.284745
    #> 284  284   x 0.167483
    #> 285  285   x 0.461775
    #> 286  286   x 0.701912
    #> 287  287   x 0.802767
    #> 288  288   x 0.952495
    #> 289  289   x 0.319777
    #> 290  290   x 0.520280
    #> 291  291   x 0.332802
    #> 292  292   x 0.711546
    #> 293  293   x 0.158367
    #> 294  294   x 0.863566
    #> 295  295   x 0.269175
    #> 296  296   x 0.140211
    #> 297  297   x 0.626980
    #> 298  298   x 0.419483
    #> 299  299   x 0.351759
    #> 300  300   x 0.902842
    #> 301  301   x 0.788715
    #> 302  302   x 0.396531
    #> 303  303   x 0.870567
    #> 304  304   x 0.347908
    #> 305  305   x 0.946024
    #> 306  306   x 0.165103
    #> 307  307   x 0.456284
    #> 308  308   x 0.606132
    #> 309  309   x 0.489827
    #> 310  310   x 0.593914
    #> 311  311   x 0.831722
    #> 312  312   x 0.196452
    #> 313  313   x 0.147074
    #> 314  314   x 0.206740
    #> 315  315   x 0.899231
    #> 316  316   x 0.264790
    #> 317  317   x 0.001739
    #> 318  318   x 0.284465
    #> 319  319   x 0.539036
    #> 320  320   x 0.580633
    #> 321  321   x 0.664257
    #> 322  322   x 0.037451
    #> 323  323   x 0.493513
    #> 324  324   x 0.416429
    #> 325  325   x 0.964390
    #> 326  326   x 0.973205
    #> 327  327   x 0.712404
    #> 328  328   x 0.434793
    #> 329  329   x 0.542540
    #> 330  330   x 0.584376
    #> 331  331   x 0.533974
    #> 332  332   x 0.708526
    #> 333  333   x 0.714495
    #> 334  334   x 0.482048
    #> 335  335   x 0.077093
    #> 336  336   x 0.998449
    #> 337  337   x 0.984607
    #> 338  338   x 0.099361
    #> 339  339   x 0.219269
    #> 340  340   x 0.414045
    #> 341  341   x 0.582422
    #> 342  342   x 0.090429
    #> 343  343   x 0.535613
    #> 344  344   x 0.581919
    #> 345  345   x 0.397729
    #> 346  346   x 0.549584
    #> 347  347   x 0.573737
    #> 348  348   x 0.447064
    #> 349  349   x 0.689066
    #> 350  350   x 0.175925
    #> 351  351   x 0.839597
    #> 352  352   x 0.831947
    #> 353  353   x 0.940644
    #> 354  354   x 0.508181
    #> 355  355   x 0.869272
    #> 356  356   x 0.878399
    #> 357  357   x 0.242726
    #> 358  358   x 0.354886
    #> 359  359   x 0.761675
    #> 360  360   x 0.497327
    #> 361  361   x 0.570429
    #> 362  362   x 0.544869
    #> 363  363   x 0.547206
    #> 364  364   x 0.951230
    #> 365  365   x 0.335976
    #> 366  366   x 0.123054
    #> 367  367   x 0.767345
    #> 368  368   x 0.694530
    #> 369  369   x 0.589375
    #> 370  370   x 0.116573
    #> 371  371   x 0.640176
    #> 372  372   x 0.596723
    #> 373  373   x 0.242544
    #> 374  374   x 0.996192
    #> 375  375   x 0.186854
    #> 376  376   x 0.673327
    #> 377  377   x 0.691865
    #> 378  378   x 0.958049
    #> 379  379   x 0.694005
    #> 380  380   x 0.449659
    #> 381  381   x 0.214184
    #> 382  382   x 0.150706
    #> 383  383   x 0.887069
    #> 384  384   x 0.082414
    #> 385  385   x 0.511244
    #> 386  386   x 0.972510
    #> 387  387   x 0.189574
    #> 388  388   x 0.364059
    #> 389  389   x 0.777554
    #> 390  390   x 0.564273
    #> 391  391   x 0.443353
    #> 392  392   x 0.735040
    #> 393  393   x 0.666700
    #> 394  394   x 0.622253
    #> 395  395   x 0.464697
    #> 396  396   x 0.428804
    #> 397  397   x 0.531088
    #> 398  398   x 0.656463
    #> 399  399   x 0.811441
    #> 400  400   x 0.874628
    #> 401    1   y 0.000183
    #> 402    2   y 0.155732
    #> 403    3   y 0.873416
    #> 404    4   y 0.648545
    #> 405    5   y 0.826873
    #> 406    6   y 0.926590
    #> 407    7   y 0.308540
    #> 408    8   y 0.741526
    #> 409    9   y 0.393468
    #> 410   10   y 0.846041
    #> 411   11   y 0.281525
    #> 412   12   y 0.948790
    #> 413   13   y 0.348011
    #> 414   14   y 0.013456
    #> 415   15   y 0.814513
    #> 416   16   y 0.275943
    #> 417   17   y 0.927687
    #> 418   18   y 0.689675
    #> 419   19   y 0.166494
    #> 420   20   y 0.282393
    #> 421   21   y 0.943686
    #> 422   22   y 0.618783
    #> 423   23   y 0.025198
    #> 424   24   y 0.711721
    #> 425   25   y 0.961377
    #> 426   26   y 0.810826
    #> 427   27   y 0.706806
    #> 428   28   y 0.020492
    #> 429   29   y 0.800801
    #> 430   30   y 0.160464
    #> 431   31   y 0.488463
    #> 432   32   y 0.180498
    #> 433   33   y 0.482467
    #> 434   34   y 0.276557
    #> 435   35   y 0.198618
    #> 436   36   y 0.129442
    #> 437   37   y 0.743469
    #> 438   38   y 0.897698
    #> 439   39   y 0.190162
    #> 440   40   y 0.245063
    #> 441   41   y 0.248908
    #> 442   42   y 0.268675
    #> 443   43   y 0.821389
    #> 444   44   y 0.217688
    #> 445   45   y 0.623633
    #> 446   46   y 0.852871
    #> 447   47   y 0.569763
    #> 448   48   y 0.696233
    #> 449   49   y 0.429293
    #> 450   50   y 0.755610
    #> 451   51   y 0.903503
    #> 452   52   y 0.082637
    #> 453   53   y 0.211696
    #> 454   54   y 0.233427
    #> 455   55   y 0.035974
    #> 456   56   y 0.849729
    #> 457   57   y 0.743487
    #> 458   58   y 0.664288
    #> 459   59   y 0.687896
    #> 460   60   y 0.757540
    #> 461   61   y 0.587138
    #> 462   62   y 0.353742
    #> 463   63   y 0.982364
    #> 464   64   y 0.343048
    #> 465   65   y 0.103248
    #> 466   66   y 0.552006
    #> 467   67   y 0.597679
    #> 468   68   y 0.058360
    #> 469   69   y 0.029152
    #> 470   70   y 0.057000
    #> 471   71   y 0.302992
    #> 472   72   y 0.432338
    #> 473   73   y 0.752854
    #> 474   74   y 0.778307
    #> 475   75   y 0.001763
    #> 476   76   y 0.630651
    #> 477   77   y 0.407259
    #> 478   78   y 0.637073
    #> 479   79   y 0.408029
    #> 480   80   y 0.789276
    #> 481   81   y 0.909727
    #> 482   82   y 0.176853
    #> 483   83   y 0.480486
    #> 484   84   y 0.496430
    #> 485   85   y 0.071937
    #> 486   86   y 0.260386
    #> 487   87   y 0.739384
    #> 488   88   y 0.797941
    #> 489   89   y 0.905060
    #> 490   90   y 0.227307
    #> 491   91   y 0.591236
    #> 492   92   y 0.101057
    #> 493   93   y 0.245249
    #> 494   94   y 0.887802
    #> 495   95   y 0.786332
    #> 496   96   y 0.351690
    #> 497   97   y 0.824439
    #> 498   98   y 0.727198
    #> 499   99   y 0.134077
    #> 500  100   y 0.860104
    #> 501  101   y 0.911462
    #> 502  102   y 0.696489
    #> 503  103   y 0.601132
    #> 504  104   y 0.150031
    #> 505  105   y 0.648470
    #> 506  106   y 0.963804
    #> 507  107   y 0.917374
    #> 508  108   y 0.206234
    #> 509  109   y 0.207676
    #> 510  110   y 0.001802
    #> 511  111   y 0.814661
    #> 512  112   y 0.495500
    #> 513  113   y 0.893404
    #> 514  114   y 0.386023
    #> 515  115   y 0.023418
    #> 516  116   y 0.136076
    #> 517  117   y 0.314449
    #> 518  118   y 0.679153
    #> 519  119   y 0.152030
    #> 520  120   y 0.590536
    #> 521  121   y 0.751760
    #> 522  122   y 0.776249
    #> 523  123   y 0.574050
    #> 524  124   y 0.257096
    #> 525  125   y 0.846421
    #> 526  126   y 0.085886
    #> 527  127   y 0.448217
    #> 528  128   y 0.334354
    #> 529  129   y 0.732954
    #> 530  130   y 0.922249
    #> 531  131   y 0.996288
    #> 532  132   y 0.334478
    #> 533  133   y 0.742936
    #> 534  134   y 0.483157
    #> 535  135   y 0.163685
    #> 536  136   y 0.619823
    #> 537  137   y 0.803920
    #> 538  138   y 0.789033
    #> 539  139   y 0.787949
    #> 540  140   y 0.337993
    #> 541  141   y 0.658675
    #> 542  142   y 0.957802
    #> 543  143   y 0.400012
    #> 544  144   y 0.130337
    #> 545  145   y 0.029737
    #> 546  146   y 0.190735
    #> 547  147   y 0.783277
    #> 548  148   y 0.988879
    #> 549  149   y 0.854271
    #> 550  150   y 0.844219
    #> 551  151   y 0.415515
    #> 552  152   y 0.636694
    #> 553  153   y 0.714252
    #> 554  154   y 0.962482
    #> 555  155   y 0.918567
    #> 556  156   y 0.806713
    #> 557  157   y 0.586944
    #> 558  158   y 0.646237
    #> 559  159   y 0.640872
    #> 560  160   y 0.793970
    #> 561  161   y 0.040708
    #> 562  162   y 0.632276
    #> 563  163   y 0.497789
    #> 564  164   y 0.656287
    #> 565  165   y 0.001116
    #> 566  166   y 0.458995
    #> 567  167   y 0.194751
    #> 568  168   y 0.438392
    #> 569  169   y 0.212369
    #> 570  170   y 0.592862
    #> 571  171   y 0.958214
    #> 572  172   y 0.794482
    #> 573  173   y 0.519044
    #> 574  174   y 0.890815
    #> 575  175   y 0.899433
    #> 576  176   y 0.384215
    #> 577  177   y 0.093140
    #> 578  178   y 0.773607
    #> 579  179   y 0.136040
    #> 580  180   y 0.417145
    #> 581  181   y 0.731443
    #> 582  182   y 0.485078
    #> 583  183   y 0.796438
    #> 584  184   y 0.720595
    #> 585  185   y 0.348199
    #> 586  186   y 0.812374
    #> 587  187   y 0.032013
    #> 588  188   y 0.705962
    #> 589  189   y 0.788106
    #> 590  190   y 0.642097
    #> 591  191   y 0.162147
    #> 592  192   y 0.608467
    #> 593  193   y 0.084615
    #> 594  194   y 0.727397
    #> 595  195   y 0.077057
    #> 596  196   y 0.375336
    #> 597  197   y 0.288538
    #> 598  198   y 0.040997
    #> 599  199   y 0.023432
    #> 600  200   y 0.577419
    #> 601  201   y 0.991980
    #> 602  202   y 0.089157
    #> 603  203   y 0.897896
    #> 604  204   y 0.719255
    #> 605  205   y 0.672573
    #> 606  206   y 0.561583
    #> 607  207   y 0.173276
    #> 608  208   y 0.322564
    #> 609  209   y 0.983029
    #> 610  210   y 0.692256
    #> 611  211   y 0.582682
    #> 612  212   y 0.158178
    #> 613  213   y 0.111002
    #> 614  214   y 0.706220
    #> 615  215   y 0.686402
    #> 616  216   y 0.001418
    #> 617  217   y 0.334265
    #> 618  218   y 0.727290
    #> 619  219   y 0.446613
    #> 620  220   y 0.285899
    #> 621  221   y 0.896198
    #> 622  222   y 0.521708
    #> 623  223   y 0.934895
    #> 624  224   y 0.030776
    #> 625  225   y 0.326589
    #> 626  226   y 0.452729
    #> 627  227   y 0.297567
    #> 628  228   y 0.416429
    #> 629  229   y 0.332744
    #> 630  230   y 0.021153
    #> 631  231   y 0.103830
    #> 632  232   y 0.383583
    #> 633  233   y 0.385918
    #> 634  234   y 0.366787
    #> 635  235   y 0.201722
    #> 636  236   y 0.630868
    #> 637  237   y 0.128050
    #> 638  238   y 0.105847
    #> 639  239   y 0.206892
    #> 640  240   y 0.385813
    #> 641  241   y 0.723816
    #> 642  242   y 0.879300
    #> 643  243   y 0.749180
    #> 644  244   y 0.324282
    #> 645  245   y 0.282988
    #> 646  246   y 0.987139
    #> 647  247   y 0.602009
    #> 648  248   y 0.013343
    #> 649  249   y 0.478115
    #> 650  250   y 0.483600
    #> 651  251   y 0.819595
    #> 652  252   y 0.252577
    #> 653  253   y 0.482508
    #> 654  254   y 0.108312
    #> 655  255   y 0.006967
    #> 656  256   y 0.688824
    #> 657  257   y 0.353543
    #> 658  258   y 0.428389
    #> 659  259   y 0.847797
    #> 660  260   y 0.079371
    #> 661  261   y 0.986270
    #> 662  262   y 0.566457
    #> 663  263   y 0.015045
    #> 664  264   y 0.578564
    #> 665  265   y 0.762346
    #> 666  266   y 0.889618
    #> 667  267   y 0.589417
    #> 668  268   y 0.422875
    #> 669  269   y 0.019265
    #> 670  270   y 0.034351
    #> 671  271   y 0.110633
    #> 672  272   y 0.389394
    #> 673  273   y 0.464916
    #> 674  274   y 0.642355
    #> 675  275   y 0.386511
    #> 676  276   y 0.402763
    #> 677  277   y 0.662440
    #> 678  278   y 0.182326
    #> 679  279   y 0.184859
    #> 680  280   y 0.665549
    #> 681  281   y 0.710412
    #> 682  282   y 0.232668
    #> 683  283   y 0.933185
    #> 684  284   y 0.687051
    #> 685  285   y 0.243325
    #> 686  286   y 0.600151
    #> 687  287   y 0.551215
    #> 688  288   y 0.585544
    #> 689  289   y 0.877392
    #> 690  290   y 0.649494
    #> 691  291   y 0.541224
    #> 692  292   y 0.038967
    #> 693  293   y 0.226658
    #> 694  294   y 0.218901
    #> 695  295   y 0.461181
    #> 696  296   y 0.259041
    #> 697  297   y 0.614429
    #> 698  298   y 0.496517
    #> 699  299   y 0.905709
    #> 700  300   y 0.363830
    #> 701  301   y 0.605007
    #> 702  302   y 0.232836
    #> 703  303   y 0.119851
    #> 704  304   y 0.523734
    #> 705  305   y 0.480742
    #> 706  306   y 0.664040
    #> 707  307   y 0.370651
    #> 708  308   y 0.255925
    #> 709  309   y 0.789398
    #> 710  310   y 0.521274
    #> 711  311   y 0.201555
    #> 712  312   y 0.242217
    #> 713  313   y 0.086822
    #> 714  314   y 0.500864
    #> 715  315   y 0.694610
    #> 716  316   y 0.045376
    #> 717  317   y 0.999939
    #> 718  318   y 0.570467
    #> 719  319   y 0.865490
    #> 720  320   y 0.098863
    #> 721  321   y 0.735050
    #> 722  322   y 0.486162
    #> 723  323   y 0.316322
    #> 724  324   y 0.345021
    #> 725  325   y 0.156445
    #> 726  326   y 0.895476
    #> 727  327   y 0.271338
    #> 728  328   y 0.896695
    #> 729  329   y 0.560330
    #> 730  330   y 0.401351
    #> 731  331   y 0.120324
    #> 732  332   y 0.097289
    #> 733  333   y 0.269064
    #> 734  334   y 0.962044
    #> 735  335   y 0.594896
    #> 736  336   y 0.354264
    #> 737  337   y 0.170710
    #> 738  338   y 0.001970
    #> 739  339   y 0.702578
    #> 740  340   y 0.121073
    #> 741  341   y 0.328899
    #> 742  342   y 0.608475
    #> 743  343   y 0.543698
    #> 744  344   y 0.402526
    #> 745  345   y 0.788006
    #> 746  346   y 0.189554
    #> 747  347   y 0.136097
    #> 748  348   y 0.158607
    #> 749  349   y 0.698637
    #> 750  350   y 0.967504
    #> 751  351   y 0.372643
    #> 752  352   y 0.981712
    #> 753  353   y 0.888431
    #> 754  354   y 0.683509
    #> 755  355   y 0.236571
    #> 756  356   y 0.422338
    #> 757  357   y 0.003171
    #> 758  358   y 0.888175
    #> 759  359   y 0.399618
    #> 760  360   y 0.338204
    #> 761  361   y 0.343214
    #> 762  362   y 0.192988
    #> 763  363   y 0.327989
    #> 764  364   y 0.641805
    #> 765  365   y 0.520226
    #> 766  366   y 0.857719
    #> 767  367   y 0.021121
    #> 768  368   y 0.821414
    #> 769  369   y 0.059233
    #> 770  370   y 0.088764
    #> 771  371   y 0.508267
    #> 772  372   y 0.610511
    #> 773  373   y 0.070984
    #> 774  374   y 0.423421
    #> 775  375   y 0.227890
    #> 776  376   y 0.180656
    #> 777  377   y 0.119631
    #> 778  378   y 0.575105
    #> 779  379   y 0.414871
    #> 780  380   y 0.232798
    #> 781  381   y 0.410710
    #> 782  382   y 0.114502
    #> 783  383   y 0.620527
    #> 784  384   y 0.362020
    #> 785  385   y 0.424449
    #> 786  386   y 0.361654
    #> 787  387   y 0.490444
    #> 788  388   y 0.070724
    #> 789  389   y 0.119375
    #> 790  390   y 0.863991
    #> 791  391   y 0.925122
    #> 792  392   y 0.805249
    #> 793  393   y 0.831245
    #> 794  394   y 0.835295
    #> 795  395   y 0.748429
    #> 796  396   y 0.390935
    #> 797  397   y 0.992282
    #> 798  398   y 0.930601
    #> 799  399   y 0.008876
    #> 800  400   y 0.240614
    #> 801    1   z 0.000824
    #> 802    2   z 0.533939
    #> 803    3   z 0.838542
    #> 804    4   z 0.990648
    #> 805    5   z 0.418881
    #> 806    6   z 0.777664
    #> 807    7   z 0.413932
    #> 808    8   z 0.884338
    #> 809    9   z 0.501968
    #> 810   10   z 0.678107
    #> 811   11   z 0.860718
    #> 812   12   z 0.769314
    #> 813   13   z 0.319211
    #> 814   14   z 0.908380
    #> 815   15   z 0.370327
    #> 816   16   z 0.037394
    #> 817   17   z 0.707165
    #> 818   18   z 0.626002
    #> 819   19   z 0.844727
    #> 820   20   z 0.741801
    #> 821   21   z 0.224398
    #> 822   22   z 0.438229
    #> 823   23   z 0.472110
    #> 824   24   z 0.488282
    #> 825   25   z 0.692023
    #> 826   26   z 0.750198
    #> 827   27   z 0.326013
    #> 828   28   z 0.021528
    #> 829   29   z 0.695158
    #> 830   30   z 0.620887
    #> 831   31   z 0.362880
    #> 832   32   z 0.631916
    #> 833   33   z 0.420333
    #> 834   34   z 0.251881
    #> 835   35   z 0.193051
    #> 836   36   z 0.507559
    #> 837   37   z 0.203826
    #> 838   38   z 0.233957
    #> 839   39   z 0.000203
    #> 840   40   z 0.156666
    #> 841   41   z 0.165943
    #> 842   42   z 0.676477
    #> 843   43   z 0.984216
    #> 844   44   z 0.025225
    #> 845   45   z 0.306233
    #> 846   46   z 0.344595
    #> 847   47   z 0.702484
    #> 848   48   z 0.434983
    #> 849   49   z 0.434638
    #> 850   50   z 0.929275
    #> 851   51   z 0.684661
    #> 852   52   z 0.966753
    #> 853   53   z 0.313584
    #> 854   54   z 0.575129
    #> 855   55   z 0.695843
    #> 856   56   z 0.414026
    #> 857   57   z 0.386009
    #> 858   58   z 0.791052
    #> 859   59   z 0.023192
    #> 860   60   z 0.431892
    #> 861   61   z 0.464327
    #> 862   62   z 0.868689
    #> 863   63   z 0.160990
    #> 864   64   z 0.006286
    #> 865   65   z 0.755330
    #> 866   66   z 0.933698
    #> 867   67   z 0.269141
    #> 868   68   z 0.868532
    #> 869   69   z 0.596032
    #> 870   70   z 0.754833
    #> 871   71   z 0.780100
    #> 872   72   z 0.980210
    #> 873   73   z 0.275437
    #> 874   74   z 0.435841
    #> 875   75   z 0.529245
    #> 876   76   z 0.213333
    #> 877   77   z 0.317265
    #> 878   78   z 0.115167
    #> 879   79   z 0.783687
    #> 880   80   z 0.350829
    #> 881   81   z 0.598596
    #> 882   82   z 0.798474
    #> 883   83   z 0.544515
    #> 884   84   z 0.532351
    #> 885   85   z 0.679465
    #> 886   86   z 0.448975
    #> 887   87   z 0.497480
    #> 888   88   z 0.270380
    #> 889   89   z 0.718384
    #> 890   90   z 0.460576
    #> 891   91   z 0.987116
    #> 892   92   z 0.161660
    #> 893   93   z 0.348813
    #> 894   94   z 0.638839
    #> 895   95   z 0.389299
    #> 896   96   z 0.418931
    #> 897   97   z 0.891560
    #> 898   98   z 0.814741
    #> 899   99   z 0.257241
    #> 900  100   z 0.347981
    #> 901  101   z 0.278116
    #> 902  102   z 0.216269
    #> 903  103   z 0.615928
    #> 904  104   z 0.895009
    #> 905  105   z 0.059942
    #> 906  106   z 0.722801
    #> 907  107   z 0.779985
    #> 908  108   z 0.389302
    #> 909  109   z 0.878674
    #> 910  110   z 0.127325
    #> 911  111   z 0.069990
    #> 912  112   z 0.606222
    #> 913  113   z 0.772282
    #> 914  114   z 0.531450
    #> 915  115   z 0.802219
    #> 916  116   z 0.275507
    #> 917  117   z 0.692158
    #> 918  118   z 0.998243
    #> 919  119   z 0.926825
    #> 920  120   z 0.159884
    #> 921  121   z 0.606141
    #> 922  122   z 0.587872
    #> 923  123   z 0.689467
    #> 924  124   z 0.828997
    #> 925  125   z 0.562486
    #> 926  126   z 0.901061
    #> 927  127   z 0.691361
    #> 928  128   z 0.253643
    #> 929  129   z 0.089762
    #> 930  130   z 0.255363
    #> 931  131   z 0.722781
    #> 932  132   z 0.345322
    #> 933  133   z 0.307697
    #> 934  134   z 0.619240
    #> 935  135   z 0.744991
    #> 936  136   z 0.566103
    #> 937  137   z 0.123949
    #> 938  138   z 0.428958
    #> 939  139   z 0.395616
    #> 940  140   z 0.733798
    #> 941  141   z 0.891827
    #> 942  142   z 0.413294
    #> 943  143   z 0.392450
    #> 944  144   z 0.147450
    #> 945  145   z 0.903937
    #> 946  146   z 0.551055
    #> 947  147   z 0.210702
    #> 948  148   z 0.158136
    #> 949  149   z 0.096780
    #> 950  150   z 0.286411
    #> 951  151   z 0.408684
    #> 952  152   z 0.310077
    #> 953  153   z 0.341272
    #> 954  154   z 0.127040
    #> 955  155   z 0.976924
    #> 956  156   z 0.177067
    #> 957  157   z 0.694211
    #> 958  158   z 0.750083
    #> 959  159   z 0.085731
    #> 960  160   z 0.019209
    #> 961  161   z 0.985681
    #> 962  162   z 0.740914
    #> 963  163   z 0.606191
    #> 964  164   z 0.384968
    #> 965  165   z 0.117391
    #> 966  166   z 0.089724
    #> 967  167   z 0.779596
    #> 968  168   z 0.783530
    #> 969  169   z 0.446952
    #> 970  170   z 0.570913
    #> 971  171   z 0.410318
    #> 972  172   z 0.552483
    #> 973  173   z 0.633091
    #> 974  174   z 0.130301
    #> 975  175   z 0.957119
    #> 976  176   z 0.045802
    #> 977  177   z 0.316805
    #> 978  178   z 0.405140
    #> 979  179   z 0.897895
    #> 980  180   z 0.267152
    #> 981  181   z 0.013450
    #> 982  182   z 0.501955
    #> 983  183   z 0.742860
    #> 984  184   z 0.073498
    #> 985  185   z 0.590769
    #> 986  186   z 0.157183
    #> 987  187   z 0.071166
    #> 988  188   z 0.074034
    #> 989  189   z 0.707856
    #> 990  190   z 0.405631
    #> 991  191   z 0.965964
    #> 992  192   z 0.349420
    #> 993  193   z 0.590057
    #> 994  194   z 0.881746
    #> 995  195   z 0.222900
    #> 996  196   z 0.116333
    #> 997  197   z 0.514816
    #> 998  198   z 0.878697
    #> 999  199   z 0.707045
    #> 1000 200   z 0.462329
    #> 1001 201   z 0.383442
    #> 1002 202   z 0.290269
    #> 1003 203   z 0.233271
    #> 1004 204   z 0.276022
    #> 1005 205   z 0.753833
    #> 1006 206   z 0.560572
    #> 1007 207   z 0.345054
    #> 1008 208   z 0.497568
    #> 1009 209   z 0.709126
    #> 1010 210   z 0.766557
    #> 1011 211   z 0.366728
    #> 1012 212   z 0.814469
    #> 1013 213   z 0.937284
    #> 1014 214   z 0.958351
    #> 1015 215   z 0.104159
    #> 1016 216   z 0.927685
    #> 1017 217   z 0.396624
    #> 1018 218   z 0.882402
    #> 1019 219   z 0.568873
    #> 1020 220   z 0.513061
    #> 1021 221   z 0.942288
    #> 1022 222   z 0.212432
    #> 1023 223   z 0.100615
    #> 1024 224   z 0.003063
    #> 1025 225   z 0.288634
    #> 1026 226   z 0.413211
    #> 1027 227   z 0.263338
    #> 1028 228   z 0.313830
    #> 1029 229   z 0.682588
    #> 1030 230   z 0.362135
    #> 1031 231   z 0.889644
    #> 1032 232   z 0.642540
    #> 1033 233   z 0.662911
    #> 1034 234   z 0.853626
    #> 1035 235   z 0.648653
    #> 1036 236   z 0.460078
    #> 1037 237   z 0.280421
    #> 1038 238   z 0.111334
    #> 1039 239   z 0.512309
    #> 1040 240   z 0.824036
    #> 1041 241   z 0.154143
    #> 1042 242   z 0.433157
    #> 1043 243   z 0.495129
    #> 1044 244   z 0.137001
    #> 1045 245   z 0.738398
    #> 1046 246   z 0.094077
    #> 1047 247   z 0.042384
    #> 1048 248   z 0.475179
    #> 1049 249   z 0.175830
    #> 1050 250   z 0.631798
    #> 1051 251   z 0.441726
    #> 1052 252   z 0.612313
    #> 1053 253   z 0.111374
    #> 1054 254   z 0.640212
    #> 1055 255   z 0.633237
    #> 1056 256   z 0.863928
    #> 1057 257   z 0.842153
    #> 1058 258   z 0.195659
    #> 1059 259   z 0.792934
    #> 1060 260   z 0.876876
    #> 1061 261   z 0.178324
    #> 1062 262   z 0.041016
    #> 1063 263   z 0.064698
    #> 1064 264   z 0.489201
    #> 1065 265   z 0.389852
    #> 1066 266   z 0.652587
    #> 1067 267   z 0.815645
    #> 1068 268   z 0.785317
    #> 1069 269   z 0.614222
    #> 1070 270   z 0.314814
    #> 1071 271   z 0.789938
    #> 1072 272   z 0.471495
    #> 1073 273   z 0.100099
    #> 1074 274   z 0.327792
    #> 1075 275   z 0.536028
    #> 1076 276   z 0.696662
    #> 1077 277   z 0.661911
    #> 1078 278   z 0.472607
    #> 1079 279   z 0.482342
    #> 1080 280   z 0.443517
    #> 1081 281   z 0.720379
    #> 1082 282   z 0.859474
    #> 1083 283   z 0.036403
    #> 1084 284   z 0.614955
    #> 1085 285   z 0.303981
    #> 1086 286   z 0.283698
    #> 1087 287   z 0.082387
    #> 1088 288   z 0.940804
    #> 1089 289   z 0.386357
    #> 1090 290   z 0.214444
    #> 1091 291   z 0.252119
    #> 1092 292   z 0.829882
    #> 1093 293   z 0.934643
    #> 1094 294   z 0.541315
    #> 1095 295   z 0.344511
    #> 1096 296   z 0.292351
    #> 1097 297   z 0.043756
    #> 1098 298   z 0.203753
    #> 1099 299   z 0.268427
    #> 1100 300   z 0.057402
    #> 1101 301   z 0.531603
    #> 1102 302   z 0.828238
    #> 1103 303   z 0.883996
    #> 1104 304   z 0.011235
    #> 1105 305   z 0.370236
    #> 1106 306   z 0.498314
    #> 1107 307   z 0.117356
    #> 1108 308   z 0.080367
    #> 1109 309   z 0.327940
    #> 1110 310   z 0.782420
    #> 1111 311   z 0.723837
    #> 1112 312   z 0.685240
    #> 1113 313   z 0.197263
    #> 1114 314   z 0.144530
    #> 1115 315   z 0.074585
    #> 1116 316   z 0.889149
    #> 1117 317   z 0.983978
    #> 1118 318   z 0.862617
    #> 1119 319   z 0.341617
    #> 1120 320   z 0.367481
    #> 1121 321   z 0.431987
    #> 1122 322   z 0.579916
    #> 1123 323   z 0.456322
    #> 1124 324   z 0.322264
    #> 1125 325   z 0.259161
    #> 1126 326   z 0.614010
    #> 1127 327   z 0.216388
    #> 1128 328   z 0.467031
    #> 1129 329   z 0.479117
    #> 1130 330   z 0.148725
    #> 1131 331   z 0.916178
    #> 1132 332   z 0.206997
    #> 1133 333   z 0.183931
    #> 1134 334   z 0.433832
    #> 1135 335   z 0.875538
    #> 1136 336   z 0.139545
    #> 1137 337   z 0.162795
    #> 1138 338   z 0.117574
    #> 1139 339   z 0.242041
    #> 1140 340   z 0.000029
    #> 1141 341   z 0.731601
    #> 1142 342   z 0.836991
    #> 1143 343   z 0.441672
    #> 1144 344   z 0.177886
    #> 1145 345   z 0.148473
    #> 1146 346   z 0.191069
    #> 1147 347   z 0.652950
    #> 1148 348   z 0.928060
    #> 1149 349   z 0.990230
    #> 1150 350   z 0.221694
    #> 1151 351   z 0.679484
    #> 1152 352   z 0.402746
    #> 1153 353   z 0.864786
    #> 1154 354   z 0.527427
    #> 1155 355   z 0.595973
    #> 1156 356   z 0.628435
    #> 1157 357   z 0.834495
    #> 1158 358   z 0.135077
    #> 1159 359   z 0.542634
    #> 1160 360   z 0.553278
    #> 1161 361   z 0.925426
    #> 1162 362   z 0.254105
    #> 1163 363   z 0.043078
    #> 1164 364   z 0.289766
    #> 1165 365   z 0.097574
    #> 1166 366   z 0.038824
    #> 1167 367   z 0.220619
    #> 1168 368   z 0.677714
    #> 1169 369   z 0.051020
    #> 1170 370   z 0.483424
    #> 1171 371   z 0.288014
    #> 1172 372   z 0.292563
    #> 1173 373   z 0.243013
    #> 1174 374   z 0.574797
    #> 1175 375   z 0.685654
    #> 1176 376   z 0.023994
    #> 1177 377   z 0.491003
    #> 1178 378   z 0.828191
    #> 1179 379   z 0.243179
    #> 1180 380   z 0.349853
    #> 1181 381   z 0.536605
    #> 1182 382   z 0.330657
    #> 1183 383   z 0.739542
    #> 1184 384   z 0.430390
    #> 1185 385   z 0.945496
    #> 1186 386   z 0.417328
    #> 1187 387   z 0.236500
    #> 1188 388   z 0.147809
    #> 1189 389   z 0.718263
    #> 1190 390   z 0.105492
    #> 1191 391   z 0.560552
    #> 1192 392   z 0.216132
    #> 1193 393   z 0.987175
    #> 1194 394   z 0.411495
    #> 1195 395   z 0.308307
    #> 1196 396   z 0.486373
    #> 1197 397   z 0.173898
    #> 1198 398   z 0.675440
    #> 1199 399   z 0.750293
    #> 1200 400   z 0.572030

rock
----

``` r
rock_tidy <- rock %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs) 
rock_tidy
```

    #>     obs   key       value
    #> 1     1  area 4.99000e+03
    #> 2     2  area 7.00200e+03
    #> 3     3  area 7.55800e+03
    #> 4     4  area 7.35200e+03
    #> 5     5  area 7.94300e+03
    #> 6     6  area 7.97900e+03
    #> 7     7  area 9.33300e+03
    #> 8     8  area 8.20900e+03
    #> 9     9  area 8.39300e+03
    #> 10   10  area 6.42500e+03
    #> 11   11  area 9.36400e+03
    #> 12   12  area 8.62400e+03
    #> 13   13  area 1.06510e+04
    #> 14   14  area 8.86800e+03
    #> 15   15  area 9.41700e+03
    #> 16   16  area 8.87400e+03
    #> 17   17  area 1.09620e+04
    #> 18   18  area 1.07430e+04
    #> 19   19  area 1.18780e+04
    #> 20   20  area 9.86700e+03
    #> 21   21  area 7.83800e+03
    #> 22   22  area 1.18760e+04
    #> 23   23  area 1.22120e+04
    #> 24   24  area 8.23300e+03
    #> 25   25  area 6.36000e+03
    #> 26   26  area 4.19300e+03
    #> 27   27  area 7.41600e+03
    #> 28   28  area 5.24600e+03
    #> 29   29  area 6.50900e+03
    #> 30   30  area 4.89500e+03
    #> 31   31  area 6.77500e+03
    #> 32   32  area 7.89400e+03
    #> 33   33  area 5.98000e+03
    #> 34   34  area 5.31800e+03
    #> 35   35  area 7.39200e+03
    #> 36   36  area 7.89400e+03
    #> 37   37  area 3.46900e+03
    #> 38   38  area 1.46800e+03
    #> 39   39  area 3.52400e+03
    #> 40   40  area 5.26700e+03
    #> 41   41  area 5.04800e+03
    #> 42   42  area 1.01600e+03
    #> 43   43  area 5.60500e+03
    #> 44   44  area 8.79300e+03
    #> 45   45  area 3.47500e+03
    #> 46   46  area 1.65100e+03
    #> 47   47  area 5.51400e+03
    #> 48   48  area 9.71800e+03
    #> 49    1  peri 2.79190e+03
    #> 50    2  peri 3.89260e+03
    #> 51    3  peri 3.93066e+03
    #> 52    4  peri 3.86932e+03
    #> 53    5  peri 3.94854e+03
    #> 54    6  peri 4.01015e+03
    #> 55    7  peri 4.34575e+03
    #> 56    8  peri 4.34475e+03
    #> 57    9  peri 3.68204e+03
    #> 58   10  peri 3.09865e+03
    #> 59   11  peri 4.48005e+03
    #> 60   12  peri 3.98624e+03
    #> 61   13  peri 4.03654e+03
    #> 62   14  peri 3.51804e+03
    #> 63   15  peri 3.99937e+03
    #> 64   16  peri 3.62907e+03
    #> 65   17  peri 4.60866e+03
    #> 66   18  peri 4.78762e+03
    #> 67   19  peri 4.86422e+03
    #> 68   20  peri 4.47941e+03
    #> 69   21  peri 3.42874e+03
    #> 70   22  peri 4.35314e+03
    #> 71   23  peri 4.69765e+03
    #> 72   24  peri 3.51844e+03
    #> 73   25  peri 1.97739e+03
    #> 74   26  peri 1.37935e+03
    #> 75   27  peri 1.91624e+03
    #> 76   28  peri 1.58542e+03
    #> 77   29  peri 1.85121e+03
    #> 78   30  peri 1.23966e+03
    #> 79   31  peri 1.72814e+03
    #> 80   32  peri 1.46106e+03
    #> 81   33  peri 1.42676e+03
    #> 82   34  peri 9.90388e+02
    #> 83   35  peri 1.35076e+03
    #> 84   36  peri 1.46106e+03
    #> 85   37  peri 1.37670e+03
    #> 86   38  peri 4.76322e+02
    #> 87   39  peri 1.18946e+03
    #> 88   40  peri 1.64496e+03
    #> 89   41  peri 9.41543e+02
    #> 90   42  peri 3.08642e+02
    #> 91   43  peri 1.14569e+03
    #> 92   44  peri 2.28049e+03
    #> 93   45  peri 1.17411e+03
    #> 94   46  peri 5.97808e+02
    #> 95   47  peri 1.45588e+03
    #> 96   48  peri 1.48558e+03
    #> 97    1 shape 9.03296e-02
    #> 98    2 shape 1.48622e-01
    #> 99    3 shape 1.83312e-01
    #> 100   4 shape 1.17063e-01
    #> 101   5 shape 1.22417e-01
    #> 102   6 shape 1.67045e-01
    #> 103   7 shape 1.89651e-01
    #> 104   8 shape 1.64127e-01
    #> 105   9 shape 2.03654e-01
    #> 106  10 shape 1.62394e-01
    #> 107  11 shape 1.50944e-01
    #> 108  12 shape 1.48141e-01
    #> 109  13 shape 2.28595e-01
    #> 110  14 shape 2.31623e-01
    #> 111  15 shape 1.72567e-01
    #> 112  16 shape 1.53481e-01
    #> 113  17 shape 2.04314e-01
    #> 114  18 shape 2.62727e-01
    #> 115  19 shape 2.00071e-01
    #> 116  20 shape 1.44810e-01
    #> 117  21 shape 1.13852e-01
    #> 118  22 shape 2.91029e-01
    #> 119  23 shape 2.40077e-01
    #> 120  24 shape 1.61865e-01
    #> 121  25 shape 2.80887e-01
    #> 122  26 shape 1.79455e-01
    #> 123  27 shape 1.91802e-01
    #> 124  28 shape 1.33083e-01
    #> 125  29 shape 2.25214e-01
    #> 126  30 shape 3.41273e-01
    #> 127  31 shape 3.11646e-01
    #> 128  32 shape 2.76016e-01
    #> 129  33 shape 1.97653e-01
    #> 130  34 shape 3.26635e-01
    #> 131  35 shape 1.54192e-01
    #> 132  36 shape 2.76016e-01
    #> 133  37 shape 1.76969e-01
    #> 134  38 shape 4.38712e-01
    #> 135  39 shape 1.63586e-01
    #> 136  40 shape 2.53832e-01
    #> 137  41 shape 3.28641e-01
    #> 138  42 shape 2.30081e-01
    #> 139  43 shape 4.64125e-01
    #> 140  44 shape 4.20477e-01
    #> 141  45 shape 2.00744e-01
    #> 142  46 shape 2.62651e-01
    #> 143  47 shape 1.82453e-01
    #> 144  48 shape 2.00447e-01
    #> 145   1  perm 6.30000e+00
    #> 146   2  perm 6.30000e+00
    #> 147   3  perm 6.30000e+00
    #> 148   4  perm 6.30000e+00
    #> 149   5  perm 1.71000e+01
    #> 150   6  perm 1.71000e+01
    #> 151   7  perm 1.71000e+01
    #> 152   8  perm 1.71000e+01
    #> 153   9  perm 1.19000e+02
    #> 154  10  perm 1.19000e+02
    #> 155  11  perm 1.19000e+02
    #> 156  12  perm 1.19000e+02
    #> 157  13  perm 8.24000e+01
    #> 158  14  perm 8.24000e+01
    #> 159  15  perm 8.24000e+01
    #> 160  16  perm 8.24000e+01
    #> 161  17  perm 5.86000e+01
    #> 162  18  perm 5.86000e+01
    #> 163  19  perm 5.86000e+01
    #> 164  20  perm 5.86000e+01
    #> 165  21  perm 1.42000e+02
    #> 166  22  perm 1.42000e+02
    #> 167  23  perm 1.42000e+02
    #> 168  24  perm 1.42000e+02
    #> 169  25  perm 7.40000e+02
    #> 170  26  perm 7.40000e+02
    #> 171  27  perm 7.40000e+02
    #> 172  28  perm 7.40000e+02
    #> 173  29  perm 8.90000e+02
    #> 174  30  perm 8.90000e+02
    #> 175  31  perm 8.90000e+02
    #> 176  32  perm 8.90000e+02
    #> 177  33  perm 9.50000e+02
    #> 178  34  perm 9.50000e+02
    #> 179  35  perm 9.50000e+02
    #> 180  36  perm 9.50000e+02
    #> 181  37  perm 1.00000e+02
    #> 182  38  perm 1.00000e+02
    #> 183  39  perm 1.00000e+02
    #> 184  40  perm 1.00000e+02
    #> 185  41  perm 1.30000e+03
    #> 186  42  perm 1.30000e+03
    #> 187  43  perm 1.30000e+03
    #> 188  44  perm 1.30000e+03
    #> 189  45  perm 5.80000e+02
    #> 190  46  perm 5.80000e+02
    #> 191  47  perm 5.80000e+02
    #> 192  48  perm 5.80000e+02

sleep
-----

stackloss
---------

``` r
stackloss_tidy <- stackloss %>%
  mutate(obs = seq_len(n())) %>%
  gather(key, value, -obs)
stackloss_tidy
```

    #>    obs        key value
    #> 1    1   Air.Flow    80
    #> 2    2   Air.Flow    80
    #> 3    3   Air.Flow    75
    #> 4    4   Air.Flow    62
    #> 5    5   Air.Flow    62
    #> 6    6   Air.Flow    62
    #> 7    7   Air.Flow    62
    #> 8    8   Air.Flow    62
    #> 9    9   Air.Flow    58
    #> 10  10   Air.Flow    58
    #> 11  11   Air.Flow    58
    #> 12  12   Air.Flow    58
    #> 13  13   Air.Flow    58
    #> 14  14   Air.Flow    58
    #> 15  15   Air.Flow    50
    #> 16  16   Air.Flow    50
    #> 17  17   Air.Flow    50
    #> 18  18   Air.Flow    50
    #> 19  19   Air.Flow    50
    #> 20  20   Air.Flow    56
    #> 21  21   Air.Flow    70
    #> 22   1 Water.Temp    27
    #> 23   2 Water.Temp    27
    #> 24   3 Water.Temp    25
    #> 25   4 Water.Temp    24
    #> 26   5 Water.Temp    22
    #> 27   6 Water.Temp    23
    #> 28   7 Water.Temp    24
    #> 29   8 Water.Temp    24
    #> 30   9 Water.Temp    23
    #> 31  10 Water.Temp    18
    #> 32  11 Water.Temp    18
    #> 33  12 Water.Temp    17
    #> 34  13 Water.Temp    18
    #> 35  14 Water.Temp    19
    #> 36  15 Water.Temp    18
    #> 37  16 Water.Temp    18
    #> 38  17 Water.Temp    19
    #> 39  18 Water.Temp    19
    #> 40  19 Water.Temp    20
    #> 41  20 Water.Temp    20
    #> 42  21 Water.Temp    20
    #> 43   1 Acid.Conc.    89
    #> 44   2 Acid.Conc.    88
    #> 45   3 Acid.Conc.    90
    #> 46   4 Acid.Conc.    87
    #> 47   5 Acid.Conc.    87
    #> 48   6 Acid.Conc.    87
    #> 49   7 Acid.Conc.    93
    #> 50   8 Acid.Conc.    93
    #> 51   9 Acid.Conc.    87
    #> 52  10 Acid.Conc.    80
    #> 53  11 Acid.Conc.    89
    #> 54  12 Acid.Conc.    88
    #> 55  13 Acid.Conc.    82
    #> 56  14 Acid.Conc.    93
    #> 57  15 Acid.Conc.    89
    #> 58  16 Acid.Conc.    86
    #> 59  17 Acid.Conc.    72
    #> 60  18 Acid.Conc.    79
    #> 61  19 Acid.Conc.    80
    #> 62  20 Acid.Conc.    82
    #> 63  21 Acid.Conc.    91
    #> 64   1 stack.loss    42
    #> 65   2 stack.loss    37
    #> 66   3 stack.loss    37
    #> 67   4 stack.loss    28
    #> 68   5 stack.loss    18
    #> 69   6 stack.loss    18
    #> 70   7 stack.loss    19
    #> 71   8 stack.loss    20
    #> 72   9 stack.loss    15
    #> 73  10 stack.loss    14
    #> 74  11 stack.loss    14
    #> 75  12 stack.loss    13
    #> 76  13 stack.loss    11
    #> 77  14 stack.loss    12
    #> 78  15 stack.loss     8
    #> 79  16 stack.loss     7
    #> 80  17 stack.loss     8
    #> 81  18 stack.loss     8
    #> 82  19 stack.loss     9
    #> 83  20 stack.loss    15
    #> 84  21 stack.loss    15

swiss
-----

``` r
swiss_tidy <- swiss %>%
  mutate(region = rownames(.)) %>%
  gather(key, value, -region)
swiss_tidy
```

    #>           region              key  value
    #> 1     Courtelary        Fertility  80.20
    #> 2       Delemont        Fertility  83.10
    #> 3   Franches-Mnt        Fertility  92.50
    #> 4        Moutier        Fertility  85.80
    #> 5     Neuveville        Fertility  76.90
    #> 6     Porrentruy        Fertility  76.10
    #> 7          Broye        Fertility  83.80
    #> 8          Glane        Fertility  92.40
    #> 9        Gruyere        Fertility  82.40
    #> 10        Sarine        Fertility  82.90
    #> 11       Veveyse        Fertility  87.10
    #> 12         Aigle        Fertility  64.10
    #> 13       Aubonne        Fertility  66.90
    #> 14      Avenches        Fertility  68.90
    #> 15      Cossonay        Fertility  61.70
    #> 16     Echallens        Fertility  68.30
    #> 17      Grandson        Fertility  71.70
    #> 18      Lausanne        Fertility  55.70
    #> 19     La Vallee        Fertility  54.30
    #> 20        Lavaux        Fertility  65.10
    #> 21        Morges        Fertility  65.50
    #> 22        Moudon        Fertility  65.00
    #> 23         Nyone        Fertility  56.60
    #> 24          Orbe        Fertility  57.40
    #> 25          Oron        Fertility  72.50
    #> 26       Payerne        Fertility  74.20
    #> 27  Paysd'enhaut        Fertility  72.00
    #> 28         Rolle        Fertility  60.50
    #> 29         Vevey        Fertility  58.30
    #> 30       Yverdon        Fertility  65.40
    #> 31       Conthey        Fertility  75.50
    #> 32     Entremont        Fertility  69.30
    #> 33        Herens        Fertility  77.30
    #> 34      Martigwy        Fertility  70.50
    #> 35       Monthey        Fertility  79.40
    #> 36    St Maurice        Fertility  65.00
    #> 37        Sierre        Fertility  92.20
    #> 38          Sion        Fertility  79.30
    #> 39        Boudry        Fertility  70.40
    #> 40  La Chauxdfnd        Fertility  65.70
    #> 41      Le Locle        Fertility  72.70
    #> 42     Neuchatel        Fertility  64.40
    #> 43    Val de Ruz        Fertility  77.60
    #> 44  ValdeTravers        Fertility  67.60
    #> 45  V. De Geneve        Fertility  35.00
    #> 46   Rive Droite        Fertility  44.70
    #> 47   Rive Gauche        Fertility  42.80
    #> 48    Courtelary      Agriculture  17.00
    #> 49      Delemont      Agriculture  45.10
    #> 50  Franches-Mnt      Agriculture  39.70
    #> 51       Moutier      Agriculture  36.50
    #> 52    Neuveville      Agriculture  43.50
    #> 53    Porrentruy      Agriculture  35.30
    #> 54         Broye      Agriculture  70.20
    #> 55         Glane      Agriculture  67.80
    #> 56       Gruyere      Agriculture  53.30
    #> 57        Sarine      Agriculture  45.20
    #> 58       Veveyse      Agriculture  64.50
    #> 59         Aigle      Agriculture  62.00
    #> 60       Aubonne      Agriculture  67.50
    #> 61      Avenches      Agriculture  60.70
    #> 62      Cossonay      Agriculture  69.30
    #> 63     Echallens      Agriculture  72.60
    #> 64      Grandson      Agriculture  34.00
    #> 65      Lausanne      Agriculture  19.40
    #> 66     La Vallee      Agriculture  15.20
    #> 67        Lavaux      Agriculture  73.00
    #> 68        Morges      Agriculture  59.80
    #> 69        Moudon      Agriculture  55.10
    #> 70         Nyone      Agriculture  50.90
    #> 71          Orbe      Agriculture  54.10
    #> 72          Oron      Agriculture  71.20
    #> 73       Payerne      Agriculture  58.10
    #> 74  Paysd'enhaut      Agriculture  63.50
    #> 75         Rolle      Agriculture  60.80
    #> 76         Vevey      Agriculture  26.80
    #> 77       Yverdon      Agriculture  49.50
    #> 78       Conthey      Agriculture  85.90
    #> 79     Entremont      Agriculture  84.90
    #> 80        Herens      Agriculture  89.70
    #> 81      Martigwy      Agriculture  78.20
    #> 82       Monthey      Agriculture  64.90
    #> 83    St Maurice      Agriculture  75.90
    #> 84        Sierre      Agriculture  84.60
    #> 85          Sion      Agriculture  63.10
    #> 86        Boudry      Agriculture  38.40
    #> 87  La Chauxdfnd      Agriculture   7.70
    #> 88      Le Locle      Agriculture  16.70
    #> 89     Neuchatel      Agriculture  17.60
    #> 90    Val de Ruz      Agriculture  37.60
    #> 91  ValdeTravers      Agriculture  18.70
    #> 92  V. De Geneve      Agriculture   1.20
    #> 93   Rive Droite      Agriculture  46.60
    #> 94   Rive Gauche      Agriculture  27.70
    #> 95    Courtelary      Examination  15.00
    #> 96      Delemont      Examination   6.00
    #> 97  Franches-Mnt      Examination   5.00
    #> 98       Moutier      Examination  12.00
    #> 99    Neuveville      Examination  17.00
    #> 100   Porrentruy      Examination   9.00
    #> 101        Broye      Examination  16.00
    #> 102        Glane      Examination  14.00
    #> 103      Gruyere      Examination  12.00
    #> 104       Sarine      Examination  16.00
    #> 105      Veveyse      Examination  14.00
    #> 106        Aigle      Examination  21.00
    #> 107      Aubonne      Examination  14.00
    #> 108     Avenches      Examination  19.00
    #> 109     Cossonay      Examination  22.00
    #> 110    Echallens      Examination  18.00
    #> 111     Grandson      Examination  17.00
    #> 112     Lausanne      Examination  26.00
    #> 113    La Vallee      Examination  31.00
    #> 114       Lavaux      Examination  19.00
    #> 115       Morges      Examination  22.00
    #> 116       Moudon      Examination  14.00
    #> 117        Nyone      Examination  22.00
    #> 118         Orbe      Examination  20.00
    #> 119         Oron      Examination  12.00
    #> 120      Payerne      Examination  14.00
    #> 121 Paysd'enhaut      Examination   6.00
    #> 122        Rolle      Examination  16.00
    #> 123        Vevey      Examination  25.00
    #> 124      Yverdon      Examination  15.00
    #> 125      Conthey      Examination   3.00
    #> 126    Entremont      Examination   7.00
    #> 127       Herens      Examination   5.00
    #> 128     Martigwy      Examination  12.00
    #> 129      Monthey      Examination   7.00
    #> 130   St Maurice      Examination   9.00
    #> 131       Sierre      Examination   3.00
    #> 132         Sion      Examination  13.00
    #> 133       Boudry      Examination  26.00
    #> 134 La Chauxdfnd      Examination  29.00
    #> 135     Le Locle      Examination  22.00
    #> 136    Neuchatel      Examination  35.00
    #> 137   Val de Ruz      Examination  15.00
    #> 138 ValdeTravers      Examination  25.00
    #> 139 V. De Geneve      Examination  37.00
    #> 140  Rive Droite      Examination  16.00
    #> 141  Rive Gauche      Examination  22.00
    #> 142   Courtelary        Education  12.00
    #> 143     Delemont        Education   9.00
    #> 144 Franches-Mnt        Education   5.00
    #> 145      Moutier        Education   7.00
    #> 146   Neuveville        Education  15.00
    #> 147   Porrentruy        Education   7.00
    #> 148        Broye        Education   7.00
    #> 149        Glane        Education   8.00
    #> 150      Gruyere        Education   7.00
    #> 151       Sarine        Education  13.00
    #> 152      Veveyse        Education   6.00
    #> 153        Aigle        Education  12.00
    #> 154      Aubonne        Education   7.00
    #> 155     Avenches        Education  12.00
    #> 156     Cossonay        Education   5.00
    #> 157    Echallens        Education   2.00
    #> 158     Grandson        Education   8.00
    #> 159     Lausanne        Education  28.00
    #> 160    La Vallee        Education  20.00
    #> 161       Lavaux        Education   9.00
    #> 162       Morges        Education  10.00
    #> 163       Moudon        Education   3.00
    #> 164        Nyone        Education  12.00
    #> 165         Orbe        Education   6.00
    #> 166         Oron        Education   1.00
    #> 167      Payerne        Education   8.00
    #> 168 Paysd'enhaut        Education   3.00
    #> 169        Rolle        Education  10.00
    #> 170        Vevey        Education  19.00
    #> 171      Yverdon        Education   8.00
    #> 172      Conthey        Education   2.00
    #> 173    Entremont        Education   6.00
    #> 174       Herens        Education   2.00
    #> 175     Martigwy        Education   6.00
    #> 176      Monthey        Education   3.00
    #> 177   St Maurice        Education   9.00
    #> 178       Sierre        Education   3.00
    #> 179         Sion        Education  13.00
    #> 180       Boudry        Education  12.00
    #> 181 La Chauxdfnd        Education  11.00
    #> 182     Le Locle        Education  13.00
    #> 183    Neuchatel        Education  32.00
    #> 184   Val de Ruz        Education   7.00
    #> 185 ValdeTravers        Education   7.00
    #> 186 V. De Geneve        Education  53.00
    #> 187  Rive Droite        Education  29.00
    #> 188  Rive Gauche        Education  29.00
    #> 189   Courtelary         Catholic   9.96
    #> 190     Delemont         Catholic  84.84
    #> 191 Franches-Mnt         Catholic  93.40
    #> 192      Moutier         Catholic  33.77
    #> 193   Neuveville         Catholic   5.16
    #> 194   Porrentruy         Catholic  90.57
    #> 195        Broye         Catholic  92.85
    #> 196        Glane         Catholic  97.16
    #> 197      Gruyere         Catholic  97.67
    #> 198       Sarine         Catholic  91.38
    #> 199      Veveyse         Catholic  98.61
    #> 200        Aigle         Catholic   8.52
    #> 201      Aubonne         Catholic   2.27
    #> 202     Avenches         Catholic   4.43
    #> 203     Cossonay         Catholic   2.82
    #> 204    Echallens         Catholic  24.20
    #> 205     Grandson         Catholic   3.30
    #> 206     Lausanne         Catholic  12.11
    #> 207    La Vallee         Catholic   2.15
    #> 208       Lavaux         Catholic   2.84
    #> 209       Morges         Catholic   5.23
    #> 210       Moudon         Catholic   4.52
    #> 211        Nyone         Catholic  15.14
    #> 212         Orbe         Catholic   4.20
    #> 213         Oron         Catholic   2.40
    #> 214      Payerne         Catholic   5.23
    #> 215 Paysd'enhaut         Catholic   2.56
    #> 216        Rolle         Catholic   7.72
    #> 217        Vevey         Catholic  18.46
    #> 218      Yverdon         Catholic   6.10
    #> 219      Conthey         Catholic  99.71
    #> 220    Entremont         Catholic  99.68
    #> 221       Herens         Catholic 100.00
    #> 222     Martigwy         Catholic  98.96
    #> 223      Monthey         Catholic  98.22
    #> 224   St Maurice         Catholic  99.06
    #> 225       Sierre         Catholic  99.46
    #> 226         Sion         Catholic  96.83
    #> 227       Boudry         Catholic   5.62
    #> 228 La Chauxdfnd         Catholic  13.79
    #> 229     Le Locle         Catholic  11.22
    #> 230    Neuchatel         Catholic  16.92
    #> 231   Val de Ruz         Catholic   4.97
    #> 232 ValdeTravers         Catholic   8.65
    #> 233 V. De Geneve         Catholic  42.34
    #> 234  Rive Droite         Catholic  50.43
    #> 235  Rive Gauche         Catholic  58.33
    #> 236   Courtelary Infant.Mortality  22.20
    #> 237     Delemont Infant.Mortality  22.20
    #> 238 Franches-Mnt Infant.Mortality  20.20
    #> 239      Moutier Infant.Mortality  20.30
    #> 240   Neuveville Infant.Mortality  20.60
    #> 241   Porrentruy Infant.Mortality  26.60
    #> 242        Broye Infant.Mortality  23.60
    #> 243        Glane Infant.Mortality  24.90
    #> 244      Gruyere Infant.Mortality  21.00
    #> 245       Sarine Infant.Mortality  24.40
    #> 246      Veveyse Infant.Mortality  24.50
    #> 247        Aigle Infant.Mortality  16.50
    #> 248      Aubonne Infant.Mortality  19.10
    #> 249     Avenches Infant.Mortality  22.70
    #> 250     Cossonay Infant.Mortality  18.70
    #> 251    Echallens Infant.Mortality  21.20
    #> 252     Grandson Infant.Mortality  20.00
    #> 253     Lausanne Infant.Mortality  20.20
    #> 254    La Vallee Infant.Mortality  10.80
    #> 255       Lavaux Infant.Mortality  20.00
    #> 256       Morges Infant.Mortality  18.00
    #> 257       Moudon Infant.Mortality  22.40
    #> 258        Nyone Infant.Mortality  16.70
    #> 259         Orbe Infant.Mortality  15.30
    #> 260         Oron Infant.Mortality  21.00
    #> 261      Payerne Infant.Mortality  23.80
    #> 262 Paysd'enhaut Infant.Mortality  18.00
    #> 263        Rolle Infant.Mortality  16.30
    #> 264        Vevey Infant.Mortality  20.90
    #> 265      Yverdon Infant.Mortality  22.50
    #> 266      Conthey Infant.Mortality  15.10
    #> 267    Entremont Infant.Mortality  19.80
    #> 268       Herens Infant.Mortality  18.30
    #> 269     Martigwy Infant.Mortality  19.40
    #> 270      Monthey Infant.Mortality  20.20
    #> 271   St Maurice Infant.Mortality  17.80
    #> 272       Sierre Infant.Mortality  16.30
    #> 273         Sion Infant.Mortality  18.10
    #> 274       Boudry Infant.Mortality  20.30
    #> 275 La Chauxdfnd Infant.Mortality  20.50
    #> 276     Le Locle Infant.Mortality  18.90
    #> 277    Neuchatel Infant.Mortality  23.00
    #> 278   Val de Ruz Infant.Mortality  20.00
    #> 279 ValdeTravers Infant.Mortality  19.50
    #> 280 V. De Geneve Infant.Mortality  18.00
    #> 281  Rive Droite Infant.Mortality  18.20
    #> 282  Rive Gauche Infant.Mortality  19.30

Theoph
------

ToothGrowth
-----------

trees
-----

``` r
trees_tidy <- trees %>%
  mutate(tree = seq_len(n())) %>%
  gather(key, value, -tree)
trees_tidy
```

    #>    tree    key value
    #> 1     1  Girth   8.3
    #> 2     2  Girth   8.6
    #> 3     3  Girth   8.8
    #> 4     4  Girth  10.5
    #> 5     5  Girth  10.7
    #> 6     6  Girth  10.8
    #> 7     7  Girth  11.0
    #> 8     8  Girth  11.0
    #> 9     9  Girth  11.1
    #> 10   10  Girth  11.2
    #> 11   11  Girth  11.3
    #> 12   12  Girth  11.4
    #> 13   13  Girth  11.4
    #> 14   14  Girth  11.7
    #> 15   15  Girth  12.0
    #> 16   16  Girth  12.9
    #> 17   17  Girth  12.9
    #> 18   18  Girth  13.3
    #> 19   19  Girth  13.7
    #> 20   20  Girth  13.8
    #> 21   21  Girth  14.0
    #> 22   22  Girth  14.2
    #> 23   23  Girth  14.5
    #> 24   24  Girth  16.0
    #> 25   25  Girth  16.3
    #> 26   26  Girth  17.3
    #> 27   27  Girth  17.5
    #> 28   28  Girth  17.9
    #> 29   29  Girth  18.0
    #> 30   30  Girth  18.0
    #> 31   31  Girth  20.6
    #> 32    1 Height  70.0
    #> 33    2 Height  65.0
    #> 34    3 Height  63.0
    #> 35    4 Height  72.0
    #> 36    5 Height  81.0
    #> 37    6 Height  83.0
    #> 38    7 Height  66.0
    #> 39    8 Height  75.0
    #> 40    9 Height  80.0
    #> 41   10 Height  75.0
    #> 42   11 Height  79.0
    #> 43   12 Height  76.0
    #> 44   13 Height  76.0
    #> 45   14 Height  69.0
    #> 46   15 Height  75.0
    #> 47   16 Height  74.0
    #> 48   17 Height  85.0
    #> 49   18 Height  86.0
    #> 50   19 Height  71.0
    #> 51   20 Height  64.0
    #> 52   21 Height  78.0
    #> 53   22 Height  80.0
    #> 54   23 Height  74.0
    #> 55   24 Height  72.0
    #> 56   25 Height  77.0
    #> 57   26 Height  81.0
    #> 58   27 Height  82.0
    #> 59   28 Height  80.0
    #> 60   29 Height  80.0
    #> 61   30 Height  80.0
    #> 62   31 Height  87.0
    #> 63    1 Volume  10.3
    #> 64    2 Volume  10.3
    #> 65    3 Volume  10.2
    #> 66    4 Volume  16.4
    #> 67    5 Volume  18.8
    #> 68    6 Volume  19.7
    #> 69    7 Volume  15.6
    #> 70    8 Volume  18.2
    #> 71    9 Volume  22.6
    #> 72   10 Volume  19.9
    #> 73   11 Volume  24.2
    #> 74   12 Volume  21.0
    #> 75   13 Volume  21.4
    #> 76   14 Volume  21.3
    #> 77   15 Volume  19.1
    #> 78   16 Volume  22.2
    #> 79   17 Volume  33.8
    #> 80   18 Volume  27.4
    #> 81   19 Volume  25.7
    #> 82   20 Volume  24.9
    #> 83   21 Volume  34.5
    #> 84   22 Volume  31.7
    #> 85   23 Volume  36.3
    #> 86   24 Volume  38.3
    #> 87   25 Volume  42.6
    #> 88   26 Volume  55.4
    #> 89   27 Volume  55.7
    #> 90   28 Volume  58.3
    #> 91   29 Volume  51.5
    #> 92   30 Volume  51.0
    #> 93   31 Volume  77.0

USArrests
---------

``` r
USArrests_tidy <- USArrests %>%
  mutate(state = rownames(.)) %>%
  gather(key, value, -state)
USArrests_tidy
```

    #>              state      key value
    #> 1          Alabama   Murder  13.2
    #> 2           Alaska   Murder  10.0
    #> 3          Arizona   Murder   8.1
    #> 4         Arkansas   Murder   8.8
    #> 5       California   Murder   9.0
    #> 6         Colorado   Murder   7.9
    #> 7      Connecticut   Murder   3.3
    #> 8         Delaware   Murder   5.9
    #> 9          Florida   Murder  15.4
    #> 10         Georgia   Murder  17.4
    #> 11          Hawaii   Murder   5.3
    #> 12           Idaho   Murder   2.6
    #> 13        Illinois   Murder  10.4
    #> 14         Indiana   Murder   7.2
    #> 15            Iowa   Murder   2.2
    #> 16          Kansas   Murder   6.0
    #> 17        Kentucky   Murder   9.7
    #> 18       Louisiana   Murder  15.4
    #> 19           Maine   Murder   2.1
    #> 20        Maryland   Murder  11.3
    #> 21   Massachusetts   Murder   4.4
    #> 22        Michigan   Murder  12.1
    #> 23       Minnesota   Murder   2.7
    #> 24     Mississippi   Murder  16.1
    #> 25        Missouri   Murder   9.0
    #> 26         Montana   Murder   6.0
    #> 27        Nebraska   Murder   4.3
    #> 28          Nevada   Murder  12.2
    #> 29   New Hampshire   Murder   2.1
    #> 30      New Jersey   Murder   7.4
    #> 31      New Mexico   Murder  11.4
    #> 32        New York   Murder  11.1
    #> 33  North Carolina   Murder  13.0
    #> 34    North Dakota   Murder   0.8
    #> 35            Ohio   Murder   7.3
    #> 36        Oklahoma   Murder   6.6
    #> 37          Oregon   Murder   4.9
    #> 38    Pennsylvania   Murder   6.3
    #> 39    Rhode Island   Murder   3.4
    #> 40  South Carolina   Murder  14.4
    #> 41    South Dakota   Murder   3.8
    #> 42       Tennessee   Murder  13.2
    #> 43           Texas   Murder  12.7
    #> 44            Utah   Murder   3.2
    #> 45         Vermont   Murder   2.2
    #> 46        Virginia   Murder   8.5
    #> 47      Washington   Murder   4.0
    #> 48   West Virginia   Murder   5.7
    #> 49       Wisconsin   Murder   2.6
    #> 50         Wyoming   Murder   6.8
    #> 51         Alabama  Assault 236.0
    #> 52          Alaska  Assault 263.0
    #> 53         Arizona  Assault 294.0
    #> 54        Arkansas  Assault 190.0
    #> 55      California  Assault 276.0
    #> 56        Colorado  Assault 204.0
    #> 57     Connecticut  Assault 110.0
    #> 58        Delaware  Assault 238.0
    #> 59         Florida  Assault 335.0
    #> 60         Georgia  Assault 211.0
    #> 61          Hawaii  Assault  46.0
    #> 62           Idaho  Assault 120.0
    #> 63        Illinois  Assault 249.0
    #> 64         Indiana  Assault 113.0
    #> 65            Iowa  Assault  56.0
    #> 66          Kansas  Assault 115.0
    #> 67        Kentucky  Assault 109.0
    #> 68       Louisiana  Assault 249.0
    #> 69           Maine  Assault  83.0
    #> 70        Maryland  Assault 300.0
    #> 71   Massachusetts  Assault 149.0
    #> 72        Michigan  Assault 255.0
    #> 73       Minnesota  Assault  72.0
    #> 74     Mississippi  Assault 259.0
    #> 75        Missouri  Assault 178.0
    #> 76         Montana  Assault 109.0
    #> 77        Nebraska  Assault 102.0
    #> 78          Nevada  Assault 252.0
    #> 79   New Hampshire  Assault  57.0
    #> 80      New Jersey  Assault 159.0
    #> 81      New Mexico  Assault 285.0
    #> 82        New York  Assault 254.0
    #> 83  North Carolina  Assault 337.0
    #> 84    North Dakota  Assault  45.0
    #> 85            Ohio  Assault 120.0
    #> 86        Oklahoma  Assault 151.0
    #> 87          Oregon  Assault 159.0
    #> 88    Pennsylvania  Assault 106.0
    #> 89    Rhode Island  Assault 174.0
    #> 90  South Carolina  Assault 279.0
    #> 91    South Dakota  Assault  86.0
    #> 92       Tennessee  Assault 188.0
    #> 93           Texas  Assault 201.0
    #> 94            Utah  Assault 120.0
    #> 95         Vermont  Assault  48.0
    #> 96        Virginia  Assault 156.0
    #> 97      Washington  Assault 145.0
    #> 98   West Virginia  Assault  81.0
    #> 99       Wisconsin  Assault  53.0
    #> 100        Wyoming  Assault 161.0
    #> 101        Alabama UrbanPop  58.0
    #> 102         Alaska UrbanPop  48.0
    #> 103        Arizona UrbanPop  80.0
    #> 104       Arkansas UrbanPop  50.0
    #> 105     California UrbanPop  91.0
    #> 106       Colorado UrbanPop  78.0
    #> 107    Connecticut UrbanPop  77.0
    #> 108       Delaware UrbanPop  72.0
    #> 109        Florida UrbanPop  80.0
    #> 110        Georgia UrbanPop  60.0
    #> 111         Hawaii UrbanPop  83.0
    #> 112          Idaho UrbanPop  54.0
    #> 113       Illinois UrbanPop  83.0
    #> 114        Indiana UrbanPop  65.0
    #> 115           Iowa UrbanPop  57.0
    #> 116         Kansas UrbanPop  66.0
    #> 117       Kentucky UrbanPop  52.0
    #> 118      Louisiana UrbanPop  66.0
    #> 119          Maine UrbanPop  51.0
    #> 120       Maryland UrbanPop  67.0
    #> 121  Massachusetts UrbanPop  85.0
    #> 122       Michigan UrbanPop  74.0
    #> 123      Minnesota UrbanPop  66.0
    #> 124    Mississippi UrbanPop  44.0
    #> 125       Missouri UrbanPop  70.0
    #> 126        Montana UrbanPop  53.0
    #> 127       Nebraska UrbanPop  62.0
    #> 128         Nevada UrbanPop  81.0
    #> 129  New Hampshire UrbanPop  56.0
    #> 130     New Jersey UrbanPop  89.0
    #> 131     New Mexico UrbanPop  70.0
    #> 132       New York UrbanPop  86.0
    #> 133 North Carolina UrbanPop  45.0
    #> 134   North Dakota UrbanPop  44.0
    #> 135           Ohio UrbanPop  75.0
    #> 136       Oklahoma UrbanPop  68.0
    #> 137         Oregon UrbanPop  67.0
    #> 138   Pennsylvania UrbanPop  72.0
    #> 139   Rhode Island UrbanPop  87.0
    #> 140 South Carolina UrbanPop  48.0
    #> 141   South Dakota UrbanPop  45.0
    #> 142      Tennessee UrbanPop  59.0
    #> 143          Texas UrbanPop  80.0
    #> 144           Utah UrbanPop  80.0
    #> 145        Vermont UrbanPop  32.0
    #> 146       Virginia UrbanPop  63.0
    #> 147     Washington UrbanPop  73.0
    #> 148  West Virginia UrbanPop  39.0
    #> 149      Wisconsin UrbanPop  66.0
    #> 150        Wyoming UrbanPop  60.0
    #> 151        Alabama     Rape  21.2
    #> 152         Alaska     Rape  44.5
    #> 153        Arizona     Rape  31.0
    #> 154       Arkansas     Rape  19.5
    #> 155     California     Rape  40.6
    #> 156       Colorado     Rape  38.7
    #> 157    Connecticut     Rape  11.1
    #> 158       Delaware     Rape  15.8
    #> 159        Florida     Rape  31.9
    #> 160        Georgia     Rape  25.8
    #> 161         Hawaii     Rape  20.2
    #> 162          Idaho     Rape  14.2
    #> 163       Illinois     Rape  24.0
    #> 164        Indiana     Rape  21.0
    #> 165           Iowa     Rape  11.3
    #> 166         Kansas     Rape  18.0
    #> 167       Kentucky     Rape  16.3
    #> 168      Louisiana     Rape  22.2
    #> 169          Maine     Rape   7.8
    #> 170       Maryland     Rape  27.8
    #> 171  Massachusetts     Rape  16.3
    #> 172       Michigan     Rape  35.1
    #> 173      Minnesota     Rape  14.9
    #> 174    Mississippi     Rape  17.1
    #> 175       Missouri     Rape  28.2
    #> 176        Montana     Rape  16.4
    #> 177       Nebraska     Rape  16.5
    #> 178         Nevada     Rape  46.0
    #> 179  New Hampshire     Rape   9.5
    #> 180     New Jersey     Rape  18.8
    #> 181     New Mexico     Rape  32.1
    #> 182       New York     Rape  26.1
    #> 183 North Carolina     Rape  16.1
    #> 184   North Dakota     Rape   7.3
    #> 185           Ohio     Rape  21.4
    #> 186       Oklahoma     Rape  20.0
    #> 187         Oregon     Rape  29.3
    #> 188   Pennsylvania     Rape  14.9
    #> 189   Rhode Island     Rape   8.3
    #> 190 South Carolina     Rape  22.5
    #> 191   South Dakota     Rape  12.8
    #> 192      Tennessee     Rape  26.9
    #> 193          Texas     Rape  25.5
    #> 194           Utah     Rape  22.9
    #> 195        Vermont     Rape  11.2
    #> 196       Virginia     Rape  20.7
    #> 197     Washington     Rape  26.2
    #> 198  West Virginia     Rape   9.3
    #> 199      Wisconsin     Rape  10.8
    #> 200        Wyoming     Rape  15.6

USJudgeRatings
--------------

``` r
USJudgeRatings_tidy <- USJudgeRatings %>%
  mutate(judge= rownames(.)) %>%
  gather(key, value, -judge)
USJudgeRatings_tidy
```

    #>               judge  key value
    #> 1     AARONSON,L.H. CONT   5.7
    #> 2    ALEXANDER,J.M. CONT   6.8
    #> 3    ARMENTANO,A.J. CONT   7.2
    #> 4       BERDON,R.I. CONT   6.8
    #> 5      BRACKEN,J.J. CONT   7.3
    #> 6        BURNS,E.B. CONT   6.2
    #> 7     CALLAHAN,R.J. CONT  10.6
    #> 8        COHEN,S.S. CONT   7.0
    #> 9         DALY,J.J. CONT   7.3
    #> 10     DANNEHY,J.F. CONT   8.2
    #> 11        DEAN,H.H. CONT   7.0
    #> 12      DEVITA,H.J. CONT   6.5
    #> 13    DRISCOLL,P.J. CONT   6.7
    #> 14      GRILLO,A.E. CONT   7.0
    #> 15   HADDEN,W.L.JR. CONT   6.5
    #> 16      HAMILL,E.C. CONT   7.3
    #> 17      HEALEY.A.H. CONT   8.0
    #> 18        HULL,T.C. CONT   7.7
    #> 19        LEVINE,I. CONT   8.3
    #> 20    LEVISTER,R.L. CONT   9.6
    #> 21      MARTIN,L.F. CONT   7.1
    #> 22     MCGRATH,J.F. CONT   7.6
    #> 23     MIGNONE,A.F. CONT   6.6
    #> 24      MISSAL,H.M. CONT   6.2
    #> 25      MULVEY,H.M. CONT   7.5
    #> 26       NARUK,H.J. CONT   7.8
    #> 27     O'BRIEN,F.J. CONT   7.1
    #> 28  O'SULLIVAN,T.J. CONT   7.5
    #> 29        PASKEY,L. CONT   7.5
    #> 30     RUBINOW,J.E. CONT   7.1
    #> 31       SADEN.G.A. CONT   6.6
    #> 32  SATANIELLO,A.G. CONT   8.4
    #> 33        SHEA,D.M. CONT   6.9
    #> 34     SHEA,J.F.JR. CONT   7.3
    #> 35       SIDOR,W.J. CONT   7.7
    #> 36    SPEZIALE,J.A. CONT   8.5
    #> 37      SPONZO,M.J. CONT   6.9
    #> 38   STAPLETON,J.F. CONT   6.5
    #> 39       TESTO,R.J. CONT   8.3
    #> 40  TIERNEY,W.L.JR. CONT   8.3
    #> 41        WALL,R.A. CONT   9.0
    #> 42      WRIGHT,D.B. CONT   7.1
    #> 43    ZARRILLI,K.J. CONT   8.6
    #> 44    AARONSON,L.H. INTG   7.9
    #> 45   ALEXANDER,J.M. INTG   8.9
    #> 46   ARMENTANO,A.J. INTG   8.1
    #> 47      BERDON,R.I. INTG   8.8
    #> 48     BRACKEN,J.J. INTG   6.4
    #> 49       BURNS,E.B. INTG   8.8
    #> 50    CALLAHAN,R.J. INTG   9.0
    #> 51       COHEN,S.S. INTG   5.9
    #> 52        DALY,J.J. INTG   8.9
    #> 53     DANNEHY,J.F. INTG   7.9
    #> 54        DEAN,H.H. INTG   8.0
    #> 55      DEVITA,H.J. INTG   8.0
    #> 56    DRISCOLL,P.J. INTG   8.6
    #> 57      GRILLO,A.E. INTG   7.5
    #> 58   HADDEN,W.L.JR. INTG   8.1
    #> 59      HAMILL,E.C. INTG   8.0
    #> 60      HEALEY.A.H. INTG   7.6
    #> 61        HULL,T.C. INTG   7.7
    #> 62        LEVINE,I. INTG   8.2
    #> 63    LEVISTER,R.L. INTG   6.9
    #> 64      MARTIN,L.F. INTG   8.2
    #> 65     MCGRATH,J.F. INTG   7.3
    #> 66     MIGNONE,A.F. INTG   7.4
    #> 67      MISSAL,H.M. INTG   8.3
    #> 68      MULVEY,H.M. INTG   8.7
    #> 69       NARUK,H.J. INTG   8.9
    #> 70     O'BRIEN,F.J. INTG   8.5
    #> 71  O'SULLIVAN,T.J. INTG   9.0
    #> 72        PASKEY,L. INTG   8.1
    #> 73     RUBINOW,J.E. INTG   9.2
    #> 74       SADEN.G.A. INTG   7.4
    #> 75  SATANIELLO,A.G. INTG   8.0
    #> 76        SHEA,D.M. INTG   8.5
    #> 77     SHEA,J.F.JR. INTG   8.9
    #> 78       SIDOR,W.J. INTG   6.2
    #> 79    SPEZIALE,J.A. INTG   8.3
    #> 80      SPONZO,M.J. INTG   8.3
    #> 81   STAPLETON,J.F. INTG   8.2
    #> 82       TESTO,R.J. INTG   7.3
    #> 83  TIERNEY,W.L.JR. INTG   8.2
    #> 84        WALL,R.A. INTG   7.0
    #> 85      WRIGHT,D.B. INTG   8.4
    #> 86    ZARRILLI,K.J. INTG   7.4
    #> 87    AARONSON,L.H. DMNR   7.7
    #> 88   ALEXANDER,J.M. DMNR   8.8
    #> 89   ARMENTANO,A.J. DMNR   7.8
    #> 90      BERDON,R.I. DMNR   8.5
    #> 91     BRACKEN,J.J. DMNR   4.3
    #> 92       BURNS,E.B. DMNR   8.7
    #> 93    CALLAHAN,R.J. DMNR   8.9
    #> 94       COHEN,S.S. DMNR   4.9
    #> 95        DALY,J.J. DMNR   8.9
    #> 96     DANNEHY,J.F. DMNR   6.7
    #> 97        DEAN,H.H. DMNR   7.6
    #> 98      DEVITA,H.J. DMNR   7.6
    #> 99    DRISCOLL,P.J. DMNR   8.2
    #> 100     GRILLO,A.E. DMNR   6.4
    #> 101  HADDEN,W.L.JR. DMNR   8.0
    #> 102     HAMILL,E.C. DMNR   7.4
    #> 103     HEALEY.A.H. DMNR   6.6
    #> 104       HULL,T.C. DMNR   6.7
    #> 105       LEVINE,I. DMNR   7.4
    #> 106   LEVISTER,R.L. DMNR   5.7
    #> 107     MARTIN,L.F. DMNR   7.7
    #> 108    MCGRATH,J.F. DMNR   6.9
    #> 109    MIGNONE,A.F. DMNR   6.2
    #> 110     MISSAL,H.M. DMNR   8.1
    #> 111     MULVEY,H.M. DMNR   8.5
    #> 112      NARUK,H.J. DMNR   8.7
    #> 113    O'BRIEN,F.J. DMNR   8.3
    #> 114 O'SULLIVAN,T.J. DMNR   8.9
    #> 115       PASKEY,L. DMNR   7.7
    #> 116    RUBINOW,J.E. DMNR   9.0
    #> 117      SADEN.G.A. DMNR   6.9
    #> 118 SATANIELLO,A.G. DMNR   7.9
    #> 119       SHEA,D.M. DMNR   7.8
    #> 120    SHEA,J.F.JR. DMNR   8.8
    #> 121      SIDOR,W.J. DMNR   5.1
    #> 122   SPEZIALE,J.A. DMNR   8.1
    #> 123     SPONZO,M.J. DMNR   8.0
    #> 124  STAPLETON,J.F. DMNR   7.7
    #> 125      TESTO,R.J. DMNR   7.0
    #> 126 TIERNEY,W.L.JR. DMNR   7.8
    #> 127       WALL,R.A. DMNR   5.9
    #> 128     WRIGHT,D.B. DMNR   8.4
    #> 129   ZARRILLI,K.J. DMNR   7.0
    #> 130   AARONSON,L.H. DILG   7.3
    #> 131  ALEXANDER,J.M. DILG   8.5
    #> 132  ARMENTANO,A.J. DILG   7.8
    #> 133     BERDON,R.I. DILG   8.8
    #> 134    BRACKEN,J.J. DILG   6.5
    #> 135      BURNS,E.B. DILG   8.5
    #> 136   CALLAHAN,R.J. DILG   8.7
    #> 137      COHEN,S.S. DILG   5.1
    #> 138       DALY,J.J. DILG   8.7
    #> 139    DANNEHY,J.F. DILG   8.1
    #> 140       DEAN,H.H. DILG   7.4
    #> 141     DEVITA,H.J. DILG   7.2
    #> 142   DRISCOLL,P.J. DILG   6.8
    #> 143     GRILLO,A.E. DILG   6.8
    #> 144  HADDEN,W.L.JR. DILG   8.0
    #> 145     HAMILL,E.C. DILG   7.7
    #> 146     HEALEY.A.H. DILG   7.2
    #> 147       HULL,T.C. DILG   7.5
    #> 148       LEVINE,I. DILG   7.8
    #> 149   LEVISTER,R.L. DILG   6.6
    #> 150     MARTIN,L.F. DILG   7.1
    #> 151    MCGRATH,J.F. DILG   6.8
    #> 152    MIGNONE,A.F. DILG   6.2
    #> 153     MISSAL,H.M. DILG   7.7
    #> 154     MULVEY,H.M. DILG   8.6
    #> 155      NARUK,H.J. DILG   8.9
    #> 156    O'BRIEN,F.J. DILG   8.0
    #> 157 O'SULLIVAN,T.J. DILG   8.7
    #> 158       PASKEY,L. DILG   8.2
    #> 159    RUBINOW,J.E. DILG   9.0
    #> 160      SADEN.G.A. DILG   8.4
    #> 161 SATANIELLO,A.G. DILG   7.9
    #> 162       SHEA,D.M. DILG   8.5
    #> 163    SHEA,J.F.JR. DILG   8.7
    #> 164      SIDOR,W.J. DILG   5.6
    #> 165   SPEZIALE,J.A. DILG   8.3
    #> 166     SPONZO,M.J. DILG   8.1
    #> 167  STAPLETON,J.F. DILG   7.8
    #> 168      TESTO,R.J. DILG   6.8
    #> 169 TIERNEY,W.L.JR. DILG   8.3
    #> 170       WALL,R.A. DILG   7.0
    #> 171     WRIGHT,D.B. DILG   7.7
    #> 172   ZARRILLI,K.J. DILG   7.5
    #> 173   AARONSON,L.H. CFMG   7.1
    #> 174  ALEXANDER,J.M. CFMG   7.8
    #> 175  ARMENTANO,A.J. CFMG   7.5
    #> 176     BERDON,R.I. CFMG   8.3
    #> 177    BRACKEN,J.J. CFMG   6.0
    #> 178      BURNS,E.B. CFMG   7.9
    #> 179   CALLAHAN,R.J. CFMG   8.5
    #> 180      COHEN,S.S. CFMG   5.4
    #> 181       DALY,J.J. CFMG   8.6
    #> 182    DANNEHY,J.F. CFMG   7.9
    #> 183       DEAN,H.H. CFMG   7.3
    #> 184     DEVITA,H.J. CFMG   7.0
    #> 185   DRISCOLL,P.J. CFMG   6.9
    #> 186     GRILLO,A.E. CFMG   6.5
    #> 187  HADDEN,W.L.JR. CFMG   7.9
    #> 188     HAMILL,E.C. CFMG   7.3
    #> 189     HEALEY.A.H. CFMG   6.5
    #> 190       HULL,T.C. CFMG   7.4
    #> 191       LEVINE,I. CFMG   7.7
    #> 192   LEVISTER,R.L. CFMG   6.9
    #> 193     MARTIN,L.F. CFMG   6.6
    #> 194    MCGRATH,J.F. CFMG   6.7
    #> 195    MIGNONE,A.F. CFMG   5.4
    #> 196     MISSAL,H.M. CFMG   7.4
    #> 197     MULVEY,H.M. CFMG   8.5
    #> 198      NARUK,H.J. CFMG   8.7
    #> 199    O'BRIEN,F.J. CFMG   7.9
    #> 200 O'SULLIVAN,T.J. CFMG   8.4
    #> 201       PASKEY,L. CFMG   8.0
    #> 202    RUBINOW,J.E. CFMG   8.4
    #> 203      SADEN.G.A. CFMG   8.0
    #> 204 SATANIELLO,A.G. CFMG   7.8
    #> 205       SHEA,D.M. CFMG   8.1
    #> 206    SHEA,J.F.JR. CFMG   8.4
    #> 207      SIDOR,W.J. CFMG   5.6
    #> 208   SPEZIALE,J.A. CFMG   8.4
    #> 209     SPONZO,M.J. CFMG   7.9
    #> 210  STAPLETON,J.F. CFMG   7.6
    #> 211      TESTO,R.J. CFMG   7.0
    #> 212 TIERNEY,W.L.JR. CFMG   8.4
    #> 213       WALL,R.A. CFMG   7.0
    #> 214     WRIGHT,D.B. CFMG   7.5
    #> 215   ZARRILLI,K.J. CFMG   7.5
    #> 216   AARONSON,L.H. DECI   7.4
    #> 217  ALEXANDER,J.M. DECI   8.1
    #> 218  ARMENTANO,A.J. DECI   7.6
    #> 219     BERDON,R.I. DECI   8.5
    #> 220    BRACKEN,J.J. DECI   6.2
    #> 221      BURNS,E.B. DECI   8.0
    #> 222   CALLAHAN,R.J. DECI   8.5
    #> 223      COHEN,S.S. DECI   5.9
    #> 224       DALY,J.J. DECI   8.5
    #> 225    DANNEHY,J.F. DECI   8.0
    #> 226       DEAN,H.H. DECI   7.5
    #> 227     DEVITA,H.J. DECI   7.1
    #> 228   DRISCOLL,P.J. DECI   6.6
    #> 229     GRILLO,A.E. DECI   7.0
    #> 230  HADDEN,W.L.JR. DECI   8.0
    #> 231     HAMILL,E.C. DECI   7.3
    #> 232     HEALEY.A.H. DECI   6.5
    #> 233       HULL,T.C. DECI   7.5
    #> 234       LEVINE,I. DECI   7.7
    #> 235   LEVISTER,R.L. DECI   6.6
    #> 236     MARTIN,L.F. DECI   6.6
    #> 237    MCGRATH,J.F. DECI   6.8
    #> 238    MIGNONE,A.F. DECI   5.7
    #> 239     MISSAL,H.M. DECI   7.3
    #> 240     MULVEY,H.M. DECI   8.4
    #> 241      NARUK,H.J. DECI   8.8
    #> 242    O'BRIEN,F.J. DECI   7.9
    #> 243 O'SULLIVAN,T.J. DECI   8.5
    #> 244       PASKEY,L. DECI   8.1
    #> 245    RUBINOW,J.E. DECI   8.6
    #> 246      SADEN.G.A. DECI   7.9
    #> 247 SATANIELLO,A.G. DECI   7.8
    #> 248       SHEA,D.M. DECI   8.2
    #> 249    SHEA,J.F.JR. DECI   8.5
    #> 250      SIDOR,W.J. DECI   5.9
    #> 251   SPEZIALE,J.A. DECI   8.2
    #> 252     SPONZO,M.J. DECI   7.9
    #> 253  STAPLETON,J.F. DECI   7.7
    #> 254      TESTO,R.J. DECI   7.1
    #> 255 TIERNEY,W.L.JR. DECI   8.3
    #> 256       WALL,R.A. DECI   7.2
    #> 257     WRIGHT,D.B. DECI   7.7
    #> 258   ZARRILLI,K.J. DECI   7.7
    #> 259   AARONSON,L.H. PREP   7.1
    #> 260  ALEXANDER,J.M. PREP   8.0
    #> 261  ARMENTANO,A.J. PREP   7.5
    #> 262     BERDON,R.I. PREP   8.7
    #> 263    BRACKEN,J.J. PREP   5.7
    #> 264      BURNS,E.B. PREP   8.1
    #> 265   CALLAHAN,R.J. PREP   8.5
    #> 266      COHEN,S.S. PREP   4.8
    #> 267       DALY,J.J. PREP   8.4
    #> 268    DANNEHY,J.F. PREP   7.9
    #> 269       DEAN,H.H. PREP   7.1
    #> 270     DEVITA,H.J. PREP   6.9
    #> 271   DRISCOLL,P.J. PREP   7.1
    #> 272     GRILLO,A.E. PREP   6.6
    #> 273  HADDEN,W.L.JR. PREP   7.9
    #> 274     HAMILL,E.C. PREP   7.3
    #> 275     HEALEY.A.H. PREP   6.8
    #> 276       HULL,T.C. PREP   7.1
    #> 277       LEVINE,I. PREP   7.7
    #> 278   LEVISTER,R.L. PREP   6.2
    #> 279     MARTIN,L.F. PREP   6.7
    #> 280    MCGRATH,J.F. PREP   6.4
    #> 281    MIGNONE,A.F. PREP   5.8
    #> 282     MISSAL,H.M. PREP   7.3
    #> 283     MULVEY,H.M. PREP   8.5
    #> 284      NARUK,H.J. PREP   8.9
    #> 285    O'BRIEN,F.J. PREP   7.8
    #> 286 O'SULLIVAN,T.J. PREP   8.4
    #> 287       PASKEY,L. PREP   8.2
    #> 288    RUBINOW,J.E. PREP   9.1
    #> 289      SADEN.G.A. PREP   8.2
    #> 290 SATANIELLO,A.G. PREP   7.6
    #> 291       SHEA,D.M. PREP   8.4
    #> 292    SHEA,J.F.JR. PREP   8.5
    #> 293      SIDOR,W.J. PREP   5.6
    #> 294   SPEZIALE,J.A. PREP   8.2
    #> 295     SPONZO,M.J. PREP   7.9
    #> 296  STAPLETON,J.F. PREP   7.7
    #> 297      TESTO,R.J. PREP   6.7
    #> 298 TIERNEY,W.L.JR. PREP   7.7
    #> 299       WALL,R.A. PREP   6.9
    #> 300     WRIGHT,D.B. PREP   7.8
    #> 301   ZARRILLI,K.J. PREP   7.4
    #> 302   AARONSON,L.H. FAMI   7.1
    #> 303  ALEXANDER,J.M. FAMI   8.0
    #> 304  ARMENTANO,A.J. FAMI   7.5
    #> 305     BERDON,R.I. FAMI   8.7
    #> 306    BRACKEN,J.J. FAMI   5.7
    #> 307      BURNS,E.B. FAMI   8.0
    #> 308   CALLAHAN,R.J. FAMI   8.5
    #> 309      COHEN,S.S. FAMI   5.1
    #> 310       DALY,J.J. FAMI   8.4
    #> 311    DANNEHY,J.F. FAMI   8.1
    #> 312       DEAN,H.H. FAMI   7.2
    #> 313     DEVITA,H.J. FAMI   7.0
    #> 314   DRISCOLL,P.J. FAMI   7.3
    #> 315     GRILLO,A.E. FAMI   6.8
    #> 316  HADDEN,W.L.JR. FAMI   7.8
    #> 317     HAMILL,E.C. FAMI   7.2
    #> 318     HEALEY.A.H. FAMI   6.7
    #> 319       HULL,T.C. FAMI   7.3
    #> 320       LEVINE,I. FAMI   7.8
    #> 321   LEVISTER,R.L. FAMI   6.0
    #> 322     MARTIN,L.F. FAMI   6.7
    #> 323    MCGRATH,J.F. FAMI   6.3
    #> 324    MIGNONE,A.F. FAMI   5.9
    #> 325     MISSAL,H.M. FAMI   7.3
    #> 326     MULVEY,H.M. FAMI   8.5
    #> 327      NARUK,H.J. FAMI   9.0
    #> 328    O'BRIEN,F.J. FAMI   7.8
    #> 329 O'SULLIVAN,T.J. FAMI   8.3
    #> 330       PASKEY,L. FAMI   8.4
    #> 331    RUBINOW,J.E. FAMI   9.1
    #> 332      SADEN.G.A. FAMI   8.4
    #> 333 SATANIELLO,A.G. FAMI   7.4
    #> 334       SHEA,D.M. FAMI   8.5
    #> 335    SHEA,J.F.JR. FAMI   8.5
    #> 336      SIDOR,W.J. FAMI   5.6
    #> 337   SPEZIALE,J.A. FAMI   8.1
    #> 338     SPONZO,M.J. FAMI   7.7
    #> 339  STAPLETON,J.F. FAMI   7.7
    #> 340      TESTO,R.J. FAMI   6.7
    #> 341 TIERNEY,W.L.JR. FAMI   7.6
    #> 342       WALL,R.A. FAMI   6.9
    #> 343     WRIGHT,D.B. FAMI   8.2
    #> 344   ZARRILLI,K.J. FAMI   7.2
    #> 345   AARONSON,L.H. ORAL   7.1
    #> 346  ALEXANDER,J.M. ORAL   7.8
    #> 347  ARMENTANO,A.J. ORAL   7.3
    #> 348     BERDON,R.I. ORAL   8.4
    #> 349    BRACKEN,J.J. ORAL   5.1
    #> 350      BURNS,E.B. ORAL   8.0
    #> 351   CALLAHAN,R.J. ORAL   8.6
    #> 352      COHEN,S.S. ORAL   4.7
    #> 353       DALY,J.J. ORAL   8.4
    #> 354    DANNEHY,J.F. ORAL   7.7
    #> 355       DEAN,H.H. ORAL   7.1
    #> 356     DEVITA,H.J. ORAL   7.0
    #> 357   DRISCOLL,P.J. ORAL   7.2
    #> 358     GRILLO,A.E. ORAL   6.3
    #> 359  HADDEN,W.L.JR. ORAL   7.8
    #> 360     HAMILL,E.C. ORAL   7.1
    #> 361     HEALEY.A.H. ORAL   6.4
    #> 362       HULL,T.C. ORAL   7.1
    #> 363       LEVINE,I. ORAL   7.5
    #> 364   LEVISTER,R.L. ORAL   5.8
    #> 365     MARTIN,L.F. ORAL   6.8
    #> 366    MCGRATH,J.F. ORAL   6.3
    #> 367    MIGNONE,A.F. ORAL   5.2
    #> 368     MISSAL,H.M. ORAL   7.2
    #> 369     MULVEY,H.M. ORAL   8.4
    #> 370      NARUK,H.J. ORAL   8.8
    #> 371    O'BRIEN,F.J. ORAL   7.8
    #> 372 O'SULLIVAN,T.J. ORAL   8.3
    #> 373       PASKEY,L. ORAL   8.0
    #> 374    RUBINOW,J.E. ORAL   8.9
    #> 375      SADEN.G.A. ORAL   7.7
    #> 376 SATANIELLO,A.G. ORAL   7.4
    #> 377       SHEA,D.M. ORAL   8.1
    #> 378    SHEA,J.F.JR. ORAL   8.4
    #> 379      SIDOR,W.J. ORAL   5.3
    #> 380   SPEZIALE,J.A. ORAL   7.9
    #> 381     SPONZO,M.J. ORAL   7.6
    #> 382  STAPLETON,J.F. ORAL   7.5
    #> 383      TESTO,R.J. ORAL   6.7
    #> 384 TIERNEY,W.L.JR. ORAL   7.5
    #> 385       WALL,R.A. ORAL   6.5
    #> 386     WRIGHT,D.B. ORAL   8.0
    #> 387   ZARRILLI,K.J. ORAL   6.9
    #> 388   AARONSON,L.H. WRIT   7.0
    #> 389  ALEXANDER,J.M. WRIT   7.9
    #> 390  ARMENTANO,A.J. WRIT   7.4
    #> 391     BERDON,R.I. WRIT   8.5
    #> 392    BRACKEN,J.J. WRIT   5.3
    #> 393      BURNS,E.B. WRIT   8.0
    #> 394   CALLAHAN,R.J. WRIT   8.4
    #> 395      COHEN,S.S. WRIT   4.9
    #> 396       DALY,J.J. WRIT   8.5
    #> 397    DANNEHY,J.F. WRIT   7.8
    #> 398       DEAN,H.H. WRIT   7.2
    #> 399     DEVITA,H.J. WRIT   7.1
    #> 400   DRISCOLL,P.J. WRIT   7.2
    #> 401     GRILLO,A.E. WRIT   6.6
    #> 402  HADDEN,W.L.JR. WRIT   7.8
    #> 403     HAMILL,E.C. WRIT   7.2
    #> 404     HEALEY.A.H. WRIT   6.5
    #> 405       HULL,T.C. WRIT   7.3
    #> 406       LEVINE,I. WRIT   7.6
    #> 407   LEVISTER,R.L. WRIT   5.8
    #> 408     MARTIN,L.F. WRIT   6.8
    #> 409    MCGRATH,J.F. WRIT   6.3
    #> 410    MIGNONE,A.F. WRIT   5.8
    #> 411     MISSAL,H.M. WRIT   7.3
    #> 412     MULVEY,H.M. WRIT   8.4
    #> 413      NARUK,H.J. WRIT   8.9
    #> 414    O'BRIEN,F.J. WRIT   7.7
    #> 415 O'SULLIVAN,T.J. WRIT   8.3
    #> 416       PASKEY,L. WRIT   8.1
    #> 417    RUBINOW,J.E. WRIT   9.0
    #> 418      SADEN.G.A. WRIT   7.9
    #> 419 SATANIELLO,A.G. WRIT   7.4
    #> 420       SHEA,D.M. WRIT   8.3
    #> 421    SHEA,J.F.JR. WRIT   8.4
    #> 422      SIDOR,W.J. WRIT   5.5
    #> 423   SPEZIALE,J.A. WRIT   8.0
    #> 424     SPONZO,M.J. WRIT   7.7
    #> 425  STAPLETON,J.F. WRIT   7.6
    #> 426      TESTO,R.J. WRIT   6.7
    #> 427 TIERNEY,W.L.JR. WRIT   7.7
    #> 428       WALL,R.A. WRIT   6.6
    #> 429     WRIGHT,D.B. WRIT   8.1
    #> 430   ZARRILLI,K.J. WRIT   7.0
    #> 431   AARONSON,L.H. PHYS   8.3
    #> 432  ALEXANDER,J.M. PHYS   8.5
    #> 433  ARMENTANO,A.J. PHYS   7.9
    #> 434     BERDON,R.I. PHYS   8.8
    #> 435    BRACKEN,J.J. PHYS   5.5
    #> 436      BURNS,E.B. PHYS   8.6
    #> 437   CALLAHAN,R.J. PHYS   9.1
    #> 438      COHEN,S.S. PHYS   6.8
    #> 439       DALY,J.J. PHYS   8.8
    #> 440    DANNEHY,J.F. PHYS   8.5
    #> 441       DEAN,H.H. PHYS   8.4
    #> 442     DEVITA,H.J. PHYS   6.9
    #> 443   DRISCOLL,P.J. PHYS   8.1
    #> 444     GRILLO,A.E. PHYS   6.2
    #> 445  HADDEN,W.L.JR. PHYS   8.4
    #> 446     HAMILL,E.C. PHYS   8.0
    #> 447     HEALEY.A.H. PHYS   6.9
    #> 448       HULL,T.C. PHYS   8.1
    #> 449       LEVINE,I. PHYS   8.0
    #> 450   LEVISTER,R.L. PHYS   7.2
    #> 451     MARTIN,L.F. PHYS   7.5
    #> 452    MCGRATH,J.F. PHYS   7.4
    #> 453    MIGNONE,A.F. PHYS   4.7
    #> 454     MISSAL,H.M. PHYS   7.8
    #> 455     MULVEY,H.M. PHYS   8.7
    #> 456      NARUK,H.J. PHYS   9.0
    #> 457    O'BRIEN,F.J. PHYS   8.3
    #> 458 O'SULLIVAN,T.J. PHYS   8.8
    #> 459       PASKEY,L. PHYS   8.4
    #> 460    RUBINOW,J.E. PHYS   8.9
    #> 461      SADEN.G.A. PHYS   8.4
    #> 462 SATANIELLO,A.G. PHYS   8.1
    #> 463       SHEA,D.M. PHYS   8.7
    #> 464    SHEA,J.F.JR. PHYS   8.8
    #> 465      SIDOR,W.J. PHYS   6.3
    #> 466   SPEZIALE,J.A. PHYS   8.0
    #> 467     SPONZO,M.J. PHYS   8.1
    #> 468  STAPLETON,J.F. PHYS   8.5
    #> 469      TESTO,R.J. PHYS   8.0
    #> 470 TIERNEY,W.L.JR. PHYS   8.1
    #> 471       WALL,R.A. PHYS   7.6
    #> 472     WRIGHT,D.B. PHYS   8.3
    #> 473   ZARRILLI,K.J. PHYS   7.8
    #> 474   AARONSON,L.H. RTEN   7.8
    #> 475  ALEXANDER,J.M. RTEN   8.7
    #> 476  ARMENTANO,A.J. RTEN   7.8
    #> 477     BERDON,R.I. RTEN   8.7
    #> 478    BRACKEN,J.J. RTEN   4.8
    #> 479      BURNS,E.B. RTEN   8.6
    #> 480   CALLAHAN,R.J. RTEN   9.0
    #> 481      COHEN,S.S. RTEN   5.0
    #> 482       DALY,J.J. RTEN   8.8
    #> 483    DANNEHY,J.F. RTEN   7.9
    #> 484       DEAN,H.H. RTEN   7.7
    #> 485     DEVITA,H.J. RTEN   7.2
    #> 486   DRISCOLL,P.J. RTEN   7.7
    #> 487     GRILLO,A.E. RTEN   6.5
    #> 488  HADDEN,W.L.JR. RTEN   8.0
    #> 489     HAMILL,E.C. RTEN   7.6
    #> 490     HEALEY.A.H. RTEN   6.7
    #> 491       HULL,T.C. RTEN   7.4
    #> 492       LEVINE,I. RTEN   8.0
    #> 493   LEVISTER,R.L. RTEN   6.0
    #> 494     MARTIN,L.F. RTEN   7.3
    #> 495    MCGRATH,J.F. RTEN   6.6
    #> 496    MIGNONE,A.F. RTEN   5.2
    #> 497     MISSAL,H.M. RTEN   7.6
    #> 498     MULVEY,H.M. RTEN   8.7
    #> 499      NARUK,H.J. RTEN   9.0
    #> 500    O'BRIEN,F.J. RTEN   8.2
    #> 501 O'SULLIVAN,T.J. RTEN   8.7
    #> 502       PASKEY,L. RTEN   8.1
    #> 503    RUBINOW,J.E. RTEN   9.2
    #> 504      SADEN.G.A. RTEN   7.5
    #> 505 SATANIELLO,A.G. RTEN   7.9
    #> 506       SHEA,D.M. RTEN   8.3
    #> 507    SHEA,J.F.JR. RTEN   8.8
    #> 508      SIDOR,W.J. RTEN   5.3
    #> 509   SPEZIALE,J.A. RTEN   8.2
    #> 510     SPONZO,M.J. RTEN   8.0
    #> 511  STAPLETON,J.F. RTEN   7.7
    #> 512      TESTO,R.J. RTEN   7.0
    #> 513 TIERNEY,W.L.JR. RTEN   7.9
    #> 514       WALL,R.A. RTEN   6.6
    #> 515     WRIGHT,D.B. RTEN   8.1
    #> 516   ZARRILLI,K.J. RTEN   7.1

USPersonalExpenditure
---------------------

``` r
USPersonalExpenditure_tidy <- USPersonalExpenditure %>%
  as.data.frame() %>%
  mutate(group = rownames(.)) %>%
  gather(year, expense, -group)
USPersonalExpenditure_tidy
```

    #>                  group year expense
    #> 1     Food and Tobacco 1940  22.200
    #> 2  Household Operation 1940  10.500
    #> 3   Medical and Health 1940   3.530
    #> 4        Personal Care 1940   1.040
    #> 5    Private Education 1940   0.341
    #> 6     Food and Tobacco 1945  44.500
    #> 7  Household Operation 1945  15.500
    #> 8   Medical and Health 1945   5.760
    #> 9        Personal Care 1945   1.980
    #> 10   Private Education 1945   0.974
    #> 11    Food and Tobacco 1950  59.600
    #> 12 Household Operation 1950  29.000
    #> 13  Medical and Health 1950   9.710
    #> 14       Personal Care 1950   2.450
    #> 15   Private Education 1950   1.800
    #> 16    Food and Tobacco 1955  73.200
    #> 17 Household Operation 1955  36.500
    #> 18  Medical and Health 1955  14.000
    #> 19       Personal Care 1955   3.400
    #> 20   Private Education 1955   2.600
    #> 21    Food and Tobacco 1960  86.800
    #> 22 Household Operation 1960  46.200
    #> 23  Medical and Health 1960  21.100
    #> 24       Personal Care 1960   5.400
    #> 25   Private Education 1960   3.640

VADeaths
--------

``` r
VADeaths_tidy <- VADeaths %>%
  as.data.frame() %>%
  mutate(age = rownames(.)) %>%
  gather(demographic, deaths, -age)
VADeaths_tidy
```

    #>      age  demographic deaths
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
  gather(continent, value, -year) 
WPs_tidy
```

    #>    year continent value
    #> 1  1951    N.Amer 45939
    #> 2  1956    N.Amer 60423
    #> 3  1957    N.Amer 64721
    #> 4  1958    N.Amer 68484
    #> 5  1959    N.Amer 71799
    #> 6  1960    N.Amer 76036
    #> 7  1961    N.Amer 79831
    #> 8  1951    Europe 21574
    #> 9  1956    Europe 29990
    #> 10 1957    Europe 32510
    #> 11 1958    Europe 35218
    #> 12 1959    Europe 37598
    #> 13 1960    Europe 40341
    #> 14 1961    Europe 43173
    #> 15 1951      Asia  2876
    #> 16 1956      Asia  4708
    #> 17 1957      Asia  5230
    #> 18 1958      Asia  6662
    #> 19 1959      Asia  6856
    #> 20 1960      Asia  8220
    #> 21 1961      Asia  9053
    #> 22 1951    S.Amer  1815
    #> 23 1956    S.Amer  2568
    #> 24 1957    S.Amer  2695
    #> 25 1958    S.Amer  2845
    #> 26 1959    S.Amer  3000
    #> 27 1960    S.Amer  3145
    #> 28 1961    S.Amer  3338
    #> 29 1951   Oceania  1646
    #> 30 1956   Oceania  2366
    #> 31 1957   Oceania  2526
    #> 32 1958   Oceania  2691
    #> 33 1959   Oceania  2868
    #> 34 1960   Oceania  3054
    #> 35 1961   Oceania  3224
    #> 36 1951    Africa    89
    #> 37 1956    Africa  1411
    #> 38 1957    Africa  1546
    #> 39 1958    Africa  1663
    #> 40 1959    Africa  1769
    #> 41 1960    Africa  1905
    #> 42 1961    Africa  2005
    #> 43 1951  Mid.Amer   555
    #> 44 1956  Mid.Amer   733
    #> 45 1957  Mid.Amer   773
    #> 46 1958  Mid.Amer   836
    #> 47 1959  Mid.Amer   911
    #> 48 1960  Mid.Amer  1008
    #> 49 1961  Mid.Amer  1076
