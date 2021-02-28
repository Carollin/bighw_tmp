bighw
================
Chenyi Lin
2/28/2021

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.0 ─

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)
library(ggplot2)
```

\#\#\#\#Question: What time of day should you fly if you want to avoid
delays as much as possible? Arrive delay.  
Overview:time of the day

``` r
time <- flights%>%
  group_by(hour)%>%
  summarise(time_mean=mean(dep_delay,na.rm = TRUE))
```

``` r
ggplot(time, aes(hour,time_mean))+
  geom_point()
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Untitled_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Introduction:  
Withouting relating any factors(flights, airplane…), if you want to
avoid delay as less as possible, you can choose to take the flight
between 5:00am-12:00pm, between thsi peried, the average of
flights’delay are less than 10 minutes.

\#\#\#\#1. carrier Introduction:  
In overview, we already known the time between 5-12 in the morning has
the less delay time. So, in part 1, I want to know which airlines that
we should choose at these period.

``` r
carrierf<- flights%>%
  filter(hour>=5|hour<=10)%>%
  group_by(carrier)%>%
  summarise(mean_dep_delay=mean(dep_delay,na.rm = TRUE))
```

``` r
ggplot(carrierf,aes(x=mean_dep_delay,y=carrier))+
  geom_point()
```

![](Untitled_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
\#\#summary:  
In the period 5-12 in the morning, we can choose AS,HA, and Us
airlines.These only delay abut 5 minutes.

\#\#\#\#2. airports  
Same as part 1. We already known the time between 5-12 in the morning
has the less delay time. So, in part 1, I want to know which airports
that we should choose at these period.

``` r
flights%>%
  filter(hour>=5|hour<=10)%>%
  group_by(origin)%>%
  summarise(mean_dep_delay=mean(dep_delay,na.rm = TRUE))
```

    ## # A tibble: 3 x 2
    ##   origin mean_dep_delay
    ## * <chr>           <dbl>
    ## 1 EWR              15.1
    ## 2 JFK              12.1
    ## 3 LGA              10.3

\#\#Summary:  
In the period 5-12 in the morning, there is no big differnce between the
three origins’ means of the delay time. But the flights at LGA has the
less delay time, whcih is 12.3 minutes.

\#\#\#\#3. Weather:  
Same as before. We already known the time between 5-12 in the morning
has the less delay time. So, in part 3, how temperature affect delay
time.

``` r
flights2 <- flights %>%
  left_join(weather)
```

    ## Joining, by = c("year", "month", "day", "origin", "hour", "time_hour")

``` r
flights2
```

    ## # A tibble: 336,776 x 28
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # … with 336,766 more rows, and 20 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   temp <dbl>, dewp <dbl>, humid <dbl>, wind_dir <dbl>, wind_speed <dbl>,
    ## #   wind_gust <dbl>, precip <dbl>, pressure <dbl>, visib <dbl>

``` r
flights3 <- flights2%>%
  filter(hour>=5|hour<=10)%>%
  group_by(temp)%>%
  summarise(mean_dep_delay=mean(dep_delay,na.rm = TRUE))
```

``` r
ggplot(flights3,aes(x=temp,y=mean_dep_delay))+
  geom_point()
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Untitled_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
\#\#Summary:  
From the plot,In the period 5-12 in the morning, we can find that when
the temperature is around 50 Fahrenheit, the delay time is less.
