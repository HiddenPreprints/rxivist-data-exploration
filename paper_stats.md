bioRxiv preprint stats
================
Hao Ye
2019-09-05

# Setup

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

# Data Overview

See the `compute_paper_stats.R` file for the code used to generate the
paper stats data.

The data collects information on preprints, with one row per preprint.
Stats are computed by summing up monthly views and downloads for up to
the first 3 months after posting date (posted\_date + 3 months \>=
traffic\_date \>= posted\_date). Traffic stats are collected monthly, so
we assign a date equal to the last day of the month for the traffic.

Columns are: \* `id` - unique numerical id for the preprint \*
`downloads` - number of pdf downloads \* `views` - number of views of
the abstract \* `traffic_date` - latest traffic date for the summed
views and downloads \* `posted_date` - date of initial posting to
bioRxiv \* `collection` - the subject area designation on bioRxiv \*
`duration` - the \# of days, computed as traffic\_date - posted\_date +
1 \* `downloads_per_day` - downloads / duration \* `views_per_day` -
views / duration \* `pub_date` - date of publication of preprint (NA if
not yet published)

``` r
paper_stats <- readRDS("paper_stats.RDS")
str(paper_stats)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    56060 obs. of  10 variables:
    ##  $ id               : num  386 387 388 389 390 391 392 393 394 395 ...
    ##  $ downloads        : num  142 102 100 447 34 39 66 86 187 81 ...
    ##  $ views            : num  1065 520 504 1236 337 ...
    ##  $ traffic_date     : Date, format: "2018-07-31" "2018-10-31" ...
    ##  $ posted_date      : Date, format: "2018-05-21" "2018-08-03" ...
    ##  $ collection       : chr  "microbiology" "microbiology" "microbiology" "microbiology" ...
    ##  $ duration         : num  72 90 90 93 90 90 90 90 90 91 ...
    ##  $ downloads_per_day: num  1.972 1.133 1.111 4.806 0.378 ...
    ##  $ views_per_day    : num  14.79 5.78 5.6 13.29 3.74 ...
    ##  $ pub_date         : Date, format: "2019-01-04" "2019-01-29" ...

## Filter papers

We want to focus on “low-visibility” preprints, so use only the papers
that are unpublished or published after the last date of traffic
collection (posted date + 3 months).

``` r
paper_stats <- paper_stats %>%
    filter(is.na(pub_date) | 
               pub_date > traffic_date)
```

## Examine statistical quantiles of views per day

Compute quantiles:

``` r
views_quantiles <- paper_stats %>%
    select(collection, views_per_day, downloads_per_day) %>%
    nest(-collection) %>%
    mutate(views_q = map(data, ~ quantile(.$views_per_day, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))), 
           views_q = map(views_q, ~ bind_rows(.) %>% gather())) %>%
    select(-data) %>% 
    unnest() %>%
    mutate(key = factor(key, levels = c("5%", "10%", "25%", "50%", "75%", "90%", "95%"))) %>%
    spread(key, value)

knitr::kable(views_quantiles)
```

| collection                             |       5% |      10% |      25% |       50% |       75% |      90% |      95% |
| :------------------------------------- | -------: | -------: | -------: | --------: | --------: | -------: | -------: |
| animal-behavior-and-cognition          | 3.659956 | 4.098578 | 5.109633 |  6.723077 | 10.144444 | 15.86632 | 20.32472 |
| biochemistry                           | 3.634848 | 4.355556 | 5.619383 |  7.739130 | 11.244444 | 17.57143 | 24.95576 |
| bioengineering                         | 2.871371 | 3.360342 | 4.476923 |  6.181818 |  9.748686 | 17.28893 | 26.93382 |
| bioinformatics                         | 4.375417 | 5.303664 | 7.220319 | 10.561910 | 16.699878 | 26.29810 | 35.50930 |
| biophysics                             | 3.584343 | 4.363636 | 5.644958 |  7.854839 | 11.294733 | 17.41772 | 24.72743 |
| cancer-biology                         | 3.382740 | 4.102465 | 5.380282 |  7.473106 | 11.143809 | 17.71127 | 23.20409 |
| cell-biology                           | 3.505562 | 4.112500 | 5.480379 |  7.666667 | 11.657239 | 18.76923 | 25.71091 |
| clinical-trials                        | 4.571548 | 4.906239 | 6.744136 |  8.873563 | 12.554486 | 17.85068 | 26.67536 |
| developmental-biology                  | 3.502465 | 4.297835 | 5.847222 |  7.962025 | 11.724138 | 17.66352 | 23.45082 |
| ecology                                | 2.394613 | 2.925329 | 3.895943 |  5.384168 |  8.100733 | 12.18857 | 15.77949 |
| epidemiology                           | 2.322565 | 2.723317 | 3.597170 |  4.894845 |  7.175446 | 11.47048 | 15.45469 |
| evolutionary-biology                   | 3.453079 | 4.212121 | 5.666667 |  8.173913 | 12.444272 | 19.60377 | 27.11551 |
| genetics                               | 3.727032 | 4.517102 | 6.357519 |  9.656470 | 15.657213 | 27.35199 | 41.62478 |
| genomics                               | 4.595370 | 5.582189 | 8.000000 | 12.476744 | 21.559524 | 37.78824 | 55.14722 |
| immunology                             | 3.801020 | 4.461326 | 5.723077 |  7.666667 | 11.616279 | 18.28019 | 25.58618 |
| microbiology                           | 2.918966 | 3.442377 | 4.574856 |  6.560976 | 10.258444 | 16.58691 | 22.74561 |
| molecular-biology                      | 3.255369 | 3.935508 | 5.199466 |  7.612903 | 12.171359 | 20.34815 | 28.45934 |
| neuroscience                           | 3.376471 | 4.120000 | 5.580247 |  8.000000 | 12.423224 | 19.25397 | 26.92897 |
| NULL                                   | 1.379310 | 1.602410 | 6.256410 |  8.484849 | 15.517857 | 30.10526 | 86.27273 |
| paleontology                           | 4.528357 | 4.808138 | 6.977011 | 10.285714 | 12.721519 | 18.32421 | 30.40724 |
| pathology                              | 2.612284 | 2.993182 | 3.647636 |  4.699631 |  6.695514 | 11.05046 | 15.73520 |
| pharmacology-and-toxicology            | 2.681746 | 3.151899 | 3.925926 |  4.943662 |  7.207317 | 10.91858 | 14.37532 |
| physiology                             | 2.305619 | 2.727273 | 3.404054 |  4.616377 |  6.923115 | 10.96394 | 14.20284 |
| plant-biology                          | 3.918788 | 4.814074 | 6.235294 |  8.869048 | 12.640625 | 18.71366 | 23.76184 |
| scientific-communication-and-education | 2.885185 | 3.559551 | 4.524390 |  7.729730 | 17.641975 | 48.30246 | 87.45330 |
| synthetic-biology                      | 4.857839 | 6.072276 | 8.103596 | 11.521164 | 18.361991 | 31.28075 | 46.76590 |
| systems-biology                        | 3.728750 | 4.713350 | 6.333333 |  8.554886 | 13.128507 | 20.64376 | 27.93924 |
| zoology                                | 2.900903 | 3.257258 | 4.137005 |  5.409350 |  7.438027 | 10.39593 | 13.66403 |

## Distributions of views per day

``` r
paper_stats %>%
    ggplot(aes(x = views_per_day, fill = views_per_day)) + 
    geom_density() + 
    coord_cartesian(xlim = c(0, 100)) + 
    facet_wrap(~collection, scales = "free", ncol = 4) + 
    theme_bw(base_size = 16) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](paper_stats_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## How does traffic decrease over time

``` r
db <- readRDS("raw_stats.RDS")

set.seed(42)
selected_ids <- sample(db$id, 50)

db %>%
    filter(id %in% selected_ids) %>%
    mutate(id = factor(id)) %>%
    group_by(id) %>%
    ggplot(aes(x = traffic_date - posted, y = views, color = id)) + 
    geom_line() + 
    theme_bw(base_size = 20) + 
    guides(color = "none")
```

    ## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.

![](paper_stats_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
