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

Columns are:

  - `id` - unique numerical id for the preprint
  - `downloads` - number of pdf downloads
  - `views` - number of views of the abstract
  - `traffic_date` - latest traffic date for the summed views and
    downloads
  - `posted_date` - date of initial posting to bioRxiv
  - `collection` - the subject area designation on bioRxiv
  - `duration` - the \# of days, computed as traffic\_date -
    posted\_date + 1
  - `downloads_per_day` - downloads / duration
  - `views_per_day` - views / duration
  - `pub_date` - date of publication of preprint (NA if not yet
    published)

<!-- end list -->

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
    mutate(unpublished = is.na(pub_date) | 
               pub_date > traffic_date)
```

## Examine statistical quantiles of views per day

Compute quantiles:

``` r
quantile_vec <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

views_quantiles <- paper_stats %>%
    filter(unpublished) %>%
    select(collection, views_per_day, downloads_per_day) %>%
    nest(-collection) %>%
    mutate(views_q = map(data, ~ quantile(.$views_per_day, probs = quantile_vec)), 
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
    ggplot(aes(x = views_per_day, color = unpublished)) + 
    geom_density() + 
    coord_cartesian(xlim = c(0, 100)) + 
    facet_wrap(~collection, scales = "free", ncol = 4) + 
    theme_bw(base_size = 12) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position  = "top")
```

    ## Warning: Groups with fewer than two data points have been dropped.

![](paper_stats_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## How does recency of a preprint affect views

With more preprints being posted, we might expect the \# of views per
day to change with recency. For the least-visible preprints, the number
of views per day seems to be mostly constant. However, depending on
category, the upper-quantiles can vary, sometimes decreasing with
recency if sample sizes are small.

``` r
paper_stats %>%
    ggplot(aes(x = posted_date, y = views_per_day)) + 
    geom_point() + 
    geom_quantile(quantiles = quantile_vec, formula = "y ~ x") + 
    facet_wrap(~collection, scales = "free", ncol = 4) + 
    theme_bw(base_size = 12)
```

![](paper_stats_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## How does traffic decrease over time for a sample of preprints

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

![](paper_stats_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Compute an “Invisibility” index

Compute index as fraction of preprints in the same collection with at
least as many views\_per\_day.

``` r
paper_indices <- paper_stats %>%
    group_by(collection) %>%
    mutate(index = rank(views_per_day, ties.method = "min"), 
           index = 1 - index / n()) %>%
    ungroup() %>%
    select(id, collection, views_per_day, index, 
           traffic_date, posted_date, pub_date, 
           views, downloads)
```

## Select a few papers per collection to display

``` r
title_lookup <- db %>%
    select(id, title) %>%
    distinct()

selected_papers <- paper_indices %>%
    group_by(collection) %>%
    top_n(10, index) %>%
    arrange(collection, desc(index)) %>%
    left_join(title_lookup, by = "id") %>%
    mutate(pub = ifelse(is.na(pub_date), "N", "Y")) %>%
    select(collection, index, 
           "views/day" = views_per_day, 
           posted = posted_date, pub, title)

knitr::kable(selected_papers, digits = 3)
```

| collection                             | index | views/day | posted     | pub | title                                                                                                                                                                                                          |
| :------------------------------------- | ----: | --------: | :--------- | :-- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| animal-behavior-and-cognition          | 0.999 |     1.831 | 2014-08-28 | Y   | Amino acid and carbohydrate tradeoffs by honey bee nectar foragers and their implications for plant-pollinator interactions                                                                                    |
| animal-behavior-and-cognition          | 0.998 |     1.877 | 2017-07-20 | N   | Pitch discrimination performance of ferrets and humans on a go/no-go task.                                                                                                                                     |
| animal-behavior-and-cognition          | 0.997 |     1.931 | 2014-08-06 | Y   | Ontogeny of aerial righting and wing flapping in juvenile birds                                                                                                                                                |
| animal-behavior-and-cognition          | 0.995 |     2.145 | 2018-02-28 | N   | Ontogeny of familiarity with foraging landscape and foraging abilities in the tropical social wasp Ropalidia marginata                                                                                         |
| animal-behavior-and-cognition          | 0.994 |     2.341 | 2018-12-03 | N   | Adaptations in the echolocation behavior of fruit-eating bats when orienting under challenging conditions                                                                                                      |
| animal-behavior-and-cognition          | 0.993 |     2.435 | 2017-06-01 | N   | Recency Order Judgments In Short Term Memory: Replication And Extension Of Hacker (1980)                                                                                                                       |
| animal-behavior-and-cognition          | 0.992 |     2.462 | 2018-08-02 | N   | Brood-holding causes workers to pay attention to the queen in the carpenter ant Camponotus japonicus                                                                                                           |
| animal-behavior-and-cognition          | 0.991 |     2.464 | 2014-09-08 | N   | SPOILS OF WAR AND PEACE: ENEMY ADOPTION AND QUEEN-RIGHT COLONY FUSION FOLLOW COSTLY INTRASPECIFIC CONFLICT IN ACACIA ANTS                                                                                      |
| animal-behavior-and-cognition          | 0.990 |     2.473 | 2018-05-02 | N   | Dynamic gait transition in the Scolopendromorpha scolopocryptops rubiginosus L. Koch centipede                                                                                                                 |
| animal-behavior-and-cognition          | 0.988 |     2.543 | 2014-08-12 | N   | A quantitative analysis of objective feather colour assessment: measurements in the lab are more reliable than in the field                                                                                    |
| biochemistry                           | 0.999 |     1.880 | 2018-10-01 | Y   | Crystal structure of a Thermus aquaticus diversity-generating retroelement variable protein                                                                                                                    |
| biochemistry                           | 0.999 |     2.014 | 2017-11-21 | N   | Fe2+/H2O2-mediated oxidation of homogentisic acid indicates the production of ochronotic and non-ochronotic pigments. Implications in Alkaptonuria and beyond.                                                 |
| biochemistry                           | 0.998 |     2.034 | 2019-02-01 | N   | Effect of flow rate and freezing on cyanocobalamin recovery using a commercial solid phase extraction cartridge.                                                                                               |
| biochemistry                           | 0.998 |     2.145 | 2014-01-22 | Y   | Correcting a SHAPE-directed RNA structure by a mutate-map-rescue approach                                                                                                                                      |
| biochemistry                           | 0.997 |     2.145 | 2019-07-01 | N   | Characterization of Sorbitol Dehydrogenase SmoS from Sinorhizobium meliloti 1021                                                                                                                               |
| biochemistry                           | 0.996 |     2.284 | 2018-01-10 | N   | 216 GHz Electron Paramagnetic Resonance of Mycobacterium Tuberculosis Catalase-Peroxidase: The Role of the Arg418 Residue                                                                                      |
| biochemistry                           | 0.996 |     2.342 | 2015-02-11 | N   | The experimental determination of redox-potential of mutated cytochrome c with eight charge changing substitutions                                                                                             |
| biochemistry                           | 0.995 |     2.360 | 2018-01-02 | Y   | Biochemical analyses reveal amino acid residues critical for cell cycle-dependent phosphorylation of human Cdc14A phosphatase by cyclin-dependent kinase 1                                                     |
| biochemistry                           | 0.994 |     2.371 | 2019-02-01 | N   | Development of Triazoles based on AZT and their Anti-Viral Activity Against HIV-1                                                                                                                              |
| biochemistry                           | 0.994 |     2.429 | 2018-10-02 | Y   | Structural and biochemical characterization on the cognate and heterologous interactions of the MazEF-mt9 TA system                                                                                            |
| bioengineering                         | 0.999 |     1.097 | 2019-07-01 | N   | STATIC LOADING OF THE KNEE JOINT RESULTS IN MODIFIED SINGLE LEG LANDING BIOMECHANICS                                                                                                                           |
| bioengineering                         | 0.998 |     1.430 | 2015-07-07 | Y   | Automated segmentation of skin strata in reflectance confocal microscopy depth stacks                                                                                                                          |
| bioengineering                         | 0.997 |     1.598 | 2019-03-01 | N   | Deconvolving multiplexed protease signatures with substrate reduction and activity clustering                                                                                                                  |
| bioengineering                         | 0.996 |     1.620 | 2019-05-01 | N   | Targeting Fe3O4 Au nanoparticles in photoacoustic nuclear magnetic dual-mode imaging                                                                                                                           |
| bioengineering                         | 0.995 |     1.652 | 2018-11-01 | N   | Osmotic Swelling Responses are Conserved Across Cartilaginous Tissues with Varied Sulfated-Glycosaminoglycan Contents                                                                                          |
| bioengineering                         | 0.994 |     1.717 | 2018-11-01 | Y   | Structural and Practical Identifiability of Dual-input Kinetic Modeling in Dynamic PET of Liver Inflammation                                                                                                   |
| bioengineering                         | 0.993 |     1.985 | 2018-11-26 | Y   | Thermopneumatic suction integrated microfluidic blood analysis system                                                                                                                                          |
| bioengineering                         | 0.993 |     2.016 | 2019-07-01 | N   | Tissue-specific changes in size and shape of the ligaments and tendons of the porcine knee during post-natal growth                                                                                            |
| bioengineering                         | 0.992 |     2.033 | 2019-03-01 | N   | Molecular Phenotyping of Oxidative Stress in Diabetes Mellitus with Point-of-care NMR system                                                                                                                   |
| bioengineering                         | 0.991 |     2.087 | 2019-03-01 | Y   | The Microbial Metagenome and Tissue Composition in Mice with Microbiome-Induced Reductions in Bone Strength                                                                                                    |
| bioinformatics                         | 1.000 |     0.718 | 2013-11-15 | N   | Gappy TotalReCaller for RNASeq Base-Calling and Mapping                                                                                                                                                        |
| bioinformatics                         | 1.000 |     0.795 | 2013-11-15 | Y   | Unexpected links reflect the noise in networks                                                                                                                                                                 |
| bioinformatics                         | 0.999 |     1.161 | 2014-02-03 | N   | Improving Protein Docking with Constraint Programming and Coevolution Data                                                                                                                                     |
| bioinformatics                         | 0.999 |     1.468 | 2019-07-01 | N   | A geometric approach to human stress based on stress-related surrogate measures                                                                                                                                |
| bioinformatics                         | 0.999 |     1.482 | 2014-04-09 | N   | A signature of power law network dynamics                                                                                                                                                                      |
| bioinformatics                         | 0.999 |     1.515 | 2013-11-25 | Y   | Inferring tree causal models of cancer progression with probability raising                                                                                                                                    |
| bioinformatics                         | 0.999 |     1.582 | 2013-12-12 | Y   | libRoadRunner: A High Performance SBML Compliant Simulator                                                                                                                                                     |
| bioinformatics                         | 0.999 |     1.598 | 2018-11-01 | N   | Predicting JNK1 Inhibitors Regulating Autophagy in Cancer using Random Forest Classifier                                                                                                                       |
| bioinformatics                         | 0.998 |     1.691 | 2014-07-25 | N   | Data mining of Gene expression profiles of Saccharomyces cerevisiae in response to mild heat stress response                                                                                                   |
| bioinformatics                         | 0.998 |     1.705 | 2018-12-03 | N   | Bayesian parameter estimation in non-stationary semiflexible polymers from ensembles of trajectories                                                                                                           |
| biophysics                             | 1.000 |     0.974 | 2014-01-13 | N   | Quantification of nuclear transport in single cells                                                                                                                                                            |
| biophysics                             | 0.999 |     1.146 | 2018-01-02 | Y   | Modified Potential Functions Result in Enhanced Predictions of a Protein Complex by All-Atom MD Simulations, Confirming a Step-wise Association Process for Native PPIs                                        |
| biophysics                             | 0.999 |     1.393 | 2014-06-04 | Y   | Spatial epidemiology of networked metapopulation: An overview                                                                                                                                                  |
| biophysics                             | 0.998 |     1.423 | 2014-01-13 | Y   | Shifts in stability and control effectiveness during evolution of Paraves support aerial maneuvering hypotheses for flight origins                                                                             |
| biophysics                             | 0.998 |     1.553 | 2016-09-16 | Y   | Single variant bottleneck in the early dynamics of H. influenzae bacteremia in neonatal rats questions the theory of independent action                                                                        |
| biophysics                             | 0.997 |     1.618 | 2014-03-04 | N   | Does ‘information control the living state’?                                                                                                                                                                   |
| biophysics                             | 0.997 |     1.620 | 2018-10-01 | N   | An information thermodynamic approach quantifying MAPK-related signaling cascades by average entropy production rate                                                                                           |
| biophysics                             | 0.997 |     1.633 | 2018-04-02 | N   | Velocity and Diameter Measurements of Penetrating Arteries by Model Based Analysis of Complex Difference Images in Phase Contrast MRI                                                                          |
| biophysics                             | 0.996 |     1.707 | 2018-10-01 | N   | How to obtain cell volume from dynamic pH, temperature and pressure in plants                                                                                                                                  |
| biophysics                             | 0.996 |     1.707 | 2019-03-01 | Y   | Robust Determination of Protein Allosteric Signaling Pathways                                                                                                                                                  |
| cancer-biology                         | 0.999 |     1.457 | 2018-10-01 | Y   | A seleno-hormetine protects bone marrow hematopoietic cells against ionizing radiation-induced toxicity                                                                                                        |
| cancer-biology                         | 0.999 |     1.726 | 2019-07-01 | N   | Reducing expression of dynamin-related protein 1 increases radiation sensitivity of glioblastoma cells                                                                                                         |
| cancer-biology                         | 0.998 |     1.753 | 2018-12-02 | Y   | Dynamic urinary proteomic analysis in a Walker 256 intracerebral tumor model                                                                                                                                   |
| cancer-biology                         | 0.998 |     1.774 | 2019-07-01 | N   | Injectable diblock copolypeptide hydrogel provides platform to maintain high local concentrations of taxol and local tumor control                                                                             |
| cancer-biology                         | 0.997 |     1.778 | 2019-04-02 | N   | Different localization of fluorescently labeled N- and C-termini of nucleolin variants in human glioblastoma cell culture                                                                                      |
| cancer-biology                         | 0.997 |     1.884 | 2013-11-07 | Y   | A filter-flow perspective of hematogenous metastasis offers a non-genetic paradigm for personalized cancer therapy                                                                                             |
| cancer-biology                         | 0.996 |     1.944 | 2018-01-02 | N   | Candidate urine biomarker discovery from only five pairs of samples before and after tumor resection in glioma patients                                                                                        |
| cancer-biology                         | 0.996 |     1.967 | 2019-03-03 | Y   | MicroRNA-138 negatively regulates the hypoxia-inducible factor 1α to suppress melanoma growth and metastasis                                                                                                   |
| cancer-biology                         | 0.995 |     1.978 | 2018-04-01 | N   | Co-expression of MDM2 and CDK4 in transformed human mesenchymal stem cells induces dedifferentiated liposarcoma potency                                                                                        |
| cancer-biology                         | 0.994 |     1.989 | 2013-12-02 | N   | Mutated SF3B1 is associated with transcript isoform changes of the genes UQCC and RPL31 both in CLLs and uveal melanomas                                                                                       |
| cell-biology                           | 1.000 |     1.228 | 2019-05-01 | N   | The Ras family members follow the blood progesterone level during formation and regression in bovine corpus luteum                                                                                             |
| cell-biology                           | 0.999 |     1.294 | 2018-02-05 | N   | Ultrastructure of viruliferous Javesella pellucida transmitting Festuca leaf streak virus (genus Cytorhabdovirus)                                                                                              |
| cell-biology                           | 0.999 |     1.554 | 2019-05-01 | Y   | Impact of Itga2-Gp6-double collagen receptor deficient mice for bone marrow megakaryocytes and platelets                                                                                                       |
| cell-biology                           | 0.999 |     1.658 | 2016-09-19 | Y   | N-Acyl Dopamines Induce Apoptosis in PC12 Cell Line via the O-1918-Sensitive non-CB1-non-CB2 Receptor                                                                                                          |
| cell-biology                           | 0.998 |     1.697 | 2018-12-02 | N   | Neutrophil Elastase Activates Macrophage MMPs, Promotes Cell Adhesion And Cytokine Production Via Integrin-Src Kinases Pathway                                                                                 |
| cell-biology                           | 0.998 |     1.831 | 2013-11-22 | N   | Journey to the Center of the Mitochondria Guided by the Tail Anchor of Protein Tyrosine Phosphatase 1B                                                                                                         |
| cell-biology                           | 0.998 |     1.831 | 2018-11-22 | N   | Effects of 5-aza-2 ́-deoxycytidine on human osteoarthritic chondrocytes                                                                                                                                        |
| cell-biology                           | 0.997 |     1.921 | 2016-09-16 | Y   | Lipid metabolic perturbation is an early-onset phenotype in adult spin mutants: a Drosophila model for lysosomal storage disorders                                                                             |
| cell-biology                           | 0.997 |     1.933 | 2019-04-03 | N   | Potential role of heat shock protein 90 in regulation of hyperthermia-driven differentiation of neural stem cells                                                                                              |
| cell-biology                           | 0.996 |     1.935 | 2019-03-01 | N   | Predicting nucleation sites in chemotaxing Dictyostelium discoideum                                                                                                                                            |
| clinical-trials                        | 0.990 |     3.429 | 2016-12-07 | N   | Optimizing Communication of Emergency Response Adaptive Randomization Clinical Trials to Potential Participants                                                                                                |
| clinical-trials                        | 0.980 |     3.901 | 2018-11-02 | Y   | Characterization of intact proviruses in blood and lymph node from HIV-infected individuals undergoing analytical treatment interruption                                                                       |
| clinical-trials                        | 0.970 |     4.094 | 2017-06-08 | Y   | Performance Of Children With Autism In Parent-Administered Cognitive And Language Exercises                                                                                                                    |
| clinical-trials                        | 0.960 |     4.114 | 2016-11-05 | N   | Cis-Compound Mutations are Prevalent in Triple Negative Breast Cancer and Can Drive Tumor Progression                                                                                                          |
| clinical-trials                        | 0.949 |     4.131 | 2018-12-07 | N   | Brain volumes of very low birth weight infants measured by two dimensional cranial ultrasonography, a prospective cohort study                                                                                 |
| clinical-trials                        | 0.939 |     4.547 | 2018-04-06 | N   | Economic evaluation of a healthy lifestyle intervention for chronic low back pain: a randomised controlled trial                                                                                               |
| clinical-trials                        | 0.929 |     4.582 | 2018-11-14 | N   | A factorial cluster-randomised controlled trial combining home-environmental and early child development interventions to improve child health and development: rationale, trial design and baseline findings. |
| clinical-trials                        | 0.919 |     4.750 | 2018-09-20 | N   | Predictive Modelling of The Dynamic Patterns of Thinking in Attention-Deficit/Hyperactivity Disorder: Diagnostic Accuracy of Spatiotemporal Fractal Measures                                                   |
| clinical-trials                        | 0.909 |     4.785 | 2017-06-14 | Y   | A multicenter, randomized study of decitabine as epigenetic priming with induction chemotherapy in children with AML                                                                                           |
| clinical-trials                        | 0.899 |     4.835 | 2018-11-02 | N   | Meropenem vs standard of care for treatment of neonatal late onset sepsis (NeoMero1): a randomised controlled trial                                                                                            |
| developmental-biology                  | 0.999 |     1.870 | 2014-10-24 | N   | Interplay of TGFb superfamily members governs optic fissure closure                                                                                                                                            |
| developmental-biology                  | 0.999 |     1.871 | 2014-10-08 | Y   | Epithelial flow into the optic cup facilitated by suppression of BMP drives eye morphogenesis                                                                                                                  |
| developmental-biology                  | 0.998 |     1.933 | 2014-10-03 | N   | DevoWorm: differentiation waves and computation in C. elegans embryogenesis                                                                                                                                    |
| developmental-biology                  | 0.998 |     2.065 | 2018-11-01 | N   | Inferring novel lncRNA associated with Ventricular septal defect by DNA methylation interaction network                                                                                                        |
| developmental-biology                  | 0.997 |     2.116 | 2015-07-07 | N   | The relationship between interpregnancy interval (IP) and mother’s preceding pregnancies                                                                                                                       |
| developmental-biology                  | 0.996 |     2.146 | 2017-07-04 | N   | Physiological tolerance of the early life history stages of fresh water prawn (Macrobrachium rosenbergii De Man, 1879) to environmental stress                                                                 |
| developmental-biology                  | 0.996 |     2.150 | 2017-07-13 | N   | Cadmium Disrupts Vestibular Function by Interfering with Otolith Formation                                                                                                                                     |
| developmental-biology                  | 0.995 |     2.176 | 2015-09-01 | Y   | Species Tailoured Contribution of Volumetric Growth and Tissue Convergence to Posterior Body Elongation in Vertebrates                                                                                         |
| developmental-biology                  | 0.995 |     2.176 | 2015-11-08 | N   | Assisted reproduction causes intrauterus growth restriction by disrupting placental lipid metabolism                                                                                                           |
| developmental-biology                  | 0.994 |     2.195 | 2017-06-06 | N   | Impairment of zebrafish reproduction upon exposure to melengestrol acetate                                                                                                                                     |
| ecology                                | 1.000 |     0.662 | 2014-01-14 | N   | Emergence of structural and dynamical properties of ecological mutualistic networks                                                                                                                            |
| ecology                                | 0.999 |     0.716 | 2014-01-17 | N   | Estimating seed bank accumulation and dynamics in three obligate-seeder Proteaceae species                                                                                                                     |
| ecology                                | 0.999 |     0.856 | 2019-04-02 | Y   | Managing congestion at visitor hotspots using park-level use level data: Case study of a Chinese World Heritage site                                                                                           |
| ecology                                | 0.998 |     0.989 | 2019-05-01 | N   | Feral pig exclusion fencing provides limited fish conservation value on tropical floodplains                                                                                                                   |
| ecology                                | 0.998 |     1.000 | 2013-11-19 | N   | Sap flow through petioles and petioles reveals leaf-level responses to light and vapor pressure deficit in the tropical tree Tabebuia rosea (Bignoniaceae)                                                     |
| ecology                                | 0.998 |     1.000 | 2018-12-03 | N   | Population dynamics of Cymodocea nodosa under future ocean scenarios.                                                                                                                                          |
| ecology                                | 0.997 |     1.016 | 2019-07-01 | N   | Effects of simulated acid rain on soil respiration and microbial community in a Moso bamboo forest in subtropical China                                                                                        |
| ecology                                | 0.997 |     1.023 | 2018-12-03 | N   | Implementation of the Perfect Plasticity Approximation with biogeochemical compartments in R                                                                                                                   |
| ecology                                | 0.996 |     1.103 | 2013-11-25 | Y   | The Effectiveness of Chinas National Forest Protection Program and National-level Nature Reserves, 2000 to 2010                                                                                               |
| ecology                                | 0.996 |     1.113 | 2019-07-01 | N   | Seasonal diversity of soil microarthropods in two different vegetable plots of Aligarh-India                                                                                                                   |
| epidemiology                           | 0.999 |     0.933 | 2019-04-02 | N   | Primary pterygium was not associated with corneal endothelial cell decrease in a rural Chinese population                                                                                                      |
| epidemiology                           | 0.999 |     1.078 | 2019-04-02 | N   | Prevalence and socioeconomic determinants of development delay among children in Ceará, Brazil: a population-based study                                                                                       |
| epidemiology                           | 0.998 |     1.213 | 2019-02-01 | N   | Countries are out of step with international recommendations for tuberculosis testing, treatment, and care: Findings from a 29-country survey of policy adoption and implementation                            |
| epidemiology                           | 0.997 |     1.295 | 2018-12-03 | N   | Health seeking behaviour and cost of fever treatment to households in a malaria endemic setting of northern Ghana: A cross sectional study                                                                     |
| epidemiology                           | 0.997 |     1.344 | 2019-04-02 | N   | Prevalence and determinants of neonatal danger signs in northwest Ethiopia: a multilevel analysis                                                                                                              |
| epidemiology                           | 0.996 |     1.371 | 2019-02-01 | N   | More accuracy estimation of the worm burden in the ascariasis of children in Kinshasa                                                                                                                          |
| epidemiology                           | 0.995 |     1.371 | 2018-11-23 | N   | Regional reinfection by Dengue: a network approach using data from Mexico                                                                                                                                      |
| epidemiology                           | 0.995 |     1.378 | 2019-04-02 | N   | Nasal and gut microbiota for sows of different health status within six commercial swine farms from one swine production system                                                                                |
| epidemiology                           | 0.994 |     1.391 | 2019-05-01 | N   | Role of ethnicity and socio-economic status (SES) in the presentation of retinoblastoma: findings from the UK                                                                                                  |
| epidemiology                           | 0.993 |     1.398 | 2018-12-03 | N   | Mass media promotion of a smartphone smoking cessation app: Modeled health and cost impacts                                                                                                                    |
| evolutionary-biology                   | 1.000 |     0.756 | 2013-11-07 | Y   | A Scalable Formulation for Engineering Combination Therapies for Evolutionary Dynamics of Disease                                                                                                              |
| evolutionary-biology                   | 0.999 |     0.965 | 2013-11-07 | Y   | Speciation and introgression between Mimulus nasutus and Mimulus guttatus                                                                                                                                      |
| evolutionary-biology                   | 0.999 |     1.091 | 2018-12-03 | N   | Evolution of host resistance and damage-limiting mechanisms to an emerging bacterial pathogen                                                                                                                  |
| evolutionary-biology                   | 0.999 |     1.100 | 2014-02-20 | Y   | Mapping the structure of drosophilid behavior                                                                                                                                                                  |
| evolutionary-biology                   | 0.999 |     1.293 | 2018-10-01 | N   | Sequence data from the internal and external transcribed spacers of nuclear ribosomal DNA of Cyclamen purpurascens allow geographic mapping                                                                    |
| evolutionary-biology                   | 0.998 |     1.347 | 2014-03-21 | N   | Response to a population bottleneck can be used to infer recessive selection                                                                                                                                   |
| evolutionary-biology                   | 0.998 |     1.351 | 2013-12-17 | N   | Direct Reciprocity Under Uncertainty Does Not Explain One-Shot Cooperation, But It Can Explain Norm Psychology                                                                                                 |
| evolutionary-biology                   | 0.998 |     1.371 | 2014-01-21 | Y   | Coalescence 2.0: a multiple branching of recent theoretical developments and their applications                                                                                                                |
| evolutionary-biology                   | 0.998 |     1.375 | 2018-12-03 | N   | The Olson conjecture for discrete public goods                                                                                                                                                                 |
| evolutionary-biology                   | 0.997 |     1.388 | 2018-11-26 | N   | Immunological female role tested on artificial plugs in three scorpion species                                                                                                                                 |
| genetics                               | 1.000 |     1.295 | 2018-12-03 | N   | Mutational analysis of N-ethyl-N-nitrosourea (ENU) in the fission yeast Schizosaccharomyces pombe.                                                                                                             |
| genetics                               | 0.999 |     1.364 | 2018-12-03 | Y   | Construction of a genetic linkage map in Pyropia yezoensis (Bangiales, Rhodophyta) and QTL analysis of several economic traits of blades                                                                       |
| genetics                               | 0.999 |     1.511 | 2018-12-03 | N   | Genetic interactions among Ghd7, Ghd7.1, Ghd8 and Hd1 contribute to large variation in heading date in rice                                                                                                    |
| genetics                               | 0.999 |     1.584 | 2018-12-02 | N   | A combined in silico, in vitro and clinical approach to characterise novel pathogenic missense variants in PRPF31 in retinitis pigmentosa                                                                      |
| genetics                               | 0.998 |     1.790 | 2013-11-12 | Y   | Matchmaker, Matchmaker, Make Me a Match: Migration of Populations via Marriages in the Past                                                                                                                    |
| genetics                               | 0.998 |     1.888 | 2019-02-01 | Y   | Plasmodium knowlesi clinical isolates from Malaysia show extensive diversity and strong differential selection pressure at the merozoite surface protein 7D (MSP7D)                                            |
| genetics                               | 0.998 |     1.919 | 2019-07-01 | N   | Linkage disequilibrium and haplotype block patterns in popcorn populations                                                                                                                                     |
| genetics                               | 0.998 |     2.099 | 2019-05-02 | N   | Models for infantile hypertrophic pyloric stenosis development in patients with esophageal atresia                                                                                                             |
| genetics                               | 0.997 |     2.120 | 2018-05-01 | Y   | Antioxidant activity and phycoremediation ability of four cyanobacterial isolates obtained from a stressed aquatic system                                                                                      |
| genetics                               | 0.997 |     2.127 | 2013-11-14 | Y   | The evolution of sex differences in disease genetics                                                                                                                                                           |
| genomics                               | 1.000 |     0.915 | 2013-11-22 | N   | Genomics via Optical Mapping (I): 0-1 Laws for Mapping with Single Molecules                                                                                                                                   |
| genomics                               | 0.999 |     1.413 | 2013-11-13 | Y   | On the Reproducibility of TCGA Ovarian Cancer MicroRNA Profiles                                                                                                                                                |
| genomics                               | 0.999 |     1.476 | 2014-01-28 | N   | Transcriptome pyrosequencing of abnormal phenotypes in Trypanosoma cruzi epimastigotes after ectopic expression of a small zinc finger protein                                                                 |
| genomics                               | 0.999 |     1.678 | 2014-05-06 | N   | N-BLR, a primate-specific non-coding transcript, modulates the epithelial-to-mesenchymal transition and leads to colorectal cancer invasion and migration                                                      |
| genomics                               | 0.999 |     1.741 | 2013-11-12 | Y   | A genome wide dosage suppressor network reveals genetic robustness and a novel mechanism for Huntingtons disease                                                                                              |
| genomics                               | 0.998 |     1.750 | 2014-01-23 | Y   | Genome-wide patterns of copy number variation in the diversified chicken genomes using next-generation sequencing                                                                                              |
| genomics                               | 0.998 |     1.778 | 2013-11-12 | N   | A Complete Public Domain Family Genomics Dataset                                                                                                                                                               |
| genomics                               | 0.998 |     1.822 | 2014-08-20 | Y   | Genome-wide Comparative Analysis Reveals Possible Common Ancestors of NBS Domain Containing Genes in Hybrid Citrus sinensis Genome and Original Citrus clementina Genome                                       |
| genomics                               | 0.998 |     1.897 | 2013-11-25 | N   | The-LHON-Enigma: explaining the behaviour of Lebers Hereditary Optic Neuropathy by the use of a simple computer model                                                                                         |
| genomics                               | 0.997 |     1.975 | 2014-04-11 | Y   | Whole Genome Bisulfite Sequencing of Cell Free DNA and its Cellular Contributors Uncovers Placenta Hypomethylated Domains                                                                                      |
| immunology                             | 0.999 |     0.753 | 2013-11-16 | N   | Lack of evidence for the presence of an interferon in invertebrate                                                                                                                                             |
| immunology                             | 0.999 |     0.922 | 2014-04-28 | Y   | Lack of association between Toll Like Receptor-2 & Toll Like Receptor-4 Gene Polymorphisms and Iranian Asthmatics risk or features                                                                             |
| immunology                             | 0.998 |     1.128 | 2014-02-12 | Y   | Genomic V-gene repertoire in reptiles                                                                                                                                                                          |
| immunology                             | 0.997 |     1.194 | 2017-06-30 | Y   | CD18-mediated adhesion is required for lung inflammation induced by mononuclear cell-derived extracellular vesicles                                                                                            |
| immunology                             | 0.996 |     1.716 | 2014-01-17 | Y   | Tracking global changes induced in the CD4 T cell receptor repertoire by immunization with a complex antigen using short stretches of CDR3 protein sequence.                                                   |
| immunology                             | 0.996 |     1.773 | 2014-06-27 | N   | Regulatory vs. helper CD4+ T-cell ratios and the progression of HIV/AIDS disease                                                                                                                               |
| immunology                             | 0.995 |     1.788 | 2014-07-08 | N   | A framework of immunities for pathogens and hypersensitivities                                                                                                                                                 |
| immunology                             | 0.994 |     1.791 | 2017-06-02 | N   | Mycophenolate Mofetil Increases Inflammation Resolution In Zebrafish Via Neutrophil Apoptosis                                                                                                                  |
| immunology                             | 0.993 |     1.860 | 2017-06-30 | Y   | Decreased intra-lymphocyte cytokines measurement in septic shock patients: a proof of concept study in whole blood                                                                                             |
| immunology                             | 0.993 |     1.946 | 2018-11-01 | Y   | TLR3 deficiency exacerbates the loss of epithelial barrier function during genital tract Chlamydia muridarum infection                                                                                         |
| microbiology                           | 1.000 |     0.919 | 2019-07-01 | N   | Effects of a novel thermophilic cellulose-degrading agent on the quality of compost and change in microbial community of garden waste                                                                          |
| microbiology                           | 1.000 |     1.078 | 2018-12-01 | N   | Alternative Promoters Drive Human Cytomegalovirus Reactivation from Latency                                                                                                                                    |
| microbiology                           | 0.999 |     1.239 | 2018-11-01 | Y   | A pilot study of the effects of Mycoplasma ovipneumoniae exposure on domestic lamb growth and performance                                                                                                      |
| microbiology                           | 0.999 |     1.242 | 2019-07-01 | N   | Preserving cultural heritage: Analyzing the antifungal potential of ionic liquids tested in paper restoration                                                                                                  |
| microbiology                           | 0.999 |     1.289 | 2014-06-03 | Y   | Bacillus Calmette-Guerin infection in NADPH oxidase deficiency: defective mycobacterial sequestration and granuloma formation                                                                                  |
| microbiology                           | 0.999 |     1.295 | 2019-07-02 | N   | Treatment of Mixed Azo Dyes in an Aerobic Sequential Batch Reactor and Toxicity Assessment using Vigna radiata                                                                                                 |
| microbiology                           | 0.998 |     1.337 | 2018-05-01 | N   | The Impact of Remediation Through Stabilizing Amendments on Taxonomic and Metabolic Patterns of Bacteria and Archaea in Cadmium-Contaminated Paddy Fields in Southwestern China                                |
| microbiology                           | 0.998 |     1.337 | 2018-10-01 | Y   | Experimental co-transmission of simian immunodeficiency virus (SIV) and the macaque homologs of the Kaposi sarcoma-associated herpesvirus (KSHV) and Epstein-Barr virus (EBV)                                  |
| microbiology                           | 0.998 |     1.352 | 2018-12-03 | N   | Antimicrobial Resistance Surveillance Among Gram Negative Bacterial Isolates from Patients in Khartoum State Hospitals                                                                                         |
| microbiology                           | 0.998 |     1.371 | 2014-02-28 | N   | High Bacterial Load Predicts Poor Outcomes in Patients with Idiopathic Pulmonary Fibrosis                                                                                                                      |
| molecular-biology                      | 0.999 |     1.259 | 2013-11-12 | Y   | Water and the biology of *prions* and plaques                                                                                                                                                                  |
| molecular-biology                      | 0.999 |     1.267 | 2013-11-18 | N   | Differentiation-dependent telomeric long non-coding transcription in a model of skeletal myogenesis                                                                                                            |
| molecular-biology                      | 0.998 |     1.409 | 2018-12-03 | N   | Deciphering the molecular pathways of apoptosis using leaf extract of Basella alba against Ehrlichs Ascites Carcinoma (EAC) cell line in Swiss albino mice model                                               |
| molecular-biology                      | 0.998 |     1.557 | 2018-12-03 | Y   | The chaperone-client network subordinates cell-cycle entry to growth and stress                                                                                                                                |
| molecular-biology                      | 0.997 |     1.596 | 2017-05-04 | N   | Smc6 And Top1 Prevent Aberrant Recombination At The Silent Mating-Type Locus HMR In Budding Yeast                                                                                                              |
| molecular-biology                      | 0.997 |     1.603 | 2014-06-25 | N   | ADP-ribose derived Nuclear ATP is Required for Chromatin Remodeling and Hormonal Gene Regulation                                                                                                               |
| molecular-biology                      | 0.996 |     1.691 | 2015-01-23 | Y   | The positive inside rule is stronger when followed by a transmembrane helix.                                                                                                                                   |
| molecular-biology                      | 0.995 |     1.694 | 2019-07-01 | N   | Serum miR-379 expression is related to the development and progression of hypercholesterolemia in non-alcoholic fatty liver disease                                                                            |
| molecular-biology                      | 0.995 |     1.846 | 2018-10-02 | N   | Functional characterization of Komagataella phaffii centromeres by a color-based plasmid stability assay                                                                                                       |
| molecular-biology                      | 0.994 |     1.934 | 2018-05-02 | N   | Interaction between Retinoschisin and Norrin: Physical or Functional Relationship?                                                                                                                             |
| neuroscience                           | 1.000 |     0.891 | 2018-10-01 | N   | The effect of facial expression on contrast sensitivity: a behavioural investigation and extension of Hedger, Garner, & Adams (2015).                                                                          |
| neuroscience                           | 1.000 |     0.968 | 2019-07-01 | N   | Chronic paroxetine blunts stress response and normalizes adverse behavioral effects seen acutely in nulliparous rats                                                                                           |
| neuroscience                           | 1.000 |     1.000 | 2018-12-03 | Y   | Visual mismatch negativity to disappearing parts of objects and textures                                                                                                                                       |
| neuroscience                           | 1.000 |     1.054 | 2018-10-01 | Y   | Relationship between MEG global dynamic functional network connectivity measures and symptoms in schizophrenia                                                                                                 |
| neuroscience                           | 0.999 |     1.082 | 2017-09-07 | Y   | A comparison between the neural correlates of laser and electric pain stimulation and their modulation by expectation                                                                                          |
| neuroscience                           | 0.999 |     1.102 | 2018-12-03 | Y   | Bilateral spontaneous otoacoustic emissions show coupling between active oscillators in the two ears                                                                                                           |
| neuroscience                           | 0.999 |     1.117 | 2017-11-16 | N   | Hyperacuity Bayesian methods to enhance temporal resolution of two-photon recording of the complex spikes in the cerebellar Purkinje cells                                                                     |
| neuroscience                           | 0.999 |     1.130 | 2019-03-01 | N   | Effective Public Apologies                                                                                                                                                                                     |
| neuroscience                           | 0.999 |     1.262 | 2019-07-02 | N   | Foveal feedback supports peripheral perception of both object color and form                                                                                                                                   |
| neuroscience                           | 0.999 |     1.269 | 2014-04-14 | N   | A novel paradigm for auditory discrimination training with social reinforcement in songbirds                                                                                                                   |
| NULL                                   | 0.955 |     1.000 | 2017-07-10 | N   | shRNA mediated inhibition of Cdc42 gene expression in Calu-6 lung cancer cells                                                                                                                                 |
| NULL                                   | 0.909 |     1.379 | 2018-12-04 | N   | Genome-wide analysis of drug resistant Mycobacterium tuberculosis isolates causing pulmonary and extrapulmonary tuberculosis in Russia                                                                         |
| NULL                                   | 0.864 |     1.602 | 2017-07-10 | N   | Love, not food, could have paved the path for dog domestication: A lesson from free-ranging dogs                                                                                                               |
| NULL                                   | 0.818 |     1.648 | 2019-02-02 | N   | Scavenging dicarbonyls with 5’-O-pentyl-pyridoxamine improves insulin sensitivity and reduces atherosclerosis through modulating inflammatory Ly6Chi monocytosis and macrophage polarization                   |
| NULL                                   | 0.773 |     2.035 | 2015-09-06 | N   | Crucial Roles of the Arp2/3 Complex during Mammalian Corticogenesis                                                                                                                                            |
| NULL                                   | 0.727 |     6.256 | 2018-11-15 | N   | Optogenetic vision restoration with high resolution                                                                                                                                                            |
| NULL                                   | 0.682 |     6.293 | 2019-02-15 | N   | Endogenous and exogenous control of visuospatial selective attention in freely behaving mice                                                                                                                   |
| NULL                                   | 0.636 |     6.372 | 2018-11-15 | N   | The double tubular contractile structure of the type VI secretion system displays striking flexibility and elasticity                                                                                          |
| NULL                                   | 0.591 |     6.500 | 2016-09-08 | N   | Calculation of a distribution free estimate of effect size and confidence intervals using VBA/Excel                                                                                                            |
| NULL                                   | 0.545 |     7.879 | 2019-07-30 | N   | TIS7 and SKMc15 Regulate Adipocyte Differentiation and Intestinal Lipid Absorption                                                                                                                             |
| paleontology                           | 0.987 |     2.169 | 2014-06-04 | Y   | Osmunda pulchella sp. nov. from the Jurassic of Swedenreconciling molecular and fossil evidence in the phylogeny of Osmundaceae                                                                               |
| paleontology                           | 0.974 |     2.747 | 2015-03-10 | N   | Fables and foibles: a critical analysis of the Palaeoflora database and the Coexistence approach for palaeoclimate reconstruction                                                                              |
| paleontology                           | 0.961 |     3.581 | 2017-11-19 | Y   | Root grooves on two adjacent anterior teeth of Australopithecus africanus                                                                                                                                      |
| paleontology                           | 0.948 |     4.512 | 2018-08-09 | N   | Modern botanical analogue of endangered Yak (Bos mutus) dung from India: Plausible linkage with living and extinct megaherbivores                                                                              |
| paleontology                           | 0.935 |     4.539 | 2015-02-01 | Y   | Community stability and selective extinction during Earth’s greatest mass extinction                                                                                                                           |
| paleontology                           | 0.922 |     4.569 | 2017-07-21 | Y   | Three-dimensional mobility and muscle attachments in the pectoral limb of the Triassic cynodont Massetognathus pascuali                                                                                        |
| paleontology                           | 0.909 |     4.641 | 2017-08-15 | N   | The measurement of species selection on evolving characters                                                                                                                                                    |
| paleontology                           | 0.896 |     4.732 | 2018-11-22 | Y   | Ignoring stratigraphic age uncertainty leads to erroneous estimates of species divergence times under the fossilized birth-death process                                                                       |
| paleontology                           | 0.883 |     5.111 | 2019-04-02 | N   | Dental caries in human evolution: frequency of carious lesions in South African fossil hominins                                                                                                                |
| paleontology                           | 0.870 |     5.244 | 2018-01-01 | N   | Stability, incumbency and ecological reorganization after the Permian-Triassic mass extinction                                                                                                                 |
| pathology                              | 0.996 |     1.330 | 2018-12-03 | Y   | Changes choroidal area following trabeculectomy: long-term effect of intraocular pressure reduction                                                                                                            |
| pathology                              | 0.993 |     1.557 | 2018-12-03 | N   | Cerebral amyloid angiopathy and comparative analysis of amyloid-β protein in birds                                                                                                                             |
| pathology                              | 0.989 |     2.099 | 2018-12-10 | N   | REACTION-DIFFUSION MECHANISMS UNDERLYING HIRSCHPRUNG’S DISEASE AND THEIR PRACTICAL IMPICATIONS                                                                                                                 |
| pathology                              | 0.985 |     2.194 | 2019-07-01 | N   | Dynamic healing and remodeling of mandibular ramus in segmental mobility and cortical overlap after intraoral vertical osteotomy                                                                               |
| pathology                              | 0.981 |     2.253 | 2019-05-02 | N   | Glomerulosclerosis and kidney failure in a mouse model of monoclonal immunoglobulin light-chain deposition disease                                                                                             |
| pathology                              | 0.978 |     2.256 | 2019-06-11 | N   | The P387 Thrombospondin-4 Variant Promotes Accumulation of Macrophages in Atherosclerotic Lesions                                                                                                              |
| pathology                              | 0.974 |     2.271 | 2017-09-07 | N   | The function of macromolecular complex of CFTR-NHERF2-LPA2 in inflammatory responses of intestinal epithelial cells                                                                                            |
| pathology                              | 0.970 |     2.316 | 2018-01-12 | N   | A laboratory demand optimisation project in primary care                                                                                                                                                       |
| pathology                              | 0.967 |     2.422 | 2017-07-03 | N   | A mouse model of necrotic biliary pancreatitis induced by combining gallstone formation and ligation of the biliary-pancreatic duct                                                                            |
| pathology                              | 0.963 |     2.465 | 2014-04-21 | N   | Notorious Novel Avian Influenza Viruses H10N8 and H7N9 in China in 2013 Co-originated from H9N2                                                                                                                |
| pharmacology-and-toxicology            | 0.998 |     1.452 | 2019-07-01 | N   | Prevalence of Alcohol use during pregnancy and its association with partner alcohol use in East Africa: systematic review and meta-analysis                                                                    |
| pharmacology-and-toxicology            | 0.996 |     1.609 | 2014-05-24 | N   | Novel Natural Product Discovery from Marine Sponges and their Obligate Symbiotic Organisms                                                                                                                     |
| pharmacology-and-toxicology            | 0.993 |     1.652 | 2018-10-01 | Y   | Proteomic Profile of TGF-β1 treated Lung Fibroblasts identifies Novel Markers of Activated Fibroblasts in the Silica Exposed Rat Lung                                                                          |
| pharmacology-and-toxicology            | 0.991 |     1.742 | 2019-07-01 | N   | A 3D brain unit model to further improve prediction of local drug distribution within the brain                                                                                                                |
| pharmacology-and-toxicology            | 0.989 |     1.826 | 2019-05-01 | N   | Pharmacodynamics of linezolid-plus-fosfomycin against vancomycin-susceptible and -resistant enterococci in vitro and in vivo of a Galleria mellonella larval infection model                                   |
| pharmacology-and-toxicology            | 0.987 |     1.978 | 2018-05-01 | Y   | The Biological Evaluation of Fusidic Acid and Its Hydrogenation Derivative as Antimicrobial and Anti-inflammatory Agents                                                                                       |
| pharmacology-and-toxicology            | 0.985 |     2.095 | 2017-06-09 | N   | Development of an HPTLC method for determination of hypoglycin A in aqueous extracts of seedlings and samaras of Acer species                                                                                  |
| pharmacology-and-toxicology            | 0.983 |     2.141 | 2018-08-01 | Y   | A standardised framework to identify optimal animal models for efficacy assessment in drug development                                                                                                         |
| pharmacology-and-toxicology            | 0.980 |     2.144 | 2017-07-03 | N   | Light-dependent decomposition of FICZ, an endogenous ligand of the aryl hydrocarbon receptor                                                                                                                   |
| pharmacology-and-toxicology            | 0.978 |     2.163 | 2017-06-07 | N   | Effects of Progesterone on the reproductive physiology in zebrafish                                                                                                                                            |
| physiology                             | 0.998 |     0.866 | 2014-05-26 | N   | Hip and knee kinematics display complex and time-varying sagittal kinematics during repetitive stepping: Implications for design of a functional fatigue model of the knee extensors and flexors               |
| physiology                             | 0.997 |     1.250 | 2019-03-01 | Y   | An Oculometrics-based Biofeedback System to Impede Fatigue Development during Computer Work: a Proof-of-Concept Study                                                                                          |
| physiology                             | 0.995 |     1.370 | 2018-06-01 | N   | Phloem structure and development in Illicium parviflorum, a basal angiosperm shrub                                                                                                                             |
| physiology                             | 0.994 |     1.391 | 2018-10-01 | N   | APAP–induced organ toxicity in rats: The prophylactic role of Acrocarpus fraxinifolius                                                                                                                         |
| physiology                             | 0.992 |     1.467 | 2019-05-01 | N   | Purification and characterization of fat body lipase from the Greater Wax Moth Galleria mellonella (Lepidoptera: Pyralidae).                                                                                   |
| physiology                             | 0.991 |     1.493 | 2018-11-26 | N   | Effects of food restriction on body mass, energy metabolism and thermogenesis in a tree shrew (Tupaia belangeri)                                                                                               |
| physiology                             | 0.989 |     1.500 | 2018-10-01 | N   | Decreased integrity of exercise-induced plasma cell free nuclear DNA – negative association with the increased oxidants production by circulating phagocytes                                                   |
| physiology                             | 0.988 |     1.725 | 2018-05-02 | N   | Altered Fecal Microbiota and Urine Metabolome as Signatures of Soman Poisoning                                                                                                                                 |
| physiology                             | 0.986 |     1.770 | 2018-12-04 | Y   | Urinary AQP5 is independently associated with eGFR decline in patients with type 2 diabetes and nephropathy                                                                                                    |
| physiology                             | 0.985 |     1.787 | 2014-12-16 | N   | Estimation of erythrocyte surface area in mammals                                                                                                                                                              |
| plant-biology                          | 0.999 |     1.271 | 2014-08-08 | N   | Growth of Primary and Lateral Roots of Vicia faba L. in the Solution of Calcium Sulfate                                                                                                                        |
| plant-biology                          | 0.999 |     1.516 | 2014-02-28 | N   | Effect of alternating red and blue light irradiation generated by light emitting diodes on the growth of leaf lettuce                                                                                          |
| plant-biology                          | 0.998 |     1.530 | 2014-05-27 | N   | Different profile of transcriptome between wheat Yunong 201 and its high-yield mutant Yunong 3114                                                                                                              |
| plant-biology                          | 0.997 |     1.839 | 2017-07-06 | N   | Genotype, nitrogen and herbivory shape plant defense: the case of a vitamin-enriched maize                                                                                                                     |
| plant-biology                          | 0.997 |     2.200 | 2018-01-01 | N   | Green-gradient based canopy segmentation: A multipurpose image mining technique with potential use in crop phenotyping and canopy studies                                                                      |
| plant-biology                          | 0.996 |     2.205 | 2018-12-03 | N   | Arbuscular cotton-associated mycorrhizal fungi in Yeola region of Maharashtra, India                                                                                                                           |
| plant-biology                          | 0.995 |     2.261 | 2014-06-24 | N   | Mesoscale analyses of fungal networks as an approach for quantifying phenotypic traits                                                                                                                         |
| plant-biology                          | 0.995 |     2.313 | 2014-03-26 | Y   | Rice BiP3 regulates immunity mediated by the PRRs XA3 and XA21 but not immunity mediated by the NB-LRR protein, Pi5                                                                                            |
| plant-biology                          | 0.994 |     2.319 | 2017-06-21 | N   | Varying water deficit stress (WDS) tolerance in grain amaranths involves multifactorial shifts in WDS-related responses                                                                                        |
| plant-biology                          | 0.994 |     2.363 | 2017-06-02 | Y   | Alkyl Gallates Display Elicitor Activities In Tobacco Plants                                                                                                                                                   |
| scientific-communication-and-education | 0.997 |     1.232 | 2014-07-11 | N   | A plea for evidence in ecosystem service science: a framework and its application                                                                                                                              |
| scientific-communication-and-education | 0.995 |     1.352 | 2018-11-22 | N   | Construction of an index system for big data application in health care enterprises: A study based on the Delphi method                                                                                        |
| scientific-communication-and-education | 0.992 |     1.511 | 2018-12-03 | Y   | Trends and projections of universal health coverage indicators in Ghana, 1995-2030: A national and subnational study                                                                                           |
| scientific-communication-and-education | 0.989 |     1.761 | 2018-10-01 | Y   | Adherence to the iDSI reference case among published cost-per-DALY averted studies                                                                                                                             |
| scientific-communication-and-education | 0.987 |     1.771 | 2018-11-23 | Y   | Evolution of international collaborative research efforts to develop non-Cochrane systematic reviews                                                                                                           |
| scientific-communication-and-education | 0.984 |     1.823 | 2019-07-01 | N   | DENTAL CARIES AND ORAL HEALTH BEHAVIOR ASSESSMENT AMONG PORTUGUESE ADOLESCENTS                                                                                                                                 |
| scientific-communication-and-education | 0.981 |     1.955 | 2018-11-26 | N   | Development of the Brief Personal Values Inventory for sense of values in the Japanese general population                                                                                                      |
| scientific-communication-and-education | 0.979 |     1.965 | 2014-10-08 | Y   | An evidence assessment tool for ecosystem services and conservation studies                                                                                                                                    |
| scientific-communication-and-education | 0.976 |     2.306 | 2019-07-01 | N   | Predicting long-term Type 2 Diabetes with Support Vector Machine using Oral Glucose Tolerance Test                                                                                                             |
| scientific-communication-and-education | 0.974 |     2.315 | 2018-06-01 | Y   | Policy driven changes in animal research practices: mapping researchers’ attitudes towards animal-free innovations using the Netherlands as an example                                                         |
| synthetic-biology                      | 0.998 |     0.965 | 2013-11-07 | N   | Designing Robustness to Temperature in a Feedforward Loop Circuit                                                                                                                                              |
| synthetic-biology                      | 0.996 |     1.066 | 2014-03-02 | N   | Complementation of a temperature sensitive Escherichia coli rpoD mutation using Lactobacillus sigma factors                                                                                                    |
| synthetic-biology                      | 0.994 |     1.171 | 2014-03-17 | Y   | Hydrogen peroxide thermochemical oscillator as driver for primordial RNA replication                                                                                                                           |
| synthetic-biology                      | 0.993 |     2.224 | 2014-10-08 | N   | Recurrence-Based Information Processing in Gene Regulatory Networks                                                                                                                                            |
| synthetic-biology                      | 0.991 |     2.258 | 2013-12-02 | Y   | Efficient Search, Mapping, and Optimization of Multi-protein Genetic Systems in Diverse Bacteria                                                                                                               |
| synthetic-biology                      | 0.989 |     2.355 | 2014-03-17 | Y   | Rapidly characterizing the fast dynamics of RNA genetic circuitry with cell-free transcription-translation (TX-TL) systems                                                                                     |
| synthetic-biology                      | 0.987 |     3.051 | 2013-11-15 | Y   | Design and implementation of a synthetic biomolecular concentration tracker                                                                                                                                    |
| synthetic-biology                      | 0.985 |     3.267 | 2017-08-07 | N   | Gossypol biosynthesis in cotton revealed through organ culture, plant grafting and gene expression profiling                                                                                                   |
| synthetic-biology                      | 0.983 |     3.543 | 2017-02-09 | N   | Investigating circadian rhythmicity in pain sensitivity using a neural circuit model for spinal cord processing of pain                                                                                        |
| synthetic-biology                      | 0.982 |     3.570 | 2017-05-14 | N   | Expression Of Pokeweed Antiviral Protein Isoform S1 (PAP-S1) And Of Ricin-A-Chain/PAP-S1 Novel Fusion Protein (RTA/PAP-S1) In Escherichia coli And Their Comparative Inhibition Of Protein Synthesis In Vitro  |
| systems-biology                        | 0.999 |     0.813 | 2013-11-18 | Y   | A structural classification of candidate oscillators and multistationary systems                                                                                                                               |
| systems-biology                        | 0.999 |     0.835 | 2014-09-01 | N   | Modelling reactions catalysed by carbohydrate-active enzymes                                                                                                                                                   |
| systems-biology                        | 0.998 |     1.038 | 2013-11-14 | Y   | A model of flux regulation in the cholesterol biosynthesis pathway: Immune mediated graduated flux reduction versus statin-like led stepped flux reduction                                                     |
| systems-biology                        | 0.998 |     1.357 | 2014-05-09 | N   | Mycoplasma stress response: adaptive regulation or broken brakes?                                                                                                                                              |
| systems-biology                        | 0.997 |     1.681 | 2017-06-02 | N   | Deciphering Molecular Cascades In A Novel Acclimatization Strategy For Rapid Ascent To High Altitude                                                                                                           |
| systems-biology                        | 0.996 |     1.804 | 2018-10-01 | N   | High content analysis methods enable high throughput nematode discovery screening for viability and movement behavior in a multiplex sample in response to natural product treatment.                          |
| systems-biology                        | 0.996 |     2.100 | 2015-10-03 | Y   | Mechanisms of blood homeostasis: lineage tracking and a neutral model of cell populations in rhesus macaque                                                                                                    |
| systems-biology                        | 0.995 |     2.185 | 2019-03-01 | Y   | Operating regimes in a single enzymatic cascade at ensemble-level                                                                                                                                              |
| systems-biology                        | 0.994 |     2.197 | 2014-07-22 | N   | Negative Feedback Facilitates Temperature Robustness in Biomolecular Circuit Dynamics                                                                                                                          |
| systems-biology                        | 0.994 |     2.241 | 2015-10-14 | Y   | An Effective Model of HL-60 Differentiation                                                                                                                                                                    |
| zoology                                | 0.996 |     1.565 | 2018-08-01 | Y   | Moving in complex environments: a biomechanical analysis of locomotion on inclined and narrow substrates                                                                                                       |
| zoology                                | 0.993 |     1.588 | 2014-06-25 | N   | The mandibular gland in Nasonia vitripennis (Hymenoptera: Pteromalidae)                                                                                                                                        |
| zoology                                | 0.989 |     1.738 | 2014-06-13 | N   | Mitochondrial DNA variation and structure among North American populations of Megaselia scalaris                                                                                                               |
| zoology                                | 0.985 |     1.837 | 2014-11-13 | Y   | Recognition and identification of species in the Bombus lucorum-complex - A review and outlook                                                                                                                 |
| zoology                                | 0.982 |     2.228 | 2014-08-14 | Y   | The impact of radio-tags on Ruby-throated Hummingbirds (Archilochus colubris)                                                                                                                                  |
| zoology                                | 0.978 |     2.258 | 2018-06-04 | Y   | Visual modelling supports the potential for prey detection by means of diurnal active photolocation in a small cryptobenthic fish                                                                              |
| zoology                                | 0.974 |     2.312 | 2015-10-16 | Y   | Changes in the milk metabolome of the giant panda (Ailuropoda melanoleuca) with time after birth: Three phases in early lactation and progressive individual differences                                       |
| zoology                                | 0.971 |     2.500 | 2013-11-13 | N   | Filling up the tree: considering the self-organization of avian roosting behavior                                                                                                                              |
| zoology                                | 0.967 |     2.636 | 2015-10-16 | N   | Blood circulation in the tunicate Corella inflata (Corellidae).                                                                                                                                                |
| zoology                                | 0.964 |     2.688 | 2018-08-13 | N   | Co-invasion of the ladybird Harmonia axyridis and its parasites Hesperomyces virescens fungus and Parasitylenchus bifurcatus nematode to the Caucasus                                                          |
