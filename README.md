Poll Predictions and Errors
================

This data combines three state-level datasets on the 2016 Presidential Election.

Output
======

The final dataset (`pres16_state.csv`) is a spreadsheet of the 50 states and DC.

``` r
read_csv("data/output/pres16_state.csv")
```

    ## # A tibble: 51 x 14
    ##                   state    st      vap      vep votes_hrc tot_votes
    ##                   <chr> <chr>    <int>    <int>     <int>     <int>
    ##  1              Alabama    AL  3770142  3601361    729547   2123372
    ##  2               Alaska    AK   555367   519849    116454    318608
    ##  3              Arizona    AZ  5331034  4734313   1161167   2573165
    ##  4             Arkansas    AR  2286625  2142571    380494   1130635
    ##  5           California    CA 30201571 25017408   8753788  14181595
    ##  6             Colorado    CO  4305728  3966297   1338870   2780220
    ##  7          Connecticut    CT  2821935  2561555    897572   1644920
    ##  8             Delaware    DE   749872   689125    235603    441590
    ##  9 District of Columbia    DC   562329   511463    282830    311268
    ## 10              Florida    FL 16565588 14572210   4504975   9420039
    ## # ... with 41 more rows, and 8 more variables: pct_hrc_vep <dbl>,
    ## #   pct_hrc_voters <dbl>, cces_pct_hrc_vep <dbl>,
    ## #   cces_pct_hrc_voters <dbl>, cces_n_raw <int>, cces_n_voters <dbl>,
    ## #   yougov_pct_hrc <dbl>, yougov_n <dbl>

The main columns are

Identifiers

-   `state`: Name of state (full name)
-   `st`: Name of state (abbreviation)

Outcomes (including estimates of VAP/VEP)

-   `vap`: Estimated Voting Age Population (see below for definition)
-   `vep`: Estimated Voting Eligible Population (see below for definiton)
-   `votes_hrc`: Votes for Clinton
-   `tot_votes`: Ballots cast for the Office of President.
-   `pct_hrc_voters`: Election Oucome. Hillary Clinton's Vote as a Percentage of Ballots Cast for President. Computed by `votes_hrc / tot_votes`
-   `pct_hrc_vep`: Hillary Clinton's Vote as a Percentage of Ballots (estimated) eligible population. Computed by `votes_hrc / vep`

Poll estiamtes. Construction detailed below and in `03_tabulate_polls.R` \* `cces_pct_hrc_vep`: CCES estimated percent of Clinton votes among voting eligible population (loosely defined) \* `cces_pct_hrc_voters`: CCES estimated percent of Clinton votes among voters (i.e. those who turn out) \* `cces_n_voters`: CCES sample size adjusted for estimated turnout propensity \* `cces_n_raw`: CCES raw number of respondents, or unadjusted proxy estimate of eligible population. \* `yougov_pct_hrc`: YouGov estimated of Clinton votes among voters \* `yougov_n`: YouGov poll sample size

Data Sources
============

The data comes from three sources and is built in `01_read_data.R`

Observed Values
---------------

First, the sample space. The U.S. does not have an official census of citizens or voting *eligible* citizens. Numbers on voter registrants are also out-of-date in some states. Thus the denominator of interest is fairly tricky to compute.

Here we rely on Michael McDonald's estimates at <http://www.electproject.org/>

**Voting Age Population (VAP)** is [defined](http://www.electproject.org/home/voter-turnout/faq/denominator) as folllows:

> The voting-age population, known by the acronym VAP, is defined by the Bureau of the Census as everyone residing in the United States, age 18 and older. Before 1971, the voting-age population was age 21 and older for most states.

**Voting Eligible Population (VEP)** is [defined](http://www.electproject.org/home/voter-turnout/faq/denominator) as follows:

> The voting-eligible population or VEP is a phrase I coined to describe the population that is eligible to vote. Counted among the voting-age population are persons who are ineligible to vote, such as non-citizens, felons (depending on state law), and mentally incapacitated persons. Not counted are persons in the military or civilians living overseas.

I pulled the VAP and VEP numbers from his spreadsheet [here](https://docs.google.com/spreadsheets/d/1VAcF0eJ06y_8T4o2gvIL4YcyQy8pxb1zYkgXF76Uu1s/edit#gid=2030096602)

Next, the observed election outcome. Vote counts are reported from official election reports and measured exactly. Any final count will do; I used the Cook Political Report's spreadsheet [here](https://docs.google.com/spreadsheets/d/133Eb4qQmOxNvtesw2hdVns073R68EZx4SfCnP4IGQf8)

The column `votes_hrc` refers to the column `Clinton (D)` in the above-linked spreadsheet. `tot_votes` refers to the sum of the three columns `Clinton (D)`, `Trump (R)`, and `Others`.

Poll Prediction
===============

**CCES**

The Cooperative Congressional Election Study (CCES) is one of the largest pre-election studies conducted in the 2016 election. The CCES is conducted online for the several weeks before the election.

The target population is registered voters. Sampling is continuouslly adjusted to obtain a representative sample. Multi-level models and other weighting schemes contribute to final state-level estimates.

I estimated state-level predictions without adjusting for sampling or any other covariate adjustment weights.

I did estimate voters and adjust for estimated turnout in variables `pct_hrc_voters` and `cces_n_voters`. These give a rough proxy for turnout by the coarse weights from the vote intent question. The weights are as specified in the CCES press release:

> \[A weight of\] 1 for those who have already voted, 0.9 for those "definitely voting", 0.3 for those "probably voting", and 0.1 for those not sure).

I coded other responses (No, Skipped, Missing).. to 0.

The code is in `03_tabulate_polls.R`,

``` r
tab_cc <- cc_raw %>% 
  group_by(state) %>% 
  summarize(cces_n_raw = n(),
            cces_n_voters = sum(turnout_wgt, na.rm = TRUE),
            cces_tothrc_raw = sum(vote_hrc, na.rm = TRUE),
            cces_tothrc_adj_trn = sum(vote_hrc*turnout_wgt, na.rm = TRUE)) %>%
  mutate(cces_pct_hrc_vep = cces_tothrc_raw / cces_n_raw,
         cces_pct_hrc_voters = cces_tothrc_adj_trn / cces_n_voters)
```

**YouGov Release**

YouGov runs the CCES survey and generates their estimates with their algorithm. I took estimates from their November 16, 2016 press release [here](https://cces.gov.harvard.edu/news/cces-pre-election-survey-2016). A Google Sheets version of the same table in the release is [here](https://docs.google.com/spreadsheets/d/1pJLEHfvCN-eX1mBfe6sgs0dwF2oq9G1FcUhKFk0Pe8g).

Read the press release and guides (e.g. for 2014: [dataverse](https://dataverse.harvard.edu/file.xhtml?fileId=2794577&version=RELEASED&version=.0)) for more details on implementation.

Comparisons
===========

Setup plot...

``` r
library(ggplot2)
library(ggrepel)
library(scales)
library(readr)

df <- read_csv("data/output/pres16_state.csv")

gg0 <- ggplot(df, aes(x = cces_pct_hrc_voters, y = pct_hrc_voters, size = cces_n_voters)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1), label = percent) +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  guides(size = FALSE) +
  coord_equal() +
  theme_bw() +
  labs(x = "CCES Pre-election Survey Clinton Support",
       y = "Final Clinton Popular Vote Share",
       caption = "Sized proportional to the survey's estimated votes. Surevy estimates are unofficial, and calculated as described in the vignette.")
```

As points

``` r
gg0 + geom_point()
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
gg0 + geom_point(size = 1) +  geom_label_repel(aes(label = st), segment.alpha = 0.5)
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

References
==========

-   McDonald, Michael P. 2017. "2016 November General Election" *United States Elections Project.* Accessed July 23, 2017.
-   CCES. 2016. Press Release. <https://cces.gov.harvard.edu/news/cces-pre-election-survey-2016>
-   Cook Political Report. 2017. <http://cookpolitical.com/story/10174>
