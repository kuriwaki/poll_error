library(readr)
suppressPackageStartupMessages(library("dplyr"))



# slim full survey
cc_raw <- readRDS("data/input/cces2016_slim.Rds")



# tabulate means and estimates
tab_cc <- cc_raw %>% 
  group_by(state) %>% 
  summarize(cces_n_raw = n(),
            cces_n_voters = sum(turnout_wgt, na.rm = TRUE),
            cces_tothrc_raw = sum(vote_hrc, na.rm = TRUE),
            cces_tothrc_adj_trn = sum(vote_hrc*turnout_wgt, na.rm = TRUE),
            sd_turnout_wgt = sqrt(sum((turnout_wgt - mean(turnout_wgt))^2)/n()),
            cv_turnout_wgt = sd_turnout_wgt / mean(turnout_wgt)) %>%
  mutate(cces_pct_hrc_raw = cces_tothrc_raw / cces_n_raw,
         cces_pct_hrc_vep = cces_tothrc_adj_trn / cces_n_raw,
         cces_pct_hrc_voters = cces_tothrc_adj_trn / cces_n_voters)


tab_cc

saveRDS(tab_cc, "data/output/cc_tabulation_state.rds")

plot(cces_n)