library(readr)
suppressPackageStartupMessages(library("dplyr"))



# slim full survey
cc_raw <- readRDS("data/input/cces2016_slim.Rds")



# tabulate means and estimates
tab_cc <- cc_raw %>% 
  group_by(state) %>% 
  summarize(cces_n_raw = n(),
            cces_n_voters = sum(turnout_wgt, na.rm = TRUE),
            cces_n_vv = sum(vv_turnout, na.rm = TRUE),
            cces_tothrc_raw = sum(vote_hrc_pre, na.rm = TRUE),
            cces_totdjt_raw = sum(vote_djt_pre, na.rm = TRUE),
            cces_tothrc_raw_post = sum(vote_hrc_post, na.rm = TRUE),
            cces_totdjt_raw_post = sum(vote_djt_post, na.rm = TRUE),
            cces_tothrc_adj_trn = sum(vote_hrc_pre*turnout_wgt, na.rm = TRUE),
            cces_totdjt_adj_trn = sum(vote_djt_pre*turnout_wgt, na.rm = TRUE),
            cces_tothrc_vv = sum(vote_hrc_pre*vv_turnout, na.rm = TRUE),
            cces_totdjt_vv = sum(vote_djt_pre*vv_turnout, na.rm = TRUE),
            sd_turnout_wgt = sqrt(sum((turnout_wgt - mean(turnout_wgt))^2)/n()),
            cv_turnout_wgt = sd_turnout_wgt / mean(turnout_wgt)) %>%
  mutate(cces_pct_hrc_raw = cces_tothrc_raw / cces_n_raw,
         cces_pct_hrc_vep = cces_tothrc_adj_trn / cces_n_raw,
         cces_pct_hrc_voters = cces_tothrc_adj_trn / cces_n_voters,
         cces_pct_hrc_voters_post = cces_tothrc_adj_trn / cces_n_voters,
         cces_pct_hrc_vv = cces_tothrc_vv / cces_n_vv,
         cces_pct_djt_raw = cces_totdjt_raw / cces_n_raw,
         cces_pct_djt_vep = cces_totdjt_adj_trn / cces_n_raw,
         cces_pct_djt_voters = cces_totdjt_adj_trn / cces_n_voters,
         cces_pct_djt_voters_post = cces_totdjt_adj_trn / cces_n_voters,
         cces_pct_djt_vv = cces_totdjt_vv / cces_n_vv)
tab_cc

saveRDS(tab_cc, "data/output/cc_tabulation_state.rds")
  



# post survey results
cc_raw %>% 
  filter(combined_pres_pre == 5, tookpost == 1, voted_pres_16 != 99) %>% 
  group_by(combined_pres_pre) %>%
  summarize(djt_among_undecided = mean(vote_djt_post, na.rm = TRUE),
            hrc_among_undecided = mean(vote_hrc_post, na.rm = TRUE),
            n = n())

cc_raw %>% 
  filter(tookpost == 1) %>% 
  group_by(combined_pres_pre) %>%
  summarize(n = n())
