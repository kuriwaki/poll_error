library(readr)
library(dplyr)
library(haven)
library(readr)

# slim full survey
cc_raw <- readRDS("data/input/cces2016_slim.Rds")

table(as_factor(cc_raw$pid3), cc_raw$pid3)


# tabulate means and estimates
tab_cc <- cc_raw %>% 
  group_by(state) %>% 
  summarize(cces_n_raw = n(),
            cces_n_voters = sum(turnout_wgt, na.rm = TRUE),
            cces_n_vv = sum(vv_turnout, na.rm = TRUE),
            cces_n_wvv = sum(vv_turnout*commonweight_vv, na.rm = TRUE),
            cces_n_postvoters = sum(post_turnout, na.rm = TRUE),
            
            cces_tothrc_raw = sum(vote_hrc_pre, na.rm = TRUE),
            cces_totdjt_raw = sum(vote_djt_pre, na.rm = TRUE),
            cces_totund_raw = sum(vote_und_pre, na.rm = TRUE),
            cces_demund_raw = sum(vote_und_pre*(pid3 == 1), na.rm = TRUE),
            cces_repund_raw = sum(vote_und_pre*(pid3 == 2), na.rm = TRUE),
            
            cces_tothrc_raw_post = sum(vote_hrc_post, na.rm = TRUE),
            cces_totdjt_raw_post = sum(vote_djt_post, na.rm = TRUE),
            
            cces_tothrc_adj_trn = sum(vote_hrc_pre*turnout_wgt, na.rm = TRUE),
            cces_totdjt_adj_trn = sum(vote_djt_pre*turnout_wgt, na.rm = TRUE),
            cces_totund_adj_trn = sum(vote_und_pre*turnout_wgt, na.rm = TRUE),
            cces_demund_adj_trn = sum(vote_und_pre*turnout_wgt*(pid3 == 1), na.rm = TRUE),
            cces_repund_adj_trn = sum(vote_und_pre*turnout_wgt*(pid3 == 2), na.rm = TRUE),
            
            cces_tothrc_vv = sum(vote_hrc_pre*vv_turnout, na.rm = TRUE),
            cces_totdjt_vv = sum(vote_djt_pre*vv_turnout, na.rm = TRUE),
            cces_totund_vv = sum(vote_und_pre*vv_turnout, na.rm = TRUE),
            cces_demund_vv = sum(vote_und_pre*vv_turnout*(pid3 == 1), na.rm = TRUE),
            cces_repund_vv = sum(vote_und_pre*vv_turnout*(pid3 == 2), na.rm = TRUE),
            
            cces_tothrc_wvv = sum(vote_hrc_pre*vv_turnout*commonweight_vv, na.rm = TRUE),
            cces_totdjt_wvv = sum(vote_djt_pre*vv_turnout*commonweight_vv, na.rm = TRUE),
            cces_totund_wvv = sum(vote_und_pre*vv_turnout*commonweight_vv, na.rm = TRUE),
            cces_demund_wvv = sum(vote_und_pre*vv_turnout*(pid3 == 1)*commonweight_vv, na.rm = TRUE),
            cces_repund_wvv = sum(vote_und_pre*vv_turnout*(pid3 == 2)*commonweight_vv, na.rm = TRUE),
            
            cces_varhat_hrc_voters = sum(turnout_wgt^2*((vote_hrc_pre - mean(vote_hrc_pre, na.rm = TRUE))^2), na.rm = TRUE) / (sum(turnout_wgt, na.rm = TRUE)^2),
            cces_varhat_djt_voters = sum(turnout_wgt^2*((vote_djt_pre - mean(vote_djt_pre, na.rm = TRUE))^2), na.rm = TRUE) / (sum(turnout_wgt, na.rm = TRUE)^2),
            
            sd_turnout_wgt = sqrt(sum((turnout_wgt - mean(turnout_wgt))^2)/n()),
            cv_turnout_wgt = sd_turnout_wgt / mean(turnout_wgt),
            
            sd_common_wgt = sqrt(sum((commonweight_vv - mean(commonweight_vv))^2)/n()),
            cv_common_wgt = sd_common_wgt / mean(commonweight_vv)) %>%
  mutate(cces_pct_hrc_raw = cces_tothrc_raw / cces_n_raw,
         cces_pct_hrcund_raw = (cces_tothrc_raw + cces_totund_raw) / cces_n_raw,
         cces_pct_hrcdund_raw = (cces_tothrc_raw + cces_demund_raw) / cces_n_raw,
         
         cces_pct_hrc_vep = cces_tothrc_adj_trn / cces_n_raw,
         cces_pct_hrc_voters = cces_tothrc_adj_trn / cces_n_voters,
         cces_pct_hrcund_voters = (cces_tothrc_adj_trn + cces_totund_adj_trn) / cces_n_voters,
         cces_pct_hrcdund_voters = (cces_tothrc_adj_trn + cces_demund_adj_trn) / cces_n_voters,
         
         cces_pct_hrc_vv = cces_tothrc_vv / cces_n_vv,
         cces_pct_hrcund_vv = (cces_tothrc_vv + cces_totund_vv) / cces_n_vv,
         cces_pct_hrcdund_vv = (cces_tothrc_vv + cces_demund_vv) / cces_n_vv,
  
         cces_pct_hrc_wvv = cces_tothrc_wvv / cces_n_wvv,
         cces_pct_hrcund_wvv = (cces_tothrc_wvv + cces_totund_wvv) / cces_n_wvv,
         cces_pct_hrcdund_wvv = (cces_tothrc_wvv + cces_demund_wvv) / cces_n_wvv,
         
         cces_pct_hrc_postvoters = cces_tothrc_raw_post / cces_n_voters) %>%
  mutate(cces_pct_djt_raw = cces_totdjt_raw / cces_n_raw,
         cces_pct_djtund_raw = (cces_totdjt_raw + cces_totund_raw) / cces_n_raw,
         cces_pct_djtrund_raw = (cces_totdjt_raw + cces_repund_raw) / cces_n_raw,
         
         cces_pct_djt_vep = cces_totdjt_adj_trn / cces_n_raw,
         cces_pct_djt_voters = cces_totdjt_adj_trn / cces_n_voters,
         cces_pct_djtund_voters = (cces_totdjt_adj_trn + cces_totund_adj_trn) / cces_n_voters,
         cces_pct_djtrund_voters = (cces_totdjt_adj_trn + cces_repund_adj_trn) / cces_n_voters,
         
         cces_pct_djt_vv = cces_totdjt_vv / cces_n_vv,
         cces_pct_djtund_vv = (cces_totdjt_vv + cces_totund_vv) / cces_n_vv,
         cces_pct_djtrund_vv = (cces_totdjt_vv + cces_repund_vv) / cces_n_vv,
         
         cces_pct_djt_wvv = cces_totdjt_wvv / cces_n_wvv,
         cces_pct_djtund_wvv = (cces_totdjt_wvv + cces_totund_wvv) / cces_n_wvv,
         cces_pct_djtrund_wvv = (cces_totdjt_wvv + cces_repund_wvv) / cces_n_wvv,
         
         cces_pct_djt_postvoters = cces_totdjt_raw_post / cces_n_voters)
tab_cc

# get validated voter variance separately by filtering validated voters
# get approximate validated vote variance for ZnN, by using the true p

dw <- readRDS("data/output/dw_results_state.rds") %>% 
  select(state, pct_hrc_voters, pct_djt_voters)

vv_var <- cc_raw %>% 
  filter(vv_turnout == 1) %>%
  left_join(dw, by = "state") %>%
  group_by(state) %>%
  summarize(
    cces_varhat_hrc_wvv = sum(commonweight_vv^2*((vote_hrc_pre - mean(vote_hrc_pre))^2), na.rm = TRUE) / (sum(commonweight_vv, na.rm = TRUE)^2),
    cces_varhatN_hrc_wvv = sum(commonweight_vv^2*((vote_hrc_pre - mean(pct_hrc_voters))^2), na.rm = TRUE) / (sum(commonweight_vv, na.rm = TRUE)^2),
    cces_varhat_djt_wvv = sum(commonweight_vv^2*((vote_djt_pre - mean(vote_djt_pre))^2), na.rm = TRUE) / (sum(commonweight_vv, na.rm = TRUE)^2),
    cces_varhatN_djt_wvv = sum(commonweight_vv^2*((vote_djt_pre - mean(pct_djt_voters))^2), na.rm = TRUE) / (sum(commonweight_vv, na.rm = TRUE)^2)
  )
  





tab_cc <- left_join(tab_cc, vv_var, by = "state")

saveRDS(tab_cc, "data/output/cc_tabulation_state.rds")
  