library(dplyr)
library(readr)
library(tibble)
library(data.table)

mm <- readRDS("data/output/mm_popultn_state.rds")
dw <- readRDS("data/output/dw_results_state.rds")
yg <- readRDS("data/output/yg_release_state.rds")
cc <- readRDS("data/output/cc_tabulation_state.rds")


# Join ---
df_joined <-  inner_join(mm, dw, by = "state") %>%
  inner_join(cc, by = "state") %>% 
  inner_join(yg, by = "state")


# select ----
df <- df_joined %>% 
  mutate(pct_hrc_vep = votes_hrc / vep,
         pct_djt_vep = votes_djt / vep) %>%
  select(state, st, color, vap, vep, 
         votes_hrc, votes_djt, tot_votes, 
         pct_hrc_vep, pct_hrc_voters,
         pct_djt_vep, pct_djt_voters,
         matches("cces_n_"),
         matches("cces_tothrc"),
         matches("cces_pct_hrc"),
         matches("cces_totdjt"),
         matches("cces_pct_djt"),
         matches("cces_varhat"),
         cv_turnout_wgt,
         yougov_pct_hrc, yougov_pct_djt, yougov_n)




saveRDS(df, "data/output/df_joined.rds")


