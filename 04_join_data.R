

mm <- readRDS("data/output/mm_popultn_state.rds")
dw <- readRDS("data/output/dw_results_state.rds")
yg <- readRDS("data/output/yg_release_state.rds")
cc <- readRDS("data/output/cc_tabulation_state.rds")


# Join ---
df_joined <-  inner_join(mm, dw, by = "state") %>%
  inner_join(cc, by = "state") %>% 
  inner_join(yg, by = "state")
  
  
# select
df <- df_joined %>% 
  mutate(pct_hrc_vep = votes_hrc / vep) %>%
  select(state, st, vap, vep, 
         votes_hrc, tot_votes, 
         pct_hrc_vep, pct_hrc_voters,
         cces_pct_hrc_vep, cces_pct_hrc_voters,
         cces_n_raw, cces_n_voters,
         yougov_pct_hrc, yougov_n)





# Save
write_csv(df, "data/output/pres16_state.csv")


