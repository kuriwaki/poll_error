library(dplyr)
library(readr)

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
         votes_hrc, tot_votes, 
         pct_hrc_vep, pct_hrc_voters,
         pct_djt_vep, pct_djt_voters,
         cces_n_voters, cces_n_raw,
         cces_tothrc_adj_trn, cces_tothrc_raw, 
         cces_pct_hrc_voters, cces_pct_hrc_vep, cces_pct_hrc_raw,
         cces_pct_djt_voters, cces_pct_djt_vep, cces_pct_djt_raw,
         cv_turnout_wgt,
         yougov_pct_hrc, yougov_pct_djt, yougov_n,
         `State Results Website`)


# categorize swing state, red, blue
# https://docs.google.com/spreadsheets/d/133Eb4qQmOxNvtesw2hdVns073R68EZx4SfCnP4IGQf8/edit#gid=19




# estimate rho ----
rho_estimate <- function(data = df, N, mu, muhat, n, cv = NULL) {
  
  
  N <- data[[N]]
  n <- data[[n]]
  mu <- data[[mu]]
  muhat <- data[[muhat]]
  if (!is.null(cv)) cv <- data[[cv]]
  
  ## parts
  one_over_sqrtN <- 1 / sqrt(N)
  diff_mu <- muhat - mu
  f <- n / N
  one_minus_f <- 1 - f
  s2hat <- mu * (1 - mu)
  if (!is.null(cv)) {
    A <- sqrt(1 + (cv^2 / one_minus_f))
    one_over_A <- 1 /A
  }

  
  ## estimate of rho
  if (!is.null(cv))
    return(one_over_A* one_over_sqrtN * diff_mu / sqrt((one_minus_f / n) * s2hat))
  
  if (is.null(cv))
     return(one_over_sqrtN * diff_mu / sqrt((one_minus_f / n) * s2hat))
}



df$rho_voter <- rho_estimate(N = "tot_votes",
                             mu = "pct_hrc_voters",
                             muhat = "cces_pct_hrc_voters",
                             n = "cces_n_voters",
                             cv = "cv_turnout_wgt")

df$rho_vep <- rho_estimate(N = "vep",
                             mu = "pct_hrc_voters",
                             muhat = "cces_pct_hrc_raw",
                             n = "cces_n_raw")


# Save ----
write_csv(df, "data/output/pres16_state.csv")

