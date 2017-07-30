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
         votes_hrc, votes_djt, tot_votes, 
         pct_hrc_vep, pct_hrc_voters,
         pct_djt_vep, pct_djt_voters,
         cces_n_voters, cces_n_raw,
         cces_tothrc_adj_trn, cces_tothrc_raw, 
         cces_pct_hrc_voters, cces_pct_hrc_vep, cces_pct_hrc_raw,
         cces_totdjt_adj_trn, cces_totdjt_raw, 
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

df <- df %>% 
  mutate(rho_hrc_vot = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrc_voters",
                                    n = "cces_n_voters",
                                    cv = "cv_turnout_wgt"),
         rho_hrc_vep = rho_estimate(N = "vep",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrc_raw",
                                    n = "cces_n_raw"),
         rho_djt_vot = rho_estimate(N = "tot_votes",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djt_voters",
                                    n = "cces_n_voters",
                                    cv = "cv_turnout_wgt"),
         rho_djt_vep = rho_estimate(N = "vep",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djt_raw",
                                    n = "cces_n_raw")
  )


# Estimate n_eff ----
eff_estimate <- function(data = df, rho, n, N) {
  N <- data[[N]]
  n <- data[[n]]
  rho <- data[[rho]]
  
  f <- n/N
  one_minus_f <- 1 - f
  
  neff_raw <- rho^{-2} * (f / one_minus_f)
  
  ifelse(neff_raw > n, 0 , neff_raw) # if too large, assign 0
}

df <- df %>% 
  mutate(neff_hrc_vot = eff_estimate(rho = "rho_hrc_vot",
                                     N = "tot_votes",
                                     n = "cces_n_voters"),
         neff_hrc_vep = eff_estimate(rho = "rho_hrc_vep", 
                                     N = "vep",
                                     n = "cces_n_raw"),
         neff_djt_vot = eff_estimate(rho = "rho_djt_vot",
                                     N = "tot_votes",
                                     n = "cces_n_voters"),
         neff_djt_vep = eff_estimate(rho = "rho_djt_vep", 
                                     N = "vep",
                                     n = "cces_n_raw")
  )

# estimate drop ---
loss_estimate <- function(data = df, n, neff) {
  n <- data[[n]]
  neff <- data[[neff]]
  
  neff_over_n <- neff/n
  
  1 - neff_over_n
}


df <- df %>% 
  mutate(loss_hrc_vot = loss_estimate(neff = "neff_hrc_vot",
                                     n = "cces_n_voters"),
         loss_hrc_vep = loss_estimate(neff = "neff_hrc_vep",
                                     n = "cces_n_raw"),
         loss_djt_vot = loss_estimate(neff = "neff_djt_vot",
                                     n = "cces_n_voters"),
         loss_djt_vep = loss_estimate(neff = "neff_djt_vep",
                                     n = "cces_n_raw")
  )



# Save ----
write_csv(df, "data/output/pres16_state.csv")

# totals
colSums(select(df, vap, vep, matches("votes"), matches("cces_tot"),  matches("cces_n_"))) %>% 
  t() %>% as.data.frame() %>%
  write_csv("data/output/pres16_US.csv")


# summary stats ------
my_qtl <- function(vec, vecname) {
  this_mean <- mean(vec)
  this_length <- length(vec)
  this_sd <- sd(vec)
  
  quantile(vec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) %>%
    t() %>%
    as.data.frame() %>% 
    add_column("length" = this_length, .after = 5) %>%
    add_column("std_deviation" = this_sd, .after = 5) %>%
    add_column("mean" = this_mean, .after = 2) %>%
    as.data.frame(row.names = vecname) %>% 
    t()
}

rhos <- 
  cbind(my_qtl(df$rho_hrc_vep, "rho_hrc_vep"),
        my_qtl(df$rho_hrc_vep, "rho_hrc_vep"),
        my_qtl(df$rho_djt_vot[df$rho_djt_vot < 0], "rho_djt_avp"),
        my_qtl(df$rho_djt_vep[df$rho_djt_vep < 0], "rho_djt_vep"))  

rhos <- cbind("statistic" = rownames(rhos), rhos) %>%
  as.data.frame()

write_csv(rhos, "data/output/rho_sum_stats.csv")
