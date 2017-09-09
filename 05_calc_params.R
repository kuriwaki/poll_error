library(dplyr)
library(readr)
library(tibble)
library(data.table)

# df 

df <- readRDS("data/output/df_joined.rds")


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
                                    muhat = "cces_pct_hrc_raw", # flag
                                    n = "cces_n_raw"),
         rho_hrc_vvt = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrc_vv",
                                    n = "cces_n_vv"),
         rho_hrc_pst = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrc_voters_post",
                                    n = "cces_n_post_voters"),
         rho_hcu_vot = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrcund_voters",
                                    n = "cces_n_voters",
                                    cv = "cv_turnout_wgt"),
         rho_hcu_vep = rho_estimate(N = "vep",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrcund_raw",
                                    n = "cces_n_raw"),
         rho_hcu_vvt = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrcund_vv",
                                    n = "cces_n_vv")) # no post in hcu

df <- df %>% 
  mutate(
         rho_djt_vot = rho_estimate(N = "tot_votes",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djt_voters",
                                    n = "cces_n_voters",
                                    cv = "cv_turnout_wgt"),
         rho_djt_vep = rho_estimate(N = "vep",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djt_raw",
                                    n = "cces_n_raw"),
         rho_djt_vvt = rho_estimate(N = "tot_votes",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djt_vv",
                                    n = "cces_n_vv"),
         rho_djt_pst = rho_estimate(N = "tot_votes",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djt_voters_post",
                                    n = "cces_n_post_voters"),
         rho_dtu_vot = rho_estimate(N = "tot_votes",
                                    mu = "pct_djt_voters",
                                    muhat = "cces_pct_djtund_voters",
                                    n = "cces_n_voters",
                                    cv = "cv_turnout_wgt"),
         rho_dtu_vep = rho_estimate(N = "vep",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_djtund_raw",
                                    n = "cces_n_raw"),
         rho_dtu_vvt = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_djtund_vv",
                                    n = "cces_n_vv"))


# Estimate n_eff ----
eff_estimate <- function(data = df, rho, n, N, color = "color", avg_by_color = TRUE) {
  N <- data[[N]]
  n <- data[[n]]
  color <- data[[color]]
  rho <- data[[rho]]
  
  ## we may take the average of groups of rho
  one_over_rhosq <- rho^{-2}
  
  if (avg_by_color) {
    rho_means <- tapply(X = one_over_rhosq, INDEX = color, FUN = mean) 
    one_over_rhosq <- rho_means[color]
  }
  
  f <- n/N
  one_minus_f <- 1 - f
  
  neff_raw <- one_over_rhosq * (f / one_minus_f)
  
  neff_raw
}

df <- df %>% 
  mutate(neff_hrc_vot = eff_estimate(rho = "rho_hrc_vot",
                                     N = "tot_votes",
                                     n = "cces_n_voters"),
         neff_hrc_vep = eff_estimate(rho = "rho_hrc_vep", 
                                     N = "vep",
                                     n = "cces_n_raw"),
         neff_hrc_vvt = eff_estimate(rho = "rho_hrc_vvt", 
                                     N = "tot_votes",
                                     n = "cces_n_vv"),
         neff_djt_vot = eff_estimate(rho = "rho_djt_vot",
                                     N = "tot_votes",
                                     n = "cces_n_voters"),
         neff_djt_vep = eff_estimate(rho = "rho_djt_vep", 
                                     N = "vep",
                                     n = "cces_n_raw"),
         neff_djt_vvt = eff_estimate(rho = "rho_djt_vvt", 
                                     N = "tot_votes",
                                     n = "cces_n_vv")
  )

# estimate ratio of neff over n ---
eff_ratio_estimate <- function(data = df, n, neff) {
  n <- data[[n]]
  neff <- data[[neff]]
  
  neff/n
}


df <- df %>% 
  mutate(effratio_hrc_vot = eff_ratio_estimate(neff = "neff_hrc_vot",
                                               n = "cces_n_voters"),
         effratio_hrc_vep = eff_ratio_estimate(neff = "neff_hrc_vep",
                                               n = "cces_n_raw"),
         effratio_hrc_vvt = eff_ratio_estimate(neff = "neff_hrc_vvt",
                                               n = "cces_n_vv"),
         effratio_djt_vot = eff_ratio_estimate(neff = "neff_djt_vot",
                                               n = "cces_n_voters"),
         effratio_djt_vep = eff_ratio_estimate(neff = "neff_djt_vep",
                                               n = "cces_n_raw"),
         effratio_djt_vvt = eff_ratio_estimate(neff = "neff_djt_vvt",
                                               n = "cces_n_vv")
  )

# Save ----
write_csv(df, "data/output/pres16_state.csv")




