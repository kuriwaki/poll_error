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
                                    muhat = "cces_pct_hrc_raw",
                                    n = "cces_n_raw"),
         rho_hrc_vvt = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrc_vv",
                                    n = "cces_n_vv"),
         rho_hrc_pst = rho_estimate(N = "tot_votes",
                                    mu = "pct_hrc_voters",
                                    muhat = "cces_pct_hrc_voters_post",
                                    n = "cces_n_post_voters"),
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
                                    n = "cces_n_post_voters")
  )


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

rhos_sum <- function(data = df, suffix) {
  dframe <-  cbind(my_qtl(data$rho_hrc_vot, "rho_hrc_vot"),
                   my_qtl(data$rho_hrc_vep, "rho_hrc_vep"),
                   my_qtl(data$rho_hrc_vvt, "rho_hrc_vvt"),
                   my_qtl(data$rho_djt_vot[data$rho_djt_vot < 0], "rho_djt_avp"),
                   my_qtl(data$rho_djt_vep[data$rho_djt_vep < 0], "rho_djt_vep"),  
                   my_qtl(data$rho_djt_vvt[data$rho_djt_vvt < 0], "rho_djt_vvt"))  
  colnames(dframe) <- paste0(colnames(dframe), "_", suffix)
  dframe
}

all_rho_sums <- cbind(
  rhos_sum(df, "allstates"),
  rhos_sum(filter(df, color == "R"), "Rstates"),
  rhos_sum(filter(df, color == "D"), "Dstates"),
  rhos_sum(filter(df, color == "swing"), "swingstates"))

all_rho_sums <- cbind("statistic" = rownames(all_rho_sums), all_rho_sums) %>%
  as.data.frame()

write_csv(all_rho_sums, "data/output/rho_sum_stats.csv")


# rhosq
rho_long <- melt(as.data.table(df), 
                 id.vars = c("st", "color"), 
                 variable.factor = FALSE,
                 measure.vars = patterns("rho_"),
                 value.name = "rho") %>% 
  tbl_df() %>% 
  mutate(rho_sq = rho^2)



# with color and all states ----
rho_long <- bind_rows(rho_long, mutate(rho_long, color = "all"))


rho_means <- rho_long %>% 
  group_by(variable, color) %>% 
  summarize(average_of_rho_sq = formatC(mean(rho_sq), digits = 12, format = "f"),
            one_over_avg_rho_sq = 1 / mean(rho_sq),
            number_of_states = n())
write_csv(rho_means, "data/output/rho_sq_state_averages.csv")


# get f for all four estimators
f_long <- df %>% 
  mutate(f_vot = cces_n_voters / tot_votes,
         f_vep = cces_n_raw / vep)  %>% 
  select(st, matches("^f_"))

range(f_long$f_vot)
range(f_long$f_vep)
