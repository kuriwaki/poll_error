library(tibble)
library(readr)
library(dplyr)

df <- read_csv("data/output/pres16_state.csv", col_types = cols())


# totals ----
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
