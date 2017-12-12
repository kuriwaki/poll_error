library(tidyverse)
library(scales)
library(ggrepel)
library(glue)

df <- read_csv("data/output/pres16_state.csv", col_types = cols())


# totals ----
colSums(select(df, vap, vep, matches("votes"), matches("cces_tot"),  matches("cces_n_"))) %>% 
  t() %>% as.data.frame() %>%
  write_csv("data/output/pres16_US.csv")


cces_n_raw <- sum(df$cces_n_raw)
cces_n_vv <- sum(df$cces_n_vv)

# validated voters  ------

df %>% 
  mutate(pct_n_vv = percent(round(cces_n_vv / cces_n_raw, 2))) %>%
  mutate(state = glue("{state} ({pct_n_vv})")) %>%
  arrange(cces_n_raw) %>%
  ggplot(aes(x = fct_inorder(state), y = cces_n_raw)) +
  geom_point() +
  geom_point(aes(y = cces_n_vv), shape = 21, fill = "white") +
  scale_y_continuous(label = comma) +
  theme_bw() +
  ggExtra::rotateTextX() +
  labs(x = "State (In parentheses: proportion of validated voters among full sample)",
       y = "CCES sample size \nBlack: full sample, white:validaated voters",
       caption = "Source: 2016 CCES pre-election survey")
ggsave("figures/sample-size_by-state.pdf", w = 10, h = 5)


# sample to population fractions -----
df %>% 
  mutate(f_raw = (cces_n_raw / tot_votes),
         f_vot = (cces_n_voters / tot_votes),
         f_vvt = (cces_n_vv / tot_votes)) %>%  
  melt(id.vars = "st", measure.vars = c("f_raw", "f_vot", "f_vvt")) %>% 
  group_by(variable) %>%
  summarize(n = n(),
            min = min(value),
            p25 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            p75 = quantile(value, 0.75),
            max = max(value))


# turnout estimates  vs. actual turnout ------
ggplot(df, aes(x = tot_votes/vap, y = cces_n_vv/cces_n_raw, color = color)) +
  coord_equal() +
  scale_color_manual(values =colorvec <- c("R" =  '#d7191c', "swing" = 'darkgreen', "D" = '#2c7bb6')) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  guides(color = FALSE) +
  labs(x = "Population Turnout\n(Total Votes for President / Voting Age Population)",
       y = "Estimated Turnout\n(CCES validated voters / CCES total sample)")
ggsave("figures/turnout_vvt.pdf", h = 5, w = 4)

tots <- colSums(select(df, vap, vep, matches("votes"), matches("cces_tot"),  matches("cces_n_"))) %>% 
  t() %>% as.data.frame()

tots["tot_votes"] / tots["vap"]


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
