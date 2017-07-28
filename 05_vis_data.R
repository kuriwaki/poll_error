rm(list = ls())
library(ggplot2)
library(scales)
library(readr)
library(ggrepel)
library(dplyr)

fig.w <- 6
fig.h <- 4

colorvec <- c("R" =  '#d7191c', "swing" = 'darkgreen', "D" = '#2c7bb6')


# plots for rho ----------
# data and vars
df <- read_csv("data/output/pres16_state.csv") %>%
  mutate(rho_vep_positive = rho_vep > 0,
         rho_voter_positive = rho_voter > 0)


rho_pos_labs <- c(`TRUE` = "rho > 0 (Overestimated Clinton)",
                  `FALSE` = "rho < 0 (Underestimated Clinton)")

# https://stackoverflow.com/a/7549819/5525412
lm_eqn <- function(ff, dat = df){
  # ff needs to be univariate regression
  m <- lm(as.formula(ff), dat)
  
  # slope 
  coef <- sprintf("%.2f", coef(m)[2])
  se <- sprintf("%.2f", summary(m)$coef[2, 2])
  
  glue::glue("{coef}\n ({se})")
}



slopes <- c(lm_eqn(ff = "log(abs(rho_voter)) ~ log(tot_votes)", df),
            lm_eqn(ff = "log(abs(rho_voter)) ~ log(tot_votes)", filter(df, rho_voter_positive)),
            lm_eqn(ff = "log(abs(rho_voter)) ~ log(tot_votes)", filter(df, !rho_voter_positive)),
            lm_eqn(ff = "log(abs(rho_vep)) ~ log(vep)", df),
            lm_eqn(ff = "log(abs(rho_vep)) ~ log(vep)", filter(df, rho_vep_positive)),
            lm_eqn(ff = "log(abs(rho_vep)) ~ log(vep)", filter(df, !rho_vep_positive))
)

sdf <- tibble(est = rep(c("rho_voter", "rho_vep"), each = 3),
              x = rep(c(16.2, 16.75), each = 3),
              y = rep(c(-9.5, -9), each = 3),
              pooled = rep(c(TRUE, FALSE, FALSE), 2),
              rho_voter_positive = c(NA, TRUE, FALSE, rep(NA, 3)),
              rho_vep_positive = c(rep(NA, 3), NA, TRUE, FALSE),
              slopes = slopes)

gg0 <- ggplot(df, aes(label = st, color = color)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_point() +
  scale_color_manual(values = colorvec)  +
  geom_text_repel(alpha = 0.5) +
  theme_bw() +
  guides(color = FALSE)


gg0 + 
  aes(x = log(tot_votes), y = log(abs(rho_voter))) +
  labs(x = "log(Total Voters)", y = expression(log(abs(rho)))) +
  geom_label(data = filter(sdf, est == "rho_voter", pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_voter_pooled.pdf", width = fig.w, height = fig.h)

gg0 + 
  aes(x = log(tot_votes), y = log(abs(rho_voter))) +
  labs(x = "log(Total Voters)", y = expression(log(abs(rho)))) +
  facet_grid( ~ rho_voter_positive, labeller = labeller(rho_voter_positive = rho_pos_labs)) +
  geom_label(data = filter(sdf, est == "rho_voter", !pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_voter_separated.pdf", width = fig.w, height = fig.h)

  
gg0 + 
  aes(x = log(vep), y = log(abs(rho_vep))) +
  labs(x = "log(Total Voting Eligible Population)", y = expression(log(abs(rho)))) +
  geom_label(data = filter(sdf, est == "rho_vep", pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_vep_pooled.pdf", width = fig.w, height = fig.h)

gg0 + 
  aes(x = log(vep), y = log(abs(rho_vep))) +
  labs(x = "log(Total Voting Eligible Population)", y = expression(log(abs(rho)))) +
  facet_grid( ~ rho_vep_positive, labeller = labeller(rho_vep_positive = rho_pos_labs)) +
  geom_label(data = filter(sdf, est == "rho_vep", !pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_vep_separated.pdf", width = fig.w, height = fig.h)

rm(gg0)

# Histogram ---

ggplot(df, aes(x = rho_voter)) + 
  labs(x = expression(rho), y = "Count") +
  geom_histogram(bins = 25) + theme_bw()
ggsave("figures/rho_voter_hist.pdf", width = fig.w, height = fig.h)

ggplot(df, aes(x = rho_vep)) + 
  labs(x = expression(rho), y = "Count") +
  geom_histogram(bins = 25) + theme_bw()
ggsave("figures/rho_vep_hist.pdf", width = fig.w, height = fig.h)

ggplot(df, aes(x = cv_turnout_wgt)) + 
  labs(x = "Coefficient of Variation of Turnout Adjustment Weights at state-level", y = "Count") +
  geom_histogram(bins = 25) + theme_bw()
ggsave("figures/cv_turnout_hist.pdf", width = fig.w, height = fig.h)



# Scatter ------
gg0 <- ggplot(df, aes(x = cces_pct_hrc_voters, y = pct_hrc_voters, color = color)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1), label = percent) +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  scale_color_manual(values = colorvec) +
  guides(size = FALSE, color = FALSE) +
  coord_equal() +
  theme_bw() +
  geom_point(alpha = 0.8) +
  theme(plot.caption = element_text(size = 8)) +
  labs(caption = "Source: CCES.\nSized proportional to population.\n States colored by R (red) or D (blue) or swing (green).")


ggpres <- gg0 +
  annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nClinton support", color = "darkgray") +
  annotate("text", x = 0.2, y = 0.9, label = "Poll underestimated\nClinton support", color = "darkgray") +
  labs(y = "Final Clinton Popular Vote Share")

ggpres + aes(x = cces_pct_hrc_voters, size = vap) +
  xlab("Turnout-adjusted Poll Estimate, Clinton Support")
ggsave("figures/scatter_turnout_adj.pdf", h = fig.h, w = 0.8*fig.w)

ggpres + aes(x = cces_pct_hrc_raw, size = vap) +
  xlab("Raw Poll Estimate, Clinton Suport")
ggsave("figures/scatter_raw.pdf", h = fig.h, w = 0.8*fig.w)

gg0 + aes(x = (cces_n_voters/cces_n_raw), y = (tot_votes/vep), size = vap) +
  annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nturnout", color = "darkgray") +
  annotate("text", x = 0.2, y = 0.9, label = "Poll underestimated\nturnout", color = "darkgray") +
  xlab("Turnout-Adjusted Poll Estimate of Turnout") +
  ylab("Final Turnout\n(% of Voting Eligible Population)") +
  labs(caption = "Source: CCES.\nStates colored by R (red) or D (blue) or swing (green).")
ggsave("figures/scatter_estimate_turnout.pdf", h = fig.h, w = 0.8*fig.w)

