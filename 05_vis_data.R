rm(list = ls())
library(ggplot2)
library(scales)
library(readr)
library(ggrepel)
library(dplyr)
library(tibble)

fig.w <- 6*0.80
fig.h <- 4*0.751

colorvec <- c("R" =  '#d7191c', "swing" = 'darkgreen', "D" = '#2c7bb6')


# plots for rho ----------
# data and vars
df <- read_csv("data/output/pres16_state.csv") %>%
  mutate(rc_vot_pos = rho_hrc_vot > 0,
         rc_vep_pos = rho_hrc_vep > 0,
         rt_vot_pos = rho_djt_vot > 0,
         rt_vep_pos = rho_djt_vep > 0)


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


# can't run a regression with only one data point
stopifnot(sum(df$rt_vep_pos == TRUE) <= 1)
stopifnot(sum(df$rt_vot_pos == TRUE) <= 1)

slopes <- c(lm_eqn(ff = "log(abs(rho_hrc_vot)) ~ log(tot_votes)", df),
            lm_eqn(ff = "log(abs(rho_hrc_vot)) ~ log(tot_votes)", filter(df, rc_vot_pos)),
            lm_eqn(ff = "log(abs(rho_hrc_vot)) ~ log(tot_votes)", filter(df, !rc_vot_pos)),
            lm_eqn(ff = "log(abs(rho_hrc_vep)) ~ log(vep)", df),
            lm_eqn(ff = "log(abs(rho_hrc_vep)) ~ log(vep)", filter(df, rc_vep_pos)),
            lm_eqn(ff = "log(abs(rho_hrc_vep)) ~ log(vep)", filter(df, !rc_vep_pos)),
            lm_eqn(ff = "log(abs(rho_djt_vot)) ~ log(tot_votes)", df),
            lm_eqn(ff = "log(abs(rho_djt_vep)) ~ log(vep)", df)
)

sdf <- tibble(est = rep(c("rho_vot", "rho_vep"), each = 3),
              race = "hrc",
              x = rep(c(16.75, 16.75), each = 3),
              y = rep(c(-9.5, -9.5), each = 3),
              pooled = rep(c(TRUE, FALSE, FALSE), 2),
              rc_vot_pos = c(NA, TRUE, FALSE, rep(NA, 3)),
              rc_vep_pos = c(rep(NA, 3), NA, TRUE, FALSE)) %>% 
  bind_rows(tibble(est = c("rho_vot", "rho_vep"),
                   race = "djt",
                   x = rep(c(16.75, 16.75), 1),
                   y = rep(c(-9.5, -9.5), 1),
                   pooled = c(TRUE, TRUE))) %>%
  add_column(slopes = slopes)

# y-axislimit
all_rhos <- unlist(select(df, matches("rho")), use.names = FALSE)
all_pops <- unlist(select(df, vep, tot_votes), use.names = FALSE)



# ranges
lim_rho <- range(all_rhos)
lim_lro <- range(log(abs(all_rhos)))
lim_lpp <- range(log(all_pops))

# template
gg0 <- ggplot(df, aes(label = st, color = color)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_point() +
  scale_color_manual(values = colorvec)  +
  geom_text_repel(alpha = 0.5) +
  theme_bw() +
  coord_cartesian(ylim = lim_lro, xlim = lim_lpp) +
  guides(color = FALSE)

gg_vot <- gg0 + aes(x = log(tot_votes)) +
  labs(x = "log(Total Voters)")

gg_vep <- gg0 + aes(x = log(tot_votes)) +
  labs(x = "log(Total Voting Eligible Population)")


# start to save figures
gg_vot + 
  aes(y = log(abs(rho_hrc_vot))) +
  labs(y = expression(log(abs(rho[N[avp]])))) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vot", pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vot_pooled.pdf", width = fig.w, height = fig.h)

gg_vot + 
  aes(y = log(abs(rho_hrc_vot))) +
  labs(y = expression(log(abs(rho[N[avp]])))) +
  facet_grid( ~ rc_vot_pos, labeller = labeller(rc_vot_pos = rho_pos_labs)) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vot", !pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vot_separated.pdf", width = fig.w, height = fig.h)

  
gg_vep + 
  aes(y = log(abs(rho_hrc_vep))) +
  labs(y = expression(log(abs(rho[N[vep]])))) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vep", pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vep_pooled.pdf", width = fig.w, height = fig.h)

gg_vep + 
  aes(y = log(abs(rho_hrc_vep))) +
  labs(y = expression(log(abs(rho[N[vep]])))) +
  facet_grid( ~ rc_vep_pos, labeller = labeller(rc_vep_pos = rho_pos_labs)) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vep", !pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vep_separated.pdf", width = fig.w, height = fig.h)

# now TRUMP
gg_vot + 
  aes(y = log(abs(rho_djt_vot))) +
  labs(y = expression(log(abs(rho[N[avp]])))) +
  geom_label(data = filter(sdf, race == "djt", est == "rho_vot", pooled), 
             aes(x = x, y = y, label = slopes), 
             inherit.aes = FALSE)
ggsave("figures/rho_djt_vot.pdf", width = fig.w, height = fig.h)


gg_vep + 
  aes(y = log(abs(rho_djt_vep))) +
  labs(y = expression(log(abs(rho[N[avp]])))) +
  geom_label(data = filter(sdf, race == "djt", est == "rho_vep", pooled), 
             aes(x = x, y = y, label = slopes), 
             inherit.aes = FALSE)
ggsave("figures/rho_djt_vep.pdf", width = fig.w, height = fig.h)


rm(gg0)

# Histogram ----
ylim_hist <- c(0, 12.5)

gg0 <- ggplot(df) + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = "Count") +
  coord_cartesian(xlim = lim_rho, ylim = ylim_hist) + 
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  geom_histogram(binwidth = 0.001) + theme_bw()
  

gg0 + aes(x = rho_hrc_vot) + labs(x = expression(rho[N[avp]]))
ggsave("figures/rho_hrc_vot_hist.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_hrc_vep) + labs(x = expression(rho[N[vep]])) +
ggsave("figures/rho_hrc_vep_hist.pdf", width = fig.w, height = fig.h)


gg0 + aes(x = rho_djt_vot) + labs(x = expression(rho[N[avp]]))
ggsave("figures/rho_djt_vot_hist.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_djt_vep) + labs(x = expression(rho[N[vep]])) +
  ggsave("figures/rho_djt_vep_hist.pdf", width = fig.w, height = fig.h)


ggplot(df, aes(x = cv_turnout_wgt)) +
  geom_histogram(bins = 25) + theme_bw() +
  labs(x = "Coefficient of Variation of Turnout Adjustment Weights at state-level", y = "Count")
ggsave("figures/cv_turnout_hist.pdf", width = fig.w, height = fig.h)

rm(gg0)

# Scatter ------
captext <- "Source: CCES 2016 Common Content." #\nSized proportional to population.\n States colored by R (red) or D (blue) or swing (green)."
fig.w <- 1.2*fig.w*0.8
fig.h <- 1.2*fig.h


gg0 <- ggplot(df, aes(x = cces_pct_hrc_voters, y = pct_hrc_voters, color = color, size = vap)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1), label = percent, minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1), label = percent, minor_breaks = NULL) +
  scale_color_manual(values = colorvec) +
  guides(size = FALSE, color = FALSE) +
  coord_equal() +
  theme_bw() +
  geom_point(alpha = 0.8) +
  theme(plot.caption = element_text(size = 8)) +
  labs(caption = captext)


# Hillary
gg_hrc <- gg0 +
  annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nClinton support", color = "darkgray") +
  annotate("text", x = 0.2, y = 0.9, label = "Poll underestimated\nClinton support", color = "darkgray") +
  labs(y = "Final Clinton Popular Vote Share")

gg_hrc + aes(x = cces_pct_hrc_voters) +
  xlab("Turnout-adjusted Poll Estimate, Clinton Support")
ggsave("figures/scatter_hrc_turnout_adj.pdf", h = fig.h, w = fig.w)

gg_hrc + aes(x = cces_pct_hrc_raw) +
  xlab("Raw Poll Estimate, Clinton Suport")
ggsave("figures/scatter_hrc_raw.pdf", h = fig.h, w = fig.w)


# Trump
gg_djt <- gg0 + aes(y = pct_djt_voters) +
  annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nTrump support", color = "darkgray") +
  annotate("text", x = 0.2, y = 0.9, label = "Poll underestimated\nTrump support", color = "darkgray") +
  labs(y = "Final Trump Popular Vote Share")

gg_djt + aes(x = cces_pct_djt_voters) +
  xlab("Turnout-adjusted Poll Estimate, Trump Support")
ggsave("figures/scatter_djt_turnout_adj.pdf", h = fig.h, w = fig.w)

gg_djt + aes(x = cces_pct_djt_raw) +
  xlab("Raw Poll Estimate, Trump Suport")
ggsave("figures/scatter_djt_raw.pdf", h = fig.h, w = fig.w)



gg0 + aes(x = (cces_n_voters/cces_n_raw), y = (tot_votes/vep), size = vap) +
  annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nturnout", color = "darkgray") +
  annotate("text", x = 0.2, y = 0.9, label = "Poll underestimated\nturnout", color = "darkgray") +
  xlab("Turnout-Adjusted Poll Estimate of Turnout") +
  ylab("Final Turnout\n(% of Voting Eligible Population)") +
  labs(caption = captext)
ggsave("figures/scatter_turnout_accuracy.pdf", h = fig.h, w = fig.w)

