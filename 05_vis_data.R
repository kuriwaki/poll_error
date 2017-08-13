rm(list = ls())
library(cowplot)
library(ggplot2)
library(scales)
library(readr)
library(ggrepel)
library(dplyr)
library(tibble)

fig.w <- 6*0.80
fig.h <- 4*0.751
mfig.w <- fig.w



# labels -----
colorvec <- c("R" =  '#d7191c', "swing" = 'darkgreen', "D" = '#2c7bb6')
rho_pos_labs <- c(`TRUE` = "rho > 0 (Overestimated Clinton)",
                  `FALSE` = "rho < 0 (Underestimated Clinton)")

# conveinence functions
lm_eqn <- function(ff, dat = df){
  m <- lm(as.formula(ff), dat) 
  coef <- sprintf("%.2f", coef(m)[2])
  se <- sprintf("%.2f", summary(m)$coef[2, 2])
  
  glue::glue("{coef}\n ({se})")
}


# percent function
make_pct <- function(dbl, points = FALSE) {
  if (points) 
    return(paste0(round(dbl*100), "%"))

    if (!points) 
    return(paste0(round(dbl, 2)))
}


# data and vars -----
df_raw <- read_csv("data/output/pres16_state.csv", col_types = cols())
df <- df_raw %>%
  mutate(rc_vot_pos = rho_hrc_vot > 0,
         rc_vep_pos = rho_hrc_vep > 0,
         rt_vot_pos = rho_djt_vot > 0,
         rt_vep_pos = rho_djt_vep > 0)

# limit parameters ---
# y-axislimit
all_rhos <- unlist(select(df, matches("rho")), use.names = FALSE)
all_pops <- unlist(select(df, vep, tot_votes), use.names = FALSE)

# ranges
lim_rho <- range(all_rhos)
lim_lro <- range(log(abs(all_rhos)))
lim_lpp <- range(log(all_pops))

# for hist
ylim_hist <- c(0, 12.5)


# labels ---
# log absolute value expressions 
lar_exp <- 
  list(hrc_vot = expression(log~bgroup("(", abs(~Clinton~~italic(widehat(italic(rho))[N[avp]])), ")")),
       hrc_vep = expression(log~bgroup("(", abs(~Clinton~~italic(widehat(italic(rho))[N[vep]])), ")")),
       djt_vot = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[avp]])), ")")),
       djt_vep = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[vep]])), ")")))

# normal stuff
rho_exp <- 
  list(hrc_vot = expression(Clinton~~italic(widehat(italic(rho))[N[avp]])),
       hrc_vep = expression(Clinton~~italic(widehat(italic(rho))[N[vep]])),
       djt_vot = expression(Trump~~italic(widehat(italic(rho))[N[avp]])),
       djt_vep = expression(Trump~~italic(widehat(italic(rho))[N[vep]])))

# titles
eff_t <- list(
  hrc_vot = expression(frac(n[eff], n):~Clinton~widehat(mu)[avp]),
  hrc_vep = expression(frac(n[eff], n):~Clinton~widehat(mu)[vep]),
  djt_vot = expression(frac(n[eff], n):~Trump~widehat(mu)[avp]),
  djt_vep = expression(frac(n[eff], n):~Trump~widehat(mu)[vep])
)





# plots for rho ---------

# can't run a regression with only one data point
stopifnot(sum(df$rt_vep_pos == TRUE) <= 1)
stopifnot(sum(df$rt_vot_pos == TRUE) <= 1)

# collect slopes
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




# template
gg0 <- ggplot(df, aes(label = st, color = color)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_point() +
  scale_color_manual(values = colorvec)  +
  geom_text_repel(alpha = 0.5) +
  theme_bw() +
  coord_cartesian(ylim = lim_lro, xlim = lim_lpp) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  guides(color = FALSE)

gg_vot <- gg0 + aes(x = log(tot_votes)) +
  labs(x = "log(Total Voters)")

gg_vep <- gg0 + aes(x = log(tot_votes)) +
  labs(x = "log(Total Voting Eligible Population)")


# start to save figures
gg_vot + 
  aes(y = log(abs(rho_hrc_vot))) +  labs(y = lar_exp[["hrc_vot"]]) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vot", pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vot_pooled.pdf", width = fig.w, height = fig.h)

gg_vot + 
  aes(y = log(abs(rho_hrc_vot))) +  labs(y = lar_exp[["hrc_vot"]]) +
  facet_grid( ~ rc_vot_pos, labeller = labeller(rc_vot_pos = rho_pos_labs)) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vot", !pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vot_separated.pdf", width = fig.w, height = fig.h)

  
gg_vep + 
  aes(y = log(abs(rho_hrc_vep))) +  labs(y = lar_exp[["hrc_vep"]]) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vep", pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vep_pooled.pdf", width = fig.w, height = fig.h)

gg_vep + 
  aes(y = log(abs(rho_hrc_vep))) +  labs(y = lar_exp[["hrc_vep"]]) +
  facet_grid( ~ rc_vep_pos, labeller = labeller(rc_vep_pos = rho_pos_labs)) +
  geom_label(data = filter(sdf, race == "hrc", est == "rho_vep", !pooled), 
            aes(x = x, y = y, label = slopes), 
            inherit.aes = FALSE)
ggsave("figures/rho_hrc_vep_separated.pdf", width = fig.w, height = fig.h)

# now TRUMP
gg_vot + 
  aes(y = log(abs(rho_djt_vot))) +  labs(y = lar_exp[["djt_vot"]]) +
  geom_label(data = filter(sdf, race == "djt", est == "rho_vot", pooled), 
             aes(x = x, y = y, label = slopes), 
             inherit.aes = FALSE)
ggsave("figures/rho_djt_vot.pdf", width = fig.w, height = fig.h)


gg_vep + 
  aes(y = log(abs(rho_djt_vep))) +  labs(y = lar_exp[["djt_vep"]]) +
  geom_label(data = filter(sdf, race == "djt", est == "rho_vep", pooled), 
             aes(x = x, y = y, label = slopes), 
             inherit.aes = FALSE)
ggsave("figures/rho_djt_vep.pdf", width = fig.w, height = fig.h)


rm(gg0)





# Histogram ----


gg0 <- ggplot(df) + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = "Count") +
  coord_cartesian(xlim = lim_rho, ylim = ylim_hist) + 
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  geom_histogram(binwidth = 0.001) + theme_bw()
  

gg0 + aes(x = rho_hrc_vot) + labs(x = rho_exp[["hrc_vot"]])
ggsave("figures/rho_hrc_vot_hist.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_hrc_vep) + labs(x = rho_exp[["hrc_vep"]]) +
ggsave("figures/rho_hrc_vep_hist.pdf", width = fig.w, height = fig.h)


gg0 + aes(x = rho_djt_vot) + labs(x = rho_exp[["djt_vot"]])
ggsave("figures/rho_djt_vot_hist.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_djt_vep) + labs(x = rho_exp[["djt_vep"]])
  ggsave("figures/rho_djt_vep_hist.pdf", width = fig.w, height = fig.h)


ggplot(df, aes(x = cv_turnout_wgt)) +
  geom_histogram(bins = 25) + theme_bw() +
  labs(x = "Coefficient of Variation of Turnout Adjustment Weights", y = "Count")
ggsave("figures/cv_turnout_hist.pdf", width = fig.w, height = fig.h)

rm(gg0)




# what log transforms look like ----
gg0 <- df %>% 
  ggplot(aes(col = color, label = st)) +
  coord_cartesian(xlim = lim_rho, ylim = lim_lro) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = colorvec) +
  theme_bw() +
  scale_x_continuous(minor_breaks = FALSE) +
  scale_y_continuous(minor_breaks = FALSE) +
  guides(color = FALSE)

ycap <- "log(abs(.))"

# four things
log_trans_list <- list(
  gg0 + aes(x = rho_hrc_vot, y = log(abs(rho_hrc_vot))) +
    labs(x = rho_exp[["hrc_vot"]], y = ycap),
  gg0 + aes(x = rho_djt_vot, y = log(abs(rho_djt_vot))) +
    labs(x = rho_exp[["djt_vot"]], y = ycap),
  gg0 + aes(x = rho_hrc_vep, y = log(abs(rho_hrc_vep))) + 
    labs(x = rho_exp[["hrc_vep"]], y = ycap),
  gg0 + aes(x = rho_djt_vep, y = log(abs(rho_djt_vep))) +
    labs(x = rho_exp[["djt_vep"]], y = ycap)
    )


plot_grid(plotlist = log_trans_list)
ggsave("figures/rho_logabsrho_transformation.pdf", w = fig.w, h = fig.h)

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

gg_hrc + aes(x = cces_pct_hrc_vv) +
  xlab("Poll Estimate among Validated Voters, Clinton Suport")
ggsave("figures/scatter_hrc_vv.pdf", h = fig.h, w = fig.w)


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

gg_djt + aes(x = cces_pct_djt_vv) +
  xlab("Poll Estimate among Validated Voters, Trump Suport")
ggsave("figures/scatter_djt_vv.pdf", h = fig.h, w = fig.w)



# Turnout
gg0 + aes(x = (cces_n_voters/cces_n_raw), y = (tot_votes/vep), size = vap) +
  annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nturnout", color = "darkgray") +
  annotate("text", x = 0.2, y = 0.9, label = "Poll underestimated\nturnout", color = "darkgray") +
  xlab("Turnout-Adjusted Poll Estimate of Turnout") +
  ylab("Final Turnout\n(% of Voting Eligible Population)") +
  labs(caption = captext)
ggsave("figures/scatter_turnout_accuracy.pdf", h = fig.h, w = fig.w)

rm(gg0)


# more summary stats
df_diff <- df %>% 
  mutate(mudiff_hrc_vot = cces_pct_hrc_voters - pct_hrc_voters,
         mudiff_djt_vot = cces_pct_djt_voters - pct_djt_voters)


summary(df_diff$mudiff_djt_vot)
summary(df_diff$mudiff_hrc_vot)

cor(df_diff$mudiff_djt_vot, df_diff$mudiff_hrc_vot)

ggplot(df_diff, aes(x = mudiff_hrc_vot, y = mudiff_djt_vot, color = color, size = vap)) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  scale_color_manual(values = colorvec) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_abline(intercept = 0, slope = -1) +
  coord_equal() +
  labs(y = expression(widehat(mu[Trump]) - mu[Trump]),
       x = expression(widehat(mu[Clinton]) - mu[Clinton])) +
  guides(color = FALSE, size = FALSE)
ggsave("figures/scatter_error_vs_error.pdf", h = fig.h, w = fig.w)



# Map -----

source("data/input/state_coords.R")
df_map <- left_join(df, st)

gg0 <- ggplot(df_map, aes(x = col, y = row, fill = color)) + 
  geom_tile(alpha = 0) +
  geom_tile(color = "white", alpha = 0.5, size = 1) +
  geom_text(color = "black", size = 2.75) +
  scale_fill_manual(values = colorvec) +
  scale_y_reverse() +
  coord_equal() +
  theme_minimal() +
  theme(panel.border = element_blank()) +
  theme(panel.grid = element_blank()) +  
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  labs(x = NULL, y = NULL, title = NULL) +
  guides(fill = FALSE, alpha = FALSE)



gg0 + aes(label = make_pct(effratio_hrc_vot)) +
  labs(title = eff_t[["hrc_vot"]])
ggsave("figures/map_hrc_vot.pdf", h = fig.h, w = mfig.w)

gg0 + aes(label = make_pct(effratio_hrc_vep)) +
  labs(title = eff_t[["hrc_vep"]])
ggsave("figures/map_hrc_vep.pdf", h = fig.h, w = mfig.w)

gg0 + aes(label = make_pct(effratio_djt_vot)) +
  labs(title = eff_t[["djt_vot"]])
ggsave("figures/map_djt_vot.pdf", h = fig.h, w = mfig.w)

gg0 + aes(label = make_pct(effratio_djt_vep)) +
  labs(title = eff_t[["djt_vep"]])
ggsave("figures/map_djt_vep.pdf", h = fig.h, w = mfig.w)

rm(gg0)

# the percentages as a dot/barplot --

# arrange
df_dot <-  df %>% 
  arrange(desc(pct_hrc_voters)) %>%
  mutate(st = forcats::as_factor(st))


gg0 <-  ggplot(df_dot, aes(x = st, y = effratio_hrc_vot, fill = color)) + 
  scale_y_continuous(name = NULL, label = percent, expand = c(0, 0)) +
  scale_fill_manual(values = colorvec) +
  geom_col(alpha = 0.8) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 13),
        panel.grid.major.x = element_blank()) +
  guides(fill = FALSE)  +
  labs(x = "")


gg0 + aes(y = effratio_hrc_vot) +
  labs(title = eff_t[["hrc_vot"]])
ggsave("figures/bars_hrc_vot.pdf", h = fig.h, w = mfig.w)

gg0 + aes(y = effratio_hrc_vep) +
  labs(title = eff_t[["hrc_vep"]])
ggsave("figures/bars_hrc_vep.pdf", h = fig.h, w = mfig.w)

gg0 + aes(y = effratio_djt_vot) +
  scale_y_continuous(name = NULL, label = percent, expand = c(0, 0), limit = c(0, 1)) +
  labs(title = eff_t[["djt_vot"]])
ggsave("figures/bars_djt_vot.pdf", h = fig.h, w = mfig.w)

gg0 + aes(y = effratio_djt_vep) +
  scale_y_continuous(name = NULL, label = percent, expand = c(0, 0), limit = c(0, 1)) +
  labs(title = eff_t[["djt_vep"]])
ggsave("figures/bars_djt_vep.pdf", h = fig.h, w = mfig.w)

