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
         rc_vvt_pos = rho_hrc_vvt > 0,
         rt_vot_pos = rho_djt_vot > 0,
         rt_vep_pos = rho_djt_vep > 0,
         rt_vvt_pos = rho_djt_vvt > 0)


slopes <- readRDS("data/output/rho-N_lm-output.rds")


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
       hrc_vvt = expression(log~bgroup("(", abs(~Clinton~~italic(widehat(italic(rho))[N[vv]])), ")")),
       hrc_pst = expression(log~bgroup("(", abs(~Clinton~~italic(widehat(italic(rho))[N[post]])), ")")),
       djt_vot = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[avp]])), ")")),
       djt_vep = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[vep]])), ")")),
       djt_vvt = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[vv]])), ")")),
       djt_pst = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[post]])), ")")))

# normal stuff
rho_exp <- 
  list(hrc_vot = expression(Clinton~~italic(widehat(italic(rho))[N[avp]])),
       hrc_vep = expression(Clinton~~italic(widehat(italic(rho))[N[vep]])),
       hrc_vvt = expression(Clinton~~italic(widehat(italic(rho))[N[vv]])),
       hrc_pst = expression(Clinton~~italic(widehat(italic(rho))[N[post]])),
       djt_vot = expression(Trump~~italic(widehat(italic(rho))[N[avp]])),
       djt_vep = expression(Trump~~italic(widehat(italic(rho))[N[vep]])),
       djt_vvt = expression(Trump~~italic(widehat(italic(rho))[N[vv]])),
       djt_pst = expression(Trump~~italic(widehat(italic(rho))[N[post]])))

# titles
eff_t <- list(
  hrc_vot = expression(frac(n[eff], n):~Clinton~widehat(mu)[avp]),
  hrc_vep = expression(frac(n[eff], n):~Clinton~widehat(mu)[vep]),
  hrc_vvt = expression(frac(n[eff], n):~Clinton~widehat(mu)[vvt]),
  djt_vot = expression(frac(n[eff], n):~Trump~widehat(mu)[avp]),
  djt_vep = expression(frac(n[eff], n):~Trump~widehat(mu)[vep]),
  djt_vvt = expression(frac(n[eff], n):~Trump~widehat(mu)[vv])
)


# clear ----
fig_pdfs <- list.files("figures", pattern = "pdf$", full.names = TRUE)
file.remove(fig_pdfs)



# plots for rho vs. N ---------

# can't run a regression with only one data point
stopifnot(sum(df$rt_vep_pos == TRUE) <= 1)
stopifnot(sum(df$rt_vot_pos == TRUE) <= 1)

plot_corr <- function(dat = df, slp = slopes, lmrow) {
  
  slope_i <- slp[lmrow, ]
  cand <- slope_i$cand
  subset <- slope_i$subset
  rho_type <- slope_i$rho_type
  rho_text <- slope_i$rho_text
  N_text <- slope_i$N_text
  filename <- paste0("rho-", slope_i$descrip, ".pdf")
  lab <- slope_i$lab
  
  # pretty labels 
  if (N_text == "tot_votes") xlab_text <- "log(Total Voters)"
  if (N_text == "vep") xlab_text <- "log(Voting Eligible Population)"
  lar_code <- gsub("rho_", "", rho_text)
  
  # update lab by adding state subset info
  if (subset == "R") stlab <- "Red states"
  if (subset == "D") stlab <- "Blue states"
  if (subset == "swing") stlab <- "Swing states"
  if (subset == "pos") stlab <- "States where rho > 0" 
  if (subset == "neg") stlab <- "States where rho < 0"
  if (subset == "all") stlab <- "All states"
  lab <- paste0("Slope Coefficient =   ", lab)

    
  # repel text labels?
  
  # subset data frame
  if (subset %in% c("R", "D", "swing")) df_plot <- filter(dat, color == subset)
  if (subset == "pos") df_plot <- filter(dat, .data[[rho_text]] > 0)
  if (subset == "neg") df_plot <- filter(dat, .data[[rho_text]] < 0)
  if (subset == "all") df_plot <- dat
  
  # get yvar
  df_plot <- df_plot %>%
    mutate(rhovar = .data[[rho_text]])
  
  # add log version of x and y variable
  df_plot <- df_plot %>%
    mutate(log_abs_rho = log(abs(rhovar)),
           log_N = log(.data[[N_text]]))
  
  
  
  # skeleton
  gg0 <- ggplot(df_plot, aes(label = st, color = color)) +
    aes(x = log_N, y = log_abs_rho) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    geom_point() +
    scale_color_manual(values = colorvec)  +
    theme_bw() +
    coord_cartesian(ylim = lim_lro, xlim = lim_lpp) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    guides(color = FALSE) +
    labs(y = lar_exp[[lar_code]],
         x = xlab_text,
         subtitle = stlab,
         caption = lab)
  
  cat(filename, "\n")
  ggsave(file.path("figures", filename), gg0, width = fig.w, height = fig.h)
}


# run through all of them
for (i in which(!is.na(slopes$lab))) {
  plot_corr(lmrow = i)
}


# show distribution of slopes -----
coef_plot <- slopes %>% 
  arrange(coef) %>% 
  mutate(descrip = forcats::as_factor(gsub("(hrc|djt)-", "", descrip)),
         ymin = coef - qnorm(0.975)*se,
         ymax = coef + qnorm(0.975)*se) 

candlab <- c("hrc" = "Clinton Support Responses",
             "djt" = "Trump Support Responses")

ggplot(coef_plot, aes(y = coef, x = descrip, ymin = ymin, ymax = ymax, color = subset)) +
  facet_grid(~cand, labeller = labeller(cand = candlab)) +
  scale_color_manual(values = c(colorvec, "all" = "black", "pos" = "gray", "neg" = "darkgray")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(shape = 18) +
  coord_flip() +
  theme_bw() +
  labs(x = "Specifications of rho",
       y = "Slope coefficient from log(abs(rho)) regressed on log(N), with 95 percent CI",
       caption = "Specifications roughly ordered by magnitude of coefficient.
       Missing values occur when there were\n too few observations (3 or less) to calculate a slope.")
ggsave("figures/corr-rho-N_intervals.pdf", w = 1.5*fig.w, h = 1.5*fig.h)


# Histogram of rho ----

gg0 <- ggplot(df) + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = "Count") +
  coord_cartesian(xlim = lim_rho, ylim = ylim_hist) + 
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  geom_histogram(binwidth = 0.001) + theme_bw()
  

gg0 + aes(x = rho_hrc_vot) + labs(x = rho_exp[["hrc_vot"]])
ggsave("figures/hist_rho_hrc_vot.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_hrc_vep) + labs(x = rho_exp[["hrc_vep"]])
ggsave("figures/hist_rho_hrc_vep.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_hrc_vvt) + labs(x = rho_exp[["hrc_vvt"]])
ggsave("figures/hist_rho_hrc_vvt.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_hrc_pst) + labs(x = rho_exp[["hrc_pst"]])
ggsave("figures/hist_rho_hrc_post.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_djt_vot) + labs(x = rho_exp[["djt_vot"]])
ggsave("figures/hist_rho_djt_vot.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_djt_vep) + labs(x = rho_exp[["djt_vep"]])
ggsave("figures/hist_rho_djt_vep.pdf", width = fig.w, height = fig.h)

gg0 + aes(x = rho_djt_vvt) + labs(x = rho_exp[["djt_vvt"]])
ggsave("figures/hist_rho_djt_vvt.pdf", width = fig.w, height = fig.h)
  

gg0 + aes(x = rho_djt_pst) + labs(x = rho_exp[["djt_pst"]])
ggsave("figures/hist_rho_djt_post.pdf", width = fig.w, height = fig.h)
  
# Histogram of cv_turnout
ggplot(df, aes(x = cv_turnout_wgt)) +
  geom_histogram(bins = 25) + theme_bw() +
  labs(x = "Coefficient of Variation of Turnout Adjustment Weights", y = "Count")
ggsave("figures/hist_cv_turnout.pdf", width = fig.w, height = fig.h)

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
ggsave("figures/logabsrho_transformation.pdf", w = fig.w, h = fig.h)

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

gg_hrc + aes(x = cces_pct_hrc_voters_post) +
  xlab("Poll Estimate from Post-Election wave, Clinton Suport")
ggsave("figures/scatter_hrc_post.pdf", h = fig.h, w = fig.w)



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

gg_djt + aes(x = cces_pct_djt_voters_post) +
  xlab("Poll Estimate from Post-Election wave, Trump Suport")
ggsave("figures/scatter_djt_post.pdf", h = fig.h, w = fig.w)




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

