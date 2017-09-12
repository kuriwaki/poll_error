rm(list = ls())
library(cowplot)
library(ggplot2)
library(scales)
library(readr)
library(ggrepel)
library(dplyr)
library(tibble)
library(foreach)


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
       hcu_vot = expression(log~bgroup("(", abs(~Clinton+Undecided~~italic(widehat(italic(rho))[N[avp]])), ")")),
       hcu_vep = expression(log~bgroup("(", abs(~Clinton+Undecided~~italic(widehat(italic(rho))[N[vep]])), ")")),
       hcu_vvt = expression(log~bgroup("(", abs(~Clinton+Undecided~~italic(widehat(italic(rho))[N[vv]])), ")")),
       djt_vot = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[avp]])), ")")),
       djt_vep = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[vep]])), ")")),
       djt_vvt = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[vv]])), ")")),
       djt_pst = expression(log~bgroup("(", abs(~Trump~~italic(widehat(italic(rho))[N[post]])), ")")),
       dtu_vot = expression(log~bgroup("(", abs(~Trump+Undecided~~italic(widehat(italic(rho))[N[avp]])), ")")),
       dtu_vep = expression(log~bgroup("(", abs(~Trump+Undecided~~italic(widehat(italic(rho))[N[vep]])), ")")),
       dtu_vvt = expression(log~bgroup("(", abs(~Trump+Undecided~~italic(widehat(italic(rho))[N[vv]])), ")"))
  )

# normal rho
rho_exp <- 
  list(hrc_vot = expression(Clinton~~italic(widehat(italic(rho))[N[avp]])),
       hrc_vep = expression(Clinton~~italic(widehat(italic(rho))[N[vep]])),
       hrc_vvt = expression(Clinton~~italic(widehat(italic(rho))[N[vv]])),
       hrc_pst = expression(Clinton~~italic(widehat(italic(rho))[N[post]])),
       hcu_vot = expression(Clinton+Undecided~~italic(widehat(italic(rho))[N[avp]])),
       hcu_vep = expression(Clinton+Undecided~~italic(widehat(italic(rho))[N[vep]])),
       hcu_vvt = expression(Clinton+Undecided~~italic(widehat(italic(rho))[N[vv]])),
       djt_vot = expression(Trump~~italic(widehat(italic(rho))[N[avp]])),
       djt_vep = expression(Trump~~italic(widehat(italic(rho))[N[vep]])),
       djt_vvt = expression(Trump~~italic(widehat(italic(rho))[N[vv]])),
       djt_pst = expression(Trump~~italic(widehat(italic(rho))[N[post]])),
       dtu_vot = expression(Trump+Undecided~~italic(widehat(italic(rho))[N[avp]])),
       dtu_vep = expression(Trump+Undecided~~italic(widehat(italic(rho))[N[vep]])),
       dtu_vvt = expression(Trump+Undecided~~italic(widehat(italic(rho))[N[vv]])))

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
  arrange(rho_type, subset) %>% 
  mutate(descrip = forcats::as_factor(gsub("(hrc|djt|hcu|dtu)-", "", descrip)),
         descrip = forcats::as_factor(gsub("_states-", "; ", descrip)),
         cand = factor(cand, levels = c("hrc", "djt", "hcu", "dtu")),
         emph = case_when(subset == "all" ~ "1", subset != "all" ~ "0"),
         ymin = coef - qnorm(0.975)*se,
         ymax = coef + qnorm(0.975)*se) 

candlab <- c("hrc" = "Clinton Support Responses",
             "djt" = "Trump Support Responses",
             "hcu" = "Clinton + Undecideds Responses",
             "dtu" = "Trump + Undecideds Response")

colorvec_pn <- c(colorvec, 
                 "all" = "black",
                 "pos" = "#ff7f00",
                 "neg" = "#6a3d9a")

labvec <- c("D" = "Blue states",
            "R" = "Red states", 
            "swing" = "Swing states", 
            "all" = "All states",
            "pos" = "rho > 0 (overestimates)",
            "neg" = "rho < 0 (underestimates)")

ggplot(coef_plot, aes(y = coef, x = descrip, ymin = ymin, ymax = ymax, color = subset, size = emph)) +
  facet_wrap(~cand, labeller = labeller(cand = candlab), nrow = 2) +
  theme(axis.line=element_line()) + 
  scale_color_manual(name = "States used", values = colorvec_pn, labels = labvec) +
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgray") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_pointrange(shape = 18) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_size_manual(values = c("1" = 0.75, "0" = 0.5)) +
  guides(size = FALSE, color = guide_legend(nrow = 1)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Specifications of rho",
       y = "Slope coefficient from log(abs(rho)) regressed on log(N), with 95 percent CI",
       caption = "Points ordered by specification then by color.
       Missing values occur when there were too few observations (3 or less) to calculate a slope.")
ggsave("figures/corr-rho-N_intervals.pdf", w = 1.9*fig.w, h = 2.1*fig.h)


# Histogram of rho ----

gg0 <- ggplot(df) + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = "Count") +
  coord_cartesian(xlim = lim_rho, ylim = ylim_hist) + 
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  geom_histogram(binwidth = 0.001) + theme_bw()
  

rho_vec <- names(rho_exp)

for (rho_name in rho_vec) {
  var_name <- paste0("rho_", rho_name)
  file_name <- paste0("figures/hist_", var_name, ".pdf")
  gg0 + aes_string(x = var_name) + labs(x = rho_exp[[rho_name]])
  ggsave(file_name, width = fig.w, height = fig.h)
}

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
fig.w <- 1.2*fig.w*0.8
fig.h <- 1.2*fig.h



muhats <- grep("cces_pct_", colnames(df), value = TRUE)
muhats <- setdiff(muhats, grep("vep", muhats, value = TRUE))


# make data frame for plots top print
sct_labs <- tibble(var_name = muhats)
sct_labs <- sct_labs %>%
  mutate(cand = case_when(grepl("cces_pct_h", var_name) ~ "H",
                          grepl("cces_pct_d", var_name) ~ "T")) %>%
  mutate(est_t = case_when(grepl("raw$", var_name) ~ "Raw ",
                           grepl("voters$", var_name) ~ "Turnout-adjusted ",
                           grepl("vv$", var_name) ~ "Validated Voter ",
                           grepl("post$", var_name) ~ "Post-Election Wave ")) %>%
  mutate(cand_t = case_when(grepl("_hrc_", var_name) ~ "Clinton Support",
                            grepl("_hrcund_", var_name) ~ "Clinton + Undecideds",
                            grepl("_djt_", var_name) ~ "Trump Support",
                            grepl("_djtund_", var_name) ~ "Trump + Undecideds")) %>%
  mutate(xlab_text = paste0(est_t, "Poll Estimate, ", cand_t))


# starting scatter
gg0 <- ggplot(df, aes(x = cces_pct_hrc_voters, y = pct_hrc_voters, color = color, size = vap)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1), label = percent, minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1), label = percent, minor_breaks = NULL) +
  scale_color_manual(values = colorvec) +
  guides(size = FALSE, color = FALSE) +
  coord_equal() +
  theme_bw() +
  geom_point(alpha = 0.8) +
  theme(plot.caption = element_text(size = 8))

# loop through and list
sct_gglist <- foreach(i = 1:nrow(sct_labs)) %do% {
  
  if (sct_labs$cand[i] == "H") {
    mu <- df$pct_hrc_voters
    gg <- gg0 +
      annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nClinton support", color = "darkgray") +
      annotate("text", x = 0.22, y = 0.9, label = "Poll underestimated\nClinton support", color = "darkgray") +
      labs(y = "Final Clinton Popular Vote Share")
  }
  
  if (sct_labs$cand[i] == "T") {
    mu <- df$pct_djt_voters
    gg <- gg0 + aes(y = pct_djt_voters) +
      annotate("text", x = 0.8, y = 0.1, label = "Poll overestimated\nTrump support", color = "darkgray") +
      annotate("text", x = 0.22, y = 0.9, label = "Poll underestimated\nTrump support", color = "darkgray") +
      labs(y = "Final Trump Popular Vote Share")
  }
  
  rmse <- sqrt(mean((mu - df[[sct_labs$var_name[i]]])^2))
  
  gg <- gg + aes_string(x = sct_labs$var_name[i]) +
    xlab(sct_labs$xlab_text[i]) +
    labs(caption = paste0("Root Mean Squared Error: ", sprintf("%3.2f", rmse)))
  
  return(gg)
}

names(sct_gglist) <- sct_labs$var_name


# plot one by one
for (fnames in names(sct_gglist)) {
  est_name <- gsub("cces_pct_", "", fnames)
  est_name <- gsub("voters_post", "pst", est_name)
  est_name <- gsub("vv", "vvt", est_name)
  est_name <- gsub("voters", "vot", est_name)
  est_name <- gsub("hrcund", "hcu", est_name)
  est_name <- gsub("djtund", "dtu", est_name)
  
  ggsave(paste0("figures/scatter_", est_name, ".pdf"), 
         plot = sct_gglist[[fnames]],
         h = fig.h, w = fig.w)
}

ord_labs <- sort(sct_labs$var_name)
stopifnot(length(ord_labs) == 14)  # for grid
ord_gg <- sct_gglist[ord_labs]


ord_gg_16 <- c(ord_gg[1:6], 
               NA, 
               ord_gg[7], 
               ord_gg[8:13],
               NA,
               ord_gg[14])


gg16 <- plot_grid(ord_gg[[1]], ord_gg[[2]], ord_gg[[3]], ord_gg[[4]],
          ord_gg[[5]], ord_gg[[6]], NULL, ord_gg[[7]],
          ord_gg[[8]], ord_gg[[9]], ord_gg[[10]], ord_gg[[11]],
          ord_gg[[12]], ord_gg[[13]], NULL, ord_gg[[14]],
          nrow = 4)

save_plot("figures/scatter_all.pdf", gg16, ncol = 4, nrow = 4)

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

