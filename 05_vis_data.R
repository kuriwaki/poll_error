library(ggplot2)
library(scales)
library(readr)

fig.w <- 6
fig.h <- 4


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



slopes <- c(lm_eqn("log(abs(rho_voter)) ~ log(tot_votes)", df),
            lm_eqn(ff = "log(abs(rho_voter)) ~ log(tot_votes)", filter(df, rho_voter_positive)),
            lm_eqn(ff = "log(abs(rho_voter)) ~ log(tot_votes)", filter(df, !rho_voter_positive)),
            lm_eqn("log(abs(rho_vep)) ~ log(vep)", df),
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




# plots for rho

gg0 <- ggplot(df, aes(label = st, color = color)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_point() +
  scale_color_manual(values = colorvec)  +
  geom_text_repel(alpha = 0.5) +
  # coord_equal() +
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

