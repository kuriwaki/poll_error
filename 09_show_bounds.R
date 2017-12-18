library(plotly)
library(tidyverse)
library(glue)

f <- seq(0, 1, 0.01)
p <- seq(0, 1, 0.01)


# vectorized functions ----
calc_lb <- function(f, p) {
  Do <- (1 - f)/f
  Og <- p / (1 - p)
  
  cands <- tibble(lb1 = sqrt(Do/Og),
                  lb2 = sqrt(Og/Do))
  
  cands %>% 
    rowwise() %>% 
    summarize(lb = (-1)*min(lb1, lb2)) %>% 
    pull(lb)
}

calc_ub <- function(f, p) {
  Do <- (1 - f)/f
  Og <- p / (1 - p)
  
  cands <- tibble(ub1 = sqrt(Do*Og),
                  ub2 = 1/sqrt(Og*Do))
  
  
  cands %>% 
    rowwise() %>% 
    summarize(ub = min(ub1, ub2)) %>% 
    pull(ub)
}


pfdf <- tidyr::complete(tibble(f = f, p = p), f, p) %>% 
  mutate(rho_lb = calc_lb(f, p),
         rho_ub = calc_ub(f, p))


# calculate rho for grid
rho_ub <- rho_lb <- matrix(NA,
                           nrow = length(f),
                           ncol = length(p),
                 dimnames = list(f, p))
for (xi in f) {
  for (yi in p) {
    rho_lb[as.character(xi), as.character(yi)] <- calc_lb(xi, yi)
    rho_ub[as.character(xi), as.character(yi)] <- calc_ub(xi, yi)
  }
}


pp.surface <- plot_ly(showscale = FALSE) %>%
  add_surface(x = f, y = p, z = rho_ub, opacity = 0.65, reversescale = TRUE) %>%
  add_surface(x = f, y = p, z = rho_lb, opacity = 0.65) %>%
  layout(scene = list(
    xaxis = list(title = "f = n / N"),
    yaxis = list(title = "p = P(G = 1)"),
    zaxis = list(title = "Bounds on ρ")))

chart_link <- api_create(pp.surface, filename = "poll-error_rho", sharing = "public")
chart_link




# flat image

df <- tibble(f = 0:1)


plists <- c(0.05, 0.10, 0.25, 0.5)

boundslist <- list()

for (p_i in 1:length(plists)) {
  
  boundslist[[p_i]] <- ggplot(df, aes(x = f)) +
    theme_bw() +
    scale_y_continuous(limits = c(-1, 1), minor_breaks = FALSE) +
    scale_x_continuous(minor_breaks = FALSE) +
    stat_function(fun = calc_ub,  args = list(p = plists[p_i]), geom = "line") +
    stat_function(fun = calc_lb,  args = list(p = plists[p_i]), geom = "line") +
    labs(y = expression(plain("Bounds on ")~italic(rho[list(italic(R),italic(G))])),
         x = expression(italic(f[R]==n/N)),
         title = substitute(italic(p[G])==X, list(X = plists[p_i]))) +
    theme(plot.title = element_text(size = 12, hjust = 0.5))
}

bounds <- cowplot::plot_grid(plotlist = boundslist, ncol = 2)
ggsave("figures/sims/bounds_2d.pdf", bounds,  w = 4, h = 4)


# coverage
dfz <- tibble(b = c(-6, 6))

Cb <- function(b) {
  pnorm(q = 2 - b) - pnorm(q = - 2 - b)
}

gg6 <-  ggplot(dfz, aes(x = b)) +
  stat_function(fun = Cb) +
  scale_y_continuous(breaks = c(seq(0, 1, 0.25), 0.95), minor_breaks = FALSE) +
  scale_x_continuous(breaks = seq(-6, 6, 1), minor_breaks = FALSE, expand = c(0, 0)) +
  theme_bw() +
  labs(y = "Confidence Coverage",
       x = "Relative Bias")

gg6
ggsave("figures/sims/coverage_6.pdf", gg6, w  = 6, h = 2.25)
