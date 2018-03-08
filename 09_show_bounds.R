library(plotly)
library(tidyverse)
library(glue)



# coverage ----
dfz <- tibble(b = c(-6, 6))

Cb <- function(b) {
  pnorm(q = 2 - b) - pnorm(q = -2 - b)
}

gg6 <- ggplot(dfz, aes(x = b)) +
  stat_function(fun = Cb) +
  scale_y_continuous(limit = c(-0.005, 1), breaks = c(seq(0, 1, 0.25), 0.95), minor_breaks = FALSE, , expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-6, 6, 1), minor_breaks = FALSE, expand = c(0, 0)) +
  theme_bw() +
  labs(
    y = "Confidence Coverage",
    x = "Relative Bias"
  )

gg6
ggsave("figures/sims/coverage_6.pdf", gg6, w = 6, h = 2.25)



# vectorized functions ----
calc_lb <- function(f, p) {
  Do <- (1 - f) / f
  Og <- p / (1 - p)

  cands <- tibble(
    lb1 = sqrt(Do / Og),
    lb2 = sqrt(Og / Do)
  )

  cands %>%
    rowwise() %>%
    summarize(lb = (-1) * min(lb1, lb2)) %>%
    pull(lb)
}

calc_ub <- function(f, p) {
  Do <- (1 - f) / f
  Og <- p / (1 - p)

  cands <- tibble(
    ub1 = sqrt(Do * Og),
    ub2 = 1 / sqrt(Og * Do)
  )


  cands %>%
    rowwise() %>%
    summarize(ub = min(ub1, ub2)) %>%
    pull(ub)
}


# flat image of bounds ----

df <- tibble(f = 0:1)

plists <- c(0.05, 0.10, 0.25, 0.50)

boundslist <- list()

for (p_i in 1:length(plists)) {
  boundslist[[p_i]] <- df %>% 
    mutate(p = plists[[p_i]]) %>%
    ggplot(aes(x = f)) +
    theme_bw() +
    facet_wrap(~p, labeller = label_bquote(italic(p[G]) == .(p))) +
    scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    stat_function(fun = calc_ub, args = list(p = plists[p_i]), geom = "line") +
    stat_function(fun = calc_lb, args = list(p = plists[p_i]), geom = "line") +
    labs(
      y = expression(plain("Bounds on ")~italic(rho[list(italic(R), italic(G))])),
      x = expression(italic(f == n / N))) +
    theme(plot.title = element_text(size = 12, hjust = 0.5))
}

bounds <- cowplot::plot_grid(plotlist = boundslist, ncol = 2)
ggsave("figures/sims/bounds_2d.pdf", bounds, w = 4, h = 4)




# grid of bounds ---
f <- seq(0, 1, 0.01)
p <- seq(0, 1, 0.01)
pfdf <- tidyr::complete(tibble(f = f, p = p), f, p) %>%
  mutate(
    rho_lb = calc_lb(f, p),
    rho_ub = calc_ub(f, p)
  )


# calculate rho for grid
rho_ub <- rho_lb <- matrix(
  NA,
  nrow = length(f),
  ncol = length(p),
  dimnames = list(f, p)
)
for (xi in f) {
  for (yi in p) {
    rho_lb[as.character(xi), as.character(yi)] <- calc_lb(xi, yi)
    rho_ub[as.character(xi), as.character(yi)] <- calc_ub(xi, yi)
  }
}


pp.surface <- plot_ly(showscale = FALSE) %>%
  add_surface(x = f, y = p, z = rho_ub, opacity = 0.65, reversescale = TRUE,
              colorscale = list(c(0,1), c("rgb(0,0,0)","rgb(256,256,256)"))) %>%
  add_surface(x = f, y = p, z = rho_lb, opacity = 0.65, 
              colorscale = list(c(0,1), c("rgb(0,0,0)","rgb(256,256,256)"))) %>%
  layout(scene = list(
    xaxis = list(title = "f = n / N",  ticks = "",    ticklen = 10),
    yaxis = list(title = "p = P(G = 1)", ticks = "",  ticklen = 10),
    zaxis = list(title = "Bounds on ρ", ticks = "",   ticklen = 10)
  ))

pp.surface

# replace if needed
chart_link <- api_create(pp.surface, filename = "poll-error_rho", sharing = "public")
chart_link
