library(glue)
library(tidyr)
library(foreach)

df_raw <- read_csv("data/output/pres16_state.csv", col_types = cols())

lm_store <- function(cand_text, subset, rho_type, N_text) {
  
  rho_text <- glue("rho_{cand_text}_{rho_type}") # e.g. rho_hrc_vot
  ff <- glue("log(abs({rho_text})) ~ log({N_text})")
  
  ## subset states
  if (subset == "all") dfreg <- df
  if (subset %in% c("D", "R", "swing")) dfreg <- filter(df, color == subset)
  if (subset == "pos") dfreg <- filter(df, .data[[rho_text]] > 0)
  if (subset == "neg") dfreg <- filter(df, .data[[rho_text]] < 0)
  
  ## run model
  if (nrow(dfreg) > 1) {
    mod <- lm(as.formula(ff), dfreg)
    coef <- sprintf("%.2f", coef(mod)[2])
    se <- sprintf("%.2f", summary(mod)$coef[2, 2])
    lab <- glue("{coef}\n ({se})")
  } else {
    lab <- NA
  }
  
  tibble(cand = cand_text,
         subset = subset,
         rho_type = rho_type,
         rho_text = as.character(rho_text),
         N_text = N_text,
         nstates = nrow(dfreg),
         lm_form = as.character(ff),
         lab = as.character(lab))
}


reg_specs <- tibble(cand = c("hrc", "djt", rep(NA, 4)),
                    subset = c("all", "R", "D", "swing", "pos", "neg"),
                    rho_type = c("vot", "vep", "vvt", "pst", rep(NA, 2))) %>%
  complete(cand, subset, rho_type)

rho_N <- tibble(rho_type = c("vot", "vep", "vvt", "pst"),
                N_type = c("tot_votes", "vep", "tot_votes", "tot_votes"))

reg_specs <- left_join(reg_specs, rho_N, by = "rho_type")


lm_stored <-  foreach(i = 1:nrow(reg_specs), .combine = "bind_rows") %do% {
  lm_store(cand_text = reg_specs$cand[i],
           subset = reg_specs$subset[i],
           rho_type = reg_specs$rho_type[i],
           N_text = reg_specs$N_type[i])
}


saveRDS(lm_stored, "data/output/rho-N_lm-output.rds")
