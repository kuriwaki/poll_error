library(haven)
library(dplyr)



cc16_full <- read_dta("~/Dropbox/cces_cumulative/data/source/cces/2016_cc_vv.dta")


# select columns

# rename
cc16 <- cc16_full %>%
  rename(caseID = V101,
         intent_turnout = CC16_364,
         intent_pres_16 = CC16_364c,
         post_turnout = CC16_401,
         ev_pres_16 = CC16_364b,
         voted_pres_16 = CC16_410a,
         vv_16 = CL_E2016GVM) 


# check that levels are the same between pre and post vote questiion
levs_pre <- attr(cc16$intent_pres_16, "labels")
labs_pre <- levels(as_factor(cc16$intent_pres_16))
labs_ev <- levels(as_factor(cc16$ev_pres_16))

stopifnot(
  which(labs_pre == "I won't vote in this election") == which(labs_ev == "I didn't vote in this election") &
identical(setdiff(labs_pre, "I won't vote in this election"), setdiff(labs_ev, "I didn't vote in this election"))
)

pres_labels <- gsub("I won't vote in this election", "I didn't / won't vote in this election", labs_pre)

cc16 <- cc16 %>%
  mutate(state = as.character(as_factor(inputstate)),
         intent_pres_16 = replace(intent_pres_16, intent_pres_16 == 9, NA), # make Not Asked NA for coalesce to work
         ev_pres_16 = replace(ev_pres_16, ev_pres_16 == 9, NA),
         combined_pres_pre = coalesce(zap_labels(intent_pres_16), zap_labels(ev_pres_16)),
         vote_hrc_pre = as.numeric(combined_pres_pre == 2),
         vote_djt_pre = as.numeric(combined_pres_pre == 1),
         vote_und_pre = as.numeric(combined_pres_pre == 7),
         vote_hrc_post = as.numeric(voted_pres_16 == 2),
         vote_djt_post = as.numeric(voted_pres_16 == 1),
         turnout_wgt = case_when(intent_turnout == 3 ~ 1.0,
                                 intent_turnout == 1 ~ 0.9,
                                 intent_turnout == 2 ~ 0.3,
                                 intent_turnout == 5 ~ 0.1,
                                 intent_turnout %in% c(4,8,9, NA) ~ 0),
         post_turnout =  as.numeric(post_turnout == 5),
         vv_turnout = as.numeric(vv_16 != ""))  %>%
  select(caseID, commonweight, commonweight_post, tookpost, state, pid3, race,
         matches("vote_"), combined_pres_pre, voted_pres_16,
         intent_turnout, post_turnout, turnout_wgt, vv_turnout)

# add labels
cc16 <- cc16 %>% 
  mutate(combined_pres_pre = factor(combined_pres_pre, levels = levs_pre, labels = pres_labels))



saveRDS(cc16, "data/input/cces2016_slim.Rds")
write_dta(cc16, "data/input/cces2016_slim.dta")
