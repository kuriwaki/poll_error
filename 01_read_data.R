library(readr)
library(googlesheets)
suppressPackageStartupMessages(library("dplyr"))

# Denominators ----
# Michael McDonald's Tracker
# "https://docs.google.com/spreadsheets/d/1VAcF0eJ06y_8T4o2gvIL4YcyQy8pxb1zYkgXF76Uu1s/edit#gid=2030096602"
mm2016 <- gs_title("2016 November General Election")
mm_gs <- gs_read_csv(mm2016, skip = 1)

# rename and drop total
mm <- mm_gs %>% 
  rename(state = X1, 
         st = `State Abv`,
         vap = `Voting-Age Population (VAP)`,
         vep = `Voting-Eligible Population (VEP)`) %>% 
  filter(state != "United States") %>%
  select(state, st, vap, vep, everything())

# state color
# https://docs.google.com/spreadsheets/d/1-EU9o8bHdXKudYIGUAj00rGtDPumhwGW2bAGmPr9iUo/edit#gid=0
color <- gs_read(gs_title("2016_dw_color"))

# Final count ----
# David Wasserman's Tracker
# https://docs.google.com/spreadsheets/d/133Eb4qQmOxNvtesw2hdVns073R68EZx4SfCnP4IGQf8/edit#gid=19
dw2016 <- gs_title("2016 National Popular Vote Tracker")
dw_gs <- gs_read_csv(dw2016, skip = 4)

# rename and drop empty rows
dw <-  dw_gs %>% 
  rename(state = State,
         votes_hrc = `Clinton (D)`,
         votes_djt = `Trump (R)`,
         votes_oth = `Others`,
         tot_votes = `Total '16 Votes`) %>% 
  filter(!is.na(votes_hrc)) %>% 
  filter(!grepl("States|Total", state)) %>%
  mutate(state = gsub("\\*", "", state),
         pct_hrc_voters = votes_hrc / tot_votes,
         pct_djt_voters = votes_djt / tot_votes) %>%
  select(state, pct_hrc_voters, pct_djt_voters, votes_hrc, votes_djt, tot_votes, everything()) %>%
  inner_join(color)

# Poll prediction -----
# CCES Release by YouGov Numbers
# Copied from  https://cces.gov.harvard.edu/news/cces-pre-election-survey-2016
# https://docs.google.com/spreadsheets/d/1pJLEHfvCN-eX1mBfe6sgs0dwF2oq9G1FcUhKFk0Pe8g/edit#gid=0
yg2016 <- gs_title("20161107_CCES")
yg_gs <- gs_read(yg2016)

# rename
yg <- yg_gs %>% 
  rename(state = X1,
         yougov_hrc = Clinton,
         yougov_djt = Trump,
         yougov_johnson = Johnson,
         yougov_stein = Stein,
         yougov_n = `Sample Size`) %>% 
  filter(state != "U. S.") %>% 
  mutate(state = replace(state, state == "DC", "District of Columbia"),
         yougov_pct_hrc = yougov_hrc / 100,
         yougov_pct_djt = yougov_djt / 100,
         yougov_pct_johnson = yougov_johnson / 100,
         yougov_pct_stein = yougov_stein / 100)

# save -----
saveRDS(yg, "data/output/yg_release_state.rds")
saveRDS(dw, "data/output/dw_results_state.rds")
saveRDS(mm, "data/output/mm_popultn_state.rds")