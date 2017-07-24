
library(readr)
library(googlesheets)
library("googlesheets")
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
         pct_hrc = votes_hrc / tot_votes) %>%
  select(state, pct_hrc, votes_hrc, tot_votes, everything())


# Poll prediction -----
# CCES Release
# Copied from  https://cces.gov.harvard.edu/news/cces-pre-election-survey-2016
# https://docs.google.com/spreadsheets/d/1pJLEHfvCN-eX1mBfe6sgs0dwF2oq9G1FcUhKFk0Pe8g/edit#gid=0
cc2016 <- gs_title("20161107_CCES")
cc_gs <- gs_read(cc2016)


# rename
cc <- cc_gs %>% 
  rename(state = X1,
         cces_hrc = Clinton,
         cces_djt = Trump,
         cces_johnson = Johnson,
         cces_stein = Stein,
         cces_n = `Sample Size`) %>% 
  filter(state != "U. S.") %>% 
  mutate(state = replace(state, state == "DC", "District of Columbia"),
         cces_hrc = cces_hrc/100,
         cces_djt = cces_djt/100,
         cces_johnson = cces_johnson/100,
         cces_stein = cces_stein/100)





# Join ---
df <-  inner_join(mm, dw, by = "state") %>%
  inner_join(cc, by = "state") %>% 
  select(state, st, vap, vep, pct_hrc, votes_hrc, tot_votes, cces_hrc, cces_n, everything())



# Save
write_csv(df, "data/output/pres16_state.csv")



