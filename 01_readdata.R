

library(googlesheets)
library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))


?gs_read

# "https://docs.google.com/spreadsheets/d/1VAcF0eJ06y_8T4o2gvIL4YcyQy8pxb1zYkgXF76Uu1s/edit#gid=2030096602"
mm2016 <- gs_title("2016 November General Election")
st <- gs_read_csv(mm2016, skip = 1)
