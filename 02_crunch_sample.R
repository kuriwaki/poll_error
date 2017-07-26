
library(crunch)



login()

ds <- loadDataset("CCES 2016 Common")


crtabs(~inputstate, ds, weight = NULL)

crtabs( ~ inputstate + (ds$presvote == "Hillary Clinton (Democrat)"), ds)

