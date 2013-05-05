# TODO: Add comment
# 
# Author: fbally
###############################################################################


tdata1 <- array(data=c(1:24), dim=c(2,3,4), dimnames=c("v1","v2","v3"))

data.folder <- "C:/Documents and Settings/fbally/git/table2html/table2html/data/"
save(tdata1,file = paste(data.folder,"test.RData",sep="/"))
