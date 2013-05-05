# TODO: Add comment
# 
# Author: fbally
###############################################################################

pkg <- "package:table2html"
detach(pkg, character.only = TRUE)


d<-"C:/Documents and Settings/fbally/git/table2html/table2html/R/"
#d <- ""
source( paste0(d,"data2html.R"), local=FALSE )
source( paste0(d,"table2html.createhtml.R"), local=FALSE )
source( paste0(d,"table2html.header.1.R"), local=FALSE )
source( paste0(d,"table2html.header.2.R"), local=FALSE )
source( paste0(d,"table2html.initialise.R"), local=FALSE )
source( paste0(d,"table2html.outline.R"), local=FALSE )
source( paste0(d,"table2html.R"), local=FALSE )
source( paste0(d,"table2html.rows.R"), local=FALSE )
source( paste0(d,"html.R"), local=FALSE )
source( paste0(d,"utils.R"), local=FALSE )

# install.packages("svUnit")
# install.packages("svGUI")
