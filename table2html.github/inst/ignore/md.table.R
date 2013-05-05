# TODO: Add comment
# 
# Author: fbally
###############################################################################

library(table2html)
f <- "C:/Documents and Settings/fbally/git/table2html/table2html/inst/ignore/test.xml"
xml.doc <- xmlParse(f)
xmlName(xml)
xmlAttrs(xml)
xmlGetAttr(xml,"class")
xmlValue(xml)
xmlToDataFrame	(xml.doc)
parse.xml(xml.doc)
	
parse.xml <- function(xml.doc) {
	xml.table <- xmlChildren(xml.doc)
	parts <- xmlChildren(xml.table[[1]])
	t.header <- parts[[1]]
	t.body <- parts[[2]]
	if(length( parts ) > 2 ){
		t.footer <- parts[[3]]
	}
	
	# get table dimensions
	rows <- c(xmlChildren(t.header),xmlChildren(t.body))
	n.rows <- length(rows)
	first.row <- xmlChildren(t.header)["tr"]
	tds <- xmlElementsByTagName(first.row[[1]],"td")
	n.cols <- length(tds)
	for ( i in 1:n.cols ) {
		n <- xmlGetAttr(tds[[i]],"colspan")
		n.cols <- ifelse(!is.null(n), n.cols+as.numeric(n)-1, as.numeric(n.cols))
	}

	
	for( row in 1:n.rows ) {
		r.span <- xmlGetAttr(rows[[row]],"rowspan")
		row.span <- ifelse(is.null(r.span),1,as.numeric(r.span))
		row.0 <- ""
		row.1 <- ""
		row.2 <- ""
		cols <- xmlChildren(rows[[row]])
		for( col in 1:length(cols) ) {
			c.span <- xmlGetAttr(cols[[col]],"colspan")
			col.span <- ifelse(is.null(c.span),1,as.numeric(c.span))
			
			if( row==1 ) {
				row.0 <- paste0(row.0,"+","----")
			}
			node.value <- xmlValue( xmlChildren(rows[[row]])[[col]])
			row.1 <- paste0(row.1,"|",node.value)
			row.2 <- paste0(row.2,"+","-")
		
			if(col==length(cols)){
				if( row.0 != '' ) {
					row.0 <- paste0(row.0,"+\r")
				}
				row.1 <- paste0(row.1,"|\r")
				row.2 <- paste0(row.2,"+\r")
			}
		}
		if( row.0 != '' ) {
			cat( row.0,sep="")
		}
		cat(row.1,row.2,sep="")
	}
}