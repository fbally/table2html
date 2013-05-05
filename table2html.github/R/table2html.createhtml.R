# table2html: creates the whole html table
# 
# Author: fbally
###############################################################################





table2html.createHTML <- function( table.outline ) {

	library(XML)
	
	xmlTable <- newXMLNode("div", attrs = c(class="rTableDiv"))
	ttable <- newXMLNode("table", attrs = c(class="rTable"))
	if( ! is.null(table.outline$caption) ) {
		tcaption <- newXMLNode("caption",table.outline$caption)
		addChildren( ttable, kids=list(tcaption))
	}
	

	thead <- newXMLNode("thead")
	tbody <- newXMLNode("tbody")
	
	trList <- list()
	trList <- append(trList, table2html.header.1( table.outline ))
	trList <- append(trList, table2html.header.2( table.outline ))
	addChildren( thead, kids=trList)
	
	trList <- list()
	trList <- append(trList, table2html.rows( table.outline ))
	addChildren( tbody, kids=trList)
	addChildren( ttable, kids=list(thead,tbody))
	addChildren( xmlTable, kids=list(ttable))
	
	xmlTable
}
