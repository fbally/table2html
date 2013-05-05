# data2html and table2html: utility function for html creation
# 
# Author: fbally
###############################################################################

get.css<- function() {
	f <- system.file("www","custom.css", package="table2html", mustWork = T)
	con <- file(f, "r", blocking = FALSE)
	content <- readLines(con) 
	close(con)
	content
}

#html.body="<p>put content here</p>"
#str="<p>put content here</p>"
#title="HTML document"
#css="default"
#docHeaderEntries=c()
#file="myhtml.htm"


html.document <- function( html.body="<p>put content here</p>",
		title="HTML document",
		css="default",
		docHeaderEntries=c(),
		as.html5=FALSE,
		file=NULL){
	
	library(XML)
	if( is.null( html.body )) {
		stop("html.body is empty (NULL): if table2html or data2html is used to produce html be sure to specify output=\"c\".")
	}
	if( as.html5 ) {
		f <- system.file("www", "html5.html", package ="table2html")
	}else {
		f <- system.file("www", "html4.html", package ="table2html")
	}
	
	htmlDoc <- htmlParse(f)
	docHead <- getNodeSet(htmlDoc, "/html/head")[[1]]
	docBody <- getNodeSet(htmlDoc, "/html/body")[[1]]
	
	for(e in docHeaderEntries ) {
		if( length(intersect( class(e) ,c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" )))>0 ) {
			ch <- addChildren( docHead, kids=list(e))
		} else if( is.character(e) && isXMLString(e) ){
			tNode <- htmlParse( f, asText=T )
			ch <- addChildren( docHead, kids=list(tNode))
		}
	}
	if( css %in% c("default","f") ){
		style <- newXMLNode("style", get.css(), attrs=c(type="text/css") )
		ch <- addChildren( docHead, kids=list(style) )
	} else {
		for( st in css ) {
			style <- newXMLNode("style", st, attrs=c(type="text/css") )
			ch <- addChildren( docHead, kids=list(style))
		}
	}
	
	if( ! is.null(title) && title != '' ) {
		ch <- addChildren( docHead, kids=list(newXMLNode("title", title)))
	}
	
	# add content
	# options(warn=-1)
	
	if( length(intersect( class(html.body) ,c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" )))>0 ) {
		ch <- addChildren( docBody, kids=list( html.body ) )
	} else if ( is.character( html.body ) ) {
		for( str in html.body ) {
			if ( isXMLString( str ) ) {
				x <- xmlTreeParse( str, isHTML=F, asText=T, getDTD=F, useInternalNodes=T)
				ch <- suppressWarnings(addChildren( docBody, kids=list(x) ) )
			} else {
				x <- newXMLNode("p", paste0("Error: the string \"",str,"\" is not valid xml/html"), addFinalizer = FALSE)
				ch <- addChildren( docBody, kids=list(x) )
			}	
		}
	} else {
		x <- newXMLNode("p", paste0("Error: \"",e,"\" is not valid xml/html"), addFinalizer = FALSE)
		ch <- addChildren( docBody, kids=list(x) )
	}
	
	file <- ifelse(is.null(file),"",file)
	if( file != '' ) {
		out.file <- saveXML(htmlDoc, file)
	} else {
		htmlDoc
	}
}
