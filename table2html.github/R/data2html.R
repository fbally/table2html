# data2html: creates a html table of a data.frame (listing)
# 
# Author: fbally
###############################################################################

data2html <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	UseMethod("data2html")
}

data2html.data.frame <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL) {
	if (!inherits(x, "data.frame"))
		stop("x must inherit from class `data.frame`'")
	
	# output
	if( is.null( output ) ) {
		output <- "p"
	}
	if( ! output %in% c("print","character","xml","p","c","x" ) ) {
		stop( paste("Invalid output format ",output) )
	}
	
	if( ! is.null(col.names) ) {
		if(! is.character(col.names)) {
			stop("Column names must be given as a character vector")
		}
		if( !is.null(col.names) && ! length(colnames(x)==length(col.names))){
			stop("Column names character vector length must equal to number of columns in data.frame")
		}
	}
	
	# rownames and line numbers are considered without difference
	row.labels <- NULL
	num.labels <- FALSE
	line.numbers <- ifelse(is.na(line.numbers),FALSE,line.numbers) 
	if( is.all.numeric(row.names(x)) ) {
		if( ( line.numbers ) ) {
			num.labels <- TRUE
			row.labels <- row.names(x)
		}  
	} else {
		row.labels <- row.names(x)
	}
	
	# print(x)
	library(XML)
	xmlTable <- newXMLNode("div", attrs = c(class="rTableDiv"))
	ttable <- newXMLNode("table", attrs = c(class="rTable"))
	if( ! is.null(caption) ) {
		tcaption <- newXMLNode("caption",toString(caption))
		addChildren( ttable, kids=list(tcaption))
	}
	
		tbody <- newXMLNode("tbody")	
	
	# column header row
	if( ! is.null(colnames(x))) {
		thead <- newXMLNode("thead")
		td.list <- list()
		if( ! is.null( row.labels ) ) {
			attrs <- c( class="rTableRowHeader1" )
			td <- newXMLNode("td", attrs = c(style="text-align:center") )
			td.list <- append(td.list,td)
		}
		
		for( n in 1:length(colnames(x))) {
			# label <- iconv(colnames(x)[n],from,to)
			cAlign <- get.alignment( align, n )
			attrs <- c( class="rTableColHeader1" )
			if( cAlign != "center" ) {
				attrs <- c( attrs, style=paste0("text-align:",cAlign) )
			}
			label <- ifelse(! is.null(col.names[n]),col.names[n],colnames(x)[n])
			td <- newXMLNode("td", label, attrs = attrs )
			td.list <- append(td.list,td)
		}
		tr <- newXMLNode("tr", attrs = c(class="rTableColHeaderRow"))
		addChildren( tr, kids=td.list)
		addChildren( thead, kids=list(tr))
		addChildren( ttable, kids=list(thead))
	} else {
		# put back colnames otherwise error further on 
		colnames(x) <- paste0("v",1:ncol(x))
	}
	if( ! is.null(align) ) {
		if( length(align) != length(x[1,]) ) {
			stop("number of column alignment instructions must be equal to number of columns")
		}
	}
	# 
	td.list <- list()
	no.rows <- nrow(x)
	if( from.head ) {
		l.start <- 1
		if ( no.rows > max.lines ) {
			l.stop <- max.lines
		} else {
			l.stop <- no.rows
		}
	} else {
		l.stop <- length(x[,1])
		if ( no.rows > max.lines ) {
			l.start <- no.rows - max.lines +1
		} else {
			l.start <- 1
		}
	}
	
	rAttrs <- c(class="rTableCellRow")
	
	
	tr.list <- list()
	for( r in l.start:l.stop ) {
		tr <- newXMLNode("tr", attrs = rAttrs)
		td.list <- list()
		if( ! is.null( row.labels ) ) {
			attrs = c( class="rTableRowHeader1")
			if( num.labels ) {
				attrs <- c( attrs, style="text-align:right" )
			} else {
				attrs <- c( attrs, style="text-align:left" )
			}
			td <- newXMLNode("td", row.names(x)[r], attrs = attrs )
			td.list <- append(td.list,td)
		}
		for (col in 1:ncol(x)) {
			
			cValue <- as.character(x[r,col])
			cAlign <- get.alignment( align, col )
			if( cAlign == "center" ) {
				cAttrs <- c( class="rTableCell" )
			} else {
				cAttrs <- c( class="rTableCell", style=paste0("text-align:",cAlign) )
			}
			# td <- newXMLNode("td", iconv(cValue,from,to) , attrs = cAttrs)
			td <- newXMLNode("td", cValue, attrs = cAttrs)
			td.list <- append(td.list,td)
		}
		addChildren( tr, kids=td.list)
		tr.list <- append(tr.list,tr)
	}
	addChildren( tbody, kids=tr.list)
	addChildren( ttable, kids=list(tbody))
	addChildren( xmlTable, kids=list(ttable))
	
	
	if( output %in% c('print','p') ) {
		cat(saveXML(xmlTable,prefix=NULL))
	} else if ( output %in% c('character','c') ) {
		saveXML(xmlTable,prefix=NULL)
	} else if ( output %in% c('xml','x') ) {
		xmlTable
	}
}

data2html.integer <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	
	df <- data.frame(x)
	if( ! is.null(names(x))) {
		row.names(df) <- names(x)
	}
	colnames(df) <- NULL
	line.numbers <- line.numbers || ! is.all.numeric(rownames(x))
	
	data2html.data.frame(df,
			max.lines,
			from.head,
			output,
			col.names,
			align,
			line.numbers,
			caption )
}

data2html.factor <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	
	df <- data.frame(x)
	if( ! is.null(names(x))) {
		row.names(df) <- names(x)
	}
	colnames(df) <- NULL
	line.numbers <- line.numbers || ! is.all.numeric(rownames(x))
	
	data2html.data.frame(df,
			max.lines,
			from.head,
			output,
			col.names,
			align,
			line.numbers,
			caption )
}

data2html.character <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	
	df <- data.frame(x)
	if( ! is.null(names(x))) {
		row.names(df) <- names(x)
	}
	colnames(df) <- NULL
	line.numbers <- line.numbers || ! is.all.numeric(rownames(x))
	
	data2html.data.frame(df,
			max.lines,
			from.head,
			output,
			col.names,
			align,
			line.numbers,
			caption )
}


data2html.table <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	stop("use table2html tables of ftables (has more functionality)")
}

#
#
#
data2html.summaryDefault <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	
	if (!inherits(x, "summaryDefault")) {
		stop("x must inherit from class `summaryDefault`'")
	}
	
	if(is.integer(x) ){
		
		data2html.integer(x,
				max.lines,
				from.head,
				output,
				col.names,
				align,
				line.numbers,
				caption )
	} else {
		df <- as.data.frame(matrix(x,nrow=1))
		colnames(df) <- names(x)
		if( ( is.null(row.names(x)) || is.all.numeric(row.names(x))) && ! line.numbers ) {
			row.names(df) <- NULL
		}
		data2html.data.frame(df,
				max.lines,
				from.head,
				output,
				col.names,
				align,
				line.numbers,
				caption )
	}
}

data2html.htest <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	df <- data.frame()
	# print(x)
	library(XML)
	if( ! is.null(caption) ) {
		tcaption <- newXMLNode("caption",toString(caption))
		addChildren( ttable, kids=list(tcaption))
	}
	
	tr.list <- c()
	# discarded items c("data.name")
	# c("method","parameter","statistic","p.value","conf.int","estimate","alternative")
	disgarded <- c("data.name")
	for(  item in names(x) ) {
		if( ! item %in% disgarded && !is.null(x[[item]]) && x[[item]] !='' ) {
			label <- ifelse( is.null(names(x[[item]])), item, toString(names(x[[item]]) ))
			td1 <- newXMLNode("td", label, attrs=c(class="rTableRowHeader1", htest=item))
			value <- x[[item]]
			if( is.numeric(value) ) {
				if(item=="p.value" && value<0.001 ) {
					value = "<0.001"
				} else {
					value <- signif(value,5)
				}
			}
			td2 <- newXMLNode("td", toString(value), attrs=c(class="rTableCell"))
			tr <- newXMLNode("tr")
			addChildren( tr, kids=list(td1,td2))
			tr.list <- append(tr.list, tr)
		}
	}

	if(length(warnings)>0) {
		td1 <- newXMLNode("td", "warnings", attrs=c(class="rTableRowHeader1", htest="warnings"))
		p.list <- list()
		for( w in warnings() ) {
			p <- newXMLNode("p", label, attrs=c(htest="warning"))
			p.list <- append(tr.list, tr)
		}
		
		td2 <- newXMLNode("td", toString(value), attrs=c(class="rTableCell"))
		addChildren( td2, kids=p.list)
	}
	tbody <- newXMLNode("tbody")
	addChildren( tbody, kids=tr.list)
	ttable <- newXMLNode("table", attrs = c(class="rTable"))
	addChildren( ttable, kids=list(tbody))
	xmlTable <- newXMLNode("div", attrs = c(class="rTableDiv"))
	addChildren( xmlTable, kids=list(ttable))


	if( output %in% c('print','p') ) {
		cat(saveXML(xmlTable,prefix=NULL))
	} else if ( output %in% c('character','c') ) {
		saveXML(xmlTable,prefix=NULL)
	} else if ( output %in% c('xml','x') ) {
		xmlTable
	}
	
}

data2html.default <- function(x,
		max.lines=100,
		from.head=TRUE,
		output=c("print", "character", "xml","p","c","x")[[1]],
		col.names=c(),
		align=NULL,
		line.numbers=FALSE,
		caption=NULL ) {
	
	if( class(x) %in% c("array","matrix") ) {
		stop("Try table2html instead of data2html" )
	} else if (class(x)=='htest'){
		data2html.htest(x,
				max.lines,
				from.head,
				output,
				col.names,
				align,
				line.numbers,
				caption)
	} else {
		data2html.data.frame(as.data.frame(x),
				max.lines,
				from.head,
				output,
				col.names,
				align,
				line.numbers,
				caption)
	}
}