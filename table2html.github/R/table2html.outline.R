# table2html: consructs the table outline (description)
# 
# Author: fbally
###############################################################################


table2html.outlineTable <- function(
		x,
		rows=NULL,
		cols=NULL,
		sub.totals=NULL,
		pct=NULL,
		fun=length,
		pct.signif=2,
		total.label="Total",
		display.zeros=FALSE,
		hide.empty=FALSE,
		table.size=1024,
		caption=NULL
) {
	#
	# table outline
	#
	dimensions<-dim(x)
	
	# tableoutline
	tOutline <- list()
	tOutline$table <- x
	tOutline$dimensions <- dimensions

	# find or attribute dimnames
	ndn <- names( dimnames(x))
	if( is.null(ndn) ) {
		ndn[1:length(dimensions)] <- NA
	}
	for ( i in 1:length(dimensions)) {
		if( is.na(ndn[i]) || ndn[i]=='' ){
			ndn[i] <- paste0('V',i)
		}
	}
	tOutline$names <- ndn
	tOutline$totals <- sub.totals
	tOutline$pct <- pct
	tOutline$total.label <- total.label
	if( ! is.null(sub.totals) ) {
		tOutline$total.sum <- sum(x)
	}
	if( ! is.null(pct) ) {
		tOutline$pct.signif <- pct.signif
	}

	tOutline$display.zero.label =" "
	if( is.logical( display.zeros ) ) {
		tOutline$display.zeros <- display.zeros
	} else if ( is.character( display.zeros ) )  {
		tOutline$display.zeros <- FALSE
		tOutline$display.zero.label <- display.zeros
	}
	
	# rows
	rowList <- list()
	rowList$nFields = length(rows)
	
	if( is.null(rows) ) {
		rowList$pos <- NA
		rowList$dimensions <- 0
		rowList$names <- c("")
	} else {
		rowList$pos <- rows
		rowList$dimensions <- dimensions[rows]
		rowList$names <- ndn[rows]
	}
	rowList$totals <- c()
	if( length(rows[rows %in% sub.totals])>0 ) rowList$totals <- rbind(rowList$totals , rows[rows %in% sub.totals ])
	rowList$fields <- list()
	
	# cols
	colList <- list()
	colList$nFields = length(cols)
	if( is.null(cols) ){
		colList$pos <- NA
		colList$dimensions <- 0
		colList$names <- c("")
	} else {
		colList$pos <- cols	
		colList$dimensions <- dimensions[cols]
		colList$names <- ndn[cols]
	}
	colList$totals <- c()
	if( length(cols[cols %in% sub.totals])>0 ) colList$totals <- rbind(colList$totals , cols[cols %in% sub.totals ])
	
	for( i in 1:length(dimensions) ) {
		fld <- list()
		fld$pos <- i
		fld$dimension <- dimensions[i]
		fld$name <- ndn[i]
		# fld$labels <- iconv(as.vector(dimnames(x)[[i]]),encFrom,encTo)
		fld$labels <- dimnames(x)[[i]]
		
		if( i %in% rows ) {
			fld$orientation <- "r"
			fld.pos <- match(i,rows)
			flds <- rows
		} else if( i %in% cols ) {
			fld$orientation <- "c"
			fld.pos <- match(i,cols)
			flds <- cols
		}
		
		fld$allDim <- prod(dimensions[flds][fld.pos:length(flds)])
		fld$span <- prod(dimensions[flds][fld.pos:length(flds)][-1])
		fld$repet <- prod(dimensions[flds])/(fld$span * fld$dimension)
		# fld$index <- ifelse(is.null(rowList$fields),1,length(rowList$fields)+1)
		fld$index <- match(fld$pos,flds)
		
		tOutline$fields[[i]] <- fld
		
	}
	rowList$fields <- tOutline$fields[ rows ]
	tOutline$rowList <- rowList
	colList$fields <- tOutline$fields[ cols ]
	tOutline$colList <- colList
	
	# to show or not empty rows
	tOutline$hide.empty <- hide.empty
	# rm(fld,rowList,colList)
	
	tOutline$caption = caption
	
	tOutline
}

