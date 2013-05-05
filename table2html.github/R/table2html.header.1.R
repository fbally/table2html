# table2html: Makes the table first set of header rows
# 
# Author: fbally
###############################################################################


table2html.header.1 <- function( table.outline ) {
	#
	# table header
	#

	nRows <- length(table.outline$rowList$fields)
	nCols <- length(table.outline$colList$fields)
	nFields <- nRows+nCols
	pColDimensions <- prod(table.outline$colList$dimensions)
	pRowDimensions <- prod(table.outline$rowList$dimensions)
	
	#
	# 1st column headers with field names
	trList <- list()
	if( nCols > 0 ) {
		for ( i in 1:nCols ) {
			fld <- table.outline$colList$fields[[i]]
			
			# row 1: column header titles
			attrs <- c(class="rTableColHeaderRow")
			tr<- newXMLNode("tr", attrs = attrs)
			
			# cell 1.1: left upper cell
			tdList <- list()
			if( i==1 && nRows>0) {
				rowspan <- table.outline$colList$nFields*2	
				attrs <- c( class="rTableLeftTop", 
						colspan=nRows,
						rowspan=rowspan )
				tdList <- append( tdList, newXMLNode("td", attrs = attrs))
			}
			
			# cell 1.2: column header
			rClass <- paste0( "rTableColHeader",nCols-i+1)
			colspan <- prod(fld$allDim)
			
			# calculate colspan for subtotals
			tspans <- list()
			for(ii in length(table.outline$colList$fields):1) {
				f <- table.outline$colList$fields[[ii]]
				if( ii == length(table.outline$colList$fields) ) {
					tspans[[ii]] <- f$allDim
				} else {
					tspans[[ii]] <- tspans[[ii+1]] * f$dim
				}
				tspans[[ii]] <- tspans[[ii]] + ifelse(f$pos %in% table.outline$colList$totals,1,0)
			}
			
			# cell 1.2: column label
			for( ii in 1: (prod(table.outline$colList$dimensions)/(fld$allDim))) {
				# add td's
				attrs <- c( class=rClass,
						colspan=tspans[i] )
				label <- table.outline$colList$names[i]
				## label <- paste0(label," (",fld$pos,")")
				tdList <- append( tdList, newXMLNode("td", label, attrs = attrs))
				
			}
			
			
			# row 2: col labels
			addChildren( tr, kids=tdList )
			trList <-  append(trList, tr)
			rm(tdList)
			
			# cell 2.1: column header labels
			attrs <- c(class="rTableColHeaderRow")
			tr <- newXMLNode("tr", attrs = attrs)
			# empty cell for row fields
			tdList <- list()
			
			# one cell spanning over the number of dimensions of the columns fields
			tCol <- fld$pos
			
			# add labels
			for( ii in 1:fld$repet ) {
				for( iii in 1:fld$dimension ) {
					rClass <- paste0( "rTableColLabel",nCols-i+1)
					attrs <- c(class=rClass, colspan=ifelse(i==nCols,1,tspans[i+1]) )
					label <- fld$labels[iii]
					tdList <- append( tdList, newXMLNode("td", label, attrs = attrs))
					
				}
				# col totals
				# calculate rowspan
				rowspan <- 0
				if( nCols>1) {
					diff <- table.outline$colList$nFields - match(fld$pos, table.outline$colList$pos)+1
					rowspan <- rowspan+(2*diff)-1
				} else if (nCols==1) {
					rowspan <- 1
				}
				
				if( fld$pos %in% table.outline$colList$totals ) {
					rClass <- paste0( "rTableColTotal",nCols-i+1,"Label")
					attrs <- c(class=rClass, rowspan=rowspan)
					
					label <- gsub("^\\s+|\\s+$", "", paste(table.outline$total.label, fld$name))
					tdList <- append( tdList, newXMLNode("td", label, attrs = attrs))
				}
			}
			addChildren( tr, kids=tdList )
			rm(tdList)
			trList <- append(trList, tr)
		}
	}
	trList
}
