# table2html: Makes a line with column descriptors
# 
# Author: fbally
###############################################################################


table2html.header.2 <- function( table.outline ) {
	
	nRows <- length(table.outline$rowList$fields)
	nCols <- length(table.outline$colList$fields)
	nFields <- nRows+nCols
	pColDimensions <- prod(table.outline$colList$dimensions)
	pRowDimensions <- prod(table.outline$rowList$dimensions)
	
	
	# row 3: column header titles
	# write one title line for each column
	attrs <- c(class="rTableRowHeaderRow")
	tr <- newXMLNode("tr", attrs=attrs)
	
	# row 3.1: row header titles
	# write dim names
	tdList <- list()
	if( nRows > 0 ) {
		for( i in 1:nRows ) {
			attrs = c(class=paste0("rTableRowHeader",nRows-i+1) )
			label <- table.outline$rowList$names[i]
			# label <- paste0(label," (",table.outline$rowList$fields[[i]]$pos,")")
			tdList <- append( tdList, newXMLNode("td",label , attrs = attrs))
		}
	}
	# write dim names
	
	# row 3.2: row descriptor
	# write dim levels for last dimension
	for( tCol in 1:ifelse(nCols>0,pColDimensions,1) ) {
		attrs = c(class="rTableCellDescriptor")
		tdList <- append( tdList, newXMLNode("td","n" , attrs = attrs))
		# col totals
		if( nCols > 0 ) {
			for( i in c(nCols:1)) {
				fld <- table.outline$colList$fields[[i]] 
				print.total <-  fld$pos %in% table.outline$colList$totals && ((tCol %% fld$allDim)==0)
				if( print.total ) {
					attrs = c( class = "rTableCellDescriptor")
					tdList <- append( tdList, newXMLNode("td", "n" , attrs = attrs))
				}
			}
		} 
	}
	
	# add col totals cell descriptor
#	if( 1 %in% table.outline$totals ) {
#		attrs = c(class="rTableCellDescriptor")
#		tdList <- append( tdList, newXMLNode("td","n" , attrs = attrs))
#	}
	
	addChildren( tr, kids=tdList )
	tr
}

