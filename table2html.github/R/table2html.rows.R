# table2html: makes the table rows
# 
# Author: fbally
###############################################################################



###############################################################################
#
###############################################################################
table2html.rows <- function( table.outline ) {
	
	###############################################################################
	#
	###############################################################################
	getValues<-function( fldDim ) {
		if( length(fldDim) != length(table.outline$dimensions) )
			stop(paste0("Incorrect number of dimensions: ", length(table.outline$dimensions)," in x, ", length(fldDim)," in fldDim."))
		v<-rep(0,length(table.outline$dimensions))
		v<-ifelse(is.na(fldDim) | fldDim<=0,"",fldDim)
		evalStr <- paste0("table.outline$table[", toString(v),"]")
		cValue <- eval(parse(text=evalStr))
		cValue
	}
	
	getPos<-function( step, span, dimension ) {
		x1 <- floor((step-1)/span)+1
		x2 <- floor((step-1)/(dimension*span))
		x3 <- x1-(x2*dimension)
		x3
	}
	
	set.row.pos <- function( vPos, row  ) {
		for( i in c(1:nRows)) {
			fld <- table.outline$rowList$fields[[i]]
			lPos <- getPos( row, fld$span, fld$dimension )
			vPos[fld$pos] <- lPos
		}
		vPos
	}
	
	
	set.col.pos <- function( vPos, col  ) {
		for( i in c(1:nCols)) {
			fld <- table.outline$colList$fields[[i]]
			lPos <- getPos( col, fld$span, fld$dimension )
			vPos[fld$pos] <- lPos
		}
		vPos
	}
	
	get.sums <- function( sum.flds, pos, flds ) {
		sums <- c()
		
		if ( 0 %in% sum.flds ) {
			sums <- c( sums, table.outline$total.sum )
		}
		for( sum.fld in sum.flds[ sum.flds>0 ] ) {
			sPos <- pos
			stopped <- FALSE
			for( ii in length( flds ):1 ) {
				fld <-  flds[[ii]]
				if( ! stopped ) {
					sPos[fld$pos] <- 0
					stopped <- ifelse( fld$pos==sum.fld, TRUE, stopped )
				}
			}
			sums <- c( sums, sum( getValues( sPos )))
		}
		sums
	}
	
	get.pct <- function( c, d ) {
		# format(c/d*100,digits=3)
		# d
		ifelse(d>0,sapply(c/d*100,function(x) format(signif(x,table.outline$pct.signif), scientific=FALSE)),"")
		
	}
	
	pct.format <- function(x, digits = 2, format = "f", ...)
	{
		paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
	}
	
	# finds the indices of the rows or columns to summarise, according to level
	# level: hierarchical level of row or column in its list
	# fldList: table.outline$rowList or table.outline$colList
	# inverse level (nFields-level+1) for rowList
	summarise.pos <- function( level, fldList ) {
		lowerLevel <- level
		upperLevel <- fldList$nFields
		summarise <- fldList$pos[lowerLevel:upperLevel]
		summarise
	}
	
	###############################################################################
	#
	###############################################################################
	row.labels <- function( tRow ) {
		# row labels
		tdList <- list()
		
		for( i in c(1:nRows)) {
			fld <- table.outline$rowList$fields[[i]]
		
			lPos <- getPos( tRow, fld$span, fld$dimension )		
			# cell label
			label <- fld$label[lPos]
			
			# calculate rowspan 
			# rowspan is number of rows, the remaining level rows make up to
			rowspan <- fld$span
			
			# add for subtotals
			if( length(table.outline$rowList$totals)>0 ) {	
				# startlevel: level from which on rowspan needs adaptation
				startlevel <- match(fld$pos,table.outline$rowList$pos,nomatch=fld$pos)
				lastpos <- startlevel
				if( startlevel < length(table.outline$rowList$pos ) ) {
					for( iii in (startlevel+1):length(table.outline$rowList$pos) ) {
						f <- table.outline$rowList$fields[[iii]]
						if( f$pos %in% table.outline$rowList$totals ) {
							lf <- table.outline$rowList$fields[[lastpos]]
							rowspan <- rowspan + floor(fld$span/(f$span*f$dimension))
						}
						lastpos <- f$pos
					}
				}
				
			}

# obsolete now
#			for( ii in table.outline$rowList$totals ) {
#				f <- table.outline$fields[[ii]]
#				if( ! started && f$pos==fld$pos) {
#					started <- TRUE
#				} else if ( started ) {
#					rowspan <- rowspan + fld$span/(f$span * f$dimension)
#				}
#			}
		
			# add number of remaining level field sum lines
			# rowspan <- rowspan + length(rows[rows %in% sums & rows>=i])
			printRow <- is.null(fld$span) || fld$span <= 1 || (tRow %% fld$span) == 1
			if( printRow) {
				attrs = c( class=paste0( "rTableRowLabel", nRows-i+1), 
						rowspan=rowspan)
				tdList <- append( tdList, newXMLNode("td",  label, attrs = attrs))
			}
		}
		tdList
	}
	
	###############################################################################
	#
	###############################################################################
	row.values <- function( tRow) {
		
		# values
		
		vPos <- array(0,length(table.outline$fields))
		# set col positions
		if( nRows > 0 ) {
			vPos <- set.row.pos( vPos, tRow )
		} 
		
		
		tdList <- list()
		for ( tCol in c(1:ifelse(nCols>0,pColDimensions,1) ) ) {
			if( nCols > 0 ) {
				vPos <- set.col.pos( vPos, tCol )
			} 
			
			cValue <- sum(getValues( vPos ))
			cContentList <- list()
			
			if( cValue > 0 ) {
				table.outline$row.empty <- FALSE
			}
			
			cContent <- ifelse( table.outline$display.zeros || cValue , cValue, table.outline$display.zero.label )
			# cContent <- paste0(cValue," (",gsub(" ","",toString(vPos)),")")
			
			sums <- c()
			classes <- c()
			if( length(table.outline$pct) && ( table.outline$display.zeros || cValue) ) {
				flds <- table.outline$pct[table.outline$pct %in% table.outline$rowList$pos]
				s <- get.sums(flds, vPos, table.outline$rowList$fields)
				classes <- c(classes, rep("rTableColPct",length(s)))
				sums <- c( sums, s)
				
				flds <- table.outline$pct[table.outline$pct %in% table.outline$colList$pos]
				s <- get.sums(flds, vPos, table.outline$colList$fields)
				classes <- c(classes, rep("rTableRowPct",length(s)))
				sums <- c( sums, s)
				
				if( 0 %in% table.outline$pct ) {
					sums <- c(sums,table.outline$total.sum )
					classes <- c(classes, "rTablePct")
				}
				pct.value <- get.pct(cValue, rev(sums))
				# pct.value <-paste0(cValue,"/", rev(sums))
			}
			
			
			cContentList <- append( cContentList, newXMLNode("p", cContent, attrs=c( class="rTableCount") ))
			if ( length(table.outline$pct) >0 ) {
				if( length(sums)>0 ) {
					for( i in 1:length(pct.value) ) {
						pct.v <- pct.value[[i]]
						dTxt <- ifelse(is.na(pct.v) ||is.nan(pct.v) || pct.v=='' || pct.v==0,
									ifelse( table.outline$display.zeros,"(0%)",paste0("(", table.outline$display.zero.label,")") ),
									paste0(" (",pct.v,"%)" ))
						cContentList <- append( cContentList, newXMLNode("p", dTxt, attrs=c( class=classes[[length(classes)-i+1]])))
					}
				} else {
					for( i in 1:length(table.outline$pct) ) {
						cContentList <- append( cContentList, newXMLNode("p", " ", attrs=c( class=classes[[length(classes)-i+1]])))
					}
				}
			}
			
			attrs <- c( class = "rTableCell")
			td <- newXMLNode("td", attrs = attrs)
			addChildren( td, kids=cContentList )
			tdList <- append( tdList, td)
			
			# row 3.3: col totals
			if( nCols > 0 ) {
				tdList <- append( tdList, col.totals( tRow, tCol ) )
			}
		}
		tdList
	}
	
	
	
	###############################################################################
	#
	###############################################################################
	col.totals <- function( tRow, tCol ) {
		tdList <- list()
				
		vPos <- array(0,length(table.outline$fields))
		if( nRows > 0 ) {
			vPos <- set.row.pos( vPos, tRow )
		}
		vPos <- set.col.pos( vPos, tCol )
		for( i in c(nCols:1)) {
			fld <- table.outline$colList$fields[[i]] 
			cValue <- sum(getValues( vPos ))
			print.total <-  fld$pos %in% table.outline$colList$totals && ((tCol %% fld$allDim)==0 )
			if( print.total ) {
				# level: inversed order of variable hierarchy, from 1 (innermost) to number of dimensions (outermost)	
				level <- i
				vPos[ summarise.pos( level, table.outline$colList ) ] <- 0
				
				vPos[fld$pos] <- 0
				cValue <- sum(getValues( vPos ))
				pct.value <- c()
				sums <- c()
				classes <- c()
				if( length(table.outline$pct) && ( table.outline$display.zeros || cValue) ) {
					
					flds <- table.outline$pct[table.outline$pct %in% table.outline$rowList$pos]
					s <- get.sums(flds, vPos, table.outline$rowList$fields)
					classes <- c(classes, rep("rTableColPct",length(s)))
					sums <- c( sums, s)
					
					flds <- table.outline$pct[table.outline$pct %in% table.outline$colList$pos]
					s <- get.sums(flds, vPos, table.outline$colList$fields)
					classes <- c(classes, rep("rTableRowPct",length(s)))
					sums <- c( sums, s)
					if( 0 %in% table.outline$pct ) {
						sums <- c(sums,table.outline$total.sum)
						classes <- c(classes, "rTablePct" )
					}
					pct.value <-  get.pct(cValue,rev(sums))
					# pct.value <-paste0(cValue,"/", rev(sums))
				}
				cContentList <- list()				
				cContent <- ifelse( table.outline$display.zeros || cValue , cValue, table.outline$display.zero.label )
				# cContent <- paste0( cContent, ' rt(',toString(vPos),')')
				
				cContentList <- append( cContentList, newXMLNode("p", cContent, attrs=c( class="rTableCount") ))
				if( length(pct.value)>0 ) {
					for( ii in 1:length(pct.value) ) {
						pct.v <- pct.value[[ii]]
						dTxt <- ifelse(is.na(pct.v) || is.nan(pct.v) || pct.v=='' || pct.v==0,
								ifelse( table.outline$display.zeros,"(0%)", paste0("(", table.outline$display.zero.label,")") ),
								paste0(" (",pct.v,"%)" ))
						cContentList <- append( cContentList, newXMLNode("p", dTxt, attrs=c( class=classes[[length(classes)-ii+1]])))
					}
				}
				
				
				attrs = c( class=paste0( "rTableColTotal",table.outline$colList$nFields-level+1 ,"Cell"))
				td <- newXMLNode("td", attrs = attrs)
				addChildren( td, kids=cContentList )
				tdList <- append( tdList, td)
			}
		}
		
		tdList
	}
	


	###############################################################################
	#
	###############################################################################
	
	row.totals<- function( tFld, ttRow ) {
		attrs <- c(class=paste0("rTableRowTotal",ifelse(is.null(tFld),1,nRows-tFld$index+1)))
		tr <- newXMLNode("tr", attrs = attrs)
		sPos <- array(0,length(table.outline$fields))
		tdList <- list()
		# level: inversed order of variable hierarchy, from 1 (innermost) to number of dimensions (outermost)	
		level <- 1
		if( ! is.null( tFld )) {
			level <- table.outline$rowList$nFields - tFld$index + 1
			attrs = c( 
					class=paste0( "rTableRowTotal",table.outline$rowList$nFields- tFld$index +1,"Label" ),
					colspan = table.outline$rowList$nFields -  tFld$index  + 1)
			label <- gsub("^\\s+|\\s+$", "", paste(table.outline$total.label,tFld$name))
			tdList <- append( tdList, newXMLNode("td",  label, attrs = attrs))
			# calc coordinates for sum calculation
			sPos <- set.row.pos( sPos, ttRow )
		}
		
		# vector of row dimensions to summarise
		summarise <- summarise.pos( table.outline$rowList$nFields - level + 1, table.outline$rowList )
		sPos[summarise] <- 0
		for ( tCol in c(1:ifelse(nCols>0,pColDimensions,1) ) ) {
			# coordinates of cell in columns
			if( nCols > 0 ) {
				sPos <- set.col.pos( sPos, tCol )
			} # else sPos remains unchanged			
			
			cValue <- sum(getValues( sPos ))
			
			pct.value <- c()
			sums <- c()
			classes <- c()
			if( length(table.outline$pct) && ( table.outline$display.zeros || cValue) ) {
				flds <- table.outline$pct[table.outline$pct %in% table.outline$rowList$pos]
				s <- get.sums(flds, sPos, table.outline$rowList$fields)
				classes <- c(classes, rep("rTableColPct",length(s)))
				sums <- c( sums, s)
				flds <- table.outline$pct[table.outline$pct %in% table.outline$colList$pos]
				s <- get.sums(flds, sPos, table.outline$colList$fields)
				classes <- c(classes, rep("rTableRowPct",length(s)))
				sums <- c( sums, s)
				if( 0 %in% table.outline$pct ) {
					sums <- c(sums,table.outline$total.sum)
					classes <- c(classes, "rTablePct" )
				}
				pct.value <- get.pct(cValue, rev(sums))
				# pct.value <-paste0(cValue,"/", rev(sums))
			}
		
		
			cContentList <- list()
			cContent <- ifelse( table.outline$display.zeros || cValue , cValue, table.outline$display.zero.label )
			# cContent <- paste( cValue, 'rt(',toString(sPos),')')
			cContentList <- append( cContentList, newXMLNode("p", cContent, attrs=c( class="rTableCount") ))
			
			if( length(pct.value)>0 ) {
				for( i in 1:length(pct.value) ) {
					pct.v <- pct.value[[i]]
					dTxt <- ifelse(is.na(pct.v) || is.nan(pct.v) || pct.v=='' || pct.v==0,
						ifelse( table.outline$display.zeros,"(0%)", paste0("(", table.outline$display.zero.label,")") ),
						paste0(" (",pct.v,"%)" ))
					cContentList <- append( cContentList, newXMLNode("p", dTxt, attrs=c( class=classes[[length(classes)-i+1]])))
				}
			}
			
			dTxt<-""

			if(nRows>0) {
				class <- paste0( "rTableRowTotal",level ,"Cell")
			} else {
				class <- "rTableCell"
			}
			attrs = c( class=class )
			td <- newXMLNode("td", attrs = attrs)
			addChildren( td, kids=cContentList )
			tdList <- append( tdList, td )
			
			#tdList <- append( tdList, newXMLNode("td",  cValue, attrs=attrs))
			
			# col totals
			if( nCols > 0 ) {
				for( i in c(nCols:1)) {
					fld <- table.outline$colList$fields[[i]] 
					print.total <-  fld$pos %in% table.outline$colList$totals && ((tCol %% fld$allDim)==0)
					if( print.total ) {
						
						# vector of col dimensions to summarise
						# summarise cols
						summarise <- summarise.pos( i, table.outline$colList )
						
						sPos[ summarise ] <- 0
						
						cValue <- sum(getValues( sPos ))
						
						
						pct.value <- c()
						sums <- c()
						classes <- c()
						
						#if( is.numeric(table.outline$pct) ) {
						if( length(table.outline$pct) && ( table.outline$display.zeros || cValue) ) {
							flds <- table.outline$pct[table.outline$pct %in% table.outline$rowList$pos]
							s <- get.sums(flds, sPos, table.outline$rowList$fields)
							classes <- c(classes, rep("rTableColPct",length(s)))
							sums <- c(sums, s)
							
							flds <- table.outline$pct[table.outline$pct %in% table.outline$colList$pos]
							s <- get.sums(flds, sPos, table.outline$colList$fields)
							classes <- c(classes, rep("rTableRowPct",length(s)))
							sums <- c(sums, s)
							
							if( 0 %in% table.outline$pct ) {
								sums <- c(sums,table.outline$total.sum)
								classes <- c(classes, "rTablePct" )
							}
							pct.value <- get.pct(cValue, rev(sums))
							# pct.value <-paste0(cValue,"/", rev(sums))
						}
			
						cContentList <- list()
						cContent <- ifelse( table.outline$display.zeros || cValue , cValue, table.outline$display.zero.label )
						# cContent <- paste0( cContent, ' rt(',toString(sPos),')')
						
						cContentList <- append( cContentList, newXMLNode("p", cContent, attrs=c( class="rTableCount") ))
						if( length(pct.value)>0 ) {
							for( ii in 1:length(pct.value) ) {
								attrs <- c( class="rTableColPct")
								pct.v <- pct.value[[ii]]
								dTxt <- ifelse(is.na(pct.v) || is.nan(pct.v) || pct.v=='' || pct.v==0,
										ifelse( table.outline$display.zeros,"(0%)", paste0("(", table.outline$display.zero.label,")") ),
										# ifelse( table.outline$display.zeros,"(0%)", "" ),
										paste0(" (",pct.v,"%)" ))
								cContentList <- append( cContentList, newXMLNode("p", dTxt, attrs=c(class=classes[[length(classes)-ii+1]])))
							}
						}
						
						
						l1 <- ifelse(is.null(tFld),1,nRows- tFld$index +1)
						l2 <- nCols-i+1
						
						attrs = c( class=paste0( "rTableRowColTotal",l1,l2,"Cell"))
						
						td <- newXMLNode("td", attrs = attrs)
						addChildren( td, kids=cContentList )
						tdList <- append( tdList, td )
					}
				}
			}
		}
		
		addChildren( tr, kids=tdList )
		tr
	}

	
	########################################
	# table rows
	########################################
	itStartsHere <- function() {} # just to find this place
	
	nRows <- length(table.outline$rowList$fields)
	nCols <- length(table.outline$colList$fields)
	nFields <- nRows+nCols
	pColDimensions <- prod(table.outline$colList$dimensions)
	pRowDimensions <- prod(table.outline$rowList$dimensions)
	
	# row 3: table rows
	trList <- list()
	if( nRows > 0 ) {
		for( tRow in c(1:pRowDimensions) ) {
			# line
			
			attrs <- c(class="rTableCellRow")
			tr <- newXMLNode("tr", attrs = attrs)
			
			# row 3.1: row labels
			tdList <- row.labels( tRow )
			
			# row 3.2: values
			table.outline$row.empty <- TRUE
			tdList <- append( tdList, row.values( tRow) )
			addChildren( tr, kids=tdList )
			
			# row 3.4: row totals
			cPos <- array(0,length(table.outline$fields))
			
			trList <- append( trList, tr)
			
			# 
			for( i in c(table.outline$rowList$nFields:1)) {
				fld <- table.outline$rowList$fields[[i]]
				## lPos <- getPos( tRow, fld$span, fld$dimension )
				print.total <-  fld$pos %in% table.outline$rowList$totals && ((tRow %% fld$allDim)==0 )
				if( print.total ) {
					trL <- row.totals( fld, tRow )
					trList <- append( trList, trL )
				}
			}
		}
		
	} else {
		#only one row
		tr <- row.totals( NULL, 1 )
		trList <- append( trList, tr)
	}
	
	trList

}