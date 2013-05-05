# table2html: Checks the arguments
# 
# Author: fbally
###############################################################################


table2html.initialise <- function( x,
		row.vars=NULL,
		col.vars=NULL,
		sub.totals=NULL,
		pct=NULL,
		fun=length,
		pct.signif=2,
		total.label="Total",
		display.zeros=FALSE,
		hide.empty=FALSE,
		table.size=1024,
		output=c("print","character","xml")[[1]],
		caption=NULL
) {
	#
	# initialise and check
	#
	if( ! is.table(x) ) {
		stop("Object of type table required")
	}
	dimensions<-dim(x)
	# check table size
	nDim <- length(dimensions)
	dp <- prod(dimensions)
	if( ! prod(dp) > 0 ) {
		stop( "Nothing to display")
	}
	if ( dp > .Machine$integer.max) 
		stop("attempt to make a table with >= 2^31 elements")
	if ( dp > table.size ) 
		stop(paste0("attempt to make a table with >= ",dp," elements: you can override this by given a higher value to  the table.size parameter (1024 by default)" ))
	rm(dp)
	
	
	
	# check fields attribution to rows and cols
	
	if( is.null(row.vars) && is.null(col.vars) ) {
		if(nDim==1) {
			rows.checked <- c(1)
			cols.checked <- c()
		} else {
			rows.checked <- c(1:(nDim-1))
			cols.checked <- c(nDim:nDim)
		}
	} else {
		if(length(row.vars)+length(col.vars) > nDim  ) {
			stop( paste0("Dimension mismatch: Table has ",nDim," dimension(s), row.vars and col.vars together have ",length(row.vars)+length(col.vars)," dimension(s)") )
		}
		if( ! is.null(row.vars) ) {
			if( is.character(row.vars) || is.factor(row.vars)) {
				if(is.null(names(dimnames(x)))) {
					stop( "Dimension names are not specified for x. Specify rows by their position or names.")
				}
	
				rows.checked <- match( row.vars, names(dimnames(x)))
				if( length(rows.checked[is.na(rows.checked)])>0) {
					stop( paste0("Row field \'",row.vars[is.na(rows.checked)]), "\' in: ",toString(names(dimnames(x))))
				}
			} else if( is.numeric(row.vars)) {
				rows.checked <- row.vars
			} else {
				stop( "row.vars must be either a numeric or a character vector")
			}
		} else {
			rows.checked <- c()
		}
		
		if( ! is.null(col.vars) ) {
			if( is.character(col.vars)) {
				if(is.null(names(dimnames(x)))) {
					stop( "Dimension names are not specified for x. Specify columns by their position or name.")
				}
				cols.checked <- match( col.vars, names(dimnames(x)))
				if( length(cols.checked[is.na(cols.checked)])>0) {
					stop( paste0("Column field \'",col.vars[is.na(cols.checked)]), "\' in: ",toString(names(dimnames(x))))
				}
			} else if( is.numeric(col.vars)) {
				cols.checked <- col.vars
			} else {
				stop( "col.vars must be either a numeric or a character vector")
			}
		} else {
			cols.checked <- c()
		}
	}
	# substitute missing rows or cols, but only if only one the other (cols or rows) is given
	if( (length(cols.checked)+length(rows.checked)) < nDim ) {
		if( is.null( row.vars ) ) {
			rows.checked <- (1:length(dimensions))[! (1:length(dimensions))  %in% cols.checked]
		} else if( is.null( col.vars ) ) {
			cols.checked <- (1:length(dimensions))[! (1:length(dimensions))  %in% rows.checked]
		} else {
			stop("Missing row(s) or column(s)")
		}
	}
	
	if( length(cols.checked[! cols.checked %in% 1:length(dimensions)])>0 ) {
		stop( "Row mismatch: one or more fields are outside of bounds")
	}
	if( length(rows.checked[! rows.checked %in% 1:length(dimensions)])>0 ) {
		stop( "Column mismatch: one or more fields are outside of bounds")
	}
	if( length(rows.checked[rows.checked %in% cols.checked])>0 || length(rows.checked[rows.checked %in% cols.checked])>0 ) {
		stop( "Row or column mismatch: one or more field(s) are in both columns and rows")
	}
	
	# sub.totals
	if( ! is.null( sub.totals ) ) {
		if( length(sub.totals[ ! sub.totals %in% rows.checked & ! sub.totals %in% sub.totals ])>0 ) {
			stop("Invalid identification of row or column fields to sum up: non existing rows or columns")
		}
	}
	if( ! is.null(sub.totals) ) {
		if( is.character(sub.totals)) {
			if(is.null(names(dimnames(x)))) {
				stop( "Dimension names are not specified for x. Specify rows by their position.")
			}
			sub.totals.checked <- match( sub.totals, names(dimnames(x)))
			if( length(sub.totals[is.na(sub.totals.checked)])>0) {
				stop( paste0("Column field \'",sub.totals[is.na(sub.totals.checked)]), "\' in: ",toString(names(dimnames(x))))
			}
		} else if( is.numeric(sub.totals)) {
			sub.totals.checked <- sub.totals
		} else {
			stop( "sub.totals must be either a numeric or a character vector")
		}
	} else {
		sub.totals.checked <- c()
	}
	
	# pct
	if( length( pct[pct>0] ) >0 ) {
		if ( ! pct[pct>0] %in% rows.checked && ! pct[pct>0] %in% cols.checked ) {
			paste(stop("Invalid pct option containing not existing row or columns:", toString( pct ) ))
		}
	}
	if( ! is.numeric(pct.signif) || pct.signif<1 || pct.signif>22 ) {
		warning("Invalid percentage significant figures. Must be within 1 and 22 figures. Set to 2")
		pct.signif <- 2
	}
	if( ! is.null(pct) ) {
		if( is.character(pct)) {
			if(is.null(names(dimnames(x)))) {
				stop( "Dimension names are not specified for x. Specify rows by their position.")
			}
			pct.checked <- match( pct, names(dimnames(x)))
			if( length(pct.checked[is.na(pct.checked)])>0) {
				stop( paste0("Column field \'",pct[is.na(pct.checked)]), "\' in: ",toString(names(dimnames(x))))
			}
		} else if( is.numeric(pct)) {
			pct.checked <- pct
		} else {
			stop( "pct must be either a numeric or a character vector")
		}
	} else {
		pct.checked <- c()
	}
	
	# caption
	t.caption <- as.character(caption[[1]])
	if( is.null(t.caption) ) {
		t.caption <- NULL
	} else if (length(toString( t.caption=='' ) ) ) {
		t.caption <- NULL
	}

	# output	
	if( ! output %in% c("print","character","xml","p","c","x" ) ) {
		stop( paste("Invalid output format ",output) )
	}
	tOutline <- table2html.outlineTable ( x, 
			rows.checked,
			cols.checked,
			sub.totals,
			pct,
			fun,
			pct.signif,
			total.label,
			display.zeros,
			hide.empty,
			table.size,
			t.caption
	)
	# rm(dimensions,rows,cols,row.sums,col.sums)
	tOutline
}

