# Utility functions for internal use
# 
# Author: fbally
###############################################################################


get.alignment <- function( align, col ) {
	if( ! is.null(align) ) {
		if( length(align) == 0 ) {
			cAlign <- "center"
		} else if( align[col] %in% c( 'left',  'l' ) ) {
			cAlign <- "left"
		} else if ( align[col] %in% c( 'right', 'r' ) ) {
			cAlign <- "right"
		} else {
			# any other including align[col] %in% c( 'center', 'c' ) ) 
			cAlign <- "center"
		}
	} else {
		# default
		cAlign <- "center"
	}
	cAlign
}

is.all.numeric <- function(x) {
	if( is.null(x) ) {
		NA
	} else {
		nn <- suppressWarnings(as.numeric(x))
		length(nn[is.na(nn)])==0
	}
}