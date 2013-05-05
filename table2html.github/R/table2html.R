### table2html package
###
### Produces HTML coded tables from R tables, matrices, arrays and data.frames.
###
### Copyright 2012 Frank Bally <frank.bally@gmail.com>
###
### This file is part of the `table2html' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

table2html <- function (x, 
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
		output=c("print","character","xml","p","c","x")[[1]],
		caption=NULL
) {
	UseMethod("table2html")
}


table2html.default <- function (x, 
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
	table2html.table ( as.table(x),
			row.vars,
			col.vars,
			sub.totals,
			pct,
			fun,
			pct.signif,
			total.label,
			display.zeros,
			hide.empty,
			table.size,
			output,
			caption
	) 
}

table2html.table <- function (x, 
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

	# TODO implement hide.empty
	
	table.outline <- table2html.initialise(x,
			row.vars,
			col.vars,
			sub.totals,
			pct,
			fun,
			pct.signif,
			total.label,
			display.zeros,
			hide.empty,
			table.size,
			output,
			caption
	)

	xml <- table2html.createHTML( table.outline )
	if( output %in% c('print','p') ) {
		cat(saveXML(xml,prefix=NULL))
	} else if ( output %in% c('character','c') ) {
		saveXML(xml,prefix=NULL)
	} else if ( output %in% c('xml','x') ) {
		xml
	}
}

table2html.matrix <- function (x, 
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
	if( is.null(colnames(x)) ) colnames(testm) <- (LETTERS[1:length(x[1,])])
	if( is.null(rownames(x)) ) rownames(testm) <- (LETTERS[1:length(x[1,])])
	table2html(as.table(x),		
					row.vars,
					col.vars,
					sub.totals,
					pct,
					fun,
					pct.signif,
					total.label,
					display.zeros,
					hide.empty,
					table.size,
					output,
					caption )
}

table2html.array <- function (x, 
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
	table2html.table ( as.table(x),
			row.vars,
			col.vars,
			sub.totals,
			pct,
			fun,
			pct.signif,
			total.label,
			display.zeros,
			hide.empty,
			table.size,
			output,
			caption)
}

table2html.factor <- function (x, 
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
	table2html.table ( table(x),
			row.vars,
			col.vars,
			sub.totals,
			pct,
			fun,
			pct.signif,
			total.label,
			display.zeros,
			hide.empty,
			table.size,
			output,
			caption
	)
}

table2html.character <- function (x, 
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
	table2html(table(factor(x)),
			row.vars,
			col.vars,
			sub.totals,
			pct,
			fun,
			pct.signif,
			total.label,
			display.zeros,
			hide.empty,
			table.size,
			output,
			caption
	)
}

#table2html.formula <-function(x, fun=length, data=NULL, ... ) {
#	if( is.null(fun) ) {
#		fun=length;
#		warning( "No function given: length taken by default" );
#	}
#	if( is.null(data) ) {
#		stop( "No data" );
#	}
#	v <- all.vars(x)
#		
#	table2html(as.table(ftable(x , data=data )) ,
#			fun,
#			data
#	)
#}

table2html.data.frame <- function (x, 
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
	stop("For listing data.frame entries use data2html instead of table2html")
}

table2html.data.table <- function (x, 
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
	stop("For listing data.frame entries coerce data.table into data.frame and use data2html instead of table2html")
}