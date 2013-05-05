\name{table2html}
\alias{table2html}
\alias{table2html.character}
\alias{table2html.data.frame}
\alias{table2html.data.table}
\alias{table2html.default}
\alias{table2html.factor}
\alias{table2html.matrix}
\alias{table2html.table}
\alias{table2html.array}
\alias{table2html.table}
\alias{tdata1}
\concept{HTML document creation}
\concept{HTML output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates flat tables with subtotals and percentages
%%  ~~function to do ... ~~
}
\description{
Creates a table in HTML format styled with css classes or xml object.  Takes a frequency table (\code{table()} 
or \code{ftable()}), as well as one- to multidimensional arrays or matrices.  Adds subtotals or percentages to 
specified rows and columns.  Typical (and suggested) use would be within R code blocks 
in R Markdown files processed by e.g. \link[knitr]{knitr} and passed on to 
e.g. \link[markdown:markdownToHTML]{markdownToHTML}() of package \link[markdown]{markdown} or the Pandoc document 
converter to produce HTML files.  This package allows to produce (simple) 
HTML documents on its own, with \code{html.document} by using the \link[XML]{xml} package.
}
\usage{
table2html(
  x, 
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
  )
}


%- maybe also 'usage' for other objects documented here.
\arguments{	
	\item{x}{
  		a table object (or one that can be coerced into table), a matrix or an array
	}
	\item{row.vars}{
  		Dimensions being displayed in rows. A vector of integers (indexes) or 
  		strings (names).  Must not overlap with col.vars. By default first dimension or, if col.vars not empty, remaining rows.
	}
	\item{col.vars}{
  		Dimensions being displayed in columns. A vector of integers (indexes) or 
  		strings (names).  Must not overlap with col.vars. By default al but first dimension or, if row.vars not empty, remaining colums.
	}
	\item{sub.totals}{
  		Dimensions having an added row or column displaing the total (sum) of the 
  		corresponding dimension.  A vector of integers (indexes) or strings (names of 
  		rows or columns).
	}
	\item{pct}{
  		Dimensions having percentages calculated. 0 is to be to be added for table totals.  
  		A vector of integers (indexes) or strings (names of rows or columns).
	}
	\item{fun}{
  		Function to generate cell content, not implemented, always \code{length}() (count) by default.
	}
	\item{pct.signif}{
  		Rounds numbers to significant numbers with the signif function
	}
	\item{total.label}{
  		String label to describe totals of row or columns. Default is 'Total'
	}
	\item{display.zeros}{
  		String to display of table celle equals to zero. Default is ''. 
	}
	\item{hide.empty}{
  		If TRUE empty rows or columns are not displayed. Not (yet) implemented. You can achieve the same by (re)-factoring the tables dimensions. 
	}
	\item{table.size}{
  		Maximum number of dimensions (table cells), defaults to 1024, can be set to any integer value with memory being the only restriction. Tables this large wouldn't make sense because difficult to read.
	} 
	\item{output}{
  		Output format: "print" or "p" prints it to the console.  This is default 
  		behaviour and useful when e.g. passing it to a markdown file;  "character" or "c" 
  		returns it as a single entry character vector.  This is useful if the string 
  		is passed further on, such as to be written to a file.  "xml" or "x" returns 
  		it as a xml node of class \code{XMLInternalNode} for further manipulation, 
  		e.g. formatting.  
	}
	\item{caption}{
		A string for table caption (\code{<caption>} tag). Default is NULL. 
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
	A table in HTML code with css class parameters formatting
}
\references{
HTML code \href{http://www.w3.org}{www.w3.org}.

Package \link[knitr]{knitr} (\url{http://yihui.name/knitr/}).

Pandoc document converter \href{http://johnmacfarlane.net/pandoc/}{Pandoc}.

Package \link[markdown]{markdown}, function \link[markdown:markdownToHTML]{markdownToHTML}.

Package \link[XML]{xml}.
}
\author{
Frank Bally
%%  ~~who you are~~
}
\note{
It is suggested to use \code{data2html} within R code blocks in a R Markdown (.Rmd). 
This file can be processed by the function \code{\link[knitr:knit]{knit}}of package \code{\link[knitr]{knitr}} 
to a Markdown (.md.) file. This file then can then be passed to the function \code{\link[markdown:markdownToHTML]{markdown}} 
of package \code{\link[markdown]{markdown}} or be processed with a \code{system()} call to Pandoc 
in order to generate a HTML document.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{table}} for multidimiensional tables, \code{\link{ftable}} for flat multidimiensional tables or 
	 \code{\link{data2html}} for for creating a HTML table out of a data.frame.
See \code{\link{html.document}} and \code{\link{get.css}} for creating a simple HTML file.
See \code{\link{signif}} for rounding.
The \code{\link[knitr]{knitr}}package processes R Markdown (.Rmd) file to Markdown (.md) files.
The \code{\link[markdown]{markdown}} package with its function \code{\link[markdown:markdownToHTML]{markdownToHTML}} or Pandoc\url{http://johnmacfarlane.net/pandoc/} can generate HTML output from .md files.

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# a table with only defaults; by specifying \code{output="c"} (or"character" the resulting html code is returned as a string which can be passed further on
table1 <- table2html(Titanic, output="c")
# different layout
table2 <- table2html(Titanic, row.vars=c(3,2), col.vars=c(4,1), output="c")
# totalling first colum and last row which comes to the same as the addmargins function for tables
table3 <- table2html(Titanic, sub.totals = c(1, 4), output="c")
# all row and coumn percentages
table4 <- table2html(Titanic, pct = c(1,2,3,4), output="c")
# save tables as html file 
html.document ( title="Titanic survival",
	html.body=c(table1,table2,table3,table4),
	file="titanic.html" ) # in the working directory (see \code{\link{getwd}()})


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ classes & character }
