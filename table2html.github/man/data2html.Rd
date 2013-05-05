% file /table2html/man/data2html.Rd
\name{data2html}
\alias{data2html}
\alias{data2html.data.frame}
\alias{data2html.default}
\alias{data2html.summaryDefault}
\alias{data2html.table}
\alias{data2html.integer}
\alias{data2html.factor}
\alias{data2html.htest}
\concept{HTML document creation}
\concept{HTML output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a HTML table or xml object by listing a \code{data.frame} and ohter objects in columns and rows
}
\description{
Creates a HTML table or xml object by listing a \code{data.frame}, outputs
from \code{summary()}, \code{t.test()} and \code{kruskal.test()} and 
vectors such as integers, factors and characters in columns and rows.  
A title line with the column names is displayed followed by one line 
per record in the data.frame. Column names can be provided. Rows can be numbered or named.
}
\usage{
data2html(x,
    max.lines=100,
    from.head=TRUE,
    output=c("print", "character", "xml", "p", "c","x")[[1]],
    col.names=c(),
    align=NULL,
    line.numbers=FALSE,
    caption=NULL
    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{x}{
		A \code{data.frame} object (or one that can be coerced into a data.frame, such as \code{data.table})
	}
	\item{max.lines}{
		Maximum number of lines to be displayed. By default 100 lines.
	}
	\item{from.head}{
		If TRUE \code{max.lines} lines from beginning (head), otherwise 
		the last \code{max.lines} lines (tail) are displayed. By default TRUE.
	}
	\item{col.names}{
		Optionally column names can be provided in a character vector. The number of column names must 
		be equal to the number of columns in the \code{data.frame}
	}
	\item{line.numbers}{
  		If TRUE a column containing line numbers is added. Default is FALSE. The \code{data.frame} function \code{row.names()} allows
  		to pass a (non-numeric) character vector to label rows. In this case the \code{line.numbers} parameter will be ignored.
	}
	\item{align}{
		Output table column alignment: A character vector of cell alignment instructions, one per column: "l" for left, 
		"c" for centered, "r" for right.  By default centered.
	}
	\item{output}{
		Output format: "print" or "p" prints it to the console. This is default behaviour and useful when e.g. writing a R code chunk in a R Markdown (.Rmd) file;  
		The option "character" or "c" returns the table as a single entry character vector.  This is useful if the string is passed further on, such as to be written to a file.  
		"xml" or "x" returns it as xml of class \code{XMLInternalNode}.
	}
	\item{caption}{
		A string for the table caption (\code{<caption>} tag). Default is NULL. 
	}
}
\details{
This function creates a listing of a \code{data.frame} in HTML table code.  
It can be sent to console as well as be retrieved as character vector or xml 
of class \code{XMLInternalNode}.
In order to name the rows, the \code{rownames()} function can be used before passing the
object to \code{data2table()}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A table in HTML code as a single entry character vector or a xml object
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
Use \code{\link{table2html}} for creating a HTML table with a \code{\link{table}}, \code{\link{ftable}}, \code{\link{array}}, \code{\link{matrix}}
}
\examples{
## listing
df <- data.frame( 	
    name=c("John","Alicia","Martin"), 
    gender=c("male","female","male" ), 
    age=c(24,25,54)
)
table1 <- data2html(
    df,align=c("left","center","right"), 
    output="character", 
    col.names=c("Name","Gender","Age"),
    line.numbers=TRUE,
	caption="Table 1: data.frame listing")
## shortcut for the alignment argument is: align=c("l","c","r")
## shortcut for the output argument is: output="c"
## you must specify output="character" (or output="c") if the output is passed further on
## otherwise the output is dumped to the console and table1 will be NULL

# basic variable types and summaries
x<-1:10
table2a <- data2html(x, output="c",
	caption="Table 2a: Numeric listing") # numeric
table2b <- data2html(summary(x), output="c",
	caption="Table 2b: Summary of numeric vector")
x<-factor(c("a","a","b","c","c","c"))
table3a <- data2html(x, output="c",
	caption="Table 3a: Factor listing") # factor
table3b <- data2html(summary(x), output="c",
	caption="Table 3b: Summary of factorised vector")
x<-c("John","Alicia","Martin")
table4a <- data2html(x, output="c",
	caption="Table 4a: Character listing") # character
table4b <- data2html(summary(x), output="c",
	caption="Table 4b: Summary of character vector")


## for basic use, put it into a html document and save it in a file
html.document ( title="Data frame listing",
	html.body=c(table1,table2a,table2b,table3a,table3b,table4a,table4b),
	file="data2html.html" )



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ classes & character }
