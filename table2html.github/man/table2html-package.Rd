\name{table2html-package}
\alias{table2html-package}
\concept{HTML document creation}
\concept{HTML output}
\docType{package}
\title{
Creates HTML tables or xml objects out of R objects
}
\description{
A table created by \code{table}(), \code{ftable}() and other R functions is returned as a HTML table (<table>...</table>) or a XML object.
Multidimensional tables are flattened in the same way \code{ftable()} does. 
The HTML code is rich in css-styles allowing custom formatting. A simple utility function to create complete html file is
provided for convenience.
}
\details{
\tabular{ll}{
Package: \tab table2html\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2012-10-06\cr
Suggests: \tab knitr, markdown\cr
Depends: \tab XML,base\cr
License: \tab GPL (>= 2)\cr
LazyData: \tab yes\cr
Built: \tab R 2.15.2; ; 2013-03-18 07:21:03 UTC; windows\cr
}

Index:
\preformatted{
	table2html              Creates flat tables with (sub)totals and
	                        percentages in HTML code or as XML object
	data2html               Lists the contents of a data.frame in columns and rows and 
                             returns it as HTML code or a XML object
	html.document           Creates a (simple) HTML document
	get.css                 Gets the package's default css style definitions
}
}
\author{
Frank Bally

Maintainer: Frank Bally <frank.bally@gmail.com>
}
\references{
HTML code \href{http://www.w3.org}{www.w3.org}.

Package \link[knitr]{knitr} (\url{http://yihui.name/knitr/}).

Pandoc document converter \href{http://johnmacfarlane.net/pandoc/}{Pandoc}.

Package \link[markdown]{markdown}, function \link[markdown:markdownToHTML]{markdownToHTML}.

Package \link[XML]{xml}.
}
\keyword{ package }
\keyword{ classes & character }

\seealso{
}
\examples{
# a table with only defaults from the Titanic data
table1 <- table2html(Titanic)
# a listing of a data.frame
df <- data.frame( name=c("John","Alicia","Martin"), gender=c("male","female","male" ), age=c(24,25,54) )
table2 <- data2html(df,align=c("left","center","right"), output="character" )
# shortcut for alignment: align=c("l","c","r")

# save as html file in the working directory (see \code{\link{getwd}()}
html.document ( title="Titanic survival",
	html.body=c(table1,table2),
	file="titanic.html" )
}
