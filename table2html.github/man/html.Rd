% file /table2html/man/data2html.Rd
\name{html}
\alias{get.css}
\alias{html.document}
\concept{HTML document creation}
\concept{HTML output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a HTML file
}
\description{
Utility function to create a simple but complete HTML document with CSS styles.
}
\usage{
html.document( html.body="<p>put content here</p>",
  title="HTML document",
  css="default",
  docHeaderEntries=c(),
  as.html5=FALSE,
  file=NULL )
get.css() 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 	 \item{html.body}{
	  		The documents content in HTML containing all possible tags such as \code{div},\code{p},etc.
	%%     ~~Describe \code{max.lines} here~~
	}
  	\item{title}{
	  		The codument's title.
	}
	\item{css}{
	  		a css style string (without the surrounding \code{<style>} tag
	}
	\item{docHeaderEntries}{
	  		Entries to put in the document header, either as string with tags or as \link{xml}
	}
	\item{as.html5}{
	  		If TRUE a HTML 5 formatted output is produced.  
	  		Otherwise the format is HTML 4.01 Transitional. 
	  		Defaults to FALSE.
	}
	\item{file}{
		Name of html file (should end in .htm or .html) 
	}
}
\details{
The function \code{html.document()} creates a HTML document.
The function \code{get.css()} gets this package's default css style definition (without surrounding \code{<style>} tags). It can replaced by a custom css definition.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
	A HTML document
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
Instead of using \code{html.document} it is suggested to create a R Markdown (.Rmd) file which is passed to 
\code{\link[knitr]{knitr}} resulting in a Markdown (.md.) file.  
This file then can then be passed to the function \code{\link[markdown:markdownToHTML]{markdown}} 
of package \code{\link[markdown]{markdown}} in order to generate a HTML document.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link[XML]{xml}}package for xml creation and manipulation.  
The \code{\link[knitr]{knitr}}package processes R Markdown (.Rmd) file to Markdown (.md) files.  
The \code{\link[markdown]{markdown}} package with its function \code{\link[markdown:markdownToHTML]{markdownToHTML}} or Pandoc\url{http://johnmacfarlane.net/pandoc/} can generate HTML output from .md files.  

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## document content (going into \code{body} tag)
html.body <- "<p>Hello World!</p>"
html.document ( title="Titanic survival",
	html.body=html.body,
	file="mydoc.html" )
## it seems the xml package does not consider valid xml tags 
##  containing numbers such as "<h1>"
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ classes & character }
