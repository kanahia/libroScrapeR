% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_links}
\alias{get_links}
\title{get_links}
\usage{
get_links(html, core_link = "https://lubimyczytac.pl")
}
\arguments{
\item{html}{the result of rvest::read_html() of class xml_document xml_node}

\item{core_link}{string, core link to the website}
}
\value{
vector of links
}
\description{
get_links
}
\examples{

URL <- "https://lubimyczytac.pl/ksiegozbior/xc5GvparjG"
html <- rvest::read_html(x = URL)
links <- get_links(html = html)

}
