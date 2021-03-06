% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntwk_info.R
\name{ntwk_info_ig}
\alias{ntwk_info_ig}
\title{Return information about an iGraph network object}
\usage{
ntwk_info_ig(ntwk_ig, loops_p = FALSE)
}
\arguments{
\item{ntwk_ig}{the iGraph network object}

\item{loops_p}{value for igraph::degree \code{loops} parameter}
}
\value{
a list of network information
}
\description{
Return information about an iGraph network object
}
\examples{
\dontrun{
x <- ntwk_info_ig(flomarr.ig)
flo_dir <- x$is_directed
}
}
