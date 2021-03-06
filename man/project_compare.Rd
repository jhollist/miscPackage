% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_compare.R
\name{project_compare}
\alias{project_compare}
\title{Compare projections}
\usage{
project_compare(insp, ...)
}
\arguments{
\item{insp}{An Spatial* object from the \code{sp} package.  Input CRS must
be defined}

\item{...}{spTransform arguments}
}
\description{
Different projections represent the distortions resulting from projection 
spatial data in different ways.  This results in variation of basic 
measurements that result solely from the projection chosen.  This function
compares the resultant area and/or length measurements that result from 
different projections.
}
