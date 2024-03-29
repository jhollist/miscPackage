% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_perform_parallell.R
\name{sys_perform_parallel}
\alias{sys_perform_parallel}
\title{sys_perform}
\usage{
sys_perform_parallel(n = 10, file = NULL, num_core = 1)
}
\arguments{
\item{n}{numeric value indicating the number of times to run the test.  Default is 10}

\item{file}{Character string for file (and path) of output file to write results to}
}
\description{
This function returns the time it takes to run the following: a big matrix
multiplication, a loop without pre-allocated mem, a loop with 
pre-allocated memory, and writing a csv to file. As I am not a computer 
scientist, I have no idea if these test are meaningful.  Just things I do a 
lot and wanted to see if they work faster on different machines.
}
\examples{
\dontrun{
#Following example will write to a shared file on ORD_public
#Linux
#You will need sudo rights and mount ORD_Public, like:
# $sudo mkdir /mnt/ord_public
# $sudo mount //V2626UMCEC501.aa.ad.epa.gov/ORD_Public$ -o username=aa\\YOURUSERNAME,
#            sec=ntlmsspi,uid=YOURUSERNAME /mnt/ord_public

#Linux Example then is
sys_perform(file='/mnt/ord_public/jhollist/R/epa_r_sys_performance.csv')

#Windows

#No need to map, the following should work
sys_perform(file='//aa.ad.epa.gov/ORD/ORD/DATA/Public/jhollist/R/epa_r_sys_performance.csv')
}
}
