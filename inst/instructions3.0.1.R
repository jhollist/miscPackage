#devtools on 3.0.1
#need older versions and set to local directory
setwd("C:/PROGRA~1/R/R-30~1.1/") #This may or may not work on your machine...

#get dependent packages
install.packages(c("httr","memoise","whisker","evaluate"))

#download and install older version of devtools
download.file("devtools_1.2.tar.gz")
install.packages("devtools_1.2.tar.gz",repos=NULL,type="source")

#install and run sys_perfrom
devtools::install_github("miscPackage","jhollist")
miscPackage::sys_perform(file="//aa.ad.epa.gov/ORD/ORD/DATA/Public/jhollist/R/epa_r_sys_performance.csv")

