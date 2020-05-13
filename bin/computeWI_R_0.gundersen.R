# run instantaneous R0 calculations per county from data pulled through API by Steve Wangen

library(incidence) ## incidence
library(earlyR)    ## get_R
library(EpiEstim)  ## estimate_R
library(ggplot2)

library(dplyr)
library(tidyverse)  ##rowname_to_column and column_to_rownames

defaultArgs <- list (
  mu    = 7.5,  ## serial interval  mean (days)
  sigma = 3.4,  ## serial interval SD (days)
  plotFile = NULL,
  outFile =  'WI_RO.csv',
  inFile = NULL,       ## by-pass download
  current = FALSE,     ## current R0 or all?
  includeTotal = NULL, ## Wisconsin total
  verbose = FALSE
)

args <- R.utils::commandArgs(trailingOnly = TRUE,
                             asValues = TRUE ,
                             defaults = defaultArgs)
#source("lib/estimate.R")
source("lib/estimate.gundersen.R")

results <- 
  calculate_r0(
    mu    = as.numeric(args$mu),      ## 7.5 serial interval  mean (days)
    sigma = as.numeric(args$sigma),   ## 3.4 serial interval SD (days)
    plotFile = args$plotFile,
    inFile = args$inFile,             ## by-pass download
    current = as.logical(args$current),   ## current R0 or all?
    includeTotal = args$includeTotal, ## Wisconsin total
    verbose = args$verbose
)

write.csv(results,args$outFile,quote=FALSE, row.names=FALSE)
warnings()
if (!is.null(args$plotFile)) {
  dev.off()
}
q()


#### to do
##  plots in a facet

