# run instantaneous R0 calculations per county from data pulled through API by Steve Wangen

library(incidence) ## incidence
library(earlyR)    ## get_R
library(EpiEstim)  ## estimate_R
library(ggplot2)

library(dplyr)
library(tidyverse)  ##rowname_to_column and column_to_rownames

defaultArgs <- list (
  plotFile = NULL,
  outFile =  'WI_RO.csv',
  inFile = NULL,       ## by-pass download
  current = FALSE,     ## current R0 or all?
  includeState = TRUE, ## Wisconsin total
  verbose = FALSE
)

args <- R.utils::commandArgs(trailingOnly = TRUE,
                             asValues = TRUE ,
                             defaults = defaultArgs)

includeState <- as.logical(args$includeState)
if (! is.null(args$plotFile)) {
  pdf(args$plotFile)
}

if (is.null(args$inFile)) {
  dataSource <- "https://afidsi-covid19.s3.amazonaws.com/wi_county_data.csv"
} else {
  dataSource <- args$inFile
}

apiData <- read.csv(dataSource,stringsAsFactors = FALSE)

cv1dd <- apiData %>% 
  select(grep("_cases",names(.), value=TRUE)) %>% 
  column_to_rownames(var="Admin2_cases") %>% 
  select(tail(names(.),-9))
counties <- make.names(row.names(cv1dd))

names(cv1dd) <- names(cv1dd) %>% 
  gsub("X","", .) %>% 
  gsub("_cases","",.) %>% 
  gsub("\\.", "/", .) %>% 
  as.Date("%m/%d/%y")

cv1dd <-  data.frame(t(cv1dd)) %>% 
  rownames_to_column(var = "date")

if (includeState) {
  cv1dd <- cv1dd %>% 
    mutate(Wisconsin=rowSums(.[,-which(names(.) =="date")]))
  counties <- c(counties,"Wisconsin")
}
#write.csv(cv1dd,args$outFile,quote=FALSE, row.names=FALSE)
#q()

results <- data.frame()
for (ind in seq(length(counties))){
  county <- counties[ind]
  countyName <- county %>% 
    gsub("\\.([^.])", " \\1",., perl=TRUE )
  vars <- c("date",county)
  
  ##############   initial date   ######################
  # first case and last day with counts
  dateEndpts <- as.Date(range(cv1dd[vars]$date))
  ini_date  <- dateEndpts[1]
  last_date <- dateEndpts[2]
  
  cvCounty <- cv1dd %>% 
    filter(date > ini_date & date <= last_date) %>% 
    select(all_of(vars))
  names(cvCounty) <- c("dates", "I")
  rownames(cvCounty) <- seq(nrow(cvCounty))

  numCases <- sum(cvCounty$I)
  if (numCases == 0) {
    numDaysWithCases <- NA
    df <-  unname(data.frame(
      matrix(rep(NA,11),nrow=1),county,numCases,numDaysWithCases,
      stringsAsFactors = FALSE
    ))
    results[nrow(results)+1,1:length(df)] <- df
    next
  }
  ###################################################################
  onset <- cvCounty$dates[
    unlist(lapply(1:nrow(cvCounty), 
                  function(i) rep(i, cvCounty$I[i]))
    )
    ]
  
  ####################################################################
  ############## till end
  i <- incidence(onset, last_date = last_date)
  numDaysWithCases <- length(as.vector(i$counts))
  if ( numDaysWithCases <= 7){
    df <-  unname(data.frame(
      matrix(rep(NA,11),nrow=1),county,numCases,numDaysWithCases,
      stringsAsFactors = FALSE
    ))
    results[nrow(results)+1,1:length(df)] <- df
    next
  }
  #The function get_R is then used to estimate the most likely values of R:
  mu <- 7.5 # mean in days days
  sigma <- 3.4 # standard deviation in days
  
  if (args$verbose) {
    res <- get_R(i, si_mean = mu, si_sd = sigma)
    si <- res$si
    print(si)
  }
  ########################################################################
  # calculate the instantaneous R0 over time using the most reasonable uncertainty distributions for the serial interval:
  res_before_during_after_closure <- 
    estimate_R(as.vector(i$counts), 
               method="parametric_si",
               config = make_config(list(
                 mean_si = mu, 
                 std_si = sigma)
               )
    )
  # plot R0 over time by county  
  if (!is.null(args$plotFile)) {
    R0_plot <- 
      plot(res_before_during_after_closure, "R") +
      geom_hline(aes(yintercept = 1), color = "red", lty = 2) +
      ggtitle(countyName)
    print(R0_plot)
  }
  
  # all R0's over time in a table, including the quantiles, can construct 95% credibility interval from this
  R_R = res_before_during_after_closure$R
  R_val = R_R$`Mean(R)`[dim(R_R)[1]]
  R_CIhi = R_R$`Quantile.0.95(R)`[dim(R_R)[1]]
  R_CIlo = R_R$`Quantile.0.05(R)`[dim(R_R)[1]]
  if (args$verbose) {
    print(paste(ind,"The current R_0 in ",countyName," is: ", round(R_val, digits = 3),
                "with 95% Credibility Interval (",round(R_CIlo, digits = 3),
                ",",round(R_CIhi, digits = 3),")")
    )
  }
  ## if first R0 calculation:  name the columns;
  if (!is.na(names(results)[1]) & names(R_R)[1] != names(results)[1]) {
    names(results) <- c(names(R_R),"county","numCases","numDaysWithCases")  
  }
  R_R <- round(R_R,digits=3)
  R_R$county = rep(countyName,dim(R_R)[1])
  R_R$numCases = rep(numCases,dim(R_R)[1])
  R_R$numDaysWithCases = rep(numDaysWithCases,dim(R_R)[1])
  
  ## change interval indices to dates;
  R_R <- R_R %>% 
    mutate(t_start = i$dates[t_start]) %>% 
    mutate(t_end =i$dates[t_end])
  
  if (as.logical(args$current)) {
    results = rbind(results,R_R[dim(R_R)[1],])
  } else {
    results = rbind(results,R_R)
  }
}

write.csv(results,args$outFile,quote=FALSE, row.names=FALSE)
warnings()
if (!is.null(args$plotFile)) {
  dev.off()
}
q()


#### to do
##  plots in a facet

