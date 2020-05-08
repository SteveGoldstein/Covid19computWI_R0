# run instantaneous R0 calculations per county from data pulled through API by Steve Wangen

library(incidence)
library(earlyR)
library(EpiEstim)
library(projections)
library(epitools)
library(Epi)
library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)
library(data.table)

defaultArgs <- list (
  plotFile = NULL,
  outFile =  'results/2020-05-04/WI_RO_all.csv',
  inFile = NULL,
  current = FALSE,
  verbose = FALSE
)

args <- R.utils::commandArgs(trailingOnly = TRUE,
                             asValues = TRUE ,
                             defaults = defaultArgs)

if (! is.null(args$plotFile)) {
  pdf(args$plotFile)
}

if (is.null(args$inFile)) {
  dataSource <- "https://afidsi-covid19.s3.amazonaws.com/wi_county_data.csv"
} else {
  dataSource <- args$inFile
}

apiData <- read.csv(dataSource)
cv1dd <- apiData[,grep("_cases", names(apiData), value=TRUE)]
cv1dd <- t(cv1dd)
counties <- c(cv1dd["Admin2_cases",])

data <- cv1dd[tail(row.names(cv1dd),-10),]
storage.mode(data) <- "numeric"

rownames(data) <- as.character(rownames(data))
rownames(data) <- gsub("X", "", rownames(data))
rownames(data) <- gsub("_cases", "", rownames(data))
rownames(data) <- gsub('\\.', "/", rownames(data))

covid <-  data.frame(data)
names(covid) <-  counties
rownames(covid) <- as.Date(rownames(covid),"%m/%d/%y")
#############################
# aggregate cases by day

shelter_date <- as.Date("3/25/2020","%m/%d/%Y")
last_date <- range(rownames(covid))[2]

######################################################################

covid$date <- rownames(covid)
cv1dd <- covid

#write.csv(cv1dd,args$outFile,quote=FALSE, row.names=FALSE)
#q()

results <- data.frame()
for (ind in seq(length(counties))){
#for (ind in 7:8){
  county <- counties[ind]
  date_s <- shelter_date
  vars <- c("date",county)
  
  ##############   initial date   ######################
  # first case
  ini_date <- first(na.omit( cv1dd[vars] ))$date
  
  ######################################################
  
  cv2x <- cv1dd[which(cv1dd$date>ini_date & cv1dd$date<=last_date),] # data after school closure 
  cv2 <- cv2x[vars]
  colnames(cv2) <- c("Date", "Count")
  
  ### for input including the date info (needed later)
  cv3 <- cv2
  colnames(cv3) <- c("dates", "I")
  
  cv4 <- cv3
  rownames(cv4) <- seq(dim(cv4)[1])
  numCases <- sum(cv4$I)
  if (numCases == 0) {
    numDaysWithCases <- NA
    df <-  unname(data.frame(
      matrix(rep(NA,11),nrow=1),county,numCases,numDaysWithCases
    ))
    results[nrow(results)+1,1:length(df)] <- df
    next
  }
  
  ###################################################################
  dates_onset <- cv3$dates[
    unlist(lapply(1:nrow(cv4), 
                  function(i) rep(i, cv4$I[i]))
    )
    ]
  onset <-dates_onset
  ####################################################################
  ############## till end
  today <- as.Date(last_date)
  i <- incidence(onset, last_date = last_date)
  numDaysWithCases <- length(as.vector(i$counts))
  if ( numDaysWithCases <= 7){
    df <-  unname(data.frame(
      matrix(rep(NA,11),nrow=1),county,numCases,numDaysWithCases
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
      ggtitle(county)
    print(R0_plot)
  }
  
  # all R0's over time in a table, including the quantiles, can construct 95% credibility interval from this
  R_R = res_before_during_after_closure$R
  R_val = R_R$`Mean(R)`[dim(R_R)[1]]
  R_CIhi = R_R$`Quantile.0.975(R)`[dim(R_R)[1]]
  R_CIlo = R_R$`Quantile.0.025(R)`[dim(R_R)[1]]
  if (args$verbose) {
    print(paste(ind,"The current R_0 in ",county," is: ", round(R_val, digits = 3),
                "with 95% Credibility Interval (",round(R_CIlo, digits = 3),
                ",",round(R_CIhi, digits = 3),")")
    )
  }

  ## if first R0 calculation:  name the columns;
  if (!is.na(names(results)[1]) & names(R_R)[1] != names(results)[1]) {
    names(results) <- c(names(R_R),"county","numCases","numDaysWithCases")  
  }
  #R_R <- round(R_R,digits=3)
  R_R$county = rep(county,dim(R_R)[1])
  R_R$numCases = rep(numCases,dim(R_R)[1])
  R_R$numDaysWithCases = rep(numDaysWithCases,dim(R_R)[1])
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
##  select .95 and .25 columns
##  plots in a facet
##  one more thing.  dates in output

