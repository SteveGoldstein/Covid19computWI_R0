## Initialize ------------------------------------
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

pdf("R0.pdf")
### Read and format data --------------------------------
#directly read from API - on 4_30_20, the cases were mistaken by deaths, API was corrected by SW
apiData <-read.csv("https://afidsi-covid19.s3.amazonaws.com/wi_county_data.csv")
cv1dd = apiData[,grep("_cases", names(apiData), value=TRUE)]

################################
cv1dd = t(cv1dd)
counties <- c(cv1dd["Admin2_cases",])

data <- cv1dd[tail(row.names(cv1dd),-10),]
storage.mode(data) <- "numeric"
#dim(data)
# 98 72
rownames(data)<-as.character(rownames(data))
rownames(data)<-gsub("X", "", rownames(data))
rownames(data)<-gsub("_cases", "", rownames(data))
rownames(data)<-gsub('\\.', "/", rownames(data))

covid <-  data.frame(data)
names(covid) <-  counties
rownames(covid)<-as.Date(rownames(covid),"%m/%d/%y")

##   aggregate data ---------------------

#############################
# aggregate cases by day

######### shelter in place dates #####################################
shelter_date = as.Date("3/25/2020","%m/%d/%Y")
last_date  = range(rownames(covid))[2]

######################################################################
covid$date <- rownames(covid)
cv1dd = covid

results = data.frame()
for (ind in seq(length(counties))){
  county = counties[ind]
  date_s = shelter_date
  vars<-c("date",county)
  
  ##############   initial date   ######################
  # first case
  ini_date = first(na.omit( cv1dd[vars] ))$date
  ######################################################
  ######Question: this omits ini_date;  is that right?
  cv2x<-cv1dd[which(cv1dd$date>ini_date & cv1dd$date<=last_date),] # data after school closure 
  cv2<-cv2x[vars]
  colnames(cv2) <- c("Date", "Count")
  
  ### for input including the date info (needed later)
  cv3<-cv2
  colnames(cv3) <- c("dates", "I")
  
  cv4<-cv3
  rownames(cv4) = seq(dim(cv4)[1])
  
  if (sum(cv4$I)==0) {
    print(paste(ind,"There are no cases in", county))
    results[ind,1] = county
    results[ind,2] = "there are no cases"
    results[ind,3] = "there are no cases"
    results[ind,4] = "there are no cases"
    next
  }
  
  ###################################################################
  dates_onset <- cv3$dates[
    unlist(lapply(1:nrow(cv4), 
                  function(i) rep(i, cv4$I[i])
    )
    )
    ]
  onset <-dates_onset
  ####################################################################
  
  
  ############## till end
  today <- as.Date(last_date)
  i <- incidence(onset, last_date = last_date)
  ################################################################################
  ################################################################################
  if ( length(as.vector(i$counts)) < 7){
    print(paste(ind,"In", county, "there are not enough data to compute R_0"))
    
    results[ind,1] = county
    results[ind,2] = "there are not enough data to compute R_0"
    results[ind,3] = "there are not enough data to compute R_0"
    results[ind,4] = "there are not enough data to compute R_0"
    
    next
  }
  
  #The function get_R is then used to estimate the most likely values of R:
  mu <- 7.5 # mean in days days
  sigma <- 3.4 # standard deviation in days
  
  res <- get_R(i, si_mean = mu, si_sd = sigma)
  #################################################################################
  si <- res$si
  #print(si)
  ######################################
  
  ########################################################################
  # calculate the instantaneous R0 over time using the most reasonable uncertainty distributions for the serial interval:
  res_before_during_after_closure <- estimate_R(as.vector(i$counts), 
                                                method="parametric_si",
                                                config = make_config(list(
                                                  # t_start = t_start,
                                                  #t_end = t_end,
                                                  mean_si = 7.5, 
                                                  std_si = 3.4))
  )
  
  # plot R0 over time by county  
  R0_plot <- 
    plot(res_before_during_after_closure, "R") +
    geom_hline(aes(yintercept = 1), color = "red", lty = 2) + 
    ggtitle(county)
  print(R0_plot)
  
  # all R0's over time in a table, including the quantiles, can construct 95% credibility interval from this
  R_R = res_before_during_after_closure$R
  R_val = R_R$`Mean(R)`[dim(R_R)[1]]
  R_CIhi = R_R$`Quantile.0.975(R)`[dim(R_R)[1]]
  R_CIlo = R_R$`Quantile.0.025(R)`[dim(R_R)[1]]
    
  print(paste(ind,"The current R_0 in ",county," is: ", round(R_val, digits = 3),
              "with 95% Credibility Interval (",round(R_CIlo, digits = 3),
              ",",round(R_CIhi, digits = 3),")"))
  
  results[ind,1] = county
  results[ind,2] = round(R_val, digits = 3)
  results[ind,3] = round(R_CIlo, digits = 3)
  results[ind,4] = round(R_CIhi, digits = 3)
  
}
names(results) = c("County","R_0","Quantile.0.025","Quantile.0.975")
results

warnings()
dev.off()
q()
