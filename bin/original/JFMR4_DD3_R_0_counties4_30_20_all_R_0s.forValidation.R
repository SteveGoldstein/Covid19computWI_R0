# run instantaneous R0 calculations per county from data pulled through API by Steve Wangen
#install.packages("epitools")
#install.packages("Epi")

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

#setwd("C:/Users/ddvd/Desktop/COVID19_SIR/DD_COVID_model_toolbox/Cori_Thompson_by_county")
#setwd("/Users/ddvd/Mac-PC Shared Folder/UWM/COVID19_SIR/JFMR/Cori_Thompson_by_county")
######################################
### read  data into the work space
#cv1dd<-read.csv("C:\\Users\\mandujanorey\\Documents\\COVID-19\\Graphs\\Nuevas\\WI_counties\\wi_county_data.csv",na.strings=".")

#cv1dd<-read.csv("C:/Users/ddvd/Desktop/COVID19_SIR/DD_COVID_model_toolbox/Cori_Thompson_by_county/wi_county_data_cases_deaths4_29_20.csv",na.strings=".")
#cv1ddx<-read.csv("C:/Users/ddvd/Desktop/COVID19_SIR/DD_COVID_model_toolbox/Cori_Thompson_by_county/wi_county_data_cases_deaths4_29_20.csv",na.strings=".")


#cv1dd<-read.csv("/Users/ddvd/Mac-PC Shared Folder/UWM/COVID19_SIR/JFMR/Cori_Thompson_by_county/wi_county_data_cases_deaths4_29_20.csv",na.strings=".")
#cv1ddx<-read.csv("/Users/ddvd/Mac-PC Shared Folder/UWM/COVID19_SIR/JFMR/Cori_Thompson_by_county/wi_county_data_cases_deaths4_29_20.csv",na.strings=".")

#directly read from API - on 4_30_20, the cases were mistaken by deaths, API was corrected by SW
cv1dd<-read.csv("https://afidsi-covid19.s3.amazonaws.com/wi_county_data.csv")

################################
cv1dd = cv1dd[,grep("_cases", names(cv1dd), value=TRUE)]
cv1dd = t(cv1dd)

counties = c(cv1dd["Admin2_cases",])

data = cv1dd[tail(row.names(cv1dd),-10),]
#dates = rownames(data)

storage.mode(data) <- "numeric"
dim(data)
# 98 72
rownames(data)<-as.character(rownames(data))
rownames(data)<-gsub("X", "", rownames(data))
rownames(data)<-gsub("_cases", "", rownames(data))
rownames(data)<-gsub('\\.', "/", rownames(data))

covid = data.frame(data)
names(covid) = counties
rownames(covid)<-as.Date(rownames(covid),"%m/%d/%y")



#############################
# aggregate cases by day

#str(covid)
#tail(covid)

######### shelter in place dates #####################################
#shelter_dates = read.csv("C:\\Users\\mandujanorey\\Documents\\COVID-19\\Graphs\\Nuevas\\stayathome_date.csv",na.strings=".")
#shelter_dates$state_name = gsub(" ", ".", shelter_dates$state_name, fixed = TRUE)
## format date:
#shelter_dates$effective_date<-as.character(shelter_dates$effective_date)
#shelter_dates$effective_date<-as.Date(shelter_dates$effective_date,"%m/%d/%Y")
#range(shelter_dates$effective_date)

#names(shelter_dates)

shelter_date = as.Date("3/25/2020","%m/%d/%Y")
last_date  = range(rownames(covid))[2]

######################################################################

counties = names(covid)

length(counties)


################
# Problems:
# Puerto Rico
################
#states = states[!states == "Puerto.Rico"]

counties



covid$date <- rownames(covid)

cv1dd = covid


results = data.frame()

results_current = data.frame()

index = 1

for (ind in seq(length(counties))){
  
  county = counties[ind]
  
  
  date_s = shelter_date #shelter_dates[shelter_dates$state_name == state,]$effective_date
  
  #date_s
  
  vars<-c("date",county)
  
  ##############   initial date   ######################
  # first case
  
  ini_date = first(na.omit( cv1dd[vars] ))$date
  
  #last_date = "2020-04-10"
  
  ######################################################
  
  cv2x<-cv1dd[which(cv1dd$date>ini_date & cv1dd$date<=last_date),] # data after school closure 
  
  #vars<-c("date",state)
  
  cv2<-cv2x[vars]
  colnames(cv2) <- c("Date", "Count")
  
  
  ### for input including the date info (needed later)
  cv3<-cv2
  colnames(cv3) <- c("dates", "I")
  
  cv4<-cv3
  #cv4
  rownames(cv4) = seq(dim(cv4)[1])
  
  if (sum(cv4$I)==0) {
    print(paste(ind,"There are no cases in", county))
    
    results_current[index,1:11] = rep("there are no cases in",11)
    results_current[index,12] = county
    
    index = index + 1
    
    next
    }
  
  ###################################################################
  dates_onset <- cv3$dates[unlist(lapply(1:nrow(cv4), function(i) 
    rep(i, cv4$I[i])))]
  
  onset <-dates_onset
  ####################################################################
  
  
  ############## till end
  today <- as.Date(last_date)
  i <- incidence(onset, last_date = last_date)
  i
  
  #plot(i)
  
  #str(onset)
  
  #
  #plot(i, border = "white")
  #
  
  ################################################################################
  ################################################################################
  ##  
  if ( length(as.vector(i$counts)) <= 7){
    print(paste(ind,"In", county, "there are not enough data to compute R_0"))
    
    results_current[index,1:11] = rep("there are not enough data to compute R_0",11)
    results_current[index,12] = county
    
    index = index + 1
    
    next
    }
  
  #t_start <- c(2, 13) # starting at 2 as conditional on the past observations, period 3-19 until 3-24 (schools closed)
  #t_end <- c(12, 20)  # starting 3-25 until 4-7-2020 (shelter in place)
 # res_before_during_after_closure <- estimate_R(as.vector(i$counts), 
 #                                               method="parametric_si",
 #                                               config = make_config(list(
 #                                                 #t_start = t_start,
 #                                                 #t_end = t_end,
 #                                                 mean_si = 7.5, 
 #                                                 std_si = 3.4))
 # )
  
  #plot(res_before_during_after_closure, "R") +
  #  geom_hline(aes(yintercept = 1), color = "red", lty = 2)
  
  #R_R = res_before_during_after_closure$R
  
  #R_val = R_R$`Mean(R)`[dim(R_R)[1]]
  
  #The function get_R is then used to estimate the most likely values of R:
  mu <- 7.5 # mean in days days
  sigma <- 3.4 # standard deviation in days
  
  res <- get_R(i, si_mean = mu, si_sd = sigma)
  #res
  
  #################################################################################
  
  si <- res$si
  si
  
  ######################################
  #### predicting difference between dates
  #n_days = 15
    #as.numeric(30-difftime(last_date ,date_s , units = c("days")))
  
  
  #future_i <- project(i, R = R_val, n_sim = 1000, si = res$si, n_days = n_days) #until 4-24-2020, the end of shelter in palce
  #future_i
  
 # print(paste(ind,"The R_0 in ",county," is: ", round(R_val, digits = 3)))
  
  #plot(future_i)
  
  #mat_future_i = as.data.frame(future_i)
  
 # t_start <- c(20, 21) # starting at 2 as conditional on the past observations, period 3-19 until 3-24 (schools closed)
  #t_end <- c(3, 7)  # starting 3-25 until 4-7-2020 (shelter in place)

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
    plot(res_before_during_after_closure, "R") +
     geom_hline(aes(yintercept = 1), color = "red", lty = 2)
    
 # all R0's over time in a table, including the quantiles, can construct 95% credibility interval from this
   R_R = res_before_during_after_closure$R
   #mea R0:
   R_val = R_R$`Mean(R)`[dim(R_R)[1]]
   #R_val_table = rbind(c(R_R$`Mean(R)`[dim(R_R)[1]],R_R$`Quantile.0.025(R)`[dim(R_R)[1]],R_R$`Quantile.0.975(R)`[dim(R_R)[1]]))
   #R_CI = rbind(c(R_R$`Quantile.0.025(R)`[dim(R_R)[1]],R_R$`Quantile.0.975(R)`[dim(R_R)[1]]))
   R_CIhi = rbind(c(R_R$`Quantile.0.975(R)`[dim(R_R)[1]]))
   R_CIlo = rbind(c(R_R$`Quantile.0.025(R)`[dim(R_R)[1]]))
   
   #print(paste(ind,"The R_0 in ",county," is: ", round(R_val, digits = 3),"95% Credibility interval is: ",round(R_CI, digits = 3)))
   print(paste(ind,"The current R_0 in ",county," is: ", round(R_val, digits = 3),
         "with 95% Credibility Interval (",round(R_CIlo, digits = 3),
         ",",round(R_CIhi, digits = 3),")"))
   
   
   R_R$county = rep(county,dim(R_R)[1])
   
   results = rbind(results,R_R)
   
   #results[ind,1] = county
   #results[ind,2] = round(R_val, digits = 3)
   #results[ind,3] = round(R_CIlo, digits = 3)
   #results[ind,4] = round(R_CIhi, digits = 3)
   
   #print(paste(ind,"The current R_0 in ",county," is: ", round(R_val, digits = 3),
   #            "with 95% Credibility Interval ranges from: ",round(R_CIlo, digits = 3),
   #            "to",round(R_CIhi, digits = 3)))
   
   # ##################   # ##################   # ##################   # ##################  # ##################  
   
   
   # ##################   # ##################   # ##################   # ##################   # ################## 
  # ################## predict cases using R0 from estimates using data til 3-29-2020 ) ################## 
  # today <- as.Date(date_s)
  # i <- incidence(onset, last_date = today)
  # i
  # 
  # #plot(i)
  # 
  # ######################################################################
  # 
  # #t_start <- c(2, 3) # starting at 2 as conditional on the past observations, period 3-19 until 3-24 (schools closed)
  # #t_end <- c(3, 7)  # starting 3-25 until 4-7-2020 (shelter in place)
  # res_before_during_after_closure <- estimate_R(as.vector(i$counts), 
  #                                               method="parametric_si",
  #                                               config = make_config(list(
  #                                                 #t_start = t_start,
  #                                                 #t_end = t_end,
  #                                                 mean_si = 7.5, 
  #                                                 std_si = 3.4))
  # )
  # 
  # 
  # # plot(res_before_during_after_closure, "R") +
  # #   geom_hline(aes(yintercept = 1), color = "red", lty = 2)
  # 
  # R_R = res_before_during_after_closure$R
  # 
  # R_val = R_R$`Mean(R)`[dim(R_R)[1]]
  # 
  # #The function get_R is then used to estimate the most likely values of R:
  # mu <- 7.5 # mean in days days
  # sigma <- 3.4 # standard deviation in days
  # 
  # res <- get_R(i, si_mean = mu, si_sd = sigma)
  # res
  # 
  # 
  # ######################################################################
  # 
  # si <- res$si
  # si
  # 
  # 
  # #R_val <- 2.54   #from instantaneous R0 estimation below, 2.73, 3.9
  # 
  # future_i30 <- project(i, R = R_val, n_sim = 1000, si = res$si, n_days = 30)
  # future_i30
  # 
  # #plot(future_i30)
  # 
  # mat_future_i30 = as.data.frame(future_i30)
  # 
  
  ############################### ************************************************ ###################
  ############################### ************************************************ ###################
  
  # read data sets into R
  #sa10 <- mat_future_i
  
  #sa30 <- mat_future_i30
  ############
  
  ############
  
  
  # with SIP starting 4-14, R0=0.76, n=1000 simulations
  #sa10$dates<-as.character(sa10$dates)
  #sa10$dates<-as.Date(sa10$dates,"%Y-%m-%d")
  
  #range(sa10$dates)
  #"2020-04-15" "2020-04-24"
  
  # no SIP, R0=2.59, projected until 4-24-2020
  #sa30$dates<-as.character(sa30$dates)
  #sa30$dates<-as.Date(sa30$dates,"%Y-%m-%d")
  
  #range(sa30$dates)
  #"2020-03-30" "2020-04-24"
  
  
  # make more simple data sets
  #sa10a<-sa10[,c(1:100)]
  #sa30a<-sa30[,c(1:100)]
  
  
  #mdf10a <- reshape2::melt(sa10a,id.vars="dates")
  
  #mdf30a <- reshape2::melt(sa30a,id.vars="dates")
  
  #visuals = mdf10a #rbind(mdf10a,mdf30a)
  
  ##################################################################
  ############################## JFMR ############################## 
  #options(scipen=999)
  
  #x11()
  # p1 = ggplot() +
  #   geom_point(visuals, mapping = aes( x=dates, y=value, colour=variable, group=variable, tittle=state ))+
  #   theme(legend.position = "none")+
  #   geom_point(data = cv4, mapping = aes(dates, I)) +
  #   stat_summary(fun = sum,geom = "point", col="blue")  + coord_trans(y="log10") +
  #   ggtitle(state) + ylab("Number of Confirmed Cases") + xlab("Dates") +
  #   scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  #   theme(text = element_text(size=20))#+ylim(5,10000)
  ############################## JFMR ############################## 
  ##################################################################
  #print(p1)
  
  #ggsave(paste(state,".png"))
  
  #pplots[[i]] = state
  #print(state, R_val)
}

dim(results)
dim(results_current)

names(results_current) = names(results)
 
# if you want all counties together:
results = rbind(results,results_current) # if not just drop this line
write.csv(results,"results.csv", quote=FALSE, row.names = FALSE)
write.csv(results_current,"results_current.csv", quote=FALSE, row.names = FALSE)
