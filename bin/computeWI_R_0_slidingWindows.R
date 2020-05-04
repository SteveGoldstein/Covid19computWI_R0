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

cv1dd<-read.csv("https://afidsi-covid19.s3.amazonaws.com/wi_county_data.csv")

################################
cv1dd = cv1dd[,grep("_cases", names(cv1dd), value=TRUE)]
cv1dd = t(cv1dd)

counties = c(cv1dd["Admin2_cases",])

data = cv1dd[tail(row.names(cv1dd),-10),]
storage.mode(data) <- "numeric"

rownames(data)<-as.character(rownames(data))
rownames(data)<-gsub("X", "", rownames(data))
rownames(data)<-gsub("_cases", "", rownames(data))
rownames(data)<-gsub('\\.', "/", rownames(data))

covid = data.frame(data)
names(covid) = counties
rownames(covid)<-as.Date(rownames(covid),"%m/%d/%y")



#############################
# aggregate cases by day

shelter_date = as.Date("3/25/2020","%m/%d/%Y")
last_date  = range(rownames(covid))[2]

######################################################################

counties = names(covid)

length(counties)

covid$date <- rownames(covid)
cv1dd = covid


results = data.frame()
results_current = data.frame()

index = 1

for (ind in seq(length(counties))){
  county = counties[ind]
  date_s = shelter_date   
  vars<-c("date",county)
  
  ##############   initial date   ######################
  # first case
  ini_date = first(na.omit( cv1dd[vars] ))$date
  
  ######################################################
  
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

  if ( length(as.vector(i$counts)) <= 7){
    print(paste(ind,"In", county, "there are not enough data to compute R_0"))
    results_current[index,1:11] = rep("there are not enough data to compute R_0",11)
    results_current[index,12] = county
    index = index + 1
    next
    }
  
  
  #The function get_R is then used to estimate the most likely values of R:
  mu <- 7.5 # mean in days days
  sigma <- 3.4 # standard deviation in days
  
  res <- get_R(i, si_mean = mu, si_sd = sigma)
  si <- res$si
  
  ########################################################################
  # calculate the instantaneous R0 over time using the most reasonable uncertainty distributions for the serial interval:
     res_before_during_after_closure <- estimate_R(as.vector(i$counts), 
                                                 method="parametric_si",
                                                 config = make_config(list(
                                                   mean_si = 7.5, 
                                                   std_si = 3.4))
   )
   
 # plot R0 over time by county  
    plot(res_before_during_after_closure, "R") +
     geom_hline(aes(yintercept = 1), color = "red", lty = 2)
    
 # all R0's over time in a table, including the quantiles, can construct 95% credibility interval from this
   R_R = res_before_during_after_closure$R
   R_val = R_R$`Mean(R)`[dim(R_R)[1]]
   R_CIhi = rbind(c(R_R$`Quantile.0.975(R)`[dim(R_R)[1]]))
   R_CIlo = rbind(c(R_R$`Quantile.0.025(R)`[dim(R_R)[1]]))
   
   print(paste(ind,"The current R_0 in ",county," is: ", round(R_val, digits = 3),
         "with 95% Credibility Interval (",round(R_CIlo, digits = 3),
         ",",round(R_CIhi, digits = 3),")"))
   
   R_R$county = rep(county,dim(R_R)[1])
   results = rbind(results,R_R)
   
   #results[ind,1] = county
   #results[ind,2] = round(R_val, digits = 3)
   #results[ind,3] = round(R_CIlo, digits = 3)
   #results[ind,4] = round(R_CIhi, digits = 3)
   
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
