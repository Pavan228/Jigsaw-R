library(dplyr)
library(caret)
library(irr)
library(ggplot2)
library(ggthemes)
library(gains)
options(scipen=999)

path ='E:\\Courses\\Jigsaw R\\JIGSAW\\DSR\\R Lab\\Data Science with R - files\\Data Science with R - files\\Assignments\\Graded Assignments\\Topic 13 - Final Case Study Course Wrap up\\'
filename= 'telecomfinal.csv'


tele_data<-read.csv(paste0(path,filename),stringsAsFactors = T)

###------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#--------DATA PREPARATION(Quality Report)#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------#

str(tele_data)
sort(names(tele_data))->co_names

##------#------#------#------#------#------#------#------#------#----------numeric variables col numbers------
data_type = c()
for(i in 1:length(co_names))
{data_type[i]<-class(tele_data[,co_names[i]])}

num_var<-which(data_type=="numeric")
fact_var<-which(data_type=="factor")
int_var<-which(data_type=="integer")

length(num_var)
##35 variables are numeric

length(fact_var)
#21 variables are factor

length(int_var)
#25 variables are integer.

numbers<-c(num_var,int_var)
 ##------#------#------#------#------#------#------#------#------#------------Colnames Initilization#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------# 

obs_name<-c()
no_obs<-c()
data_type<-c()
uniqueRecords<-c()
data_available<-c()
missing<-c()


pct1<-c(length=81)#5th
length(pct1)<-81

pct2<-c(length=81)#10th
length(pct2)<-81

pct3<-c(length=81)#25th
length(pct3)<-81


pct4<-c(length=81)#50
length(pct4)<-81


pct5<-c(length=81)#75
length(pct5)<-81


pct6<-c(length=81)#90
length(pct6)<-81

pct7<-c(length=81)#95
length(pct7)<-81




for(i in 1:length(co_names)){
  obs_name[i]<-co_names[i]
  no_obs[i]<-length(tele_data[,co_names[i]])
  data_type[i]<-class(tele_data[,co_names[i]])
  uniqueRecords[i]<-length(unique(tele_data[,co_names[i]]))
  data_available[i]<-sum(complete.cases(tele_data[,co_names[i]]))
  missing[i]<-sum(is.na(tele_data[,co_names[i]]))
  }
 
##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------For calculating min values#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------------
  
min_val<-vector(mode="numeric",length = 81)
  for(i in num_var){
  min_val[i]<-min(tele_data[,co_names[i]],na.rm = T)
  }

  for(i in int_var){
    min_val[i]<-min(tele_data[,co_names[i]],na.rm = T)
  }
  ##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------For calculating max values#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------------
max_val<-vector(mode="numeric",length = 81)
  for(i in num_var){
    max_val[i]<-max(tele_data[,co_names[i]],na.rm = T)
  }
  
  for(i in int_var){
    max_val[i]<-max(tele_data[,co_names[i]],na.rm = T)
  }
  
##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#--------MeanVaues for all Variables#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-------#
mean_val<-vector(mode="numeric",length = 81)
for(i in num_var){
  mean_val[i]<-mean(tele_data[,co_names[i]],na.rm = T)
}

for(i in int_var){
  mean_val[i]<-mean(tele_data[,co_names[i]],na.rm = T)
}
##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------------caluculating percentiles#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------
quantile_var<-vector(length = 81)
quantile(tele_data[,co_names[1]],p=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm = T)



##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------5th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct1[i]<-quantile(tele_data[,co_names[i]],p=c(0.05),na.rm = T)
}

for(i in int_var){
  pct1[i]<-quantile(tele_data[,co_names[i]],p=c(0.05),na.rm = T)
}


##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------10th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct2[i]<-quantile(tele_data[,co_names[i]],p=c(0.10),na.rm = T)
}

for(i in int_var){
  pct2[i]<-quantile(tele_data[,co_names[i]],p=c(0.10),na.rm = T)
}

##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------25th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct3[i]<-quantile(tele_data[,co_names[i]],p=c(0.25),na.rm = T)
}

for(i in int_var){
  pct3[i]<-quantile(tele_data[,co_names[i]],p=c(0.25),na.rm = T)
}

##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------50th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct4[i]<-quantile(tele_data[,co_names[i]],p=c(0.5),na.rm = T)
}

for(i in int_var){
  pct4[i]<-quantile(tele_data[,co_names[i]],p=c(0.5),na.rm = T)
}

##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------75th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct5[i]<-quantile(tele_data[,co_names[i]],p=c(0.75),na.rm = T)
}

for(i in int_var){
  pct5[i]<-quantile(tele_data[,co_names[i]],p=c(0.75),na.rm = T)
}

##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------90th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct6[i]<-quantile(tele_data[,co_names[i]],p=c(0.9),na.rm = T)
}

for(i in int_var){
  pct6[i]<-quantile(tele_data[,co_names[i]],p=c(0.9),na.rm = T)
}


##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#----------95th Percentile#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------
for(i in num_var){
  pct7[i]<-quantile(tele_data[,co_names[i]],p=c(0.95),na.rm = T)
}

for(i in int_var){
  pct7[i]<-quantile(tele_data[,co_names[i]],p=c(0.95),na.rm = T)
}



dataquality<-data.frame(obs_name,no_obs, data_type, uniqueRecords,data_available, missing,Minvalue=round(min_val,2),MaxValue=round(max_val,2),
                        round(pct1,2),round(pct2,2),round(pct3,2),round(pct4,2),round(pct5,2),round(pct6,2),round(pct7,2))



pct1<-ifelse(is.na(pct1),0,pct1)
pct2<-ifelse(is.na(pct2),0,pct1)
pct3<-ifelse(is.na(pct3),0,pct1)
pct4<-ifelse(is.na(pct4),0,pct1)
pct5<-ifelse(is.na(pct5),0,pct1)
pct6<-ifelse(is.na(pct6),0,pct1)
pct7<-ifelse(is.na(pct7),0,pct1)

setwd(path)
write.csv(dataquality,"qualityReport.csv")

# forgntvl-Foreign travel dummy variable.
# ;mtrcycle;truck are seems to be categorical variables. (Min: 0 , Max:1)




##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-------END OF Quality Report Preparation#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#

##------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------Continuous variable profiling#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-------
numbers = c(num_var,int_var)
a<-vector(length = 60)
for(i in 1:length(numbers)){
  a[i]<-paste(co_names[numbers][i],numbers[i],sep="-dat")
}
        
# [6] "blck_dat_Mean-dat16"    "callwait_Mean-dat17"    "change_mou-dat22"       "comp_dat_Mean-dat25"    "comp_vce_Mean-dat26"   
# [11] "custcare_Mean-dat29"    "da_Mean-dat31"          "da_Range-dat32"         "datovr_Mean-dat33"      "datovr_Range-dat34"    
# [16] "drop_blk_Mean-dat36"    "drop_dat_Mean-dat37"    "drop_vce_Mean-dat38"    "hnd_price-dat45"        "iwylis_vce_Mean-dat48" 
# [21] "mou_Mean-dat54"         "mou_opkv_Range-dat55"   "mou_pead_Mean-dat56"    "mou_Range-dat57"        "opk_dat_Mean-dat61"    
# [26] "ovrmou_Mean-dat62"      "ovrrev_Mean-dat63"      "plcd_dat_Mean-dat65"    "plcd_vce_Mean-dat66"    "recv_sms_Mean-dat69"   
# [31] "rev_Mean-dat72"         "rev_Range-dat73"        "roam_Mean-dat74"        "totmrc_Mean-dat77"      "totrev-dat78"          
# [36] "actvsubs-dat1"          "adjqty-dat3"            "age1-dat5"              "age2-dat6"              "avg3mou-dat9"          
# [41] "avg3qty-dat10"          "avg6mou-dat11"          "avg6qty-dat12"          "callwait_Range-dat18"   "ccrndmou_Range-dat21"  
# [46] "churn-dat24"            "Customer_ID-dat30"      "drop_vce_Range-dat39"   "eqpdays-dat42"          "forgntvl-dat44"        
# [51] "income-dat47"           "models-dat52"           "months-dat53"           "mtrcycle-dat58"         "numbcars-dat59"        
# [56] "owylis_vce_Range-dat64" "retdays-dat71"          "totcalls-dat76"         "truck-dat79"            "uniqsubs-dat80"        

# actvsubs-dat1 #------#------#------#------#------#------#------#------#------#-------- should be Categorical 
# blck_dat_Mean-dat16#------#------#------#------#------#------#------#------#------#------------removed because of mostly 0's all percentiles
# churn-dat24 #------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-------TARGET
# Customer_ID-dat30 #------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------UNIQUE ID
# forgntvl-dat44 #------#------#------#------#------#------#------#------#------#-------- should be Categorical 
# income-dat47 #------#------#------#------#------#------#------#------#------#-------- should be Categorical (CHECK again!)
# models-dat52  #------#------#------#------#------#------#------#------#------#--------should be Categorical
# months-dat53 #------#------#------#------#------#------#------#------#------#--------should be Categorical
# mtrcycle-dat58  #------#------#------#------#------#------#------#------#------#-------- should be Categorical
# numbcars-dat59 #------#------#------#------#------#------#------#------#------#-------- should be Categorical
# truck-dat79 #------#------#------#------#------#------#------#------#------#-------- should be Categorical
# uniqsubs-dat80 #------#------#------#------#------#------#------#------#------#--------- should be Categorical

####------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-------MISSING VALUES IMPUATATION#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#---------###########
cont_missCols = dataquality[((dataquality$missing>0) & (dataquality$data_type=='integer' | dataquality$data_type=='numeric')),c("obs_name")]

for (columns in cont_missCols){
  tele_data[is.na(tele_data[columns]),columns]<-mean(tele_data[,columns],na.rm = TRUE)
  print(columns)
}
###------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#--------CONTINUOUS VARIBALE PROFILING#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#-----------####

#------#------#------#------#------#------#------#------#------#-----------
number = 10
tele_data%>%mutate(dec=ntile(adjmou,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat2

dat2$N<-unclass(tele_data%>%mutate(dec=ntile(adjmou,n=number))%>%count(dec)%>%unname())[[2]]

dat2$churn_perc<-dat2$n/dat2$N

dat2$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(adjmou,n=number))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]

dat2$LessThan<-unclass(tele_data%>%mutate(dec=ntile(adjmou,n=number))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]

dat2$varname<-rep("adjmou",nrow(dat2))


#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(adjqty,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat3

dat3$N<-unclass(tele_data%>%mutate(dec=ntile(adjqty,n=number))%>%count(dec)%>%unname())[[2]]

dat3$churn_perc<-dat3$n/dat3$N

dat3$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(adjqty,n=number))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]

dat3$LessThan<-unclass(tele_data%>%mutate(dec=ntile(adjqty,n=number))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]

dat3$varname<-rep("adjqty",nrow(dat3))
#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(adjrev,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat4

dat4$N<-unclass(tele_data%>%mutate(dec=ntile(adjrev,n=number))%>%count(dec)%>%unname())[[2]]

dat4$churn_perc<-dat4$n/dat4$N

dat4$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(adjrev,n=number))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]

dat4$LessThan<-unclass(tele_data%>%mutate(dec=ntile(adjrev,n=number))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]

dat4$varname<-rep("adjrev",nrow(dat4))

##------#------#------#------#------#------#------#------#------#------
number = 7
tele_data%>%mutate(dec=ntile(age1,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat5

dat5$N<-unclass(tele_data%>%mutate(dec=ntile(age1,n=number))%>%count(dec)%>%unname())[[2]]

dat5$churn_perc<-dat5$n/dat5$N

dat5$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(age1,n=number))%>%group_by(dec)%>%summarise(min(age1)))[[2]]

dat5$LessThan<-unclass(tele_data%>%mutate(dec=ntile(age1,n=number))%>%group_by(dec)%>%summarise(max(age1)))[[2]]

dat5$varname<-rep("age1",nrow(dat5))

##------#------#------#------#------#------#------#------#------------
number = 3
tele_data%>%mutate(dec=ntile(age2,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat6

dat6$N<-unclass(tele_data%>%mutate(dec=ntile(age2,n=number))%>%count(dec)%>%unname())[[2]]

dat6$churn_perc<-dat6$n/dat6$N

dat6$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(age2,n=number))%>%group_by(dec)%>%summarise(min(age2)))[[2]]

dat6$LessThan<-unclass(tele_data%>%mutate(dec=ntile(age2,n=number))%>%group_by(dec)%>%summarise(max(age2)))[[2]]

dat6$varname<-rep("age2",nrow(dat6))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avg3mou,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat9

dat9$N<-unclass(tele_data%>%mutate(dec=ntile(avg3mou,n=number))%>%count(dec)%>%unname())[[2]]

dat9$churn_perc<-dat9$n/dat9$N

dat9$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avg3mou,n=number))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]

dat9$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avg3mou,n=number))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]

dat9$varname<-rep("avg3mou",nrow(dat9))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avg3qty,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat10

dat10$N<-unclass(tele_data%>%mutate(dec=ntile(avg3qty,n=number))%>%count(dec)%>%unname())[[2]]

dat10$churn_perc<-dat10$n/dat10$N

dat10$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avg3qty,n=number))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]

dat10$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avg3qty,n=number))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]

dat10$varname<-rep("avg3qty",nrow(dat10))
#
#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avg6mou,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat11

dat11$N<-unclass(tele_data%>%mutate(dec=ntile(avg6mou,n=number))%>%count(dec)%>%unname())[[2]]

dat11$churn_perc<-dat11$n/dat11$N

dat11$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avg6mou,n=number))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]

dat11$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avg6mou,n=number))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]

dat11$varname<-rep("avg6mou",nrow(dat11))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avg6qty,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat12

dat12$N<-unclass(tele_data%>%mutate(dec=ntile(avg6qty,n=number))%>%count(dec)%>%unname())[[2]]

dat12$churn_perc<-dat12$n/dat12$N

dat12$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avg6qty,n=number))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]

dat12$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avg6qty,n=number))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]

dat12$varname<-rep("avg6qty",nrow(dat12))



#------#------#------#------#------#------#------#------#------#------

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avgmou,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat13

dat13$N<-unclass(tele_data%>%mutate(dec=ntile(avgmou,n=number))%>%count(dec)%>%unname())[[2]]

dat13$churn_perc<-dat13$n/dat13$N

dat13$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avgmou,n=number))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]

dat13$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avgmou,n=number))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]

dat13$varname<-rep("avgmou",nrow(dat13))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avgqty,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat14

dat14$N<-unclass(tele_data%>%mutate(dec=ntile(avgqty,n=number))%>%count(dec)%>%unname())[[2]]

dat14$churn_perc<-dat14$n/dat14$N

dat14$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avgqty,n=number))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]

dat14$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avgqty,n=number))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]

dat14$varname<-rep("avgqty",nrow(dat14))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(avgrev,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat15

dat15$N<-unclass(tele_data%>%mutate(dec=ntile(avgrev,n=number))%>%count(dec)%>%unname())[[2]]

dat15$churn_perc<-dat15$n/dat15$N

dat15$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avgrev,n=number))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]

dat15$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avgrev,n=number))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]

dat15$varname<-rep("avgrev",nrow(dat15))

#------#------#------#------#------#------#------#------#------------
#Resume
number =2
tele_data%>%mutate(dec=ntile(blck_dat_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat16

dat16$N<-unclass(tele_data%>%mutate(dec=ntile(blck_dat_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat16$churn_perc<-dat16$n/dat16$N

dat16$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(blck_dat_Mean,n=number))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]

dat16$LessThan<-unclass(tele_data%>%mutate(dec=ntile(blck_dat_Mean,n=number))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]

dat16$varname<-rep("blck_dat_Mean",nrow(dat16))

#------#------#------#------#------#------#------#------#------#------
  
  
number = 4
tele_data%>%mutate(dec=ntile(callwait_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat17

dat17$N<-unclass(tele_data%>%mutate(dec=ntile(callwait_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat17$churn_perc<-dat17$n/dat17$N

dat17$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(callwait_Mean,n=number))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]

dat17$LessThan<-unclass(tele_data%>%mutate(dec=ntile(callwait_Mean,n=number))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]

dat17$varname<-rep("callwait_Mean",nrow(dat17))

#------#------#------#------#------#------#------#------#------#------
number =3
tele_data%>%mutate(dec=ntile(callwait_Range,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat18

dat18$N<-unclass(tele_data%>%mutate(dec=ntile(callwait_Range,n=number))%>%count(dec)%>%unname())[[2]]

dat18$churn_perc<-dat18$n/dat18$N

dat18$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(callwait_Range,n=number))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]

dat18$LessThan<-unclass(tele_data%>%mutate(dec=ntile(callwait_Range,n=number))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]

dat18$varname<-rep("callwait_Range",nrow(dat18))

#------#------#------#------#------#------#------#------#------#------
number = 3
tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat21

dat21$N<-unclass(tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=number))%>%count(dec)%>%unname())[[2]]

dat21$churn_perc<-dat21$n/dat21$N

dat21$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=number))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]

dat21$LessThan<-unclass(tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=number))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]

dat21$varname<-rep("ccrndmou_Range",nrow(dat21))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(change_mou,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat22

dat22$N<-unclass(tele_data%>%mutate(dec=ntile(change_mou,n=number))%>%count(dec)%>%unname())[[2]]

dat22$churn_perc<-dat22$n/dat22$N

dat22$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(change_mou,n=number))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]

dat22$LessThan<-unclass(tele_data%>%mutate(dec=ntile(change_mou,n=number))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]

dat22$varname<-rep("change_mou",nrow(dat22))

#------#------#------#------#------#------#------#------#------#------
number = 2
tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat25

dat25$N<-unclass(tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat25$churn_perc<-dat25$n/dat25$N

dat25$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=number))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]

dat25$LessThan<-unclass(tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=number))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]

dat25$varname<-rep("comp_dat_Mean",nrow(dat25))

#------#------#------#------#------#------#------#------#------#------
number = 10
tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat26

dat26$N<-unclass(tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat26$churn_perc<-dat26$n/dat26$N

dat26$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=number))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]

dat26$LessThan<-unclass(tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=number))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]

dat26$varname<-rep("comp_vce_Mean",nrow(dat26))

#------#------#------#------#------#------#------#------#------#------
number =3
tele_data%>%mutate(dec=ntile(custcare_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat29

dat29$N<-unclass(tele_data%>%mutate(dec=ntile(custcare_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat29$churn_perc<-dat29$n/dat29$N

dat29$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(custcare_Mean,n=number))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]

dat29$LessThan<-unclass(tele_data%>%mutate(dec=ntile(custcare_Mean,n=number))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]

dat29$varname<-rep("custcare_Mean",nrow(dat29))

#------#------#------#------#------#------#------#------#------#------
number =4
tele_data%>%mutate(dec=ntile(da_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat31

dat31$N<-unclass(tele_data%>%mutate(dec=ntile(da_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat31$churn_perc<-dat31$n/dat31$N

dat31$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(da_Mean,n=number))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]

dat31$LessThan<-unclass(tele_data%>%mutate(dec=ntile(da_Mean,n=number))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]

dat31$varname<-rep("da_Mean",nrow(dat31))


#------#------#------#------#------#------#------#------#------#------
number = 4
tele_data%>%mutate(dec=ntile(da_Range,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat32

dat32$N<-unclass(tele_data%>%mutate(dec=ntile(da_Range,n=number))%>%count(dec)%>%unname())[[2]]

dat32$churn_perc<-dat32$n/dat32$N

dat32$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(da_Range,n=number))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]

dat32$LessThan<-unclass(tele_data%>%mutate(dec=ntile(da_Range,n=number))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]

dat32$varname<-rep("da_Range",nrow(dat32))

#------#------#------#------#------#------#------#------#------#------
number = 2
tele_data%>%mutate(dec=ntile(datovr_Mean,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat33

dat33$N<-unclass(tele_data%>%mutate(dec=ntile(datovr_Mean,n=number))%>%count(dec)%>%unname())[[2]]

dat33$churn_perc<-dat33$n/dat33$N

dat33$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Mean,n=number))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]

dat33$LessThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Mean,n=number))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]

dat33$varname<-rep("datovr_Mean",nrow(dat33))

#------#------#------#------#------#------#------#------#------#------
number = 2
tele_data%>%mutate(dec=ntile(datovr_Range,n=number))%>%count(churn,dec)%>%filter(churn==1)->dat34

dat34$N<-unclass(tele_data%>%mutate(dec=ntile(datovr_Range,n=number))%>%count(dec)%>%unname())[[2]]

dat34$churn_perc<-dat34$n/dat34$N

dat34$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Range,n=number))%>%group_by(dec)%>%summarise(min(datovr_Rang