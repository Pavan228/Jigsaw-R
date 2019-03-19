
path = "E:\\Courses\\Jigsaw R\\JIGSAW\\DSR\\R Lab\\Data Science with R - files\\Data Science with R - files\\Assignments\\Graded Assignments\\Topic 8.2 - Data Preparation"

library(dplyr)
library(lubridate)

setwd(path)

dir()

campaign_File = read.csv("Campaign_File.txt",sep = "\t",stringsAsFactor=FALSE)

customers_File = read.csv('Customers_File.txt',sep = "\t",stringsAsFactor=FALSE)

products_File = read.csv('Products_File.txt',sep = "\t",stringsAsFactor=FALSE)

transactions_File = read.csv("Transactions_File.txt",sep = "\t",stringsAsFactor=FALSE)

head(campaign_File) 
head(customers_File)

names(products_File)
head(products_File)

names(transactions_File)
head(transactions_File)

trans_prod = merge(transactions_File, products_File, by="Product_Code")

head(trans_prod)
str(trans_prod)

trans_prod%>%group_by(Product_Category)%>%summarise("Amount"=mean(Items_Amount))%>%arrange(-Amount)

head(customers_File)
head(transactions_File)

trans_cust = merge(transactions_File, customers_File, by="Card_ID")
head(trans_cust)

trans_cust$Birth_Date = as.Date((trans_cust$Birth_Date),format = '%Y-%m-%d')

trans_cust['Age'] = (Sys.Date()-trans_cust$Birth_Date)/duration(num = 1,units = 'year')

trans_cust[trans_cust$Age>=20 & trans_cust$Age<=50,'Age_seg']="20-50"

trans_cust[trans_cust$Age>50 & trans_cust$Age<=75,'Age_seg']="50-75"

trans_cust[trans_cust$Age>75 & trans_cust$Age<=100,'Age_seg']="75-100"
trans_cust[trans_cust$Age>100 & trans_cust$Age<=130,'Age_seg']="100-130"


head(trans_cust)

trans_cust$Age_seg=as.factor(trans_cust$Age_seg)

summary(trans_cust$Age)

levels(trans_cust$Age_seg)

trans_cust%>%group_by(Age_seg)%>%summarise("Mean_Amount_spend"=mean(Items_Amount))%>%arrange(-Mean_Amount_spend)

trans_cust%>%group_by(Age_seg)%>%summarise("Mean_Amount_spend"=sum(Items_Amount))%>%arrange(-Mean_Amount_spend)

head(campaign_File)

head(trans_cust)

trans_cust_camp = merge(trans_cust,campaign_File,by="Card_ID")

str(trans_cust_camp)

prop.table(table(trans_cust_camp$Age_seg,trans_cust_camp$Campaign_Responce))

df1 = trans_cust_camp%>%group_by(Age_seg)%>%summarise("Count"=n())
names(df1)[2] ="Total_count"

df2 = trans_cust_camp%>%filter(Campaign_Responce=='TRUE')%>%group_by(Age_seg)%>%summarise(n())
names(df2)[2] ="Responded_count"

df = merge(df1,df2,by='Age_seg',all = TRUE)
df

df[is.na(df$Responded_count),'Responded_count']=0
df

df['Response_rate'] = round((df$Responded_count/df$Total_count),2)
df[order(df$Response_rate,decreasing = TRUE)]

#Age 20-50 people responding high... And Its also has consistend Trend..

customers_File$Registration_Date<-as.Date(customers_File$Registration_Date,format = "%Y-%m-%d")

head(customers_File$Registration_Date)

customers_File["Tenure_period"] = as.Date("31/12/2002",format='%d/%m/%Y') - customers_File$Registration_Date
head(customers_File)

customers_File$Tenure_period <- as.numeric(customers_File$Tenure_period)

head(customers_File)

customers_File["Tenure_bin"] = ntile(customers_File$Tenure_period,5)# Binning the customers based on Tenure Period

head(customers_File)

class(customers_File$Tenure_bin)

customers_File$Tenure_bin = as.factor(customers_File$Tenure_bin)

class(customers_File$Tenure_bin)

head(campaign_File)
head(customers_File)

cust_camp = merge(customers_File,campaign_File,by= 'Card_ID')

prop.table(table(cust_camp$Tenure_bin,cust_camp$Campaign_Responce))# Slightly 5th bin people are responding High followed by 3&4.

names(trans_cust)
names(cust_camp)

trans_cust_camp2 = merge(trans_cust,cust_camp[,c('Card_ID',"Campaign_Responce","Tenure_bin")],by='Card_ID')

head(trans_cust_camp2)

data.frame(prop.table(table(trans_cust_camp2$Age_seg,trans_cust_camp2$Campaign_Responce)))%>%filter(Var2=='TRUE')%>%arrange(-Freq)
## 50-75 are Response rate high!

filterd_df = trans_cust_camp2%>%filter(Age_seg=="50-75")

table(filterd_df$Tenure_bin)

table(transactions_File$Payment_Method)# More Transcations from CC

head(transactions_File)

str(transactions_File)

transactions_File$Timestamp = ymd_hms(transactions_File$Timestamp)

transactions_File["Hour_derived"] = hour(transactions_File$Timestamp)

unique(transactions_File$Hour_derived)

prop.table(table(transactions_File$Hour_derived,transactions_File$Payment_Method))

head(trans_cust)

trans_cust%>%group_by(Gender,Age_seg)%>%summarize("Avg_Amount"=mean(Items_Amount))%>%arrange(-Avg_Amount)

### No impact on Age

#### Question 15
hist_dat = trans_cust_camp2%>%group_by(Tenure_bin,Gender)%>%summarize(count=n())
hist_dat

library(ggplot2)

trans_cust_camp2 = merge(trans_cust,cust_camp[,c('Card_ID',"Campaign_Responce","Tenure_bin","Tenure_period")],by='Card_ID')

names(trans_cust_camp2)


library(repr)

# Change plot size to 4 x 3
options(repr.plot.width=4, repr.plot.height=3)
ggplot(trans_cust_camp2,aes(Tenure_period,fill=Gender,alpha=0.5))+geom_histogram(position = "stack")+xlab("Tenure")
