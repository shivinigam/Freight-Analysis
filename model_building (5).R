#build a linear regression model

#import the modified_outboundset
modified_conmed_data<-read.csv("latest_conmed_data.csv")

#model building

install.packages("data.table")
library(data.table)

#check for missing values
list_na <-colnames(modified_conmed_data)[apply(modified_conmed_data,2,anyNA)]
list_na

#find mode for month, year, day, weekday
#create a function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Calculate the mode for weekday, day and month using the user function and impute the result in place of missing values.
result_weekday <- getmode(modified_conmed_data$ship_weekday)
result_day<-getmode(modified_conmed_data$ship_day)
result_month<-getmode(modified_conmed_data$ship_month)
result_year<-getmode(modified_conmed_data$ship_year)


#impute missing day
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$ship_day[i])){
    modified_conmed_data$ship_day[i]= result_day
  }
}

#impute missing year
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$ship_year[i])){
    modified_conmed_data$ship_year[i]= result_year
  }
}

#impute missing weekday 
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$ship_weekday[i])){
    modified_conmed_data$ship_weekday[i]= result_weekday
  }
}

#impute missing months
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$ship_month[i])){
    modified_conmed_data$ship_month[i]= result_month
  }
}

#fill missing day difference with mean day difference
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$days_difference[i])){
    modified_conmed_data$days_difference[i]= ceiling(mean(modified_conmed_data$days_difference,na.rm =TRUE))
  }
}

#fill missing quantities
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$Quantity1[i]) | modified_conmed_data$Quantity1[i]==0){
    modified_conmed_data$Quantity1[i]= ceiling(mean(modified_conmed_data$Quantity1,na.rm =TRUE))
  }
}

#fill missing packages and 0
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$Packages[i]) | modified_conmed_data$Packages[i]==0){
    modified_conmed_data$Packages[i]= ceiling(mean(modified_conmed_data$Packages,na.rm =TRUE))
  }
}

#fill missing and 0 net charges
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$Net_Charges[i]) | modified_conmed_data$Net_Charges[i]==0){
    modified_conmed_data$Net_Charges[i]= (mean(modified_conmed_data$Net_Charges,na.rm =TRUE))
  }
}

#fill missing and 0 billed weight 
for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$Billed_Weight[i]) | modified_conmed_data$Billed_Weight[i]==0){
    modified_conmed_data$Billed_Weight[i]= (mean(modified_conmed_data$Billed_Weight,na.rm =TRUE))
  }
}

#impute missing year in invoice
result_year<-getmode(modified_conmed_data$invoice_year)

for (i in 1:nrow(modified_conmed_data)){
  if(is.na(modified_conmed_data$invoice_year[i])){
    modified_conmed_data$invoice_year[i]= result_year
  }
}

#drop packaging2
#modified_conmed_data$Packaging2<-NULL
#modified_conmed_data$Receiver_Company<-NULL
modified_conmed_data$X<-NULL
modified_conmed_data$Receiver_City<-NULL
#modified_conmed_data$invoice_day<-NULL
#modified_conmed_data$invoice_weekday<-NULL
modified_conmed_data$Invoice<-NULL
modified_conmed_data$Trackingno<-NULL

#fit the data into the model
#divide the data into train and test 75% - 25%
#set.seed(12345)
#row.number<-sample(x=1:nrow(modified_conmed_data),size = 0.75*nrow(modified_conmed_data))
#sample_train=modified_conmed_data[row.number,]
#sample_test=modified_conmed_data[-row.number,]

#build a linear regression model
#saturated_model<-lm(Net_Charges~.,data=sample_test)
#summary(saturated_model)

#shipping data within US
us_data<-modified_conmed_data[modified_conmed_data$Receiver_Country=="US",]
us_data$Receiver_Country<-NULL

#check for missing values
list_na <-colnames(us_data)[apply(us_data,2,anyNA)]
list_na


#shipping data international
int_data<-modified_conmed_data[modified_conmed_data$Receiver_Country!="US",]

#check for missing values
list_na <-colnames(int_data)[apply(int_data,2,anyNA)]
list_na

#drop Receiver_state from international shipping data
int_data$Receiver_State<-NULL

#build the model for domestic shipping (in US)
set.seed(12345)
row.number<-sample(x=1:nrow(us_data),size = 0.75*nrow(us_data))
sample_us_train=us_data[row.number,]
sample_us_test=us_data[-row.number,]


saturated_us_model<-lm(Net_Charges~.,data =sample_us_train)
summary(saturated_us_model)

#build the model for international shipping
set.seed(12345)
row.number<-sample(x=1:nrow(int_data),size = 0.75*nrow(int_data))
sample_int_train=int_data[row.number,]
sample_int_test=int_data[-row.number,]


saturated_int_model<-lm(Net_Charges~.,data =sample_int_train)
summary(saturated_int_model)

#reduce the US shipping model using backward selection method
step <- step(saturated_us_model, direction="backward")
step$anova # display results

#reduce the international shipping model using backward selection method
step <- step(saturated_int_model, direction="backward")
step$anova # display results


#reduced US shipping model
reduced_us_model<-lm(Net_Charges~ Carrier_Modified + Service_Modified + Bill_Option + 
                       Packages + Billed_Weight + Receiver_State + ship_month + 
                       ship_day, 
                     data = sample_us_train)
summary(reduced_us_model)






#reduce international shipping model
reduced_int_model<-lm(Net_Charges~ Carrier_Modified + Service_Modified + Bill_Option + 
                        Billed_Weight + Receiver_Country + days_difference + ship_day + 
                        ship_weekday,
                      data = sample_int_train)
summary(reduced_int_model)




