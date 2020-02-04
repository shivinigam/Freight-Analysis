#conmed freight analysis

#import the data file
library(readxl)
updated_outbound <- read_excel("D:/NEU Courses/XN Project/Data/Outbound Lithia 1.1.18-31.12.18.xlsx")

#drop unwanted columns based on their relationship with the target variable
modified_outbound<-updated_outbound[ , -which(names(updated_outbound) %in% c("BOL","GL Code",
                                                          "Account","Reference 1","Reference 2","Reference 3","Sender Street",
                                                          "Receiver Street","Receiver Street 2"))]


#find columns with missing values
list_na <- colnames(modified_outbound)[ apply(modified_outbound, 2, anyNA) ]
list_na





#find unique tracking numbers and remove duplicate entries
modified_outbound<-modified_outbound[!duplicated(modified_outbound$Trackingno),]


#fill billing option using mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Calculate the mode for bill option using the user function and impute the result in place of missing values.
result_bill_option <- getmode(modified_outbound$`Bill Option`)

for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$`Bill Option`[i])){
    modified_outbound$`Bill Option`[i]= result_bill_option
  }
}

#fill mean net charge in negative, 0 and null net charge
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$`Net Charges`[i]) | modified_outbound$`Net Charges`[i] <= 0 ){
    modified_outbound$`Net Charges`[i]= mean(modified_outbound$`Net Charges`)
  }
}

#fill mean approved charge in negative, 0 and null approved
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$Approved[i]) | modified_outbound$Approved[i] <= 0 ){
    modified_outbound$Approved[i]= mean(modified_outbound$Approved)
  }
}

#fill number of packages with mean number of packages

for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$`# Packages`[i]) | modified_outbound$`# Packages`[i]==0){
    modified_outbound$`# Packages`[i]= ceiling(mean(modified_outbound$`# Packages`,na.rm=TRUE))
  }
}

#fill mean billed weight in place of missng and 0 weight
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$`Billed Weight`[i]) | modified_outbound$`Billed Weight`[i] <= 0 ){
    modified_outbound$`Billed Weight`[i]= mean(modified_outbound$`Billed Weight`,na.rm = TRUE)
  }
}

#find month and year from ship date
#fill missing dates
#convert date into month, year and day

#modified_outbound$`Ship Date` <-as.character(modified_outbound$`Ship Date`)
modified_outbound$`Ship Date`<-as.Date(modified_outbound$`Ship Date`, "%m/%d/%Y")

#extract month and create a month column
modified_outbound$ship_month<-format(modified_outbound$`Ship Date`,format="%b")

#extract year and create new column year
modified_outbound$ship_year<-format(modified_outbound$`Ship Date`,format="%Y")

#extract day and create another coumn day
modified_outbound$ship_day<-format(modified_outbound$`Ship Date`,format="%d")

#get the weekday for each date
modified_outbound$ship_weekday<-format(modified_outbound$`Ship Date`,format="%a")

#find mode for month, year, day, weekday
#create a function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Calculate the mode for weekday, day and month using the user function and impute the result in place of missing values.
result_weekday <- getmode(modified_outbound$ship_weekday)
result_day<-getmode(modified_outbound$ship_day)
result_month<-getmode(modified_outbound$ship_month)
result_year<-getmode(modified_outbound$ship_year)

#calculate median for year and impute
#result_year<-as.character(median(modified_outbound$year, na.rm = TRUE))

#impute missing weekday 
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$ship_weekday[i])){
    modified_outbound$ship_weekday[i]= result_weekday
  }
}

#impute missing months
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$ship_month[i])){
    modified_outbound$ship_month[i]= result_month
  }
}

#impute missing day
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$ship_day[i])){
    modified_outbound$ship_day[i]= result_day
  }
}

#impute missing year
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$ship_year[i])){
    modified_outbound$ship_year[i]= result_year
  }
}

#modified_outbound$`Invoice Date` <-as.character(modified_outbound$`Invoice Date`)
modified_outbound$`Invoice Date`<-as.Date(modified_outbound$`Invoice Date`, "%m/%d/%Y")


#extract month and create a month column for invoice
modified_outbound$invoice_month<-format(modified_outbound$`Invoice Date`,format="%b")

#extract year and create new column year
modified_outbound$invoice_year<-format(modified_outbound$`Invoice Date`,format="%Y")


#fill missing or 0 quantities with mean
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_conmed_data$Quantity1[i])){
    modified_conmed_data$Quantity1[i]= ceiling(mean(modified_conmed_data$Quantity1,na.rm =TRUE))
  }
}

#get statstics
#sort(table(modified_outbound$weekday), decreasing = TRUE)
#sort(table(modified_outbound$day), decreasing = TRUE)
#sort(table(modified_outbound$month), decreasing = TRUE)
#sort(table(modified_outbound$year), decreasing = TRUE)
#sort(table(modified_outbound$`Service Modified`), decreasing = TRUE)
#sort(table(modified_outbound$`Carrier Modified`), decreasing = TRUE)
#sort(table(modified_outbound$`Receiver Country`), decreasing = TRUE)

#length(unique(modified_outbound$`Receiver Country`))
#length(unique(modified_outbound$`Receiver City`))
#summary(modified_outbound$`Net Charges`)
#summary(modified_outbound$`# Packages`)
#summary(modified_outbound$`Billed Weight`)

#drop modified or unwanted columns
modified_outbound<-modified_outbound[ , -which(names(modified_outbound) %in% c("Zone","Ship Date", "Quantity 2",
                                                                             "Packaging 2","Sender name" ,"Sender Zip","Receiver Name","Receiver Company",
                                                                             "Receiver Zip","Invoice Date","Approved","Packaging 1",
                                                                             "Sender City","Sender State","Sender Country"))]

install.packages("data.table")
library(data.table)

#change column names
nms <- c("Invoice","Trackingno","Carrier_Modified", "Service_Modified", "Bill_Option", "Net_Charges", "Packages","Quantity1",     
         "Billed_Weight", "Sender_Company","Receiver_City","Receiver_State", "Receiver_Country","days_difference", "ship_month", "ship_year", "ship_day", "ship_weekday",    
         "invoice_month","invoice_year")
setnames(modified_outbound, nms)

#fill missing or 0 quantities with mean
for (i in 1:nrow(modified_outbound)){
  if(is.na(modified_outbound$Quantity1[i]) | modified_outbound$Quantity1[i]==0){
    modified_outbound$Quantity1[i]= ceiling(mean(modified_outbound$Quantity1,na.rm =TRUE))
  }
}


#export the file
write.csv(modified_outbound,file = "final_conmed_data.csv")