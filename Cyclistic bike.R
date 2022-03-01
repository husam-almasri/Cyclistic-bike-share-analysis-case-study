# Load Libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(janitor)
library(data.table)
library(tidyr)
library(lubridate)
library(skimr)

# Load datasets
nov20<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202011-divvy-tripdata.csv")
dec20<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202012-divvy-tripdata.csv")
jan21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202101-divvy-tripdata.csv")
feb21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202102-divvy-tripdata.csv")
mar21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202103-divvy-tripdata.csv")
apr21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202104-divvy-tripdata.csv")
may21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202105-divvy-tripdata.csv")
jun21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202106-divvy-tripdata.csv")
jul21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202107-divvy-tripdata.csv")
aug21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202108-divvy-tripdata.csv")
sep21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202109-divvy-tripdata.csv")
oct21<-read.csv("C:/projects/Cyclistic/final project/Cyclistic Bike-datasets/202110-divvy-tripdata.csv")

#List has all datasets' names
ds<-list(nov20,dec20,jan21,feb21,mar21,apr21,may21,jun21,jul21,aug21,sep21,oct21)

#Check the fields' names for each dataset
for(i in 1:length(ds)){
	print(colnames(ds[[1]]))
}

#Check the fields' structure for each dataset
for(i in 1:length(ds)){
	str(ds[[1]])
}

#Combine all datasets in one
all_trips<-do.call('rbind',ds)

#Check the fields' structure for the combined data
skim(all_trips)

#Delete unnecessary fields
all_trips<-all_trips%>%select(-c(start_station_id , end_station_id))

#Fill the empty cells in start_station_name and end_station_name fields by (N/A)
all_trips$start_station_name[all_trips$start_station_name=='']<-'N/A'
all_trips$end_station_name[all_trips$end_station_name=='']<-'N/A'

#Convert the data type in started_at and ended_at fields to (datetime)
all_trips$started_at<-as_datetime(all_trips$started_at)
all_trips$ended_at<-as_datetime(all_trips$ended_at)

#Create ride_length field by the difference between the ended_at and started_at in minutes
all_trips$ride_length<-(as.double(difftime(all_trips$ended_at,all_trips$started_at)))/60

#Summary of ride_length
summary(all_trips$ride_length)

#Delete all trips under 5 minutes and above 1080 minutes
all_trips<-filter(all_trips,ride_length>5)
all_trips<-filter(all_trips,ride_length<1080)

#Delete all test trips
all_trips<-all_trips[!((all_trips$start_station_name %like% "TEST")),]
nrow(subset(all_trips, start_station_name %like% "TEST"))

#Create hour field by the hours part in started_at
all_trips$hour<-format(all_trips$started_at,'%H')
all_trips$hour<-as.POSIXct(all_trips$hour,format="%H")

#Create day field from started_at field
all_trips$day<-format(all_trips$started_at,'%A')
all_trips$day <- ordered(all_trips$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Create month field from started_at field
all_trips$month<-format(as.Date(all_trips$started_at),"%b-%y")
all_trips$month <- ordered(all_trips$month, levels=c('Nov-20','Dec-20','Jan-21','Feb-21','Mar-21','Apr-21','May-21','Jun-21','Jul-21','Aug-21','Sep-21','Oct-21'))

#Get some information about data
all_trips%>%summarise(Min_ride_length=min(ride_length),Max_ride_length=max(ride_length),Average_ride_length=mean(ride_length))
table(all_trips$member_casual)
setNames(aggregate(ride_length~member_casual+day,all_trips,sum),c("customer_type","Day of the week", "total_trip_duration(mins)"))
setNames(aggregate(ride_length~member_casual+day,all_trips,mean),c("customer_type","Day of the week", "Mean_trip_duration(mins)"))
setNames(aggregate(ride_length~member_casual+month,all_trips,sum),c("customer_type","Month", "total_trip_duration(mins)"))
setNames(aggregate(ride_length~member_casual+month,all_trips,mean),c("customer_type","Month", "Mean_trip_duration(mins)"))

#Analysis

#Number of trips by user type
all_trips%>%group_by(member_casual)%>%
summarise(number_of_rides = n())

all_trips%>%group_by(member_casual)%>%
summarise(number_of_rides = n())%>%
ggplot(aes(x='',y=number_of_rides,fill=member_casual))+
geom_col(color = "black") +
geom_label(aes(label = paste0(round(100 * number_of_rides/sum(number_of_rides), 2), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE) +
coord_polar(theta = "y")+
labs(title="Total trips by user type") +
theme_void()

#Ride Duration by Minutes
#Average ride length for all users
all_trips%>%
summarise(Average_ride_length=mean(ride_length))

#Average ride length per user type
all_trips%>%group_by(member_casual)%>%
summarise(Average_ride_length=mean(ride_length))

#Total ride length per user type
all_trips%>%group_by(member_casual)%>%
summarise(Total_ride_length=sum(ride_length))

#Average trips by customer type Vs. Day of the week
all_trips%>%group_by(member_casual,day)%>%
summarise(Average_ride_length=mean(ride_length))%>% 
arrange(member_casual, desc(Average_ride_length))

all_trips%>%group_by(member_casual,day)%>%
summarise(Average_ride_length=mean(ride_length))%>% 
arrange(member_casual, day)%>%
ggplot(aes(x=day,y=Average_ride_length,fill=member_casual))+
geom_col(width=0.75,position=position_dodge(width=0.75))+
labs(title ="Average trips by customer type Vs. Day of the week")+
theme(axis.text.x = element_text(angle = 30))

#Average trips by customer type Vs. Month
all_trips%>%group_by(member_casual,month)%>%
summarise(Average_ride_length=mean(ride_length))%>%
arrange(desc(member_casual),desc(Average_ride_length))

all_trips%>%group_by(member_casual,month)%>%
summarise(Average_ride_length=mean(ride_length))%>%
arrange(member_casual,month)%>%
ggplot(aes(x=month,y=Average_ride_length,fill=member_casual))+
geom_bar(stat='identity') +
labs(title="Average trips by customer type Vs. Month") +
theme(axis.text.x = element_text(angle = 90))

#Peak by day
all_trips%>%group_by(member_casual,day)%>%
summarise(number_of_rides = n(),Min_ride_length=min(ride_length),Max_ride_length=max(ride_length),Average_ride_length=mean(ride_length),Median_ride_length=median(ride_length))%>% 
arrange(member_casual, desc(Average_ride_length))

all_trips%>%group_by(member_casual,day)%>%
summarise(number_of_rides = n())%>% 
arrange(member_casual, desc(number_of_rides))

all_trips%>%group_by(member_casual,day)%>%
summarise(number_of_rides = n())%>% 
arrange(member_casual, day)%>%
ggplot(aes(x=day,y=number_of_rides,fill=member_casual))+
geom_col(width=0.75,position=position_dodge(width=0.75))+
labs(title ="Total trips by customer type Vs. Day of the week")+
theme(axis.text.x = element_text(angle = 30))

#Peak by month
all_trips%>%group_by(member_casual,month)%>%
summarise(number_of_rides = n(),Average_ride_length=mean(ride_length))%>%
arrange(member_casual,desc(number_of_rides))

all_trips%>%group_by(member_casual,month)%>%
summarise(number_of_rides = n())%>%
arrange(member_casual,month)%>%
ggplot(aes(x=month,y=number_of_rides,fill=member_casual))+
geom_bar(stat='identity') +
labs(title="Total trips by customer type Vs. Month") +
theme(axis.text.x = element_text(angle = 90))

#Hourly usage
all_trips%>%group_by(member_casual,hour)%>%
summarise(number_of_rides = n())%>% 
arrange(member_casual, desc(number_of_rides))

all_trips%>%group_by(member_casual,hour)%>%
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = hour, y = number_of_trips, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL, date_labels = "%H:%M") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Number of trips per hour during the week", x = "Day hours", y = "Number of rides")

#Bike types
all_trips%>%group_by(member_casual,rideable_type)%>%
summarise(number_of_rides = n())%>%
arrange(member_casual,desc(number_of_rides))

all_trips%>%group_by(member_casual,rideable_type)%>%
summarise(number_of_rides = n())%>%
arrange(member_casual)%>%
ggplot(aes(x=rideable_type,y=number_of_rides,fill=member_casual))+
geom_bar(stat='identity') +
labs(title="Total trips by customer type Vs. rideable type")

#Stations� locations
#Distribution of members' rides during the week
all_trips%>%group_by(member_casual,day,start_station_name)%>%
filter(any(member_casual=='member'))%>%
filter(any(start_station_name!='N/A'))%>%
summarise(number_of_rides= n()) %>% top_n(5)%>%
arrange(desc(number_of_rides))
 
all_trips%>%group_by(member_casual,day,start_station_name)%>%
filter(any(member_casual=='member'))%>%
filter(any(start_station_name!='N/A'))%>%
summarise(number_of_rides= n()) %>% top_n(5)%>%
arrange(desc(number_of_rides))%>%
ggplot(aes(x = start_station_name, y =day, fill= number_of_rides)) + 
geom_tile() +
scale_fill_gradient(high="red", low="blue") + 
labs(title="Distribution of members' rides during the week")+
theme(axis.text.x = element_text(angle = 90))

#Distribution of casuals' rides during the week
all_trips%>%group_by(member_casual='casual',day,start_station_name)%>%
filter(any(member_casual=='casual'))%>%
filter(any(start_station_name!='N/A'))%>%
summarise(number_of_rides= n()) %>% top_n(5)%>%
arrange(desc(number_of_rides))
 
all_trips%>%group_by(member_casual,day,start_station_name)%>%
filter(any(member_casual=='casual'))%>%
filter(any(start_station_name!='N/A'))%>%
summarise(number_of_rides= n()) %>% top_n(5)%>%
arrange(desc(number_of_rides))%>%
ggplot(aes(x = start_station_name, y =day, fill= number_of_rides)) + 
geom_tile() +
scale_fill_gradient(high="red", low="blue") +
labs(title="Distribution of casuals' rides during the week")+ 
theme(axis.text.x = element_text(angle = 90))

#Create a file with the modified data
write.csv(all_trips, "Cyclistic Bike.csv", row.names = F)
