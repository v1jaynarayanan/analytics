install.packages(c("gridExtra", "stringr", "ggplot2", "dplyr"))
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)

###############################
### Author: Vijay Narayanan ###
###############################

#Load data
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = F, header = T)
str(uber)
summary(uber)

#Check if there are any duplicate rows for Request.id?
sapply(uber, function(x) sum(duplicated(x)))

#Check for unique values
sapply(uber, function(x) length(unique(x)))

#Check for NA values
sapply(uber, function(x) sum(is.na(x)))
#Driver.Id has NA values when status is No Cars Available and Drop.timestamp has NA values when status is cancelled. 
#These rows are important for analysis and hence are not removed

###### DATA CLEANING ######
#1.Change to factors
uber$Pickup.point <- factor(uber$Pickup.point)
uber$Status  <- factor(uber$Status)

#2.Request.timestamp and Drop.timestamp columns have inconsistent date format with both / and - as seperators
#Change separator from / to - for consistency
uber$request_datetime <- str_replace_all(uber$Request.timestamp, "\\/", "-")
uber$drop_datetime <- str_replace_all(uber$Drop.timestamp, "\\/", "-")

#3. Standardize the date and time formats for both Request and Drop timestamp columns
# Note: Since seconds is not useful in this analysis only hour and minutes are considered
uber$request_datetime <- as.POSIXct(uber$request_datetime, format = "%d-%m-%Y %H:%M")
uber$drop_datetime <- as.POSIXct(uber$drop_datetime, format = "%d-%m-%Y %H:%M")

#4. Derive new variables from existing data
#Extract day and hour from request date time as it will be useful to see patterns by days and hours
uber$request_day <- format(uber$request_datetime, "%d")
uber$request_hour <- format(uber$request_datetime, "%H")

###### Analysis ######
#Status column is an unordered categorical variable. It provides information of whether Trip was completed, cancelled or no cars available.
#As it is important from the prespectie of both driver and customer, this field is chosen first for analysis.
uber %>% group_by(Status) %>% summarise(count = n()) %>% arrange(desc(count))
#Number of "No Cars Available" and "Cancelled" are quite high. But, further analysis required to see if there is a 
#pattern for all days.

#Data set contains Hourly data for days from 11th July 2016 to 15th July 2016. This information can be used to view the
#number of requests for each hour on all days. A bar graph is chosen as it can represent requests on all days along with their statuses.
#A custom function to Plot Number of Requests for each hour day wise along with the status
plotByDay <- function(dayNumber) {
   day <- filter(uber, request_day == dayNumber)
   ggplot(day, aes(x=as.factor(request_hour), fill=Status)) + geom_bar() + labs(x = "Hour", y = "No of Requests")  
}

day1Plot <- plotByDay("11")
day2Plot <- plotByDay("12")
day3Plot <- plotByDay("13")
day4Plot <- plotByDay("14")
day5Plot <- plotByDay("15")

#Plot graphs one below the other for easy comparison
#Note: expand plots view to see entire graph
grid.arrange(day1Plot, day2Plot, day3Plot, day4Plot, day5Plot, ncol=1, nrow = 5)
#From the plots, an interesting pattern emerges and it can be seen that there are high requests between 5 am and 9 am
#and between 5 pm and 9 pm. This pattern is similar for all days. It will be useful to see from which locations these requests arise from?

#Which locations are these requests originating from? Pickup.point provides location information and it can be used for this analysis
#A bar graph is chosen as it can clearly represent requests on all days along with their locations.
#A custom function to plot Number of Requests for each hour day wise by Location 
plotByLocation <- function(dayNumber) {
  day <- filter(uber, request_day == dayNumber)
  ggplot(day, aes(x=as.factor(request_hour), fill=Pickup.point)) + geom_bar() + labs(x = "Hour", y = "No of Requests")
}

loc1Plot <- plotByLocation("11")
loc2Plot <- plotByLocation("12")
loc3Plot <- plotByLocation("13")
loc4Plot <- plotByLocation("14")
loc5Plot <- plotByLocation("15")

#Plot graphs one below the other for easy comparison
#Note: expand plots view to see entire graph
grid.arrange(loc1Plot, loc2Plot, loc3Plot, loc4Plot, loc5Plot, ncol=1, nrow = 5)
#Another interesting pattern can be observed. City has more demand between 5 am and 9 am and Airport has more 
#demand between 5 pm and 9 pm. This pattern is similar for all days.

#What are the most problematic types of requests for UBER?
#Plot combined hourly data for requests on all days by their Statuses.
#Status of requests is shown adjacent to each other for ease of comparison
ggplot(uber, aes(x=as.factor(request_hour), fill = Status)) + geom_bar(position = "dodge") + labs(x = "Hour of day", y = "Number of Requests", title = "Combined Hourly Data for all Days by Status")
#Number of cancellations are high from 5am to 9am and number of No Cars Available are high from 5pm to 9pm

#Plot combined hourly data for requests on all days by Location
#Location of requests is shown adjacent to each other for ease of comparison
ggplot(uber, aes(x=as.factor(request_hour), fill = Pickup.point)) + geom_bar(position = "dodge") + labs(x = "Hour of day", y = "Number of Requests", title = "Combined Hourly Data for all Days by Location")
#No of requests to airport from are high in the morning from 5am to 9am and No of requests from the airport to city are high from 5pm to 10pm
#So, City has high demand in the morning and Airport has high demand in the evening

#Create timeslots Early Morning, Morning, Day, Evening and Night to group by hours into these slots
#Time slots would help us understand the supply and demand in particular hours
uber$request_hour <- as.numeric(uber$request_hour)
uber$timeslot <- ifelse(uber$request_hour < 5, "Early Morning", ifelse(uber$request_hour < 10, "Morning", ifelse(uber$request_hour < 17, "Day", ifelse(uber$request_hour < 22, "Evening", "Night"))))

#Custom function to group number of requests in each timeslot
requestsInTimeSlot <- function(hourOfDay) {
  nrow(filter(uber, timeslot == hourOfDay))
}

requestsInTimeSlot("Early Morning")
requestsInTimeSlot("Morning")
requestsInTimeSlot("Day")
requestsInTimeSlot("Evening")
requestsInTimeSlot("Night")
#From the results, it can be seen that number of requests are high in the morning and evening

#Plot requests by timeslots
#Number of requests grouped by time slots showing the status
ggplot(uber, aes(x=as.factor(timeslot), fill = Status)) + geom_bar() + labs(x="Time slots", y="Number of Requests", title = "Number of Requests Grouped in Time Slots showing Status")
#Number of requests grouped by time slots showing the location
ggplot(uber, aes(x=as.factor(timeslot), fill = Pickup.point)) + geom_bar() + labs(x="Time slots", y="Number of Requests", title = "Number of Requests Grouped in Time Slots showing Location")

##########################################################################
#Two issues have emerged
#1.Cancellations are high in the morning time slot at the City 
#2.High number of cars unavailable in the evening timeslots at the Airport
##########################################################################


##########################################################################
### Critical Problem 1 - High cancellations in the city in the morning ###
##########################################################################
#Number of cancellations in the morning at the airpot and city
morning <- filter(uber, timeslot == "Morning")
nrow(filter(morning, Pickup.point == "City" & Status == "Cancelled"))
nrow(filter(morning, Pickup.point == "Airport" & Status == "Cancelled"))

ggplot(morning, aes(x=as.factor(Pickup.point), fill = Status)) + geom_bar() + stat_count(aes(label = ..count..), geom = "text", position=position_stack(vjust=0.5)) + labs(x="Location", y="Number of Requests", title = "Number of Requests in the Morning")

#City - calculate supply and demand in the morning
#Supply
supply_city <- nrow(filter(morning, Pickup.point == "City" & Status == "Trip Completed"))
supply_city
#Demand
demand_city <- nrow(filter(morning, Pickup.point == "City"))
demand_city
#Supply Demand Gap in the City
demand_city - supply_city
#Supply demand gap is 1205. Marketplace not in good health as there is more demand and less supply.

################################################################################################
### Critical Problem 2 - Hign unavailability of cars at the airport in the evening time slot ###
################################################################################################
#Number of No cars available
evening <- filter(uber, timeslot == "Evening")
nrow(filter(evening, Pickup.point == "Airport" & Status == "No Cars Available"))
nrow(filter(evening, Pickup.point == "City" & Status == "No Cars Available"))

ggplot(evening, aes(x=as.factor(Pickup.point), fill = Status)) + geom_bar() + stat_count(aes(label = ..count..), geom = "text", position=position_stack(vjust=0.5)) + labs(x="Location", y="Number of Requests", title = "Number of Requests in the Evening")

#Airport - calculate supply and demand in the evening
#Supply
supply_airport <- nrow(filter(evening, Pickup.point == "Airport" & Status == "Trip Completed"))
supply_airport
#Demand
demand_airport <- nrow(filter(evening, Pickup.point == "Airport"))
demand_airport
#Supply Demand Gap at the Airport
demand_airport - supply_airport
#Supply demand gap is 1427. Once again, marketplace not in good health as there is more demand than supply.

###############
##Conclusions
###############
#1. UBER needs to reduce cancellations from City to Airport in the morning
#2. UBER needs to increase availability of cars at the Airport in the evening

#####END#####