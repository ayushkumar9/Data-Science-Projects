# Project Title:  <Hotel Room Pricing In The Indian Market>
# NAME: <AYUSH KUMAR>
# EMAIL: <ayushkum10@gmail.com>
# COLLEGE: <IIT GUWAHATI>
#Setting the working directory
setwd("~/IIM Intern")
#Reading the data file into R
cities <- read.csv("Cities42.csv")
# Summary of the data
summary(cities)
summary(cities$IsMetroCity)

##----------------------------------------------------------

model <- lm(RoomRent ~ StarRating + IsWeekend + IsTouristDestination + IsMetroCity + IsNewYearEve +  FreeWifi + FreeBreakfast +  HasSwimmingPool +  Airport,data = cities  )
summary(model)

model1 <- lm(RoomRent ~ StarRating + IsTouristDestination + IsNewYearEve +  FreeWifi + FreeBreakfast +  HasSwimmingPool +  Airport + IsMetroCity,data = cities)
summary(model1)

##--------------------------------------------------------------------------------------

boxplot(RoomRent ~ IsTouristDestination,data = cities)
boxplot(RoomRent ~ StarRating,data = cities)
boxplot(RoomRent ~ HasSwimmingPool,data = cities)