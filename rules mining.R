#######################################################################################################################################

library(arules)
library(arulesViz)

#Visitortype-Cancellation Rate

#City

city_data$VisitorType <- as.factor(city_data$VisitorType)
Cencal_Visitor_city <- data.frame(city_data$IsCanceled, city_data$VisitorType)
Cencal_Visitor_city_trans <- as(Cencal_Visitor_city,"transactions")
rules1 <- apriori(Cencal_Visitor_city_trans, 
                  parameter=list(supp=0.005, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("city_data.IsCanceled=Canceled")))
inspect(rules1)
#1. The overall cancellation rate of city hotel is about 40%.
#2. The reservation cancellation rate for business travel is only about 50% of the overall cancellation rate. Therefore, 
#city hotel can charge only a low deposit when making reservations for business travel.
#3. The cancellation rate of couples are significantly higher than that of the other three types of tourists. So we will try to 
#find the specific connection through correlation analysis.

city_couples <- city_data %>% dplyr::filter(VisitorType == "Couple")  # Using dplyr package, filtering rows for couples only.
city_couples <- city_couples %>% select_if(is.factor)
city_couples <- city_couples %>% select(-ReservationStatus)
city_couples <- city_couples %>% select(-VisitorType)
city_couples <- city_couples %>% select(-HotelType)
str(city_couples)
city_couples <- as(city_couples, "transactions")
rules1_1 <- apriori(data = city_couples, parameter = list(support = 0.2, confidence = 0.9), appearance = list(rhs = c("IsCanceled=Canceled")))
inspect(sort(rules1_1, by = "lift", decreasing = F))
#Meal=BB, Country=PRT, DistributionChannel=TA/TO, AssignedRoomType=A, ReservedRoomType=A, AnySpecialRequest=No, 
#ParkingSpaceNeeded=No, IsRepeatedGuest=Not Repeated.
#With the above characteristics, the occurrence rate of reservations from couples is more than 20%, and the cancellation rate is more than 90%.

#Resort

resort_data$VisitorType <- as.factor(resort_data$VisitorType)
Cencal_Visitor_resort <- data.frame(resort_data$IsCanceled, resort_data$VisitorType)
Cencal_Visitor_resort_trans <- as(Cencal_Visitor_resort,"transactions")
rules2 <- apriori(Cencal_Visitor_resort_trans, 
                  parameter=list(supp=0.005, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("resort_data.IsCanceled=Canceled")))
inspect(rules2)
#1. The overall cancellation rate of resort hotel is about 30%, less than city hotel.
#2. For resort hotel, in addition to business travel(76% of all), solo travelers(63% of all) also have a low cancellation rate for reservations. 
#Therefore hotel can also charge a lower deposit for solo travelers.
#3. For resort hotel, the cancellation rate of family is the highest, about 17% higher than the overall cancellation rate.
#Therefore, they need to pay more deposit to ensure that the reservation will not be cancelled.
#4. Besides, the largest number of cancellations came from couples. So we will try to find the specific connection
#through correlation analysis.

resort_couples <- resort_data %>% dplyr::filter(VisitorType == "Couple")  # Using dplyr package, filtering rows for families only.
resort_couples <- resort_couples %>% select_if(is.factor)
resort_couples <- resort_couples %>% select(-ReservationStatus)
resort_couples <- resort_couples %>% select(-VisitorType)
resort_couples <- resort_couples %>% select(-HotelType)
str(resort_couples)
resort_couples <- as(resort_couples, "transactions")
rules2_1 <- apriori(data = resort_couples, parameter = list(support = 0.14, confidence = 0.6), appearance = list(rhs = c("IsCanceled=Canceled")))
inspect(sort(rules2_1, by = "lift", decreasing = F))
#Country=PRT, IsRepeatedGuest=Not Repeated, CustomerType=Transient, ParkingSpaceNeeded=No, DistributionChannel=TA/TO.
#With the above characteristics, the occurrence rate of reservations from couples is more than 14%, and the cancellation rate is more than 60%.

#Country=PRT, IsRepeatedGuest=Not Repeated, ParkingSpaceNeeded=No, DistributionChannel=TA/TO.
#For both hotels, the cancellation rate of orders with the above characteristics is very high. Therefore, when receiving orders
#from couples, they should pay more attention to the above characteristics and be prepared to cancel the orders with these characteristics.


################################################################################