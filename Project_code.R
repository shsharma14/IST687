



cat('\014')
rm(list=ls())

EnsurePackage <- function(library) {
  library <- as.character(library)
  if(!require(library, character.only = T)){
    install.packages(pkgs = library,  repos="http://cran.r-project.org")
    require(library, character.only = T)
  }
}

EnsurePackage("gdata")
EnsurePackage("ggplot2")
EnsurePackage("readxl")
EnsurePackage("dplyr")
EnsurePackage("gridExtra")
EnsurePackage("ggmap")
EnsurePackage("ggrepel")
EnsurePackage("scales")
EnsurePackage("patchwork")
EnsurePackage("arules")
EnsurePackage("arulesViz")
EnsurePackage("lubridate")

#################################### Reading in the Data #################

getwd()
setwd("C:/Users/shubh/Desktop/IST 687/Final_Project")   # Set working directory according to your config

resort_data <- data.frame(read_excel("H1-Resort.xlsx"))
city_data <- read_excel("H2-City.xlsx")


# colnames(resort_data)
# colnames(city_data)


########### Combining both dataset for better analysis.

# To combine rows using rbind, we need to match the row names. In both dataset, one column name is different, "Arrival.Date" in resort data & "Arrival Date" in city data

# converting  columns having dates as character to date data type. (Only 2 columns.....) and renaming arrival date column in both to apply rbind.

resort_data$ArrivalDate <- c(1:nrow(resort_data))
resort_data$ArrivalDate <- as.Date(resort_data$Arrival.Date)
resort_data$Arrival.Date <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)


city_data$ArrivalDate <- c(1:nrow(city_data))
city_data$ArrivalDate <- as.Date(city_data$'Arrival Date')
city_data$'Arrival Date' <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)

city_data$ReservationStatusDate <- as.Date(city_data$ReservationStatusDate)


resort_data$HotelType <- "resort"
city_data$HotelType <- "city"


# Now, we can combine the data.
combined_data <- rbind(city_data, resort_data)

#################################  CLEANING  DATA #########################################################################################################################
# -------------------------------------------------------------------------------------------------
###########################################################################################################################################################################


# View(combined_data)

# str(combined_data)

# summary(combined_data)  # Checking descriptive stats of columns.


# removing white spaces from categorical columns (https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r)
# because Deposit type column contains values like "No Deposit       " instead of "No Deposit".      
cols_to_be_rectified <- names(combined_data)[vapply(combined_data, is.character, logical(1))]
combined_data[,cols_to_be_rectified] <- lapply(combined_data[,cols_to_be_rectified], trimws)



############ Tables for variables. #################
str(combined_data)

sort(table(combined_data$IsCanceled), decreasing = T)

sort(table(combined_data$ReservationStatus), decreasing = T)
# Remove that one row containing '`` in Reservation Status
combined_data <- combined_data %>% filter(ReservationStatus != "`")


sort(table(combined_data$StaysInWeekendNights), decreasing = T)
sort(table(combined_data$StaysInWeekNights), decreasing = T)
sort(table(combined_data$Adults), decreasing = T)
sort(table(combined_data$Children), decreasing = T)
sort(table(combined_data$Babies), decreasing = T)

sort(table(combined_data$Meal), decreasing = T)
## Meal Type Undefined and SC are same. Will rename Undefined as SC.
combined_data$Meal[which(combined_data$Meal == "Undefined")] <- "SC"


sort(table(combined_data$Country), decreasing = T)
sort(table(combined_data$MarketSegment), decreasing = T)
# Removing 2 undefined rows for Market Segment - 
combined_data <- combined_data %>% filter(MarketSegment != "Undefined")

sort(table(combined_data$DistributionChannel), decreasing = T)
# Removing that 1 undefined row for distribution Channel - 
combined_data <- combined_data %>% filter(DistributionChannel != "Undefined")


sort(table(combined_data$IsRepeatedGuest), decreasing = T)

sort(table(combined_data$PreviousCancellations), decreasing = T)
sort(table(combined_data$PreviousBookingsNotCanceled), decreasing = T)

sort(table(combined_data$ReservedRoomType), decreasing = T)
sort(table(combined_data$AssignedRoomType), decreasing = T)
sort(table(combined_data$BookingChanges), decreasing = T)
sort(table(combined_data$DepositType), decreasing = T)
sort(table(combined_data$DaysInWaitingList), decreasing = T)
sort(table(combined_data$CustomerType), decreasing = T)
sort(table(combined_data$RequiredCarParkingSpaces), decreasing = T)
sort(table(combined_data$TotalOfSpecialRequests), decreasing = T)
sort(table(combined_data$HotelType), decreasing = T)

## After removing undefined and ambiguous rows --- 
############ MAKING COLUMNS PROPER DATATYPE  ####### (Converting into factor)

# https://datascience.stackexchange.com/questions/12018/when-to-choose-character-instead-of-factor-in-r

####### Convert character columns to factors and character columns containing numeric values to num.
#combined_data[sapply(combined_data, is.character)] <-
#  lapply(combined_data[sapply(combined_data, is.character)], as.factor)


combined_data$IsCanceled <- factor(combined_data$IsCanceled, labels = c("Not Canceled", "Canceled"))
combined_data$ReservationStatus <- as.factor(combined_data$ReservationStatus)
combined_data$Children <- as.numeric(combined_data$Children)
combined_data$Meal <- as.factor(combined_data$Meal)
combined_data$Country <- as.factor(combined_data$Country)
combined_data$MarketSegment <- as.factor(combined_data$MarketSegment)
combined_data$DistributionChannel <- as.factor(combined_data$DistributionChannel)
combined_data$IsRepeatedGuest <- factor(combined_data$IsRepeatedGuest,  labels = c("Not Repeated", "Repeated"))
combined_data$ReservedRoomType <- as.factor(combined_data$ReservedRoomType)
combined_data$AssignedRoomType <- as.factor(combined_data$AssignedRoomType)
combined_data$DepositType <- as.factor(combined_data$DepositType)
combined_data$CustomerType <- as.factor(combined_data$CustomerType)
combined_data$HotelType <- as.factor(combined_data$HotelType)


# str(combined_data)


#----------------------- checking for missing data  -------------------------

colSums(is.na(combined_data)) # Checking which columns have NA values.
# Arrival date has 39270 missing values. 
# we can get those values using ReservationStatusDate and TotalNumberOfDaysStayedWeekNIghts and TotalNumberOfDaysStayedWeekendNights where reservationstatus is checkout.

combined_data$ArrivalDate[which(is.na(combined_data$ArrivalDate) & combined_data$ReservationStatus == 'Check-Out')] <-
  combined_data$ReservationStatusDate - (combined_data$StaysInWeekendNights + combined_data$StaysInWeekNights)

# Now, 1519 rows remain that have NA in arrival date. We will remove those NA values using na.omit().
colSums(is.na(combined_data))  

# Removing all NA data.
combined_data <- na.omit(combined_data)   

anyNA(combined_data)   # FALSE meaning no NA values in dataset.


#################------------------- Adding new columns based on arrival date  -----------------##########################

#### SEASON COLUMN ######
combined_data$Season <- quarter(combined_data$ArrivalDate)
combined_data$Season[combined_data$Season == 1] <- "Spring"
combined_data$Season[combined_data$Season == 2] <- "Summer"
combined_data$Season[combined_data$Season == 3] <- "Fall"
combined_data$Season[combined_data$Season == 4] <- "Winter"

combined_data$Season <- as.factor(combined_data$Season)


### Arrival Year column ######
combined_data$ArrivalYear <- as.factor(format(combined_data$ArrivalDate, "%Y"))

#### Arrival Month Column ######
combined_data$ArrivalMonth <- as.factor(month.name[as.numeric(format(combined_data$ArrivalDate, "%m"))])






#######-----------------------Adding new categorical column, ParkingSpaceNeeded and AnySpecialRequests based on column 
# RequiredCarParkingSpaces and TotalOfSpecialRequests ----------------------------###########################
### Might be useful for association rule mining

combined_data$ParkingSpaceNeeded <- as.factor(ifelse(combined_data$RequiredCarParkingSpaces > 0 , "Yes", "No"))

combined_data$AnySpecialRequest <- as.factor(ifelse(combined_data$TotalOfSpecialRequests > 0 , "Yes", "No"))



###################----------------- adding new column Visitor Type. ----------------####################
########### Visitor Type can be Business Traveler, Solo Traveler, Couple and Family ########

table(combined_data$Adults)  # Can remove 0 adults column since babies and children won't visit alone assuming children need to be accompanied by an adult.

# Removing using filter command.
combined_data <- combined_data %>% dplyr::filter(Adults != 0)

# ADR is zero for adults greater than 4.  ( 16 rows)
combined_data$ADR[combined_data$Adults > 4]

# Removing those 16 rows as well.
combined_data <- combined_data %>% dplyr::filter(Adults < 5)

'%notin%' <- Negate('%in%')

# Adding the column
combined_data$VisitorType <- c(1:nrow(combined_data))   # Initializing

# SOLO TRAVELER
combined_data$VisitorType[which(combined_data$Adults == 1 & combined_data$Babies == 0 & combined_data$Children == 0  & 
                                  combined_data$CustomerType %notin% "Group" )] <- "Solo Traveler"

# COUPLE
combined_data$VisitorType[which(combined_data$Adults == 2 & combined_data$Babies == 0 & combined_data$Children == 0  & 
                                  combined_data$CustomerType %notin% "Group" )] <- "Couple"

# FAMILY
combined_data$VisitorType[which(combined_data$Adults >= 2 & (combined_data$Babies != 0 | combined_data$Children != 0 ) & combined_data$DistributionChannel != "Corporate" &
                                  combined_data$CustomerType %notin% "Contract")] <- "Family"

# BUSINESS TRAVEL
combined_data$VisitorType[which(combined_data$Babies == 0 & combined_data$Children == 0 & combined_data$DistributionChannel == "Corporate")] <- "Business Travel"


dataframe <-
  combined_data[which(
    combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")
  ),
  c("Adults",
    "Children",
    "Babies",
    "DistributionChannel",
    "CustomerType")]

#Analyzing above dataframe, remaining rows are majorly 3 adults having either 1 or more children or 1 or more babies and distribution channel as TA/TO - can classify 
# them as families 

# FAMILY
combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel") & combined_data$Adults >= 3 
                                & combined_data$DistributionChannel != "Corporate" & combined_data$CustomerType %notin% "Contract")] <- "Family"
# Families rarely will have contract bookings, so excluded Customer Type as Contract.

# Checking rows which are left to be labeled a visitor type
length(combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family"))])      # 1214 rows.

# Removing remaining 1214 rows.
combined_data <- combined_data %>% slice(-which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")))


table(combined_data$VisitorType)
combined_data$VisitorType <- as.factor(combined_data$VisitorType)

#########################-------------Analyzing IsCanceled, ReservationStatus for any mismatched data-------------------##############################


#                         ReservationStatus
# IsCancelled      
#                 Check-Out         No Show        Canceled
# Canceled				   X                 
# Not Canceled                                        X 

# There shouldn't be any rows where ReservationStatus is Check-Out and Iscanceled shows Canceled. 
length(which(combined_data$IsCanceled == "Canceled" & combined_data$ReservationStatus %in% "Check-Out"))  # 0 rows. THIS IS FINE!!!!

# Similarly, there shouldn't be any rows where reservation Status is Canceled and IsCanceled contains Not Canceled.
length(which(combined_data$IsCanceled == "Not Canceled" & combined_data$ReservationStatus %in% "Canceled")) # 0 rows. THIS IS FINE!!!!   


####################----------------creating new Columns 1. Total Number of Days Stayed and 2. Average Revenue per Stay -------------############################### 

# Summing up no of days stayed in Weekend and Weekdays
combined_data$TotalNumberOfDaysStayed <- combined_data$StaysInWeekendNights + combined_data$StaysInWeekNights

ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = TotalNumberOfDaysStayed)) + coord_flip() # So many outliers.

# Checking how many people stayed for more than 15 days.
length(which(combined_data$TotalNumberOfDaysStayed > 14))  # 419 rows

# We can remove these 419 rows since TotalNumberOfDaysStayedg at a hotel for a max of 2 weeks seems logical. 

combined_data <- combined_data %>% filter(TotalNumberOfDaysStayed <15)

# Checking how many rows have total number of days stayed as zero.
length(which(combined_data$TotalNumberOfDaysStayed == 0))   # Does it make sense to have this as 0? I dont think so.

# Removing rows where TotalNumberOfDaysStayed = 0
combined_data <- combined_data %>% slice(-which(combined_data$TotalNumberOfDaysStayed == 0))  

# Before creating new column, let's get rid of outliers in ADR.
summary(combined_data$ADR) # This shows 3rd quartile is 126 while max is 5400.

# Boxplot of ADR is difficult to analyze because of that max value. We will remove that big outlier.
ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = ADR)) + coord_flip() 

# removing that one big outlier of ADR
combined_data <- combined_data[-which(combined_data$ADR > 5000),] 

# Now draw boxplot again.
ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = ADR)) + coord_flip()  

#New Column 
combined_data$AvgRevenuePerStay <- combined_data$TotalNumberOfDaysStayed * combined_data$ADR


####################----------------creating new Column ADRType (Categorizing ADR values by a range) -------------###############################

# Histogram of ADR 
ggplot(data = combined_data) + geom_histogram(mapping = aes(x = ADR),color = "white") +
  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) + 
  geom_vline(xintercept = quantile(combined_data$ADR, 0.75), col = "red") +
  geom_vline(xintercept = quantile(combined_data$ADR, 0.15), col = "blue")

# Based on the plot, maybe we can create a new categorical column where 0-50 can be low ADR, 50-150 can be medium ADR and above 150 can be high ADR.
combined_data$ADRType <- cut(combined_data$ADR, breaks = c(0,50,150, Inf), labels = c("Low", "Medium", "High"), include.lowest = T)



######################   ------- Final Check for NA values after creating new columns ###################################

colSums(is.na(combined_data))   # ADRType has 1 NA.

# Removing that 1 NA value
combined_data <- na.omit(combined_data)


################---------------------- Looking for Outliers --------------------------------------------#########################

EnsurePackage("dlookr")
diagnose_outlier(combined_data)

str(combined_data)



# Cleaned resort data and city data seperately if needed for plots
resort_data <- combined_data %>% dplyr::filter(HotelType == "resort")
city_data <- combined_data %>% dplyr::filter(HotelType == "city")


################################################   CLEANING DONE!!!!!! READY TO MAKE PLOTS ############################################
# -------------------------------------------------------------------------------------------------------------------------------------
#######################################################################################################################################





############ Tables for variables. #################
str(combined_data)

sort(table(combined_data$IsCanceled), decreasing = T)
sort(table(combined_data$ReservationStatus), decreasing = T)
sort(table(combined_data$StaysInWeekendNights), decreasing = T)
sort(table(combined_data$StaysInWeekNights), decreasing = T)
sort(table(combined_data$Adults), decreasing = T)
sort(table(combined_data$Children), decreasing = T)
sort(table(combined_data$Babies), decreasing = T)
sort(table(combined_data$Meal), decreasing = T)
sort(table(combined_data$Country), decreasing = T)
sort(table(combined_data$MarketSegment), decreasing = T)
sort(table(combined_data$DistributionChannel), decreasing = T)
sort(table(combined_data$IsRepeatedGuest), decreasing = T)
sort(table(combined_data$PreviousCancellations), decreasing = T)
sort(table(combined_data$PreviousBookingsNotCanceled), decreasing = T)
sort(table(combined_data$ReservedRoomType), decreasing = T)
sort(table(combined_data$AssignedRoomType), decreasing = T)
sort(table(combined_data$BookingChanges), decreasing = T)
sort(table(combined_data$DepositType), decreasing = T)
sort(table(combined_data$DaysInWaitingList), decreasing = T)
sort(table(combined_data$CustomerType), decreasing = T)
sort(table(combined_data$RequiredCarParkingSpaces), decreasing = T)
sort(table(combined_data$TotalOfSpecialRequests), decreasing = T)
sort(table(combined_data$HotelType), decreasing = T)
sort(table(combined_data$ArrivalYear), decreasing = T)
sort(table(combined_data$ArrivalMonth), decreasing = T)
sort(table(combined_data$Season), decreasing = T)
sort(table(combined_data$ParkingSpaceNeeded), decreasing = T)
sort(table(combined_data$AnySpecialRequest), decreasing = T)
sort(table(combined_data$VisitorType), decreasing = T)
sort(table(combined_data$ADRType), decreasing = T)


