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

#################################### Reading in the Data #################
###################### Only resort Data ############################

getwd()
setwd("C:/Users/shubh/Desktop/IST 687/Final_Project")   # Set working directory according to your config

resort_data <- data.frame(read_excel("H1-Resort.xlsx"))
#city_data <- read_excel("H2-City.xlsx")

#################################  ANALYZING RESORT DATA ##################################################
View(resort_data)



str(resort_data)
#str(city_data)



# RESPONSE/DEPENDENT/OUTCOME Variable - ADR

# CATEGORICAL COLUMNS (By Default) - ReservationStatus, Meal, Country, MarketSegment, DistributionChannel, ReservedRoomType, AssignedRoomType, DepositType, 
# CustomerType

# Numerical Columns - Lead Time, StaysInWeekendNights, StaysInWeekNights, Adults, Children, Babies, PreviousCancellations, PreviousBookingsNotCanceled, BookingChanges,
# BookingChanges, DaysInWaitingList, ADR, RequiredCarParkingSpaces, TotalOfSpecialRequests

# Columns that can be turned into factors - IsCanceled, IsRepeatedGuest

# Date Columns - Arrival.Date, ReservationStatusDate

# Ambiguous Columms - Agent (is a character column containing agent IDs), Company





colnames(resort_data)  #Using this, we see the columns and ensure that no columns have spaces in between them. 
# We can access a column with space by using backticks (``).


summary(resort_data)  # Checking descriptive stats of columns.


colSums(is.na(resort_data)) # Checking which columns have NA values.
table(rowSums(is.na(resort_data)))




# removing white spaces from categorical columns (https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r)

cols_to_be_rectified <- names(resort_data)[vapply(resort_data, is.character, logical(1))]
resort_data[,cols_to_be_rectified] <- lapply(resort_data[,cols_to_be_rectified], trimws)




#Cleaning column ADR. Removing excess characters and converting into numeric.
StringCleaner <- function(x) {
  x <- as.numeric(gsub(" .*", "", x))
  return(x)
}

resort_data$ADR <- StringCleaner(resort_data$ADR)
str(resort_data)




# converting  columns having dates as character to date data type. (Only 2 columns.....)

resort_data$ArrivalDate <- c(1:nrow(resort_data))
resort_data$ArrivalDate <- as.Date(resort_data$Arrival.Date)
resort_data$Arrival.Date <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)

########### Looking for Outliers ###################


EnsurePackage("dlookr")
diagnose_outlier(resort_data)
plot_outlier(resort_data)


############ Tables for categorical variables. #################


sort(table(resort_data$ReservationStatus), decreasing = T)
sort(table(resort_data$Meal), decreasing = T)
sort(table(resort_data$Country), decreasing = T)
sort(table(resort_data$MarketSegment), decreasing = T)
sort(table(resort_data$DistributionChannel), decreasing = T)
sort(table(resort_data$ReservedRoomType), decreasing = T)
sort(table(resort_data$AssignedRoomType), decreasing = T)
sort(table(resort_data$DepositType), decreasing = T)
sort(table(resort_data$CustomerType), decreasing = T)

sort(table(resort_data$IsCanceled), decreasing = T)
sort(table(resort_data$IsRepeatedGuest), decreasing = T)





################################ Adding new column SEASON based on arrival date  #####################################3
# and then we can plot ADR vs Season  ##############

EnsurePackage("lubridate")
resort_data$Season <- quarter(resort_data$ArrivalDate)
resort_data$Season[resort_data$Season == 1] <- "Spring"
resort_data$Season[resort_data$Season == 2] <- "Summer"
resort_data$Season[resort_data$Season == 3] <- "Fall"
resort_data$Season[resort_data$Season == 4] <- "Winter"


################################## adding new column visitor type based on number of adults, children and babies. ##########
# Visitor Type can be Single, Couples and Family

table(resort_data$Adults)  # Can remove 0 adults column since babies and children won't visit alone assuming children need to be accompanied by an adult.

resort_data <- resort_data[-which(resort_data$Adults == 0),]

# Adding a column VisitorType
resort_data$VisitorType <- c(1:nrow(resort_data))

# If num(Adults) is 1 & num(babies & CHildren = 0), can label as single. 
resort_data$VisitorType[which(resort_data$Adults == 1 & resort_data$Children == 0 & resort_data$Babies == 0)] <- "Single"

# If num(Adults) is 2 & num(babies & CHildren = 0), can label as couple. 
resort_data$VisitorType[which(resort_data$Adults == 2 & resort_data$Children == 0 & resort_data$Babies == 0)] <- "Couple"

# If num(Adults) is 2 or greater than 2 & num(babies | CHildren != 0), can label as family. 
resort_data$VisitorType[which(resort_data$Adults >= 2 & (resort_data$Babies != 0 | resort_data$Children != 0))] <- "Family"

# Remaining ones (When adults >=2 & children,Babies = 0) are also family.
resort_data$VisitorType[which(resort_data$VisitorType != c("Couple", "Single", "Family"))] <- "Family"


#######################################################------------- PLOTS  -----------------------###################################################

#Plot Boxplots/ Stacked Barcharts / Histogram / Map (since there is country data).
# Grouped Boxplots  --  Categorical variables on x-axis and numerical variables on y-axis.


################ BIVARIATE POINT PLOTS ####################


# 1. 

Adult_ADR_plot <- ggplot(data = resort_data, aes(x = Adults, y = ADR)) + geom_point()
Adult_ADR_plot + coord_cartesian(xlim=c(0,10)) + scale_x_continuous(breaks = seq(0,10,2)) + xlab("Number of adults")

# We see that there is no data for adults more than 4. So, maybe we can filter out all the rows containing adults 5 or more than 5 since our outcome variable 
# is zero for those values.

EnsurePackage("dplyr")
resort_data <- resort_data %>% dplyr::filter(Adults < 5) # Not much effect since only 16 rows are removed but still, cleaning out the noise. 





#########################################################################################

###### Multiple Histograms from Grouped Data   ###############
# https://r-graphics.org/recipe-distribution-multi-hist

# 1. Analyzing ADR based on season (Seeing the distribution and Mean(ADR) season wise) ###################

# Calculate mean of ADR season wise to add a line to plot 
# https://stackoverflow.com/questions/63016568/how-to-plot-multiple-mean-lines-in-a-single-histogram-with-multiple-groups-prese
mean_stats_season_wise <- resort_data %>% group_by(Season) %>% summarise(mean = mean(ADR),n = n())

# create the plot
ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(Season ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_season_wise, aes(xintercept = mean, color = Season), size = 1)


# Fall season, ADR is high as compared to other seasons. 


#2. Analyzing ADR based on VisitorType (Seeing the distribution and Mean(ADR) season wise) ###################

# Calculate mean of ADR VisitorType wise to add a line to plot 
# https://stackoverflow.com/questions/63016568/how-to-plot-multiple-mean-lines-in-a-single-histogram-with-multiple-groups-prese
mean_stats_VisitorType_wise <- resort_data %>% group_by(VisitorType) %>% summarise(mean = mean(ADR),n = n())

# create the plot
ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(VisitorType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_VisitorType_wise, aes(xintercept = mean, color = VisitorType), size = 1)


# 3. Analyzing ADR and customer type 

ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(CustomerType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red")

# 4. Analyzing ADR and deposit type 

ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(DepositType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red")








#########################################################################################






########################## Boxplots  #####################

##################### 1. Deposit Type and ADR ####################################

#Basic plot with outliers in magenta color. 
ADR_DepositType_Boxplot <- ggplot(data = resort_data) + aes(x = DepositType, y = ADR, group = DepositType) + 
  geom_boxplot(outlier.colour="magenta")
ADR_DepositType_Boxplot


#Adding color to boxes 
ADR_DepositType_Boxplot <-  ADR_DepositType_Boxplot + 
  aes(color = DepositType) 
ADR_DepositType_Boxplot

# Writing count and mean of datapoints for all categories. (https://gscheithauer.medium.com/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80)

stat_box_data <- function(y, upper_limit = max(resort_data$ADR) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

ADR_DepositType_Boxplot <- ADR_DepositType_Boxplot + 
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  )

#Another way to see the density of data points - Box plot displaying the data points to see for which category we have more data
ADR_DepositType_Boxplot <- 
  ADR_DepositType_Boxplot + geom_jitter(shape=16, position=position_jitter(0.2))

ADR_DepositType_Boxplot    # FINAL BOXPLOT



####################### Meal and ADR #####################################


#Basic plot with outliers in magenta color. 
ADR_Meal_Boxplot <- ggplot(data = resort_data) + aes(x = Meal, y = ADR, group = Meal) + 
  geom_boxplot(outlier.colour="black")
ADR_Meal_Boxplot


#Adding color to boxes 
ADR_Meal_Boxplot <-  ADR_Meal_Boxplot + 
  aes(color = Meal) 
ADR_Meal_Boxplot

# Writing count and mean of datapoints for all categories. (https://gscheithauer.medium.com/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80)

stat_box_data <- function(y, upper_limit = max(resort_data$ADR) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

ADR_Meal_Boxplot <- ADR_Meal_Boxplot + 
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  )
ADR_Meal_Boxplot

#Another way to see the density of data points - Box plot displaying the data points to see for which category we have more data
ADR_Meal_Boxplot <- 
  ADR_Meal_Boxplot + geom_jitter(shape=16, position=position_jitter(0.2))

# Changing X-axis labels to make it more clearer.
ADR_Meal_Boxplot <- 
  ADR_Meal_Boxplot + scale_x_discrete(labels = c("BB" = "Bed & Breakfast", "FB" = "Full Board", "HB" = "Half Board"))

ADR_Meal_Boxplot     # FINAL BOXPLOT

# CAN WE REMOVE SC because of less data points or can we combine SC and Undefined into one column??????????????


# ##########################################################################
# #ADR and adults.
# # with(resort_data, boxplot(ADR ~ Adults))
# 
# summary(resort_data$Adults)
# 
# #Basic plot (group = Adults is important to get different number of adults on x-axis.)
# ADR_Adults_Boxplot <- ggplot(data = resort_data) + aes(x = Adults, y = ADR, group = Adults) + 
#   geom_boxplot()
# 
# #Limiting the number of adults to 4.
# ADR_Adults_Boxplot + xlim("0", "1")
# 
# 
# 
# 
# # Countries and ADR
# 
# table(resort_data$Country)  # Getting the count of Country Values to figure out which countries to analyze. 
# # mean(table(resort_data$Country))
# 
# length(which(table(resort_data$Country) > 100)) # Checking how many countries have data points greater than 100 and using only those in the boxplot.
# 
# ADR_Country_Boxplot <- ggplot(data = resort_data) + aes(x = Country, y = ADR, group = Country) + 
#   geom_boxplot()
# 
# ADR_Country_Boxplot
# 
# 
# 
# # IsRepeatedGuest and ADR
# 
# #Basic plot
# ADR_IsRepeatedGuest_Boxplot <- ggplot(data = resort_data) + aes(x = IsRepeatedGuest, y = ADR, group = IsRepeatedGuest) + 
#   geom_boxplot()
#  
# ADR_IsRepeatedGuest_Boxplot

#################################################################################################################################


######################## PLOTTING MAP FOR COUNTRY DATA ###########################

#### NEED TO REFINE THIS. JUST A RAW IDEA OF HOW WE CAN DO THIS. ###############

# I think, first we need to convert country codes to full names and get the latitude and longitude for that and then 
# we can plot a jitter plot showing each occurence of a country as a point on the map. That way we will know from which country most of our visitors
# come from.

# (https://stackoverflow.com/questions/26818257/how-to-convert-country-codes-into-country-names-in-a-column-within-a-data-frame)

EnsurePackage("countrycode")

# There are different formats. Ours is ISO3 (https://unstats.un.org/unsd/tradekb/knowledgebase/country-code)

?countrycode
?codelist

resort_data$countryFullName <- countrycode(resort_data$Country, "iso3c", "country.name")
sort(table(resort_data$countryFullName))

colSums(is.na(resort_data))
# BUt using this, we get some NA values. Need to think how to deal with them. Gotta deal with United States as well. 

# Now, we have got country names, we need to get central coordinates of those country names. 

EnsurePackage("rgeos")
EnsurePackage("rworldmap")

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
# df$Country <- rownames(df)  # Converting rownames to a column. 
head(df)


# To be continued - how to match two dataframes based on values???????  -> Can use match() function.
# https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc


resort_data$Countrylat <- df[match(resort_data$countryFullName, rownames(df)),"y"]
resort_data$Countrylon <- df[match(resort_data$countryFullName, rownames(df)),"x"]

# Again, more NA values. 


mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the countries on top
mp <- mp+ geom_point(aes(x=resort_data$Countrylon, y=resort_data$Countrylat) ,color="blue", size=1) 
mp + coord_map()



##################################################################################

# Bar charts - 

# We will have to reshape data to create side by side bar charts. Using tidyr, we can do that. pivot_longer() and pivot_wider() functions. 

EnsurePackage("ggthemes")

length(resort_data$IsCanceled)

# IsCancelled and DepositType


# IsCancelled and CustomerType








#  We can plot how many hotels were canceled for 2015, 2016, 2017


str(resort_data)

resort_data$Canceled <- ifelse(resort_data$IsCanceled == 1, "Canceled", "Not Canceled")

ggplot(data = resort_data, aes(x = Season, y = ADR, fill = Canceled)) +
  geom_bar(position = "dodge", stat = "identity")


ggplot(data = resort_data, aes(x = Season, y = ADR, fill = IsRepeatedGuest)) +
  geom_bar(position = "dodge", stat = "identity")


resort_data$ADR[which(resort_data$ADR <= 0)]




