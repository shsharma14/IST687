# What visitor type has the most cancellations, and how can we prevent certain types from not cancelling? 

EnsurePackage("ggplot2")
EnsurePackage("arules")
EnsurePackage("arulesViz")

# Resort
#using dplyr package, making a new dataframe that contains count 
VisitorType_IsCancelled_resort <- resort_data %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

# Making the plot
VisitorType_IsCancelled_resort_plot <-
  ggplot(
    data = (VisitorType_IsCancelled_resort) ,
    mapping = aes(x = VisitorType, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(breaks = seq(0,30000,2500)) +
  scale_fill_brewer(palette = "Reds") +
  theme_grey() +
  labs(title = "Resort",
       x = "VisitorType",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 



# City
#using dplyr package, making a new dataframe that contains count 
VisitorType_IsCancelled_city <- city_data %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

# Making the plot

VisitorType_IsCancelled_city_plot <-
  ggplot(
    data = (VisitorType_IsCancelled_city) ,
    mapping = aes(x = VisitorType, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  scale_fill_brewer(palette = "Reds") +
  theme_grey() +
  labs(title = "City",
       x = "VisitorType",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


# Arranging both plots in same figure
grid.arrange(VisitorType_IsCancelled_resort_plot,
             VisitorType_IsCancelled_city_plot,
             nrow = 2)


#### Now we need to get some rules why couples are canceling in the city more?
### USE Association Rules Mining



city_couples <- city_data %>% dplyr::filter(VisitorType == "Couple")  # Using dplyr package, filtering rows for couples only.

# now, we create a dataframe containing only factor columns so that we can apply association rules mining. 
city_couples <- city_couples %>% select_if(is.factor)
str(city_couples)

# Remove column ReservationStatus
city_couples <- city_couples %>% select(-ReservationStatus)

# Need to convert our dataframe to a transaction matrix 
city_couples <- as(city_couples, "transactions")

# Inspecting the first transaction
inspect(city_couples[1])

# create a frequency plot
itemFrequencyPlot(city_couples, topN = 20, cex.names = 0.75)


rules <- apriori(data = city_couples, parameter = list(support = 0.2, confidence = 0.8), appearance = list(rhs = c("IsCanceled=Canceled")))

inspect(sort(rules, by = "lift", decreasing = F))




