# What season has the most cancellations, and how can we prevent other seasons from having less cancellations? 



EnsurePackage("ggplot2")
EnsurePackage("arules")
EnsurePackage("arulesViz")

# Resort
#using dplyr package, making a new dataframe that contains count 
Season_IsCancelled_resort <- resort_data %>% group_by(Season, IsCanceled) %>% summarise(count = n())

# Making the plot
Season_IsCancelled_resort_plot <-
  ggplot(
    data = (Season_IsCancelled_resort) ,
    mapping = aes(x = Season, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(breaks = seq(0,30000,2500)) +
  scale_fill_brewer(palette = "Reds") +
  theme_grey() +
  labs(title = "Resort",
       x = "Season",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 



# City
#using dplyr package, making a new dataframe that contains count 
Season_IsCancelled_city <- city_data %>% group_by(Season, IsCanceled) %>% summarise(count = n())

# Making the plot

Season_IsCancelled_city_plot <-
  ggplot(
    data = (Season_IsCancelled_city) ,
    mapping = aes(x = Season, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_brewer(palette = "Reds") +
  theme_grey() +
  labs(title = "City",
       x = "Season",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


# Arranging both plots in same figure
grid.arrange(Season_IsCancelled_resort_plot,
             Season_IsCancelled_city_plot,
             nrow = 2)




# For city, summer has more canceled hotel than not canceled hotel.
# In other seasons also, people tend to cancel more in city than resort

# Use association rules mining to analyze


