## Which hotel type had a better ADR? City or resort?


mean_stats_hotelType <- combined_data %>% group_by(HotelType) %>% summarise(mean = mean(ADR),count = n())

# create the plot
ggplot(data = combined_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(HotelType ~ .) + scale_x_continuous(breaks = seq(0,500,50)) +
  scale_fill_gradient("Count", low="orange", high="red") + 
  geom_vline(data = mean_stats_hotelType, aes(xintercept = mean, color = HotelType), size = 1) + 
  theme_bw() +
  labs(title = "Distribution of ADR for each type of Hotel",
       x = "Average Daily Rate (in Euros)",
       y = "Count") + 
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) 


