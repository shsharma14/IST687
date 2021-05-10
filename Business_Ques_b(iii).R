# Which hotel had more cancellations? City or Resort? (Insert plot lead time for cancellations) 

IsCanceled_HotelType <- combined_data %>% group_by(HotelType, IsCanceled) %>% summarise(count = n())

ggplot(data = IsCanceled_HotelType, aes(x = HotelType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Cancelation Rate Hotel Wise",
       x = "Type of Hotel",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,50000,5000))


# Ratio of cancelation in city Hotel is higher. 