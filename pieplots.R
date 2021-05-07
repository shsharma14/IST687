#R - code file for all the pieplots
#the code needs to placed in the main code file to run
install.packages("ggrepel")
library("ggrepel")
#plot piechat for visitor type
View(combined_data)
View(visitortable)
visitortable <- data.frame(table(combined_data$VisitorType))
visitortable$percent <- round(visitortable$Freq/sum(visitortable$Freq) * 100,digits = 2)
visitorplot <- ggplot(visitortable, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Visitor Type"))+ theme_void()
visitorplot

#meal,visitortype,customertype,season,deposit#plot
#plot piechart for mealplan
mealplan <-  data.frame(table(combined_data$Meal))
View(mealplan)
mealplan$percent <- round(mealplan$Freq/sum(mealplan$Freq) * 100,digits = 2)
mealplanplot <- ggplot(mealplan, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Meal Plan"))+ theme_void()
mealplanplot

#plot piechart for customertype
customertypeframe <-  data.frame(table(combined_data$CustomerType))
View(customertypeframe)
customertypeframe$percent <- round(customertypeframe$Freq/sum(customertypeframe$Freq) * 100,digits = 2)
customertypeplot <- ggplot(customertypeframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Customer Type"))+ theme_void()
customertypeplot

#plot piechart for season
seasonframe <-  data.frame(table(combined_data$Season))
View(seasonframe)
seasonframe$percent <- round(seasonframe$Freq/sum(seasonframe$Freq) * 100,digits = 2)
seasonframeplot <- ggplot(seasonframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Season"))+ theme_void()
seasonframeplot

#plot piechart for deposit
depositframe <-  data.frame(table(combined_data$DepositType))
View(seasonframe)
depositframe$percent <- round(depositframe$Freq/sum(depositframe$Freq) * 100,digits = 2)
depositframeplot <- ggplot(depositframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1,color = "white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Deposit Status"))+ theme_void()
depositframeplot