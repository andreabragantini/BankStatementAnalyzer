library(plyr)
library(ggplot2)

s <- read.csv("<filename.csv>", sep=",", row.names=NULL)
colnames(s) <- c("date", "type", "desc", "value", "balance", "acc")
s$date <- as.Date(s$date, format="%d/%m/%Y")
s <- s[,1:5]
s$month <- as.Date(cut(s$date, breaks="month"))
s <- subset(s, s$value < 0)

# Build simple regexp strings
coffee <- "PRET|STARBUCKS|NERO|COSTA"
cash <- "NATWEST|BARCLAYS|BANK"
food <- "TESCO|SAINSBURY|WAITROSE"
flights <- "EASYJET|RYANAIR|AIRWAYS"
trains <- "EC MAINLINE|TRAINLINE|GREATER ANGLIA"
# Do this for as many useful classes as you can think of

# Add a class field to the data, default "other"
s$class <- "Other"

# Apply the regexp and return their class
s$class <- ifelse(grepl(coffee, s$desc), "Coffee",
                  ifelse(grepl(cash, s$desc), "Cash",
                         ifelse(grepl(food, s$desc), "Food",
                                ifelse(grepl(flights, s$desc), "Flights",
                                       ifelse(grepl(trains, s$desc), "Trains", "Other")))))


smr <- ddply(s, .(month, class), summarise, cost=abs(sum(value)))
ggplot(smr, aes(month, cost, col=class)) +
  facet_wrap(~class, ncol=2, scale="free_y") +
  geom_smooth(method="loess", se=F) + geom_point() +  
  theme(axis.text.x=element_text(angle=45, hjust=1), 
        legend.position="none") + 
  labs(x="", y="Monthly total (£)")


yl <- ddply(smr, .(class), summarise, m=mean(cost))
ggplot(yl, aes(x=class, y=m)) + 
  geom_bar(stat="identity") +
  labs(y="Average monthly expense (£)", x="")
