# Script to fetch, rinse&clean, analyse and show data regarding Danish bank statement from Danske Bank

library(plyr)
library(ggplot2)
library(readxl)

#import data
DanskeKonto <- read_excel("DanskeKonto-3618712532-20181107.xlsx", 
                                                col_types = c("text", "text", "numeric", 
                                                              "numeric", "text", "text"))
# Dont forget to change at will the name of the xlsx file with bank data


DanskeKonto$Date <- as.Date(DanskeKonto$Date, format="%d.%m.%Y")
DanskeKonto$Month <- as.Date(cut(DanskeKonto$Date, breaks="month"))

# Let's look at the expenses only 
DanskeKonto <- subset(DanskeKonto, DanskeKonto$Amount < 0)

# Build simple regexp strings
cash <- "ATM|BANK|client"
food <- "Foetex|Bilka|Netto|7-ELEVEN|Irma|Fakta|7Eleven|ALDI"
flights <- "EASYJET|RYANAIR|AIRWAYS|flights"
transport <- "DSB|Metrostation|gomore|Flixbus"
gas <- "oil|tank|F24|Shell|Circle K|Bonus|Gasoline|CIRCLE K|UnoX"
rent <- "rent|Rent"
freetime <- "Boldklub|McDonald|Street Food|Hotel|Old Irish|Sushi|Bar|Temabar|Burger|King|Fitness|Shawarmahuset"
exeptional <- "payment|fixing"
medical <- "Apotek|medical|Doctor|Apotheke"
# Do this for as many useful classes as you can think of

# Add a class field to the data, default "other"
DanskeKonto$class <- "Other"

# Apply the regexp and return their class
DanskeKonto$Class <- ifelse(grepl(transport, DanskeKonto$Text), "Transport",
            ifelse(grepl(cash, DanskeKonto$Text), "Cash",
            ifelse(grepl(food, DanskeKonto$Text), "Food",
            ifelse(grepl(flights, DanskeKonto$Text), "Flights",
            ifelse(grepl(rent, DanskeKonto$Text), "Rent",
            ifelse(grepl(freetime, DanskeKonto$Text), "Freetime",
            ifelse(grepl(exeptional, DanskeKonto$Text), "Exeptional",
            ifelse(grepl(medical, DanskeKonto$Text), "Medical", "Other"))))))))

## PLOTS
# Build summary table of monthly spend per class
smr <- ddply(DanskeKonto, .(Month, Class), summarise, cost=abs(sum(Amount)))

# Expenditure Overview for each class along months
ggplot(smr, aes(Month, cost, col=Class)) +
  facet_wrap(~Class, ncol=2, scale="free_y") +
  geom_smooth(method="loess", se=F) + geom_point() +  
  theme(axis.text.x=element_text(angle=45, hjust=1), 
        legend.position="none") + 
  labs(x="", y="Monthly total (DKK)")

# Dont worry for possible warnings...

# Barplot for average monthly expense
yl <- ddply(smr, .(Class), summarise, m=mean(cost))
ggplot(yl, aes(x=Class, y=m)) + 
  geom_bar(stat="identity") +
  labs(y="Average monthly expense (DKK)", x="")
