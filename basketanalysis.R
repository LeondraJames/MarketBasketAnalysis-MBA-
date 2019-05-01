###########################################################################################################
###########################################################################################################
######################################### Market Basket Analysis Project ##################################
###########################################################################################################
###########################################################################################################
###########################################################################################################

#Load libraries
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(arulesViz)

#Load data - Online-Retail dataset from UCI ML Repository
df <- read_excel("C:/Users/leojames/Desktop/R Tutorials/Online Retail Market Basket Analysis.xlsx")
View(df)

#Remove missing values
df <- df[complete.cases(df),]

#Format fields
df %>% 
  mutate(Description = as.factor(Description),
         Country = as.factor(Country))

df$Date <- as.Date(df$InvoiceDate)
TransTime <- format(df$InvoiceDate, '%H:%M:%S')
InvoiceNo <- as.numeric(as.character(df$InvoiceDate))

#Add new fields
cbind(df,TransTime)
cbind(df,InvoiceNo)

#Combine data by Invoice No and Date (keys) to get single basket with multiple product descriptions (values)
#for every singular basket
baskets <- ddply(df, c("InvoiceNo","Date"),
                 function(data)paste(data$Description,
                                     collapse = ",")) #sep by comma


#Remove InvoiceNo and Date (no longer needed)
baskets$InvoiceNo <- NULL
baskets$Date <- NULL

#Set column names
colnames(baskets) <- c('items')

#Save then upload as transaction data 'trans'
write.csv(baskets, "C:/Users/leojames/Desktop/R Tutorials/baskets.csv", quote = F, row.names = F)
trans <- read.transactions("C:/Users/leojames/Desktop/R Tutorials/baskets.csv", format = 'basket', sep = ',')
#trObj <- as(dataframe.dat,'transactions')

#EDA of most frequent items
itemFrequencyPlot(trans, topN = 20, type = "absolute",col = brewer.pal(8,'Set1'), main = 'Absolute Item Frequency Plot')
itemFrequencyPlot(trans, topN = 20, type = "relative",col = brewer.pal(8,'Set1'), main = 'Relative Item Frequency Plot')

#Plot SubRules
plot(subRules)

#Apriori algorithm model to generate rules
#Minimum Support at 0.001, confidence at 0.8, maxlen @ 10
rules <- apriori(trans, parameter = list(supp = 0.001, conf = 0.8, maxlen = 10))
summary(rules)


#Print first 10 rules
inspect(rules[1:10])


#Apriori algorithm model to generate tules
#Change maxlen to 3
short.rules <- apriori(trans, parameter = list(supp = 0.001, conf = 0.8, maxlen = 3))
summary(short.rules)

#Remove redundant rules
subset.rules <- which(colSums(is.subset(rules, rules)) > 1) #create vector
length(subset.rules) 

subset.association.rules <- rules[-subset.rules] #remove subset / redundant rules

#Exploration of "Metal" Product Association Rules as concequent
metal.association.rules <- apriori(trans, parameter = list(supp = 0.001, conf = 0.8), 
                                   appearance = list(default = 'lhs', rhs = "METAL"))
inspect(metal.association.rules)

#Exploration of "Metal" Product Association Rules as antecedent
metal.association.rules <- apriori(trans, parameter = list(supp=0.001, conf=0.8),
                                   appearance = list(lhs="METAL",default="rhs"))
inspect(metal.association.rules)


#Subset rules with confidence greater than 40%
subRules<-rules[quality(rules)$confidence>0.4]

# Plot 1: Scatter Plot of subset 
plot(subRules)

#Plot 2: Scatter plot with order information
plot(subRules, method = 'two-key plot')

#Plot3: Interactive plot
plotly_arules(subRules)

#Plot4: Graph of top 10 rules with highest confidence
top10 <- head(subRules, n = 10, by = "confidence")
plot(top10, method = 'graph', engine = 'htmlwidget')
