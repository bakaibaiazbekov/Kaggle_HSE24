
----------------------------------------------------------------------------------------------------------------
  # Install package "pastecs,"
  install.packages("pastecs")
install.packages("VIM")
install.packages("dplyr")

# Load it into working memory
library(pastecs) 
#NAs to zero in Menge_call & Menge_Ecom
train3$MENGE_ECOM[is.na(train3$MENGE_ECOM)] <- 0
train3 %>% select(5, 18)
head(melt(train3 %>% select(5, 18), id = "SHOW_DATUM"))
melt(train3 %>% select(5, 18), id = "SHOW_DATUM")[1:10, 3]
melt(train3 %>% select(5, 18), id = "SHOW_DATUM")[1:10,]

ggplot(melt(train3 %>% select(5, 18), id = "SHOW_DATUM")[1:10,], aes(x = SHOW_DATUM, y = value, color= variable)) + geom_line(size = 2) + geom_point(size=5) + ylab ("Number of item sold by phone") 


# convert date info in format 'mm/dd/yyyy'
dates <- as.Date(train3$SHOW_DATUM, "%d.%m.%y")
train3$weekday = weekdays(train3$SHOW_DATUM)
WeekdaysCounts = as.data.frame(table(train3$weekday))
train3$Time = train3$Time
x <- chron(times = train3$Time)
ggplot(WeekdaysCounts, aes(x= Var1, y = Freq)) + geom_line(aes(group = 1)) + xlab("Day of the week") + ylab("Total Sold Outs")

WeekdaysCounts$Var1 = factor(WeekdaysCounts$Var1, ordered = TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Adding the Hour of the Day

# Create a counts table for the weekday and hour:
Time <- as.POSIXct(DayHourCounts$Var2, "%H:%M:%S")
DayHourCounts = as.data.frame(table(train3$weekday, train3$Time))
# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts, aes(x=Hour, y = Freq)) + geom_line(aes(group=Var1, color = Var1))

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=1) 

# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=1, alpha=0.5) 

# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total SOLD_OUTS") + theme(axis.title.y = element_blank())

# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total SOLD_OUTS", low="white", high="red") + theme(axis.title.y = element_blank())


#KANAL
ChannelCounts = as.data.frame(table(train3$KANAL, train3$MENGE_CALL))
ggplot(ChannelCounts, aes(x= Var1, y = Freq)) + geom_line(aes(group = 1)) + xlab("KANAL") + ylab("Total Sold Outs") + geom_text(aes(label = Freq), vjust = -0.3) + theme_pubclean()
-----------------------------------------------------------------------------------------------------------------
  # Clustering thing 
  library(plyr)
#seperate into 2 tables FARBE and freq
y = count(training, "FARBE")
#view new data frame y
y
# Compute distances
distances = dist(y$freq, method = "euclidean")
# Hierarchical clustering
clusterY = hclust(distances, method = "ward") 
# Plot the dendrogram
plot(clusterY)
#rectangle 
rect.hclust(clusterY, k = 6, border = "red")


# Assign points to clusters
clusterGroups = cutree(clusterY, k = 6)
# to compute the percentage of y in each colour and cluster
tapply(y$freq, clusterGroups, mean)

# Find which cluster colour OLIV is.
subset(y, freq =="185")
clusterGroups[62]

# Create a new data set with just the y from cluster 2
cluster2 = subset(y, clusterGroups==2)
# Look at the first 10 colours in this cluster:
cluster2$freq[1:10]

# Create a new data set with just the y from cluster 3
cluster3 = subset(y, clusterGroups==3)
# Look at the first 10 colours(or 20 it doesn't matter) in this cluster:
cluster3$freq[1:10]
# check in which clusters this colour
subset(y, freq =="1471")
clusterGroups3[1471]
#Create new column for cluster group 
y["clustergroup"] <- NA
y$clustergroup <- clusterGroups
#Adding a data frame to another 
trainingFULL <- merge(training, y, all = TRUE)

y.long <- melt(y, id = c("FARBE", "freq"))

hheatmap.plot <- ggplot(data = y.long, aes(x = value, y = freq)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2() +
  theme(axis.text.y = element_text(size = 6)) + xlab("Number of Clusters")

print(heatmap.plot)



rstudioapi::documentSave()
------------------------------------------------------------------------------ 
  #Daten einlesen
  
  library(readr)
OGdata <- read.csv(file.choose(), header = FALSE)
View(OGdata)

----------------------------------------------------------------------------------------------------------------
  # remove ID
  training = training[ , -c(1)]
# a function to calculate % of missing data

MissingPercentage <- function(x){ sum(is.na(x)/length(x)*100 )} 
sort( apply(training, 2, MissingPercentage ), decreasing = TRUE )

# check number of NA
sort(sapply(training, function(x) sum(is.na(x))) , decreasing = TRUE )
# Visualizing Mising data and delete
library(VIM)
aggr_plot <- aggr(training, col = c( 'blue', 'red'),
                  numbers= TRUE , sortVars= TRUE , labels = names(training) , 
                  cex.axis= .7, gap= 3 , ylab = c("Histogram of missing data", "Pattern")
)
# Delete columns with more than 5% missing data
library(dplyr)
training = select(training, -c("GROESSE" , "FARBE", "ARTIKEL_ID", "BESTELL_ID" , "SHOW_POSITION" , "AIRING_23_FLG" , "AIRING_456_FLG" ,
                               "WG_ID" , "WGH1_ID" , "WGH3_ID" , "WGH4_ID" , "WG_DESC" ,"WGH1_DESC" , "DIVISION_DESC_SORT" , "WGH2_DESC", "WGH3_DESC" , "PREISKLASSE_DESC", "PREIS_LABEL_DESC", "WGH4_DESC"))





#Lister aller meiner Variablen

ls(X1_training_data)


#Alle 6662 Varibalen sind mode, also droppen da es keinen mehrwert bietet ohne die genaueren beschreibungen
table(updated_1$DIVISION_DESC_SORT)

#Subsetting data: excluding data

varupdated_1 <- names(OGdata) %in% c("GROESSE" , "FARBE", "ARTIKEL_ID", "BESTELL_ID" , "SHOW_POSITION" , "AIRING_23_FLG" , "AIRING_456_FLG" ,
                                     "WG_ID" , "WGH1_ID" , "WGH3_ID" , "WGH4_ID" , "WG_DESC" ,"WGH1_DESC" , "DIVISION_DESC_SORT" , "WGH2_DESC", "WGH3_DESC" , "PREISKLASSE_DESC", "PREIS_LABEL_DESC", "WGH4_DESC")
updated_1 <- OGdata[!varupdated_1]

ls(updated_1)

---------------------------------------------------------------------------------
  
  
  # Die Varibale KAnal von den einzelnen Sendern in nummer Umwanlden, Mit table kann die transformation gechked werden.
  
  #Transform Variable Kanal
  
  updated_1$KANAL[updated_1$KANAL == "ATV"] <- "1"
updated_1$KANAL[updated_1$KANAL == "DIG"] <- "2"
updated_1$KANAL[updated_1$KANAL == "TRE"] <- "3"

table(updated_1$KANAL)

# Transformation of the Show DAtes - Seperation Into Date and Time


#Creating new columns for date - removing time  

updated_1$Date <- as.Date(updated_1$SHOW_DATUM, format = "%d.%m.%Y")

updated_1$SHOW_DATUM <- NULL


------------------------------------------------------------------------------------------------------------
  #DEaling with NA, Entfernen oder den Durchschnitt annehmen.
  # Information over missing Data http://uc-r.github.io/missing_values
  
  
  
  #Checks and sums the NA for every collum
  colSums(is.na(updated_1))

#We have missing DAta in:
# Brand_ID
# Menge_Call
# Menge_Ecom
# PREIS_LABEL_DESC
# Bewertung
# Preis_Discount


#Check if it is senensible to ommited, use the median or mean for the variable
table(updated_1$BRAND_ID)

table(updated_1$PREIS_DISCOUNT)

table(updated_1$MENGE_CALL)

stat.desc(updated_1$MENGE_CALL)



#This Formula updates varibales of Preis-Discount to the median of the preisdiscount. Arguments for the mean could be made.

#SEt to median (for not particualr reason)

updated_1$PREIS_DISCOUNT <- (updated_1$PREIS_DISCOUNT/100)

updated_1$PREIS_DISCOUNT[is.na(updated_1$PREIS_DISCOUNT)] <- median(updated_1$PREIS_DISCOUNT, na.rm = TRUE)


updated_1$BEWERTUNG[is.na(updated_1$BEWERTUNG)] <- median(updated_1$BEWERTUNG, na.rm = TRUE)


#Set to mean

updated_1$MENGE_CALL[is.na(updated_1$MENGE_CALL)] <- mean(updated_1$MENGE_CALL, na.rm = TRUE)

updated_1$MENGE_ECOM[is.na(updated_1$MENGE_ECOM)] <- mean(updated_1$MENGE_ECOM, na.rm = TRUE)


#For simplicity I will change all NAs into 0, so we dont loose any rows

updated_1$BRAND_ID[is.na(updated_1$BRAND_ID)] <- "0"

updated_1$PREIS_LABEL_DESC[is.na(updated_1$PREIS_LABEL_DESC)] <- "0"  

----------------------------------------------------------------------------------------------------------------
  #Transforming Classification Dataset 
  
  library(readr)
Classdata <- read_csv("/Users/osaid/Desktop/Data\ Science/OG\ Class.csv")
View(Classdata)

----------------------------------------------------------------------------------------------------------------
  
  #Subsetting data: excluding data
  
  varupdated_1Class <- names(Classdata) %in% c("GROESSE" , "FARBE", "ARTIKEL_ID", "BESTELL_ID" , "SHOW_POSITION" , "AIRING_23_FLG" , "AIRING_456_FLG" ,
                                               "WG_ID" , "WGH1_ID" , "WGH3_ID" , "WGH4_ID" , "WG_DESC" ,"WGH1_DESC" , "DIVISION_DESC_SORT" , "WGH2_DESC", "WGH3_DESC" , "PREISKLASSE_DESC", "PREIS_LABEL_DESC", "WGH4_DESC") 
updated_1Class <-  Classdata[!varupdated_1Class]

ls(updated_1Class)

# Die Varibale KAnal von den einzelnen Sendern in nummer Umwanlden, Mit table kann die transformation gechked werden.

#Transform Variable Kanal

updated_1Class$KANAL[updated_1Class$KANAL == "ATV"] <- "1"
updated_1Class$KANAL[updated_1Class$KANAL == "DIG"] <- "2"
updated_1Class$KANAL[updated_1Class$KANAL == "TRE"] <- "3"

#Creating new columns for date and time 

updated_1Class$Date <- as.Date(updated_1Class$SHOW_DATUM, format = "%d.%m.%Y")
View(updated_1)
class(updated_1$Datum)
updated_1Class$SHOW_DATUM <- NULL


#Checks and sums the NA for every collum
colSums(is.na(updated_1Class))

#We have missing DAta in:
# Brand_ID
# Menge_Call
# Menge_Ecom
# PREIS_LABEL_DESC
# Bewertung
# Preis_Discount


#Check if it is senensible to ommited, use the median or mean for the variable
table(updated_1Class$BRAND_ID)

table(updated_1Class$PREIS_DISCOUNT)

table(updated_1Class$MENGE_CALL)

stat.desc(updated_1Class$MENGE_CALL)



#This Formula updates varibales of Preis-Discount to the median of the preisdiscount. Arguments for the mean could be made.

#SEt to median (for not particualr reason)


updated_1Class$PREIS_DISCOUNT[is.na(updated_1Class$PREIS_DISCOUNT)] <- median(updated_1Class$PREIS_DISCOUNT, na.rm = TRUE)


updated_1Class$BEWERTUNG[is.na(updated_1Class$BEWERTUNG)] <- median(updated_1Class$BEWERTUNG, na.rm = TRUE)


#Set to mean

updated_1Class$MENGE_CALL[is.na(updated_1Class$MENGE_CALL)] <- mean(updated_1Class$MENGE_CALL, na.rm = TRUE)

updated_1Class$MENGE_ECOM[is.na(updated_1Class$MENGE_ECOM)] <- mean(updated_1Class$MENGE_ECOM, na.rm = TRUE)


#For simplicity I will change all NAs into 0, so we dont loose any rows

updated_1Class$BRAND_ID[is.na(updated_1Class$BRAND_ID)] <- "0"

updated_1Class$PREIS_LABEL_DESC[is.na(updated_1Class$PREIS_LABEL_DESC)] <- "0"


----------------------------------------------------------------------------------------------------------------
  
  
  #Model
  
  #Loading required libraries
  
  library(tidyverse)
library(recipes)
library(skimr)
library(h2o)

#Loading data

----------------------------------------------------------------------------------------------------------------
  
  # Training data: Separate into x and y tibbles
  x_train <- updated_1 %>% select(SOLD_OUT_FLG)
y_train <- updated_1 %>% select(SOLD_OUT_FLG) 


# Classification/Test data: What we compare our model on
x_test  <- updated_1Class

#Taking a look at the variables and their types

glimpse(x_train)
glimpse(y_train)
glimpse(x_test)

----------------------------------------------------------------------------------------------------------------
  
  #Check for missing data 
  
  missing_tbl <- x_train %>%
  summarize_all(.funs = ~ sum(is.na(.)) / length(.)) %>%
  gather() %>%
  arrange(desc(value)) %>%
  filter(value > 0)
missing_tbl

----------------------------------------------------------------------------------------------------------------
  
  #Turning variables with high unique observations (>7) to be converted into factors
  
  unique_numeric_values_tbl <- x_train %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  mutate(key = as_factor(key))

unique_numeric_values_tbl

----------------------------------------------------------------------------------------------------------------
  
  #Applying the factor limit 
  
  factor_limit <- 7

num_2_factor_names <- unique_numeric_values_tbl %>%
  filter(value < factor_limit) %>%
  arrange(desc(value)) %>%
  pull(key) %>%
  as.character()

num_2_factor_names

----------------------------------------------------------------------------------------------------------------
  
  #Transformations
  
  #Converting from Character strings to Factors
  
  string_2_factor_names <- x_train %>%
  select_if(is.character) %>%
  names()

string_2_factor_names  

rec_obj <- recipe(~ ., data = x_train) %>%
  step_string2factor(string_2_factor_names) %>%
  step_num2factor(num_2_factor_names)%>%
  prep(stringsAsFactors = FALSE)

rec_obj


x_processed_train <- bake(rec_obj, x_train) 
x_processed_test  <- bake(rec_obj, x_test)
glimpse(x_processed_test) #Checking to make sure

#Transforming Preis Discount Variable 

x_processed_test$PREIS_DISCOUNT <- as.double(x_processed_test$PREIS_DISCOUNT)
x_processed_test$PREIS_DISCOUNT <- as.numeric(x_processed_test$PREIS_DISCOUNT)

#Converting the target variable to a factor as this is required for h2o auto-machine learning modelling 

y_processed_train <- y_train %>%
  mutate(SOLD_OUT_FLG = SOLD_OUT_FLG %>% as.numeric() %>% as.factor())
glimpse(y_processed_train) #Checking to make sure 

----------------------------------------------------------------------------------------------------------------
  
  #Neat trick I found to remove old data to clear up memory 
  
  rm(rec_obj)
rm(x_train)
rm(x_test)
rm(y_train)

----------------------------------------------------------------------------------------------------------------
  
  #Modelling 
  
  #Start up h2o
  
  h2o.init()

#Turn the datasets into h2o frames but first bind them together 

data_h2o <- as.h2o(bind_cols(y_processed_train, x_processed_train))

#Split the datasets into train, test and validation data

splits_h2o <- h2o.splitFrame(data_h2o, ratios = c(0.7, 0.15), seed = 1234)

train_h2o <- splits_h2o[[1]]
valid_h2o <- splits_h2o[[2]]
test_h2o  <- splits_h2o[[3]]

y <- "SOLD_OUT_FLG"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 90
) #Lower the max_runtime if you want a quicker prediction :D

automl_leader <- automl_models_h2o@leader

#Check which models were at the top 

lb <- automl_models_h2o@leaderboard
print(lb, n = nrow(lb))

#Evaluating performance of the h2o model

performance_h2o <- h2o.performance(automl_leader, newdata = test_h2o)

performance_h2o %>%
  h2o.confusionMatrix()

prediction_h2o <- h2o.predict(automl_leader, newdata = as.h2o(x_processed_test))

#Export

prediction_tbl <- prediction_h2o %>%
  as.tibble() %>%
  bind_cols(
    x_processed_test %>% select(OBS_ID)
  ) %>%
  select(OBS_ID, p1) %>%
  rename(SOLD_OUT_FLG = p1)

prediction_tbl$col1 <- NULL

prediction_tbl

write.csv(prediction_tbl, "/Users/osaid/Desktop/Data\ Science/Predictions.csv", row.names = FALSE)

----------------------------------------------------------------------------------------------------------------
  
  