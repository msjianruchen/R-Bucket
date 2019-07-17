

# Base-R ------------------------------------------------------------------


at<- matrix(1:9,nrow=3,ncol=3)
#at<- matrix(1:9,ncol=3,byrow=TRUE)
colnames(at)<-c("x1","x2","x3")

WINDOW_ID <- gsub("MSK(\\d{4})","MSK-P\\1", WINDOW_ID)

#use the shortcut Ctrl + Shift + H to browse to the desired directory.

# Getting and Cleaning Data -----------------------------------------------

#Parse the raw text file, figure out the structure, extract out
#Databases: mySQL, MongoDB
#Raw data->Processing script->tidy data->data analysis->data communication


# Introduction to dplyr ---------------------------------------------------
#A grammar of data manipulation (e.g. tbl, %>%)
#Use dplyr to analyze a data set of airline flight data containing flights that departed from Houston. This data is stored in a package called hflights.
library(dplyr)
library(hflights)
head(hflights)
summary(hflights)
dim(hflights) # discover the dimensionality of the data set

hflights
class(hflights) #assert the structure of dataset
hflights <- tbl_df(hflights) #Convert the hflights data.frame into a hflights tbl
glimpse(hflights) #Show data types & initial value of each columns in the dataset
hflights<- as.data.frame(hflights) #Recommended, data easier to look at
as_tibble(hflights) # derive a tbl from a data.frame structure

#Rename the UniqueCarrier
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental",
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways",
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier",
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

#Add a new Carrier column to hflights by combining lut with the UniqueCarrier column of hflights.
hflights$Carrier <- lut[hflights$UniqueCarrier]

# he lookup table
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")
#Use lut to change the labels of the CancellationCode column of hflights. Store the recoded vector in a new column Code
hflights$Code <- lut[hflights$CancellationCode]
hflights <- hflights %>% select(-CancellationCode)

glimpse(hflights) #Glimpse at hflights
#str(hflights)

# Select -------------------------------------------------------
#select() which returns a subset of the columns,
#filter() that is able to return a subset of the rows,
#arrange() that reorders the rows according to single or multiple variables,
#mutate() used to add columns from existing data,
#summarise() which reduces each group to a single row by calculating aggregate measures.
#verb focus: select and mutate manipulate variables | filter and arrange manipulate observations | summarize manipulates groups of observations

select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay) #Print out a tbl with the four columns of hflights related to delay
select(hflights, Origin:Cancelled) #Print out the columns Origin up to Cancelled of hflights
select(hflights, ends_with(c('Delay'))) #Print out a tbl containing just ArrDelay and DepDelay
#colnames(hflights)[grepl("Delay", colnames(hflights), ignore.case = TRUE)]
select(hflights, UniqueCarrier, ends_with(c('Num')), starts_with(c('Cancel')))
select(hflights, contains(c('Time')), contains(c('Delay')))

#Comparison to base R
ex1r <- hflights[c("TaxiIn","TaxiOut","Distance")]
ex1d <- select(hflights, contains(c('Taxi')), Distance)

ex2r <- hflights[c("Year","Month","DayOfWeek","DepTime","ArrTime")]
ex2d <- select(hflights, Year:ArrTime, -3)


# Mutate ------------------------------------------------------------------

g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime) # Add the new variable ActualGroundTime to a copy of hflights and save the result as g1.

g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut) # Add the new variable GroundTime to a g1. Save the result as g2.

# Add a second variable loss_percent to the dataset: m1
m1 <- mutate(hflights,
             loss = ArrDelay - DepDelay,
             loss_percent = (ArrDelay - DepDelay)/DepDelay * 100
)
glimpse(g3) # Print out g3

c2 <- mutate(c1, Date = paste(Year,Month,DayofMonth, sep="-")) # Combine the Year, Month and DayofMonth variables to create a Date column: c2

es<-resp[which(nchar(resp$Exam_Result_Id)>36),] #nchar

# Filter -----------------------------------------------
### Logical Operators ###
# All flights that traveled 3000 miles or more
filter(hflights, Distance >= 3000) %>% glimpse()

filter(hflights, UniqueCarrier %in% c('JetBlue', 'Southwest', 'Delta')) %>% glimpse() # All flights flown by one of JetBlue, Southwest, or Delta

filter(hflights, (TaxiIn + TaxiOut) > AirTime) %>% glimpse() # All flights where taxiing took longer than flying

temp1 <- filter(hflights, !is.na(ArrDelay)) # Remove rows that have NA ArrDelay: temp1

### Combining tests using Boolean Operators ( | & ) ###
filter(hflights, DepTime < 500 | ArrTime > 2200) %>% glimpse()
filter(hflights, DepDelay > 0 & ArrDelay < 0) %>% glimpse()
filter(hflights, Cancelled == 1 & DayOfWeek %in% c(6,7)) %>% glimpse() # All cancelled weekend flights
#How many weekend flights flew a distance of more than 1000 miles but had a total taxiing time below 15 minutes?
hflights %>%
  filter(
    Distance > 1000,
    DayOfWeek > 5,
    TaxiIn + TaxiOut < 15
  ) %>%
  glimpse()



# Arrange Verb ------------------------------------------------------------

dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay)) # Definition of dtc

arrange(dtc, DepDelay) %>% glimpse() # Arrange dtc by departure delays(smallest to largest)

arrange(dtc, UniqueCarrier, DepDelay) %>% glimpse() # Arrange dtc according to carrier (category data) and increasing departure delays

arrange(hflights, UniqueCarrier, desc(DepDelay)) %>% glimpse() # Arrange according to carrier and decreasing departure delays

arrange(hflights, (DepDelay + ArrDelay)) %>% glimpse() # Arrange flights by total delay (normal order).

hflights %>% filter(Dest == 'DFW', DepTime < 800) %>% arrange(desc(AirTime)) %>% glimpse() # Keep flights leaving to DFW before 8am and arrange according to decreasing AirTime



# Summarise ---------------------------------------------------------------

summarize(hflights, min_dist = min(Distance), max_dist = max(Distance)) # Print out a summary with variables min_dist and max_dist

### Aggregate ###

# min(x) - minimum value of vector x
# max(x) - maximum value of vector x
# mean(x) - mean value of vector x
# median(x) - median value of vector x
# quantile(x, p) - pth quantile of vector x
# sd(x) - standard deviation of vector x
# var(x) - variance of vector x
# IQR(x) - Inter Quartile Range (IQR) of vector x
# diff(range(x)) - total range of vector x

temp1 <- filter(hflights, !is.na(ArrDelay))
summarize(temp1,
          earliest = min(ArrDelay),   # Generate summary about ArrDelay column of temp1
          average = mean(ArrDelay),
          latest = max(ArrDelay),
          sd = sd(ArrDelay)
)

temp2 <- filter(hflights, !is.na(TaxiIn) & !is.na(TaxiOut))
summarise(temp2, max_taxi_diff = max(abs(TaxiIn - TaxiOut))) # Print the maximum taxiing difference of temp2 with summarise()


### dplyr aggregate functions ###

# first(x) - The first element of vector x
# last(x) - The last element of vector x
# nth(x, n) - The nth element of vector x
# n() - The number of rows in the data.frame or group of observations that summarise() describes
# n_distinct(x) - The number of unique values in vector x


summarise(hflights,
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest),                   # Generate summarizing statistics for hflights
          dest100 = nth(Dest, 100)
)


aa <- filter(hflights, UniqueCarrier == 'American') # Filter hflights to keep all American Airline flights: aa
summarise(aa,
          n_flights = n(),
          n_canc = sum(Cancelled),
          p_canc = n_canc/n_flights * 100,        # Generate summarizing statistics for aa
          avg_delay = mean(ArrDelay, na.rm=T)
)


# %>% Pipe Operator -------------------------------------------------------

hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarize(avg = mean(diff))

# Filter and summarise d according to the instructions
d %>%
  filter(
    !is.na(mph),
    mph < 70
  ) %>%
  summarize(
    n_less = n(),
    n_dest = n_distinct(Dest),
    min_dist = min(Distance),
    max_dist = max(Distance)
  )

#using a combination of dplyr verbs and %>%
hflights %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
  summarise(n_non = n(),
            p_non = n_non / nrow(hflights) * 100,
            n_dest = n_distinct(Dest),
            min_dist = min (Distance),
            max_dist = max(Distance))

# group_by ----------------------------------------------------------------

# e.g.1 Make an ordered per-carrier summary of hflights
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(
    n_flights = n(),
    n_canc = sum(Cancelled),
    p_canc = n_canc/n_flights * 100,
    avg_delay = mean(ArrDelay, na.rm=T)
  ) %>%
  arrange(avg_delay, p_canc)

#e.g.2
hflights %>%
  filter(!is.na(ArrDelay)) %>%
  group_by(UniqueCarrier) %>%
  summarize(p_delay = sum(ArrDelay > 0)/n()) %>%
  mutate(rank = rank(p_delay)) %>%
  arrange(rank)


# dplyr and database ------------------------------------------------------

library(data.table)
# Convert hflights to a data.table
class(hflights)
hflights2 <- as.data.table(hflights)
class(hflights2)


#dplyr and mySQL databases
library(RMySQL)
library(dbplyr)

# Set up a connection to the mysql database
my_db <- src_mysql(dbname = "dplyr",
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com",
                   port = 3306,
                   user = "student",
                   password = "datacamp")

# Reference a table within that source: nycflights
nycflights <- tbl(my_db, "dplyr")


# Mutating Joins with dplyr----------------------------------------------------------

### Keys ###
# The Primary key needs to be unique in a table
# The foreign key in the second table can be duplicated
# second table will be matched to the primary table based on the primary key
# The primary key may be one, two or even more columns in the table

#e.g. https://rpubs.com/williamsurles/293454
bands2 <- left_join(bands, artists, by = c("first", "last"))
bands3 <- right_join(artists, bands, by = c("first", "last"))
setequal(bands2, bands3)
inner_join(songs, albums, by = "album")
full_join(artists, bands, by = c("first","last"))
# left_join - prioritizes left dataset
# right_join - prioritizes right dataset
# inner_join - only retains rows in both datasets
# full_join - retains all rows
# Use %>% (pipes) to string together these joins


temp <- left_join(bands, artists, by = c("first", "last"))
temp <- filter(temp, instrument == "Guitar")
select(temp, first, last, band)
#Equivalent to
bands %>%
  left_join(artists, by = c("first","last")) %>%
  filter(instrument == "Guitar") %>%
  select(first, last, band)


# Set operations ----------------------------------------------------------

# union() will return every row that appears in one or more of the datasets.If a row appears multiple times union() will only return it once
# interesect() will return only the rows that appear in both datasets
# setdiff() will return the rows that appear in the first dataset but not the second

aerosmith %>%
  # Create the new dataset using a set operation
  union(greatest_hits) %>%
  # Count the total number of songs
  nrow()

x <- c(sort(sample(1:20, 9)), NA)
y <- c(sort(sample(3:23, 7)), NA)
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y) # if one data set is the same as another dataset
identical(x,y) #Check if same order &same row









