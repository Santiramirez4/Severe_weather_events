# NOAA Storm Database - Analysis: Severe weather events
Santiago R  
21 November 2017  



# Questions:

The data Analysis will address the following questions:


- Across the United States, which types of events (as indicated in the EVTYPE variable) are
most harmful with respect to population health?


- Across the United States, which types of events have the greatest economic consequences?

# Synopsis

Add here the Synopsis! **Pending**

# Data processing

## Reading the Data


```r
# for Reading Files *ZIP
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, "dataset.zip")
Rawdata <- read.csv("dataset.zip", header = TRUE, sep = ",", check.names = TRUE)

#Check data
str(Rawdata)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

BGN_DATE represent a Date and is being shown as "Factor". TO convert to type: "Date"


```r
#Make a copy of data to avoid reading again the file
Rawdata2 <- Rawdata
Rawdata2$BGN_DATE <- as.Date(Rawdata2$BGN_DATE, "%m/%d/%Y")
#validate if type of data for "Date" has changed
str(Rawdata2$BGN_DATE)
```

```
##  Date[1:902297], format: "1950-04-18" "1950-04-18" "1951-02-20" "1951-06-08" "1951-11-15" ...
```

One of the Questions is to know which Event Type has been more harmful with respect to
population. 

The first approach is to validate wich Event Type has occurred more times


```r
EventTypeSummary <- table(Rawdata2$EVTYPE)
EventTypeSummary <- data.frame(EventTypeSummary)
EventTypeSummary <- arrange(EventTypeSummary, desc(Freq))
head(EventTypeSummary, 5)
```

```
##                Var1   Freq
## 1              HAIL 288661
## 2         TSTM WIND 219940
## 3 THUNDERSTORM WIND  82563
## 4           TORNADO  60652
## 5       FLASH FLOOD  54277
```
From this table, we can think that HAIL is the event that happens
the most in USA. Nevertheless, it does not guarantee that it is the most harmful for the 
population.

To validate the Harmfulness, the "Fatalities" and "Injuries" caused by the events will be
analyzed.


```r
FatalitiesInjuries <- subset(Rawdata2, select =  c(STATE, BGN_DATE, EVTYPE, FATALITIES, INJURIES))
EvFatalities <- aggregate(FatalitiesInjuries$FATALITIES, by = list(FatalitiesInjuries$EVTYPE), FUN = sum)
EvInjuries <- aggregate(FatalitiesInjuries$INJURIES, by = list(FatalitiesInjuries$EVTYPE), FUN = sum)
#Order the Data from Higher to lower to see the events with more fatal and injure events:

EvFatalities <- arrange(EvFatalities, desc(x))
EvInjuries <- arrange(EvInjuries, desc(x))
HarmfulEvents <- merge(EvFatalities, EvInjuries, by = "Group.1", all = TRUE)
HarmfulEvents <- arrange(HarmfulEvents, desc(x.x))
names <- c("EVTYPE", "FATALITIES", "INJURIES")
colnames(HarmfulEvents) <- names
head(HarmfulEvents, 5)
```

```
##           EVTYPE FATALITIES INJURIES
## 1        TORNADO       5633    91346
## 2 EXCESSIVE HEAT       1903     6525
## 3    FLASH FLOOD        978     1777
## 4           HEAT        937     2100
## 5      LIGHTNING        816     5230
```

Here, one can see that **TORNADO** and **EXCESSIVE HEAT** have the higher values in Fatalities.
Comparing with the frequency of the events (Table shown above "EventTypeSummary"), the 
Frequency of the events is not directly related with the Harmfulness. 

Considering that the Fatal cases are worst as the Injuries, but without taking out consideration
the injuries, a Weighted calculation is made, where a weight of "5" is given to "Fatality"
and a Weight of "1" to "Injury"


```r
HarmfulEvents2 <- HarmfulEvents
HarmfulEvents2 <- HarmfulEvents2 %>% mutate(WeightedValue = FATALITIES*5 + INJURIES)
HarmfulEvents2 <- arrange(HarmfulEvents2, desc(WeightedValue))
head(HarmfulEvents2, 5)
```

```
##           EVTYPE FATALITIES INJURIES WeightedValue
## 1        TORNADO       5633    91346        119511
## 2 EXCESSIVE HEAT       1903     6525         16040
## 3      TSTM WIND        504     6957          9477
## 4      LIGHTNING        816     5230          9310
## 5          FLOOD        470     6789          9139
```
With this conisderation, the most harmful event is the **TORNADO**, followed by the **EXCESIVE HEAT**
To be clear with the "Weighted value", this result might vary according to the weight given
to the "FATALITY". To validate its impact, another calculation with a higher weight is made to see
if the top 3 events vary:


```r
HarmfulEvents2 <- HarmfulEvents
HarmfulEvents2 <- HarmfulEvents2 %>% mutate(WeightedValue = FATALITIES*10 + INJURIES)
HarmfulEvents2 <- arrange(HarmfulEvents2, desc(WeightedValue))
head(HarmfulEvents2, 3)
```

```
##           EVTYPE FATALITIES INJURIES WeightedValue
## 1        TORNADO       5633    91346        147676
## 2 EXCESSIVE HEAT       1903     6525         25555
## 3      LIGHTNING        816     5230         13390
```

### Answer Question 1
* It is clear that **TORNADO** and **EXCESIVE HEAT** are the 2 most harmful events in the USA.
