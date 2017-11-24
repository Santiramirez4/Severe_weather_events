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
Rawdata <- read.csv("dataset.zip", header = TRUE, sep = ",", check.names = TRUE, stringsAsFactors=FALSE)
```


The variable **BGN_DATE** represent a Date and is being shown as "Factor". To convert to type: "Date"



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





## First Question

**The first Questions is to know which Event Type has been more harmful with respect to
population.**

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

From this table, we can assure that **HAIL** is the event that happens
the most in USA. Nevertheless, it does not guarantee that it is the most harmful for the 
population.

To validate the Harmfulness, the **"Fatalities"** and **"Injuries"** caused by the events will be
analyzed.



```r
FatalitiesInjuries <- subset(Rawdata2, select =  c(EVTYPE, FATALITIES, INJURIES))
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

Considering that the Fatal cases are worst as the Injuries, but without taking out of consideration
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

With this conisderation, the most harmful event is the **TORNADO**, followed by the **EXCESIVE HEAT**.


To be clear with the **"Weighted value"**, this result might vary according to the weight given
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


## Second Question

**Identify which types of events have the greatest economic consequences**

Identify which variables are related with Economic Consecuences:

* **PROPDMG**   : Property Damage. Number of Property damaged
* **PROPDMGEXP**: Property Damage Exponential. " ",-, ?, +, 0, 1, 2, 3, 4, 5, 6, 7, 8, B, h, H, K, m, M,
* **CROPDMG**   : Crop Damage. Number of Crop damage
* **CROPDMGEXP**: Crop Damage Exponential. ?,  0,  2,  B,  k,  K,  m,  M

According to a validation made by _Soesilo Wijono_ in an article published in RPubs,
[RPubs Link](https://rpubs.com/flyingdisc/PROPDMGEXP), the equivalents are as follows:

* H,h = hundreds = 100

* K,k = kilos = thousands = 1,000

* M,m = millions = 1,000,000

* B,b = billions = 1,000,000,000

* (+) = 1

* (-) = 0

* (?) = 0

* black/empty character = 0

* numeric 0..8 = 10


To avoid working with all complete data set, a new data set is created using only the 
necessary variables


```r
EconomicVariables <- subset(Rawdata2, select =  c(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
str(EconomicVariables)
```

```
## 'data.frame':	902297 obs. of  5 variables:
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
```


Using the previous table, the equivalents values are assigned accordingly for __Property__


```r
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "H"] <- 100
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "h"] <- 100
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "K"] <- 1000
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "k"] <- 1000
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "M"] <- 1000000
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "m"] <- 1000000
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "B"] <- 1000000000
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "b"] <- 1000000000
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "+"] <- 1
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "-"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "?"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == " "] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == ""] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "0"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "1"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "2"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "3"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "4"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "5"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "6"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "7"] <- 0
EconomicVariables$PROPEXP[EconomicVariables$PROPDMGEXP == "8"] <- 0
```

Using the previous table, the equivalents values are assigned accordingly for __Crop__


```r
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == ""] <- 0
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "M"] <- 1000000
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "K"] <- 1000
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "m"] <- 1000000
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "B"] <- 1000000000
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "?"] <- 0
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "0"] <- 0
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "k"] <- 1000
EconomicVariables$CROPEXP[EconomicVariables$CROPDMGEXP == "2"] <- 0
```

Having the equivalences for both _PROPERTY_ and _CROP_, we can calculate the final value


```r
EconomicVariables$PROPValue <- EconomicVariables$PROPDMG * EconomicVariables$PROPEXP
EconomicVariables$CROPValue <- EconomicVariables$CROPDMG * EconomicVariables$CROPEXP

# To see the top 5 Types of events with most Property Damage
head(arrange(EconomicVariables, desc(PROPValue)), 5)
```

```
##              EVTYPE PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP PROPEXP CROPEXP
## 1             FLOOD  115.00          B    32.5          M   1e+09   1e+06
## 2       STORM SURGE   31.30          B     0.0              1e+09   0e+00
## 3 HURRICANE/TYPHOON   16.93          B     0.0              1e+09   0e+00
## 4       STORM SURGE   11.26          B     0.0              1e+09   0e+00
## 5 HURRICANE/TYPHOON   10.00          B     0.0              1e+09   0e+00
##   PROPValue CROPValue
## 1 1.150e+11  32500000
## 2 3.130e+10         0
## 3 1.693e+10         0
## 4 1.126e+10         0
## 5 1.000e+10         0
```

```r
# To see the top 5 Types of events with most Crop Damage
head(arrange(EconomicVariables, desc(CROPValue)), 5)
```

```
##              EVTYPE PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP PROPEXP CROPEXP
## 1       RIVER FLOOD    5.00          B    5.00          B   1e+09   1e+09
## 2         ICE STORM  500.00          K    5.00          B   1e+03   1e+09
## 3 HURRICANE/TYPHOON    5.88          B    1.51          B   1e+09   1e+09
## 4           DROUGHT    0.00               1.00          B   0e+00   1e+09
## 5      EXTREME COLD    0.00             596.00          M   0e+00   1e+06
##   PROPValue CROPValue
## 1  5.00e+09  5.00e+09
## 2  5.00e+05  5.00e+09
## 3  5.88e+09  1.51e+09
## 4  0.00e+00  1.00e+09
## 5  0.00e+00  5.96e+08
```


### Answer Question 2
* In _Properties_, the biggest consecueces are due to **FLOOD** and for _Crops_ are due to
**River Flood**.
