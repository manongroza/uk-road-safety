setwd("C:/Users/Manon/Desktop/uk-road-safety-accidents-and-vehicles")
c <- read.csv(file="Accident_Information.csv", header=TRUE, sep=",")
head(c)
names(c)
dim(c)

cc <- as.data.frame(c)
cc


dd <- subset(cc, Year >= 2006 & Year <= 2016)
dd
names(dd)
dim(dd)
View(dd)
range(dd[33]) 

#keep relevant columns
ee <- dd[c(1, 6, 7, 8, 9, 11, 13, 14, 15, 19, 20, 22, 26, 27, 28, 29, 30, 31, 32, 33)]
ee

length(ee)
names(ee)

View(ee)

#Vehicle database
x <- read.csv(file="Vehicle_Information.csv", header=TRUE, sep=",")
head(x)
names(x)
dim(x)

xx <-as.data.frame(x)
xx


yy <- subset(xx, Year >= 2006 & Year <= 2016)
yy
names(yy)
dim(yy)
View(yy)
range(yy[24]) 


#keep relevant columns
zz <- yy[c(1, 2, 6, 7, 14, 15, 19, 24)]
zz

length(zz)
names(zz)

View(zz)


mm <- merge(ee, zz, by= "Accident_Index")
mm
View(mm)
dim(ee)
dim(zz)
dim(mm)

attach(mm)

ABS_District <- table(Local_Authority_.District.)
dim(ABS_District)
View(ABS_District)
ABS_District1 <- sort(ABS_District, decreasing = TRUE)
hist(ABS_District)
top10 <- c(ABS_District1)
View(ABS_District1)
top10a <- ABS_District1[1:10]
sumtop10a <- sum(top10a)
others <- c(sum(ABS_District1)-sumtop10a)
top10a
others
View(top10a)
detach(mm)



#data from 2006:
data2006 <- subset(mm, Year.y == 2006)
View(data2006)
attach(data2006)

ABS_District2006 <- table(Local_Authority_.District.)
ABS_District1_2006 <- sort(ABS_District2006, decreasing = TRUE)
hist(ABS_District2006)
top10_2006 <- c(ABS_District1_2006)
View(ABS_District1_2006)
top10a_2006 <- ABS_District1_2006[1:10]
sumtop10a_2006 <- sum(top10a_2006)
others <- c(sum(ABS_District1_2006)-sumtop10a_2006)
top10a_2006
others
View(top10a_2006)
detach(data2006)

#data from 2016:
data2016 <- subset(mm, Year.y == 2016)
View(data2016)
attach(data2016)

ABS_District2016 <- table(Local_Authority_.District.)
ABS_District1_2016 <- sort(ABS_District2016, decreasing = TRUE)
hist(ABS_District2016)
top10_2016 <- c(ABS_District1_2016)
View(ABS_District1_2016)
top10a_2016 <- ABS_District1_2016[1:10]
sumtop10a_2016 <- sum(top10a_2016)
others <- c(sum(ABS_District1_2016)-sumtop10a_2016)
top10a_2016
others
View(top10a_2016)

#data from 2011:
data2011 <- subset(mm, Year.y == 2011)
View(data2011)
attach(data2011)

ABS_District2011 <- table(Local_Authority_.District.)
ABS_District1_2011 <- sort(ABS_District2011, decreasing = TRUE)
hist(ABS_District2011)
top10_2011 <- c(ABS_District1_2011)
View(ABS_District1_2011)
top10a_2011 <- ABS_District1_2011[1:10]
sumtop10a_2011 <- sum(top10a_2011)
others <- c(sum(ABS_District1_2011)-sumtop10a_2011)
top10a_2011
others
View(top10a_2011)

#import file with top 10 in 2016
top2016 <- read.csv(file="top2016.csv", header=TRUE, sep=";", dec = "," )
View(top2016)


x11()


#create table for Birmingham

View(mm)

Birmingham <- subset(mm, Local_Authority_.District. == 'Birmingham')

View(Birmingham)
typeof(Birmingham$Date)

#severity in 2016 for Birmingham
Birmingham2016 <- subset(Birmingham, Year.y ==2016)
Severity <- table(Birmingham2016$Accident_Severity)
SeverityRel <- table(Birmingham2016$Accident_Severity)/length(Birmingham2016$Accident_Severity)
round(SeverityRel, digits = 2)
View(SeverityRel)
pie(SeverityRel)


Birmingham2011 <- subset(Birmingham, Year.y =2011)
Severity2011 <- table(Birmingham2011$Accident_Severity)
SeverityRel2011 <- table(Birmingham2011$Accident_Severity)/length(Birmingham2011$Accident_Severity )
round(SeverityRel2011, digits = 2)
View(SeverityRel2011)

Birmingham2016 <- c(Birmingham2011$Var1, Birmingham2011$Freq)
view(Birmingham2016)

hist(Birmingham$Year.y & Birmingham$Accident_Severity, plot=TRUE, mian="hist", xlab = "Year", ylab= "Number of accidents", col="light blue", breaks = seq(2006, 2016, 1))



library(ggplot2)
View(Birmingham)
ggplot(Birmingham, aes(x=Year.y , fill=Accident_Severity)) +
       geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 20, ) +
       scale_color_manual(values = c("red", "blue", "green")) +
       scale_fill_manual(values = c("red", "blue", "green"))

#Count the number of accidents per year from 2006 to 2016 in Birmingham


noa2016 <- Birmingham$Accident_Index[Birmingham$Year.y == 2016]
table_number_of_accidents_2016 <- as.data.frame(noa2016)
number_of_accidents_2016 <- dim(table_number_of_accidents_2016)
number_of_accidents_2016

#Count the number of accidents per year from 2006 to 2016 in total

totnoa2016 <- mm$Accident_Index[mm$Year.y == 2016]
table_number_of_tot_accidents_2016 <- as.data.frame(totnoa2016)
tot_number_of_accidents_2016 <- dim(table_number_of_tot_accidents_2016)
tot_number_of_accidents_2016


# Frequency of number of vehicles involved in the accident
Freq(Birmingham$Number_of_Vehicles, breaks = hist(Birmingham$Number_of_Vehicles, plot = FALSE)$breaks, include.lowest = TRUE,
     ord = c("level", "desc", "asc", "name"),
     useNA = c("no", "ifany", "always"), ...)
Number_of_Vehicles_freq = factor(Birmingham$Number_of_Vehicles)
dummies = model.matrix(~Number_of_Vehicles_freq)

library(dummies)

Number_of_Vehicles_freq1 <- data.frame(Birmingham$Number_of_Vehicles, Birmingham$Year.y == 2016)
Number_of_Vehicles_freq1 
Number_of_Vehicles_freq1 <- cbind(Birmingham, dummy(Birmingham$Year.y, sep = "_"))


dummy <- NULL
dummy[Birmingham$col == Birmingham$Number_of_Vehicles] = 0
dummy[Birmingham$col == ] = 1

Road_Conditions <- table(Birmingham2016$Road_Surface_Conditions)
Road_Conditions_Rel <- table(Birmingham$Road_Surface_Conditions)/length(Birmingham$Road_Surface_Conditions)


#Road conditions Pie chart w/o %

#Basic pie chart
Road_Conditions <- table(Birmingham2016$Road_Surface_Conditions)
Road_Conditions_Rel <- table(Birmingham$Road_Surface_Conditions)/length(Birmingham$Road_Surface_Conditions)
round(Road_Conditions_Rel, digits = 2)
View(SeverityRel)
pie(Road_Conditions_Rel)

library(ggplot2)
# Ggplot pie chart
Road_Conditions_Rel <- data.frame(Road_Conditions_Rel)
Road_Conditions_Rel <- Road_Conditions_Rel[-1,]
ggplot(Road_Conditions_Rel, aes(x="", y=Road_Conditions_Rel$Freq, fill=Road_Conditions_Rel$Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
   coord_polar("y", start=0) +
  labs(x= NULL, y=NULL, fill = "Road conditions", title = "Pie chart of road conditions during accidents in 2016")+
  theme_void()+
  scale_fill_brewer(palette="Set3") 

roadcond <- Birmingham$Road_Surface_Conditions[Birmingham$Year.y == 2016]
roadcondtable <- as.data.frame(table(Birmingham2016$Road_Surface_Conditions))
roadcondtable
tot <- table(Birmingham2016$Road_Surface_Conditions)
tot
dry
dim(Birmingham2016)
114+3999+4+35+0+1108

3999/5260 #Dry
114/5260 # Data missing or out of range
4/5260 #Flood
35/5260 #Frost or ice
1/5260 #Snow
1108/5260 #Wet or damp

# simple pie
slices <- c(72, 26, 1, 00.04, 0.2,0.6)
lbls <- c("Dry", "Wet or damp", "Frost or ice", "Flood over 3cm. deep", "Snow","Data missing or out of range")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col= rainbow(length(lbls)), main="Pie chart of road conditions in 2016")


#another possibility
slices <- c(0.72, 0.26, 0.01, 0.0004, 0.002,0.006)
lbls <- c("Dry", "Wet or damp", "Frost or ice", "Flood over 3cm. deep", "Snow","Data missing or out of range")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
df = data.frame(slices = slices,labels =  lbls)
ggplot(df,aes(x = factor(1),fill = labels))+
  geom_bar(width = 1,)+
  coord_polar(theta = "y")+
  theme(axis.title = element_blank())


#Weather conditions Pie chart w/o %
#Count the % of each weather condition
weathercondtable <- as.data.frame(table(Birmingham2016$Weather_Conditions))
weathercondtable
tot <- table(Birmingham2016$Weather_Conditions)
tot
565+4166+529
dim(Birmingham2016)

#put the data together

library(plyr)
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Unknown"="Other"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Fine + high winds"="Fine"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Fine no high winds"="Fine"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Raining + high winds"="Raining"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Raining no high winds"="Raining"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Snowing + high winds"="Snowing"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Snowing no high winds"="Snowing"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Snowing"="Other"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Fog or mist"="Other"))
Birmingham2016$Weather_Conditions <- revalue(Birmingham2016$Weather_Conditions, c("Data missing or out of range"="Other"))

#basic pie
wcond <- table(Birmingham2016$Weather_Conditions)
wcondrel <- table(Birmingham2016$Weather_Conditions)/length(Birmingham2016$Weather_Conditions)
round(wcondrel, digits = 2)
View(SeverityRel)
pie(wcondrel)


#ggplot pie
wcondrel <- data.frame(wcondrel)
ggplot(wcondrel, aes(x="", y=wcondrel$Freq, fill=wcondrel$Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(x= NULL, y=NULL, fill = "Weather conditions", title = "Pie chart of weather conditions during accidents in 2016")+
  theme_void()+
  scale_fill_brewer(palette="Blues")

#Road types Pie chart without %

#put the data together

library(plyr)
Birmingham2016$Road_Type <- revalue(Birmingham2016$Road_Type, c("Unknown"="Other"))
Birmingham2016$Road_Type <- revalue(Birmingham2016$Road_Type, c("Data missing or out of range"="Other"))
Birmingham2016$Road_Type <- revalue(Birmingham2016$Road_Type, c("Slip road"="Other"))


#basic pie
rtype <- table(Birmingham2016$Road_Type)
rtyperel <- table(Birmingham2016$Road_Type)/length(Birmingham2016$Road_Type)
round(rtyperel, digits = 2)
View(rtyperel)
pie(rtyperel)
rtyperel <- rtyperel[nrow(rtyperel):1,]


#Count the % of each type of road
rtype <- Birmingham2016$Road_Type
rtypetable <- table(rtype)
rtypetable
66/5260 #Other
1179/5260 #Dual
102/5260 #One way
470/5260 #Roundabout
3443/5260 #Single



#ggplot pie

rtyperel <- data.frame(rtyperel)
ggplot(rtyperel, aes(x = "", y=rtyperel$Freq, fill=rtyperel$Var1)) +
  geom_bar(stat="identity", width=1, color="white", size=0.5) +
  coord_polar("y", start=0) +
  labs(x= NULL, y=NULL, fill = "Road types", title = "Pie chart of road types during accidents in 2016")+
  theme_void() +
  scale_fill_brewer(palette="YlOrRd")

#Histogramm of rural and urban accidents
# library
library(ggplot2)

# create a dataset
years <- c(rep("2011", 2), rep("2012" , 2), rep("2013" , 2), rep("2014" , 2),rep("2015" , 2),rep("2016" , 2))
condition <- rep(c("Rural" , "Urban") , 3)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(years,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=years)) + geom_bar(position="dodge", stat="identity")

#Hist nb ofcars involved
max(Birmingham2016$Number_of_Vehicles)
nb_vehicles <- Birmingham2016$Number_of_Vehicles
x11()

hist(nb_vehicles,
     main="Number of vehicles involved during the accidents in Birmingham in 2016",
     xlab="Number of vehicles",
     col="darkmagenta",
     freq=FALSE,
     breaks=c(0,1,2,3,4,5,6,7, 8,9,10, 11,12)
)



n <- table(Birmingham2016$Number_of_Vehicles)
nrel <- table(Birmingham2016$Number_of_Vehicles)/length(Birmingham2016$Number_of_Vehicles)
nrel <- data.frame(nrel)
attach(oneormorerel)
detach(oneormorerel)
attach(n)
detach(n)
ggplot(nrel, aes(x = "", y= Freq, fill= Var1)) +
  geom_bar(stat="identity", width=1, color="white", size=0.5) +
  coord_polar("y", start=0) +
  labs(x= NULL, y=NULL, fill = "Number of Vehicles", title = "Pie chart of number of vehicles involved in accidents in 2016")+
  theme_void()

nbofvehiclesrel <- table(Birmingham2016$oneormore)/length(Birmingham2016$oneormore)

Birmingham2016$oneormore[Birmingham2016$Number_of_Vehicles==1]<-"One vehicle"
Birmingham2016$oneormore[Birmingham2016$Number_of_Vehicles==2]<-"Two vehicles"
Birmingham2016$oneormore[Birmingham2016$Number_of_Vehicles==3]<-"Three vehicles"
Birmingham2016$oneormore[Birmingham2016$Number_of_Vehicles >3]<-"More than three vehicles"

b <- table(Birmingham2016$oneormore)
brel <- table(Birmingham2016$oneormore)/length(Birmingham2016$oneormore)
brel <- data.frame(brel)
attach(brel)
ggplot(brel, aes(x = "", y= Freq, fill= Var1)) +
  geom_bar(stat="identity", width=1, color="white", size=0.5) +
  coord_polar("y", start=0) +
  labs(x= NULL, y=NULL, fill = "Number of Vehicles", title = "Pie chart of number of vehicles involved in accidents in 2016")+
  theme_void()
