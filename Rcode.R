crimeData <- read.csv("/Users/abhishekjaiswal/Desktop/Winter18A/ALY6040-Data Mining/Assignments/Week6-Final Project/crime.csv", header = TRUE)
str(crimeData)
summary(crimeData)

crimeData <- subset(crimeData, !duplicated(crimeData$INCIDENT_NUMBER))
summary(crimeData)

crimeData <- subset(crimeData, !is.na(crimeData$INCIDENT_NUMBER))
crimeData <- subset(crimeData, !is.na(crimeData$OFFENSE_CODE))
crimeData <- subset(crimeData, !is.na(crimeData$OFFENSE_CODE_GROUP))
crimeData <- subset(crimeData, !is.na(crimeData$DISTRICT))
crimeData <- subset(crimeData, !is.na(crimeData$REPORTING_AREA))
crimeData <- subset(crimeData, !is.na(crimeData$OCCURRED_ON_DATE))
crimeData <- subset(crimeData, !is.na(crimeData$Location))


head(crimeData$OCCURRED_ON_DATE)
crimeData$date <- as.POSIXlt(crimeData$OCCURRED_ON_DATE, "GMT")
crimeData$date <- as.Date(crimeData$date)
head(crimeData$date)

install.packages("chron")
library("chron")
head(crimeData$HOUR)
time.tag <- c("0", "6", "12", "18", "24")
time.tag
crimeData$time.tag <- cut(crimeData$HOUR, breaks = time.tag,
                             labels = c("00-06", "06-12", "12-18", "18-00"), include.lowest = TRUE)
table(crimeData$time.tag)
table(crimeData$OFFENSE_CODE_GROUP)

crimeData$OFFENSE_CAT <- crimeData$OFFENSE_CODE_GROUP

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Fire Related Reports", "Firearm Discovery", "Arson"),
                                       "ARSON", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Simple Assault", "Verbal Disputes", "Disorderly Conduct", "Aggravated Assault", "Prisoner Related Incidents"),
                                       "Assault", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Other Burglary", "Residential Burglary", "Commercial Burglary", "Burglary - No Property Taken"),
                                       "Burglary", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Medical Assistance", "Biological Threat"),
                                       "Medical", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Larceny", "Larceny From Motor Vehicle", "Auto Theft", "Auto Theft Recovery", "HOME INVASION", "Embezzlement"),
                                       "Larceny", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Missing Person Reported", "Missing Person Located"),
                                       "Missing", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Motor Vehicle Accident Response", "Towed", "License Plate Related Incidents"),
                                       "MVT", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Investigate Person", "INVESTIGATE PERSON","Investigate Property","Drug Violation","Violations","Counterfeiting","Operating Under the Influence","Firearm Violations","License Violation","Restraining Order Violations","Liquor Violation","Assembly or Gathering Violations","Confidence Games","Harbor Related Incidents","Evading Fare","Gambling"),
                                       "NONVIO", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Phone Call Complaints", "Service", "Aircraft", "Other"),
                                       "Other", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Property Lost","Property Found","Recovered Stolen Property","Property Related Damage","Landlord/Tenant Disputes"),
                                "Property", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Robbery"),
                                "Robbery", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Harassment", "Criminal Harassment", "Prostitution"),
                                "SEX", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("HUMAN TRAFFICKING", "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE"),
                                "TRAFFICKING", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Vandalism"),
                                "Vandalism", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Police Service Incidents","Ballistics","Bomb Hoax","Offenses Against Child / Family","Homicide","Explosives","Manslaughter"),
                                "VIO", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Warrant Arrests", "Search Warrants"),
                                "Warrant", crimeData$OFFENSE_CAT)

crimeData$OFFENSE_CAT <- ifelse(crimeData$OFFENSE_CODE_GROUP %in% c("Fraud"),
                                "Fraud", crimeData$OFFENSE_CAT)

table(crimeData$OFFENSE_CAT)

crimeData$SHOOTING <- ifelse(as.character(crimeData$SHOOTING) == "Y", 1, 0)

table(crimeData$SHOOTING)

install.packages("ggplot2")
library("ggplot2")

qplot(crimeData$OFFENSE_CAT, xlab = "Crime Type", main = "Crimes in Boston - 2015-2018") + scale_y_continuous("Number of crimes")

qplot(crimeData$time.tag, xlab = "Time range of day", main = "Crimes by time of day") + scale_y_continuous("Number of crimes")

crimeData$day <- crimeData$DAY_OF_WEEK
crimeData$day <- factor(crimeData$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

qplot(crimeData$day, xlab= "Day of week", main= "Crimes by day of week") + scale_y_continuous("Number of crimes")


crimeData$month <- crimeData$MONTH
crimeData$month <- ifelse(crimeData$MONTH %in% c("1"), "Jan", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("2"), "Feb", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("3"), "Mar", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("4"), "Apr", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("5"), "May", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("6"), "Jun", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("7"), "Jul", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("8"), "Aug", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("9"), "Sep", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("10"), "Oct", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("11"), "Nov", crimeData$month)
crimeData$month <- ifelse(crimeData$MONTH %in% c("12"), "Dec", crimeData$month)

table(crimeData$MONTH)
table(crimeData$month)

crimeData$month <- factor(crimeData$month, levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
qplot(crimeData$month, xlab1= "Month", main= "Crimes by month") + scale_y_continuous("Number of crimes")

temp <- aggregate(crimeData$OFFENSE_CAT, by = list(crimeData$OFFENSE_CAT, crimeData$time.tag), FUN= length)
names(temp) <- c("OFFENSE_CAT", "time.tag", "count")

ggplot(temp, aes(x= OFFENSE_CAT, y= factor(time.tag))) +
geom_tile(aes(fill= count)) + scale_x_discrete("OFFENSE_CAT", expand = c(0,0)) + scale_y_discrete("Time of day", expand = c(0,-2)) +
scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") + theme_bw() + ggtitle("Crimes by time of day") +
theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line (colour = NA))

install.packages("plyr")
library("plyr")

temp1 <- aggregate(crimeData$OFFENSE_CAT, by = list(crimeData$OFFENSE_CAT, crimeData$day), FUN= length)
names(temp1) <- c("OFFENSE_CAT", "day", "count")

ggplot(temp1, aes(x= OFFENSE_CAT, y= day, fill= count)) + geom_tile(aes(fill= count)) +
scale_x_discrete("OFFENSE_CAT", expand = c(0,0)) + scale_y_discrete("Day of week", expand = c(0,-2)) + scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") +
theme_bw() + ggtitle("Crimes by day of week") +
theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))

temp2 <- aggregate(crimeData$OFFENSE_CAT, by = list(crimeData$OFFENSE_CAT, crimeData$month), FUN= length)
names(temp2) <- c("OFFENSE_CAT", "month", "count")

ggplot(temp2, aes(x= OFFENSE_CAT, y= month, fill= count)) +
geom_tile(aes(fill= count)) +
scale_x_discrete("OFFENSE_CAT", expand = c(0,0)) +
scale_y_discrete("month", expand = c(0,-2)) +
scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") + theme_bw() + 
ggtitle("Crimes by Month") + 
theme(panel.grid.major = element_line (colour = NA), panel.grid.minor = element_line(colour = NA))


