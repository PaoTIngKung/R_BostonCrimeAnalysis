tmp3 <- crimeData$date
crimeData$date <- as.character(crimeData$date)
crimeData.agg <- ddply(crimeData, .(OFFENSE_CAT, SHOOTING, REPORTING_AREA, date, Lat, Long, time.tag, day, month), 
                       summarise, count = length(date), .progress= 'text')
crimeData$date <- tmp3
rm(tmp3)


length(unique(crimeData.agg$REPORTING_AREA))
length(unique(crimeData.agg$date))
reportingArea <- sort(unique(crimeData.agg$REPORTING_AREA))
dates <- sort(as.character(unique(crimeData.agg$date)))

temp4 <- expand.grid(reportingArea, dates)
names(temp4) <- c("reportingArea", "dates")
temp4 <-  arrange(temp4, reportingArea)

model.data <- aggregate(crimeData.agg[, c('count', 'SHOOTING')], by= list(crimeData.agg$REPORTING_AREA, as.character(crimeData.agg$date)),
                        FUN= sum)
names(model.data) <- c("reportingArea", "dates", "count", "SHOOTING")
model.data <- merge(temp4, model.data, by= c('reportingArea', 'dates'), all.x= TRUE)
model.data$count[is.na(model.data$count)] <- 0
model.data$SHOOTING[is.na(model.data$SHOOTING)] <- 0
model.data$day <- weekdays(as.Date(model.data$dates), abbreviate= TRUE)
model.data$month <- months(as.Date(model.data$dates), abbreviate= TRUE)
pastDays <- function(x) { c(0, rep(1, x))}
model.data$past.crime.1 <- ave(model.data$count, model.data$reportingArea, FUN= function(x) filter(x, pastDays(1), sides= 1))
model.data$past.crime.7 <- ave(model.data$count, model.data$reportingArea, FUN= function(x) filter(x, pastDays(7), sides= 1))
model.data$past.crime.30 <- ave(model.data$count, model.data$reportingArea, FUN= function(x) filter(x, pastDays(30), sides= 1))
meanNA <- function(x){
  mean(x, na.rm= TRUE)
}
model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),
                                  meanNA(model.data$past.crime.1), model.data$past.crime.1)
model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7), meanNA(model.data$past.crime.7), model.data$past.crime.7)
model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30), meanNA(model.data$past.crime.30), model.data$past.crime.30)

model.data$past.shooting.30 <- ave(model.data$SHOOTING, model.data$reportingArea, FUN= function(x) filter(x, pastDays(30), sides= 1))
model.data$past.shooting.30 <- ifelse(is.na(model.data$past.shooting.30), meanNA(model.data$past.shooting.30), model.data$past.shooting.30)

cor(model.data$past.crime.30, model.data$past.shooting.30)
cor(model.data$past.crime.30, model.data$past.crime.7)

model.data$crime.trend <- ifelse(model.data$past.crime.30 == 0, 0, model.data$past.crime.7/model.data$past.crime.30)

model.data$season <- as.factor(ifelse(model.data$month %in% c("Mar", "Apr", "May"), "spring",
                                      ifelse(model.data$month %in% c("Jun", "Jul", "Aug"), "summer",
                                             ifelse(model.data$month %in% c("Sep", "Oct", "Nov"), "fall", "winter"))))
install.packages("psych")
library("psych")
model.cor <- cor(model.data[, c('count', 'past.crime.1', 'past.crime.7',
                                   'past.crime.30', 'crime.trend')]) 
cor.plot(model.cor)

model.data <- model.data[order(model.data$date),]

rows <- c(1:floor(nrow(model.data)*0.9))
test.data <- model.data[-rows, ]
model.data <- model.data[rows, ]


mean(model.data$count)
var(model.data$count)

install.packages("MASS")
library("MASS")
crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 + crime.trend + factor(day) + season, data= model.data)
summary(crime.model)


crime.model.pred <- predict(crime.model, test.data, type= "response")
sqrt(mean((test.data$count - crime.model.pred)^2))


crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 + crime.trend + factor(day) + season +
                        I(past.crime.30^2), data= model.data)
summary(crime.model)

crime.model.pred <- predict(crime.model, test.data, type= "response")
sqrt(mean((test.data$count - crime.model.pred)^2))

validate <- data.frame(test.data$count, crime.model.pred)
names(validate) <- c("actual", "predicted")
validate$bucket <- with(validate, cut(predicted, breaks= quantile(predicted, probs= seq(0, 1, 0.1)),
                                        include.lowest= TRUE, labels= c(1:10)))
validate <- aggregate(validate[, c('actual', 'predicted')], by=
                        list(validate$bucket), FUN = mean)

plot(validate$predicted, col= "red", type= "l", lwd= 1.5, ylab= "No. of Crimes", xlab= "Predicted Crime bins", 
     main= "Actual vs. Predicted from the data model")
      lines(validate$actual, col= "blue", lwd= 1.5)
        legend("topright", c("Actual", "Predicted"), col= c("blue", "red"), lwd= c(1.5, 1.5), bty= "n")
