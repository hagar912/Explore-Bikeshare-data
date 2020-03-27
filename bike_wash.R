##calling for library
install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(dplyr)
library(plyr)
library(ggplot2)
##import dataset
getwd()
setwd('I:/NTL/AAL/R course/final_project/wash')
wash <- read.csv('washington.csv', header = TRUE)
View(wash)
head(wash, 5)
names(wash)
dim(wash)

##summary for the data and unique values for each col
head(wash['Start.Time'], 5)
unique(wash$User.Type, incomparables = FALSE)
summary(wash)
summary(wash$Trip.Duration)
summary(wash$Start.Station)

##preprocessing

#2# splitting the hour
start_time <-
    sapply(strsplit(as.character(wash$Start.Time), " "), "[", 2)
##suset hour from the data
hour <- substr(x = start_time, 1, 2)
wash['hour'] <- hour
hour <- as.numeric(hour)
class(hour)


#split the date
start_date <-
    sapply(strsplit(as.character(wash$Start.Time), " "), "[", 1)
##suset month from the data
month <- substr(x = start_date, 6, 7)
wash['month'] <- month
unique(wash$month, incomparables = FALSE)
wash$month[wash$month == "01"] <- 'January'
wash$month[wash$month == "02"] <- 'February'
wash$month[wash$month == "03"] <- 'March'
wash$month[wash$month == "04"] <- 'April'
wash$month[wash$month == "05"] <- 'May'
wash$month[wash$month == "06"] <- 'June'


##visualization


# .. 1 .. #
#Q(1) .. whoes user that rents the bike more during the months ?
## from the plot ,Subscriber rents bike more than Customer and it is clearly show that there is a growth in the rental during last month June.

png(file = "rental_user.png")
rental_user <- function(data) {
    ggplot(data = data,
           aes(x = User.Type)) +
        labs(title = "Rental Count compresion for User Types",
             x = 'User Type',
             y = 'Rental Count') +
        geom_histogram(
            aes(y = ..count..),
            stat = "count",
            width = .6,
            col = "white",
            fill = "#ffb253"
        ) +
        facet_wrap(~ data$month)
}
rental_user(wash)
dev.off()


# .. 2 .. #
#Q(2) .. Comparing the growth of the usage with full time for the trip between thefirst and last months !
## from the plot , There is a growth for the usage of the bike during months as : from first month (January was 579.64 and the big change was in June760.9 .

png(file = "Trip duration  months.png")
growth_months <- function(data) {
    ggplot(data,
           aes(x = month,
               y = Trip.Duration, fill = month)) +
        geom_boxplot() +
        labs(title = "Trip duration over Last 3between first and last month",
             x = "Month",
             y = "Trip Duration") +
        coord_cartesian(ylim = c(0, 1600)) +
        theme_bw() +
        scale_fill_brewer(palette = "Accent")
}
growth_months(data = subset(wash, month == c('June', 'January')))
dev.off()
by(wash$Trip.Duration, wash$month, summary)


# .. 3 .. #
# Q(3) .. which Hour does it has most used of the bike riding ?
## from the plot ,The most used hour for bicycle borrowing was at 8 AM, followed by 7 AM and then .. ##

#plotting
? qplot
png(file = "Most_usd_hour.png")
hours_ride <- function(data) {
    qplot(
        x = data$hour,
        data = data,
        xlab = "Hours",
        ylab = "count for each hour",
        main = "The most used hour for sharing bike",
        color = I('white'),
        fill = I('#ffa4a4')
    )+
        coord_flip() 
}
hours_ride(data = wash)
dev.off()


# .. 4 .. #
# Q(4).. which Hour for each month has increasing with bike sharing ?
## from the plot , we considered that ,The hour during first 3 months is 6 PM||April,May was 5 AM||June was 8 AM ##

#plotting
png(file = "hours_per_month.png")
hours_per_month <- function(data) {
    qplot(
        x = data$hour,
        data = data,
        xlab = "Hours",
        ylab="Count for Hours",
        main = "The most used hour per months",
        color = I('white'),
        fill = I('#258150')
    ) +
        facet_wrap( ~ month, ncol = 3) +
        coord_flip() +
        theme_bw()
}
#calling for the func
hours_per_month(data = wash)
dev.off()

