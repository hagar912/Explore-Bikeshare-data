##calling for library
install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(dplyr)
library(plyr)
library(ggplot2)
##import dataset
getwd()
setwd('I:/NTL/AAL/R course/final_project/ny')
ny <- read.csv('new_york_city.csv', header = TRUE)
View(ny)
head(ny,5)
names(ny)
dim(ny)

##summary for the data and unique values for each col
head(ny['Start.Time'], 5)
unique(ny$User.Type, incomparables = FALSE)
unique(ny$Gender, incomparables = FALSE)
summary(ny)
summary(ny$Birth.Year)
summary(ny$Trip.Duration)
summary(ny$Start.Station)

##preprocessing

#1# replacing blank cell in Gender with NA
ny$Gender[ny$Gender == ""] <- '0'

##making Age.Class as anew_col for classify the Birth.Year as 3 levels (Young ,Youth ,nyld)
summary(ny$Birth.Year)
Age.Class <- ifelse(
    ny$Birth.Year >= 1885 & ny$Birth.Year <= 1981,
    ny['Age.Class'] <- 'Senior Adult',
    ifelse(
        ny$Birth.Year > 1981 & ny$Birth.Year <= 1988,
        ny['Age.Class'] <- 'Adult',
        ifelse(
            ny$Birth.Year > 1988 & ny$Birth.Year <= 2001,
            ny['Age.Class'] <- 'Adolescence ',
            ny['Age.Class'] <- '0'
        )
    )
)
ny['Age.Class']<-Age.Class

#2# splitting the hour
start_time <-
    sapply(strsplit(as.character(ny$Start.Time), " "), "[", 2)
##suset hour from the data
hour <- substr(x = start_time, 1, 2)
ny['hour'] <- hour
hour <- as.numeric(hour)
class(hour)

#split the date
start_date <-
    sapply(strsplit(as.character(ny$Start.Time), " "), "[", 1)
##suset month from the data
month <- substr(x = start_date, 6, 7)
ny['month'] <- month
unique(ny$month, incomparables = FALSE)
ny$month[ny$month == "01"] <- 'January'
ny$month[ny$month == "02"] <- 'February'
ny$month[ny$month == "03"] <- 'March'
ny$month[ny$month == "04"] <- 'April'
ny$month[ny$month == "05"] <- 'May'
ny$month[ny$month == "06"] <- 'June'


##visualization


# .. 1 .. #
#Q(1) .. whose user that rents the bike more during the months ?
## from the plot ,Subscriber rents bike more than Customer and independent decreased during March and April but then increased during last month June

png(file = "rental_user.png")
rental_user <- function(data){
    ggplot(data = data,
           aes(x = User.Type)) +
        labs(title = "rental count compresion for User Types",
             x = 'User Type',
             y = 'Rental Count') +
        geom_histogram(
            aes(y = ..count..),
            stat = "count",
            width = .6,
            col = "white",
            fill = "#5386a3"
        ) +
        facet_wrap(~ data$month)
}
rental_user(ny)
dev.off()


# .. 2 .. #
#Q(2) .. whose gender that rents the bike more during the months ?
## from the plot ,Male rents bike more than female During all months.

png(file = "rental_gender.png")
rental_gender <- function(data) {
    ggplot(data,
           aes(x = Gender,
               fill= Gender)) +
        geom_histogram(stat = "count" ,
                       width = 0.6,
                       col = "white")+
        labs(title = "Rental Count compresion for Gender",
             x = 'Gender Typer',
             y = 'Rental Count') +
        facet_wrap(~ data$month)+
        theme (legend.position = "bottom")+
        scale_fill_discrete(name = "Method of Filing Gender")
}
rental_gender(subset(ny,!is.na(Gender)))
dev.off()


# .. 3 .. #
#Q(3) .. whose age stage that rents the bike more during the months ?
## from the plot , Senior adults rents bike more than others and few changes between Adult and Adolescents but Adult transcendence than Adolescents 




png(file = "rental_age_stage.png")
rental_age_stage <- function(data) {
    ggplot(data,
           aes(x = Age.Class,
               fill= Age.Class)) +
        geom_histogram(stat = "count" ,
                       width = 0.5,
                       col = "white")+
        labs(title = "Rental Count compresion for Age Stage",
             x = 'Age Stage Classfication',
             y = 'Rental Count') +
        facet_wrap(~ data$month, ncol=3)+
        theme (legend.position = "Bottom")+
        scale_fill_discrete(name = "Method of Filing rental_age_stage")
}
rental_age_stage(subset(ny,!is.na(Age.Class)))
dev.off()

#..................... From Plot(1,2,3)  .........................#
# ........ We found that Subscriber user who is Senior Adult Male was the best rental for the bike sharing ............#
# ................................................................................................................................#

# .. 4 .. #
#Q(4) .. Comparing the growth of the usage with full time for the trip between the months !
## from the plot ,There is a growth for the usage of the bike during months as : During April the median was 646.0 and in may was 645 and then there is a growth in June as it was 660

png(file = "Trip duration over Last 3 months.png")
growth_months <- function(data) {
    ggplot(data,
           aes(x = month,
               y = Trip.Duration, fill = month)) +
        geom_boxplot() +
        labs(title = "Trip duration over Last 3 months",
             x = "Month",
             y = "Trip Duration") +
        coord_cartesian(ylim = c(0, 1600)) +
        theme_bw() +
        scale_fill_brewer(palette = "Accent")
}
growth_months(data = subset(ny, month == c('June', 'May' , 'April')))
dev.off()
by(ny$Trip.Duration, ny$month, summary)


# .. 5 .. #
# Q(5) .. which Hour does it has most used of the bike riding ?
## from the plot ,The most used hour for bicycle borrowing was at 5 pm, followed by 4 pm and then from the plotting ##

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
        fill = I('#009496')
    )
}
hours_ride(data = ny)
dev.off()


# .. 6 .. #
# Q(6).. which Hour for each month has increasing with bike sharing ?
## from the plot ,The most used hour for each month bicycle borrowing was at 5 pm, followed by 6 pm ##

#plotting
png(file = "hours_per_month.png")
hours_per_month <- function(data) {
    qplot(
        x = data$hour,
        data = data,
        xlab = "Hours",
        main = "The most used hour per months",
        color = I('white'),
        fill = I('#5DA5DA')
    ) +
        facet_wrap( ~ month, ncol = 3) +
        coord_flip() +
        theme_bw()
}
#calling for the func
hours_per_month(data = ny)
dev.off()

# .. 7 .. #
#Q(7) .. Comparing Between males and females (take care of missing values) during the trip duration
## from the plot ,Female's trip duration was longer that Males.

# plotting
png(file = "gender_t.duration.png")
duration_gender <- function(data) {
    ggplot(data,
           aes(x = Gender,
               y = Trip.Duration,
               fill = Gender)) +
        geom_boxplot() +
        labs(title = "Gender vs Trip duration",
             x = "Gender",
             y = "Duration") +
        guides(fill = guide_legend(title = "Gender")) +
        coord_cartesian(ylim = c(0, 1500))
}
duration_gender(subset(ny, !is.na(Gender)))
dev.off()
# as shown with by that the defirence from female to male is 108.3 .
by(ny$Trip.Duration, ny$Gender, summary)
