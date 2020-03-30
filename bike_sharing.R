##calling for library
install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(dplyr)
library(plyr)
library(ggplot2)
##import dataset = chi
getwd()
setwd('I:/NTL/AAL/R course/final_project')
chi <- read.csv('chi/chicago.csv', header = TRUE)
View(chi)
head(chi, 5)
names(chi)
dim(chi)

##import dataset = chi
ny <- read.csv('ny/new_york_city.csv', header = TRUE)
View(ny)
head(ny, 5)
names(ny)
dim(ny)

##import dataset = wash
wash <- read.csv('wash/washington.csv', header = TRUE)
View(wash)
head(wash, 5)
names(wash)
dim(wash)


##summary for chi dataset
head(chi['Start.Time'], 5)
unique(chi$User.Type, incomparables = FALSE)
unique(chi$Gender, incomparables = FALSE)
summary(chi)
summary(chi$Birth.Year)
summary(chi$Trip.Duration)
summary(chi$Start.Station)

##summary for ny dataset
head(ny['Start.Time'], 5)
unique(ny$User.Type, incomparables = FALSE)
unique(ny$Gender, incomparables = FALSE)
summary(ny)
summary(ny$Birth.Year)
summary(ny$Trip.Duration)
summary(ny$Start.Station)

##summary for the data and unique values for each col
head(wash['Start.Time'], 5)
unique(wash$User.Type, incomparables = FALSE)
summary(wash)
summary(wash$Trip.Duration)
summary(wash$Start.Station)



##preprocessing for dataset

#1# .. replacing blank cell in Gender with NA .. #1#
chi$Gender[chi$Gender == ""] <- '0'
ny$Gender[ny$Gender == ""] <- '0'
## .. replace blank space in user.type .. #
ny$User.Type[ny$User.Type == ""] <- '0'


##making Age.Class as anew_col for classify the Birth.Year as 3 levels (senior adult ,adult ,adulescence)
Age.Class <- ifelse(
    chi$Birth.Year >= 1899 & chi$Birth.Year <= 1975,
    chi['Age.Class'] <- 'Senior Adult',
    ifelse(
        chi$Birth.Year > 1975 & chi$Birth.Year <= 1984,
        chi['Age.Class'] <- 'Adult',
        ifelse(
            chi$Birth.Year > 1984 & chi$Birth.Year <= 2016,
            chi['Age.Class'] <- 'Adolescence ',
            chi['Age.Class'] <- '0'
        )
    )
)
chi['Age.Class'] <- Age.Class
table(chi$Age.Class)

### .. for ny data set .. ##
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
ny['Age.Class'] <- Age.Class
table(ny$Age.Class)


#2# .. splitting the hour .. #2#
hour <- function(data) {
    start_time <-
        sapply(strsplit(as.character(data$Start.Time), " "), "[", 2)
    ##suset hour from the data
    hour <- substr(x = start_time, 1, 2)
}
##calling fun hour for chi dataset
hour_chi <- hour(chi)
chi['hour'] <- hour_chi
##calling fun hour for chi dataset
hour_ny <- hour(ny)
ny['hour'] <- hour_ny
##calling fun hour for wash dataset
hour_wash <- hour(wash)
wash['hour'] <- hour_wash


#3# .. split the date .. #3#
month <- function(data) {
    start_date <-
        sapply(strsplit(as.character(data$Start.Time), " "), "[", 1)
    ##suset month from the data
    month <- substr(x = start_date, 6, 7)
}
##calling fun with data = chi
month_chi <- month(chi)
chi['month'] <- month_chi
unique(chi$month)
##calling function with data = ny
month_ny <- month(ny)
ny['month'] <- month_ny
##calling function with data = wash
month_wash <- month(wash)
wash['month'] <- month_wash


##replacing '01' with 'january' and '02' with 'February' and so on ...
old <- c('01', '02', '03', '04', '05', '06')
new <- c('January', 'Febraury', 'March', 'April', 'May', 'June')
chi$month[chi$month %in% old] <-
    new[match(chi$month, old, nomatch = 0)]
ny$month[ny$month %in% old] <-
    new[match(ny$month, old, nomatch = 0)]
wash$month[wash$month %in% old] <-
    new[match(wash$month, old, nomatch = 0)]





##visualization

# .. 1 .. #
#Q(1) .. whose user that rents the bike more during the months ?
png(file = "rental_user_chi.png")
png(file = "rental_user_ny.png")
png(file = "rental_user_wash.png")

rental_user <- function(data,title) {
    ggplot(data = data,
           aes(x = User.Type)) +
        labs(title = title,
             x = 'User Type',
             y = 'Rental Count'
             ) +
        geom_histogram(
            aes(y = ..count..),
            stat = "count",
            width = .6,
            col = "white",
            fill = "#f55e61"
        ) +
        facet_wrap( ~ data$month)
}
## for chi df ,Subscriber rents bike more than Customer and independent
rental_user(chi,title="Rental Count compresion for Chicago User Types")
table(chi$User.Type)

## for ny df ,Subscriber rents bike more than Customer and independent decreased during March and April but then increased during last month June
rental_user(subset(ny,!is.na(User.Type)),title ="Rental Count compresion for New York User Types" )
table(ny$User.Type)

## for wash df,Subscriber rents bike more than Customer and it is clearly show that there is a growth in the rental during last month June.
rental_user(wash,title="washington ~ Rental Count compresion for User Types")
table(wash$User.Type)

dev.off()


# .. 2 .. #
#Q(2) .. whoes gender that rents the bike more during the months ?

png(file = "rental_gender_chi.png")
rental_gender <- function(data,title) {
    ggplot(data,
           aes(x = Gender,
               fill = Gender)) +
        geom_histogram(stat = "count" ,
                       width = 0.6,
                       col = "white") +
        labs(title = title,
             x = 'Gender Typer',
             y = 'Rental Count') +
        facet_wrap( ~ data$month) +
        theme (legend.position = "bottom") +
        scale_fill_discrete(name = "Method of Filing Gender")
}
## for chi dataset ,Male rents bike more than female decresing during May but then incresing within June
rental_gender(subset(chi, !is.na(Gender)),title="Rental Count compresion for Chicago Gender")
table(chi$Gender)

## for ny dataset ,Male rents bike more than female During all months.
rental_gender(subset(ny, !is.na(Gender)),title="New York ~ Rental Count compresion for Gender")
table(ny$Gender)

dev.off()


# .. 3 .. #
#Q(3) .. whose age stage that rents the bike more during the months ?

png(file = "rental_age_stage_ny.png")
rental_age_stage <- function(data,title) {
    ggplot(data,
           aes(x = Age.Class,
               fill = Age.Class)) +
        geom_histogram(stat = "count" ,
                       width = 0.6,
                       col = "white") +
        labs(title = title,
             x = 'Age Stage Classfication',
             y = 'Rental Count') +
        facet_wrap( ~ data$month) +
        theme (legend.position = "Bottom")+
        scale_fill_discrete(name = "Method of Filing rental_age_stage")
}
## for chi dataset ,Adolescencence rents bike more than others and few changes between Adult and senior adult
rental_age_stage(subset(chi, !is.na(Age.Class)),title="Rental Count compresion for Chicago Age Stage")
table(chi$Age.Class)

## for ny dataset ,Senior adults rents bike more than others and few changes between Adult and Adolescences
rental_age_stage(subset(ny, !is.na(Age.Class)),title="New York ~ Rental Count compresion for Age Stage")
table(ny$Age.Class)

dev.off()


#..................... From Plot(1,2,3)  .........................#
# ........ for chi ds : We found that Subscriber user who is Male with Adolescence from 41 and above was the best use for the bike rental  ............#
# ........ for ny ds : We found that Subscriber user who is Male with Senior Adult was the best rental for the bike sharing ............#
# ................................................................................................................................#


# .. 4 .. #
#Q(4) .. Comparing the growth of the usage with full time for the trip between the months !

png(file = "Trip duration over Last 3 months_chi.png")
growth_months <- function(data,title) {
    ggplot(data,
           aes(x = month,
               y = Trip.Duration, fill = month)) +
        geom_boxplot() +
        labs(title = title,
             x = "Month",
             y = "Trip Duration") +
        coord_cartesian(ylim = c(0, 1600)) +
        theme_bw() +
        scale_fill_brewer(palette = "Accent")
}
## for chi df ,There is a growth for the usage of the bike during months as : During April the median was 697 and in may was 709 and then there is a great growth in june as it was 772
growth_months(data = subset(chi, month == c('June', 'May' , 'April')),title="Chicago trip duration over Last 3 months")
table(chi$month)
by(chi$Trip.Duration, chi$month, summary)

## for ny df, There is a growth for the usage of the bike during months as : During April the median was 646.0 and in may was 645 and then there is a growth in June as it was 660
growth_months(data = subset(ny, month == c('June', 'May' , 'April')),title = "New York ~ trip duration over Last 3 months")
table(ny$month)
by(ny$Trip.Duration, ny$month, summary)

## for wash df, There is a growth for the usage of the bike during months as : from first month (January was 579.64 and the big change was in June760.9 .
growth_months(data = subset(wash, month == c('June', 'January')),title = "Washington trip duration over Last 3 months")
table(wash$month)
by(wash$Trip.Duration, wash$month, summary)

dev.off()



# .. 5 .. #
# Q(5) .. which Hour does it has most used of the bike riding ?

#plotting
? qplot
png(file = "Most_usd_hour_chi.png")
hours_ride <- function(data,title) {
    qplot(
        x = data$hour,
        data = data,
        xlab = "Hours",
        ylab = "count for each hour",
        main = title,
        color = I('white'),
        fill = I('#60BD68')
    )
}
## for chi dataset, The most used hour for bicycle borrowing was at 5 pm, followed by 4 pm and then from the plotting ##
hours_ride(data = chi,title="The most used Chicago's hour for sharing bike")
table(chi$hour)

## for ny dataset ,The most used hour for bicycle borrowing was at 5 pm, followed by 4 pm and then from the plotting ##
hours_ride(data = ny,title="New York ~ The most used hour for sharing bike")
table(ny$hour)

## from the plot ,The most used hour for bicycle borrowing was at 8 AM, followed by 7 AM and then .. ##
hours_ride(data = wash,title="Washington ~ The most used hour for sharing bike")
table(wash$hour)

dev.off()


# .. 6 .. #
# Q(6).. which Hour for each month has increasing with bike sharing ?

#plotting
png(file = "hours_per_month_chi.png")
hours_per_month <- function(data,title) {
    qplot(
        x = data$hour,
        data = data,
        xlab = "Hours",
        main = title,
        color = I('white'),
        fill = I('#5DA5DA')
    ) +
        facet_wrap(~ month, ncol = 3) +
        coord_flip() +
        theme_bw()
}
## for chi dataset ,The most month for rental the bike was last month 'June' as we on the right way  ##
hours_per_month(data = chi,title="The most used hour per months for Chicago")
table(chi$hour)
## for ny dataset ,The most used hour for each month bicycle borrowing was at june at 5 pm, followed by 6 pm ##
hours_per_month(data = ny,title="New York ~ The most used hour per months for New York")
table(ny$hour)

dev.off()
## for wash ,The hour during first 3 months is 6 PM||April,May was 5 AM||June was 8 AM ##
hours_per_month(data = wash,title="Washington ~ The most used hour per months ")
table(wash$hour)
dev.off()


# .. 7 .. #
#Q(7) .. Comparing Between males and females (take care of missing values) during the trip duration

# plotting
png(file = "gender_t.duration_chi.png")
duration_gender <- function(data,title) {
    ggplot(data,
           aes(x = Gender,
               y = Trip.Duration,
               fill = Gender)) +
        geom_boxplot() +
        labs(title = title,
             x = "Gender",
             y = "Duration") +
        guides(fill = guide_legend(title = "Gender")) +
        coord_cartesian(ylim = c(0, 1500))
}
## for chi dtaset ,Female's trip duration was longer that Males.
duration_gender(subset(chi,!is.na(Gender)),title = "Chicago's Gender vs Trip duration")
table(chi$Gender)
by(chi$Trip.Duration, chi$Gender, summary)

## For ny dataset ,Female's trip duration was longer that Males.
duration_gender(subset(ny,!is.na(Gender)),title = "New York ~ Gender vs Trip duration")
table(ny$Gender)
by(ny$Trip.Duration, ny$Gender, summary)

dev.off()
