library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(magrittr)

## First, import the data and read into a dataframe
file <- "C:/Users/Caroline/Desktop/MIE.csv"
data <- read.csv(file, sep = ',')
class(data)

## Exploratory analysis - look at the data
summary(data)
View(data)
# class(data$id)
# class(data$visit)
# class(data$room)
# class(data$value)
# class(data$timepoint)
sapply(data, class)
sapply(data[1:3], unique)
sapply(data[4:5], summary)
plot(data$timepoint, data$value)

## Make_LD - the purpose of this is to convert the longitudinal data into a 
#"LongitudinalData" object with "n" number of subjects
# This expects an input of df, which will be the longitudinal data frame. 

make_LD <- function(df) {
    if (!is.data.frame(df)) {stop("The input must be a data frame.")}
    ld <- df %>% 
        group_by(id) %>% 
        nest %>% 
        structure(class = "LongitudinalData")
}

## The printed logitudinal function calls the data from the make_LD and returns
# the number of subjects with unique id numbers

print.LongitudinalData <- function(ld) {
    cat("This longitudinal dataset has", length(ld$id), "subjects.")
}

# subject(longdata, id) - the expectation for this will be that the data set 
# "longdata" is read, and it returns the subject "id" and contains the information 
# for the "visit", "room", and all values (by timepoint) for that particular subject. 
# subject %>% summary should produce the ID and then a table of the visits, and 
# values organized by room

subject <- function(ld, id) UseMethod("subject")

subject.LongitudinalData <- function(ld, id) {
    index <- which(ld$id == id)
    if (length(index) == 0)
        return(NULL)
    structure(list(id = id, data = ld[["data"]][[index]]), class = "subject")
}

print.subject <- function(x) {
    cat("Subject ID:", x$id)
}

summary.subject <- function(x) {
    output <- x$data %>%
        group_by(visit, room) %>%
        summarize(mean_pollution = mean(value)) %>% 
        spread(key = room, value = mean_pollution) %>%
        as.data.frame()
    structure(list(id = x$id, data = output), class = "summary")
}

# visit(id, visit) - the expectation for this function is that when a subject is
# identified, the function will return the values in the all of the rooms for 
# that subject at that visit number
# visit %>% summary should produce the ID, a single visit, and mean values 
# organized by room

visit <- function(id, visit) UseMethod("visit")

visit.subject <- function(id, vis) {
    vis_data <- id$data %>%
        filter(visit == vis) %>%
        select(-visit)
    structure(list(id = id$id, 
                   visit = vis, 
                   data = vis_data), 
              class = "visit")
}

# room(visit, room) - the expectation for this function is that when a room has 
# been selected for a particular visit, the function will return the values 
## summary should print the ID and the quartiles of the values - though it may 
# be better to show the ID, visit, room and then the quartiles of each

room <- function(visit, room) UseMethod("room")

room.visit <- function(vis, rm) {
    rm_data <- vis$data %>%
        filter(room == rm)
    structure(list(id = vis$id, 
                   visit = vis$visit,
                   room = rm,
                   data = rm_data), 
              class = "room")
}

print.room <- function(x) {
    cat("ID:", x$id, "\n")
    cat("Visit:", x$visit, "\n")
    cat("Room:", x$room)
}
    
summary.room <- function(x) {
    output <- summary(x[["data"]][["value"]])
    structure(list(id = x$id, data = output), class = "summary")
}

print.summary <- function(x) {
    cat("ID:", x$id, "\n")
    print(x$data)
}