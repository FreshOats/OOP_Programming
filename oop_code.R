library(purrr)
library(readr)
library(methods)
library(tidyr)
library(dplyr)



#Open and Read
file <- "C:/Users/Caroline/Desktop/MIE.csv"
df <- read.csv(file, sep = ",")

#verify that df is actually a data frame
class(df)

#Exploratory Analysis
head(df)
summary(df)
unique(df$id)
unique(df$visit) 
unique(df$room)
summary(df$value)
summary(df$timepoint)


#Create Class for LongitudinalData and constructor for make_LD
make_LD <- function(x) {
    df_broken <- split(x, x$id)
    structure(df_broken, class = c("LongitudinalData"))
}

#Create generic functions for subject, visit, and room
subject <- function(x, id) UseMethod("subject")
visit <- function(x, visit_number) UseMethod("visit")
room <- function(x, room_name) UseMethod("room")


#Create print (alreday generic) functions to sort by subject, then by visit and room on the LongitudinalData objects
print.LongitudinalData <- function(x) {
    cat("The longitudinal dataset contains", length(x[["id"]]), "subjects.")
}


subject.LongitudinalData <- function(x, id) {
    index <- which(x[["id"]] == id)
    structure(list(id = id, dataset = x[["dataset"]][[index]]), class = "subject")
}

print.subject <- function(x){
    cat("Subject ", x[["id"]])
}


#Need to create a function that will summarize the pollutant values for each subject
summary.subject <- function(x){
    summary_subject <- x[["dataset"]] %>%
        group_by(visit, room) %>%
        summarise(pollution = mean(pollution)) %>%
        spread(room, value) %>%
        as.data.frame  #coerce dictionary to df
    structure(list(id = x[["id"]], 
                   dataset = subject_summary),
              class = "summary")
}

#Create the function to filter by the visit per subject
visit.subject <- function(subject, visit_number){
    data <- subject[["dataset"]] %>%
        filter(visit == visit_number) %>%
        select(-visit)
    structure(list(id = subject[["id"]],
                   visit = visit_number,
                   dataset = data),
              class = "visit")
}


#Create the function that will filter room by visit
room.visit <- function(visit, room_name){
    data <- visit[["dataset"]] %>%
        filter(room == room_name) %>%
        select(-room)
    structure(list(id = visit[["id"]], 
                   visit_number = visit[["visit_number"]],
                   room = room_name, 
                   dataset = data), 
              class = 'room')
}


#Print functions for the room details and room summary 
print.room <- function(room){
    cat("Subject ", room[["id"]], "\n")
    cat("Visit number ", room[["visit_number"]], '\n')
    cat("In the ", room[["room"]])
}


summary.room <- function(room){
    summary_room <- summary(room[["dataset"]][["value"]])
    structure(list(id = room[["id"]], 
                   dataset = summary_room, class = "summary"))
}


print.summary <- function(x){
    cat("Subject ", x[["id"]], '\n')
    x[["dataset"]]
}

#############################
#Test Runs
x <- make_LD(df)
print(class(x))

ouput1 <- subject(x, 10)
print(output1)

output2 <- subject(x, 14)
print(output2)

output3 <- subject(x, 54) %>% summary
print(output3)

output4 <- subject(x, 14) %>% summary
print(output4)

output5 <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(output5)

output6 <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(output6)

output7 <- subject(x, 44) %>% visit(0) %>% room("living room") %>% summary
print(output7)
