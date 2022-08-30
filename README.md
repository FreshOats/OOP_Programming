# Object Oriented Programming in R
***Longitudinal Data Analysis with Object Oriented Programming***
by Justin R. Papreck
---

## Exploratory Data Analysis

1. Initially, the data needs to be read into a data frame, and the class is checked to verify this.
    
```{r observations}
file <- "path/MIE.csv"
data <- read.csv(file, sep = ',')
class(data)
```

2. The real exploratory analysis is looking at the data summary, viewing the data, then observing the classes, unique values, and plotting the timepoints versus the values just to get a sense of the data we're dealing with.

```{r exploratory_1}
summary(data)
View(data)
sapply(data, class)
```
Since the first three categories from the data are the ID, Visit, and Room, it's important to see how many of each we're dealing with by identifying unique values. For the value and timepoints, it's not expected to have any truly unique values, so a summary of these data are more valuable to see.  
```{r exploratory_2}
sapply(data[1:3], unique) 
sapply(data, summary)
plot(data$timepoint, data$value)
```

![exploratory_2-1](https://user-images.githubusercontent.com/33167541/187563124-cbc49944-e03f-4615-9e1e-8bc2a55a9e9c.png)


## Creating the Individual Functions and Objects
### The make_LD function

The purpose of **make_LD** is to convert the longitudinal data as a data frame into a *LongitudinalData* object. This object will have 'n' number of subject. This function expects an input of a longitudinal data frame, **df**, which will be converted into the new object. 
  I've included the condition that the input must be a data frame or the function stops. This will group the data within by ID, then nest the data to create a list of data frames that can be called on by the downstream functions.

```{r make_LD}
make_LD <- function(df) {
    if (!is.data.frame(df)) {stop("The input must be a data frame.")}
    ld <- df %>% 
        group_by(id) %>% 
        nest %>% 
        structure(class = "LongitudinalData")
}
```

The **make_LD** function returns the structure **ld**, which is of the class, *LongitudinalData*. To view the number of unique IDs of the subjects in the data set, we employ a print function to the generic defined in the *LongitudinalData* class.

```{r print_LD}
print.LongitudinalData <- function(ld) {
    cat("This longitudinal dataset has", length(ld$id), "subjects.")
}
```

### The Subject Functions and Class
The subject function will take the **ld** data structure and ID number as inputs, which will then return the information from that ID also including the visit, room, and timepoint data for that particular subject. 
Initially, the **subject** generic function is created, which is then used with the LongitudinalData to create the *subject* class. 

  Since the expected outputs for a non-existent ID number cannot produce an error but also needs to subset the data for those with an ID number, I set up an index and created the subject subset using the id as an index. If this index doesn't exist, it produces a NULL. 

```{r subject_1}
subject <- function(ld, id) UseMethod("subject")

subject.LongitudinalData <- function(ld, id) {
    index <- which(ld$id == id)
    if (length(index) == 0)
        return(NULL)
    structure(list(id = id, data = ld[["data"]][[index]]), class = "subject")
}
```

Since we actually want to see the output of our results, we again need to employ a print function, but additionally, we would like to see a summary of these data. The summary will also be defined as its own class, *summary*.
  The summary groups the data (per ID) by visit and room. The value is the pollution that has been measured per timepoint. I took the mean of the pollution per visit and room. Spread adds mean pollution as a column before applying to the data frame. 

```{r subject_2}
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
```

### The Visit Functions and Class
**visit** takes the id number and the visit number and creates a visit generic. Since this function will only be applied after **subject** has been applied, there is no need to follow the indexing from the aforementioned function. When applied, the id will be passed from **subject** into **visit** with a visit number specified. The expected return will be all of the rooms and associated pollution values for that subject on that particular visit in each of the rooms. 
  Ultimately **visit** will filter the *subject* class, by selecting/filtering from dplyr, and  passing it to the **room** function, which will take the *visit* class output data as an input along with the name of the room. 

```{r visit}
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
```

### The Room Functions and Class
**room** is the final set of functions in this pipeline. Similar to the procedure as visit, the room generic is created, then applied to visit, then filtered, organized as a list, and then defined as the class *room*. 
  The *room* is passed to the print generic, to produce the ID, Visit, and Room number. 
  
```{r room_1}
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
```

### Summary Functions
The summary function for *room* uses summary on the subset from the *room* class output and creates the final class, *summary*, which can be passed to the print  generic. 
  Finally, the **print.summary** function is defined, which should produce the ID and quartiles of the values (default in summary). 
  
```{r summary}
summary.room <- function(x) {
    output <- summary(x[["data"]][["value"]])
    structure(list(id = x$id, data = output), class = "summary")
}

print.summary <- function(x) {
    cat("ID:", x$id, "\n")
    print(x$data)
}
```


## Functional Assesment 
```{r assessment}
x <- make_LD(data)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
```

