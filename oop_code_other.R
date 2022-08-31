Subject <- setRefClass(
  "Subject",
  
  fields = list(id = "numeric",
                data = "data.frame")
  
)

Visit <- setRefClass("Visit",
                     
                     fields = list(id = "numeric",
                                   data = "data.frame"))

Room <- setRefClass("Room",
                    
                    fields = list(id = "character",
                                  data = "data.frame"))


LongitudinalData <- setRefClass(
  "LongitudinalData",
  
  fields = list(data = "data.frame"),
  
  methods = list(
    subject = function(id) {
      Subject$new(id = id, data = fata[which(data == id), ])
    }
    
    
  )
)

######################## Set Generics Here #####################################

setGeneric("subject", function(x, id){
  standardGeneric("subject")
})

setGeneric("visit", function(x, visit_id){
  standardGeneric("visit")
})

setGeneric("room", function(x, room_id){
  standardGeneric("room")
})


make_LD <- function(data){
  LongitudinalData$new(data = data)
}

######################## Set Methods Here #####################################

setMethod("subject",
          
          c(x = "LongitudinalData",
            id = "numeric"),
          
          function(x, id) {
            Subject$new(id = id, data = x$data[x$data$id == id, ])
            
          })


setMethod("print",
          
          c(x = "LongitudinalData"),
          
          function(x) {
            paste("Longitudinal dataset with", length(unique(x$data$id)), "subjects")
          })


setMethod("print",
          
          c(x = "Subject"),
          
          function(x) {
            if (nrow(x$data) == 0) {
              print(NULL)
            } else{
              paste("Subject ID:", x$id)
            }
          })

setMethod("print",
          
          c(x = "Room"),
          
          function(x) {
            cat(
              paste0(
                "ID: ",
                x$data$id[1],
                "\n",
                "Visit: ",
                x$data$visit[1],
                "\n",
                "Room: ",
                x$data$room[1]
              )
            )
            
          })


setMethod("summary", # (need to print also ID: 54)
          
          c(object = "Subject"),
          
          function(object) {
            data <- object$data
            filteredData <- data[, c("visit", "room", "value")]
            
            ta <-
              aggregate(
                filteredData ,
                by = list(filteredData$visit, filteredData$room),
                FUN = mean
              )
            ta2 <- ta[, c(2, 3, 5)]
            ta3 <- list(reshape(ta2,
                      idvar = "visit",
                      timevar = "Group.2",
                      direction = "wide")
            )
            names(ta3) <- paste("ID:", object$data$id[1])
            ta3[[1]] <- ta3[[1]][order(ta3[[1]]$visit),]
            ta3
          })


setMethod("summary",
          
          c(object = "Room"),
          
          function(object) {
            
            id <- object$data$id[1]
            stats <- summary(object$data$value)
            
            p <- list(stats)
            
            names(p) <- paste("ID: ", id)
            p
            
          })


setMethod("visit",
          
          c(x = "Subject",
            visit_id = "numeric"),
          
          function(x, visit_id) {
            Visit$new(data = x$data[x$data$visit == visit_id, ],
                      id = visit_id)
            
          })


setMethod("room",
          
          c(x = "Visit",
            room_id = "character"),
          
          function(x, room_id) {
            Room$new(data = x$data[x$data$room == room_id, ],
                     id = room_id)
            
          })
