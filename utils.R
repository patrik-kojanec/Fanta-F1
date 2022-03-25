library(tidyverse)
library(lubridate)

calculate_usr_scores <- function(usr_forms, results, user, session){
  if (session == "Race"){
    w <- c(5, 3, 2, 1, 1, 1, 1, 1, 1, 1, 3)
  }else{
    w <- c(5, 3, 2, 1, 1)
  }
  
  mtx <- as.matrix(usr_forms[, 2:ncol(usr_forms)] == results[, 3:ncol(results)])
  scs <- mtx %*% w
  return(data.frame(Username = user,
                    time = usr_forms$date_time,
                    GP = results$GP,
                    Session = session,
                    pts = as.numeric(scs)))
}
