## Homework 4
## STA 308
## Nick Martini






bday.problem <- function(num_people = 23){                 
  bdays <- 1:365
  sample1 <- sample(bdays,num_people, replace = TRUE)      ## Defining the function, sampling days from 1 to 365
  print(length(unique(sample1[duplicated(sample1)]) ))     ## Print the length aka number of matches
}


num_people <- 10      ## assign num_people to 10
value.list <- list()   ## creating an empty list
index <- 1       ## creating an index value of 1

while(num_people <= 100) {
  set.seed(314159)
  matches <- sapply(rep(num_people, 100,000), bday.problem)    ## while loop that repeats the function 100,000 times 
                                                               ## for 10 people, 20 people, 30 and so on
  value.list[[index]] <- c(matches)
  index <- index + 1
  num_people <- num_people + 10
}
  
  
for(i in 1:10) { 
    barplot(table(value.list[[i]]))                ## for loop that creates a bar plot and summary for all of our values
                                                   ## of matches in out list
    print(summary(value.list[[i]]))         
}

## For the distribution of the number of matches, we see the mean number of matches gradually increase as the number of people increases. 
## For the 30 people sample, we see the mean number of matches be more than 1. For the 50 people, the mean is over 3. Then when we get to the 
## 90 and 100 person sample, we see means of over 9 and 11 respectively. There is a gradual increase in number of matches as the number of people
## increases.

