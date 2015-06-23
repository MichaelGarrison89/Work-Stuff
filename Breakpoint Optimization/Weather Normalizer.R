

weather <- read.csv("nstar weather history.csv", header = TRUE) %>%
           filter(year >= 1985)

boston.hdds     <- select(weather, month, day, year, boston.temp)
boston.cdds     <- select(weather, month, day, year, boston.temp)
providence.hdds <- select(weather, month, day, year, providence.temp)
providence.cdds <- select(weather, month, day, year, providence.temp)


for(c in 1:100){
  
  boston.hdds[, 4 + c]     <- ifelse(boston.hdds$boston.temp < c,
                                     c - boston.hdds$boston.temp, 0)
  
  boston.cdds[, 4 + c]     <- ifelse(boston.cdds$boston.temp > c,
                                     boston.cdds$boston.temp - c, 0)
  
  providence.hdds[, 4 + c] <- ifelse(providence.hdds$providence.temp < c,
                                     c - providence.hdds$providence.temp, 0)
  
  providence.cdds[, 4 + c] <- ifelse(providence.cdds$providence.temp > c,
                                     providence.cdds$providence.temp - c, 0)
  
}

boston.hdd.normal <- select(boston.hdds, -(year:boston.temp)) %>%
                     group_by(month, day) %>%
                     summarise_each(funs(mean))

colnames(boston.hdd.normal) <- c("month", "day", seq(1, 100))



boston.cdd.normal <- select(boston.cdds, -(year:boston.temp)) %>%
                     group_by(month, day) %>%
                     summarise_each(funs(mean))

colnames(boston.cdd.normal) <- c("month", "day", seq(1, 100))


providence.hdd.normal <- select(providence.hdds, -(year:providence.temp)) %>%
                         group_by(month, day) %>%
                         summarise_each(funs(mean))

colnames(providence.hdd.normal) <- c("month", "day", seq(1, 100))



providence.cdd.normal <- select(providence.cdds, -(year:providence.temp)) %>%
                         group_by(month, day) %>%
                         summarise_each(funs(mean))

colnames(providence.cdd.normal) <- c("month", "day", seq(1, 100))