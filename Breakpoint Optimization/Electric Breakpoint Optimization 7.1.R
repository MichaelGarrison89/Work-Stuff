
# The purpose of this program is to find the optimal breakpoints for electric
# normalization.

# First we'll attach some supplemental packages we'll need and pull in the
# data.

library("grid", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("gridExtra", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("ggplot2", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("dplyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("tidyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("earth", lib.loc="C:/Program Files/R/R-3.1.3/library")



load.full <- read.csv("NSTAR Test Data.csv") %>%
             mutate(date = as.Date(date, "%m/%d/%Y")) %>%
             mutate(sunday = ifelse(weekdays(date) == "Sunday", 1, 0)) %>%
             mutate(monday = ifelse(weekdays(date) == "Monday", 1, 0)) %>%
             mutate(tuesday = ifelse(weekdays(date) == "Tuesday", 1, 0)) %>%
             mutate(wednesday = ifelse(weekdays(date) == "Wednesday", 1, 0)) %>%
             mutate(thursday = ifelse(weekdays(date) == "Thursday", 1, 0)) %>%
             mutate(friday = ifelse(weekdays(date) == "Friday", 1, 0)) %>%
             mutate(saturday = ifelse(weekdays(date) == "Saturday", 1, 0)) %>%
             mutate(weekday = ifelse(weekdays(date) == "Saturday" |
                                     weekdays(date) == "Sunday", 0, 1)) %>%
  
  # Now the monthly variables
             mutate(month = factor(months(date), levels = 
                                     c("January", "February", "March", 
                                       "April", "May", "June", "July", 
                                       "August", "September", "October", 
                                       "November", "December"))) %>%
             mutate(jan = ifelse(month == "January", 1, 0)) %>%
             mutate(feb = ifelse(month == "February", 1, 0)) %>%
             mutate(mar = ifelse(month == "March", 1, 0)) %>%
             mutate(apr = ifelse(month == "April", 1, 0)) %>%
             mutate(may = ifelse(month == "May", 1, 0)) %>%
             mutate(jun = ifelse(month == "June", 1, 0)) %>%
             mutate(jul = ifelse(month == "July", 1, 0)) %>%
             mutate(aug = ifelse(month == "August", 1, 0)) %>%
             mutate(sep = ifelse(month == "September", 1, 0)) %>%
             mutate(oct = ifelse(month == "October", 1, 0)) %>%
             mutate(nov = ifelse(month == "November", 1, 0)) %>%
             mutate(dec = ifelse(month == "December", 1, 0))


# We start with a global MARS analysis. This will show us, in general, how
# customers respond to weather. More importantly, we will use it to identify
# a "dead band" in which customers largely DO NOT respond to weather. We will
# remove the dead band from the data set before we continue our analysis.


mars <- earth(formula = beco.res ~ boston.temp, data = load.full)

# Here we check to see if there is any overlap between branches -- to use
# industry jargon: we'll check if we have a cdd breakpoint below at least one
# hdd breakpoint.

# If we do have an overlap, it's likely there's a deadband. We'll run a
# regression on the deadband data set. If the temperature variable is
# insignificant, we'll throw out the deadband data points.

if(sum(mars$dirs == 1)  > 0 &
   sum(mars$dirs == -1) > 0){
  

  dead.top <- max(mars$cuts[which(mars$dirs == -1)])
  dead.bot <- min(mars$cuts[which(mars$dirs ==  1)])
  
  dead.band.regress <-
    lm(beco.res ~ boston.temp, data = 
         filter(load.full, boston.temp <= dead.top & boston.temp >= dead.bot))
  
    if(dead.top > dead.bot &
       summary(dead.band.regress)$coefficients[2, 4] > 0.1){
      
      load <- 
        filter(load.full, boston.temp > dead.top | boston.temp < dead.bot)
      
    } else{
      
      load <- load.full
    }
  
  
  
} else{
  
  load <- load.full
}

load.full <- mutate(load.full,
                    dead.band = ifelse(boston.temp > dead.top |
                                       boston.temp < dead.bot, 1, 0))

mars.plot <-
  ggplot(load.full, aes(x = boston.temp, y = beco.res)) +
  geom_point(aes(color = factor(dead.band)), size = 2) +
  geom_line(y = predict(mars), color = "red", size = 1.5) +
  theme_minimal() +
  guides(color = FALSE, size = FALSE) +
  scale_color_manual(values = c("darkgrey", "black")) +
  labs(y = "Volumes", x = "Temperature", title = "beco.res vs. boston.temp")


# In this section we want to group the months into groups wherein each month
# has similar mean and standard deviation characteristics in the usage
# variable. We'll have different buckets for each sector.


beco.res.month.groups <- 
  select(load, boston.temp, month, beco.res) %>%
  group_by(month) %>%
  summarise(mean.temp = mean(boston.temp), mean.load = mean(beco.res))



beco.res.month.groups$slope     <- NA
beco.res.month.groups$intercept <- NA


beco.res.month.groups[1, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, jan == 1))$coefficients[2:1]

beco.res.month.groups[2, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, feb == 1))$coefficients[2:1]

beco.res.month.groups[3, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, mar == 1))$coefficients[2:1]

beco.res.month.groups[4, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, apr == 1))$coefficients[2:1]

beco.res.month.groups[5, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, may == 1))$coefficients[2:1]

beco.res.month.groups[6, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, jun == 1))$coefficients[2:1]

beco.res.month.groups[7, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, jul == 1))$coefficients[2:1]

beco.res.month.groups[8, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, aug == 1))$coefficients[2:1]

beco.res.month.groups[9, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, sep == 1))$coefficients[2:1]

beco.res.month.groups[10, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, oct == 1))$coefficients[2:1]

beco.res.month.groups[11, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, nov == 1))$coefficients[2:1]

beco.res.month.groups[12, 4:5] <- 
  lm(beco.res ~ 
       boston.temp, data = filter(load, dec == 1))$coefficients[2:1]


mean.temp      <- mean(beco.res.month.groups$mean.temp)
sd.temp        <- sd(beco.res.month.groups$mean.temp)
mean.load      <- mean(beco.res.month.groups$mean.load)
sd.load        <- sd(beco.res.month.groups$mean.load)
mean.slope     <- mean(beco.res.month.groups$slope)
sd.slope       <- sd(beco.res.month.groups$slope)
mean.intercept <- mean(beco.res.month.groups$intercept)
sd.intercept   <- sd(beco.res.month.groups$intercept)


# We have to re-scale the temperature and load numbers; otherwise the load
# numbers, which are substantially larger than temperature numbers, would
# dominate the clustering process.


beco.res.month.groups$mean.temp <- 
  (beco.res.month.groups$mean.temp - mean.temp) / sd.temp

beco.res.month.groups$mean.load <-
  (beco.res.month.groups$mean.load - mean.load) / sd.load

beco.res.month.groups$slope <- 
  (beco.res.month.groups$slope - mean.slope) / sd.slope

beco.res.month.groups$intercept <- 
  (beco.res.month.groups$intercept - mean.intercept) / sd.intercept


# We'll run the kmeans algorithm several times to help confirm that we get
# the global minimum of the within-cluster sum of squares, which would not
# be guaranteed with just one run. It's not guaranteed with 50, but I'm 
# pretty confident, given the number of clusters and parameters.

cluster.storage <- matrix(nrow = 1, ncol = 13)

cluster.first <- kmeans(beco.res.month.groups[, 4:5], 6)

cluster.storage <- c(cluster.first$cluster, 
                     sum(cluster.first$withinss))

for(c in 1:50){
  
  cluster.trial <- kmeans(beco.res.month.groups[, 4:5], 6)
  
  if(sum(cluster.trial$withinss) < cluster.storage[13]){
    cluster.storage <- c(cluster.trial$cluster, sum(cluster.trial$withinss))
  }
  
}


beco.res.month.groups$cluster.jumbled <- cluster.storage[1:12]


# This next section is basically unnecessary. I'm only doing it because kmeans
# assigns cluster names randomly, and it bothers me to have January be in
# cluster 2 sometimes and cluster 5 other times. January should always be in 
# cluster 1.

beco.res.month.groups$cluster <- NA



a <- 1

for(i in 1:12){
  
  if(is.na(beco.res.month.groups$cluster[i])){

    
    beco.res.month.groups <- 
      mutate(beco.res.month.groups, 
             cluster = ifelse(cluster.jumbled == 
                                cluster.jumbled[i], a, cluster))
    
    a <- a + 1
    
  }
  
  
}


# rownames(beco.res.month.groups) <- names(by(load$beco.res, load$month, mean))


# Here we'll set up our plot of the monthly means and the clusters they belong
# to.


groups.plot <-
  ggplot(beco.res.month.groups, aes(mean.temp, mean.load, 
                                    label = rownames(beco.res.month.groups),
                                    color = factor(cluster))) + 
  geom_point(size = 5) + 
  geom_text(size = 3, vjust = -1) +
  scale_x_continuous(breaks = seq(-3, 3)) +
  ylim(-2.5, 2.5) +
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal") +
  labs(color = "Cluster", x = "Mean Temperature", y = "Mean Load") + 
  ggtitle("Month Clusters")


# And now we will add bucket designations to our load data set. These will end
# up as binary variables in our regression.


load <- mutate(load, cluster = ifelse(month == "January", 
                                      beco.res.month.groups$cluster[1], 0)) %>%
  
        mutate(cluster = ifelse(month == "February",
                              beco.res.month.groups$cluster[2], cluster)) %>%
  
        mutate(cluster = ifelse(month == "March",
                              beco.res.month.groups$cluster[3], cluster)) %>%
  
        mutate(cluster = ifelse(month == "April",
                              beco.res.month.groups$cluster[4], cluster)) %>%
  
        mutate(cluster = ifelse(month == "May",
                              beco.res.month.groups$cluster[5], cluster)) %>%
  
        mutate(cluster = ifelse(month == "June",
                              beco.res.month.groups$cluster[6], cluster)) %>%
  
        mutate(cluster = ifelse(month == "July",
                              beco.res.month.groups$cluster[7], cluster)) %>%
  
        mutate(cluster = ifelse(month == "August",
                              beco.res.month.groups$cluster[8], cluster)) %>%
  
        mutate(cluster = ifelse(month == "September",
                              beco.res.month.groups$cluster[9], cluster)) %>%
  
        mutate(cluster = ifelse(month == "October",
                              beco.res.month.groups$cluster[10], cluster)) %>%
  
        mutate(cluster = ifelse(month == "November",
                              beco.res.month.groups$cluster[11], cluster)) %>%
  
        mutate(cluster = ifelse(month == "December",
                              beco.res.month.groups$cluster[12], cluster)) %>%
        
        mutate(group.1 = ifelse(cluster == 1, 1, 0)) %>%
        mutate(group.2 = ifelse(cluster == 2, 1, 0)) %>%
        mutate(group.3 = ifelse(cluster == 3, 1, 0)) %>%
        mutate(group.4 = ifelse(cluster == 4, 1, 0)) %>%
        mutate(group.5 = ifelse(cluster == 5, 1, 0)) %>%
        mutate(group.6 = ifelse(cluster == 6, 1, 0))




month.grid.plot <- 
  ggplot(load, aes(x = boston.temp, y = beco.res, color = factor(cluster))) + 
  geom_point(aes(shape = factor(weekday))) + 
  geom_smooth(method = lm, se = FALSE, size = 1, formula = y ~ x,
              aes(x = boston.temp, y = beco.res)) +
  scale_color_manual(values = c("dodgerblue", "lightblue", "lightgreen",
                                "hotpink", "orange", "red")) + 
  scale_shape_manual(values = c(17, 16)) +
  facet_wrap( ~ month, nrow = 3) + 
  labs(x = "Boston Temp", y = "Daily Load", 
       color = "Cluster", shape = "Weekday") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey")) +
  guides(color = FALSE, shape = FALSE)





load.group.1 <- filter(load, cluster == 1)
load.group.2 <- filter(load, cluster == 2)
load.group.3 <- filter(load, cluster == 3)
load.group.4 <- filter(load, cluster == 4)
load.group.5 <- filter(load, cluster == 5)
load.group.6 <- filter(load, cluster == 6)



results.matrix   <- matrix(nrow = 6, ncol = 8)
results.matrix.2.beco.res <- matrix(nrow = 6, ncol = 9)
results.matrix.2.beco.res <- data.frame(results.matrix.2.beco.res)



# Now that we've set everything up, we're going to run through finding
# breakpoints for each of our 6 groups.

# Here's our baseline regression. If we can't beat the SSR of this, then we'll
# use no breakpoints in our output. We also use this quick check to find out
# whether we're in a heating group or a cooling group.


base.regress <-
  
lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
              memorial + indep + labor + columbus + veterans + thanks +
              thanks.fri + christmas,
   data = load.group.1)


test.base <- sum(base.regress$residuals ^ 2)


# I want the whole range of temperatues, but I don't want to consider any
# that include just one data point, so I'll move the bounds in a little.


top <- sort(load.group.1$boston.temp)[nrow(load.group.1) - 1]
bot <- sort(load.group.1$boston.temp)[2]


test.break.1 <- matrix(nrow = (top - bot - 1), ncol = 8)

colnames(test.break.1) <- c("SSR", "breakpoint", "ordered",
                            "slope a", "slope b", "signif. a",
                            "signif. b", "b signif. match")


# This if statement is sort of secretly critical. The breakpoints of the
# groups are a function of the in-group relationship between temperature
# and load. If that relationship is positive (for hot months), then we
# want our high-temperature breakpoints to have higher slopes than our low temp
# breakpoints. In the winter, the reverse is true. So this <flip> variable
# reflects that.


if(base.regress$coefficients[2] > 0){
  
  flip <- -1
  bot  <- max(bot, dead.top)
  
} else{
  
  flip <- 1
  top  <- min(top, dead.bot)
}


for(i in 1:(top - bot - 1)){
  
  # We're using our flip variable here. Recall that the fip variable reflects
  # whether we're in a heating or cooling group. The fact that it can be 1 or
  # -1 allows us to use the same inequality to define our break.1 variable in
  # heating or cooling groups.
  
  load.group.1 <- 
    mutate(load.group.1, 
           break.1 = ifelse(flip * boston.temp < flip * (top - i), 1, 0))

  
  
  regress.break.1.a <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.1, break.1 == 1))
  
  
  regress.break.1.b <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.1, break.1 == 0))
  
  # Now we'll record everything interestingn from these two regressions.
  
  test.break.1[i, 1] <- sum(regress.break.1.a$residuals ^ 2) + 
    sum(regress.break.1.b$residuals ^ 2)
  
  test.break.1[i, 2] <- top - i
  
  test.break.1[i, 3] <- abs(regress.break.1.a$coefficients[2]) > 
                        abs(regress.break.1.b$coefficients[2])
  
  test.break.1[i, 4] <- regress.break.1.a$coefficients[2]
  test.break.1[i, 5] <- regress.break.1.b$coefficients[2]
  
  if(nrow(summary(regress.break.1.a)$coefficients) > 1){
    test.break.1[i, 6] <- summary(regress.break.1.a)$coefficients[2, 4]
  } else{
    test.break.1[i, 6] <- NA
  }
  
  if(nrow(summary(regress.break.1.b)$coefficients) > 1){
    test.break.1[i, 7] <- summary(regress.break.1.b)$coefficients[2, 4]
  } else{
    test.break.1[i, 7] <- NA
  }
  
  # This if statement has a lot of conditions. We're producing a variable that
  # tells us whether we should consider a given regression. We will consider
  # anything unless the low breakpoint coefficient is significant and has a
  # different sign than the high-breakpoint coefficient. And we'll only apply
  # that test when both regressions have non-NA coefficients on weather.
  
  if(!is.na(test.break.1[i, 6]) &
     !is.na(test.break.1[i, 7]) &
     test.break.1[i, 7] < 0.25 & 
     sign(test.break.1[i, 4]) != sign(test.break.1[i, 5])){
    
    test.break.1[i, 8] <- 0
    
  } else{
    
    test.break.1[i, 8] <- 1
    
  }

  
  
}

test.winner.1 <- filter(data.frame(test.break.1), 
                 ordered == 1, signif..a < 0.1, b.signif..match == 1) %>%
                 summarise(min(SSR))


# Occasionally we're going to have multiple winners, but that's usually going
# to be the result of not having data at that particular breakpoint. In that
# situation, the default should be to take the more central of the two.

winner.location.1 <- which(test.break.1[, 1] == as.numeric(test.winner.1))

if(length(winner.location.1) > 1){
  
 midpoint <- nrow(test.break.1) / 2 
 
 min.level <- min(abs(winner.location.1 - midpoint)) 
  
 min.point <- which(abs(winner.location.1 - midpoint) == min.level)
 
 winner.location.1 <- winner.location.1[min.point]
 
}

# Here again we're being really tricky with the flip variable. If flip is 1, 
# then it's a heating month and we want break.1 to reflect temperatures COLDER
# than our breakpoint. In cooling months, flip will be -1, so negative
# temperature is less than negative breakpoint, which is another way of saying
# temperature is ABOVE the breakpoint. The end result is that break.1 always
# reflects more extreme temperatures, and we can use the same ifelse statement
# in heating and cooling months.

break.1.temp <- test.break.1[winner.location.1, 2]

if(test.winner.1 < test.base){
  
  load.group.1 <- 
    mutate(load.group.1, 
           break.1 = ifelse(flip * boston.temp < 
                            flip * break.1.temp, 1, 0))

 
} else{
  
  load.group.1$break.1 <- 1
  
  
}



results.matrix[1, ] <- test.break.1[winner.location.1, ]


if(flip == -1){
  
  load.group.1 <- 
    mutate(load.group.1, 
           w.0 = ifelse(break.1 == 0, boston.temp - dead.top, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, boston.temp - break.1.temp, 0))

  
} else {
  
  load.group.1 <- 
    mutate(load.group.1, 
           w.0 = ifelse(break.1 == 0, dead.bot - boston.temp, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, break.1.temp - boston.temp, 0))
  
}


# Now we'll re-run and store our winning regressions.

regress.final.1.a <-
  lm(beco.res ~ w.1 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.1, break.1 == 1))


regress.final.1.b <-
  lm(beco.res ~ w.0 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.1, break.1 == 0))


results.matrix.2.beco.res[1, 1] <- regress.final.1.a$coefficients[2]
results.matrix.2.beco.res[1, 2] <- regress.final.1.b$coefficients[2]

results.matrix.2.beco.res[1, 3] <- summary(regress.final.1.a)$coefficients[2, 4]
results.matrix.2.beco.res[1, 4] <- summary(regress.final.1.b)$coefficients[2, 4]


if(flip == 1){
  
  results.matrix.2.beco.res[1, 5] <- "Heating"
  results.matrix.2.beco.res[1, 8] <- dead.bot
  results.matrix.2.beco.res[1, 9] <- results.matrix[1, 2]
  
} else {
  
  results.matrix.2.beco.res[1, 5] <- "Cooling"
  results.matrix.2.beco.res[1, 8] <- dead.top
  results.matrix.2.beco.res[1, 9] <- results.matrix[1, 2]
  
}


results.matrix.2.beco.res[1, 6] <- min(load.group.1$boston.temp)
results.matrix.2.beco.res[1, 7] <- max(load.group.1$boston.temp)











# Here's our baseline regression. If we can't beat the SSR of this, then we'll
# use no breakpoints in our output. We also use this quick check to find out
# whether we're in a heating group or a cooling group.


base.regress <-
  
  lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = load.group.2)


test.base <- sum(base.regress$residuals ^ 2)


# I want the whole range of temperatues, but I don't want to consider any
# that include just one data point, so I'll move the bounds in a little.


top <- sort(load.group.2$boston.temp)[nrow(load.group.2) - 1]
bot <- sort(load.group.2$boston.temp)[2]


test.break.2 <- matrix(nrow = (top - bot - 1), ncol = 8)

colnames(test.break.2) <- c("SSR", "breakpoint", "ordered",
                            "slope a", "slope b", "signif. a",
                            "signif. b", "b signif. match")


# This if statement is sort of secretly critical. The breakpoints of the
# groups are a function of the in-group relationship between temperature
# and load. If that relationship is positive (for hot months), then we
# want our high-temperature breakpoints to have higher slopes than our low temp
# breakpoints. In the winter, the reverse is true. So this <flip> variable
# reflects that.


if(base.regress$coefficients[2] > 0){
  
  flip <- -1
  bot  <- max(bot, dead.top)
  
} else{
  
  flip <- 1
  top  <- min(top, dead.bot)
}


for(i in 1:(top - bot - 1)){
  
  # We're using our flip variable here. Recall that the fip variable reflects
  # whether we're in a heating or cooling group. The fact that it can be 1 or
  # -1 allows us to use the same inequality to define our break.1 variable in
  # heating or cooling groups.
  
  load.group.2 <- 
    mutate(load.group.2, 
           break.1 = ifelse(flip * boston.temp < flip * (top - i), 1, 0))
  
  
  
  regress.break.2.a <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.2, break.1 == 1))
  
  
  regress.break.2.b <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.2, break.1 == 0))
  
  # Now we'll record everything interestingn from these two regressions.
  
  test.break.2[i, 1] <- sum(regress.break.2.a$residuals ^ 2) + 
    sum(regress.break.2.b$residuals ^ 2)
  
  test.break.2[i, 2] <- top - i
  
  test.break.2[i, 3] <- abs(regress.break.2.a$coefficients[2]) > 
    abs(regress.break.2.b$coefficients[2])
  
  test.break.2[i, 4] <- regress.break.2.a$coefficients[2]
  test.break.2[i, 5] <- regress.break.2.b$coefficients[2]
  
  if(nrow(summary(regress.break.2.a)$coefficients) > 1){
    test.break.2[i, 6] <- summary(regress.break.2.a)$coefficients[2, 4]
  } else{
    test.break.2[i, 6] <- NA
  }
  
  if(nrow(summary(regress.break.2.b)$coefficients) > 1){
    test.break.2[i, 7] <- summary(regress.break.2.b)$coefficients[2, 4]
  } else{
    test.break.2[i, 7] <- NA
  }
  
  # This if statement has a lot of conditions. We're producing a variable that
  # tells us whether we should consider a given regression. We will consider
  # anything unless the low breakpoint coefficient is significant and has a
  # different sign than the high-breakpoint coefficient. And we'll only apply
  # that test when both regressions have non-NA coefficients on weather.
  
  if(!is.na(test.break.2[i, 6]) &
       !is.na(test.break.2[i, 7]) &
       test.break.2[i, 7] < 0.25 & 
       sign(test.break.2[i, 4]) != sign(test.break.2[i, 5])){
    
    test.break.2[i, 8] <- 0
    
  } else{
    
    test.break.2[i, 8] <- 1
    
  }
  
  
  
}

test.winner.2 <- filter(data.frame(test.break.2), 
                        ordered == 1, signif..a < 0.1, b.signif..match == 1) %>%
  summarise(min(SSR))


# Occasionally we're going to have multiple winners, but that's usually going
# to be the result of not having data at that particular breakpoint. In that
# situation, the default should be to take the more central of the two.

winner.location.2 <- which(test.break.2[, 1] == as.numeric(test.winner.2))

if(length(winner.location.2) > 1){
  
  midpoint <- nrow(test.break.2) / 2 
  
  min.level <- min(abs(winner.location.2 - midpoint)) 
  
  min.point <- which(abs(winner.location.2 - midpoint) == min.level)
  
  winner.location.2 <- winner.location.2[min.point]
  
}

# Here again we're being really tricky with the flip variable. If flip is 1, 
# then it's a heating month and we want break.1 to reflect temperatures COLDER
# than our breakpoint. In cooling months, flip will be -1, so negative
# temperature is less than negative breakpoint, which is another way of saying
# temperature is ABOVE the breakpoint. The end result is that break.1 always
# reflects more extreme temperatures, and we can use the same ifelse statement
# in heating and cooling months.

break.2.temp <- test.break.2[winner.location.2, 2]

if(test.winner.2 < test.base){
  
  load.group.2 <- 
    mutate(load.group.2, 
           break.1 = ifelse(flip * boston.temp < 
                              flip * break.2.temp, 1, 0))
  
  
} else{
  
  load.group.2$break.1 <- 1
  
  
}



results.matrix[2, ] <- test.break.2[winner.location.2, ]


if(flip == -1){
  
  load.group.2 <- 
    mutate(load.group.2, 
           w.0 = ifelse(break.1 == 0, boston.temp - dead.top, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, boston.temp - break.2.temp, 0))
  
  
} else {
  
  load.group.2 <- 
    mutate(load.group.2, 
           w.0 = ifelse(break.1 == 0, dead.bot - boston.temp, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, break.2.temp - boston.temp, 0))
  
}


# Now we'll re-run and store our winning regressions.

regress.final.2.a <-
  lm(beco.res ~ w.1 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.2, break.1 == 1))


regress.final.2.b <-
  lm(beco.res ~ w.0 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.2, break.1 == 0))


results.matrix.2.beco.res[2, 1] <- regress.final.2.a$coefficients[2]
results.matrix.2.beco.res[2, 2] <- regress.final.2.b$coefficients[2]

results.matrix.2.beco.res[2, 3] <- summary(regress.final.2.a)$coefficients[2, 4]
results.matrix.2.beco.res[2, 4] <- summary(regress.final.2.b)$coefficients[2, 4]


if(flip == 1){
  
  results.matrix.2.beco.res[2, 5] <- "Heating"
  results.matrix.2.beco.res[2, 8] <- dead.bot
  results.matrix.2.beco.res[2, 9] <- results.matrix[2, 2]
  
} else {
  
  results.matrix.2.beco.res[2, 5] <- "Cooling"
  results.matrix.2.beco.res[2, 8] <- dead.top
  results.matrix.2.beco.res[2, 9] <- results.matrix[2, 2]
  
}


results.matrix.2.beco.res[2, 6] <- min(load.group.2$boston.temp)
results.matrix.2.beco.res[2, 7] <- max(load.group.2$boston.temp)














# Here's our baseline regression. If we can't beat the SSR of this, then we'll
# use no breakpoints in our output. We also use this quick check to find out
# whether we're in a heating group or a cooling group.


base.regress <-
  
  lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = load.group.3)


test.base <- sum(base.regress$residuals ^ 2)


# I want the whole range of temperatues, but I don't want to consider any
# that include just one data point, so I'll move the bounds in a little.


top <- sort(load.group.3$boston.temp)[nrow(load.group.3) - 1]
bot <- sort(load.group.3$boston.temp)[2]


test.break.3 <- matrix(nrow = (top - bot - 1), ncol = 8)

colnames(test.break.3) <- c("SSR", "breakpoint", "ordered",
                            "slope a", "slope b", "signif. a",
                            "signif. b", "b signif. match")


# This if statement is sort of secretly critical. The breakpoints of the
# groups are a function of the in-group relationship between temperature
# and load. If that relationship is positive (for hot months), then we
# want our high-temperature breakpoints to have higher slopes than our low temp
# breakpoints. In the winter, the reverse is true. So this <flip> variable
# reflects that.


if(base.regress$coefficients[2] > 0){
  
  flip <- -1
  bot  <- max(bot, dead.top)
  
} else{
  
  flip <- 1
  top  <- min(top, dead.bot)
}


for(i in 1:(top - bot - 1)){
  
  # We're using our flip variable here. Recall that the fip variable reflects
  # whether we're in a heating or cooling group. The fact that it can be 1 or
  # -1 allows us to use the same inequality to define our break.1 variable in
  # heating or cooling groups.
  
  load.group.3 <- 
    mutate(load.group.3, 
           break.1 = ifelse(flip * boston.temp < flip * (top - i), 1, 0))
  
  
  
  regress.break.3.a <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.3, break.1 == 1))
  
  
  regress.break.3.b <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.3, break.1 == 0))
  
  # Now we'll record everything interestingn from these two regressions.
  
  test.break.3[i, 1] <- sum(regress.break.3.a$residuals ^ 2) + 
    sum(regress.break.3.b$residuals ^ 2)
  
  test.break.3[i, 2] <- top - i
  
  test.break.3[i, 3] <- abs(regress.break.3.a$coefficients[2]) > 
    abs(regress.break.3.b$coefficients[2])
  
  test.break.3[i, 4] <- regress.break.3.a$coefficients[2]
  test.break.3[i, 5] <- regress.break.3.b$coefficients[2]
  
  if(nrow(summary(regress.break.3.a)$coefficients) > 1){
    test.break.3[i, 6] <- summary(regress.break.3.a)$coefficients[2, 4]
  } else{
    test.break.3[i, 6] <- NA
  }
  
  if(nrow(summary(regress.break.3.b)$coefficients) > 1){
    test.break.3[i, 7] <- summary(regress.break.3.b)$coefficients[2, 4]
  } else{
    test.break.3[i, 7] <- NA
  }
  
  # This if statement has a lot of conditions. We're producing a variable that
  # tells us whether we should consider a given regression. We will consider
  # anything unless the low breakpoint coefficient is significant and has a
  # different sign than the high-breakpoint coefficient. And we'll only apply
  # that test when both regressions have non-NA coefficients on weather.
  
  if(!is.na(test.break.3[i, 6]) &
       !is.na(test.break.3[i, 7]) &
       test.break.3[i, 7] < 0.25 & 
       sign(test.break.3[i, 4]) != sign(test.break.3[i, 5])){
    
    test.break.3[i, 8] <- 0
    
  } else{
    
    test.break.3[i, 8] <- 1
    
  }
  
  
  
}

test.winner.3 <- filter(data.frame(test.break.3), 
                        ordered == 1, signif..a < 0.1, b.signif..match == 1) %>%
  summarise(min(SSR))


# Occasionally we're going to have multiple winners, but that's usually going
# to be the result of not having data at that particular breakpoint. In that
# situation, the default should be to take the more central of the two.

winner.location.3 <- which(test.break.3[, 1] == as.numeric(test.winner.3))

if(length(winner.location.3) > 1){
  
  midpoint <- nrow(test.break.3) / 2 
  
  min.level <- min(abs(winner.location.3 - midpoint)) 
  
  min.point <- which(abs(winner.location.3 - midpoint) == min.level)
  
  winner.location.3 <- winner.location.3[min.point]
  
}

# Here again we're being really tricky with the flip variable. If flip is 1, 
# then it's a heating month and we want break.1 to reflect temperatures COLDER
# than our breakpoint. In cooling months, flip will be -1, so negative
# temperature is less than negative breakpoint, which is another way of saying
# temperature is ABOVE the breakpoint. The end result is that break.1 always
# reflects more extreme temperatures, and we can use the same ifelse statement
# in heating and cooling months.

break.3.temp <- test.break.3[winner.location.3, 2]

if(test.winner.3 < test.base){
  
  load.group.3 <- 
    mutate(load.group.3, 
           break.1 = ifelse(flip * boston.temp < 
                              flip * break.3.temp, 1, 0))
  
  
} else{
  
  load.group.3$break.1 <- 1
  
  
}



results.matrix[3, ] <- test.break.3[winner.location.3, ]


if(flip == -1){
  
  load.group.3 <- 
    mutate(load.group.3, 
           w.0 = ifelse(break.1 == 0, boston.temp - dead.top, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, boston.temp - break.3.temp, 0))
  
  
} else {
  
  load.group.3 <- 
    mutate(load.group.3, 
           w.0 = ifelse(break.1 == 0, dead.bot - boston.temp, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, break.3.temp - boston.temp, 0))
  
}


# Now we'll re-run and store our winning regressions.

regress.final.3.a <-
  lm(beco.res ~ w.1 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.3, break.1 == 1))


regress.final.3.b <-
  lm(beco.res ~ w.0 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.3, break.1 == 0))


results.matrix.2.beco.res[3, 1] <- regress.final.3.a$coefficients[2]
results.matrix.2.beco.res[3, 2] <- regress.final.3.b$coefficients[2]

results.matrix.2.beco.res[3, 3] <- summary(regress.final.3.a)$coefficients[2, 4]
results.matrix.2.beco.res[3, 4] <- summary(regress.final.3.b)$coefficients[2, 4]


if(flip == 1){
  
  results.matrix.2.beco.res[3, 5] <- "Heating"
  results.matrix.2.beco.res[3, 8] <- dead.bot
  results.matrix.2.beco.res[3, 9] <- results.matrix[3, 2]
  
} else {
  
  results.matrix.2.beco.res[3, 5] <- "Cooling"
  results.matrix.2.beco.res[3, 8] <- dead.top
  results.matrix.2.beco.res[3, 9] <- results.matrix[3, 2]
  
}


results.matrix.2.beco.res[3, 6] <- min(load.group.3$boston.temp)
results.matrix.2.beco.res[3, 7] <- max(load.group.3$boston.temp)










# Here's our baseline regression. If we can't beat the SSR of this, then we'll
# use no breakpoints in our output. We also use this quick check to find out
# whether we're in a heating group or a cooling group.


base.regress <-
  
  lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = load.group.4)


test.base <- sum(base.regress$residuals ^ 2)


# I want the whole range of temperatues, but I don't want to consider any
# that include just one data point, so I'll move the bounds in a little.


top <- sort(load.group.4$boston.temp)[nrow(load.group.4) - 1]
bot <- sort(load.group.4$boston.temp)[2]


test.break.4 <- matrix(nrow = (top - bot - 1), ncol = 8)

colnames(test.break.4) <- c("SSR", "breakpoint", "ordered",
                            "slope a", "slope b", "signif. a",
                            "signif. b", "b signif. match")


# This if statement is sort of secretly critical. The breakpoints of the
# groups are a function of the in-group relationship between temperature
# and load. If that relationship is positive (for hot months), then we
# want our high-temperature breakpoints to have higher slopes than our low temp
# breakpoints. In the winter, the reverse is true. So this <flip> variable
# reflects that.


if(base.regress$coefficients[2] > 0){
  
  flip <- -1
  bot  <- max(bot, dead.top)
  
} else{
  
  flip <- 1
  top  <- min(top, dead.bot)
}


for(i in 1:(top - bot - 1)){
  
  # We're using our flip variable here. Recall that the fip variable reflects
  # whether we're in a heating or cooling group. The fact that it can be 1 or
  # -1 allows us to use the same inequality to define our break.1 variable in
  # heating or cooling groups.
  
  load.group.4 <- 
    mutate(load.group.4, 
           break.1 = ifelse(flip * boston.temp < flip * (top - i), 1, 0))
  
  
  
  regress.break.4.a <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.4, break.1 == 1))
  
  
  regress.break.4.b <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.4, break.1 == 0))
  
  # Now we'll record everything interestingn from these two regressions.
  
  test.break.4[i, 1] <- sum(regress.break.4.a$residuals ^ 2) + 
    sum(regress.break.4.b$residuals ^ 2)
  
  test.break.4[i, 2] <- top - i
  
  test.break.4[i, 3] <- abs(regress.break.4.a$coefficients[2]) > 
    abs(regress.break.4.b$coefficients[2])
  
  test.break.4[i, 4] <- regress.break.4.a$coefficients[2]
  test.break.4[i, 5] <- regress.break.4.b$coefficients[2]
  
  if(nrow(summary(regress.break.4.a)$coefficients) > 1){
    test.break.4[i, 6] <- summary(regress.break.4.a)$coefficients[2, 4]
  } else{
    test.break.4[i, 6] <- NA
  }
  
  if(nrow(summary(regress.break.4.b)$coefficients) > 1){
    test.break.4[i, 7] <- summary(regress.break.4.b)$coefficients[2, 4]
  } else{
    test.break.4[i, 7] <- NA
  }
  
  # This if statement has a lot of conditions. We're producing a variable that
  # tells us whether we should consider a given regression. We will consider
  # anything unless the low breakpoint coefficient is significant and has a
  # different sign than the high-breakpoint coefficient. And we'll only apply
  # that test when both regressions have non-NA coefficients on weather.
  
  if(!is.na(test.break.4[i, 6]) &
       !is.na(test.break.4[i, 7]) &
       test.break.4[i, 7] < 0.25 & 
       sign(test.break.4[i, 4]) != sign(test.break.4[i, 5])){
    
    test.break.4[i, 8] <- 0
    
  } else{
    
    test.break.4[i, 8] <- 1
    
  }
  
  
  
}

test.winner.4 <- filter(data.frame(test.break.4), 
                        ordered == 1, signif..a < 0.1, b.signif..match == 1) %>%
  summarise(min(SSR))


# Occasionally we're going to have multiple winners, but that's usually going
# to be the result of not having data at that particular breakpoint. In that
# situation, the default should be to take the more central of the two.

winner.location.4 <- which(test.break.4[, 1] == as.numeric(test.winner.4))

if(length(winner.location.4) > 1){
  
  midpoint <- nrow(test.break.4) / 2 
  
  min.level <- min(abs(winner.location.4 - midpoint)) 
  
  min.point <- which(abs(winner.location.4 - midpoint) == min.level)
  
  winner.location.4 <- winner.location.4[min.point]
  
}

# Here again we're being really tricky with the flip variable. If flip is 1, 
# then it's a heating month and we want break.1 to reflect temperatures COLDER
# than our breakpoint. In cooling months, flip will be -1, so negative
# temperature is less than negative breakpoint, which is another way of saying
# temperature is ABOVE the breakpoint. The end result is that break.1 always
# reflects more extreme temperatures, and we can use the same ifelse statement
# in heating and cooling months.

break.4.temp <- test.break.4[winner.location.4, 2]

if(test.winner.4 < test.base){
  
  load.group.4 <- 
    mutate(load.group.4, 
           break.1 = ifelse(flip * boston.temp < 
                              flip * break.4.temp, 1, 0))
  
  
} else{
  
  load.group.4$break.1 <- 1
  
  
}



results.matrix[4, ] <- test.break.4[winner.location.4, ]


if(flip == -1){
  
  load.group.4 <- 
    mutate(load.group.4, 
           w.0 = ifelse(break.1 == 0, boston.temp - dead.top, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, boston.temp - break.4.temp, 0))
  
  
} else {
  
  load.group.4 <- 
    mutate(load.group.4, 
           w.0 = ifelse(break.1 == 0, dead.bot - boston.temp, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, break.4.temp - boston.temp, 0))
  
}


# Now we'll re-run and store our winning regressions.

regress.final.4.a <-
  lm(beco.res ~ w.1 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.4, break.1 == 1))


regress.final.4.b <-
  lm(beco.res ~ w.0 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.4, break.1 == 0))


results.matrix.2.beco.res[4, 1] <- regress.final.4.a$coefficients[2]
results.matrix.2.beco.res[4, 2] <- regress.final.4.b$coefficients[2]

results.matrix.2.beco.res[4, 3] <- summary(regress.final.4.a)$coefficients[2, 4]
results.matrix.2.beco.res[4, 4] <- summary(regress.final.4.b)$coefficients[2, 4]


if(flip == 1){
  
  results.matrix.2.beco.res[4, 5] <- "Heating"
  results.matrix.2.beco.res[4, 8] <- dead.bot
  results.matrix.2.beco.res[4, 9] <- results.matrix[4, 2]
  
} else {
  
  results.matrix.2.beco.res[4, 5] <- "Cooling"
  results.matrix.2.beco.res[4, 8] <- dead.top
  results.matrix.2.beco.res[4, 9] <- results.matrix[4, 2]
  
}


results.matrix.2.beco.res[4, 6] <- min(load.group.4$boston.temp)
results.matrix.2.beco.res[4, 7] <- max(load.group.4$boston.temp)

















# Here's our baseline regression. If we can't beat the SSR of this, then we'll
# use no breakpoints in our output. We also use this quick check to find out
# whether we're in a heating group or a cooling group.


base.regress <-
  
  lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = load.group.5)


test.base <- sum(base.regress$residuals ^ 2)


# I want the whole range of temperatues, but I don't want to consider any
# that include just one data point, so I'll move the bounds in a little.


top <- sort(load.group.5$boston.temp)[nrow(load.group.5) - 1]
bot <- sort(load.group.5$boston.temp)[2]


test.break.5 <- matrix(nrow = (top - bot - 1), ncol = 8)

colnames(test.break.5) <- c("SSR", "breakpoint", "ordered",
                            "slope a", "slope b", "signif. a",
                            "signif. b", "b signif. match")


# This if statement is sort of secretly critical. The breakpoints of the
# groups are a function of the in-group relationship between temperature
# and load. If that relationship is positive (for hot months), then we
# want our high-temperature breakpoints to have higher slopes than our low temp
# breakpoints. In the winter, the reverse is true. So this <flip> variable
# reflects that.


if(base.regress$coefficients[2] > 0){
  
  flip <- -1
  bot  <- max(bot, dead.top)
  
} else{
  
  flip <- 1
  top  <- min(top, dead.bot)
}


for(i in 1:(top - bot - 1)){
  
  # We're using our flip variable here. Recall that the fip variable reflects
  # whether we're in a heating or cooling group. The fact that it can be 1 or
  # -1 allows us to use the same inequality to define our break.1 variable in
  # heating or cooling groups.
  
  load.group.5 <- 
    mutate(load.group.5, 
           break.1 = ifelse(flip * boston.temp < flip * (top - i), 1, 0))
  
  
  
  regress.break.5.a <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.5, break.1 == 1))
  
  
  regress.break.5.b <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.5, break.1 == 0))
  
  # Now we'll record everything interestingn from these two regressions.
  
  test.break.5[i, 1] <- sum(regress.break.5.a$residuals ^ 2) + 
    sum(regress.break.5.b$residuals ^ 2)
  
  test.break.5[i, 2] <- top - i
  
  test.break.5[i, 3] <- abs(regress.break.5.a$coefficients[2]) > 
    abs(regress.break.5.b$coefficients[2])
  
  test.break.5[i, 4] <- regress.break.5.a$coefficients[2]
  test.break.5[i, 5] <- regress.break.5.b$coefficients[2]
  
  if(nrow(summary(regress.break.5.a)$coefficients) > 1){
    test.break.5[i, 6] <- summary(regress.break.5.a)$coefficients[2, 4]
  } else{
    test.break.5[i, 6] <- NA
  }
  
  if(nrow(summary(regress.break.5.b)$coefficients) > 1){
    test.break.5[i, 7] <- summary(regress.break.5.b)$coefficients[2, 4]
  } else{
    test.break.5[i, 7] <- NA
  }
  
  # This if statement has a lot of conditions. We're producing a variable that
  # tells us whether we should consider a given regression. We will consider
  # anything unless the low breakpoint coefficient is significant and has a
  # different sign than the high-breakpoint coefficient. And we'll only apply
  # that test when both regressions have non-NA coefficients on weather.
  
  if(!is.na(test.break.5[i, 6]) &
       !is.na(test.break.5[i, 7]) &
       test.break.5[i, 7] < 0.25 & 
       sign(test.break.5[i, 4]) != sign(test.break.5[i, 5])){
    
    test.break.5[i, 8] <- 0
    
  } else{
    
    test.break.5[i, 8] <- 1
    
  }
  
  
  
}

test.winner.5 <- filter(data.frame(test.break.5), 
                        ordered == 1, signif..a < 0.1, b.signif..match == 1) %>%
  summarise(min(SSR))


# Occasionally we're going to have multiple winners, but that's usually going
# to be the result of not having data at that particular breakpoint. In that
# situation, the default should be to take the more central of the two.

winner.location.5 <- which(test.break.5[, 1] == as.numeric(test.winner.5))

if(length(winner.location.5) > 1){
  
  midpoint <- nrow(test.break.5) / 2 
  
  min.level <- min(abs(winner.location.5 - midpoint)) 
  
  min.point <- which(abs(winner.location.5 - midpoint) == min.level)
  
  winner.location.5 <- winner.location.5[min.point]
  
}

# Here again we're being really tricky with the flip variable. If flip is 1, 
# then it's a heating month and we want break.1 to reflect temperatures COLDER
# than our breakpoint. In cooling months, flip will be -1, so negative
# temperature is less than negative breakpoint, which is another way of saying
# temperature is ABOVE the breakpoint. The end result is that break.1 always
# reflects more extreme temperatures, and we can use the same ifelse statement
# in heating and cooling months.

break.5.temp <- test.break.5[winner.location.5, 2]

if(test.winner.5 < test.base){
  
  load.group.5 <- 
    mutate(load.group.5, 
           break.1 = ifelse(flip * boston.temp < 
                              flip * break.5.temp, 1, 0))
  
  
} else{
  
  load.group.5$break.1 <- 1
  
  
}



results.matrix[5, ] <- test.break.5[winner.location.5, ]


if(flip == -1){
  
  load.group.5 <- 
    mutate(load.group.5, 
           w.0 = ifelse(break.1 == 0, boston.temp - dead.top, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, boston.temp - break.5.temp, 0))
  
  
} else {
  
  load.group.5 <- 
    mutate(load.group.5, 
           w.0 = ifelse(break.1 == 0, dead.bot - boston.temp, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, break.5.temp - boston.temp, 0))
  
}


# Now we'll re-run and store our winning regressions.

regress.final.5.a <-
  lm(beco.res ~ w.1 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.5, break.1 == 1))


regress.final.5.b <-
  lm(beco.res ~ w.0 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.5, break.1 == 0))


results.matrix.2.beco.res[5, 1] <- regress.final.5.a$coefficients[2]
results.matrix.2.beco.res[5, 2] <- regress.final.5.b$coefficients[2]

results.matrix.2.beco.res[5, 3] <- summary(regress.final.5.a)$coefficients[2, 4]
results.matrix.2.beco.res[5, 4] <- summary(regress.final.5.b)$coefficients[2, 4]


if(flip == 1){
  
  results.matrix.2.beco.res[5, 5] <- "Heating"
  results.matrix.2.beco.res[5, 8] <- dead.bot
  results.matrix.2.beco.res[5, 9] <- results.matrix[5, 2]
  
} else {
  
  results.matrix.2.beco.res[5, 5] <- "Cooling"
  results.matrix.2.beco.res[5, 8] <- dead.top
  results.matrix.2.beco.res[5, 9] <- results.matrix[5, 2]
  
}


results.matrix.2.beco.res[5, 6] <- min(load.group.5$boston.temp)
results.matrix.2.beco.res[5, 7] <- max(load.group.5$boston.temp)










# Here's our baseline regression. If we can't beat the SSR of this, then we'll
# use no breakpoints in our output. We also use this quick check to find out
# whether we're in a heating group or a cooling group.


base.regress <-
  
  lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = load.group.6)


test.base <- sum(base.regress$residuals ^ 2)


# I want the whole range of temperatues, but I don't want to consider any
# that include just one data point, so I'll move the bounds in a little.


top <- sort(load.group.6$boston.temp)[nrow(load.group.6) - 1]
bot <- sort(load.group.6$boston.temp)[2]


test.break.6 <- matrix(nrow = (top - bot - 1), ncol = 8)

colnames(test.break.6) <- c("SSR", "breakpoint", "ordered",
                            "slope a", "slope b", "signif. a",
                            "signif. b", "b signif. match")


# This if statement is sort of secretly critical. The breakpoints of the
# groups are a function of the in-group relationship between temperature
# and load. If that relationship is positive (for hot months), then we
# want our high-temperature breakpoints to have higher slopes than our low temp
# breakpoints. In the winter, the reverse is true. So this <flip> variable
# reflects that.


if(base.regress$coefficients[2] > 0){
  
  flip <- -1
  bot  <- max(bot, dead.top)
  
} else{
  
  flip <- 1
  top  <- min(top, dead.bot)
}


for(i in 1:(top - bot - 1)){
  
  # We're using our flip variable here. Recall that the fip variable reflects
  # whether we're in a heating or cooling group. The fact that it can be 1 or
  # -1 allows us to use the same inequality to define our break.1 variable in
  # heating or cooling groups.
  
  load.group.6 <- 
    mutate(load.group.6, 
           break.1 = ifelse(flip * boston.temp < flip * (top - i), 1, 0))
  
  
  
  regress.break.6.a <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.6, break.1 == 1))
  
  
  regress.break.6.b <-
    lm(beco.res ~ boston.temp + weekday + new.year + mlk + pres + patriot + 
         memorial + indep + labor + columbus + veterans + thanks +
         thanks.fri + christmas,
       data = filter(load.group.6, break.1 == 0))
  
  # Now we'll record everything interestingn from these two regressions.
  
  test.break.6[i, 1] <- sum(regress.break.6.a$residuals ^ 2) + 
    sum(regress.break.6.b$residuals ^ 2)
  
  test.break.6[i, 2] <- top - i
  
  test.break.6[i, 3] <- abs(regress.break.6.a$coefficients[2]) > 
    abs(regress.break.6.b$coefficients[2])
  
  test.break.6[i, 4] <- regress.break.6.a$coefficients[2]
  test.break.6[i, 5] <- regress.break.6.b$coefficients[2]
  
  if(nrow(summary(regress.break.6.a)$coefficients) > 1){
    test.break.6[i, 6] <- summary(regress.break.6.a)$coefficients[2, 4]
  } else{
    test.break.6[i, 6] <- NA
  }
  
  if(nrow(summary(regress.break.6.b)$coefficients) > 1){
    test.break.6[i, 7] <- summary(regress.break.6.b)$coefficients[2, 4]
  } else{
    test.break.6[i, 7] <- NA
  }
  
  # This if statement has a lot of conditions. We're producing a variable that
  # tells us whether we should consider a given regression. We will consider
  # anything unless the low breakpoint coefficient is significant and has a
  # different sign than the high-breakpoint coefficient. And we'll only apply
  # that test when both regressions have non-NA coefficients on weather.
  
  if(!is.na(test.break.6[i, 6]) &
       !is.na(test.break.6[i, 7]) &
       test.break.6[i, 7] < 0.25 & 
       sign(test.break.6[i, 4]) != sign(test.break.6[i, 5])){
    
    test.break.6[i, 8] <- 0
    
  } else{
    
    test.break.6[i, 8] <- 1
    
  }
  
  
  
}

test.winner.6 <- filter(data.frame(test.break.6), 
                        ordered == 1, signif..a < 0.1, b.signif..match == 1) %>%
  summarise(min(SSR))


# Occasionally we're going to have multiple winners, but that's usually going
# to be the result of not having data at that particular breakpoint. In that
# situation, the default should be to take the more central of the two.

winner.location.6 <- which(test.break.6[, 1] == as.numeric(test.winner.6))

if(length(winner.location.6) > 1){
  
  midpoint <- nrow(test.break.6) / 2 
  
  min.level <- min(abs(winner.location.6 - midpoint)) 
  
  min.point <- which(abs(winner.location.6 - midpoint) == min.level)
  
  winner.location.6 <- winner.location.6[min.point]
  
}

# Here again we're being really tricky with the flip variable. If flip is 1, 
# then it's a heating month and we want break.1 to reflect temperatures COLDER
# than our breakpoint. In cooling months, flip will be -1, so negative
# temperature is less than negative breakpoint, which is another way of saying
# temperature is ABOVE the breakpoint. The end result is that break.1 always
# reflects more extreme temperatures, and we can use the same ifelse statement
# in heating and cooling months.

break.6.temp <- test.break.6[winner.location.6, 2]

if(test.winner.6 < test.base){
  
  load.group.6 <- 
    mutate(load.group.6, 
           break.1 = ifelse(flip * boston.temp < 
                              flip * break.6.temp, 1, 0))
  
  
} else{
  
  load.group.6$break.1 <- 1
  
  
}



results.matrix[6, ] <- test.break.6[winner.location.6, ]


if(flip == -1){
  
  load.group.6 <- 
    mutate(load.group.6, 
           w.0 = ifelse(break.1 == 0, boston.temp - dead.top, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, boston.temp - break.6.temp, 0))
  
  
} else {
  
  load.group.6 <- 
    mutate(load.group.6, 
           w.0 = ifelse(break.1 == 0, dead.bot - boston.temp, 0)) %>%
    mutate(w.1 = ifelse(break.1 == 1, break.6.temp - boston.temp, 0))
  
}


# Now we'll re-run and store our winning regressions.

regress.final.6.a <-
  lm(beco.res ~ w.1 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.6, break.1 == 1))


regress.final.6.b <-
  lm(beco.res ~ w.0 + weekday + new.year + mlk + pres + patriot + 
       memorial + indep + labor + columbus + veterans + thanks +
       thanks.fri + christmas,
     data = filter(load.group.6, break.1 == 0))


results.matrix.2.beco.res[6, 1] <- regress.final.6.a$coefficients[2]
results.matrix.2.beco.res[6, 2] <- regress.final.6.b$coefficients[2]

results.matrix.2.beco.res[6, 3] <- summary(regress.final.6.a)$coefficients[2, 4]
results.matrix.2.beco.res[6, 4] <- summary(regress.final.6.b)$coefficients[2, 4]


if(flip == 1){
  
  results.matrix.2.beco.res[6, 5] <- "Heating"
  results.matrix.2.beco.res[6, 8] <- dead.bot
  results.matrix.2.beco.res[6, 9] <- results.matrix[6, 2]
  
} else {
  
  results.matrix.2.beco.res[6, 5] <- "Cooling"
  results.matrix.2.beco.res[6, 8] <- dead.top
  results.matrix.2.beco.res[6, 9] <- results.matrix[6, 2]
  
}


results.matrix.2.beco.res[6, 6] <- min(load.group.6$boston.temp)
results.matrix.2.beco.res[6, 7] <- max(load.group.6$boston.temp)







colnames(results.matrix)   <- colnames(test.break.1)


colnames(results.matrix.2.beco.res) <- c("High Coef.", "Low Coef.", 
                                         "High Signif.", "Low Signif.", 
                                         "Heat / Cool", 
                                         "Low Temp", "High Temp",
                                         "Low Break", "High Break")





# The output that you want is the results.matrix.2.beco.res



load.output <- rbind(load.group.1, load.group.2, load.group.3,
                     load.group.4, load.group.5, load.group.6)

# A quick look at all the groups, colored by high- and low-level 
# responsiveness.

ggplot(load.output, aes(x = boston.temp, y = beco.res, 
                        color = factor(break.1))) +
  geom_point() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  guides(color = FALSE) + 
  labs(y = "Residential", x = "Temperature") +
  facet_wrap(~ cluster, nrow = 3)



# This graph is just a quick example for group 1.


ggplot(load.group.1, aes(x = boston.temp, y = beco.res, 
                         color = factor(break.1))) +
  geom_point(size = 5) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  guides(color = FALSE) + 
  labs(y = "Residential", x = "Temperature", title = "Group 1") + 
  geom_smooth(method = lm, se = FALSE, size = 2, formula = y ~ x,
              aes(x = boston.temp, y = beco.res))



