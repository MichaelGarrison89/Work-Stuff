# The purpose of this program is to set up a cool monthly weather data slide.

setwd("K:/ELF/Deptdata/Yankee Gas/2015/Monthly Variance/New S&R Graphs/Weather Slide")

library("grid", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("gdata", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("ggplot2", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("gtools", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("scales", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("tidyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("dplyr", lib.loc="C:/Program Files/R/R-3.1.3/library")


CalculateLagMean <- function(x, lag){
  # Calculates a lagged moving average of the vector x.
  #
  # Args:
  # x  : vector from which to calculate lagged mean
  # lag: number of periods in the lagged mean
  #
  # Returns:
  # A vector, means.vector, in which element i of means.vector shows the mean
  # of elements i - lag - 1 through i - 1 of the original x vector. Elements 1
  # through <lag> of means.vector are set to zero. For example, if the x vector
  # were c(1, 2, 3, 4), and lag were 2, this function would return a vector
  # equal to c(0, 0, 1.5, 2.5).
  
  
  means.vector        <- vector(length=length(x))
  means.vector[1:lag] <- 0
  
  for(i in 1:(length(x) - lag)){
    means.vector[lag + i] <- mean(x[i:(lag + i - 1)])
    
    
    
  }
  
  
  return(means.vector)
  
}

# I pulled this function from Hadley Whickam's GitHub. I'll use it at the end
# to create the infographic.


vplayout <- function(x, y){
  viewport(layout.pos.row = x, 
           layout.pos.col = y)
}

# We import the weather data, then clean it up a bit before adding temperature,
# converting to fahrenheit, and adding cdd.65's and hdd.65's.


weather <- read.csv("Logan Since 1936.csv") %>%         
        mutate(date = as.Date(date, "%m/%d/%Y")) %>%    
        mutate(month = as.numeric(getMonth(date))) %>%  
        mutate(day = as.numeric(getDay(date))) %>%
        mutate(year = as.numeric(getYear(date))) %>%
        mutate(min = (1.8 * cmin / 10) + 32) %>%        
        mutate(max = (1.8 * cmax / 10) + 32) %>%
        mutate(temp = (max + min) / 2) %>%              
        mutate(cdd.65 = ifelse(temp > 65, temp - 65, 0)) %>% 
        mutate(hdd.65 = ifelse(temp < 65, 65 - temp, 0))


current.month      <- 5
current.year       <- 2015
last.day           <- length(weather$date)
thirty.year.normal <- which(weather$year >= 1984 & weather$year <= 2013)
last.year          <- (last.day - 364):last.day


# We want a data frame from the last 12 months.

last.twelve <- data.frame(date = weather$date[last.year],
                          min  = weather$min[last.year],
                          max  = weather$max[last.year],
                          precip = weather$PRCP[last.year],
                          snow = weather$SNOW[last.year])

last.twelve <- mutate(last.twelve, precip.trail = 
                      CalculateLagMean(last.twelve$precip, 3))

normal.temps <-  aggregate(weather$min[thirty.year.normal], 
                          list(day = weather$day[thirty.year.normal],
                             month = weather$month[thirty.year.normal]), mean)

colnames(normal.temps) <- c("day", "month", "min")

normal.temps$max <- aggregate(weather$max[thirty.year.normal], 
                              list(day = weather$day[thirty.year.normal],
                                 month = weather$month[thirty.year.normal]), 
                                 mean)[, 3]


normal.degree.days <- aggregate(weather$hdd.65[thirty.year.normal], 
                                list(day = weather$day[thirty.year.normal],
                                   month = weather$month[thirty.year.normal]), 
                                   mean)

colnames(normal.degree.days) <- c("day", "month", "hdd.65")


normal.degree.days$cdd.65 <- aggregate(weather$cdd.65[thirty.year.normal], 
                               list(day = weather$day[thirty.year.normal],
                                  month = weather$month[thirty.year.normal]), 
                                       mean)[, 3]


normal.degree.days <-
  normal.degree.days[c(which(normal.degree.days$month > current.month), 
                       which(normal.degree.days$month <=  current.month)), ]


# I want to re-organize the normal.temps frame so that it's easier to merge
# with the last.twelve data frame.

normal.temps <- normal.temps[c(which(normal.temps$month > current.month), 
                               which(normal.temps$month <=  current.month)), ]

last.twelve$temp <- apply(cbind(last.twelve$min, last.twelve$max), 
                          1, FUN = mean)

last.twelve$norm <- apply(cbind(normal.temps$min, normal.temps$max), 
                          1, FUN = mean)[-which(normal.temps$month == 2 &
                                                  normal.temps$day == 29)]


last.twelve$min.normal <- normal.temps$min[-which(normal.temps$month == 2 &
                                                    normal.temps$day == 29)]

last.twelve$max.normal <- normal.temps$max[-which(normal.temps$month == 2 &
                                                    normal.temps$day == 29)]


last.twelve.melt <- 
          select(last.twelve, date, temp, min.normal, max.normal) %>%
          gather("variable", "value", temp, min.normal, max.normal) %>%
          mutate(t.min = ifelse(variable == "temp", last.twelve$min, 0)) %>%
          mutate(t.max = ifelse(variable == "temp", last.twelve$max, 0)) 




this.month <- filter(weather, year >= 1984 & year != 2014) %>%
              filter(month == current.month) %>%
              select(month, year, temp) %>%
              mutate(time = factor(ifelse(year == current.year, 
                                          "2015", "Normal")))


min.max.matrix <- 
  data.frame(temp = 
               weather$temp[thirty.year.normal][which(
                 weather$month[thirty.year.normal] == 5)],
             year = 
               weather$year[thirty.year.normal][which(
                 weather$month[thirty.year.normal] == 5)],
             day = 
               weather$day[thirty.year.normal][which(
                 weather$month[thirty.year.normal] == 5)])

min.max.matrix <- acast(min.max.matrix, day ~ year, value.var = "temp")

mean(apply(min.max.matrix, MARGIN = 2, FUN = min))
mean(apply(min.max.matrix, MARGIN = 2, FUN = max))



degree.days.3 <- filter(normal.degree.days, day != 29 | month != 2) %>%
                 select(hdd.65, cdd.65) %>%
                 rename(hdd = hdd.65, cdd = cdd.65) %>%
                 mutate(date = last.twelve[, 1]) %>%
                 gather("type", "normal", -date) %>%
                 mutate(act.value = c(
                      ifelse(last.twelve$temp < 65, 65 - last.twelve$temp, 0),
                      ifelse(last.twelve$temp > 65, last.twelve$temp - 65, 0)))

# Some summary stats.

group_by(this.month, time) %>%
  summarise(mean = mean(temp), median = median(temp))


filter(degree.days.3, as.numeric(getMonth(date)) == current.month) %>%
  group_by(type) %>%
  summarise(act. = sum(act.value), norm. = sum(normal))

# And now we'll build the plots.


density.plot <-

ggplot(this.month) +
  geom_density(aes(x = temp, fill = time), alpha = 0.5, adjust = 1) +
  labs(y = NULL, x = "Temperatures", 
       fill = "Time Frame", title = "Temperature Distributions") +
  annotate("text", x = 68, y = 0.045, label = "May '15", 
           size = 4, color = "purple") +
  annotate("text", x = 45, y = 0.045, label = "Normal May", 
           size = 4, color = "orange") +
  scale_fill_manual(values = c("purple", "orange")) +
  guides(fill = FALSE) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        plot.title = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


temps.plot <-
  
  ggplot(last.twelve.melt, 
         aes(x = date, y = value, color = variable)) + 
  geom_ribbon(aes(ymin = t.min, ymax = t.max), fill = "lightgreen") +
  geom_point(data = last.twelve, aes(x = date, y = 100, 
                 size = precip.trail, color = "blue", alpha = precip.trail)) +
  geom_line(lwd = 0.4) + 
  labs(y = "Temperature", x = NULL,
       title = "Daily Temperatures and Precipitation") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        plot.title = element_text(size = 10)) +
  scale_color_manual(values = 
                       c("darkgrey", "black", "black", "lightgreen")) +
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  scale_y_continuous(breaks = c(seq(-5, 95, 20), 0)) +
  coord_cartesian(xlim = as.Date(c("2014-06-01", "2015-05-31")),
                  ylim = c(-10, 110)) +
  scale_x_date(breaks = "1 month", labels = date_format("%B"))


degree.days.3.plot <-
  
  ggplot(degree.days.3, aes(x = date)) +
  geom_area(aes(y = normal, fill = type), alpha = 0.8) +
  geom_line(aes(y = act.value, color = type), size = 1, alpha = 0.5) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        plot.title = element_text(size = 10),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  coord_cartesian(xlim = as.Date(c("2014-06-01", "2015-05-31"))) +
  scale_x_date(breaks = "1 month", labels = date_format("%B")) +
  guides(fill = FALSE, alpha = FALSE, color = FALSE) +
  facet_wrap(~ type, scales = "free", nrow = 2) + 
  labs(y = "Degree Days", x = NULL, title = "Degree Days - Actual & Normal")
  
  
grid.newpage()
pushViewport(viewport(layout = grid.layout(21, 2)))
print(degree.days.3.plot, vp = vplayout((1:7), (1:2)))
print(temps.plot, vp = vplayout((8:14), (1:2)))
print(density.plot, vp = vplayout((15:21), 1))