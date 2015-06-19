# The purpose of this program is to set up a cool monthly weather data slide.


library("grid", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("gdata", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("ggplot2", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("scales", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("tidyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("dplyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("zoo", lib.loc="C:/Program Files/R/R-3.1.3/library")

# I pulled the following function from Hadley Whickam's GitHub. I'll use it at
# the end to create the infographic.


vplayout <- function(x, y){
  viewport(layout.pos.row = x, 
           layout.pos.col = y)
}

# We import the weather data, then clean it up a bit before adding temperature,
# converting to Fahrenheit from Celsius, and adding cdd.65's and hdd.65's.


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


# We want a data frame from the last 12 months for actual and normal
# temperatures and cooling / heating degree days.

last.twelve <- filter(weather, date >= date[last.day] - 364) %>%
               select(date, year, month, day, min, max, PRCP, 
                      SNOW, temp, cdd.65, hdd.65) %>%
               rename(precip = PRCP, snow = SNOW) %>%
               mutate(precip.trail = 
                        rollmean(precip, 3, fill = NA, align = "right"))


normal.temps <- filter(weather, year >= 1984, year <= 2013) %>%
                group_by(month, day) %>%
                summarise(max.normal = mean(max), 
                          min.normal = mean(min),
                          norm = mean(temp))


normal.degree.days <- filter(weather, year >= 1984, year <= 2013) %>%
                      group_by(month, day) %>%
                      summarise(hdd.65 = mean(hdd.65), cdd.65 = mean(cdd.65)) %>%
                      gather("type", "normal", hdd.65, cdd.65)



# Combining the last.twelve and normal.temps data frames will make it easy to
# show actual temperature vs. normal. Same idea for the degree days.

last.twelve <- left_join(last.twelve, normal.temps, 
                         by = c("month" = "month", "day" = "day"))



last.twelve.melt <- 
          select(last.twelve, date, temp, min.normal, max.normal) %>%
          gather("variable", "value", temp, min.normal, max.normal) %>%
          mutate(t.min = ifelse(variable == "temp", last.twelve$min, 0)) %>%
          mutate(t.max = ifelse(variable == "temp", last.twelve$max, 0)) 


# The following left_join throws a warning about factor levels, but I think
# it's mistaken.


degree.days <- select(last.twelve, date, year, month, day, hdd.65, cdd.65) %>%
               gather("type", "actual", -(date:day)) %>%
               left_join(normal.degree.days, 
                        by = c("day" = "day", 
                               "month" = "month", 
                               "type" = "type"))


# This data frame will feed our density plot.

this.month <- filter(weather, year >= 1984 & year != 2014) %>%
              filter(month == current.month) %>%
              select(month, year, temp) %>%
              mutate(time = factor(ifelse(year == current.year, 
                                          "2015", "Normal")))


# We'll pull out some summary stats.

min.max.matrix <- select(weather, year, month, day, temp) %>%
                  filter(year >= 1984, year <=2013) %>%
                  filter(month == 5) %>%
                  group_by(year) %>%
                  summarise(max = max(temp), min = min(temp))

mean(min.max.matrix$max)
mean(min.max.matrix$min)

group_by(this.month, time) %>%
  summarise(mean = mean(temp), median = median(temp))


filter(degree.days, month == current.month) %>%
  group_by(type) %>%
  summarise(act. = sum(actual), norm. = sum(normal))



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


temps.plot.no.precip <-
  
  ggplot(last.twelve.melt, 
         aes(x = date, y = value, color = variable)) + 
  geom_ribbon(aes(ymin = t.min, ymax = t.max), fill = "lightgreen") +
  geom_line(lwd = 0.4) + 
  labs(y = "Temperature", x = NULL,
       title = "Daily Temperatures") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        plot.title = element_text(size = 10)) +
  scale_color_manual(values = 
                       c("lightgreen", "black", "black", "lightgreen")) +
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  scale_y_continuous(breaks = c(seq(-5, 95, 20), 0)) +
  coord_cartesian(xlim = as.Date(c("2014-06-01", "2015-05-31")),
                  ylim = c(-10, 110)) +
  scale_x_date(breaks = "1 month", labels = date_format("%B"))



degree.days.plot <-
  
  ggplot(degree.days, aes(x = date)) +
  geom_area(aes(y = normal, fill = type), alpha = 0.8) +
  geom_line(aes(y = actual, color = type), size = 1, alpha = 0.5) +
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
print(degree.days.plot, vp = vplayout((1:7), (1:2)))
print(temps.plot, vp = vplayout((8:14), (1:2)))
print(density.plot, vp = vplayout((15:21), 1))
