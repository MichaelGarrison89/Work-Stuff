# Here are the input variables. The following three variables define the last
# day of history for which you want analysis--often you will have more history
# than you actually want to analyze.

est.month <- 6
est.day   <- 14
est.year  <- 2015
est.date  <- as.Date(paste(est.month, est.day, est.year, sep = "/"), 
                     "%m/%d/%Y")


# This program produces demand revenue estimates using what has come to be
# called DEWM (Demand Exponential Weighting Method). The program creates what
# we call DEWM peaks, which are weighted averages of the hourly load data for
# a given month. The load data each hour is raised to some exponent (which
# varies by OpCo and is optimized in a separate program), and the exponentiated
# hourly values are divided by the monthly totals, creating the weights that
# create the weighted-average DEWM peak.
#
# Those DEWM peaks tend to be a more stable predictor of monthly revenues than
# the monthly peak, and they are on a comparable scale.
#
#
# Author notes:
#   Written by: Michael Garrison
# Version Date: 6.17.15


# Here are the non-standard packages we need.

library("gdata", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("ggplot2", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("stringr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("dplyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("tidyr", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("zoo", lib.loc="C:/Program Files/R/R-3.1.3/library")


# Import the relevant data.


NURev <- read.csv("NU Demand Revenues.csv", 
                  header=TRUE, stringsAsFactors=FALSE)


nstar <- read.csv("NSTAR-HourlyLoads.csv", 
                  header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(date = as.Date(Date,"%m/%d/%Y")) %>%
  mutate(day = as.numeric(getDay(date))) %>%
  mutate(month = as.numeric(getMonth(date))) %>%
  mutate(year = as.numeric(getYear(date)))

beco <- select(nstar, date, day, month, year, contains("Beco")) %>%
  gather("variable", "load", contains("Beco")) %>%
  mutate(hour = as.numeric(str_sub(variable, 9L)))



NSTARRev <- read.csv("NSTAR Demand Revenues.csv", 
                     header=TRUE, stringsAsFactors=FALSE) %>%
  filter(Year >= 2011)

clp <- read.csv("CL&P Daily Peaks.csv", 
                header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(date = as.Date(Date,"%m/%d/%Y")) %>%
  mutate(day = as.numeric(getDay(date))) %>%
  mutate(month = as.numeric(getMonth(date))) %>%
  mutate(year = as.numeric(getYear(date))) %>%
  rename(clp = CL.P_Daily)

# This file was created by a separate program that optimizes the DEWM peaks.

exponent <- read.csv("Exponent.csv", header=TRUE, stringsAsFactors=FALSE)

# Here we'll make several matrices that show our hourly averages. We'll use
# these to extend our data through the end of the current month.


beco.means <-  filter(beco, month == est.month, 
                      day > est.day, year < est.year) %>%
  group_by(year) %>%
  mutate(rank = row_number(load))%>%
  group_by(rank, add = FALSE) %>%
  summarise(load = mean(load))


clp.means <-  filter(clp, month == est.month, 
                     day > est.day, year < est.year) %>%
  group_by(year) %>%
  mutate(rank = row_number(clp)) %>%
  group_by(rank, add = FALSE) %>%
  summarise(load = mean(clp))


# Now we will calculate our historical DEWM peaks, which are monthly values. 
# We'll also join with revenue data and calculate the monthly ratios between 
# DEWM peaks and revenues.

beco.history <- filter(beco, date <= est.date - est.day) %>%
  mutate(exp = load ^ exponent$Beco) %>%
  group_by(year, month) %>%
  mutate(weight = exp / sum(exp)) %>%
  mutate(dewm = weight * load) %>%
  summarise(dewm = sum(dewm), check = sum(weight)) %>%
  left_join(select(NSTARRev, -Comm, -Camb), 
            by = c("month" = "Month", "year" = "Year")) %>%
  mutate(alpha = BECo / dewm)


# Because of the ratcheted rates, CL&P behaves a little differently.

clp.history <- filter(clp, date <= est.date - est.day) %>%
  mutate(exp = clp ^ exponent$CLP) %>%
  mutate(load.exp = clp * exp) %>%
  mutate(c.exp = rollsum(exp, 365, 
                         fill = NA, align = "right")) %>%
  mutate(c.load.exp = rollsum(load.exp, 365,
                              fill = NA, align = "right")) %>%
  mutate(dewm = c.load.exp / c.exp) %>%
  mutate(last.day.of.month = 
           ifelse(lead(month, 1) != month, 1, 0)) %>%
  filter(last.day.of.month == 1 | is.na(last.day.of.month), year > 2009) %>%
  select(year, month, dewm) %>%
  left_join(select(NURev, -WMECORev, -PSNHRev),
            by = c("month" = "Month", "year" = "Year")) %>%
  mutate(alpha = CLPRev / dewm)


# And now we'll combine the data we have for the month with the historical 
# averages and convert the result into a DEWM peak for the current month.


beco.current.dewm <- filter(beco, month == est.month, year == est.year) %>%
  select(load) %>%
  rbind(select(beco.means, load)) %>%
  mutate(exp = load ^ exponent$Beco) %>%
  mutate(weight = exp / sum(exp)) %>%
  mutate(dewm = weight * load) %>%
  summarise(dewm = sum(dewm))


# Again, CLP takes a different form because of the ratcheted rates.

clp.dewm <- select(clp, clp) %>%
  rename(load = clp) %>%
  rbind(select(clp.means, load)) %>%
  mutate(exp = load ^ exponent$CLP) %>%
  mutate(load.exp = load * exp) %>%
  mutate(c.exp = rollsum(exp, 365, 
                         fill = NA, align = "right")) %>%
  mutate(c.load.exp = rollsum(load.exp, 365,
                              fill = NA, align = "right")) %>%
  mutate(dewm = c.load.exp / c.exp) %>%
  summarise(dewm = last(dewm))


# Finally we'll calculate revenue numbers for the current month for each OpCo,
# using the previous two years' alphas to map our relationship between DEWM
# peaks and revenues.


beco.revenue.estimate <- filter(beco.history, month == est.month,
                                year >= est.year - 2, year < est.year) %>%
  group_by(month) %>%
  summarise(alpha = mean(alpha)) %>%
  mutate(beco = alpha * beco.current.dewm) %>%
  select(-alpha)


clp.revenue.estimate <- filter(clp.history, month == est.month,
                               year >= est.year - 2, year < est.year) %>%
  group_by(month) %>%
  summarise(alpha = mean(alpha)) %>%
  mutate(clp = alpha * clp.dewm) %>%
  select(-alpha)

# And now we prepare our table for export.

export.table <- data.frame(month = est.month,
                           year  = est.year) %>%
  mutate(beco = as.numeric(beco.revenue.estimate$beco)) %>%
  mutate(clp = as.numeric(clp.revenue.estimate$clp))