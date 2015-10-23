

consecutive.nineties <-
  weather %>%
  select(-cdd.65, -hdd.65, -min) %>%
  group_by(station) %>%
  mutate(ninety.plus = ifelse(max >= 90, 1, 0)) %>%
  mutate(lag.ninety = lag(ninety.plus)) %>%
  mutate(back.to.back = ifelse(lag.ninety + ninety.plus == 2, 1, 0)) %>%
  filter(year >= 1984, quarter == 3) %>%
  group_by(station, year) %>%
  summarize(nineties = sum(ninety.plus), consecutive = sum(back.to.back))



consecutive.nineties.avgs <-
  consecutive.nineties %>%
  filter(year <= 2013) %>%
  summarize(avg.nineties = mean(nineties), 
            avg.consecutive = mean(consecutive)) %>%
  mutate(year = "Normal") %>%
  select(station,  year, avg.nineties, avg.consecutive) %>%
  rename(nineties = avg.nineties, consecutive = avg.consecutive)



nineties.graph.feed <-
  consecutive.nineties %>%
  mutate(year = as.character(year)) %>%
  bind_rows(consecutive.nineties.avgs)


set.seed(15)

#nineties.graph <-
  consecutive.nineties %>%
  mutate(year = as.character(year)) %>%
  bind_rows(consecutive.nineties.avgs) %>% 
  filter(station != "PROVIDENCE") %>%
  gather("variable", "value", nineties, consecutive) %>%
  mutate(fill = factor(ifelse(year == "2015", "red", 
                              ifelse(year == "Normal", "blue", "grey")))) %>%
  mutate(variable = ifelse(variable == "nineties", "Total Ninety Degree Days",
                           "Consecutive Ninety Degree Days")) %>%
  ggplot(aes(x = value, y = station, color = fill)) +
  geom_jitter(size = 4, alpha = 0.65, 
              position = position_jitter(height = 0.06, width = 0.01)) +
  facet_grid(.~ variable, scales = "free") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom") +
  scale_color_identity(guide = "legend",
                       labels = c("Avg.", "History", "2015")) +
  labs(x = NULL, y = NULL, color = NULL, 
       title = "Q3 History - 90 Degree Days")


nineties.graph <-
  consecutive.nineties %>%
  filter(year >= 2014) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(consecutive.nineties.avgs) %>% 
  filter(station != "PROVIDENCE") %>%
  gather("variable", "value", nineties, consecutive) %>%
  mutate(fill = factor(ifelse(year == "2015", "red", 
                              ifelse(year == "Normal", "lightgreen", 
                                     "dodgerblue")))) %>%
  mutate(variable = ifelse(variable == "nineties", "Total 90 Degree Days",
                           "Consecutive 90 Degree Days")) %>%
  ggplot(aes(x = station, y = value, fill = fill)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  facet_grid(.~ variable, scales = "free") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.text.x = element_text(size = 14),
        legend.position = c(0.1, .75)) +
  scale_y_continuous(breaks = seq(0, 12)) +
  scale_fill_identity(guide = "legend",
                       labels = c("2014", "Normal", "2015")) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = NULL)


# This function finds all the runs of consecutive numbers and puts them in a matrix. Then
# it checks for any runs of consecutive "value"; if there aren't any such runs, it returns
# zero. If there are runs of "value", it returns the longest run. I can use this function to quickly
# check for the longest runs of 90+ degree days.


FindMaxRun <- function(vector, value){
  
  mat       <- cbind(rle(vector)$lengths, rle(vector)$values)
  
  positives <- as.matrix(mat[mat[, 2] == value, ])
  
  if(length(positives) == 0){
    
    return(0)
    
  } else{
    
    return(max(positives[, 1]))
    
  } 
  
}

nineties.streaks <-
  weather %>%
  select(-cdd.65, -hdd.65, -min) %>%
  group_by(station) %>%
  mutate(ninety.plus = ifelse(max >= 90, 1, 0)) %>%
  filter(year >= 1984, quarter == 3) %>%
  group_by(station, year) %>%
  summarize(max.ninety.run = FindMaxRun(ninety.plus, 1)) %>%
  mutate(time.frame = ifelse(year == 2015, "2015",
                             ifelse(year == 2014, "2014", "normal"))) %>%
  group_by(station, time.frame) %>%
  summarize(avg.max.streak = mean(max.ninety.run))
