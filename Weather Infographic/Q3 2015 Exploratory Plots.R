



weather <-
  weather %>%
  mutate(weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0))


weekend.density.plot <-
  weather %>%
  filter(year == 2015, quarter == 3) %>%
  ggplot() +
  geom_density(aes(x = max, fill = factor(weekend)), alpha = 0.5) +
  facet_grid(month ~ station)


cdd.ratio.data <-
  weather %>%
  mutate(weekend.cdds = ifelse(weekend == 1, cdd.65, 0)) %>%
  group_by(station, year, month) %>%
  summarize(total.cdd = sum(cdd.65), weekend.cdd = sum(weekend.cdds)) %>%
  mutate(weekend.cdd.percent = 
           ifelse(total.cdd == 0, 0, weekend.cdd / total.cdd))


cdd.ratio.plot <-
  cdd.ratio.data %>%
  filter(month == 7 | month == 8 | month == 9, year >= 1984) %>%
  mutate(current = ifelse(year == 2015, 1, 0)) %>%
  ggplot(aes(x = total.cdd, y = weekend.cdd.percent,
             color = factor(current))) +
  facet_grid(station ~ month, scales = "free") + 
  geom_point(size = 5.5, alpha = 0.75) +
  scale_color_manual(values = c("lightblue", "red")) +
  guides(color = FALSE) +
  labs(y = "Percent of CDD's on Weekend", x = "Total CDD's",
       title = "Percent of Total CDD's on Weekends") +
  geom_hline(y = 2/7, linetype = "dashed") +
  theme_minimal()


  
  
hottest.days <-
  weather %>%
  filter(year >= 1984, quarter == 3) %>%
  group_by(station, year, quarter) %>%
  summarize(max = max(temp), mean = mean(temp)) %>%
  ungroup() %>%
  mutate(color = factor(ifelse(year == 2015, "red", "grey")))
  

all.hottest.days <-
  weather %>%
  filter(quarter == 3) %>%
  group_by(station, year) %>%
  summarize(max = max(temp), mean = mean(temp)) %>%
  group_by(station) %>%
  mutate(max.percentile = rank(max)/length(max)) %>%
  mutate(mean.percentile = rank(mean)/length(mean)) %>%
  ungroup() %>%
  arrange(desc(year))


hottest.days.plot <-
  hottest.days %>%
  ggplot(aes(x = max)) +
  geom_density(color = "lightgreen", alpha = 0.8, size = 2) +
  geom_bar(aes(y = ..density..), stat = "bin", binwidth = 1,
           alpha = 0.45) +
  facet_grid(. ~ station, scales = "free") +
  scale_fill_identity() +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  labs(x = "Avg. Temperature of Hottest Day in Q3", y = NULL,
         title = "Q3 2015 Hottest Day vs. Historical Hottest Day")



hottest.versus.mean.plot <-
  hottest.days %>%
  ggplot(aes(x = mean, y = max)) +
  geom_point(aes(color = color), size = 7, alpha = .65) +
  geom_smooth(method = "lm", size = 2, alpha = .7, 
              color = "lightgreen", fill = NA) +
  facet_grid(.~ station, scales = "free") +
  scale_color_identity() +
  theme_minimal() +
  coord_cartesian(ylim = c(75, 91)) +
  scale_y_continuous(breaks = seq(75, 95, 5)) +
  theme(panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = "Max Temp", x = "Mean Temp",
       title = "Q3 Temperatures - Mean vs. Max")
  
  
