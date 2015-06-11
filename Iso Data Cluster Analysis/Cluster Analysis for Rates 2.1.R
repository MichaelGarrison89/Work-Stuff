

set.seed(10) 

# If we wanted to be precise, we could re-iterate the kmeans clustering to get
# the same answer every time. For demonstration purposes, we'll just guarantee
# that we always get the same answer by setting the random seed.

load <- read.csv("2012-14 RT ISO Load.csv", header = TRUE) %>%
          filter(DEMAND != 0)


load.clusters <- filter(load, Weekday == 1) %>%
                 select(DEMAND, Hour) %>%
                 mutate(hour.dist = abs(12.5 - Hour)) %>%  # distance from 12:30 P.M.
                 mutate(z.demand = (DEMAND - mean(DEMAND)) / sd(DEMAND)) %>%
                 mutate(z.hour = (hour.dist - mean(hour.dist)) / sd(hour.dist))


cluster.4 <- kmeans(select(load.clusters, z.demand, z.hour), centers = 4)
cluster.5 <- kmeans(select(load.clusters, z.demand, z.hour), centers = 5)
cluster.6 <- kmeans(select(load.clusters, z.demand, z.hour), centers = 6)
cluster.7 <- kmeans(select(load.clusters, z.demand, z.hour), centers = 7)


load.clusters <- mutate(load.clusters, cluster.4 = cluster.4$cluster) %>%
                 mutate(cluster.5 = cluster.5$cluster) %>%
                 mutate(cluster.6 = cluster.6$cluster) %>%
                 mutate(cluster.7 = cluster.7$cluster)


# We want to display the total number in the top group, so we'll make those
# variables.

cluster.4.top.group <- which(cluster.4$centers == 
                               max(cluster.4$centers[, "z.demand"]))
cluster.5.top.group <- which(cluster.5$centers == 
                               max(cluster.5$centers[, "z.demand"]))
cluster.6.top.group <- which(cluster.6$centers == 
                               max(cluster.6$centers[, "z.demand"]))
cluster.7.top.group <- which(cluster.7$centers == 
                               max(cluster.7$centers[, "z.demand"]))


cluster.4.top.count <- sum(load.clusters$cluster.4 == cluster.4.top.group)
cluster.5.top.count <- sum(load.clusters$cluster.5 == cluster.5.top.group)
cluster.6.top.count <- sum(load.clusters$cluster.6 == cluster.6.top.group)
cluster.7.top.count <- sum(load.clusters$cluster.7 == cluster.7.top.group)


# And now the plots

weekday.cluster.plot.4 <-
  
  ggplot(load.clusters, aes(x = Hour, y = z.demand, 
                            color = factor(cluster.4))) +
  geom_jitter(size = 1) +
  scale_shape_manual(values = c(16, 17)) +
  theme(panel.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = seq(1, 24, 1)) +
  guides(size = FALSE, color = FALSE) +
  geom_text(x = 3, y = 4, aes(label = cluster.4.top.count), 
            color = "black", size = 8) +
  labs(title = "4 Clusters", y = "Demand", x = "Hour")



weekday.cluster.plot.5 <-
  
  ggplot(load.clusters, aes(x = Hour, y = z.demand, 
                            color = factor(cluster.5))) +
  geom_jitter(size = 1) +
  scale_shape_manual(values = c(16, 17)) +
  theme(panel.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = seq(1, 24, 1)) +
  guides(size = FALSE, color = FALSE) +
  geom_text(x = 3, y = 4, aes(label = cluster.5.top.count), 
            color = "black", size = 8) +
  labs(title = "5 Clusters", y = "Demand", x = "Hour")


weekday.cluster.plot.6 <-
  
  ggplot(load.clusters, aes(x = Hour, y = z.demand, 
                            color = factor(cluster.6))) +
  geom_jitter(size = 1) +
  scale_shape_manual(values = c(16, 17)) +
  theme(panel.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = seq(1, 24, 1)) +
  guides(size = FALSE, color = FALSE) +
  geom_text(x = 3, y = 4, aes(label = cluster.6.top.count), 
            color = "black", size = 8) +
  labs(title = "6 Clusters", y = "Demand", x = "Hour")


weekday.cluster.plot.7 <-
  
  ggplot(load.clusters, aes(x = Hour, y = z.demand, 
                            color = factor(cluster.7))) +
  geom_jitter(size = 1) +
  scale_shape_manual(values = c(16, 17)) +
  theme(panel.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = seq(1, 24, 1)) +
  guides(size = FALSE, color = FALSE) +
  geom_text(x = 3, y = 4, aes(label = cluster.7.top.count), 
            color = "black", size = 8) +
  labs(title = "7 Clusters", y = "Demand", x = "Hour")


grid.arrange(weekday.cluster.plot.4, weekday.cluster.plot.5,
             weekday.cluster.plot.6, weekday.cluster.plot.7,
             nrow = 2)
