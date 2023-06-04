library(tidyverse)
library(networkD3)
library(patchwork)

draft2 <- read.csv("./data/draft2.csv",
                   header = TRUE, stringsAsFactors = FALSE)
glimpse(draft2)
dim(draft2) # row col
# we need 19th col in this chapter
# Pk -> Pick , player picked in first-round selection
# Pk has a minimum of 1 and a maximum of 30.
# creating Pk2
draft2 <- mutate(draft2, Pk2 = ifelse(Pk %in% 1:5, "1-5",
                               ifelse(Pk %in% 6:10, "6-10",
                               ifelse(Pk %in% 11:15, "11-15",
                               ifelse(Pk %in% 16:20, "16-20",
                               ifelse(Pk %in% 21:25, "21-25",
                               ifelse(Pk %in% 26:30, "26-30", "NA")))))))
draft2$Pk2 <- as.factor(draft2$Pk2)
glimpse(draft2)
# mean and median
sumG <- sum(draft2$G)
tibble1 <- draft2 %>%
  group_by(Pk2) %>%
  summarize(mean = mean(G), median = median(G), pct = sum(G)/sumG)
print(tibble1)
#visualization
g1 <- ggplot(tibble1, aes(x = Pk2, y= mean)) +
  geom_bar(stat = "identity", width = .8, fill = "coral", color = "coral4") +
  labs(title = "Average Career Games Played",
       subtitle = "First Round Selection Between 2000 and 2009 NBA Drafts",
       x = "Selection Range", y = "Average Career Games Played",
       caption = "regular season games only") +
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(mean), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 800) +
  theme(plot.title = element_text(face = "bold"))
g2 <- ggplot(tibble1, aes(x = Pk2, y= median)) +
  geom_bar(stat = "identity", width = .8, fill = "coral3", color = "coral4") +
  labs(title = "Median Career Games Played",
       subtitle = "First Round Selection Between 2000 and 2009 NBA Drafts",
       x = "Selection Range", y = "Median Career Games Played",
       caption = "regular season games only") +
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(median), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 800) +
  theme(plot.title = element_text(face = "bold"))
g1 + g2 + plot_layout(ncol = 1)

# by minute played
sumMP <- sum(draft2$MP)
tibble2 <- draft2 %>%
  group_by(Pk2) %>%
  summarise(meanMP = mean(MP), medianMP = median(MP), pctMP = sum(MP)/sum(sumMP))
g3 <- ggplot(tibble2, aes(x = Pk2, y= meanMP)) +
  geom_bar(stat = "identity", width = .8, fill = "deepskyblue", color = "deepskyblue4") +
  labs(title = "Average Minutes Played per Game",
       subtitle = "First Round Selection Between 2000 and 2009 NBA Drafts",
       x = "Selection Range", y = "Average Minutes Played per Game",
       caption = "regular season games only") +
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(meanMP), vjust = -0.3)) +
  geom_label(aes(label = trunc(pctMP*100), vjust = 1.2)) +
  ylim(0, 35) + # limitting Y height (number)
  theme(plot.title = element_text(face = "bold"))
g4 <- ggplot(tibble2, aes(x = Pk2, y= medianMP)) +
  geom_bar(stat = "identity", width = .8, fill = "deepskyblue3", color = "deepskyblue4") +
  labs(title = "Median Minutes Played per Game",
       subtitle = "First Round Selection Between 2000 and 2009 NBA Drafts",
       x = "Selection Range", y = "Median Minutes Played per Game",
       caption = "regular season games only") +
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(medianMP), vjust = -0.3)) +
  geom_label(aes(label = trunc(pctMP*100), vjust = 1.2)) +
  ylim(0, 35) + # limitting Y height (number)
  theme(plot.title = element_text(face = "bold"))
g3 + g4 + plot_layout(ncol = 1)
