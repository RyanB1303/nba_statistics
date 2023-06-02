library(tidyverse)
library(reshape2)
library(sqldf)
library(patchwork)

draft <- read.csv("data/draft.csv",
                  header=TRUE,
                  stringsAsFactors = FALSE)
dim(draft)

# remove variable (column)
draft <- select(draft, -c(3,4,16:24))
# remove observation
draft <- draft[-c(90, 131),]

#convert datatype to factor
draft$Year <- as.factor(draft$Year)
draft$Tm <- as.factor(draft$Tm)
draft$Pos <- as.factor(draft$Pos)
draft$Born <- as.factor(draft$Born)
draft$From <- as.factor(draft$From)
draft$To <- as.factor(draft$To)

#derived variables, modify Born column to USA if us or WORLD
draft <- mutate(draft, Born2 = ifelse(Born == "us", "USA", "World"))
draft$Born2 <- as.factor(draft$Born2)

# set na to 0
draft[is.na(draft)] <- 0
draft <- mutate(draft, College2 = ifelse(College == 0, 0, 1))
draft$College2 <- factor(draft$College2)


# show levels (i think this refer to unique value by row) for position
levels(draft$Pos)
# if position equals G, Pos2 equals Guard
# if position equals F, Pos2 equals Forward
# if position equals C, Pos2 eqquals Center
# ...
draft <- mutate(draft, Pos2 = ifelse(Pos == "G", "Guard",
                              ifelse(Pos == "F", "Forward",
                              ifelse(Pos == "C", "Center",
                              ifelse(Pos == "F-G", "Swingman",
                              ifelse(Pos == "G-F", "Swingman",
                              ifelse(Pos == "F-C", "Big",
                              ifelse(Pos == "C-F", "Big", "NA"))))))))
# view data after processed
glimpse(draft)
head(draft)
summary(draft)

# standard defiation for continous data
sd(draft$G)
sd(draft$MP)
sd(draft$WS)

# draw plot
p1 <- ggplot(draft, aes(x = WS)) + geom_histogram(fill="royalblue3", color="royalblue3", bins = 8) + 
      labs(title = "Career Win Shares Distribution of NBA First-Round Selections", subtitle = "2000-09 NBA Drafts",
       x = "Career Win Shares", y = "Frequency") + theme(plot.title = element_text(face = "bold"))
print(p1)

# 2nd plot
p2 <- ggplot(draft, aes(x= College2, y= WS )) + 
  geom_boxplot(color="orange4", fill="orange1") + 
  labs(title = "Career Win Shares Distribution of NBA First-Round Selections",
       x = "", y = "Career Win Shares", subtitle="2000-09 NBA Drafts") +
  stat_summary(fun = mean, geom="point", shape = 20, size = 8, color = "white", fill ="white") +
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(~Born2) +
  scale_x_discrete(breaks = c(0,1), labels = c("No College", "College"))
print(p2)

# boxplot per year
p3 <- ggplot(draft, aes(x= Year, y = WS)) +
  geom_boxplot(color="dodgerblue4", fill= "dodgerblue") +
  labs(title = "Year-over-Year Win Shares Distribution of NBA First-Round Selections",
       x = "", y = "Win Shares", subtitle = "2000-09 NBA Drafts") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color="white", fill="white") +
  theme(plot.title = element_text(face = "bold"))
print(p3)  

# heat map (correlation)
# get col to new object
draft_cor <- select(draft, c(6, 12:15))
# get corelation
draft_cor <- cor(draft_cor)
print(draft_cor)
# melt data
draft_cor <- melt(draft_cor, na.rm = TRUE)
head(draft_cor)
tail(draft_cor)
# visualize heat map
p4 <- ggplot(draft_cor, aes(x = Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid = "grey84", limits = c(-1,1)) +
  labs(title = "Correlation Matrix",
       subtitle = "Correlation Coefficient between Win Shares and Other Continous Variables",
       x = "", y = "", fill="Correlation\nCoefficient", caption = "Source: draft data set") +
  theme(plot.title = element_text(face = "bold"),
        legend.title = element_text(color = "brown", size = 10)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black",
  fontface = "bold", size = 5)
print(p4)

# visualizing means and medians
draft %>%
  group_by(Born2) %>%
  summarize(meanWS = mean(WS),
            medianWS = median(WS)) -> eight_tibble
print(eight_tibble)

# by place of birth
p5 <- ggplot(eight_tibble, aes(x = Born2, y = meanWS)) +
  geom_bar(stat = "identity", width = .5, fill = "darkorchid4") +
  geom_text(aes(label = trunc(meanWS), vjust = -0.3)) +
  labs(title = "Average Career Win Shares by Place of Birth",
       subtitle = "2000-09 NBA Drafts",
       x = "Where Born", y = "Average Carrer Win Shares") +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))

p6 <- ggplot(eight_tibble, aes(x = Born2, y = medianWS)) +
  geom_bar(stat = "identity", width = .5, fill = "sienna1") +
  geom_text(aes(label = trunc(medianWS), vjust = -0.3)) +
  labs(title = "Median Career Win Shares by Place of Birth",
       subtitle = "2000-09 NBA Drafts",
       x = "Where Born", y = "Median Carrer Win Shares") +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))
# join 2 plot (p5, p6)
p5 + p6 + plot_layout(ncol = 2)

# by entering college or not
ninth_tibble <- draft%>%
  group_by(College2) %>%
  summarize(meanWS = mean(WS),
            medianWS = median(WS))
print(ninth_tibble)
# visual
p7 <- ggplot(ninth_tibble, aes(x = College2, y = meanWS)) +
  geom_bar(stat = "identity", width = .5, fill = "darkorchid4") +
  geom_text(aes(label = trunc(meanWS), vjust = -0.3)) +
  labs(title = "Average Career Win Shares by College vs. No College",
       subtitle = "2000-09 NBA Drafts",
       x = "College or No College", y = "Average Carrer Win Shares") +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))

p8 <- ggplot(ninth_tibble, aes(x = College2, y = medianWS)) +
  geom_bar(stat = "identity", width = .5, fill = "sienna1") +
  geom_text(aes(label = trunc(medianWS), vjust = -0.3)) +
  labs(title = "Median Career Win Shares by College vs. No College",
       subtitle = "2000-09 NBA Drafts",
       x = "College or No College", y = "Median Carrer Win Shares") +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))
p7 + p8 + plot_layout(ncol = 2)

# group 3 column (position, born and college)
tenth_tibble <- draft%>%
  group_by(Pos2, Born2, College2) %>%
  summarize(mean = mean(WS),
            median = median(WS))
head(tenth_tibble, n = 3)
tail(tenth_tibble, n = 3)
new_labels <- c("0" = "No College", "1" = "College")
p9 <- ggplot(tenth_tibble, aes(x = Pos2, y = mean)) +
  geom_bar(stat = "identity", width = .5, fill = "slateblue4") +
  geom_text(aes(label = trunc(mean), vjust = -0.3)) +
  labs(title = "Mean Win Shares by Place of Birth",
       subtitle = "2000-09 NBA Drafts",
       x = "", y = "Win Shares") +
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(Born2 ~ College2, labeller = labeller(College2 = new_labels))

p10 <- ggplot(tenth_tibble, aes(x = Pos2, y = median)) +
  geom_bar(stat = "identity", width = .5, fill = "indianred3") +
  geom_text(aes(label = trunc(median), vjust = -0.3)) +
  labs(title = "Median Win Shares by Place of Birth",
       subtitle = "2000-09 NBA Drafts",
       x = "", y = "Win Shares") +
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(Born2 ~ College2, labeller = labeller(College2 = new_labels))
p9 + p10 + plot_layout(ncol = 2)

# write to new csv
draf2 <- draft
write.csv(draf2, "./data/draft2.csv", row.names = FALSE)
