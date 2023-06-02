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
p1 <- ggplot(draft, aes(x = WS)) + geom_histogram(fill="royalblue3", color="royalblue3", bins = 8) + labs(title = "Career Win Shares Distribution of NBA First-Round Selections",
       subtitle = "2000-09 NBA Drafts",
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

