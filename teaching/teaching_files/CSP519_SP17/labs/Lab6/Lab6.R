### Load libraries & data ###
library(foreign)
library(tidyr)
library(ggplot2)

### Data ###
# data
d <- read.spss("data.sav", to.data.frame = TRUE)
# what does the data look like?
summary(d)
head(d)
# label participants
d$participant <- as.factor(1:nrow(d))

### First, plotting! ###
p.change <- ggplot(d, aes(x = bgender, y = change)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange")
p.change

d.gathered <- gather(d, time, value, bhndicap:fhndicap)
# we 'gather' the data so that bhndicap and fhndicap are compressed into one column 
# that columns essentially represents time, so I call it 'time' (argument 2 of gather)
# the other column, which I assign the name 'value', will contain the actual values that were the original contents of the bhndicap and fhndicap columns

# plot data over time
p.over.time <- ggplot(d.gathered, aes(x = time, y = value, 
                                      group = bgender, shape = bgender)) +
  stat_summary(fun.y = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(fun.y = mean, geom = "line")
p.over.time

### Straight Change ###
m <- aov(change ~ bgender + Error(participant), d) # Error(participant) adds in the within-subject error term into the model
summary(m)

# equivalent to:
m.int <- aov(value ~ bgender * time + Error(participant), d.gathered)
summary(m.int)

m2 <- lm(change ~ bgender, d)
summary(m2)
anova(m2)

### Residual Change ###
m3 <- aov(fhndicap ~ bhndicap + bgender, d)
summary(m3)

m4 <- lm(fhndicap ~ bhndicap + bgender, d)
summary(m4)
