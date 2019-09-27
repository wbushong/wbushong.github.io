#########################
## Lab 5
#########################
# rgl will let us plot in 3 dimensions
library(rgl)
library(ggplot2)

# Load data & inspect 
d <- read.csv("InteractionData.csv")
head(d)
summary(d)

#### Categorical x Categorical Interactions ####
# Plotting 
# relevel factors
d$Grade <- factor(d$Grade, levels = c("4th grade", "7th grade", "10th grade"))

p.gender.grade <- ggplot(d, aes(x = Grade, y = Self_control, color = Gender, group = Gender)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange") +
  stat_summary(fun.y = "mean", geom = "line")
p.gender.grade

# Regressions
# change contrasts
contrasts(d$Gender) <- cbind(females = c(1, -1))
contrasts(d$Grade) <- cbind(grade_4 = c(1, 0, 0), grade_10 = c(0, 0, 1))

# Test interaction
m.gender.grade <- lm(Self_control ~ Gender * Grade, d)
summary(m.gender.grade)

m.gender.noint <- lm(Self_control ~ Gender + Grade, d)
anova(m.gender.grade, m.gender.noint)

# Test simple slope for males' self-control between 4th and 7th grade
# recode gender as dummy
d$Gender.dummy <- d$Gender
contrasts(d$Gender.dummy) <- cbind(females = c(1, 0))

m.gender.ss <- lm(Self_control ~ Gender.dummy * Grade, d)
summary(m.gender.ss)

#### Categorical x Continuous Interactions ####
# Plotting
p.neuro.gender <- ggplot(d, aes(x = Neurot, y = Self_control, color = Gender)) +
  stat_summary(fun.y = "mean", geom = "point") +
  geom_smooth(method = "lm") # fits an lm to each subset of the data
p.neuro.gender

# Regressions
# center continuous variable
d$Neurot.centered <- d$Neurot - mean(d$Neurot)

# Test interaction
m.neurot.gender <- lm(Self_control ~ Neurot.centered * Gender, d)
summary(m.neurot.gender)

# Simple slope for highly neurotic individuals
d$Neurot.centered.2sds <- d$Neurot - (mean(d$Neurot) + 2*sd(d$Neurot))

m.highneurot.gender <- lm(Self_control ~ Neurot.centered.2sds * Gender, d)
summary(m.highneurot.gender)

#### Continuous x Continuous Interactions ####
# Plotting -- there are many options here. See pdf of Lab 5 for more details!
# Plot of main effects of consci & neurot
p.neurot <- ggplot(d, aes(x = Neurot, y = Self_control)) +
  geom_point()
p.neurot

p.consci <- ggplot(d, aes(x = Consci, y = Self_control)) +
  geom_point()
p.consci

# Plot interaction using color
p.neurot.consci <- ggplot(d, aes(x = Neurot, y = Consci, color = Self_control)) +
  geom_point(size = 5) +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Scatterplot of Neuroticism x Conscientiousness Interaction")
p.neurot.consci

# Plot interaction using median split 
d$Consci.categorical <- ifelse(d$Consci > median(d$Consci), "high", "low")
p.neurot.consci2 <- ggplot(d, aes(x = Neurot, y = Self_control, color = Consci.categorical)) +
  geom_point() +
  ggtitle("Scatterplot of Neuroticism x Conscientiousness Interaction") +
  geom_smooth(method = "lm")
p.neurot.consci2

# 3D plot
p.3d <- plot3d(d[, c("Consci", "Neurot", "Self_control")])

# Regressions
# center our other continuous variable
d$Consci.centered <- d$Consci - mean(d$Consci)

# Test interaction
m.consci.neurot <- lm(Self_control ~ Consci.centered * Neurot.centered, d)
summary(m.consci.neurot)

# Test simple slope of less conscientious than normal people
d$Consci.1sd <- d$Consci - (mean(d$Consci) - sd(d$Consci))
m.neurot.lowconsci <- lm(Self_control ~ Neurot.centered * Consci.1sd, d)
summary(m.neurot.lowconsci)

## Plotting regression predictions (see text for more details)
# Get mean of consci, and +/-1 SD 
consci.mean <- mean(d$Consci.centered)
consci.1.below <- consci.mean - sd(d$Consci.centered)
consci.1.above <- consci.mean + sd(d$Consci.centered)
# Create fake data based on this
fake.data <- data.frame(Neurot.centered = rep(range(d$Neurot.centered), 3),
                        Consci.centered = c(rep(consci.mean, 2), rep(consci.1.below, 2), rep(consci.1.above, 2)))
# Predict values of self-control
fake.data$Self_control <- predict(m.consci.neurot, fake.data)

#  Plot!
p.predictions <- ggplot(fake.data, aes(x = Neurot.centered, y = Self_control, color = Consci.centered, group = Consci.centered)) +
  geom_point() +
  geom_line()
p.predictions


## Plot regression planes in 3D! (talk to me for more details)
# Plot regression plane with NO interaction
m.no.interaction <- lm(Self_control ~ Neurot.centered + Consci.centered, d)
f1 <- function(X, Z) {
  r <- coef(m.no.interaction)[1] + 
    coef(m.no.interaction)[2] * Z +
    coef(m.no.interaction)[3] * X
}
plot3d(d[c("Consci.centered", "Neurot.centered", "Self_control")])
plot3d(f1,
       xlim = range(d$Consci.centered),
       ylim = range(d$Neurot.centered),
       add = TRUE, col = "red", alpha = .5)


# Plot regression plane WITH interaction
m.interaction <- lm(Self_control ~ Neurot.centered * Consci.centered, d)
f2 <- function(X, Z) {
  r = coef(m.interaction)[1] + 
    coef(m.interaction)[2] * Z +
    coef(m.interaction)[3] * X +
    coef(m.interaction)[4] * Z * X
}
plot3d(d[c("Consci.centered", "Neurot.centered", "Self_control")])
plot3d(f2,
       xlim = range(d$Consci.centered),
       ylim = range(d$Neurot.centered),
       add = TRUE, col = "blue", alpha = .5)

# Plot both!
plot3d(d[c("Consci.centered", "Neurot.centered", "Self_control")])
plot3d(f1,
       xlim = range(d$Consci.centered),
       ylim = range(d$Neurot.centered),
       add = TRUE, col = "red", alpha = .5)
plot3d(f2,
       xlim = range(d$Consci.centered),
       ylim = range(d$Neurot.centered),
       add = TRUE, col = "blue", alpha = .5)



