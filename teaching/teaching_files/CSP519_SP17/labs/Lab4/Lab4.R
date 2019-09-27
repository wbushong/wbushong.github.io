## we'll need ggplot to plot the data and car for the Anova() function
library(ggplot2)
library(car)

## load data and look at it
d <- read.csv("codingdata.csv")
head(d)
summary(d)

## visualize difference between conditions
p <- ggplot(d, aes(x = group, y = self.confidence)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") 
p
# (as an aside, stat_summary() tells R to group by the "group" factor, compute the mean 
# and confidence intervals, and plot them as points with error bars. Great, convenient function!!)

## fit model and see what default contrasts are
m <- lm(self.confidence ~ group, d)
summary(m)

contrasts(d$group)

model.matrix(m)

###### Dummy Coding ######  
## Example hypothesis: Children who receive negative feedback have higher self-confidence than each of the other groups 
## Coding scheme: Use dummy coding and have child.neg be the reference group

## First way of dummy coding: using the contr.treatment() function
contrasts(d$group) <- contr.treatment(4, base = 3)
# We can rename the contrast vectors to have more sensical names
colnames(contrasts(d$group)) <- c("childneg.vs.adultneg", "childneg.vs.adultpos", "childneg.vs.childpos")

contrasts(d$group)

## Second way of dummy coding: manual assignment
contrasts(d$group) <- cbind(childneg.vs.adultneg = c(1, 0, 0, 0),
                            childneg.vs.adultpos = c(0, 1, 0, 0),
                            childneg.vs.childpos = c(0, 0, 0, 1))

contrasts(d$group)

## Fit a model
m.childneg.ref <- lm(self.confidence ~ group, d)
summary(m.childneg.ref)


###### Contrast Coding ######  
# Suppose we have the following hypotheses:
# * Children have higher self-confidence than adults.
# * Feedback does not affect adults' self-confidence.
# * Feedback does affect children's self-confidence.
contrasts(d$group) <- cbind(adult.vs.child = c(1, 1, -1, -1),
                            feedback.adult = c(1, -1, 0, 0),
                            feedback.child = c(0, 0, 1, -1))    # why do these contrasts work?

contrasts(d$group)

m.contrastcodes1 <- lm(self.confidence ~ group, d)
summary(m.contrastcodes1)

###### Practice Problems ######
# 1. Dummy code d$group so that child.pos is the reference group and fit a model. 
  # (a) What is the meaning of the intercept?
  # (b) From only looking at model output, what is the mean of the child.neg group? 
  # (c) From only looking at model output, what is the mean of the adult group? 
contrasts(d$group) <- cbind(adult.neg = c(1, 0, 0, 0),
                            adult.pos = c(0, 1, 0, 0),
                            child.neg = c(0, 0, 1, 0))
m.dummy.childpos <- lm(self.confidence ~ group, d)
summary(m.dummy.childpos)$coefficients

# 2. Create contrast codes for d$group that test the difference between:
  # (1) the mean of children and the mean of adults, 
  # (2) the mean of negative feedback and the mean of positive feedback, and 
  # (3) the difference between the effect of feedback for children vs. adults.
  # After you've created these codes, how will you interpret each of the betas in your model?
contrasts(d$group) <- cbind(adult.vs.child = c(0.5, 0.5, -0.5, -0.5),
                            neg.vs.pos = c(0.5, -0.5, 0.5, -0.5),
                            feedback.adult.vs.child = c(0.25, -0.25, -0.25, 0.25))

m.anova <- lm(self.confidence ~ group, d)
summary(m.anova)



# Part II: Type I, II, and III SS
d2 <- read.csv("UnbalancedInteractionData.csv")
head(d2)
summary(d2)

table(d2$Gender, d2$Grade)

contrasts(d2$Gender)
contrasts(d2$Grade)

# Contrast-code gender and grade: 
contrasts(d2$Gender) <- cbind(male = c(-1, 1))
contrasts(d2$Grade) <- cbind(Grade7 = c(-1, 1))

mm <- model.matrix(Self_control ~ Gender * Grade, d2) # fit model matrix

mm[, 2] %*% mm[, 3] # contrasts are not orthogonal in practice! 
mm[, 2] %*% mm[, 4]
mm[, 3] %*% mm[, 4]

m.int <- lm(Self_control ~ Gender * Grade, d2)
anova(m.int)

# Note the differences with different order:
m.int.2 <- lm(Self_control ~ Grade * Gender, d2) # enter Grade first
anova(m.int.2)

# anova() is computing Type I SS!

# You can specify Type II or III in the Anova() function
Anova(m.int, type = "II")
Anova(m.int.2, type = "II")

Anova(m.int, type = "III")
Anova(m.int.2, type = "III")

# summary() by default gives Type III 
summary(m.int)
summary(m.int.2)
