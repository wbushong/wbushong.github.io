# 1. Dummy code d$group so that child.pos is the reference group and fit a model. 
# (a) What is the meaning of the intercept?
# (b) From only looking at model output, what is the mean of the child.neg group? 
# (c) From only looking at model output, what is the mean of the adult groups? 
contrasts(d$group) <- cbind(adult.neg = c(1, 0, 0, 0),
                            adult.pos = c(0, 1, 0, 0),
                            child.neg = c(0, 0, 1, 0))
m.dummy.childpos <- lm(self.confidence ~ group, d)
summary(m.dummy.childpos)$coefficients
# intercept = mean of child.pos group 
# mean of child.neg group = 4.578 + 3.289 = 7.867
# mean of adult groups = (4.578 - 0.341) + (4.578 - 0.738) / 2 = 4.0385
# check with the actual data.frame:
mean(d$self.confidence[d$group == "child.neg"])
mean(d$self.confidence[grep("adult", d$group)])

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