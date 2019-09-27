library(foreign)
library(pwr)
source("showmelm_WB.R")

d <- read.spss("attachdiag.sav", to.data.frame = TRUE)
head(d)
nrow(d) # get number of observations
d <- d[complete.cases(d), ]
nrow(d)

d$secure.z <- scale(d$secure)

cors <- cor(d[, c("neglect", "secure", "avoid", "ambiv", "masc", "fem")], use = "pairwise.complete")
cors

m <- lm(neglect ~ secure, data = d)
## What's in a model object? check with names()
names(m)
m$coefficients

summary(m)

names(summary(m))
summary(m)$coefficients

## Hierarchical Regression
m2 <- lm(neglect ~ secure + avoid, d)
summary(m2)
r.sq.dif <- summary(m2)$r.squared - summary(m)$r.squared 

## Comparing models using the anova() function
anova(m, m2)

## Partial Correlations: showme.lm() function
showme.lm(m2, verbose = TRUE) 

### Stepwise Regression ###
m.base <- lm(neglect ~ 1, d) # base model only fits an intercept
m.full <- lm(neglect ~ secure + avoid + ambiv + masc + fem, d) # full model uses all variables of interest

step(m.base, scope = list(lower = m.base, upper = m.full), direction = "forward")
m.final <- step(m.full, scope = list(lower = m.base, upper = m.full), direction = "backward")

### Regression w/ Sets ###
m.full <- lm(neglect ~ secure + avoid + ambiv + masc + fem, d) # full model
m.attach <- lm(neglect ~ secure + avoid + ambiv, d) # model w/ attachment set
m.mascfem <- lm(neglect ~ masc + fem, d) # model with masc/fem set

# Comparing m.full and m.attach gives us the effect of adding masculinity/femininity 
# to the model that contains the set of attachment variables:
anova(m.full, m.attach) 
summary(m.full)$r.squared - summary(m.attach)$r.squared

# Comparing m.full and m.mascfem gives us the effect of adding attachment variables to 
# the model that contains the set of masculinity/femininity:
anova(m.full, m.mascfem)
summary(m.full)$r.squared - summary(m.mascfem)$r.squared

### Power Analyses ###
## Step 1: get df of model
df1 <- summary(m.final)$fstatistic["numdf"]
df2 <- summary(m.final)$fstatistic["dendf"]
## Step 2: get effect size of fem
r2.overall <- summary(m.final)$r.squared
r2.fem <- showme.lm(m.final)$RegressionTables$coefficients["fem", "sr2"]
f2.fem <- (r2.overall - r2.fem)/(1 - r2.overall)
## Step 3: Plug into pwr.f2.test() function
power <- pwr.f2.test(u = df1, v = df2, f2 = f2.fem, sig.level = 0.05)
power


