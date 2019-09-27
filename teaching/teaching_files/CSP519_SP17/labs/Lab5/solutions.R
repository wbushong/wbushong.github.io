d$Neurot.centered.2sds <- d$Neurot - (mean(d$Neurot) - 2*sd(d$Neurot))
m.highneurot.gender <- lm(Self_control ~ Neurot.centered.2sds * Gender, d)
summary(m.highneurot.gender)