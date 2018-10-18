
setwd('D:/R/BMC') # set directory
bank_full <- read.csv(file = "bank-full.csv", header = TRUE, sep = ";");

library(ggplot2)
library(vioplot)
library(vcd)
library(rCharts)
library(recharts)
library(plotly)
library(dygraphs)

### merely numeric - density and boxplot ###
par(mfrow = c(1,2))

par(lwd = 2)
plot(density(bank_full$age), main = "Density of variable - age", xlab = "age", ylab = "density")
polygon(density(bank_full$age), col = "orange", border = "red")
rug(bank_full$age, col = "black")

par(lwd = 2)
L <- quantile(bank_full$balance, probs = 0.05);
U <- quantile(bank_full$balance, probs = 0.95);
plot(density(bank_full$balance[bank_full$balance>=L & bank_full$balance<=U]), main = "Density of variable - balance", xlab = "balance", ylab = "density")
polygon(density(bank_full$balance[bank_full$balance>=L & bank_full$balance<=U]), col = "orange", border = "red")
rug(bank_full$balance[bank_full$balance>=L & bank_full$balance<=U], col = "black")

par(mfrow = c(1,2))

attach(bank_full)
par(lwd = 2)
boxplot(day, col = "lightgrey", main = "Boxplot of variable - day", ylab = "day")
par(lwd = 2);
boxplot(duration[duration<quantile(duration, probs = 0.85)], col = "lightgrey", main = "Boxplot of variable - duration", ylab = "duration")
detach(bank_full)


### merely categorical - histogram (with or without "unknown") ###
ggplot(data = bank_full, mapping = aes(x = job)) + geom_bar(stat = 'count', fill = 'steelblue', colour = 'red2')

par(mfrow = c(1,2))

mosaic(~education+y, data = bank_full, shade = TRUE, legend = TRUE)
mosaic(~default+y, data = bank_full, shade = TRUE, legend = TRUE)
mosaic(~loan+y, data = bank_full, shade = TRUE, legend = TRUE)

id <- which(bank_full$poutcome!="unknown");
dat <- bank_full[id,c(16,17)];
dat[,1] <- factor(as.numeric(dat[,1]), labels = c("failure","other","success")); # remove "unknown"
mosaic(~poutcome+y, data = dat, shade = TRUE, legend = TRUE)


### numeric & categorical ###
ggplot(bank_full, aes(x = age, y = balance, group = y, colour = y, shape = y)) + geom_point() +
  labs(x = "age", y = "balance", title = "'age' vs 'balance' distinguished by response variable 'y'") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(panel.background = element_rect(fill = "grey95", colour = "grey50")) +
  theme(legend.position = "bottom", legend.background = element_rect(fill = "grey95", colour = "grey50")) +
  theme(legend.key = element_rect(colour = "grey95")) +
  theme(legend.title = element_text(face = "bold", size = 11), legend.text = element_text(size = 10)) +
  scale_colour_discrete(name = "response variable 'y'", labels = c("no","yes")) +
  scale_shape_discrete(name = "response variable 'y'", labels = c("no","yes"))

DC <- data.frame(bank_full$duration, bank_full$contact);
DC <- DC[DC[,1]<=quantile(DC[,1], probs = 0.95),]
p <- ggplot(data = DC, aes(x = bank_full.duration, fill = bank_full.contact)) + 
  geom_density() + facet_grid(bank_full.contact~.)
ggplotly(p)

L <- quantile(bank_full$balance, probs = 0.05);
U <- quantile(bank_full$balance, probs = 0.95);
BE <- data.frame(bank_full$balance,bank_full$default);
BE <- BE[BE[,1]>=L & BE[,1]<=U,];
x1 <- BE[,1][BE[,2]=="yes"];
x2 <- BE[,1][BE[,2]=="no"];
vioplot(x1, x2, col = "gold", names = c("yes","no"));
title(main = "Violin plots of balance grouped by default status", xlab = "default", ylab = "balance");


