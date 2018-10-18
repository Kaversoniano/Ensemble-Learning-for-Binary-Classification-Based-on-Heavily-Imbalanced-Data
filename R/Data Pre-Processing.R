
### preparatory work ###
setwd('D:/R/BMC') # set directory
bank_full <- read.csv(file = "bank-full.csv", header = TRUE, sep = ";");

library(dummies)
library(vcd)


### "contact" variable - categorical ###
summary(bank_full$contact)
table(bank_full$y,bank_full$contact)
chisq.test(bank_full$y,bank_full$contact)
assocstats(table(bank_full$y,bank_full$contact))

id <- which(bank_full$contact=="unknown");
x <- factor(as.numeric(bank_full$contact[-id]), levels = c(1,2), labels = c("cellular","telephone"));
table(bank_full$y[-id],x)
chisq.test(bank_full$y[-id],x)
assocstats(table(bank_full$y[-id],x))
remove(x)
remove(id)

CONTACT <- dummy(bank_full$contact, sep = "_"); # reserved - 3 categories
CONTACT <- data.frame(CONTACT);


### "month" variable - ordered categorical ###
attach(bank_full)
MONTH <- ifelse(month=="dec",12,ifelse(month=="nov",11,ifelse(month=="oct",10,ifelse(month=="sep",9,ifelse(month=="aug",8,ifelse(month=="jul",7,ifelse(month=="jun",6,ifelse(month=="may",5,ifelse(month=="apr",4,ifelse(month=="mar",3,ifelse(month=="feb",2,1)))))))))));
detach(bank_full)


### "poutcome" variable - categorical ###
summary(bank_full$poutcome)
table(bank_full$y,bank_full$poutcome)
chisq.test(bank_full$y,bank_full$poutcome)
assocstats(table(bank_full$y,bank_full$poutcome))

id <- which(bank_full$poutcome=="unknown");
x<-factor(as.numeric(bank_full$poutcome[-id]), levels = c(1,2,3), labels = c("failure","other","success"));
table(bank_full$y[-id],x)
chisq.test(bank_full$y[-id],x)
assocstats(table(bank_full$y[-id],x))
remove(x)
remove(id)

POUTCOME <- dummy(bank_full$poutcome, sep = "_"); # reserved - 4 categories
POUTCOME <- POUTCOME[,-4]; # equivalent to IFP variable as follows - so delete it
POUTCOME <- data.frame(POUTCOME);


### "pdays" variable  - complex ###
pdaysT <- bank_full$pdays[which(bank_full$pdays!=-1)];
summary(pdaysT)
quantile(pdaysT, probs = c(0,0.25,0.5,0.75,1))
remove(pdaysT)
PDAYS <- ifelse((bank_full$pdays<=194)&(bank_full$pdays>=133),2,ifelse((bank_full$pdays<=327)&(bank_full$pdays>194),3,ifelse(bank_full$pdays>327,4,ifelse((bank_full$pdays<133)&(bank_full$pdays>0),1,0))));

PDAYS <- dummy(PDAYS, sep = "_");
IFP <- as.numeric(!PDAYS[,1]); # if the client was previously contacted before - 1 for yes and 0 for no
PDAYS <- PDAYS[,-1]; # equivalent to IFP variable as follows - so delete it
PDAYS <- data.frame(PDAYS);


### "job" and "marital" variable ###
summary(bank_full$job)
table(bank_full$y,bank_full$job)
chisq.test(bank_full$y,bank_full$job)
assocstats(table(bank_full$y,bank_full$job))

id <- which(bank_full$job=="unknown");
x <- factor(as.numeric(bank_full$job[-id]), levels = c(1,2), labels = c("cellular","telephone"));
table(bank_full$y[-id],x)
chisq.test(bank_full$y[-id],x)
assocstats(table(bank_full$y[-id],x))
remove(x)
remove(id)

JOB <- dummy(bank_full$job, sep = "_"); # "unknown is included"
JOB <- data.frame(JOB);

MARITAL <- dummy(bank_full$marital, sep = "_"); # "unknown is not included"
MARITAL <- data.frame(MARITAL);


### "education" variable ###
summary(bank_full$education)
table(bank_full$y,bank_full$education)
chisq.test(bank_full$y,bank_full$education)
assocstats(table(bank_full$y,bank_full$education))

id <- which(bank_full$education=="unknown");
x <- factor(as.numeric(bank_full$education[-id]), levels = c(1,2), labels = c("cellular","telephone"));
table(bank_full$y[-id],x)
chisq.test(bank_full$y[-id],x)
assocstats(table(bank_full$y[-id],x))
remove(x)
remove(id)

EDUCATION <- dummy(bank_full$education, sep = "_");
EDUCATION <- data.frame(EDUCATION);


### reconstruct data ###
bank_fullR <- data.frame(age = bank_full$age, JOB, MARITAL, EDUCATION, default = as.numeric(bank_full$default), balance = bank_full$balance, housing = as.numeric(bank_full$housing), loan = as.numeric(bank_full$loan));
bank_fullR <- data.frame(bank_fullR, CONTACT, day = bank_full$day, month = MONTH, duration = bank_full$duration);
bank_fullR <- data.frame(bank_fullR, campaign = bank_full$campaign, IFP, previous = bank_full$previous, POUTCOME, PDAYS);
bank_fullR <- data.frame(bank_fullR, y = as.numeric(bank_full$y)-1);


