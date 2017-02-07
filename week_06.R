#PC0. Download the dataset by clicking through on the "Red Dye Number 40" link on this webpage. You'll find that the it's not in an ideal setup. It's an Excel file (XLS) with a series of columns labeled X1.. X4. The format is not exactly tabular.#
setwd("~/Downloads/uwcom521-assignments-master")
read.csv("owan03.csv")
#PC1. Load the data into R. Now get to work on reshaping the dataset. I think a good format would be a data frame with two columns: group, time of death (i.e., lifespan).#
df<-read.csv("owan03.csv")
#I did this manually...#
d<-read.csv("Workbook1.csv")
#PC2. Create summary statistics and visualizations for each group. Write code that allows you to generate a useful way to both (a) get a visual sense both for the shape of the data and its relationships and (b) the degree to which the assumptions for t-tests and ANOVA hold. What is the global mean of your dependent variable?#
summary(df$X1)
summary(df$X2)
summary(df$X3)
summary(df$X4)
hist(df$X1)
hist(df$X2)
hist(df$X3)
hist(df$X4)
plot(d$groups,d$lifespan)
#PC3. Do a t-test between mice with any RD40 and mice with at least a small amount. Run a t-test between the group with a high dosage and control group. How would you go about doing it using formula notation? Be ready to report, interpret, and discuss the results in substantive terms.#
t.test(df$X1,df$X4)
t.test(df$X1,df$X4,paired = TRUE)
#PC4. Estimate an ANOVA analysis using aov() to see if there is a difference between the groups. Be ready to report, interpret, and discuss the results in substantive terms.#
lifespan.group.model<-aov(lifespan~groups,data=d)
summary(lifespan.group.model)