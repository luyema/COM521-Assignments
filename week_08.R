#PC0.Load up your dataset as you did in Week 3 PC2.#
getwd()
setwd("~/Downloads/uwcom521-assignments-master")
week.3<-read.csv("week3_dataset-luyue.csv")
#PC1. If you recall from Week PC6, x and y seemed like they linearly related. We now have the tools and terminology to describe this relationship and to estimate just how related they are. Run a t.test between x and y in the dataset and be ready to interpret the results for the class.#
t.test(week.3$x,week.3$y)
#PC2. Estimate how correlated x and y are with each other.#
cor(week.3$x,week.3$y)
plot(week.3$x,week.3$y)
#PC3. Recode your data in the way that I laid out in Week 3 PC7.#
week.3$i<-as.logical(week.3$i)
week.3$j<-as.logical(week.3$j)
week.3$k.factor<-factor(week.3$k,levels = c(0,1,2,3), labels = c("none","some","lots","all"))
week.3$k<-week.3$k.factor
#PC4. Generate a set of three linear models and be ready to intrepret the coefficients, standard errors, t-statistics, p-values, and {\displaystyle \mathrm {R} ^{2}} {\displaystyle \mathrm {R} ^{2}} for each:#
#(a)#
f1<-lm(week.3$y~week.3$x,data = week.3)
summary(f1)
#(b)#
f2<-lm(week.3$y~week.3$x+week.3$i+week.3$j,data = week.3)
summary(f2)
#(c)#
week3.df<-read.csv("week3_dataset-luyue.csv")
f3<-lm(week.3$y~week.3$x+week.3$i+week.3$j+week3.df$k,data = week.3)
summary(f3)
#PC5. Generate a set of residual plots for the final model (c) and be ready to interpret your model in terms of each of these:#
#(a) A histogram of the residuals.#
residuals(f3)
hist(residuals(f3))
#(b) Plot the residuals by your values of x, i, j, and k (four different plots).#
plot(week.3$x,residuals(f3))
plot(week.3$i,residuals(f3))
plot(week.3$j,residuals(f3))
plot(week.3$k,residuals(f3))
plot(week3.df$k,residuals(f3))
#(c)A QQ plot to evaluate the normality of residuals assumption.#
qqnorm(residuals(f3))
#PC6. Generate a nice looking publication-ready table with a series of fitted models and put them in a Word document.#
d.graph<-data.frame(week.3$x,residuals(f3))
library(ggplot2)
ggplot(data = d.graph)+aes(x=week.3$x,y=residuals)+geom_point()
install.packages("stargazer")
library(stargazer)
stargazer(f3, type = "text")
#Now, lets go back to the Michelle Obama dataset we used last week the week 7 problem set's programming challenges.#
#PC7. Load up the dataset once again and fit the following linear models and be ready to interpret them similar to the way you did above in PC4:#
pc7.df<-readstata13::read.dta13("Halloween2012-2014-2015_PLOS.dta")
#(a)#
table (complete.cases(pc7.df))
f4<-lm(pc7.df$fruit~pc7.df$obama,data = pc7.df)
summary(f4)
mean(pc7.df$fruit,na.rm = TRUE)
#(b)#
f5<-lm(formula = pc7.df$fruit~pc7.df$obama+pc7.df$age+as.factor(pc7.df$treat_year),data = pc7.df)
summary(f5)
hist(residuals(f5))
#PC8. Take a look at the residuals for your model in (a) and try to interpret these as you would in PC4 above. What do you notice?#
residuals(f4)
hist(f4)
#PC9. Run the simple model in (a) three times on three subsets of the dataset: just 2012, 2014, and 2015. Be ready to talk through the results.#
summary(lm(pc7.df$fruit~pc7.df$obama,data = pc7.df[pc7.df$treat_year == 2012]))
summary(lm(pc7.df$fruit~pc7.df$obama,data = pc7.df[pc7.df$treat_year == 2014]))
summary(lm(pc7.df$fruit~pc7.df$obama,data = pc7.df[pc7.df$treat_year == 2015]))
