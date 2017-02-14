#PC1. Download this dataset in Stata DTA format which contains an anonymized and reduced version of the data visualized in the Buechley and Hill paper on Lilypad. Once you have it#
getwd()
load("lilypad_anonymized.dta")
library(foreign)
d<-read.dta("lilypad_anonymized.dta")
#(a) Reproduce both Table 1 and Table 2 (just US users) using the dataset (as closely as possible).#
table1<-table(d$gender,d$order_type)
table2<-xtabs(~d$gender+d$order_type, data=d,subset = d$country =="United States")
#(b) Run a {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}}-test on both tables. Compare to the paper (for Table 1, there doesn't seem to be a {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}} test for Table 2). Did you reproduce it?#
chisq.test(table1)
chisq.test(table2)
#(c) Install the package "gmodels" and try to display the table using the function CrossTable(). This will give you output very similar to SPSS.#
install.packages("gmodels")
library(gmodels)
CrossTable(table1)
CrossTable(table2)
#(c) It's important to be able to import tables directly into your word processor without cutting and pasting individual cells. Can you export the output of your table? There are a bunch of functions you can use to do this. I used the "xtable" package but I think that write.table() and Excel would do the job just as well.#
write.table(table1)
write.table(table2)
#PC2. At the Community Data Science Workshops we had two parallel afternoon sessions on Day 1. In my session, there were 42 participants. In Tommy Guy's session, there were only 19. The next week (Day 2), we asked folks to raise their hands if they had been in Tommy's session (14 did ) and how many had been in mine (31 did). There was clearly attrition in both groups! Was there more attrition in one group than another? Try answering this both with a test of proportions (prop.test()) and with a {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}}. Compare your answers. Is there convincing evidence that there is a dependence between instructor and attrition?#
m<-rbind(c(19,42),
          c(14,31))
colnames(m)<-c("Tommy","Mako")
rownames(m)<-c("week1","week2")
chisq.test(m)
prop.test(m)
#PC3. Download this dataset that was just published on "The Effect of Images of Michelle Obama’s Face on Trick-or-Treaters’ Dietary Choices: A Randomized Control Trial." The paper doesn't seem to have even been published yet so I think the abstract is all we have. We'll come back to it again next week.#
#(a) Download and import the data into R. I needed to install the "readstata13" package to do so.#
load("halloweenforest_PLOS_final.Rdata")
install.packages("readstata13")
q<-readstata13::read.dta13("Halloween2012-2014-2015_PLOS.dta")
#(b) Take a look at the codebook if necessary. Recode the data on being presented with Michelle Obama's face and the data on whether or not kids picked up fruit. we'll leave it at that for now.#
head(q)
q.new<-table(q$obama,q$fruit)
#(c) Do a simple test on whether or not the two groups are dependent. Be ready to report those tests. The results in the paper will use linear regression. Do you have a sense, from your reading, why your results using the coding material we've learned might be different?#
chisq.test(q.new)
