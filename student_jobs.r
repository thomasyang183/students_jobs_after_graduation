#Load data for problem two
library(readr)
students <- read_csv("~/Google Drive/STA138/Project/students.csv")

table4 = table(students$year, students$job)
spilt.data = split(students, students$year)

#estimate probability of Freshmen believe it possible to find a job
prop.test(table4["Freshman","Yes"], sum(table4["Freshman",]), conf.level = 1-alpha)

prop.test(table4["Sophomore","Yes"], sum(table4["Sophomore",]), conf.level = 1-alpha)

prop.test(table4["Junior","Yes"], sum(table4["Junior",]), conf.level = 1-alpha)

prop.test(table4["Senior","Yes"], sum(table4["Senior",]), conf.level = 1-alpha)


#Pearson’s Chi squared test
chisq.test(students$year,students$job,correct = FALSE)

#test statistic
alpha = .05
g = 6
qnorm(1-(alpha/(2*g)))

#confidence intervals
wald_ci <- function(rr, pi1, y1, pi2, y2) {
  haha1 <- log(rr)
  haha2 <- (1-pi1)/y1
  haha3 <- (1-pi2)/y2
  haha4 <- sqrt(haha2 + haha3)
  haha5 <- 2.635*haha4
  c(lower = haha1-haha5, upper = haha1+haha5)
}
