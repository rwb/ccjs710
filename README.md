## CCJS 710-0101 Advanced Statistics Methods - Limited Dependent Variables

* Course Catalog Description: Application of advanced data analysis strategies to criminological and criminal justice problems, with specific focus on limited dependent variables.
* Instructor: Robert Brame
* Office: LeFrak 2139
* Course Meets: Thursdays 4-6:45 in LeFrak 1220
* Office Hours: Tuesdays and Thursdays from 10-11:00
* Readings will be assigned throughout the semester.
* Course-related policies: In all matters, the class will follow University guidance as outlined [here](https://academiccatalog.umd.edu/graduate/policies/course-related-policies/).
* Accessibility accommodations: If you think you might need one or more academic accommodations, please contact the Accessibility and Disability Service Office ([link](https://ads.umd.edu)) for guidance and assistance. Please contact me to set up an appointment to discuss any accommodations that are authorized. 
* Letter grades: At the end of the semester, letter grades will be assigned on a 100-point scale (A+ = 98 and higher; A = 92-97; A- = 90-91; B+ = 88-89; B = 82-87; B- = 80-81; C+ = for 78-79; C = 72-77; C- = 70-71; D+ = 68-69; D = 62-67; D- = 60-61; and F = any grade less than 60). All numeric grades (including the final numeric grade in the class at the end of the semester) will be rounded off to the nearest 1 point (for example, a 78.5 would be rounded to a 79 and a 78.4 would be rounded to a 78).
* Numeric grades in this class will be based on 2 in-class exams; each exam will contribute equally to your final grade and each will be graded on a 100-point scale.
* Exam questions will be short-answer format and will require you to report and interpret various calculations.
* The mid-term exam will be on Thursday 10/15 and the final exam date/time is TBA. I will let you know as soon as possible.
* On exams, you must turn off your internet connection and not consult any artificial intelligence or any other support services for help. 
* We will be using R statistical software ([link](https://www.r-project.org)) during class sessions; I will post relevant R code on this webpage.

### Course Outline

1. syllabus review
2. R primer
3. measuring the central tendency of age-at-release for prison release cohort
4. review of point estimation and confidence intervals
5. state-level changes in homicide rates
6. estimating the effect of arrest in a domestic violence experiment
7. studying the prevalence of crime victimization in a survey
8. survival time studies of criminal recidivism
9. measuring the association between static risk factors and recidivism
10. observational studies of treatment effects with limited dv outcomes
11. seat belts and injuries in car crashes
12. capital punishment and homicide rates 

### Lesson 1 - Thursday 9/3/26

#### Assigned readings

* Larry Wasserman's R primer ([link](https://www.stat.cmu.edu/~larry/all-of-statistics/=R/Rintro.pdf)).
* Brown et al., + discussion (2001; [link](https://projecteuclid.org/journals/statistical-science/volume-16/issue-2/Interval-Estimation-for-a-Binomial-Proportion/10.1214/ss/1009213286.full)).
* chapter 20 of Weisburd and Britt (2007; [link](https://link.springer.com/book/10.1007/978-0-387-34113-2)).

##### Script #1

```R
set.seed(3)

ts <- c(rep(0,110),rep(1,297),rep(2,651),rep(3,562),rep(4,547),
  rep(5,550),rep(6,625),rep(7,460),rep(8,332),rep(9,324),
  rep(10,250),rep(11,258),rep(12,303),rep(13,236),rep(14,206),
  rep(15,192),rep(16,169),rep(17,126),rep(18,131),rep(19,132),
  rep(20,134),rep(21,113),rep(22,108),rep(23,98),rep(24,107),
  rep(25,97),rep(26,95),rep(27,84),rep(28,82),rep(29,88),
  rep(30,83),rep(31,68),rep(32,61),rep(33,70),rep(34,70),
  rep(35,60),rep(36,79),rep(37,45),rep(38,50),rep(39,35),
  rep(40,40),rep(41,47),rep(42,43),rep(43,33),rep(44,48),
  rep(45,43),rep(46,28),rep(47,25),rep(48,37),rep(49,20),
  rep(50,26),rep(51,24),rep(52,25),rep(53,20),rep(54,33),
  rep(55,27),rep(56,19),rep(57,20),rep(58,20),rep(59,14),
  rep(60,29),rep(61,29),rep(62,19),rep(63,17),rep(64,19),
  rep(65,21),rep(66,15),rep(67,21),rep(68,14),rep(69,8),
  rep(70,10),rep(71,11),rep(72,17),rep(73,11),rep(74,11),
  rep(75,12),rep(76,12),rep(77,7),rep(78,12),rep(79,8),
  rep(80,9),rep(81,15),rep(82,8),rep(83,12),rep(84,12),
  rep(85,7),rep(86,7),rep(87,11),rep(88,9),rep(89,5),
  rep(90,4),rep(91,5),rep(92,5),rep(93,4),rep(94,7),
  rep(95,3),rep(96,14),rep(97,5),rep(98,3),rep(100,3),
  rep(101,4),rep(102,3),rep(103,3),rep(104,5),rep(105,2),
  rep(106,8),rep(107,6),rep(108,4),rep(109,5),rep(110,3),
  rep(111,3),rep(112,5),rep(113,2),rep(114,4),rep(115,2),
  rep(116,2),rep(117,6),rep(118,5),119,rep(120,3),rep(121,4),
  rep(123,2),rep(124,2),rep(125,2),127,rep(128,2),rep(129,4),
  130,131,rep(132,4),rep(133,3),rep(134,2),rep(135,2),
  rep(136,4),137,rep(138,2),139,140,rep(142,2),143,rep(144,2),
  rep(146,2),rep(148,4),149,151,rep(152,2),153,rep(154,2),155,
  rep(156,2),rep(158,3),rep(160,2),rep(161,3),rep(162,2),163,
  164,165,166,167,rep(168,3),rep(170,2),171,172,173,174,177,
  178,rep(179,2),182,183,184,187,190,195,200,202,205,209,213,
  rep(218,2),219,221,225,228,231,233,236,241,243,248,254,255,
  273,274,277,300,305,313,344)

mean(ts)
median(ts)
S <- sample(1:9327,size=300,replace=T)
mean(ts[S])
std.err <- sd(ts[S])/sqrt(300)
std.err
t.mult <- qt(0.91,df=300-1)
t.mult
lcl <- mean(ts[S])-t.mult*std.err
lcl
ucl <- mean(ts[S])+t.mult*std.err
ucl
t.test(ts[S],conf.level=0.82)

trap <- vector()
sm <- vector()
std.err <- vector()

for(i in 1:30000){
  s <- sample(1:9327,size=300,replace=T)
  sm[i] <- mean(ts[s])
  std.err[i] <- sd(ts[s])/sqrt(300)
  t.mult <- qt(0.91,df=300-1)
  lcl <- sm[i]-t.mult*std.err[i]
  ucl <- sm[i]+t.mult*std.err[i]
  trap[i] <- ifelse(lcl<=19.75458 & ucl>=19.75458,1,0)
  }

table(trap)
mean(trap)
mean(sm)
sd(sm)
mean(std.err)
```

* a. check to make sure you have 9,327 observations and that the mean is 19.75458 and the median is 10; this will ensure you have read the data set correctly; set a random number seed equal to your UID number.
* b. Draw a simple random sample of 300 observations from this population; calculate the mean of your sample.
* c. Calculate a 82% confidence interval for your sample mean; report on whether your single confidence interval traps the true population parameter value.
* d. Conduct a simulation study with 10,000 datasets and random samples of size N = 300 to document the coverage rate for your confidence interval procedure.

```Rout
> set.seed(3)
> 
> ts <- c(rep(0,110),rep(1,297),rep(2,651),rep(3,562),rep(4,547),
+   rep(5,550),rep(6,625),rep(7,460),rep(8,332),rep(9,324),
+   rep(10,250),rep(11,258),rep(12,303),rep(13,236),rep(14,206),
+   rep(15,192),rep(16,169),rep(17,126),rep(18,131),rep(19,132),
+   rep(20,134),rep(21,113),rep(22,108),rep(23,98),rep(24,107),
+   rep(25,97),rep(26,95),rep(27,84),rep(28,82),rep(29,88),
+   rep(30,83),rep(31,68),rep(32,61),rep(33,70),rep(34,70),
+   rep(35,60),rep(36,79),rep(37,45),rep(38,50),rep(39,35),
+   rep(40,40),rep(41,47),rep(42,43),rep(43,33),rep(44,48),
+   rep(45,43),rep(46,28),rep(47,25),rep(48,37),rep(49,20),
+   rep(50,26),rep(51,24),rep(52,25),rep(53,20),rep(54,33),
+   rep(55,27),rep(56,19),rep(57,20),rep(58,20),rep(59,14),
+   rep(60,29),rep(61,29),rep(62,19),rep(63,17),rep(64,19),
+   rep(65,21),rep(66,15),rep(67,21),rep(68,14),rep(69,8),
+   rep(70,10),rep(71,11),rep(72,17),rep(73,11),rep(74,11),
+   rep(75,12),rep(76,12),rep(77,7),rep(78,12),rep(79,8),
+   rep(80,9),rep(81,15),rep(82,8),rep(83,12),rep(84,12),
+   rep(85,7),rep(86,7),rep(87,11),rep(88,9),rep(89,5),
+   rep(90,4),rep(91,5),rep(92,5),rep(93,4),rep(94,7),
+   rep(95,3),rep(96,14),rep(97,5),rep(98,3),rep(100,3),
+   rep(101,4),rep(102,3),rep(103,3),rep(104,5),rep(105,2),
+   rep(106,8),rep(107,6),rep(108,4),rep(109,5),rep(110,3),
+   rep(111,3),rep(112,5),rep(113,2),rep(114,4),rep(115,2),
+   rep(116,2),rep(117,6),rep(118,5),119,rep(120,3),rep(121,4),
+   rep(123,2),rep(124,2),rep(125,2),127,rep(128,2),rep(129,4),
+   130,131,rep(132,4),rep(133,3),rep(134,2),rep(135,2),
+   rep(136,4),137,rep(138,2),139,140,rep(142,2),143,rep(144,2),
+   rep(146,2),rep(148,4),149,151,rep(152,2),153,rep(154,2),155,
+   rep(156,2),rep(158,3),rep(160,2),rep(161,3),rep(162,2),163,
+   164,165,166,167,rep(168,3),rep(170,2),171,172,173,174,177,
+   178,rep(179,2),182,183,184,187,190,195,200,202,205,209,213,
+   rep(218,2),219,221,225,228,231,233,236,241,243,248,254,255,
+   273,274,277,300,305,313,344)
> 
> mean(ts)
[1] 19.75458
> median(ts)
[1] 10
> S <- sample(1:9327,size=300,replace=T)
> mean(ts[S])
[1] 17.94
> std.err <- sd(ts[S])/sqrt(300)
> std.err
[1] 1.193213
> t.mult <- qt(0.91,df=300-1)
> t.mult
[1] 1.343899
> lcl <- mean(ts[S])-t.mult*std.err
> lcl
[1] 16.33644
> ucl <- mean(ts[S])+t.mult*std.err
> ucl
[1] 19.54356
> t.test(ts[S],conf.level=0.82)

	One Sample t-test

data:  ts[S]
t = 15.035, df = 299, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 0
82 percent confidence interval:
 16.33644 19.54356
sample estimates:
mean of x 
    17.94 

> 
> trap <- vector()
> sm <- vector()
> std.err <- vector()
> 
> for(i in 1:30000){
+   s <- sample(1:9327,size=300,replace=T)
+   sm[i] <- mean(ts[s])
+   std.err[i] <- sd(ts[s])/sqrt(300)
+   t.mult <- qt(0.91,df=300-1)
+   lcl <- sm[i]-t.mult*std.err[i]
+   ucl <- sm[i]+t.mult*std.err[i]
+   trap[i] <- ifelse(lcl<=19.75458 & ucl>=19.75458,1,0)
+   }
> 
> table(trap)
trap
    0     1 
 5427 24573 
> mean(trap)
[1] 0.8191
> mean(sm)
[1] 19.7508
> sd(sm)
[1] 1.537318
> mean(std.err)
[1] 1.539153
>
```

##### Script #2

```R
# coin 1

h1 <- 12
f1 <- 22
h1/f1

# coin 2

h2 <- 15
f2 <- 31
h2/f2

# build confidence interval

r1 <- rbeta(n=1e5,shape1=1/2+h1,shape2=1/2+f1-h1)
r2 <- rbeta(n=1e5,shape1=1/2+h2,shape2=1/2+f2-h2)
hist(r2-r1)
quantile(r2-r1,c(0.025,0.975))
```

```Rout
> # coin 1
> 
> h1 <- 12
> f1 <- 22
> h1/f1
[1] 0.5454545
> 
> # coin 2
> 
> h2 <- 15
> f2 <- 31
> h2/f2
[1] 0.483871
> 
> # build confidence interval
> 
> r1 <- rbeta(n=1e5,shape1=1/2+h1,shape2=1/2+f1-h1)
> r2 <- rbeta(n=1e5,shape1=1/2+h2,shape2=1/2+f2-h2)
> hist(r2-r1)
> quantile(r2-r1,c(0.025,0.975))
      2.5%      97.5% 
-0.3167316  0.2038734 
>
```

##### Script #3

```R
set.seed(381)

trap <- vector()

for(i in 1:3000){
  x1 <- rbinom(n=1,size=22,p=0.5)
  x2 <- rbinom(n=1,size=31,p=0.5)
  r1 <- rbeta(n=1e5,shape1=1/2+x1,shape2=1/2+22-x1)
  r2 <- rbeta(n=1e5,shape1=1/2+x2,shape2=1/2+31-x2)
  d <- r2-r1
  lcl.d <- quantile(d,0.025)
  ucl.d <- quantile(d,0.975)
  trap[i] <- ifelse(lcl.d<=0 & ucl.d>=0,1,0)
  }

table(trap)
mean(trap)
```

```Rout
> set.seed(381)
> 
> trap <- vector()
> 
> for(i in 1:3000){
+   x1 <- rbinom(n=1,size=22,p=0.5)
+   x2 <- rbinom(n=1,size=31,p=0.5)
+   r1 <- rbeta(n=1e5,shape1=1/2+x1,shape2=1/2+22-x1)
+   r2 <- rbeta(n=1e5,shape1=1/2+x2,shape2=1/2+31-x2)
+   d <- r2-r1
+   lcl.d <- quantile(d,0.025)
+   ucl.d <- quantile(d,0.975)
+   trap[i] <- ifelse(lcl.d<=0 & ucl.d>=0,1,0)
+   }
> 
> table(trap)
trap
   0    1 
 158 2842 
> mean(trap)
[1] 0.9473333
> 
>
```

### Lesson 2 - Thursday 9/10/26

* We begin by considering the NC FY1978 prison releasees.
* The data measure the age of each inmate at the time they were released from prison.
* I will also be providing an example dataset for the NC FY1980 prison releasees (again, age at time of release)
* The data is a limited dv in 2 senses: (1) they are measured in discrete years (integers only); and (2) since the data are based on age at release from prison, there is a lower bound on the age of release (probably around 15 or 16 years old).

#### Script #1

```R
# dataset - NC Department of Corrections FY1978 Releases 
# variable: age (in years) at time of release from prison

age <- c(rep(16,19),rep(17,161),rep(18,492),rep(19,480),rep(20,624),
         rep(21,599),rep(22,580),rep(23,468),rep(24,537),rep(25,443),rep(26,432),
         rep(27,338),rep(28,415),rep(29,292),rep(30,324),rep(31,254),rep(32,234),
         rep(33,179),rep(34,187),rep(35,167),rep(36,177),rep(37,132),rep(38,152),
         rep(39,117),rep(40,119),rep(41,93),rep(42,113),rep(43,102),rep(44,85),
         rep(45,75),rep(46,90),rep(47,72),rep(48,86),rep(49,62),rep(50,78),
         rep(51,61),rep(52,57),rep(53,50),rep(54,44),rep(55,49),rep(56,55),
         rep(57,34),rep(58,34),rep(59,25),rep(60,21),rep(61,18),rep(62,19),
         rep(63,11),rep(64,16),rep(65,7),rep(66,5),rep(67,13),rep(68,5),rep(69,3),
         rep(70,1),rep(71,3),rep(72,5),rep(73,3),rep(74,4),rep(75,2),rep(77,2),rep(78,2))

# population size

n <- length(age)
n

# population central tendency

mean(age)
median(age)

# population distribution

hist(age,xlab="Age (in years) at Time of Release",
         ylab="Number of People",
         main="Age at Release from Prison (1978 NCDOC)")
```

#### Script #2

```R
# let's draw a single sample from the population

set.seed(847)
ss <- sample(1:9327,size=300,replace=T)
yss <- age[ss]
hist(yss)
mean(yss)
median(yss)
mean(yss)-mean(age)
median(yss)-median(age)
```

#### Script #3

```R
# let's draw 3,000 samples from the population

set.seed(704)

meanvec <- vector()
medianvec <- vector()

for(i in 1:3e3){
  rs <- sample(1:9327,size=300,replace=T)
  yrs <- age[rs]
  meanvec[i] <- mean(yrs)
  medianvec[i] <- median(yrs)
  }

par(mfrow=c(1,3))
hist(meanvec)
hist(medianvec)
boxplot(meanvec,medianvec,names=c("Mean","Median"))
sd(meanvec)
sd(medianvec)
```

#### Script #4

```R
# now, we consider a 88% confidence interval for the sample mean
# we will use the t-distribution under the assumption that the
# sampling distribution of the sample means follows a t-distribution
# with N-1 degrees of freedom (in this case, 300-1 = 299 df.

set.seed(872)

ct <- qt(p=0.94,df=300-1)
ct

trap <- vector()

for(i in 1:3e3){
  rs <- sample(1:9327,size=300,replace=T)
  yrs <- age[rs]
  lcl <- mean(yrs)-ct*(sd(yrs)/sqrt(300))
  ucl <- mean(yrs)+ct*(sd(yrs)/sqrt(300))
  trap[i] <- ifelse(lcl<=mean(age) & ucl>=mean(age),1,0)
  }

mean(trap)
```

#### Script #5

```R
# what about an 88% confidence interval for the sample median?

set.seed(873)
library(DescTools)

trap <- vector()

for(i in 1:3e3){
  rs <- sample(1:9327,size=300,replace=T)
  yrs <- age[rs]
  ci <- MedianCI(yrs,conf.level=0.88,method="boot")
  lcl <- ci[2]
  ucl <- ci[3]
  trap[i] <- ifelse(lcl<=median(age) & ucl>=median(age),1,0)
  }

mean(trap)
```

#### Script #6

```R
# using the bootstrap to calculate a confidence interval
# we will calculate a 88% confidence interval for both the
# sample mean and the sample median based on the information
# in a single simple random sample of 300 cases (drawn from
# the sampling frame of 9,327 cases.

set.seed(638)

ss <- sample(1:9327,size=300,replace=T)
yss <- age[ss]
ct <- qt(p=0.94,df=300-1)
ct

# parametric confidence interval based on t-distribution

mean(yss)-ct*(sd(yss)/sqrt(300))
mean(yss)+ct*(sd(yss)/sqrt(300))

# note there is no parametric formula for the 88% confidence
# interval for the sample median.

# bootstrap - percentile method

mnvec <- vector()
mdvec <- vector()

for(i in 1:3e3){
  b <- sample(1:300,size=300,replace=T)
  yb <- yss[b]
  mnvec[i] <- mean(yb)
  mdvec[i] <- median(yb)
  }

quantile(mnvec,c(0.06,0.94))
quantile(mdvec,c(0.06,0.94))
```

#### Script #7

```R
# there is an exact procedure for calculating the
# confidence interval for the sample median (based on
# the binomial distribution)
# in this case, exact means that the procedure is guaranteed
# to cover the true population parameter across repeated
# samples at or above the advertised rate
# we will repeatedly sample from the sampling frame to
# demonstrate this property

set.seed(422)

library(DescTools)

trap <- vector()

for(i in 1:300){
  rs <- sample(1:9327,size=300,replace=T)
  yrs <- age[rs]
  ci <- MedianCI(yrs,conf.level=0.88,method="exact")
  lcl <- ci[2]
  ucl <- ci[3]
  trap[i] <- ifelse(lcl<=median(age) & ucl>=median(age),1,0)
  }

mean(trap)
```

#### NCDOC FY1980 Age at Release Dataset (N = 9,549)

```R
age <- c(rep(15,1),rep(16,20),rep(17,224),rep(18,504),rep(19,472),rep(20,626),
  rep(21,517),rep(22,601),rep(23,516),rep(24,565),rep(25,407),rep(26,495),
  rep(27,302),rep(28,397),rep(29,291),rep(30,298),rep(31,261),rep(32,330),
  rep(33,224),rep(34,231),rep(35,163),rep(36,194),rep(37,157),rep(38,149),
  rep(39,125),rep(40,129),rep(41,116),rep(42,100),rep(43,88),rep(44,105),
  rep(45,88),rep(46,80),rep(47,72),rep(48,60),rep(49,68),rep(50,67),
  rep(51,64),rep(52,50),rep(53,47),rep(54,51),rep(55,47),rep(56,42),
  rep(57,28),rep(58,39),rep(59,12),rep(60,29),rep(61,12),rep(62,13),
  rep(63,8),rep(64,19),rep(65,12),rep(66,9),rep(67,2),rep(68,5),rep(69,3),
  rep(70,6),rep(71,1),rep(73,2),rep(74,2),rep(75,1),rep(77,1),rep(79,1))
```

### Lesson 3 - Thursday 9/17/26

* Topic: review of point estimation and confidence intervals
* Our focus today will be on proportions and the difference between 2 proportions.
* Let's begin by supposing that there is a population of persons released from prison in the United States in any given year.
* Now, let's further suppose that the recidivism rate for people in this population is 67.5% (i.e., the population parameter).
* If we draw simple random samples from this population and estimate the recidivism rate in each sample, we should get estimates that are *close* to the population parameter value.
* Here is an example:

<p align="center">
<img src="/gfiles/fig1.png" width="700px"
alt="figure demonstrating the concept of sampling variation">
</p>

##### Script #1

* estimate a sample proportion when the population proportion, pi, is set to 0.2.
  
```R
set.seed(602)
pi <- 0.2
u <- runif(n=92,min=0,max=1)
y <- ifelse(u>0.2,0,1)
table(y,exclude=NULL)
N <- length(y)
N
1/N*sum(y)
mean(y)
```

* how can we calculate the standard error of this estimate?

```
N <- length(y)
N
p <- mean(y)
p
q <- 1-p
q
std.err <- sqrt(p*q/N)
std.err
```

* How could we obtain a 95% confidence interval for this estimate?
* We *consider* the idea of a sampling distribution of proportions.
* We *assume* that the standardized version of this sampling distribution can be approximated by the standard normal distribution.
* Next, we find the 2.5th and 97.5th percentiles of this normal distribution.

```R
qnorm(p=0.025)
qnorm(p=0.975)
```

* So, the 95% confidence interval for the sample proportion is:

```R
lower.limit <- p-1.96*std.err
lower.limit
upper.limit <- p+1.96*std.err
upper.limit
```

* Does the interval include the true population parameter value of 0.2?
* If so, the confidence interval "trapped" the true population value; if not, it didn't.
* Notice that whether a population parameter value is trapped is a yes or no question.

---
* Interpretation: if this is a valid procedure for calculating a 95% confidence interval, then if we drew many thousands of samples and used this same procedure to calculate the 95% confidence interval for each sample, then 95% of the sample intervals would contain the true population parameter value.

#### Script #2

```R
# standard interval

set.seed(830)

pi <- 0.2

trap <- vector()
pvec <- vector()

for(i in 1:1e4){
  u <- runif(n=92,min=0,max=1)
  y <- ifelse(u>pi,0,1)
  N <- length(y)
  pvec[i] <- mean(y)
  q <- 1-pvec[i]
  std.err <- sqrt(pvec[i]*q/N)
  lower.limit <- pvec[i]-1.96*std.err
  upper.limit <- pvec[i]+1.96*std.err
  trap[i] <- ifelse(lower.limit<=pi & upper.limit>=pi,1,0)
  }

table(trap,exclude=NULL)

# jeffreys prior

set.seed(830)

trap <- vector()
pvec <- vector()

for(i in 1:1e4){
  u <- runif(n=92,min=0,max=1)
  y <- ifelse(u>pi,0,1)
  N <- length(y)
  pvec[i] <- mean(y)
  lower.limit <- qbeta(p=0.025,shape1=1/2+sum(y),shape2=1/2+N-sum(y))
  upper.limit <- qbeta(p=0.975,shape1=1/2+sum(y),shape2=1/2+N-sum(y))
  trap[i] <- ifelse(lower.limit<=pi & upper.limit>=pi,1,0)
  }

table(trap,exclude=NULL)

# clopper-pearson

set.seed(830)

trap <- vector()
pvec <- vector()

for(i in 1:1e4){
  u <- runif(n=92,min=0,max=1)
  y <- ifelse(u>pi,0,1)
  N <- length(y)
  pvec[i] <- mean(y)
  lower.limit <- qbeta(p=0.025,shape1=sum(y),shape2=1+N-sum(y))
  upper.limit <- qbeta(p=0.975,shape1=1+sum(y),shape2=N-sum(y))
  trap[i] <- ifelse(lower.limit<=pi & upper.limit>=pi,1,0)
  }

table(trap,exclude=NULL)

# bootstrap

set.seed(830)

trap <- vector()
pvec <- vector()

for(i in 1:1e4){
  u <- runif(n=92,min=0,max=1)
  y <- ifelse(u>pi,0,1)
  N <- length(y)
  pvec[i] <- mean(y)

  pyb <- vector()

  for(j in 1:3000){
    b <- sample(1:92,size=92,replace=T)
    yb <- y[b]
    pyb[j] <- mean(yb)             
    }

  lower.limit <- quantile(pyb,0.025)
  upper.limit <- quantile(pyb,0.975)
  trap[i] <- ifelse(lower.limit<=pi & upper.limit>=pi,1,0)
  }

table(trap,exclude=NULL)
```

#### Script #3

* test Ho that probability of two coins flipping heads is equal.
* conduct test at the 92% confidence level.
* use Jeffreys prior procedure
  
```R
# coin 1

h1 <- 12
f1 <- 22
h1/f1

# coin 2

h2 <- 15
f2 <- 31
h2/f2

# build confidence interval

r1 <- rbeta(n=1e5,shape1=1/2+h1,shape2=1/2+f1-h1)
r2 <- rbeta(n=1e5,shape1=1/2+h2,shape2=1/2+f2-h2)
hist(r2-r1)
quantile(r2-r1,c(0.04,0.96))
```

#### Script #4

* Let's check on the validity of this procedure under repeated sampling

```R
set.seed(387)

trap <- vector()

for(i in 1:1e4){
  x1 <- rbinom(n=1,size=22,p=0.5)
  x2 <- rbinom(n=1,size=31,p=0.5)
  r1 <- rbeta(n=3e3,shape1=1/2+x1,shape2=1/2+22-x1)
  r2 <- rbeta(n=3e3,shape1=1/2+x2,shape2=1/2+31-x2)
  d <- r2-r1
  lcl.d <- quantile(d,0.04)
  ucl.d <- quantile(d,0.96)
  trap[i] <- ifelse(lcl.d<=0 & ucl.d>=0,1,0)
  }

table(trap)
```

#### Script #5

* We test the hypothesis that the 2024 homicide rate in Washington DC is equal to the 2024 homicide rate in St. Louis MO -- at the 88% confidence level.
* We will use the Jeffreys prior.

```R
set.seed(414)

# st. louis 2024

h1 <- 150
p1 <- 277294
h1/p1*100000

# washington dc 2024

h2 <- 179
p2 <- 702250
h2/p2*100000

# build confidence interval

r1 <- 100000*rbeta(n=1e5,shape1=1/2+h1,shape2=1/2+p1-h1)
r2 <- 100000*rbeta(n=1e5,shape1=1/2+h2,shape2=1/2+p2-h2)
hist(r2-r1)
quantile(r2-r1,c(0.06,0.94))
```

#### Script #6

* Introduction to the [Minneapolis Domestic Violence Experiment](https://www.jstor.org/stable/pdf/2288920.pdf)

```R
# arrest group

na <- 92
ra <- 10

# informal group

ni <- 108+113
ri <- 21+26

# calculate the failure rates

ra/na
ri/ni

# create a contingency table

t <- c(rep("A",92),rep("I",221))
y <- c(rep("no",92-10),rep("yes",10),rep("no",221-47),rep("yes",47))
table(y,t)

pa <- 10/(82+10)
pa
pi <- 47/(174+47)
pi
pi-pa

# we can also do arithmetic on the contingency table

t <- c(rep("A",92),rep("I",221))
y <- c(rep("no",92-10),rep("yes",10),rep("no",221-47),rep("yes",47))
mt <- table(y,t)
mt

c11 <- mt[1,1]
c11
c12 <- mt[1,2]
c12
c21 <- mt[2,1]
c21
c22 <- mt[2,2]
c22

pa <- c21/(c11+c21)
pa
pi <- c22/(c12+c22)
pi
```

### Lesson 4 - Thursday 9/24/26

* We will return to the script we were working on at the end of our last class.
* Let's begin by creating our dataset.

##### Script #1

```R
t <- c(rep("Arrest",92),rep("Informal",221))
y <- c(rep("no",92-10),rep("yes",10),rep("no",221-47),rep("yes",47))
d <- data.frame(t,y)
head(d,n=10)
tail(d,n=10)
mt <- table(y,t,exclude=NULL)
mt
```

##### Script #2

```R
c11 <- mt[1,1]
c11
c12 <- mt[1,2]
c12
c21 <- mt[2,1]
c21
c22 <- mt[2,2]
c22

pa <- c21/(c11+c21)
pa
pi <- c22/(c12+c22)
pi

delta <- pi-pa
delta
```

##### Script #3

```R
rr.ratio <- pi/pa
rr.ratio

odds.ratio <- (pi/(1-pi))/(pa/(1-pa))
odds.ratio

yulesq <- (odds.ratio-1)/(odds.ratio+1)
yulesq
```

##### Script #4

```R
d$yn <- rep(NA,313)
d$yn[d$y=="yes"] <- 1
d$yn[d$y=="no"] <- 0
table(d$yn,d$y,exclude=NULL)
```

##### Script #5

```R
# linear probability model

lpm <- lm(yn~1+as.factor(t),data=d)
summary(lpm)

ahat <- unname(coef(lpm)[1])
bhat <- unname(coef(lpm)[2])
yhat.informal.lpm <- ahat+bhat
yhat.informal.lpm
yhat.arrest.lpm <- ahat
yhat.arrest.lpm
delta.lpm <- yhat.informal.lpm-yhat.arrest.lpm
delta.lpm
```

##### Script #6

```R
# probit regression model

probit <- glm(yn~1+as.factor(t),data=d,family=binomial(link="probit"))
summary(probit)

ahat <- unname(coef(probit)[1])
bhat <- unname(coef(probit)[2])
yhat.informal.probit <- pnorm(ahat+bhat)
yhat.informal.probit
yhat.arrest.probit <- pnorm(ahat)
yhat.arrest.probit
delta.probit <- yhat.informal.probit-yhat.arrest.probit
delta.probit
```

##### Script #7

```R
# logistic regression model

logistic <- glm(yn~1+as.factor(t),data=d,family=binomial(link="logit"))
summary(logistic)

ahat <- unname(coef(logistic)[1])
bhat <- unname(coef(logistic)[2])
yhat.informal.logistic <- exp(ahat+bhat)/(1+exp(ahat+bhat))
yhat.informal.logistic
yhat.arrest.logistic <- exp(ahat)/(1+exp(ahat))
yhat.arrest.logistic
delta.logistic <- yhat.informal.logistic-yhat.arrest.logistic
delta.logistic
```

##### Script #8

```R
# association statistics

# average treatment effect (ATE)

delta.logistic <- yhat.informal.logistic-yhat.arrest.logistic
delta.logistic

# relative risk ratio (rr)

rr.logistic <- yhat.informal.logistic/yhat.arrest.logistic
rr.logistic

# odds ratio

exp(bhat)
or.num <- yhat.informal.logistic/(1-yhat.informal.logistic)
or.den <- yhat.arrest.logistic/(1-yhat.arrest.logistic)
or <- or.num/or.den
or

# Yules Q

yulesq <- (or-1)/(or+1)
yulesq
```

##### Script #9 (start a new R session for this)

```R
# statistical inference for the average treatment effect

# 92% confidence interval based on normal approximation to the binomial
# this is also called the Wald confidence interval

t <- c(rep("Arrest",92),rep("Informal",221))
y <- c(rep("no",92-10),rep("yes",10),rep("no",221-47),rep("yes",47))
d <- data.frame(t,y)
head(d,n=10)
tail(d,n=10)
mt <- table(y,t,exclude=NULL)
mt

c11 <- mt[1,1]
c11
c12 <- mt[1,2]
c12
c21 <- mt[2,1]
c21
c22 <- mt[2,2]
c22

pa <- c21/(c11+c21)
pa
pi <- c22/(c12+c22)
pi

delta <- pi-pa
delta

na <- mt[1,1]+mt[2,1]
na
ni <- mt[1,2]+mt[2,2]
ni

critical.z <- qnorm(p=0.96,mean=0,sd=1)
critical.z

se.delta.pt1 <- pi*(1-pi)/ni
se.delta.pt2 <- pa*(1-pa)/na
se.delta <- sqrt(se.delta.pt1+se.delta.pt2)
se.delta

lcl92 <- delta-critical.z*se.delta
lcl92
ucl92 <- delta+critical.z*se.delta
ucl92
```

##### Script #10

```R
# statistical inference for the average treatment effect
# using the linear probability model

d$yn <- rep(NA,313)
d$yn[d$y=="yes"] <- 1
d$yn[d$y=="no"] <- 0
table(d$yn,d$y,exclude=NULL)

lpm <- lm(yn~1+as.factor(t),data=d)
summary(lpm)

ahat <- unname(coef(lpm)[1])
bhat <- unname(coef(lpm)[2])
yhat.informal.lpm <- ahat+bhat
yhat.informal.lpm
yhat.arrest.lpm <- ahat
yhat.arrest.lpm
delta.lpm <- yhat.informal.lpm-yhat.arrest.lpm
delta.lpm

delta.lpm <- bhat
delta.lpm

vcov(lpm)
se.delta <- sqrt(vcov(lpm)[2,2])
se.delta

critical.z <- qnorm(p=0.96,mean=0,sd=1)
critical.z

lcl92 <- delta.lpm-critical.z*se.delta
lcl92
ucl92 <- delta.lpm+critical.z*se.delta
ucl92
```

##### Script #11

```R
# statistical inference for the average treatment effect
# using the bootstrap

set.seed(831)


delta.vec <- vector()

for(i in 1:1e4){
  b <- sample(1:313,size=313,replace=T)
  yb <- d$yn[b]
  tb <- d$t[b]
  bf <- data.frame(yb,tb)
  yi <- mean(bf$yb[bf$tb=="Informal"])
  ya <- mean(bf$yb[bf$tb=="Arrest"])
  delta.vec[i] <- yi-ya
  }

quantile(delta.vec,0.04)
quantile(delta.vec,0.96)
mean(delta.vec)
sd(delta.vec)
```

### Lesson 5 - Thursday 10/1/26

* Let's consider the practice dataset (using treatment delivered as the independent variable, rather than treatment assigned).
* The *t* variable represents the treatment as it was actually delivered (as opposed to the treatment as it was randomly assigned).

#### Script #1

```R
t <- c(rep("Arrest",63),rep("Informal",1),rep("Arrest",28),
  rep("Arrest",18),rep("Informal",45),rep("Informal",4),
  rep("Informal",39),rep("Informal",2),rep("Arrest",22),
  rep("Informal",2),rep("Informal",40),rep("Arrest",4),
  rep("Informal",3),rep("Informal",42))
 
y <- c(rep("yes",7),rep("no",56),"no",rep("yes",3),rep("no",25),
     rep("yes",3),rep("no",15),rep("yes",7),rep("no",38),
     rep("yes",2),rep("no",2),rep("yes",8),rep("no",31),"yes","no",
      rep("yes",4),rep("no",18),"yes",rep("no",1),rep("yes",9),
      rep("no",31),"yes",rep("no",3),rep("no",3),rep("yes",11),
      rep("no",31))
 
d <- data.frame(t,y)
mt <- table(d$y,d$t)
mt

d$yn <- rep(NA,313)
d$yn[d$y=="yes"] <- 1
d$yn[d$y=="no"] <- 0
table(d$yn,d$y,exclude=NULL)

# logistic regression model

logistic <- glm(yn~1+as.factor(t),data=d,family=binomial(link="logit"))
summary(logistic)
ahat <- unname(coef(logistic)[1])
bhat <- unname(coef(logistic)[2])
yhat.informal.logistic <- exp(ahat+bhat)/(1+exp(ahat+bhat))
yhat.informal.logistic
yhat.arrest.logistic <- exp(ahat)/(1+exp(ahat))
yhat.arrest.logistic

# association statistics
# average treatment effect (ATE) or classical treatment effect (CTE)

delta.logistic <- yhat.informal.logistic-yhat.arrest.logistic
delta.logistic

# relative risk ratio (rr)

rr.logistic <- yhat.informal.logistic/yhat.arrest.logistic
rr.logistic

# odds ratio

exp(bhat)
or.num <- yhat.informal.logistic/(1-yhat.informal.logistic)
or.den <- yhat.arrest.logistic/(1-yhat.arrest.logistic)
or <- or.num/or.den
or

# Yules Q

yulesq <- (or-1)/(or+1)
yulesq
```

#### Script #2

* Note that what we've done above is calculate point estimates of the conditional failure probabilities (conditional on whether the suspect was actually arrested or not).
* So far, we don't have a measure of uncertainty for any of these point estimates.
* We can address this by calculating a confidence interval for each statistic.
* We will use a 89% confidence interval for this exercise.

```R
# statistical inference for the average treatment effect
# using the bootstrap

set.seed(903)


delta.boot <- vector()
rr.boot <- vector()
or.boot <- vector()
yulesq.boot <- vector()

for(i in 1:1e4){
  b <- sample(1:313,size=313,replace=T)
  yb <- d$yn[b]
  tb <- d$t[b]
  logistic.boot <- glm(yb~1+as.factor(tb),family=binomial(link="logit"))
  ahat.boot <- unname(coef(logistic.boot)[1])
  bhat.boot <- unname(coef(logistic.boot)[2])
  yhat.informal.logistic.boot <- exp(ahat.boot+bhat.boot)/(1+exp(ahat.boot+bhat.boot))
  yhat.arrest.logistic.boot <- exp(ahat.boot)/(1+exp(ahat.boot))
  delta.boot[i] <- yhat.informal.logistic.boot-yhat.arrest.logistic.boot
  rr.boot[i] <- yhat.informal.logistic.boot/yhat.arrest.logistic.boot
  or.boot.num <- yhat.informal.logistic.boot/(1-yhat.informal.logistic.boot)
  or.boot.den <- yhat.arrest.logistic.boot/(1-yhat.arrest.logistic.boot)
  or.boot[i] <- or.boot.num/or.boot.den
  yulesq.boot[i] <- (or.boot[i]-1)/(or.boot[i]+1)
  }

quantile(delta.boot,c(0.055,1-0.055))
quantile(rr.boot,c(0.055,1-0.055))
quantile(or.boot,c(0.055,1-0.055))
quantile(yulesq.boot,c(0.055,1-0.055))
```

#### Script #3

* A problem with the bootstrap in these settings is that the distribution of bootstrap estimates can be quite skewed.
* We are now going to program another procedure that will allow us to address this problem.
* The process we will use is called the *bias-corrected, accelerated* bootstrap.

```
# statistical inference for the average treatment effect
# using the bootstrap based on the boot() library

set.seed(903)
library(boot)

id <- seq(from=1,to=313,by=1)
t <- d$t
yn <- d$yn
id.y <- data.frame(id,t,yn)

tboot <- function(data,i){
  b <- data[i,]
  logistic.boot <- glm(yn~1+as.factor(t),data=b,family=binomial(link="logit"))
  ahat.boot <- unname(coef(logistic.boot)[1])
  bhat.boot <- unname(coef(logistic.boot)[2])
  yhat.informal.logistic.boot <- exp(ahat.boot+bhat.boot)/(1+exp(ahat.boot+bhat.boot))
  yhat.arrest.logistic.boot <- exp(ahat.boot)/(1+exp(ahat.boot))
  delta.boot <- yhat.informal.logistic.boot-yhat.arrest.logistic.boot
  rr.boot <- yhat.informal.logistic.boot/yhat.arrest.logistic.boot
  or.boot.num <- yhat.informal.logistic.boot/(1-yhat.informal.logistic.boot)
  or.boot.den <- yhat.arrest.logistic.boot/(1-yhat.arrest.logistic.boot)
  or.boot <- or.boot.num/or.boot.den
  yulesq.boot <- (or.boot-1)/(or.boot+1)
  return(c(delta.boot,rr.boot,or.boot,yulesq.boot))
}

pdist <- boot(data=id.y,statistic=tboot,R=1e4)
par(mfrow=c(2,2))
hist(pdist$t[,1])
hist(pdist$t[,2])
hist(pdist$t[,3])
hist(pdist$t[,4])

boot.ci(pdist,conf=0.89,type="bca",index=1)
boot.ci(pdist,conf=0.89,type="bca",index=2)
boot.ci(pdist,conf=0.89,type="bca",index=3)
boot.ci(pdist,conf=0.89,type="bca",index=4)
```

#### Script #4

* Here is a new version of the dataset.

```R
id <- 1:313

ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
        rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
        rep(3,40),rep(3,4),rep(3,3),rep(3,42))
 
td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
        rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
        rep(3,40),rep(1,4),rep(2,3),rep(3,42))
 
aggcirc <- c(rep(1,63),rep(1,1),rep(0,28),rep(1,18),rep(1,45),
             rep(1,4),rep(0,39),rep(0,2),rep(1,22),rep(1,2),
             rep(1,40),rep(0,4),rep(0,3),rep(0,42))
 
y <- c(rep(1,7),rep(0,56),rep(0,1),rep(1,3),rep(0,25),rep(1,3),
       rep(0,15),rep(1,7),rep(0,38),rep(1,2),rep(0,2),rep(1,8),
       rep(0,31),rep(1,1),rep(0,1),rep(1,4),rep(0,18),rep(1,1),
       rep(0,1),rep(1,9),rep(0,31),rep(1,1),rep(0,3),rep(0,3),
       rep(1,11),rep(0,31))
 
df <- data.frame(id,ta,td,aggcirc,y)
head(df)
tail(df)
table(y,ta)
```

#### Script #5

* Now, let's consider what is involved in decomposing the two informal groups.
* So, we have 3 groups: (1) arrest; (2) advice; and (3) separate
* ta = treatment as assigned and td = treatment as delivered
* the y's are now coded as 0's (successes) and 1's (failures)
* We are now being asked to estimate the failure rates for each group and the confidence intervals for the 3 possible comparisons.
* The 3 comparisons are: (1) arrest vs. advice; (2) arrest vs. separate; and (3) advice vs. separate.
* aggcirc = 1 if there are aggravating circumstances and 0 if not (see [Berk and Sherman (1988)](https://www.jstor.org/stable/pdf/2288920.pdf) for details).
* We can use logistic regression to do this.
* First, we need to do a global likelihood ratio test; we will conduct our test at the 90% confidence level.

```R
critical.value <- qchisq(p=0.9,df=2)
Mr <- glm(y~1,data=df,family=binomial(link="logit"))
summary(Mr)
logLik(Mr)
Mf <- glm(y~1+as.factor(ta),data=df,family=binomial(link="logit"))
summary(Mf)
logLik(Mf)
test.statistic <- 2*abs(logLik(Mf)-logLik(Mr))
test.statistic
```

* Once we have completed this test, we now turn to the task of estimating the confidence intervals.
* We will focus on the difference statistics here.
* First, we get the point estimates:

```R
# use logistic regression coefficients to calculate logits

logit.arrest <- unname(coef(Mf)[1])
logit.arrest
logit.advice <- unname(coef(Mf)[1])+unname(coef(Mf)[2])
logit.advice
logit.separate <- unname(coef(Mf)[1])+unname(coef(Mf)[3])
logit.separate

# use logits to calculate conditional failure rates

pfail.arrest <- exp(logit.arrest)/(1+exp(logit.arrest))
pfail.arrest
pfail.advice <- exp(logit.advice)/(1+exp(logit.advice))
pfail.advice
pfail.separate <- exp(logit.separate)/(1+exp(logit.separate))
pfail.separate

# point estimates of the difference statistics

pfail.advice-pfail.arrest
pfail.separate-pfail.arrest
pfail.separate-pfail.advice
```

* Now that we have the point estimates, we are ready to calculate our confidence intervals:

```R
set.seed(387)
library(boot)

tboot <- function(data,i){
  b <- data[i,]
  logistic.boot <- glm(y~1+as.factor(ta),data=b,family=binomial(link="logit"))
  int.b <- unname(coef(logistic.boot)[1])
  adv.b <- unname(coef(logistic.boot)[2])
  sep.b <- unname(coef(logistic.boot)[3])
  logit.arr.b <- int.b
  logit.adv.b <- int.b+adv.b
  logit.sep.b <- int.b+sep.b
  yhat.arr.b <- exp(logit.arr.b)/(1+exp(logit.arr.b))
  yhat.adv.b <- exp(logit.adv.b)/(1+exp(logit.adv.b))
  yhat.sep.b <- exp(logit.sep.b)/(1+exp(logit.sep.b))
  delta.adv.arr.b <- yhat.adv.b-yhat.arr.b
  delta.sep.arr.b <- yhat.sep.b-yhat.arr.b
  delta.sep.adv.b <- yhat.sep.b-yhat.adv.b
  return(c(delta.adv.arr.b,delta.sep.arr.b,delta.sep.adv.b))
}

pdist <- boot(data=df,statistic=tboot,R=1e4)
boxplot(pdist$t[,1],pdist$t[,2],pdist$t[,3])

boot.ci(pdist,conf=0.9,type="bca",index=1)
boot.ci(pdist,conf=0.9,type="bca",index=2)
boot.ci(pdist,conf=0.9,type="bca",index=3)
```

#### Supplementary Material on Dummy Variable Coding

```R
id <- 1:313

ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
        rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
        rep(3,40),rep(3,4),rep(3,3),rep(3,42))
 
td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
        rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
        rep(3,40),rep(1,4),rep(2,3),rep(3,42))
 
aggcirc <- c(rep(1,63),rep(1,1),rep(0,28),rep(1,18),rep(1,45),
             rep(1,4),rep(0,39),rep(0,2),rep(1,22),rep(1,2),
             rep(1,40),rep(0,4),rep(0,3),rep(0,42))
 
y <- c(rep(1,7),rep(0,56),rep(0,1),rep(1,3),rep(0,25),rep(1,3),
       rep(0,15),rep(1,7),rep(0,38),rep(1,2),rep(0,2),rep(1,8),
       rep(0,31),rep(1,1),rep(0,1),rep(1,4),rep(0,18),rep(1,1),
       rep(0,1),rep(1,9),rep(0,31),rep(1,1),rep(0,3),rep(0,3),
       rep(1,11),rep(0,31))
 
df <- data.frame(id,ta,td,aggcirc,y)
head(df)
tail(df)
table(y,ta)

df$ta.arr <- rep(NA,313)
df$ta.adv <- rep(NA,313)
df$ta.sep <- rep(NA,313)

df$ta.arr[df$ta==1] <- 1
df$ta.arr[df$ta==2] <- 0
df$ta.arr[df$ta==3] <- 0

df$ta.adv[df$ta==1] <- 0
df$ta.adv[df$ta==2] <- 1
df$ta.adv[df$ta==3] <- 0

df$ta.sep[df$ta==1] <- 0
df$ta.sep[df$ta==2] <- 0
df$ta.sep[df$ta==3] <- 1

head(df)
tail(df)

# using as.factor() coding

Mf.ver1 <- glm(y~1+as.factor(ta),data=df,family=binomial(link="logit"))
summary(Mf.ver1)

# using dummy coding

Mf.ver2 <- glm(y~1+ta.adv+ta.sep,data=df,family=binomial(link="logit"))
summary(Mf.ver2)
```

### Lesson 6 - Thursday 10/8/26

* Let's begin by reading in the Minneapolis dataset again:

#### Script #1

```R
id <- 1:313

ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
        rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
        rep(3,40),rep(3,4),rep(3,3),rep(3,42))
 
td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
        rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
        rep(3,40),rep(1,4),rep(2,3),rep(3,42))
 
aggcirc <- c(rep(1,63),rep(1,1),rep(0,28),rep(1,18),rep(1,45),
             rep(1,4),rep(0,39),rep(0,2),rep(1,22),rep(1,2),
             rep(1,40),rep(0,4),rep(0,3),rep(0,42))
 
y <- c(rep(1,7),rep(0,56),rep(0,1),rep(1,3),rep(0,25),rep(1,3),
       rep(0,15),rep(1,7),rep(0,38),rep(1,2),rep(0,2),rep(1,8),
       rep(0,31),rep(1,1),rep(0,1),rep(1,4),rep(0,18),rep(1,1),
       rep(0,1),rep(1,9),rep(0,31),rep(1,1),rep(0,3),rep(0,3),
       rep(1,11),rep(0,31))
 
df <- data.frame(id,ta,td,aggcirc,y)
head(df)
tail(df)
table(y,ta)
```

* Next, let's create 2 dummy variables to represent the 3 treatment-as-assigned groups.
* Then, we estimate our logistic regression model:

#### Script #2

```R
df$ta.arr <- rep(NA,313)
df$ta.adv <- rep(NA,313)
df$ta.sep <- rep(NA,313)

df$ta.arr[df$ta==1] <- 1
df$ta.arr[df$ta==2] <- 0
df$ta.arr[df$ta==3] <- 0

df$ta.adv[df$ta==1] <- 0
df$ta.adv[df$ta==2] <- 1
df$ta.adv[df$ta==3] <- 0

df$ta.sep[df$ta==1] <- 0
df$ta.sep[df$ta==2] <- 0
df$ta.sep[df$ta==3] <- 1

# using dummy coding

Mf <- glm(y~1+ta.adv+ta.sep,data=df,family=binomial(link="logit"))
summary(Mf)
logLik(Mf)

Mr <- glm(y~1,data=df,family=binomial(link="logit"))
summary(Mr)
logLik(Mr)

# calculate likelihood ratio test

ts <- 2*abs(logLik(Mf)-logLik(Mr))
attributes(ts) <- NULL
ts
cv <- qchisq(p=0.88,df=3-1)
cv
ifelse(ts>cv,"reject","fail to reject")
```

* Next, we calculate our point estimates of p(fail|treatment as assigned)

#### Script #3

```R
# use logistic regression coefficients to calculate logits

logit.arrest <- unname(coef(Mf)[1])
logit.arrest
logit.advice <- unname(coef(Mf)[1])+unname(coef(Mf)[2])
logit.advice
logit.separate <- unname(coef(Mf)[1])+unname(coef(Mf)[3])
logit.separate

# use logits to calculate conditional failure rates

pfail.arrest <- exp(logit.arrest)/(1+exp(logit.arrest))
pfail.arrest
pfail.advice <- exp(logit.advice)/(1+exp(logit.advice))
pfail.advice
pfail.separate <- exp(logit.separate)/(1+exp(logit.separate))
pfail.separate

# point estimates of the difference statistics

pfail.advice-pfail.arrest
pfail.separate-pfail.arrest
pfail.separate-pfail.advice
```

* We calculate the likelihood ratio test using the 88% confidence level.
* And, we use the bootstrap to calculate the 88% confidence intervals around each of the 3 difference statistics.

#### Script #4

```R
set.seed(403)
library(boot)

tboot <- function(data,i){
  b <- data[i,]
  Mb <- glm(y~1+ta.adv+ta.sep,data=b,family=binomial(link="logit"))
  int.b <- unname(coef(Mb)[1])
  adv.b <- unname(coef(Mb)[2])
  sep.b <- unname(coef(Mb)[3])
  logit.arr.b <- int.b
  logit.adv.b <- int.b+adv.b
  logit.sep.b <- int.b+sep.b
  yhat.arr.b <- exp(logit.arr.b)/(1+exp(logit.arr.b))
  yhat.adv.b <- exp(logit.adv.b)/(1+exp(logit.adv.b))
  yhat.sep.b <- exp(logit.sep.b)/(1+exp(logit.sep.b))
  delta.adv.arr.b <- yhat.adv.b-yhat.arr.b
  delta.sep.arr.b <- yhat.sep.b-yhat.arr.b
  delta.sep.adv.b <- yhat.sep.b-yhat.adv.b
  return(c(delta.adv.arr.b,delta.sep.arr.b,delta.sep.adv.b))
}

pdist <- boot(data=df,statistic=tboot,R=1e4)
boot.ci(pdist,conf=0.88,type="bca",index=1)
boot.ci(pdist,conf=0.88,type="bca",index=2)
boot.ci(pdist,conf=0.88,type="bca",index=3)
```

* Now, we consider another approach discussed in detail in [Gary King's paper](https://www.jstor.org/stable/2669316).
  
#### Script #5

```R
B <- coef(Mf)
B
V <- vcov(Mf)
V

# simulate coefficients based on the model

library(MASS)
sb <- mvrnorm(n=1e4,mu=B,Sigma=V)

# estimated failure rate distribution for arrest group

logit.arr <- as.numeric(B[1]+B[2]*0+B[3]*0)
logit.arr
exp(logit.arr)/(1+exp(logit.arr))
sim.logit.arr <- sb[,1]+sb[,2]*0+sb[,3]*0
p.sim.arr <- exp(sim.logit.arr)/(1+exp(sim.logit.arr))

# estimated failure rate distribution for advice group

logit.adv <- as.numeric(B[1]+B[2]*1+B[3]*0)
logit.adv
exp(logit.adv)/(1+exp(logit.adv))
sim.logit.adv <- sb[,1]+sb[,2]*1+sb[,3]*0
p.sim.adv <- exp(sim.logit.adv)/(1+exp(sim.logit.adv))

# estimated failure rate distribution for separate group

logit.sep <- as.numeric(B[1]+B[2]*0+B[3]*1)
logit.sep
exp(logit.sep)/(1+exp(logit.sep))
sim.logit.sep <- sb[,1]+sb[,2]*0+sb[,3]*1
p.sim.sep <- exp(sim.logit.sep)/(1+exp(sim.logit.sep))

# now lets look at the confidence intervals for each of
# the estimated difference statistics

quantile(p.sim.adv-p.sim.arr,c(0.06,0.94))
quantile(p.sim.sep-p.sim.arr,c(0.06,0.94))
quantile(p.sim.sep-p.sim.adv,c(0.06,0.94))
```

* Now, we will complicate the model by adding the control variable for aggravating circumstances to the specification.
* Let's begin by starting a new R session and reading in the dataset again.
* We will use a 82% confidence level for our likelihood ratio test.

#### Script #6

```R
id <- 1:313

ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
        rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
        rep(3,40),rep(3,4),rep(3,3),rep(3,42))
 
td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
        rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
        rep(3,40),rep(1,4),rep(2,3),rep(3,42))
 
aggcirc <- c(rep(1,63),rep(1,1),rep(0,28),rep(1,18),rep(1,45),
             rep(1,4),rep(0,39),rep(0,2),rep(1,22),rep(1,2),
             rep(1,40),rep(0,4),rep(0,3),rep(0,42))
 
y <- c(rep(1,7),rep(0,56),rep(0,1),rep(1,3),rep(0,25),rep(1,3),
       rep(0,15),rep(1,7),rep(0,38),rep(1,2),rep(0,2),rep(1,8),
       rep(0,31),rep(1,1),rep(0,1),rep(1,4),rep(0,18),rep(1,1),
       rep(0,1),rep(1,9),rep(0,31),rep(1,1),rep(0,3),rep(0,3),
       rep(1,11),rep(0,31))
 
df <- data.frame(id,ta,td,aggcirc,y)
head(df)
tail(df)

df$ta.arr <- rep(NA,313)
df$ta.adv <- rep(NA,313)
df$ta.sep <- rep(NA,313)

df$ta.arr[df$ta==1] <- 1
df$ta.arr[df$ta==2] <- 0
df$ta.arr[df$ta==3] <- 0

df$ta.adv[df$ta==1] <- 0
df$ta.adv[df$ta==2] <- 1
df$ta.adv[df$ta==3] <- 0

df$ta.sep[df$ta==1] <- 0
df$ta.sep[df$ta==2] <- 0
df$ta.sep[df$ta==3] <- 1

table(y,ta,aggcirc)

# full model

Mf <- glm(y~1+ta.adv+ta.sep+aggcirc,data=df,family=binomial(link="logit"))
summary(Mf)
logLik(Mf)

# reduced model

Mr <- glm(y~1,data=df,family=binomial(link="logit"))
summary(Mr)
logLik(Mr)

# calculate likelihood ratio test

ts <- 2*abs(logLik(Mf)-logLik(Mr))
attributes(ts) <- NULL
ts
cv <- qchisq(p=0.82,df=4-1)
cv
ifelse(ts>cv,"reject","fail to reject")
```

* With these results in hand, we can now calculate the estimated confidence intervals for our same difference statistics.
* The difference between these results and our previous results is that these confidence intervals will take into consideration that we've held aggravating circumstances constant (for this example, we will hold aggcirc constant at the zero level).
  
#### Script #7

```R
B <- coef(Mf)
B
V <- vcov(Mf)
V

# simulate coefficients based on the model

library(MASS)
sb <- mvrnorm(n=1e4,mu=B,Sigma=V)

# estimated failure rate distribution for arrest group

logit.arr <- as.numeric(B[1]+B[2]*0+B[3]*0+B[4]*0)
logit.arr
exp(logit.arr)/(1+exp(logit.arr))
sim.logit.arr <- sb[,1]+sb[,2]*0+sb[,3]*0+sb[,4]*0
p.sim.arr <- exp(sim.logit.arr)/(1+exp(sim.logit.arr))

# estimated failure rate distribution for advice group

logit.adv <- as.numeric(B[1]+B[2]*1+B[3]*0+B[4]*0)
logit.adv
exp(logit.adv)/(1+exp(logit.adv))
sim.logit.adv <- sb[,1]+sb[,2]*1+sb[,3]*0+sb[,4]*0
p.sim.adv <- exp(sim.logit.adv)/(1+exp(sim.logit.adv))

# estimated failure rate distribution for separate group

logit.sep <- as.numeric(B[1]+B[2]*0+B[3]*1+B[4]*0)
logit.sep
exp(logit.sep)/(1+exp(logit.sep))
sim.logit.sep <- sb[,1]+sb[,2]*0+sb[,3]*1+sb[,4]*0
p.sim.sep <- exp(sim.logit.sep)/(1+exp(sim.logit.sep))

# now lets look at the confidence intervals for each of
# the estimated difference statistics

quantile(p.sim.adv-p.sim.arr,c(0.09,0.91))
quantile(p.sim.sep-p.sim.arr,c(0.09,0.91))
quantile(p.sim.sep-p.sim.adv,c(0.09,0.91))
```

* Then, we could do the same set of calculations setting aggcirc equal to 1.

```R
B <- coef(Mf)
B
V <- vcov(Mf)
V

# simulate coefficients based on the model

library(MASS)
sb <- mvrnorm(n=1e4,mu=B,Sigma=V)

# estimated failure rate distribution for arrest group

logit.arr <- as.numeric(B[1]+B[2]*0+B[3]*0+B[4]*1)
logit.arr
exp(logit.arr)/(1+exp(logit.arr))
sim.logit.arr <- sb[,1]+sb[,2]*0+sb[,3]*0+sb[,4]*1
p.sim.arr <- exp(sim.logit.arr)/(1+exp(sim.logit.arr))

# estimated failure rate distribution for advice group

logit.adv <- as.numeric(B[1]+B[2]*1+B[3]*0+B[4]*1)
logit.adv
exp(logit.adv)/(1+exp(logit.adv))
sim.logit.adv <- sb[,1]+sb[,2]*1+sb[,3]*0+sb[,4]*1
p.sim.adv <- exp(sim.logit.adv)/(1+exp(sim.logit.adv))

# estimated failure rate distribution for separate group

logit.sep <- as.numeric(B[1]+B[2]*0+B[3]*1+B[4]*1)
logit.sep
exp(logit.sep)/(1+exp(logit.sep))
sim.logit.sep <- sb[,1]+sb[,2]*0+sb[,3]*1+sb[,4]*1
p.sim.sep <- exp(sim.logit.sep)/(1+exp(sim.logit.sep))

# now lets look at the confidence intervals for each of
# the estimated difference statistics

quantile(p.sim.adv-p.sim.arr,c(0.09,0.91))
quantile(p.sim.sep-p.sim.arr,c(0.09,0.91))
quantile(p.sim.sep-p.sim.adv,c(0.09,0.91))
```

---

#### Practice Questions For Mid-Term Exam

1. The dataset below reports the time served in prison (in months) at the time of release among people who left prison in NC in 1980. This dataset is a population.

```R
set.seed(821)

ts80 <- c(rep(0,102),rep(1,298),rep(2,596),rep(3,641),rep(4,784),rep(5,700),
  rep(6,637),rep(7,402),rep(8,357),rep(9,293),rep(10,259),rep(11,218),rep(12,297),
  rep(13,187),rep(14,191),rep(15,175),rep(16,142),rep(17,150),rep(18,130),
  rep(19,116),rep(20,133),rep(21,112),rep(22,99),rep(23,103),rep(24,102),
  rep(25,97),rep(26,78),rep(27,81),rep(28,76),rep(29,93),rep(30,88),rep(31,82),
  rep(32,74),rep(33,71),rep(34,59),rep(35,67),rep(36,79),rep(37,58),rep(38,50),
  rep(39,40),rep(40,50),rep(41,42),rep(42,37),rep(43,37),rep(44,43),rep(45,33),
  rep(46,40),rep(47,40),rep(48,38),rep(49,26),rep(50,25),rep(51,36),rep(52,24),
  rep(53,31),rep(54,31),rep(55,26),rep(56,23),rep(57,30),rep(58,28),rep(59,17),
  rep(60,19),rep(61,33),rep(62,18),rep(63,15),rep(64,16),rep(65,15),rep(66,12),
  rep(67,10),rep(68,22),rep(69,11),rep(70,10),rep(71,24),rep(72,17),rep(73,13),
  rep(74,14),rep(75,12),rep(76,6),rep(77,11),rep(78,10),rep(79,12),rep(80,10),
  rep(81,15),rep(82,10),rep(83,10),rep(84,7),rep(85,4),rep(86,3),rep(87,6),
  rep(88,7),rep(89,5),rep(90,10),rep(91,7),rep(92,5),rep(93,4),rep(94,2),rep(95,3),
  rep(96,9),rep(97,4),rep(98,7),99,100,rep(101,4),102,rep(103,4),
  rep(104,4),105,rep(107,5),rep(108,8),rep(109,3),rep(110,3),
  rep(111,2),rep(112,7),113,rep(114,3),115,rep(116,8),rep(118,3),119,
  rep(120,2),rep(121,4),122,rep(123,2),rep(124,4),125,126,127,127,
  rep(128,2),129,130,130,rep(132,3),133,134,136,140,rep(141,3),143,144,145,
  146,147,148,rep(149,2),150,152,153,153,156,156,157,158,158,159,166,167,168,
  170,170,172,175,176,rep(177,4),180,182,182,183,183,191,194,197,207,216,225,
  226,241,253,256,264,266,267,268,271,273,287,324,354)
```

* calculate the population size and the population mean and median time served
* draw a simple random sample of size N = 300 from this population
* calculate the sample mean and the sample median
* estimate a 85% confidence interval for each sample statistic
* for each confidence interval address whether the true population parameter value was trapped
* for either the mean or the median, design and implement a simulation study to evaluate the rate at which your confidence interval procedure traps the true population parameter value. 
* what do you conclude about the coverage rate of your confidence interval procedure (the mean or the median)
* design and implement a procedure for using your sample data to test the hypothesis that the mean and median are equal to each other in the population (conduct your test at the 85% confidence level); what do you conclude?

2. The data below come from the FBI's Uniform Crime Reports for 2018.

| City    | # of Murders | Population |
| :---------- |---:|---:|
| Philadelphia  | 351  |  1,586,916  |
| Pittsburgh  | 57  |  302,544  |

* use 821 as a random number seed
* calculate a 87% confidence interval for the murder rate in Philadelphia
* calculate a 87% confidence interval for the murder rate in Pittsburgh
* test the hypothesis that the 2 murder rates are equal to each other (use a 87% confidence level for your test)
* develop and implement a simulation study to measure the coverage rate for the confidence interval you used for your test
 
3. Using the Minneapolis dataset with treatment as delivered.

* use 821 as a random number seed
* estimate a probit regression model examining the failure rates for each of the 3 treatment groups.
* estimate an intercept-only probit model on the Minneapolis treatment-as-delivered data
* conduct a likelihood ratio test of the hypothesis that all 3 groups have the same failure rate (75% confidence level)
* with the probit model you estimated, calculate the difference statistics for each of the 3 groups.
* calculate individual tests of whether each difference is equal to zero for each pair of group comparisons (75% confidence level)
* estimate a new probit model controlling for aggravating circumstances
* conduct a likelihood ratio test of the hypothesis that all 3 groups have the same failure rate *and* that there is no effect of aggravating circumstances; conduct your test at the 75% confidence level.
* based on your new model, calculate individual tests of whether each difference is equal to zero at each level of the aggravating circumstances variable; conduct your tests at the 75% confidence level.

#### Addendum

```R
set.seed(404)

ss <- sample(1:9549,size=300,replace=T)
yss <- ts80[ss]

# bootstrap - percentile method

delta <- vector()
mnvec <- vector()
mdvec <- vector()

for(i in 1:1e4){
  b <- sample(1:300,size=300,replace=T)
  yb <- yss[b]
  mnvec[i] <- mean(yb)
  mdvec[i] <- median(yb)
  delta[i] <- mnvec[i]-mdvec[i]
  }

quantile(delta,c(0.075,0.925))

# using the boot library

library(boot)

id <- seq(from=1,to=300,by=1)
df <- data.frame(id,yss)

tboot <- function(data,i){
  b <- data[i,]
  deltab <- mean(b$y)-median(b$y)
  return(deltab)
}

pdist <- boot(data=df,statistic=tboot,R=1e4)
boot.ci(pdist,conf=0.85,type="bca",index=1)
```

#### Practice Problem Solutions

1. The dataset below reports the time served in prison (in months) at the time of release among people who left prison in NC in 1980. This dataset is a population.

```R
set.seed(821)

ts80 <- c(rep(0,102),rep(1,298),rep(2,596),rep(3,641),rep(4,784),rep(5,700),
  rep(6,637),rep(7,402),rep(8,357),rep(9,293),rep(10,259),rep(11,218),rep(12,297),
  rep(13,187),rep(14,191),rep(15,175),rep(16,142),rep(17,150),rep(18,130),
  rep(19,116),rep(20,133),rep(21,112),rep(22,99),rep(23,103),rep(24,102),
  rep(25,97),rep(26,78),rep(27,81),rep(28,76),rep(29,93),rep(30,88),rep(31,82),
  rep(32,74),rep(33,71),rep(34,59),rep(35,67),rep(36,79),rep(37,58),rep(38,50),
  rep(39,40),rep(40,50),rep(41,42),rep(42,37),rep(43,37),rep(44,43),rep(45,33),
  rep(46,40),rep(47,40),rep(48,38),rep(49,26),rep(50,25),rep(51,36),rep(52,24),
  rep(53,31),rep(54,31),rep(55,26),rep(56,23),rep(57,30),rep(58,28),rep(59,17),
  rep(60,19),rep(61,33),rep(62,18),rep(63,15),rep(64,16),rep(65,15),rep(66,12),
  rep(67,10),rep(68,22),rep(69,11),rep(70,10),rep(71,24),rep(72,17),rep(73,13),
  rep(74,14),rep(75,12),rep(76,6),rep(77,11),rep(78,10),rep(79,12),rep(80,10),
  rep(81,15),rep(82,10),rep(83,10),rep(84,7),rep(85,4),rep(86,3),rep(87,6),
  rep(88,7),rep(89,5),rep(90,10),rep(91,7),rep(92,5),rep(93,4),rep(94,2),rep(95,3),
  rep(96,9),rep(97,4),rep(98,7),99,100,rep(101,4),102,rep(103,4),
  rep(104,4),105,rep(107,5),rep(108,8),rep(109,3),rep(110,3),
  rep(111,2),rep(112,7),113,rep(114,3),115,rep(116,8),rep(118,3),119,
  rep(120,2),rep(121,4),122,rep(123,2),rep(124,4),125,126,127,127,
  rep(128,2),129,130,130,rep(132,3),133,134,136,140,rep(141,3),143,144,145,
  146,147,148,rep(149,2),150,152,153,153,156,156,157,158,158,159,166,167,168,
  170,170,172,175,176,rep(177,4),180,182,182,183,183,191,194,197,207,216,225,
  226,241,253,256,264,266,267,268,271,273,287,324,354)
```

* calculate the population size and the population mean and median time served

```Rout
> length(ts80)
[1] 9549
>
> mean(ts80)
[1] 18.71442
> median(ts80)
[1] 9
>
```

answer: there are 9,549 cases in the population; the population mean time served was 18.714 months and the population median was 9 months.

* draw a simple random sample of size N = 300 from this population

```Rout
> s <- sample(1:9549,size=300,replace=T)
> ts80.s <- ts80[s]
> ts80.s
  [1]   4  28   7  46   3   8   6  14  70  27  43   7   5   3   4
 [16]  20  46  11  58  35   2  44   9   8   4   1  29   5  20   4
 [31]   6  49   3   4  10   4  15  13  15   9   4   5   1   5   6
 [46]  16   0  22  38   6   3   4  16   9  50   8   6   6   2  20
 [61]   5   2 101  23  79   1   6   8  63   6   2   5   5  30  37
 [76]  14  31   5  14  31  14   0   4  48  21   1   5  12  40  10
 [91]  17  52  12   5  44  32  26   3   3  20  30  31  29   5   1
[106]  19   8  38  14   7  42  14  15   3   4  15   4   9   7  15
[121]  27  39  76  14   5   2   5  12  49  54   7  17  32   3   2
[136]   4  10  48   4  59   4   2 136   4  59   5   1   3   2   2
[151]  11  40  28   5  97  17   2  13  81   4   5  41  19  24   6
[166]  11   3  10  17   4  23  17  10   9   5  68  15   6  16   1
[181]   9  28   4   4   5  20  37   4  33  25   2   9  32   7  29
[196]   8  14   6   5   6  14   2   1  17  10  24   7   3  10   4
[211]  53   6  19   3   4  12  12   8   6   4   2  32  16  22 225
[226]  57  41   5  46   2   4  40  45   3  16  65   7  29   6   6
[241]   2   8  40   5  13   4   4   5  40   2  10  40  11  12  12
[256]   5   2   2  39   3   2  28  24   4   9  93   7  38  11  36
[271]  11   8  37  17   9   1   8   7  20   4  24  11  22   7  51
[286]  70  19  25   7  21  16  12   4  26   4   3  16  24   9  21
> 

```
* calculate the sample mean and the sample median

```Rout
> mean(ts80.s)
[1] 18.56
> median(ts80.s)
[1] 10
>
```

solution: the sample mean is 18.56 months and the sample median is 10 months.

* estimate a 85% confidence interval for each sample statistic

solution part 1: first we calculate the confidence interval for the sample mean (please note: I corrected this on the evening of 10/12/26; the error was setting p = 0.935 instead of the correct value of 0.925; I'm sorry about this mistake!).

```Rout
> ct <- qt(p=0.925,df=300-1)
> ct
[1] 1.443239
> 
> lcl <- mean(ts80.s)-ct*sd(ts80.s)/sqrt(300)
> lcl
[1] 16.6344
>
> ucl <- mean(ts80.s)+ct*sd(ts80.s)/sqrt(300)
> ucl
[1] 20.4856
> 
```
so, the 85% confidence interval is [16.634,20.486] (in months).

solution part 2: now, the sample median

```Rout
> library(DescTools)
> ci <- MedianCI(ts80.s,conf.level=0.85,method="boot")
> ci
median lwr.ci upr.ci 
    10      9     12 
> 
```

and, the 85% confidence interval for the sample median is [9,12] (in months).


* for each confidence interval address whether the true population parameter value was trapped

solution: the answer is "yes" in both cases; the population mean is 18.714 months which is inside the [16.534,20.586] interval and the population median is 9 months which is inside the [9,12] interval. *note*: remember that if the population parameter value is equal to either the lower or upper bound of the confidence interval, the confidence interval has successfully trapped that parameter value.

* for either the mean or the median, design and implement a simulation study to evaluate the rate at which your confidence interval procedure traps the true population parameter value.

```Rout
> library(DescTools)
> 
> trap <- vector()
> 
> for(i in 1:3e3){
+   rs <- sample(1:9549,size=300,replace=T)
+   ts80.rs <- ts80[rs]
+   ci <- MedianCI(ts80.rs,conf.level=0.85,method="exact")
+   lcl <- ci[2]
+   ucl <- ci[3]
+   trap[i] <- ifelse(lcl<=median(ts80) & ucl>=median(ts80),1,0)
+   }
> 
> mean(trap)
[1] 0.9346667
> 
```

* what do you conclude about the coverage rate of your confidence interval procedure (the mean or the median)?

solution: based on my procedure that calculates the so-called *exact* confidence interval for the median, I conclude that my procedure's repeated sampling coverage rate (93.5%) exceeded the *nominal* or "as-advertised" coverage rate of 85%.
  
* design and implement a procedure for using your sample data to test the hypothesis that the mean and median are equal to each other in the population (conduct your test at the 85% confidence level); what do you conclude?

```Rout
> library(boot)
> 
> id <- seq(from=1,to=300,by=1)
> df <- data.frame(id,ts80.s)
> 
> tboot <- function(data,i){
+   b <- data[i,]
+   deltab <- mean(b$ts80.s)-median(b$ts80.s)
+   return(deltab)
+ }
> 
> pdist <- boot(data=df,statistic=tboot,R=1e4)
> boot.ci(pdist,conf=0.85,type="bca",index=1)
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 10000 bootstrap replicates

CALL : 
boot.ci(boot.out = pdist, conf = 0.85, type = "bca", index = 1)

Intervals : 
Level       BCa          
85%   ( 7.147, 10.494 )  
Calculations and Intervals on Original Scale
>
```

solution: my 85% confidence interval for the difference between the sample mean and the sample median is [7.147,10.494] (expressed in months). Since my 85% confidence interval does not include the number zero, I reject Ho that the population mean and population median are equal to each other (at the 85% confidence level).

2. The data below come from the FBI's Uniform Crime Reports for 2018.

| City    | # of Murders | Population |
| :---------- |---:|---:|
| Philadelphia  | 351  |  1,586,916  |
| Pittsburgh  | 57  |  302,544  |

* use 821 as a random number seed
* calculate a 87% confidence interval for the murder rate in Philadelphia
* calculate a 87% confidence interval for the murder rate in Pittsburgh

```Rout
> set.seed(821)
> 
> # philadelphia
> 
> h.phl <- 351
> p.phl <- 1586916
> hr.phl <- (h.phl/p.phl)*100000
> hr.phl
[1] 22.11837
> qbeta(p=0.065,shape1=1/2+h.phl,shape2=1/2+p.phl-h.phl)*100000
[1] 20.38906
> qbeta(p=0.935,shape1=1/2+h.phl,shape2=1/2+p.phl-h.phl)*100000
[1] 23.96494
> 
> # pittsburgh
> 
> h.pit <- 57
> p.pit <- 302544
> hr.pit <- (h.pit/p.pit)*100000
> hr.pit
[1] 18.84023
> qbeta(p=0.065,shape1=1/2+h.pit,shape2=1/2+p.pit-h.pit)*100000
[1] 15.36177
> qbeta(p=0.935,shape1=1/2+h.pit,shape2=1/2+p.pit-h.pit)*100000
[1] 22.93358
>
```

solution: the 87% confidence interval for the Philadelphia homicide rate is [20.389,23.965]; the same interval for Pittsburgh is [15.362,22.934]; all numbers are expressed in terms of the homicide rate per 100,000 population. 

* test the hypothesis that the 2 murder rates are equal to each other (use a 87% confidence level for your test)

```Rout
> r.phl <- 100000*rbeta(n=1e5,shape1=1/2+h.phl,shape2=1/2+p.phl-h.phl)
> r.pit <- 100000*rbeta(n=1e5,shape1=1/2+h.pit,shape2=1/2+p.pit-h.pit)
> quantile(r.pit-r.phl,c(0.065,0.935))
     6.5%     93.5% 
-7.217789  1.119571 
>
```

solution: the evidence from my test is not strong enough to reject the hypothesis that the 2 homicide rates are equal to each other; the 87% confidence interval for the difference between the 2 rates includes zero.

* develop and implement a simulation study to measure the coverage rate for the confidence interval you used for your test (note, this is not the only valid answer you could have come up with to address this question).

```Rout
> pop.parameter <- hr.pit-hr.phl
> pop.parameter
[1] -3.278138
> 
> trap <- vector()
> 
> for(i in 1:1e4){
+   x1 <- rbinom(n=1,size=p.phl,p=h.phl/p.phl)
+   x2 <- rbinom(n=1,size=p.pit,p=h.pit/p.pit)
+   r1 <- rbeta(n=3e3,shape1=1/2+x1,shape2=1/2+p.phl-x1)
+   r2 <- rbeta(n=3e3,shape1=1/2+x2,shape2=1/2+p.pit-x2)
+   d <- 100000*r2-100000*r1
+   lcl.d <- quantile(d,0.065)
+   ucl.d <- quantile(d,0.935)
+   trap[i] <- ifelse(lcl.d<=pop.parameter & ucl.d>=pop.parameter,1,0)
+   }
> 
> table(trap)
trap
   0    1 
1307 8693 
>
```

* Assuming the rates observed in the 2 cities are the population rates, then the trap rate for my procedure is estimated to be 86.9%.

---
3. Using the Minneapolis dataset with treatment as delivered.

* use 821 as a random number seed

```Rout
> set.seed(821)
> 
> id <- 1:313
> 
> ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
+         rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
+         rep(3,40),rep(3,4),rep(3,3),rep(3,42))
>  
> td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
+         rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
+         rep(3,40),rep(1,4),rep(2,3),rep(3,42))
>  
> aggcirc <- c(rep(1,63),rep(1,1),rep(0,28),rep(1,18),rep(1,45),
+              rep(1,4),rep(0,39),rep(0,2),rep(1,22),rep(1,2),
+              rep(1,40),rep(0,4),rep(0,3),rep(0,42))
>  
> y <- c(rep(1,7),rep(0,56),rep(0,1),rep(1,3),rep(0,25),rep(1,3),
+        rep(0,15),rep(1,7),rep(0,38),rep(1,2),rep(0,2),rep(1,8),
+        rep(0,31),rep(1,1),rep(0,1),rep(1,4),rep(0,18),rep(1,1),
+        rep(0,1),rep(1,9),rep(0,31),rep(1,1),rep(0,3),rep(0,3),
+        rep(1,11),rep(0,31))
>  
> df <- data.frame(id,ta,td,aggcirc,y)
> head(df)
  id ta td aggcirc y
1  1  1  1       1 1
2  2  1  1       1 1
3  3  1  1       1 1
4  4  1  1       1 1
5  5  1  1       1 1
6  6  1  1       1 1
> tail(df)
     id ta td aggcirc y
308 308  3  3       0 0
309 309  3  3       0 0
310 310  3  3       0 0
311 311  3  3       0 0
312 312  3  3       0 0
313 313  3  3       0 0
> 
> df$td.arr <- rep(NA,313)
> df$td.adv <- rep(NA,313)
> df$td.sep <- rep(NA,313)
> 
> df$td.arr[df$td==1] <- 1
> df$td.arr[df$td==2] <- 0
> df$td.arr[df$td==3] <- 0
> 
> df$td.adv[df$td==1] <- 0
> df$td.adv[df$td==2] <- 1
> df$td.adv[df$td==3] <- 0
> 
> df$td.sep[df$td==1] <- 0
> df$td.sep[df$td==2] <- 0
> df$td.sep[df$td==3] <- 1
> 
```

* estimate a probit regression model examining the failure rates for each of the 3 treatment groups.

```Rout
> Mf <- glm(y~1+td.adv+td.sep,data=df,family=binomial(link="probit"))
> summary(Mf)

Call:
glm(formula = y ~ 1 + td.adv + td.sep, family = binomial(link = "probit"), 
    data = df)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.1108     0.1359  -8.173 3.01e-16 ***
td.adv        0.1945     0.2063   0.943   0.3457    
td.sep        0.4626     0.1976   2.340   0.0193 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.58  on 310  degrees of freedom
AIC: 297.58

Number of Fisher Scoring iterations: 4

> logLik(Mf)
'log Lik.' -145.7898 (df=3)
>
> a <- coef(Mf)[1]
> badv <- coef(Mf)[2]
> bsep <- coef(Mf)[3]
> 
> est.fail.rate.arr <- pnorm(a+badv*0+bsep*0)
> attributes(est.fail.rate.arr) <- NULL
> est.fail.rate.arr
[1] 0.1333333
> 
> est.fail.rate.adv <- pnorm(a+badv*1+bsep*0)
> attributes(est.fail.rate.adv) <- NULL
> est.fail.rate.adv
[1] 0.1797753
> 
> est.fail.rate.sep <- pnorm(a+badv*0+bsep*1)
> attributes(est.fail.rate.sep) <- NULL
> est.fail.rate.sep
[1] 0.258427
> 
```

solution: the sample estimate of the failure rate for the arrest group was 13.3%; for the advice group, the rate was 18.0%; and for the separate group, it was 25.8%.

* estimate an intercept-only probit model on the Minneapolis treatment-as-delivered data

```Rout
> Mr <- glm(y~1,data=df,family=binomial(link="probit"))
> summary(Mr)

Call:
glm(formula = y ~ 1, family = binomial(link = "probit"), data = df)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.90736    0.08253  -10.99   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 297.08  on 312  degrees of freedom
AIC: 299.08

Number of Fisher Scoring iterations: 4

> logLik(Mr)
'log Lik.' -148.5423 (df=1)
> 
```

* conduct a likelihood ratio test of the hypothesis that all 3 groups have the same failure rate (75% confidence level)

```Rout
> ts <- 2*abs(logLik(Mf)-logLik(Mr))
> attributes(ts) <- NULL
> ts
[1] 5.504925
> cv <- qchisq(p=0.75,df=3-1)
> cv
[1] 2.772589
> ifelse(ts>cv,"reject","fail to reject")
[1] "reject"
```

solution: I reject the hypothesis that all 3 groups have the same failure rate at the 75% confidence level.

* with the probit model you estimated, calculate the difference statistics for each of the 3 groups.

```Rout
> est.fail.rate.adv-est.fail.rate.arr
[1] 0.04644195
> est.fail.rate.sep-est.fail.rate.arr
[1] 0.1250936
> est.fail.rate.sep-est.fail.rate.adv
[1] 0.07865169
>
```

* calculate individual tests of whether each difference is equal to zero for each pair of group comparisons (75% confidence level)

```Rout
> B <- coef(Mf)
> B
(Intercept)      td.adv      td.sep 
 -1.1107716   0.1945498   0.4625690 
> V <- vcov(Mf)
> V
            (Intercept)      td.adv      td.sep
(Intercept)  0.01847035 -0.01847035 -0.01847035
td.adv      -0.01847035  0.04257088  0.01847035
td.sep      -0.01847035  0.01847035  0.03906515
> 
> # simulate coefficients based on the model
> 
> library(MASS)
> sb <- mvrnorm(n=1e4,mu=B,Sigma=V)
> 
> # estimated failure rate distribution for arrest group
> 
> probit.arr <- as.numeric(B[1]+B[2]*0+B[3]*0)
> probit.arr
[1] -1.110772
> pnorm(probit.arr)
[1] 0.1333333
> sim.probit.arr <- sb[,1]+sb[,2]*0+sb[,3]*0
> p.sim.arr <- pnorm(sim.probit.arr)
> 
> # estimated failure rate distribution for advice group
> 
> probit.adv <- as.numeric(B[1]+B[2]*1+B[3]*0)
> probit.adv
[1] -0.9162218
> pnorm(probit.adv)
[1] 0.1797753
> sim.probit.adv <- sb[,1]+sb[,2]*1+sb[,3]*0
> p.sim.adv <- pnorm(sim.probit.adv)
> 
> # estimated failure rate distribution for separate group
> 
> probit.sep <- as.numeric(B[1]+B[2]*0+B[3]*1)
> probit.sep
[1] -0.6482026
> pnorm(probit.sep)
[1] 0.258427
> sim.probit.sep <- sb[,1]+sb[,2]*0+sb[,3]*1
> p.sim.sep <- pnorm(sim.probit.sep)
> 
> # now lets look at the confidence intervals for each of
> # the estimated difference statistics
> 
> quantile(p.sim.adv-p.sim.arr,c(0.125,0.875))
      12.5%       87.5% 
-0.01081096  0.10434692 
> quantile(p.sim.sep-p.sim.arr,c(0.125,0.875))
     12.5%      87.5% 
0.06287278 0.18812269 
> quantile(p.sim.sep-p.sim.adv,c(0.125,0.875))
      12.5%       87.5% 
0.009738684 0.150325842 
> 
```

solution: we fail to reject the Ho of equal rates comparing the advice and arrest groups; for the other 2 comparisons, we reject the Ho of equal failure rates.

* estimate a new probit model controlling for aggravating circumstances:

```Rout
> Mf2 <- glm(y~1+td.adv+td.sep+aggcirc,data=df,family=binomial(link="probit"))
> summary(Mf2)

Call:
glm(formula = y ~ 1 + td.adv + td.sep + aggcirc, family = binomial(link = "probit"), 
    data = df)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.07853    0.18999  -5.677 1.37e-08 ***
td.adv       0.18434    0.21045   0.876   0.3811    
td.sep       0.45147    0.20270   2.227   0.0259 *  
aggcirc     -0.04225    0.17551  -0.241   0.8097    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.52  on 309  degrees of freedom
AIC: 299.52

Number of Fisher Scoring iterations: 4

> logLik(Mf2)
'log Lik.' -145.761 (df=4)
>
```

* conduct a likelihood ratio test of the hypothesis that all 3 groups have the same failure rate *and* that there is no effect of aggravating circumstances; conduct your test at the 75% confidence level.

```Rout
> ts <- 2*abs(logLik(Mf2)-logLik(Mr))
> attributes(ts) <- NULL
> ts
[1] 5.562601
> cv <- qchisq(p=0.75,df=4-1)
> cv
[1] 4.108345
> ifelse(ts>cv,"reject","fail to reject")
[1] "reject"
>
```

solution: we reject Ho that the failure rates are equal *and* there is no effect of aggravating circumstances at the 75% confidence level.

* based on your new model, calculate individual tests of whether each difference is equal to zero at each level of the aggravating circumstances variable; conduct your tests at the 75% confidence level.

```Rout
> # extract coefficients and variance-covariance matrix from model Mf2
> 
> B <- coef(Mf2)
> B
(Intercept)      td.adv      td.sep     aggcirc 
-1.07853155  0.18433829  0.45146936 -0.04225482 
> V <- vcov(Mf2)
> V
            (Intercept)      td.adv      td.sep     aggcirc
(Intercept)  0.03609660 -0.02396162 -0.02443353 -0.02330124
td.adv      -0.02396162  0.04428837  0.02032827  0.00725895
td.sep      -0.02443353  0.02032827  0.04108744  0.00788281
aggcirc     -0.02330124  0.00725895  0.00788281  0.03080394
> 
> # simulate coefficients based on the model
> 
> library(MASS)
> sb <- mvrnorm(n=1e4,mu=B,Sigma=V)
>
```

next, we set aggcirc to zero:

```
> 
> # estimated failure rate distribution for arrest group
> 
> probit.arr <- as.numeric(B[1]+B[2]*0+B[3]*0+B[4]*0)
> probit.arr
[1] -1.078532
> pnorm(probit.arr)
[1] 0.1403983
> sim.probit.arr <- sb[,1]+sb[,2]*0+sb[,3]*0+sb[,4]*0
> p.sim.arr <- pnorm(sim.probit.arr)
> 
> # estimated failure rate distribution for advice group
> 
> probit.adv <- as.numeric(B[1]+B[2]*1+B[3]*0+B[4]*0)
> probit.adv
[1] -0.8941933
> pnorm(probit.adv)
[1] 0.1856092
> sim.probit.adv <- sb[,1]+sb[,2]*1+sb[,3]*0+sb[,4]*0
> p.sim.adv <- pnorm(sim.probit.adv)
> 
> # estimated failure rate distribution for separate group
> 
> probit.sep <- as.numeric(B[1]+B[2]*0+B[3]*1+B[4]*0)
> probit.sep
[1] -0.6270622
> pnorm(probit.sep)
[1] 0.2653092
> sim.probit.sep <- sb[,1]+sb[,2]*0+sb[,3]*1+sb[,4]*0
> p.sim.sep <- pnorm(sim.probit.sep)
> 
> # now lets look at the confidence intervals for each of
> # the estimated difference statistics
> 
> quantile(p.sim.adv-p.sim.arr,c(0.125,0.875))
      12.5%       87.5% 
-0.01469973  0.10511993 
> quantile(p.sim.sep-p.sim.arr,c(0.125,0.875))
     12.5%      87.5% 
0.05887434 0.18677782 
> quantile(p.sim.sep-p.sim.adv,c(0.125,0.875))
      12.5%       87.5% 
0.006765678 0.151561027 
>
```

and then we set aggcirc = 1

```Rout
> # estimated failure rate distribution for arrest group
> 
> probit.arr <- as.numeric(B[1]+B[2]*0+B[3]*0+B[4]*1)
> probit.arr
[1] -1.120786
> pnorm(probit.arr)
[1] 0.1311894
> sim.probit.arr <- sb[,1]+sb[,2]*0+sb[,3]*0+sb[,4]*1
> p.sim.arr <- pnorm(sim.probit.arr)
> 
> # estimated failure rate distribution for advice group
> 
> probit.adv <- as.numeric(B[1]+B[2]*1+B[3]*0+B[4]*1)
> probit.adv
[1] -0.9364481
> pnorm(probit.adv)
[1] 0.1745213
> sim.probit.adv <- sb[,1]+sb[,2]*1+sb[,3]*0+sb[,4]*1
> p.sim.adv <- pnorm(sim.probit.adv)
> 
> # estimated failure rate distribution for separate group
> 
> probit.sep <- as.numeric(B[1]+B[2]*0+B[3]*1+B[4]*1)
> probit.sep
[1] -0.669317
> pnorm(probit.sep)
[1] 0.2516466
> sim.probit.sep <- sb[,1]+sb[,2]*0+sb[,3]*1+sb[,4]*1
> p.sim.sep <- pnorm(sim.probit.sep)
> 
> # now lets look at the confidence intervals for each of
> # the estimated difference statistics
> 
> quantile(p.sim.adv-p.sim.arr,c(0.125,0.875))
      12.5%       87.5% 
-0.01341187  0.10494374 
> quantile(p.sim.sep-p.sim.arr,c(0.125,0.875))
     12.5%      87.5% 
0.05445075 0.18682891 
> quantile(p.sim.sep-p.sim.adv,c(0.125,0.875))
      12.5%       87.5% 
0.006306098 0.146920529 
>
```

solution: regardless of the presence or absence of aggravating circumstances, our hypothesis tests remain the same: we cannot reject the Ho of equal failure rates between the advice and arrest groups; on the other hand, we do reject the hypothesis of equal failure rates for the other 2 comparisons.
*Note*: please check your email for additional updates: #1 at 12:24pm on 10/13/26; #2 at 2:35pm on 10/13/26.

### Lesson 7 - Thursday 10/22/26

* Final exam material begins with today's lesson.
* Here is the Minneapolis dataset:

```R
id <- 1:313

ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
        rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
        rep(3,40),rep(3,4),rep(3,3),rep(3,42))
 
td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
        rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
        rep(3,40),rep(1,4),rep(2,3),rep(3,42))
 
aggcirc <- c(rep(1,63),rep(1,1),rep(0,28),rep(1,18),rep(1,45),
             rep(1,4),rep(0,39),rep(0,2),rep(1,22),rep(1,2),
             rep(1,40),rep(0,4),rep(0,3),rep(0,42))
 
y <- c(rep(1,7),rep(0,56),rep(0,1),rep(1,3),rep(0,25),rep(1,3),
       rep(0,15),rep(1,7),rep(0,38),rep(1,2),rep(0,2),rep(1,8),
       rep(0,31),rep(1,1),rep(0,1),rep(1,4),rep(0,18),rep(1,1),
       rep(0,1),rep(1,9),rep(0,31),rep(1,1),rep(0,3),rep(0,3),
       rep(1,11),rep(0,31))
 
df <- data.frame(id,ta,td,aggcirc,y)
head(df)
tail(df)
table(df$y,df$ta,df$aggcirc,exclude=NULL)
```

* Now, let's use the information in the table to calculate some conditional probabilities.

```R
# p(fail | arrest, no aggravating circumstances)

3/(25+3)

# p(fail | advice, no aggravating circumstances)

9/(32+9)

# p(fail | separate, no aggravating circumstances)

12/(37+12)

# p(fail | arrest, aggravating circumstances)

7/(57+7)

# p(fail | advice, aggravating circumstances)

12/(55+12)

# p(fail | separate, aggravating circumstances)

14/(50+14)
```

* Can we recover these same numbers from a logistic regression analysis?

```R
df$arr <- rep(NA,nrow(df))
df$arr[df$ta==2 | df$ta==3] <- 0
df$arr[df$ta==1] <- 1

df$adv <- rep(NA,nrow(df))
df$adv[df$ta==1 | df$ta==3] <- 0
df$adv[df$ta==2] <- 1

df$sep <- rep(NA,nrow(df))
df$sep[df$ta==1 | df$ta==2] <- 0
df$sep[df$ta==3] <- 1

M <- glm(y~1+adv+sep+aggcirc,data=df,family=binomial(link="logit"))
summary(M)

# p(fail | arrest, no aggravating circumstances)

logit <- -1.9970+0.6712*0+0.8765*0-0.1569*0
exp(logit)/(1+exp(logit))

# p(fail | advice, no aggravating circumstances)

logit <- -1.9970+0.6712*1+0.8765*0-0.1569*0
exp(logit)/(1+exp(logit))

# p(fail | separate, no aggravating circumstances)

logit <- -1.9970+0.6712*0+0.8765*1-0.1569*0
exp(logit)/(1+exp(logit))

# p(fail | arrest, aggravating circumstances)

logit <- -1.9970+0.6712*0+0.8765*0-0.1569*1
exp(logit)/(1+exp(logit))

# p(fail | advice, aggravating circumstances)

logit <- -1.9970+0.6712*1+0.8765*0-0.1569*1
exp(logit)/(1+exp(logit))

# p(fail | separate, aggravating circumstances)

logit <- -1.9970+0.6712*0+0.8765*1-0.1569*1
exp(logit)/(1+exp(logit))
```

| Group    | p(fail) table | p(fail) model |
| -------- | -------: | ------:|
| arrest, no aggravating circumstances  | 0.107 | 0.120 |
| advice, no aggravating circumstances  | 0.220 | 0.210 |
| separate, no aggravating circumstances  | 0.245 | 0.246 |
| arrest, aggravating circumstances  | 0.109 | 0.104 |
| advice, aggravating circumstances  | 0.179 | 0.185 |
| separate, aggravating circumstances  | 0.219 | 0.218 |

* Let's consider a different model specification, a specification we will call the *saturated* model, Ms.

```R
Ms <- glm(y~1+adv+sep+aggcirc+aggcirc*adv+aggcirc*sep,data=df,family=binomial(link="logit"))
summary(Ms)

bint <- -2.12026
badv <-  0.85175
bsep <-  0.99425
bagg <-  0.02312
badvagg <- -0.27704
bsepagg <- -0.17008

# p(fail | arrest, no aggravating circumstances)

xadv <- 0
xsep <- 0
xagg <- 0
xadvagg <- xagg*xadv
xsepagg <- xagg*xsep

logit <- bint+badv*xadv+
              bsep*xsep+
              bagg*xagg+
              badvagg*xadvagg+
              bsepagg*xsepagg

exp(logit)/(1+exp(logit))

# p(fail | advice, no aggravating circumstances)

xadv <- 1
xsep <- 0
xagg <- 0
xadvagg <- xagg*xadv
xsepagg <- xagg*xsep

logit <- bint+badv*xadv+
              bsep*xsep+
              bagg*xagg+
              badvagg*xadvagg+
              bsepagg*xsepagg

exp(logit)/(1+exp(logit))

# p(fail | separate, no aggravating circumstances)

xadv <- 0
xsep <- 1
xagg <- 0
xadvagg <- xagg*xadv
xsepagg <- xagg*xsep

logit <- bint+badv*xadv+
              bsep*xsep+
              bagg*xagg+
              badvagg*xadvagg+
              bsepagg*xsepagg

exp(logit)/(1+exp(logit))

# p(fail | arrest, aggravating circumstances)

xadv <- 0
xsep <- 0
xagg <- 1
xadvagg <- xagg*xadv
xsepagg <- xagg*xsep

logit <- bint+badv*xadv+
              bsep*xsep+
              bagg*xagg+
              badvagg*xadvagg+
              bsepagg*xsepagg

exp(logit)/(1+exp(logit))

# p(fail | advice, aggravating circumstances)

xadv <- 1
xsep <- 0
xagg <- 1
xadvagg <- xagg*xadv
xsepagg <- xagg*xsep

logit <- bint+badv*xadv+
              bsep*xsep+
              bagg*xagg+
              badvagg*xadvagg+
              bsepagg*xsepagg

exp(logit)/(1+exp(logit))

# p(fail | separate, aggravating circumstances)

xadv <- 0
xsep <- 1
xagg <- 1
xadvagg <- xagg*xadv
xsepagg <- xagg*xsep

logit <- bint+badv*xadv+
              bsep*xsep+
              bagg*xagg+
              badvagg*xadvagg+
              bsepagg*xsepagg

exp(logit)/(1+exp(logit))
```

| Group    | p(fail) table | p(fail) model M | p(fail) model Ms |
| -------- | -------: | ------:|---:|
| arrest, no aggravating circumstances  | 0.107 | 0.120 | 0.107 |
| advice, no aggravating circumstances  | 0.220 | 0.210 | 0.220 |
| separate, no aggravating circumstances  | 0.245 | 0.246 | 0.245 |
| arrest, aggravating circumstances  | 0.109 | 0.104 | 0.109 |
| advice, aggravating circumstances  | 0.179 | 0.185 | 0.179 |
| separate, aggravating circumstances  | 0.219 | 0.218 | 0.219 |
