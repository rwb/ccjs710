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
