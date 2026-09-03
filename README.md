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

for(i in 1:30000){
  s <- sample(1:9327,size=300,replace=T)
  sm <- mean(ts[s])
  std.err <- sd(ts[s])/sqrt(300)
  t.mult <- qt(0.91,df=300-1)
  lcl <- sm-t.mult*std.err
  ucl <- sm+t.mult*std.err
  trap[i] <- ifelse(lcl<=19.75458 & ucl>=19.75458,1,0)
  }

table(trap)
mean(trap)
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
> 
> for(i in 1:30000){
+   s <- sample(1:9327,size=300,replace=T)
+   sm <- mean(ts[s])
+   std.err <- sd(ts[s])/sqrt(300)
+   t.mult <- qt(0.91,df=300-1)
+   lcl <- sm-t.mult*std.err
+   ucl <- sm+t.mult*std.err
+   trap[i] <- ifelse(lcl<=19.75458 & ucl>=19.75458,1,0)
+   }
> 
> table(trap)
trap
    0     1 
 5427 24573 
> mean(trap)
[1] 0.8191
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


