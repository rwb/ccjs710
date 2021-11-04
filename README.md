### CCJS 710-0101 Limited Dependent Variables

* Meeting time: Thursday 4-6:45
* Classroom: LeFrak 2207
* Instructor: Bobby Brame (rbrame@umd.edu)
* Office: LeFrak 2139
* Standing office hours: Thursday 2-3:30 or by appointment.
* Course description: Application of advanced data analysis strategies to criminological and criminal justice problems, with specific focus on limited dependent variables.
* Readings: assigned as we go along.
* Disability Accommodations: If you have or think you might have a disability, injury, or other condition that could affect your class performance, please contact the [Accessibility and Disability Service](https://www.counseling.umd.edu/ads/). I will abide by accommodations recommended by the ADS in this course.
* Academic Integrity Statement: You are expected to practice the highest standards of academic integrity. Any deviation from this expectation will result in a minimum academic penalty of your failing the assignment, and may result in additional disciplinary measures. This includes improper citation of sources, presenting someone else's work as your own, and any other form of academic misrepresentation. Please take a few moments to familarize yourself with the University's expectations for student academic integrity which can be found [here](https://academiccatalog.umd.edu/undergraduate/registration-academic-requirements-regulations/academic-integrity-student-conduct-codes/).
* Grades: Your course grade will be an average of the grades on your weekly assignments. Each assignment will be graded on the traditional 0-100 scale. Letter grades will be assigned according to the following scale:

| Numeric Grade | Letter Grade |
|:--------------|:-------------|
| > 96          | A+           |
| 93-96         | A            |
| 90-92         | A-           |
| 87-89         | B+           |
| 83-86         | B            |
| 80-82         | B-           |
| 77-79         | C+           |
| 73-76         | C            |
| 70-72         | C-           |
| 67-69         | D+           |
| 63-66         | D            |
| 60-62         | D-           |
| < 60          | F            |

#### Course Outline

* 9/2: course overview, maximum likelihood
* 9/9: estimator properties, proportion diffs, relative risk statistics, odds ratios
* 9/16: linear probability, logit, and probit
* 9/23: multinomial logit
* 9/30: counted outcomes
* 10/7: zero-inflation and mixtures
* 10/14: hurdle specifications
* 10/21: censoring
* 10/28: tobit 
* 11/4: selection problem
* 11/11: instrumental variables
* 11/18: partial identification
* 11/25: no class, Thanksgiving Holiday
* 12/2: survival time
* 12/9: split population specifications

### Lesson 1 - Thursday 9/2/21

* Discussion topic: What do we mean by the term "limited dependent variables"?
* Enter some data.

```r
# key in dataset from Berk and Sherman (1988)
# JASA, 83:70-76.

n.treat <- 92
n.fail.treat <- 10

n.control <- 221
n.fail.control <- 47
```

* First inferential goal is to estimate p(fail):

```r
# likelihood function
# p(r|theta, N) = (N choose r) theta^r (1-theta)^(N-r)
# where (N choose r) = N!/[r!(N-r)!]

theta <- seq(from=0,to=1,by=0.001)

# find maximum likelihood estimate of theta 
# for the entire sample

p1.all <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
p2.all <- theta^(n.fail.treat+n.fail.control)
p3.all <- (1-theta)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
all.likelihood <- p1.all*p2.all*p3.all
all.log.likelihood <- log(all.likelihood)
df.all <- data.frame(theta,all.likelihood)
subset(df.all,theta>=0.16 & theta<=0.20)
```
* Here are the results:

```rout
> # likelihood function
> # p(r|theta, N) = (N choose r) theta^r (1-theta)^(N-r)
> # where (N choose r) = N!/[r!(N-r)!]
> 
> theta <- seq(from=0,to=1,by=0.001)
> 
> # find maximum likelihood estimate of theta 
> # for the entire sample
> 
> p1.all <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> p2.all <- theta^(n.fail.treat+n.fail.control)
> p3.all <- (1-theta)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> all.likelihood <- p1.all*p2.all*p3.all
> all.log.likelihood <- log(all.likelihood)
> df.all <- data.frame(theta,all.likelihood)
> subset(df.all,theta>=0.16 & theta<=0.20)
    theta all.likelihood
161 0.160     0.03368063
162 0.161     0.03541448
163 0.162     0.03714228
164 0.163     0.03885569
165 0.164     0.04054621
166 0.165     0.04220525
167 0.166     0.04382419
168 0.167     0.04539449
169 0.168     0.04690774
170 0.169     0.04835575
171 0.170     0.04973064
172 0.171     0.05102488
173 0.172     0.05223139
174 0.173     0.05334359
175 0.174     0.05435546
176 0.175     0.05526161
177 0.176     0.05605730
178 0.177     0.05673851
179 0.178     0.05730194
180 0.179     0.05774507
181 0.180     0.05806613
182 0.181     0.05826417
183 0.182     0.05833897
184 0.183     0.05829113
185 0.184     0.05812199
186 0.185     0.05783361
187 0.186     0.05742875
188 0.187     0.05691086
189 0.188     0.05628398
190 0.189     0.05555273
191 0.190     0.05472226
192 0.191     0.05379815
193 0.192     0.05278643
194 0.193     0.05169344
195 0.194     0.05052580
196 0.195     0.04929038
197 0.196     0.04799417
198 0.197     0.04664428
199 0.198     0.04524786
200 0.199     0.04381203
201 0.200     0.04234385
> 
```

* Now, let's graph the functions (both the likelihood and the log-likelihood functions):

```r
# generate likelihood plots

par(mfrow=c(1,2))

plot(x=theta,y=all.likelihood,type="l",lty=1,lwd=1)
abline(h=seq(from=0,to=0.06,by=0.01),lty=3,lwd=0.5)
abline(v=seq(from=0,to=1,by=0.2),lty=3,lwd=0.5)

plot(x=theta,y=all.log.likelihood,type="l",lty=1,lwd=1)
abline(h=seq(from=-600,to=0,by=100),lty=3,lwd=0.5)
abline(v=seq(from=0,to=1,by=0.2),lty=3,lwd=0.5)
```

<p align="left">
<img src="/gfiles/like-plots.png" width="800px">
</p>

* Notice that both curves reach their peak at the same location.
* The log transformation is monotone (implying order preservation).
* The next issue that arises is verifying that the log likelihood function is flat in the neighborhood of the maximum. We can check this by calculating the first derivative of the log-likelihood function:

```r
# use finite difference approximation to calculate derivative
# of log-likelihood function

# begin by dividing the number of failures by the number of people

57/313

theta1 <- 0.1821087
thetad <- 0.1821086
theta0 <- 0.1821085

pi1a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
pi1b <- theta1^(n.fail.treat+n.fail.control)
pi1c <- (1-theta1)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
pi1 <- pi1a*pi1b*pi1c
logpi1 <- log(pi1)

pida <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
pidb <- thetad^(n.fail.treat+n.fail.control)
pidc <- (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
pid <- pida*pidb*pidc
logpid <- log(pid)

pi0a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
pi0b <- theta0^(n.fail.treat+n.fail.control)
pi0c <- (1-theta0)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
pi0 <- pi0a*pi0b*pi0c
logpi0 <- log(pi0)

# approximate derivative

(logpi1-logpi0)/(theta1-theta0)

# check the result

d <- deriv(~ log(pida*thetad^(n.fail.treat+n.fail.control)*
                 (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))), "thetad")
eval(d)
```

* Here are the results of these calculations:

```rout
> # use finite difference approximation to calculate derivative
> # of log-likelihood function
> 
> # begin by dividing the number of failures by the number of people
> 
> 57/313
[1] 0.1821086
> 
> theta1 <- 0.1821087
> thetad <- 0.1821086
> theta0 <- 0.1821085
> 
> pi1a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> pi1b <- theta1^(n.fail.treat+n.fail.control)
> pi1c <- (1-theta1)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> pi1 <- pi1a*pi1b*pi1c
> logpi1 <- log(pi1)
> 
> pida <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> pidb <- thetad^(n.fail.treat+n.fail.control)
> pidc <- (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> pid <- pida*pidb*pidc
> logpid <- log(pid)
> 
> pi0a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> pi0b <- theta0^(n.fail.treat+n.fail.control)
> pi0c <- (1-theta0)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> pi0 <- pi0a*pi0b*pi0c
> logpi0 <- log(pi0)
> 
> # approximate derivative
> 
> (logpi1-logpi0)/(theta1-theta0)
[1] 5.505374e-05
> 
> # check the result
> 
> d <- deriv(~ log(pida*thetad^(n.fail.treat+n.fail.control)*
+                  (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))), "thetad")
> eval(d)
[1] -2.841473
attr(,"gradient")
           thetad
[1,] 5.505386e-05
> 
```

* Note that we can add a tangent line to the log-likelihood plot space to visually see that the
slope of the tangent line at the maximum of the function is ~0.

```r
# slope for tangent line -- tangent at max[Log(L)] = a + b*thetad

slope.line <- (logpi1-logpi0)/(theta1-theta0)
slope.line

# intercept for line -- a = log(L) - b*thetad

int.line <- -2.841473-slope.line*thetad
int.line

# draw the tangent line through the plotspace

abline(a=int.line,b=slope.line,lty=1,lwd=1,col="blue")
```

* Here are the results:

```r
> # slope for tangent line -- tangent at max[Log(L)] = a + b*thetad
> 
> slope.line <- (logpi1-logpi0)/(theta1-theta0)
> slope.line
[1] 5.505374e-05
> 
> # intercept for line -- a = log(L) - b*thetad
> 
> int.line <- -2.841473-slope.line*thetad
> int.line
[1] -2.841483
> 
> # draw the tangent line through the plotspace
> 
> abline(a=int.line,b=slope.line,lty=1,lwd=1,col="blue")
```

<p align="left">
<img src="/gfiles/likelihood-plot.png" width="800px">
</p>

#### Assignment Due Thursday 9/9/21

* Conduct a parallel analysis using the treatment-as-delivered data from the Minneapolis study. Here are the data you should use:

```r
n.treat <- 135
n.fail.treat <- 18

n.control <- 178
n.fail.control <- 39
```

* Please keep in mind that for this assignment, you will get the same final answers we got in class because the sum of the number of failures and the sum of the total number of cases are the same (all that changed was the treatment groups to which people belong).


### Lesson 2 - Thursday 9/9/21

* We begin today's lesson by keying in the dataset and recovering the information we need from last week:

```r
```r
# key in dataset from Berk and Sherman (1988)
# JASA, 83:70-76.

n.treat <- 92
n.fail.treat <- 10

n.control <- 221
n.fail.control <- 47

# begin by dividing the number of failures by the number of people

57/313

theta1 <- 0.1821087
thetad <- 0.1821086
theta0 <- 0.1821085

pi1a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
pi1b <- theta1^(n.fail.treat+n.fail.control)
pi1c <- (1-theta1)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
pi1 <- pi1a*pi1b*pi1c
logpi1 <- log(pi1)

pida <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
pidb <- thetad^(n.fail.treat+n.fail.control)
pidc <- (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
pid <- pida*pidb*pidc
logpid <- log(pid)

pi0a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
pi0b <- theta0^(n.fail.treat+n.fail.control)
pi0c <- (1-theta0)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
pi0 <- pi0a*pi0b*pi0c
logpi0 <- log(pi0)

# approximate derivative

(logpi1-logpi0)/(theta1-theta0)

# check the result

d <- deriv(~ log(pida*thetad^(n.fail.treat+n.fail.control)*
                 (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))), "thetad")
eval(d)
```

#### 2.1 Likelihood Curvature

* A key issue that arises in maximum likelihood estimation is studying the curvature of the log-likelihood function to obtain the Fisher information which can, in turn, be used to calculate the variances of the maximum likelihood estimate:

```r
# use finite difference approximation to calculate 
# second derivative of log-likelihood function
# this yields the observed Fisher information:

j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
j

# second derivative using centered finite difference
# approximation - negative sign = concave down

(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2

# check the result using the normal approximation
# to the binomial distribution

se.thetad <- 1/j^(1/2)
se.thetad

sqrt(thetad*(1-thetad)/(n.treat+n.control))

thetad-1.96*se.thetad
thetad+1.96*se.thetad

thetad-1.96*sqrt(thetad*(1-thetad)/(n.treat+n.control))
thetad+1.96*sqrt(thetad*(1-thetad)/(n.treat+n.control))
```

* Here are the results:

```rout
> # key in dataset from Berk and Sherman (1988)
> # JASA, 83:70-76.
> 
> n.treat <- 92
> n.fail.treat <- 10
> 
> n.control <- 221
> n.fail.control <- 47
> 
> # begin by dividing the number of failures by the number of people
> 
> 57/313
[1] 0.1821086
> 
> theta1 <- 0.1821087
> thetad <- 0.1821086
> theta0 <- 0.1821085
> 
> pi1a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> pi1b <- theta1^(n.fail.treat+n.fail.control)
> pi1c <- (1-theta1)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> pi1 <- pi1a*pi1b*pi1c
> logpi1 <- log(pi1)
> 
> pida <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> pidb <- thetad^(n.fail.treat+n.fail.control)
> pidc <- (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> pid <- pida*pidb*pidc
> logpid <- log(pid)
> 
> pi0a <- choose(n.treat+n.control,n.fail.treat+n.fail.control)
> pi0b <- theta0^(n.fail.treat+n.fail.control)
> pi0c <- (1-theta0)^((n.treat+n.control)-(n.fail.treat+n.fail.control))
> pi0 <- pi0a*pi0b*pi0c
> logpi0 <- log(pi0)
> 
> # approximate derivative
> 
> (logpi1-logpi0)/(theta1-theta0)
[1] 5.505374e-05
> 
> # check the result
> 
> d <- deriv(~ log(pida*thetad^(n.fail.treat+n.fail.control)*
+                  (1-thetad)^((n.treat+n.control)-(n.fail.treat+n.fail.control))), "thetad")
> eval(d)
[1] -2.841473
attr(,"gradient")
           thetad
[1,] 5.505386e-05
> 
> # use finite difference approximation to calculate 
> # second derivative of log-likelihood function
> # this yields the observed Fisher information:
> 
> j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
> j
[1] 2097.966
> 
> # second derivative using centered finite difference
> # approximation - negative sign = concave down
> 
> (logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
[1] -2097.966
> 
> # check the result using the normal approximation
> # to the binomial distribution
> 
> se.thetad <- 1/j^(1/2)
> se.thetad
[1] 0.02183236
> 
> sqrt(thetad*(1-thetad)/(n.treat+n.control))
[1] 0.02181428
> 
> thetad-1.96*se.thetad
[1] 0.1393172
> thetad+1.96*se.thetad
[1] 0.2249
> 
> thetad-1.96*sqrt(thetad*(1-thetad)/(n.treat+n.control))
[1] 0.1393526
> thetad+1.96*sqrt(thetad*(1-thetad)/(n.treat+n.control))
[1] 0.2248646
> 
```

#### 2.2 Likelihood Ratio Test

* Next, we turn to the issue of using a ratio of likelihoods to test the hypothesis of equal failure rates between the two groups.
* We begin by imposing the equality constraint that both groups have the same failure rates. 
* Then, we calculate the likelihood for each group subject to the constraint that the failure rate is a constant value of 0.182

```r
# the value of theta = 0.182 maximizes the likelihood function for all 313 cases
# note also that 57/313 = 0.182

# calculate likelihood function for each group (treatment and control)
# holding theta constant at 0.182

theta.c <- 0.182

like.treat.const <- choose(n.treat,n.fail.treat)*
                     theta.c^n.fail.treat*
                     (1-theta.c)^(n.treat-n.fail.treat)

like.control.const <- choose(n.control,n.fail.control)*
                      theta.c^n.fail.control*
                      (1-theta.c)^(n.control-n.fail.control)

likelihood.constrained <- like.treat.const*like.control.const
likelihood.constrained
```

* Here are the results:

```rout
> # the value of theta = 0.182 maximizes the likelihood function for all 313 cases
> # note also that 57/313 = 0.182

> # calculate likelihood function for each group (treatment and control)
< # holding theta constant at 0.182
> 
> theta.c <- 0.182
> 
> like.treat.const <- choose(n.treat,n.fail.treat)*
+                      theta.c^n.fail.treat*
+                      (1-theta.c)^(n.treat-n.fail.treat)
> 
> like.control.const <- choose(n.control,n.fail.control)*
+                       theta.c^n.fail.control*
+                       (1-theta.c)^(n.control-n.fail.control)
> 
> likelihood.constrained <- like.treat.const*like.control.const
> likelihood.constrained
[1] 0.0006751225
> 
```

* Then, we relax the constraint that the failure rate is constant for the two groups.
* First, we assess the likelihood function for the treatment (arrest) group:

```r
# maximum likelihood estimate of theta (free) 
# conditional on treatment is 0.109

theta <- seq(from=0,to=1,by=0.001)
p1.treat <- choose(n.treat,n.fail.treat)
p2.treat <- theta^(n.fail.treat)
p3.treat <- (1-theta)^(n.treat-n.fail.treat)
treat.likelihood <- p1.treat*p2.treat*p3.treat
treat.log.likelihood <- log(treat.likelihood)
df.treat <- data.frame(theta,treat.likelihood)
subset(df.treat,theta>=0.08 & theta<=0.12)
maxlike.treat <- max(treat.likelihood)
maxlike.treat

# use finite difference approximation to calculate derivative
# of log-likelihood function

10/92

theta1 <- 0.1086958
thetad <- 0.1086957
theta0 <- 0.1086956

pi1a <- choose(n.treat,n.fail.treat)
pi1b <- theta1^n.fail.treat
pi1c <- (1-theta1)^(n.treat-n.fail.treat)
pi1 <- pi1a*pi1b*pi1c
logpi1 <- log(pi1)

pida <- choose(n.treat,n.fail.treat)
pidb <- thetad^n.fail.treat
pidc <- (1-thetad)^(n.treat-n.fail.treat)
pid <- pida*pidb*pidc
logpid <- log(pid)

pi0a <- choose(n.treat,n.fail.treat)
pi0b <- theta0^n.fail.treat
pi0c <- (1-theta0)^(n.treat-n.fail.treat)
pi0 <- pi0a*pi0b*pi0c
logpi0 <- log(pi0)

# approximate derivative

(logpi1-logpi0)/(theta1-theta0)

# check the result

d <- deriv(~ log(pida*(thetad^n.fail.treat)*(1-thetad)^(n.treat-n.fail.treat)), "thetad")
eval(d)

# use finite difference approximation to calculate 
# second derivative of log-likelihood function

j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2

# check the result using the normal approximation
# to the binomial distribution

se.thetad <- 1/j^(1/2)
se.thetad

sqrt(thetad*(1-thetad)/n.treat)

thetad-1.96*se.thetad
thetad+1.96*se.thetad

thetad-1.96*sqrt(thetad*(1-thetad)/n.treat)
thetad+1.96*sqrt(thetad*(1-thetad)/n.treat)
```

* Here are the results:

```rout
> # maximum likelihood estimate of theta (free) 
> # conditional on treatment is 0.109
> 
> theta <- seq(from=0,to=1,by=0.001)
> p1.treat <- choose(n.treat,n.fail.treat)
> p2.treat <- theta^(n.fail.treat)
> p3.treat <- (1-theta)^(n.treat-n.fail.treat)
> treat.likelihood <- p1.treat*p2.treat*p3.treat
> treat.log.likelihood <- log(treat.likelihood)
> df.treat <- data.frame(theta,treat.likelihood)
> subset(df.treat,theta>=0.08 & theta<=0.12)
    theta treat.likelihood
81  0.080       0.08307629
82  0.081       0.08603943
83  0.082       0.08896389
84  0.083       0.09184211
85  0.084       0.09466667
86  0.085       0.09743037
87  0.086       0.10012625
88  0.087       0.10274760
89  0.088       0.10528799
90  0.089       0.10774134
91  0.090       0.11010186
92  0.091       0.11236414
93  0.092       0.11452314
94  0.093       0.11657419
95  0.094       0.11851304
96  0.095       0.12033582
97  0.096       0.12203910
98  0.097       0.12361986
99  0.098       0.12507549
100 0.099       0.12640382
101 0.100       0.12760308
102 0.101       0.12867194
103 0.102       0.12960946
104 0.103       0.13041512
105 0.104       0.13108879
106 0.105       0.13163072
107 0.106       0.13204154
108 0.107       0.13232222
109 0.108       0.13247411
110 0.109       0.13249885
111 0.110       0.13239841
112 0.111       0.13217508
113 0.112       0.13183138
114 0.113       0.13137013
115 0.114       0.13079438
116 0.115       0.13010740
117 0.116       0.12931265
118 0.117       0.12841380
119 0.118       0.12741467
120 0.119       0.12631923
121 0.120       0.12513159
> maxlike.treat <- max(treat.likelihood)
> maxlike.treat
[1] 0.1324988
> 
> # use finite difference approximation to calculate derivative
> # of log-likelihood function
> 
> 10/92
[1] 0.1086957
> 
> theta1 <- 0.1086958
> thetad <- 0.1086957
> theta0 <- 0.1086956
> 
> pi1a <- choose(n.treat,n.fail.treat)
> pi1b <- theta1^n.fail.treat
> pi1c <- (1-theta1)^(n.treat-n.fail.treat)
> pi1 <- pi1a*pi1b*pi1c
> logpi1 <- log(pi1)
> 
> pida <- choose(n.treat,n.fail.treat)
> pidb <- thetad^n.fail.treat
> pidc <- (1-thetad)^(n.treat-n.fail.treat)
> pid <- pida*pidb*pidc
> logpid <- log(pid)
> 
> pi0a <- choose(n.treat,n.fail.treat)
> pi0b <- theta0^n.fail.treat
> pi0c <- (1-theta0)^(n.treat-n.fail.treat)
> pi0 <- pi0a*pi0b*pi0c
> logpi0 <- log(pi0)
> 
> # approximate derivative
> 
> (logpi1-logpi0)/(theta1-theta0)
[1] -4.542366e-05
> 
> # check the result
> 
> d <- deriv(~ log(pida*(thetad^n.fail.treat)*(1-thetad)^(n.treat-n.fail.treat)), "thetad")
> eval(d)
[1] -2.021137
attr(,"gradient")
            thetad
[1,] -4.541657e-05
> 
> # use finite difference approximation to calculate 
> # second derivative of log-likelihood function
> 
> j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
> 
> # check the result using the normal approximation
> # to the binomial distribution
> 
> se.thetad <- 1/j^(1/2)
> se.thetad
[1] 0.03243147
> 
> sqrt(thetad*(1-thetad)/n.treat)
[1] 0.03245079
> 
> thetad-1.96*se.thetad
[1] 0.04513001
> thetad+1.96*se.thetad
[1] 0.1722614
> 
> thetad-1.96*sqrt(thetad*(1-thetad)/n.treat)
[1] 0.04509215
> thetad+1.96*sqrt(thetad*(1-thetad)/n.treat)
[1] 0.1722992
> 
```
* Next, we generate a plot of the likelihood and log-likelihood function:

```r
par(mfrow=c(1,2))

plot(x=theta,y=treat.likelihood,type="l",lty=1,lwd=1)
abline(h=seq(from=0,to=0.12,by=0.01),lty=3,lwd=0.5)
abline(v=seq(from=0,to=1,by=0.2),lty=3,lwd=0.5)

plot(x=theta,y=treat.log.likelihood,type="l",lty=1,lwd=1)
abline(h=seq(from=-500,to=0,by=50),lty=3,lwd=0.5)
abline(v=seq(from=0,to=1,by=0.1),lty=3,lwd=0.5)
```

* Here is the resulting plotspace:

<p align="left">
<img src="/gfiles/like-plot2.png" width="800px">
</p>

* We now turn to the likelihood function for the control group:

```r
# maximum likelihood estimate of theta (free) 
# conditional on control treatment is 0.213

p1.control <- choose(n.control,n.fail.control)
p2.control <- theta^(n.fail.control)
p3.control <- (1-theta)^(n.control-n.fail.control)
control.likelihood <- p1.control*p2.control*p3.control
control.log.likelihood <- log(control.likelihood)
df.control <- data.frame(theta,control.likelihood)
subset(df.control,theta>=0.18 & theta<=0.22)
maxlike.control <- max(control.likelihood)
maxlike.control

47/221

theta1 <- 0.2126698
thetad <- 0.2126697
theta0 <- 0.2126696

pi1a <- choose(n.control,n.fail.control)
pi1b <- theta1^n.fail.control
pi1c <- (1-theta1)^(n.control-n.fail.control)
pi1 <- pi1a*pi1b*pi1c
logpi1 <- log(pi1)

pida <- choose(n.control,n.fail.control)
pidb <- thetad^n.fail.control
pidc <- (1-thetad)^(n.control-n.fail.control)
pid <- pida*pidb*pidc
logpid <- log(pid)

pi0a <- choose(n.control,n.fail.control)
pi0b <- theta0^n.fail.control
pi0c <- (1-theta0)^(n.control-n.fail.control)
pi0 <- pi0a*pi0b*pi0c
logpi0 <- log(pi0)

# use finite difference approximation to calculate derivative
# of log-likelihood function

(logpi1-logpi0)/(theta1-theta0)

# check the result

d <- deriv(~ log(pida*(thetad^n.fail.control)*(1-thetad)^(n.control-n.fail.control)), "thetad")
eval(d)

# use finite difference approximation to calculate 
# second derivative of log-likelihood function

j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2

# check the result using the normal approximation
# to the binomial distribution

se.thetad <- 1/j^(1/2)
se.thetad

sqrt(thetad*(1-thetad)/n.control)

thetad-1.96*se.thetad
thetad+1.96*se.thetad

thetad-1.96*sqrt(thetad*(1-thetad)/n.control)
thetad+1.96*sqrt(thetad*(1-thetad)/n.control)
```

* Here are the results for the control group:

```rout
> # maximum likelihood estimate of theta (free) 
> # conditional on control treatment is 0.213
> 
> p1.control <- choose(n.control,n.fail.control)
> p2.control <- theta^(n.fail.control)
> p3.control <- (1-theta)^(n.control-n.fail.control)
> control.likelihood <- p1.control*p2.control*p3.control
> control.log.likelihood <- log(control.likelihood)
> df.control <- data.frame(theta,control.likelihood)
> subset(df.control,theta>=0.18 & theta<=0.22)
    theta control.likelihood
181 0.180         0.03047266
182 0.181         0.03197307
183 0.182         0.03349056
184 0.183         0.03502123
185 0.184         0.03656097
186 0.185         0.03810549
187 0.186         0.03965037
188 0.187         0.04119104
189 0.188         0.04272286
190 0.189         0.04424109
191 0.190         0.04574092
192 0.191         0.04721754
193 0.192         0.04866614
194 0.193         0.05008191
195 0.194         0.05146014
196 0.195         0.05279615
197 0.196         0.05408542
198 0.197         0.05532353
199 0.198         0.05650624
200 0.199         0.05762949
201 0.200         0.05868943
202 0.201         0.05968245
203 0.202         0.06060519
204 0.203         0.06145455
205 0.204         0.06222774
206 0.205         0.06292225
207 0.206         0.06353589
208 0.207         0.06406682
209 0.208         0.06451351
210 0.209         0.06487479
211 0.210         0.06514980
212 0.211         0.06533807
213 0.212         0.06543945
214 0.213         0.06545414
215 0.214         0.06538268
216 0.215         0.06522593
217 0.216         0.06498507
218 0.217         0.06466160
219 0.218         0.06425732
220 0.219         0.06377429
221 0.220         0.06321487
> maxlike.control <- max(control.likelihood)
> maxlike.control
[1] 0.06545414
> 
> 47/221
[1] 0.2126697
> 
> theta1 <- 0.2126698
> thetad <- 0.2126697
> theta0 <- 0.2126696
> 
> pi1a <- choose(n.control,n.fail.control)
> pi1b <- theta1^n.fail.control
> pi1c <- (1-theta1)^(n.control-n.fail.control)
> pi1 <- pi1a*pi1b*pi1c
> logpi1 <- log(pi1)
> 
> pida <- choose(n.control,n.fail.control)
> pidb <- thetad^n.fail.control
> pidc <- (1-thetad)^(n.control-n.fail.control)
> pid <- pida*pidb*pidc
> logpid <- log(pid)
> 
> pi0a <- choose(n.control,n.fail.control)
> pi0b <- theta0^n.fail.control
> pi0c <- (1-theta0)^(n.control-n.fail.control)
> pi0 <- pi0a*pi0b*pi0c
> logpi0 <- log(pi0)
> 
> # use finite difference approximation to calculate derivative
> # of log-likelihood function
> 
> (logpi1-logpi0)/(theta1-theta0)
[1] -2.209788e-05
> 
> # check the result
> 
> d <- deriv(~ log(pida*(thetad^n.fail.control)*(1-thetad)^(n.control-n.fail.control)), "thetad")
> eval(d)
[1] -2.726334
attr(,"gradient")
           thetad
[1,] -2.20973e-05
> 
> # use finite difference approximation to calculate 
> # second derivative of log-likelihood function
> 
> j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
> 
> # check the result using the normal approximation
> # to the binomial distribution
> 
> se.thetad <- 1/j^(1/2)
> se.thetad
[1] 0.02749994
> 
> sqrt(thetad*(1-thetad)/n.control)
[1] 0.0275255
> 
> thetad-1.96*se.thetad
[1] 0.1587698
> thetad+1.96*se.thetad
[1] 0.2665696
> 
> thetad-1.96*sqrt(thetad*(1-thetad)/n.control)
[1] 0.1587197
> thetad+1.96*sqrt(thetad*(1-thetad)/n.control)
[1] 0.2666197
> 
```

* And, the likelihood plots are:

```r
par(mfrow=c(1,2))

plot(x=theta,y=control.likelihood,type="l",lty=1,lwd=1)
abline(h=seq(from=0,to=0.06,by=0.01),lty=3,lwd=0.5)
abline(v=seq(from=0,to=1,by=0.2),lty=3,lwd=0.5)

plot(x=theta,y=control.log.likelihood,type="l",lty=1,lwd=1)
abline(h=seq(from=-650,to=0,by=50),lty=3,lwd=0.5)
abline(v=seq(from=0,to=1,by=0.1),lty=3,lwd=0.5)
```

<p align="left">
<img src="/gfiles/like-plot3.png" width="800px">
</p>

* Now, we are ready to obtain the likelihood (at its maximum)
for the free (2 parameter model):

```r
# we get the likelihood for the free (2 parameter) 
# model by multiplying the likelihoods for the 
# treatment and control groups.

likelihood.free <- maxlike.treat*maxlike.control
likelihood.free
```

which yields:

```rout
> # we get the likelihood for the free (2 parameter) 
> # model by multiplying the likelihoods for the 
> # treatment and control groups.
> 
> likelihood.free <- maxlike.treat*maxlike.control
> likelihood.free
[1] 0.008672599
>
```

* We are now ready to form the likelihood ratio:

```r
# calculate the likelihood ratio 
# l.constrained/l.free

likelihood.ratio <- likelihood.constrained/likelihood.free
likelihood.ratio
```

* And, the likelihood ratio is:

```rout
> # calculate the likelihood ratio 
> # l.constrained/l.free
> 
> likelihood.ratio <- likelihood.constrained/likelihood.free
> likelihood.ratio
[1] 0.07784547
> 
```

* Please note that the constrained model is in the numerator
and the free model is in the denominator:


* The sampling distribution of the likelihood ratio is not well defined but
the sampling distribution of -2 x the log-likelihood ratio is chi-square
with degrees of freedom equal to the difference between the number of parameters
in the free and constrained models:

```r
# log(likelihood ratio) = log(l.constrained)-log(l.free)

log.likelihood.ratio <- log(likelihood.constrained)-log(likelihood.free)
log.likelihood.ratio

# identify critical region for test statistic

chisq.dist <- rchisq(n=100000000,df=1)
quantile(chisq.dist,0.95)

# calculated test statistic

test.statistic <- -2*log.likelihood.ratio
test.statistic

# p-value for test

1-pchisq(test.statistic,df=2-1)
```

which yields:

```rout
> # log(likelihood ratio) = log(l.constrained)-log(l.free)
> 
> log.likelihood.ratio <- log(likelihood.constrained)-log(likelihood.free)
> log.likelihood.ratio
[1] -2.55303
> 
> # identify critical region for test statistic
> 
> chisq.dist <- rchisq(n=100000000,df=1)
> quantile(chisq.dist,0.95)
     95% 
3.841471 

> 
> # calculated test statistic
> 
> test.statistic <- -2*log.likelihood.ratio
> test.statistic
[1] 5.106059
> 
> # p-value for test
> 
> 1-pchisq(test.statistic,df=2-1)
[1] 0.02384242
> 
```

#### 2.3 Rectangular Data Set and Contingency Table

* We begin by creating a rectangular data set:

```r
# create individual level dataset

y0 <- c(rep(1,47),rep(0,221-47))
y1 <- c(rep(1,10),rep(0,92-10))
y <- c(y0,y1)
x <- c(rep(0,221),rep(1,92))
df <- data.frame(x,y)
df

# crosstable (outcome on rows; treatment on columns)

ct <- table(df$y,df$x,exclude=NULL)
ct

# conditional probabilities

py1x0 <- ct[2,1]/(ct[1,1]+ct[2,1])
py1x0
py1x1 <- ct[2,2]/(ct[1,2]+ct[2,2])
py1x1
```

* Here are the results:

```rout
> # create individual level dataset
> 
> y0 <- c(rep(1,47),rep(0,221-47))
> y1 <- c(rep(1,10),rep(0,92-10))
> y <- c(y0,y1)
> x <- c(rep(0,221),rep(1,92))
> df <- data.frame(x,y)
> df
    x y
1   0 1
2   0 1
3   0 1
4   0 1
5   0 1
*
*
*
309 1 0
310 1 0
311 1 0
312 1 0
313 1 0
> 
> # crosstable (outcome on rows; treatment on columns)
> 
> ct <- table(df$y,df$x,exclude=NULL)
> ct
   
      0   1
  0 174  82
  1  47  10
> 
> # conditional probabilities
> 
> py1x0 <- ct[2,1]/(ct[1,1]+ct[2,1])
> py1x0
[1] 0.2126697
> py1x1 <- ct[2,2]/(ct[1,2]+ct[2,2])
> py1x1
[1] 0.1086957
> 
```

* We can use this information to calculate the classical
treatment effect (delta), relative risk, and odds ratio for these data:

```r
# delta - CTE

delta <- py1x1-py1x0
delta

# relative risk and odds ratio

ct.rr <- py1x1/py1x0
ct.rr
ct.or.num <- py1x1/(1-py1x1)
ct.or.den <- py1x0/(1-py1x0)
ct.or <- ct.or.num/ct.or.den
ct.or
```
* And the results are:

```rout
> # delta - CTE
> 
> delta <- py1x1-py1x0
> delta
[1] -0.103974
> 
> # relative risk and odds ratio
> 
> ct.rr <- py1x1/py1x0
> ct.rr
[1] 0.5111008
> ct.or.num <- py1x1/(1-py1x1)
> ct.or.den <- py1x0/(1-py1x0)
> ct.or <- ct.or.num/ct.or.den
> ct.or
[1] 0.451479
> 
```

#### Assignment Due Thursday 9/16/21

* Consider the treatment-as-delivered data from the Minneapolis study. Here are the data you should use:

```r
n.treat <- 135
n.fail.treat <- 18

n.control <- 178
n.fail.control <- 39
```

* Your task in this assignment is twofold: (1) verify that the confidence interval for θ using the curvature of the likelihood function matches the confidence interval for θ using the traditional normal-approximation-to-the-binomial method; and (2) conduct a likelihood-ratio test using the likelihoods derived from the binomial probability distribution. Summarize your findings.

### Lesson 3 - Thursday 9/16/21

* We begin today's lesson by keying in the dataset and recovering the information we need from last week:

```r
# create individual level dataset

y0 <- c(rep(1,47),rep(0,221-47))
y1 <- c(rep(1,10),rep(0,92-10))
y <- c(y0,y1)
x <- c(rep(0,221),rep(1,92))
df <- data.frame(x,y)
df

# crosstable (outcome on rows; treatment on columns)

ct <- table(df$y,df$x,exclude=NULL)
ct

# conditional probabilities

py1x0 <- ct[2,1]/(ct[1,1]+ct[2,1])
py1x0
py1x1 <- ct[2,2]/(ct[1,2]+ct[2,2])
py1x1
```

* Here are the results:


```rout
> # create individual level dataset
> 
> y0 <- c(rep(1,47),rep(0,221-47))
> y1 <- c(rep(1,10),rep(0,92-10))
> y <- c(y0,y1)
> x <- c(rep(0,221),rep(1,92))
> df <- data.frame(x,y)
> df
    x y
1   0 1
2   0 1
3   0 1
4   0 1
5   0 1
*
*
*
309 1 0
310 1 0
311 1 0
312 1 0
313 1 0
> 
> # crosstable (outcome on rows; treatment on columns)
> 
> ct <- table(df$y,df$x,exclude=NULL)
> ct
   
      0   1
  0 174  82
  1  47  10
> 
> # conditional probabilities
> 
> py1x0 <- ct[2,1]/(ct[1,1]+ct[2,1])
> py1x0
[1] 0.2126697
> py1x1 <- ct[2,2]/(ct[1,2]+ct[2,2])
> py1x1
[1] 0.1086957
> 
```

#### 3.1 - Likelihood ratio test using logistic regression

* Next, we estimate the constrained and free logistic regression models:

```r
# constrained logistic regression model

constmodel <- glm(y~1,data=df,family=binomial(link="logit"))
summary(constmodel)
pfailconst <- exp(-1.5021)/(1+exp(-1.5021))
pfailconst
logLik(constmodel)

# free logistic regression model

freemodel <- glm(y~1+x,data=df,family=binomial(link="logit"))
summary(freemodel)
pfailtreat <- exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))
pfailnotreat <- exp(-1.3089)/(1+exp(-1.3089))
pfailtreat
pfailnotreat
logLik(freemodel)
```
* And the results are:

```rout
> # constrained logistic regression model
> 
> constmodel <- glm(y~1,data=df,family=binomial(link="logit"))
> summary(constmodel)

Call:
glm(formula = y ~ 1, family = binomial(link = "logit"), data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6341  -0.6341  -0.6341  -0.6341   1.8456  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.5021     0.1465  -10.26   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 297.08  on 312  degrees of freedom
AIC: 299.08

Number of Fisher Scoring iterations: 4

> pfailconst <- exp(-1.5021)/(1+exp(-1.5021))
> pfailconst
[1] 0.1821125
> logLik(constmodel)
'log Lik.' -148.5423 (df=1)
> 
> # free logistic regression model
> 
> freemodel <- glm(y~1+x,data=df,family=binomial(link="logit"))
> summary(freemodel)

Call:
glm(formula = y ~ 1 + x, family = binomial(link = "logit"), data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6915  -0.6915  -0.6915  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.3089     0.1644  -7.962 1.69e-15 ***
x            -0.7952     0.3731  -2.131   0.0331 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.98  on 311  degrees of freedom
AIC: 295.98

Number of Fisher Scoring iterations: 4

> pfailtreat <- exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))
> pfailnotreat <- exp(-1.3089)/(1+exp(-1.3089))
> pfailtreat
[1] 0.108699
> pfailnotreat
[1] 0.212671
> logLik(freemodel)
'log Lik.' -145.9891 (df=2)
> 
```

* This sets us up to calculate the likelihood ratio test:

```r
# likelihood-ratio test

lrtest <- -2*(logLik(constmodel)-logLik(freemodel))
lrtest
```

* And, the results confirm that we get the same likelihood ratio test results we had before:

```rout
> # likelihood-ratio test
> 
> lrtest <- -2*(logLik(constmodel)-logLik(freemodel))
> lrtest
'log Lik.' 5.106266 (df=1)
>
```

* We can use the parameter estimates from the logistic regression model to calculate the
classical treatment effect, relative risk, and odds ratio statistics:

```r
# classical treatment effect (delta) 

lm.delta <- pfailtreat-pfailnotreat
lm.delta

# relative risk from logistic regression

relative.risk <- pfailtreat/pfailnotreat
relative.risk

# odds ratio from logistic regression

odds.ratio.num <- pfailtreat/(1-pfailtreat)
odds.ratio.num
odds.ratio.den <- pfailnotreat/(1-pfailnotreat)
odds.ratio.den
odds.ratio <- odds.ratio.num/odds.ratio.den
odds.ratio
odds.ratio.lm <- exp(-0.7952)
odds.ratio.lm
```

* And, the results are:

```rout
> # classical treatment effect (delta) 
> 
> lm.delta <- pfailtreat-pfailnotreat
> lm.delta
[1] -0.103972
> 
> # relative risk from logistic regression
> 
> relative.risk <- pfailtreat/pfailnotreat
> relative.risk
[1] 0.5111133
> 
> # odds ratio from logistic regression
> 
> odds.ratio.num <- pfailtreat/(1-pfailtreat)
> odds.ratio.num
[1] 0.1219554
> odds.ratio.den <- pfailnotreat/(1-pfailnotreat)
> odds.ratio.den
[1] 0.270117
> odds.ratio <- odds.ratio.num/odds.ratio.den
> odds.ratio
[1] 0.4514909
> odds.ratio.lm <- exp(-0.7952)
> odds.ratio.lm
[1] 0.4514909
> 
```

* Note that these numbers match what we calculated from the contingency table.
* Maximum likelihood estimators have the following properties: (1) consistency, (2) asymptotic normality, and (3) asymptotic efficiency (King, 1989:74-80).

#### 3.2 Maximum Likelihood Estimators and Sample Size

* Here is a simulation to demonstrate the performance of a ML estimator with different sample sizes.
* We start by assuming that *x* is a binary variable with 5 values of zero and 5 values of one.

```r
# simulation to show how ML estimator 
# performs with varying sample sizes

x <- c(rep(0,5),rep(1,5))

bx <- vector()

for(i in 1:100000)
  {
    u <- runif(n=length(x),min=0,max=1)
    e <- log(u/(1-u))
    y <- ifelse((0.2*x+e)>0,1,0)
    m <- glm(y~1+x,family=binomial(link="logit"))
    bx[i] <- coef(m)[2]
  }

par(mfrow=c(1,2))

hist(bx)
boxplot(bx)
mean(bx)
median(bx)
```

* Here are the results:

```rout
> # simulation to show how ML estimator 
> # performs with varying sample sizes
> 
> x <- c(rep(0,5),rep(1,5))
> 
> bx <- vector()
> 
> for(i in 1:100000)
+   {
+     u <- runif(n=length(x),min=0,max=1)
+     e <- log(u/(1-u))
+     y <- ifelse((0.2*x+e)>0,1,0)
+     m <- glm(y~1+x,family=binomial(link="logit"))
+     bx[i] <- coef(m)[2]
+   }
> 
> par(mfrow=c(1,2))
> 
> hist(bx)
> boxplot(bx)
> mean(bx)
[1] 0.7884887
> median(bx)
[1] 4.299875e-16
```

<p align="left">
<img src="/gfiles/bx1.png" width="800px">
</p>

* Now, let's see what happens when we increase the sample size to 25 cases at each level of *x*:

```r
# simulation to show how ML estimator 
# performs with varying sample sizes

x <- c(rep(0,25),rep(1,25))

bx <- vector()

for(i in 1:100000)
  {
    u <- runif(n=length(x),min=0,max=1)
    e <- log(u/(1-u))
    y <- ifelse((0.2*x+e)>0,1,0)
    m <- glm(y~1+x,family=binomial(link="logit"))
    bx[i] <- coef(m)[2]
  }

par(mfrow=c(1,2))

hist(bx)
boxplot(bx)
mean(bx)
median(bx)
```

* Here are the results:

```rout
> # simulation to show how ML estimator 
> # performs with varying sample sizes
> 
> x <- c(rep(0,25),rep(1,25))
> 
> bx <- vector()
> 
> for(i in 1:100000)
+   {
+     u <- runif(n=length(x),min=0,max=1)
+     e <- log(u/(1-u))
+     y <- ifelse((0.2*x+e)>0,1,0)
+     m <- glm(y~1+x,family=binomial(link="logit"))
+     bx[i] <- coef(m)[2]
+   }
> 
> par(mfrow=c(1,2))
> 
> hist(bx)
> boxplot(bx)
> mean(bx)
[1] 0.2086212
> median(bx)
[1] 0.1643031
> 
```

<p align="left">
<img src="/gfiles/bx2.png" width="800px">
</p>

* And, finally, we increase the sample size to 100 cases at each level of *x*:

```r
# simulation to show how ML estimator 
# performs with varying sample sizes

x <- c(rep(0,100),rep(1,100))

bx <- vector()

for(i in 1:100000)
  {
    u <- runif(n=length(x),min=0,max=1)
    e <- log(u/(1-u))
    y <- ifelse((0.2*x+e)>0,1,0)
    m <- glm(y~1+x,family=binomial(link="logit"))
    bx[i] <- coef(m)[2]
  }

par(mfrow=c(1,2))

hist(bx)
boxplot(bx)
mean(bx)
median(bx)
```

* Here are the results:

```rout
> # simulation to show how ML estimator 
> # performs with varying sample sizes
> 
> x <- c(rep(0,100),rep(1,100))
> 
> bx <- vector()
> 
> for(i in 1:100000)
+   {
+     u <- runif(n=length(x),min=0,max=1)
+     e <- log(u/(1-u))
+     y <- ifelse((0.2*x+e)>0,1,0)
+     m <- glm(y~1+x,family=binomial(link="logit"))
+     bx[i] <- coef(m)[2]
+   }
> 
> par(mfrow=c(1,2))
> 
> hist(bx)
> boxplot(bx)
> mean(bx)
[1] 0.1998563
> median(bx)
[1] 0.2006707
> 
```

<p align="left">
<img src="/gfiles/bx3.png" width="800px">
</p>

#### 3.3 Bias and Efficiency

* We now consider the concepts of bias and efficiency using a simulated example.
* Which is a better estimator of the population mean? -- the sample mean or the sample median?

```r
# simulate a random variable, yp, for the
# population [popsize] which is normally 
# distributed with a mean of [mu] and a 
# standard deviation of [sigma]

popsize <- 1000000
mu <- 100
sigma <- 10

y.p <- rnorm(n=popsize,mean=mu,sd=sigma)

# calculate the population mean 

popmean <- mean(y.p)
popmean

# now, let's draw [nsamples] from the population

nsamples <- 100000
sampsize <- 100

y.s.mean <- vector()
se.mean <- vector()
y.s.median <- vector()

for(i in 1:nsamples){
  y.s <- sample(y.p,size=sampsize,replace=T)
  y.s.mean[i] <- mean(y.s)
  se.mean[i] <- sd(y.s)/sqrt(sampsize-1)
  y.s.median[i] <- median(y.s)
  }
  
mean(y.s.mean)
median(y.s.mean)

mean(y.s.median)
median(y.s.median)

mean(se.mean)
sd(y.s.mean)
sd(y.s.median)
```

* Here are the results:

```rout
> # simulate a random variable, yp, for the
> # population [popsize] which is normally 
> # distributed with a mean of [mu] and a 
> # standard deviation of [sigma]
> 
> popsize <- 1000000
> mu <- 100
> sigma <- 10
> 
> y.p <- rnorm(n=popsize,mean=mu,sd=sigma)
> 
> # calculate the population mean 
> 
> popmean <- mean(y.p)
> popmean
[1] 100.0079
> 
> # now, let's draw [nsamples] from the population
> 
> nsamples <- 100000
> sampsize <- 100
> 
> y.s.mean <- vector()
> se.mean <- vector()
> y.s.median <- vector()
> 
> for(i in 1:nsamples){
+   y.s <- sample(y.p,size=sampsize,replace=T)
+   y.s.mean[i] <- mean(y.s)
+   se.mean[i] <- sd(y.s)/sqrt(sampsize-1)
+   y.s.median[i] <- median(y.s)
+   }
>   
> mean(y.s.mean)
[1] 100.0058
> median(y.s.mean)
[1] 100.0057
> 
> mean(y.s.median)
[1] 100.0198
> median(y.s.median)
[1] 100.0215
> 
> mean(se.mean)
[1] 1.002279
> sd(y.s.mean)
[1] 0.9995369
> sd(y.s.median)
[1] 1.246504
> 
```

* So, the sample mean is a *better* estimator not on bias grounds but on efficiency grounds.

#### 3.4 Permutation Test

* The properties of ML estimators are all large sample properties. In some instances, we may wish to test a hypothesis about the similarity of patterns between groups without appealing to asymptotics (large samples) or distributional assumptions (i.e., the binomial or the normal approximation to the binomial). Our example here has been a comparison between 2 groups. We can extend this analysis to a set of highly general results derived from a permutation test. Larry Wasserman has a very interesting blog post (see section 2) about this issue ([linked here](https://normaldeviate.wordpress.com/2012/07/14/modern-two-sample-tests/)).
* Here is an illustration of the permutation method using Larry's approach:

```r
# key in the dataset

y0 <- c(rep(1,47),rep(0,221-47))
y1 <- c(rep(1,10),rep(0,92-10))
y <- c(y0,y1)
x <- c(rep(0,221),rep(1,92))

# create a contingency table (y on the rows, x on the columns)

stable <- table(y,x)
stable

# calculate the treatment effect observed in the sample

s.py1x0 <- stable[2,1]/(stable[1,1]+stable[2,1])
s.py1x1 <- stable[2,2]/(stable[1,2]+stable[2,2])
s.py1x1-s.py1x0

# permute the dataset 100,000 times

pdelta <- vector()

for(i in 1:100000)
  {
    ys <- sample(y,size=313,replace=F)
    ptable <- table(ys,x)
    py1x0 <- ptable[2,1]/(ptable[1,1]+ptable[2,1])
    py1x1 <- ptable[2,2]/(ptable[1,2]+ptable[2,2])
    pdelta[i] <- py1x1-py1x0
  }

hist(pdelta)
abline(v=s.py1x1-s.py1x0,lty=2,lwd=1,col="purple")

# identify the boundaries of the central 95% of the permutation distribution

quantile(pdelta,0.025)
quantile(pdelta,0.975)
```

* Here are the results:

```rout
> # key in the dataset
> 
> y0 <- c(rep(1,47),rep(0,221-47))
> y1 <- c(rep(1,10),rep(0,92-10))
> y <- c(y0,y1)
> x <- c(rep(0,221),rep(1,92))
> 
> # create a contingency table (y on the rows, x on the columns)
> 
> stable <- table(y,x)
> stable
   x
y     0   1
  0 174  82
  1  47  10
> 
> # calculate the treatment effect observed in the sample
> 
> s.py1x0 <- stable[2,1]/(stable[1,1]+stable[2,1])
> s.py1x1 <- stable[2,2]/(stable[1,2]+stable[2,2])
> s.py1x1-s.py1x0
[1] -0.103974
> 
> # permute the dataset 100,000 times
> 
> pdelta <- vector()
> 
> for(i in 1:100000)
+   {
+     ys <- sample(y,size=313,replace=F)
+     ptable <- table(ys,x)
+     py1x0 <- ptable[2,1]/(ptable[1,1]+ptable[2,1])
+     py1x1 <- ptable[2,2]/(ptable[1,2]+ptable[2,2])
+     pdelta[i] <- py1x1-py1x0
+   }
> 
> hist(pdelta)
> abline(v=s.py1x1-s.py1x0,lty=2,lwd=1,col="purple")
> 
> # identify the boundaries of the central 95% of the permutation distribution
> 
> quantile(pdelta,0.025)
       2.5% 
-0.08857958 
> quantile(pdelta,0.975)
     97.5% 
0.09615385 
> 
```

<p align="left">
<img src="/gfiles/perm1.png" width="700px">
</p>

#### Assignment Due Thursday 9/23/21

* Use the treatment-as-delivered data from the Minneapolis study:

```r
n.treat <- 135
n.fail.treat <- 18

n.control <- 178
n.fail.control <- 39
```

* Your task in this assignment is twofold: (1) calculate and interpret the classical treatment effect, the relative risk, and the odds ratio; and (2) test the significance of the treatment effect using the permutation test. Summarize your findings.
* Reading assignment - chapter 1 of Agresti (2019; An introduction to categorical data analysis) - available [here](https://media.wiley.com/product_data/excerpt/62/11194052/1119405262-36.pdf).

### Lesson 4 - Thursday 9/23/21

* We begin by re-entering the Minneapolis dataset:

```r
# enter dataset

y0 <- c(rep(1,47),rep(0,221-47))
y1 <- c(rep(1,10),rep(0,92-10))
y <- c(y0,y1)
x <- c(rep(0,221),rep(1,92))

table(y,x)
```

* Here is our output:

```rout
> # enter dataset
> 
> y0 <- c(rep(1,47),rep(0,221-47))
> y1 <- c(rep(1,10),rep(0,92-10))
> y <- c(y0,y1)
> x <- c(rep(0,221),rep(1,92))
> 
> table(y,x)
   x
y     0   1
  0 174  82
  1  47  10
> 
```

* Now, use the ```glm()''' function to estimate a logistic regression model:

```r
# estimate logistic regression model

lr <- glm(y~1+x,family="binomial"(link="logit"))
summary(lr)
logLik(lr)
```

* Here are our results:

```rout
> # estimate logistic regression model
> 
> lr <- glm(y~1+x,family="binomial"(link="logit"))
> summary(lr)

Call:
glm(formula = y ~ 1 + x, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6915  -0.6915  -0.6915  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.3089     0.1644  -7.962 1.69e-15 ***
x            -0.7952     0.3731  -2.131   0.0331 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.98  on 311  degrees of freedom
AIC: 295.98

Number of Fisher Scoring iterations: 4

> logLik(lr)
'log Lik.' -145.9891 (df=2)
> 
```

* Now, we want to look at how we find the maximum likelihood solution for this
problem. This is a grid-search analysis to find the approximate maximum likelihood
solution:

```r
# logistic regression likelihood maximization exercise

a <- seq(from=-2,to=2,by=0.01)
b <- seq(from=-2,to=2,by=0.01)
c <- expand.grid(a,b)
nrow(c)

llf <- vector()
for(i in 1:nrow(c)) {
  py1 <- exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x))
  py0 <- 1-(exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x)))
  llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
  }

max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
head(max.df,n=30)
```

* And, here are the results:

```rout
> # logistic regression likelihood maximization exercise
> 
> a <- seq(from=-2,to=2,by=0.01)
> b <- seq(from=-2,to=2,by=0.01)
> c <- expand.grid(a,b)
> nrow(c)
[1] 160801
> 
> llf <- vector()
> for(i in 1:nrow(c)) {
+   py1 <- exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x))
+   py0 <- 1-(exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x)))
+   llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
+   }
> 
> max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
> head(max.df,n=30)
      c.Var1 c.Var2       llf
48591  -1.31  -0.79 -145.9892
48190  -1.31  -0.80 -145.9893
48992  -1.31  -0.78 -145.9900
47789  -1.31  -0.81 -145.9903
48191  -1.30  -0.80 -145.9907
47790  -1.30  -0.81 -145.9907
48991  -1.32  -0.78 -145.9915
48592  -1.30  -0.79 -145.9915
48590  -1.32  -0.79 -145.9915
47389  -1.30  -0.82 -145.9917
49393  -1.31  -0.77 -145.9918
47388  -1.31  -0.82 -145.9921
49392  -1.32  -0.77 -145.9923
48189  -1.32  -0.80 -145.9925
48993  -1.30  -0.78 -145.9932
46988  -1.30  -0.83 -145.9935
49793  -1.32  -0.76 -145.9940
47788  -1.32  -0.81 -145.9944
49794  -1.31  -0.76 -145.9944
46987  -1.31  -0.83 -145.9948
49394  -1.30  -0.77 -145.9958
47791  -1.29  -0.81 -145.9958
47390  -1.29  -0.82 -145.9959
46587  -1.30  -0.84 -145.9963
50194  -1.32  -0.75 -145.9966
48192  -1.29  -0.80 -145.9966
46989  -1.29  -0.83 -145.9969
47387  -1.32  -0.82 -145.9971
49391  -1.33  -0.77 -145.9974
48990  -1.33  -0.78 -145.9975
> 
```

* These results confirm that the maximum likelihood solution is indeed close
to the exact solution produced by the logistic regression program.
* Next, we estimate a probit regression model:

```r
# estimate probit regression model

pr <- glm(y~1+x,family="binomial"(link="probit"))
summary(pr)
logLik(pr)
```

* Here are the results:

```rout
> # estimate probit regression model
> 
> pr <- glm(y~1+x,family="binomial"(link="probit"))
> summary(pr)

Call:
glm(formula = y ~ 1 + x, family = binomial(link = "probit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6915  -0.6915  -0.6915  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.7972     0.0948  -8.409   <2e-16 ***
x            -0.4363     0.1982  -2.201   0.0277 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.98  on 311  degrees of freedom
AIC: 295.98

Number of Fisher Scoring iterations: 4

> logLik(pr)
'log Lik.' -145.9891 (df=2)
> 
```

* Notice that the probit log-likelihood is virtually identical
to the logistic regression log-likelihood.
* We can also conduct a grid search to find the approximate
maximum likelihood estimate:

```
# probit regression likelihood maximization exercise

a <- seq(from=-2,to=2,by=0.01)
b <- seq(from=-2,to=2,by=0.01)
c <- expand.grid(a,b)
nrow(c)

llf <- vector()
for(i in 1:nrow(c)) {
  py1 <- pnorm(c$Var1[i]+c$Var2[i]*x)
  py0 <- 1-pnorm(c$Var1[i]+c$Var2[i]*x)
  llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
  }

max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
head(max.df,n=30)
```

* Here are the grid search results for the probit regression:

```rout
> # probit regression likelihood maximization exercise
> 
> a <- seq(from=-2,to=2,by=0.01)
> b <- seq(from=-2,to=2,by=0.01)
> c <- expand.grid(a,b)
> nrow(c)
[1] 160801
> 
> llf <- vector()
> for(i in 1:nrow(c)) {
+   py1 <- pnorm(c$Var1[i]+c$Var2[i]*x)
+   py0 <- 1-pnorm(c$Var1[i]+c$Var2[i]*x)
+   llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
+   }
> 
> max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
> head(max.df,n=30)
      c.Var1 c.Var2       llf
63078  -0.80  -0.43 -145.9898
62677  -0.80  -0.44 -145.9903
62678  -0.79  -0.44 -145.9922
63479  -0.80  -0.42 -145.9926
62277  -0.79  -0.45 -145.9927
62276  -0.80  -0.45 -145.9940
63079  -0.79  -0.43 -145.9950
61876  -0.79  -0.46 -145.9965
63478  -0.81  -0.42 -145.9984
63880  -0.80  -0.41 -145.9987
63077  -0.81  -0.43 -145.9989
61875  -0.80  -0.46 -146.0011
63480  -0.79  -0.42 -146.0012
63879  -0.81  -0.41 -146.0012
62676  -0.81  -0.44 -146.0027
61475  -0.79  -0.47 -146.0035
62278  -0.78  -0.45 -146.0058
61877  -0.78  -0.46 -146.0063
64280  -0.81  -0.40 -146.0074
64281  -0.80  -0.40 -146.0082
62679  -0.78  -0.44 -146.0086
62275  -0.81  -0.45 -146.0098
61476  -0.78  -0.47 -146.0101
63881  -0.79  -0.41 -146.0107
61474  -0.80  -0.47 -146.0114
61074  -0.79  -0.48 -146.0138
63080  -0.78  -0.43 -146.0148
64681  -0.81  -0.39 -146.0169
61075  -0.78  -0.48 -146.0171
63878  -0.82  -0.41 -146.0182
> 
```

* Notice that the parameter estimates are different for the 
probit model. This doesn't mean that any of our interpretations
will be different, however. We will return to this issue shortly.

* Now, we turn to the linear probability model:

```r
# estimate linear probability model

lp <- lm(y~1+x)
summary(lp)
logLik(lp)
```

* And, here are the results:

```rout
> # estimate linear probability model
> 
> lp <- lm(y~1+x)
> summary(lp)

Call:
lm(formula = y ~ 1 + x)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2127 -0.2127 -0.2127 -0.1087  0.8913 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21267    0.02585   8.228 5.28e-15 ***
x           -0.10397    0.04768  -2.181   0.0299 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3842 on 311 degrees of freedom
Multiple R-squared:  0.01506,	Adjusted R-squared:  0.0119 
F-statistic: 4.756 on 1 and 311 DF,  p-value: 0.02994

> logLik(lp)
'log Lik.' -143.7486 (df=3)
> 
```

* We can also find the approximate maximum likelihood solution for
this problem as follows. Notice that now we have three parameters 
to estimate -- which increases the complexity of the problem:

```r
# normal (ls) maximum likelihood

a <- seq(from=0,to=1,by=0.01)
b <- seq(from=-1,to=1,by=0.01)
s <- seq(from=0,to=1,by=0.01)
c <- expand.grid(a,b,s)
nrow(c)

llf <- vector()
for(i in 1:nrow(c)) {
  pt1 <- 1/(c$Var3[i]*sqrt(2*pi))
  pt2 <- y-(c$Var1[i]+c$Var2[i]*x)
  pt3 <- (pt2/c$Var3[i])^2 
  pt4 <- exp(-0.5*pt3)
  pdf <- pt1*pt4
  llf[i] <- sum(log(pdf))
  }

max.df <- data.frame(c$Var1,c$Var2,c$Var3,llf)[order(-llf),]    
head(max.df,n=30)
```

* Here are the results:

```rout
> # normal (ls) maximum likelihood
> 
> a <- seq(from=0,to=1,by=0.01)
> b <- seq(from=-1,to=1,by=0.01)
> s <- seq(from=0,to=1,by=0.01)
> c <- expand.grid(a,b,s)
> nrow(c)
[1] 2050401
> 
> llf <- vector()
> for(i in 1:nrow(c)) {
+   pt1 <- 1/(c$Var3[i]*sqrt(2*pi))
+   pt2 <- y-(c$Var1[i]+c$Var2[i]*x)
+   pt3 <- (pt2/c$Var3[i])^2 
+   pt4 <- exp(-0.5*pt3)
+   pdf <- pt1*pt4
+   llf[i] <- sum(log(pdf))
+   }
> 
> max.df <- data.frame(c$Var1,c$Var2,c$Var3,llf)[order(-llf),]    
> head(max.df,n=30)
       c.Var1 c.Var2 c.Var3       llf
780550   0.21  -0.10   0.38 -143.7743
780449   0.21  -0.11   0.38 -143.7979
780450   0.22  -0.11   0.38 -143.8100
780651   0.21  -0.09   0.38 -143.8145
780349   0.22  -0.12   0.38 -143.8335
780551   0.22  -0.10   0.38 -143.8501
800851   0.21  -0.10   0.39 -143.8553
800750   0.21  -0.11   0.39 -143.8777
780348   0.21  -0.12   0.38 -143.8851
800751   0.22  -0.11   0.39 -143.8892
780650   0.20  -0.09   0.38 -143.8917
800952   0.21  -0.09   0.39 -143.8934
800650   0.22  -0.12   0.39 -143.9115
780549   0.20  -0.10   0.38 -143.9152
780752   0.21  -0.08   0.38 -143.9184
780248   0.22  -0.13   0.38 -143.9208
800852   0.22  -0.10   0.39 -143.9273
780751   0.20  -0.08   0.38 -143.9319
780652   0.22  -0.09   0.38 -143.9540
800649   0.21  -0.12   0.39 -143.9605
800951   0.20  -0.09   0.39 -143.9667
800850   0.20  -0.10   0.39 -143.9891
801053   0.21  -0.08   0.39 -143.9921
800549   0.22  -0.13   0.39 -143.9944
780350   0.23  -0.12   0.38 -143.9987
780448   0.20  -0.11   0.38 -144.0025
801052   0.20  -0.08   0.39 -144.0049
780249   0.23  -0.13   0.38 -144.0222
800953   0.22  -0.09   0.39 -144.0259
780852   0.20  -0.07   0.38 -144.0357
> 
```

* A problem that arises with the linear probability model is that
the error variance is not constant (heteroscedasticity). One remedy
for this problem is to calculate a weighted least squares regression:

```r
# calculate weights

intercept <- coef(lp)[1]
intercept
reg.coeff <- coef(lp)[2]
reg.coeff
lp.yhat <- intercept+reg.coeff*x
wt <- 1/(lp.yhat*(1-lp.yhat))

# estimate weighted least squares model

wlp <- lm(y~1+x,weights=wt)
summary(wlp)
```

* Here is the output:

```rout
> # calculate weights
> 
> intercept <- coef(lp)[1]
> intercept
(Intercept) 
  0.2126697 
> reg.coeff <- coef(lp)[2]
> reg.coeff
        x 
-0.103974 
> lp.yhat <- intercept+reg.coeff*x
> wt <- 1/(lp.yhat*(1-lp.yhat))
> 
> # estimate weighted least squares model
> 
> wlp <- lm(y~1+x,weights=wt)
> summary(wlp)

Call:
lm(formula = y ~ 1 + x, weights = wt)

Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-0.5197 -0.5197 -0.5197 -0.3492  2.8636 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21267    0.02761   7.702 1.81e-13 ***
x           -0.10397    0.04269  -2.436   0.0154 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.003 on 311 degrees of freedom
Multiple R-squared:  0.01872,	Adjusted R-squared:  0.01556 
F-statistic: 5.932 on 1 and 311 DF,  p-value: 0.01543

> 
```

* Notice that the standard error of our intercept term has
gotten larger while the standard error for the slope coefficient
has gotten smaller.

* Weighted least squares is a minimization (hence, the term "least")
problem. We can solve this problem with another grid search:

```r
# minimization exercise

a <- seq(from=0,to=1,by=0.001)
b <- seq(from=-1,to=1,by=0.001)
c <- expand.grid(a,b)

fun <- vector()
for(i in 1:nrow(c)) {
  fun[i] <- sum(wt*(y-(c$Var1[i]+c$Var2[i]*x))^2)
  }

min.df <- data.frame(c$Var1,c$Var2,fun)[order(fun),]    
head(min.df,n=30)
```

* Here are the results:

```rout
> # minimization exercise
> 
> a <- seq(from=0,to=1,by=0.001)
> b <- seq(from=-1,to=1,by=0.001)
> c <- expand.grid(a,b)
> 
> fun <- vector()
> for(i in 1:nrow(c)) {
+   fun[i] <- sum(wt*(y-(c$Var1[i]+c$Var2[i]*x))^2)
+   }
> 
> min.df <- data.frame(c$Var1,c$Var2,fun)[order(fun),]    
> head(min.df,n=30)
       c.Var1 c.Var2      fun
897110  0.213 -0.104 313.0002
896109  0.213 -0.105 313.0006
898110  0.212 -0.103 313.0007
897109  0.212 -0.104 313.0011
898111  0.213 -0.103 313.0018
899111  0.212 -0.102 313.0022
896110  0.214 -0.105 313.0024
895109  0.214 -0.106 313.0028
895108  0.213 -0.106 313.0029
896108  0.212 -0.105 313.0033
899110  0.211 -0.102 313.0038
897111  0.214 -0.104 313.0040
898109  0.211 -0.103 313.0041
894108  0.214 -0.107 313.0051
899112  0.213 -0.102 313.0052
900111  0.211 -0.101 313.0053
900112  0.212 -0.101 313.0056
897108  0.211 -0.104 313.0064
894107  0.213 -0.107 313.0070
895110  0.215 -0.106 313.0073
898112  0.214 -0.103 313.0074
895107  0.212 -0.106 313.0075
894109  0.215 -0.107 313.0076
901112  0.211 -0.100 313.0087
896111  0.215 -0.105 313.0088
893107  0.214 -0.108 313.0092
900110  0.210 -0.101 313.0095
899109  0.210 -0.102 313.0099
893108  0.215 -0.108 313.0099
900113  0.213 -0.101 313.0105
> 
```

* Now, let's see how WLS differs from simple
linear regression in the context of repeated sampling.
* Let's assume we have a population regression function
that looks like the Minneapolis study. 

```r
# let's construct a simulation study to look at 
# the repeated sample performance of the ols and 
# wls estimators

b0 <- vector()
bx <- vector()
se.b0 <- vector()
se.bx <- vector()

wb0 <- vector()
wbx <- vector()
se.wb0 <- vector()
se.wbx <- vector()

for(j in 1:10000) {
  u <- runif(n=313,min=0,max=1)
  ostar <- -1.3089-0.7952*x+log(u/(1-u))
  o <- rep(NA,313)
  o[ostar>0] <- 1
  o[ostar<0] <- 0
  sm1 <- lm(o~1+x)
  b0[j] <- coef(sm1)[1]
  bx[j] <- coef(sm1)[2]
  se.b0[j] <- sqrt(diag(vcov(sm1)))[1]
  se.bx[j] <- sqrt(diag(vcov(sm1)))[2]
  p.hat <- coef(sm1)[1]+coef(sm1)[2]*x
  w <- 1/(p.hat*(1-p.hat))
  sm2 <- lm(o~1+x,weights=w)
  wb0[j] <- coef(sm2)[1]
  wbx[j] <- coef(sm2)[2]
  se.wb0[j] <- sqrt(diag(vcov(sm2)))[1]
  se.wbx[j] <- sqrt(diag(vcov(sm2)))[2]
  }
```

* Next, when this loop finishes, we can look at 
some of the resulting quantities:

```r
# intercept estimates

mean(b0)
sd(b0)
mean(se.b0)

mean(wb0)
sd(wb0)
mean(se.wb0)

# slope estimates

mean(bx)
sd(bx)
mean(se.bx)

mean(wbx)
sd(wbx)
mean(se.wbx)
```

* Here are the results:

```rout
> # intercept estimates
> 
> mean(b0)
[1] 0.2125652
> sd(b0)
[1] 0.02729389
> mean(se.b0)
[1] 0.02574354
> 
> mean(wb0)
[1] 0.2125652
> sd(wb0)
[1] 0.02729389
> mean(se.wb0)
[1] 0.02751639
> 
> # slope estimates
> 
> mean(bx)
[1] -0.1038717
> sd(bx)
[1] 0.04208526
> mean(se.bx)
[1] 0.04748392
> 
> mean(wbx)
[1] -0.1038717
> sd(wbx)
[1] 0.04208526
> mean(se.wbx)
[1] 0.04238425
> 
```

* Now, we turn to the task of model selection. We've
spent a good bit of time talking about the likelihood
ratio test but that only works when we can write the
constrained model as a special case of the free model.

* There are some other options:

```r
# Akaike Information Criterion (AIC)
# choose specification that maximizes AIC

aic.lp <- logLik(lp)-3
aic.lp
aic.lr <- logLik(lr)-2
aic.lr

# Bayesian Information (Schwarz) Criterion (BIC)
# choose specification that maximizes BIC

bic.lp <- logLik(lp)-(3/2)*log(313)
bic.lp
bic.lr <- logLik(lr)-(2/2)*log(313)
bic.lr
```

* Here are the results:

```rout
> # Akaike Information Criterion (AIC)
> # choose specification that maximizes AIC
> 
> aic.lp <- logLik(lp)-3
> aic.lp
'log Lik.' -146.7486 (df=3)
> aic.lr <- logLik(lr)-2
> aic.lr
'log Lik.' -147.9891 (df=2)
> 
> # Bayesian Information (Schwarz) Criterion (BIC)
> # choose specification that maximizes BIC
> 
> bic.lp <- logLik(lp)-(3/2)*log(313)
> bic.lp
'log Lik.' -152.3679 (df=3)
> bic.lr <- logLik(lr)-(2/2)*log(313)
> bic.lr
'log Lik.' -151.7353 (df=2)
> 
```

* The formulas for AIC and BIC differ in different textbooks and
articles. The ones I am using here come from Wasserman (2000; [link](https://www.sciencedirect.com/science/article/abs/pii/S0022249699912786) which you should look at (especially section 6).
* Next, we consider the interpretation of the results in terms of the classical treatment effect 
(relative risk and odds ratios could also be calculated.

```r
# p(y=1|x=0) - logit form

exp(-1.3089)/(1+exp(-1.3089))

# p(y=1|x=1) - logit form

exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))

# delta - logit form

exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))-exp(-1.3089)/(1+exp(-1.3089))

# p(y=1|x=0) - probit form

pnorm(-0.7972)
z <- rnorm(n=1000000,mean=0,sd=1)
ecdf(z)(-0.7972)

# p(y=1|x=1) - probit form

pnorm(-0.7972-0.4363)
ecdf(z)(-0.7972-0.4363)

# delta - probit form

pnorm(-0.7972-0.4363)-pnorm(-0.7972)
```

* Here are the results:

```rout
> # p(y=1|x=0) - logit form
> 
> exp(-1.3089)/(1+exp(-1.3089))
[1] 0.212671
> 
> # p(y=1|x=1) - logit form
> 
> exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))
[1] 0.108699
> 
> # delta - logit form
> 
> exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))-exp(-1.3089)/(1+exp(-1.3089))
[1] -0.103972
> 
> # p(y=1|x=0) - probit form
> 
> pnorm(-0.7972)
[1] 0.2126674
> z <- rnorm(n=1000000,mean=0,sd=1)
> ecdf(z)(-0.7972)
[1] 0.213002
> 
> # p(y=1|x=1) - probit form
> 
> pnorm(-0.7972-0.4363)
[1] 0.1086946
> ecdf(z)(-0.7972-0.4363)
[1] 0.109067
> 
> # delta - probit form
> 
> pnorm(-0.7972-0.4363)-pnorm(-0.7972)
[1] -0.1039728
> 
```

* #### Assignment Due Thursday 9/30/21

* Calculate logistic, probit, and linear probability estimates using the treatment-as-delivered data.
* Use the grid search process to verify the maximum likelihood solutions.
* Calculate the WLS estimate.
* Use the simulator to verify the WLS efficiency property (based on a population treatment-as-delivered specification).
* Use logistic, probit, and linear probability models to calculate classical treatment effects, relative risks, and odds ratios.
* Based on AIC and BIC which specification is preferred?
* Reading for this week (9/30/21) and next week (10/7/21) is [here](https://gking.harvard.edu/files/gking/files/ISQ33.pdf).

### Lesson 5 - Thursday 9/30/21

* Tonight's topic: interpretation of multi-category independent variables in logistic regression models.
* We begin by reading in the dataset:

```r
# Minneapolis Data Set [N = 313; See Table 2; B&S (1988)].
 
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
 
df <- data.frame(ta,td,aggcirc,y)
table(y,ta)
```

* Here is the output:

```rout
> # Minneapolis Data Set [N = 313; See Table 2; B&S (1988)].
> 
> ta <- c(rep(1,63),rep(1,1),rep(1,28),rep(2,18),rep(2,45),
+          rep(2,4),rep(2,39),rep(2,2),rep(3,22),rep(3,2),
+          rep(3,40),rep(3,4),rep(3,3),rep(3,42))
> 
> td <- c(rep(1,63),rep(3,1),rep(1,28),rep(1,18),rep(2,45),
+          rep(3,4),rep(2,39),rep(3,2),rep(1,22),rep(2,2),
+          rep(3,40),rep(1,4),rep(2,3),rep(3,42))
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
> df <- data.frame(ta,td,aggcirc,y)
> table(y,ta)
   ta
y    1  2  3
  0 82 87 87
  1 10 21 26
>
```

* A chi-square test can be used to check on the correspondence between observed frequencies in this table and the frequencies that would be expected if the assigned treatment and outcomes are independent of each other. Step 1 is to identify the observed frequencies:

```r
f01 <- 82
f02 <- 87
f03 <- 87
f11 <- 10
f12 <- 21
f13 <- 26
```

* Next, we calculate the frequencies we would expect to see in each cell if the assigned treatment and outcomes are independent:

```r
e01 <- 92*256/313
e01
e02 <- 108*256/313
e02
e03 <- 113*256/313
e03

e11 <- 92*57/313
e11
e12 <- 108*57/313
e12
e13 <- 113*57/313
e13
```

* Then, for each cell of the table, we calculate the squared difference between observed and expected frequencies divided by the the expected cell frequency:

```r
oe01 <- (f01-e01)^2/e01
oe01
oe02 <- (f02-e02)^2/e02
oe02
oe03 <- (f03-e03)^2/e03
oe03
oe11 <- (f11-e11)^2/e11
oe11
oe12 <- (f12-e12)^2/e12
oe12
oe13 <- (f13-e13)^2/e13
oe13
```

* Last, we sum across these quantities to get our test statistic:

```r
oe01+oe02+oe03+oe11+oe12+oe13
```

* Here are the results:

```rout
> f01 <- 82
> f02 <- 87
> f03 <- 87
> f11 <- 10
> f12 <- 21
> f13 <- 26
> 
> e01 <- 92*256/313
> e01
[1] 75.24601
> e02 <- 108*256/313
> e02
[1] 88.33227
> e03 <- 113*256/313
> e03
[1] 92.42173
> 
> e11 <- 92*57/313
> e11
[1] 16.75399
> e12 <- 108*57/313
> e12
[1] 19.66773
> e13 <- 113*57/313
> e13
[1] 20.57827
> 
> oe01 <- (f01-e01)^2/e01
> oe01
[1] 0.6062306
> oe02 <- (f02-e02)^2/e02
> oe02
[1] 0.02009389
> oe03 <- (f03-e03)^2/e03
> oe03
[1] 0.3180541
> oe11 <- (f11-e11)^2/e11
> oe11
[1] 2.72272
> oe12 <- (f12-e12)^2/e12
> oe12
[1] 0.09024625
> oe13 <- (f13-e13)^2/e13
> oe13
[1] 1.428453
> 
> oe01+oe02+oe03+oe11+oe12+oe13
[1] 5.185798
> 
```

* We can confirm these results:

```r
table(y,ta)
chisq.test(table(y,ta))
```

which yields:

```rout
> table(y,ta)
   ta
y    1  2  3
  0 82 87 87
  1 10 21 26
> chisq.test(table(y,ta))

	Pearson's Chi-squared test

data:  table(y, ta)
X-squared = 5.1858, df = 2, p-value = 0.0748

> 
```

* Next, we check on a logistic regression specification summarizing the information in this table. We start by creating "dummy" or indicator variables for the arrest and advise conditions (the separate condition is the suppressed or reference category):

```r
arrest <- rep(NA,313)
arrest[ta==1] <- 1
arrest[ta!=1] <- 0
table(ta,arrest)
advise <- rep(NA,0)
advise <- rep(0,313)
advise[ta==2] <- 1
table(ta,advise)
cat <- paste(ta,arrest,advise)
table(cat)
table(arrest,advise)
```

* Here are the results of the indicator variable creation process:

```rout
> arrest <- rep(NA,313)
> arrest[ta==1] <- 1
> arrest[ta!=1] <- 0
> table(ta,arrest)
   arrest
ta    0   1
  1   0  92
  2 108   0
  3 113   0
> advise <- rep(NA,0)
> advise <- rep(0,313)
> advise[ta==2] <- 1
> table(ta,advise)
   advise
ta    0   1
  1  92   0
  2   0 108
  3 113   0
> cat <- paste(ta,arrest,advise)
> table(cat)
cat
1 1 0 2 0 1 3 0 0 
   92   108   113 
> table(arrest,advise)
      advise
arrest   0   1
     0 113 108
     1  92   0
> 
```

* Now, here is an intercept-only logistic regression model:

```r
const <- glm(y~1,family=binomial(link="logit"))
summary(const)
logLik(const)
table(y)
```

yielding these results:


```rout
> summary(const)

Call:
glm(formula = y ~ 1, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6341  -0.6341  -0.6341  -0.6341   1.8456  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.5021     0.1465  -10.26   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 297.08  on 312  degrees of freedom
AIC: 299.08

Number of Fisher Scoring iterations: 4

> logLik(const)
'log Lik.' -148.5423 (df=1)
> 
>
> table(y)
y
  0   1 
256  57 
> 
```

* Now, let's notice the following equivalence:

```rout
> 57/(256+57)
[1] 0.1821086
> exp(-1.5021)/(1+exp(-1.5021))
[1] 0.1821125
> 
```

* Next, we estimate the "free" model allowing the failure rate to vary between the treatment-as-assigned groups:

```r
free <- glm(y~1+arrest+advise,family=binomial(link="logit"))
summary(free)
logLik(free)
```
* Here are the results:

```rout
> free <- glm(y~1+arrest+advise,family=binomial(link="logit"))
> summary(free)

Call:
glm(formula = y ~ 1 + arrest + advise, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7232  -0.7232  -0.6576  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.2078     0.2235  -5.404 6.52e-08 ***
arrest       -0.8963     0.4027  -2.226    0.026 *  
advise       -0.2136     0.3303  -0.647    0.518    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.56  on 310  degrees of freedom
AIC: 297.56

Number of Fisher Scoring iterations: 4

> logLik(free)
'log Lik.' -145.7792 (df=3)
> 
```

* Let's reconsider the treatment-as-assigned and failure contingency table:

```rout
> table(y,ta)
   ta
y    1  2  3
  0 82 87 87
  1 10 21 26
> 
```

* Using the information in this table, we can calculate the failure rates for each of the three groups:

```rout
> 10/(82+10)
[1] 0.1086957
> 21/(87+21)
[1] 0.1944444
> 26/(87+26)
[1] 0.2300885
> 
```

* We can use the logistic regression model parameter estimates to recover these same failure rates (but for rounding error:

```rout
> logit.arrested <- -1.2078+(-0.8963)
> logit.arrested
[1] -2.1041
> exp(logit.arrested)/(1+exp(logit.arrested))
[1] 0.108699
> 
> logit.advised <- -1.2078+(-0.2136)
> logit.advised
[1] -1.4214
> exp(logit.advised)/(1+exp(logit.advised))
[1] 0.1944422
> 
> logit.separated <- -1.2078
> exp(logit.separated)/(1+exp(logit.separated))
[1] 0.2300905
> 
```

* Notice how `logit.separated` is calculated using only the intercept term from the regression model.
* Next, we calculate the log-likelihood ratio test comparing the constrained and free models:

```r
> logLik(const)
'log Lik.' -148.5423 (df=1)
> logLik(free)
'log Lik.' -145.7792 (df=3)
> logLik(const)-logLik(free)
'log Lik.' -2.763028 (df=1)
> -2*(logLik(const)-logLik(free))
'log Lik.' 5.526057 (df=1)
> pchisq(q=5.526057,df=2)
[1] 0.9368996
> 1-pchisq(q=5.526057,df=2)
[1] 0.06310038
> 
```

* The obtained test statistic has a value of 5.526 with 2 degrees of freedom (constrained model has one parameter estimate and the free model has 3 parameter estimates so the difference between them is 2).
* The p-value for the test is 0.063 which is higher than 0.05. We conclude that the constrained model is more consistent with the data.
* Next, we add a covariate (aggravating circumstances) to the analysis:

```r
free.agg <- glm(y~1+arrest+advise+aggcirc,family=binomial(link="logit"))
summary(free.agg)
logLik(free.agg)
```

* This analysis yields the following results:

```rout
> free.agg <- glm(y~1+arrest+advise+aggcirc,family=binomial(link="logit"))
> summary(free.agg)

Call:
glm(formula = y ~ 1 + arrest + advise + aggcirc, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7513  -0.7012  -0.6397  -0.4686   2.1278  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.1206     0.2776  -4.036 5.44e-05 ***
arrest       -0.8765     0.4045  -2.167   0.0303 *  
advise       -0.2052     0.3308  -0.620   0.5350    
aggcirc      -0.1569     0.3016  -0.520   0.6028    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.29  on 309  degrees of freedom
AIC: 299.29

Number of Fisher Scoring iterations: 4

> logLik(free.agg)
'log Lik.' -145.6446 (df=4)
> 
```

* We now consider the meaning of the intercept term in this new model. The estimate of -1.1206 corresponds to the logit for persons in the separate group with no aggravating circumstances. Unlike the earlier models -- which were saturated (meaning we can perfectly reproduce the frequency table cells from the logit parameter estimates) -- this model is not saturated. We can check to see what failure rate is predicted for the separate group with no aggravating circumstances (versus the rate that is actually observed):

```rout
> table(y,ta,aggcirc)
, , aggcirc = 0

   ta
y    1  2  3
  0 25 32 37
  1  3  9 12

, , aggcirc = 1

   ta
y    1  2  3
  0 57 55 50
  1  7 12 14

> 12/(37+12)
[1] 0.244898
> exp(-1.1206)/(1+exp(-1.1206))
[1] 0.2459
> 
```

* As we can see, the rates are not very different (but they are not identical).
* Let's compare this analysis to a saturated model:

```rout
> free.agg.int <- glm(y~1+arrest+advise+aggcirc+aggcirc*arrest+aggcirc*advise,family=binomial(link="logit"))
> summary(free.agg.int)

Call:
glm(formula = y ~ 1 + arrest + advise + aggcirc + aggcirc * arrest + 
    aggcirc * advise, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7495  -0.7026  -0.6283  -0.4761   2.1136  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -1.1260     0.3322  -3.390   0.0007 ***
arrest          -0.9943     0.6955  -1.430   0.1528    
advise          -0.1425     0.5027  -0.283   0.7768    
aggcirc         -0.1470     0.4492  -0.327   0.7436    
arrest:aggcirc   0.1701     0.8576   0.198   0.8428    
advise:aggcirc  -0.1070     0.6676  -0.160   0.8727    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.19  on 307  degrees of freedom
AIC: 303.19

Number of Fisher Scoring iterations: 4

> exp(-1.1260)/(1+exp(-1.1260))
[1] 0.2449
> table(y,ta,aggcirc)
, , aggcirc = 0

   ta
y    1  2  3
  0 25 32 37
  1  3  9 12

, , aggcirc = 1

   ta
y    1  2  3
  0 57 55 50
  1  7 12 14

> 12/(37+12)
[1] 0.244898
> 
```

* The log-likelihood ratio test comparing the saturated model (with an interaction) to the main-effects only model is:

```rout
> logLik(free.agg)
'log Lik.' -145.6446 (df=4)
> logLik(free.agg.int)
'log Lik.' -145.5942 (df=6)
> -2*(-145.6446-(-145.5942))
[1] 0.1008
```

* With 2 degrees of freedom, this is not a significant difference at any conventional significance level.
* Next, let's check the AIC and BIC results:

```
# AIC results:

> logLik(free.agg)-4
'log Lik.' -149.6446 (df=4)
> logLik(free.agg.int)-6
'log Lik.' -151.5942 (df=6)
> 

# BIC results:

> logLik(free.agg)-4/2*log(313)
'log Lik.' -157.137 (df=4)
> logLik(free.agg.int)-6/2*log(313)
'log Lik.' -162.8328 (df=6)
```

* Based on both AIC and BIC, we would choose the `free.agg model` over the `free.agg.int` model.

##### Assignment Due Thursday 10/7/21

1. Use the treatment as delivered and outcome data to construct a chi-square test of independence.
2. Compare the results of the chi-square test of independence to the likelihood ratio chi-square test.
3. Calculate the failure rates for each of the treatment delivered groups using the contingency table approach compared to the rates implied by the logistic regression analysis.
4. Add the aggravating circumstances variable to the logistic regression model. Interpret the results.
5. Add an interaction term to the logit model you estimated in part 4 to get a saturated model. Test for whether the saturated model or the main-effects-only model is more consistent with the data.
6. Use AIC and BIC to check on the same question you addressed in part 5. Interpret your results.

### Lesson 6 - Thursday 10/7/21

* I have decided I want to spend some more time on interpreting results from logistic regression models and coefficients.
* We begin by reading in the nc78.csv dataset I sent you today.

```r
# read in our dataset

df <- read.csv(file="nc78.csv",sep=",",header=T)

# look at a random sample of cases

df[sample(nrow(df),size=25,replace=F), ]
```

and here is what we get (your sample will be different from mine):

```rout
> # read in our dataset
> 
> df <- read.csv(file="nc78.csv",sep=",",header=T)
> 
> # look at a random sample of cases
> 
> df[sample(nrow(df),size=25,replace=F), ]
        X male ageyears recid
4870 4870    1       57     1
7989 7989    1       34     0
8612 8612    1       21     0
1835 1835    0       33     1
5830 5830    1       44     0
331   331    1       38     0
4095 4095    1       40     0
68     68    0       30     0
1189 1189    1       30     0
2860 2860    1       30     0
7106 7106    1       31     1
6078 6078    1       31     0
6689 6689    1       19     1
1616 1616    1       34     0
3284 3284    1       26     1
4524 4524    1       34     0
4080 4080    1       25     0
6101 6101    1       17     1
2799 2799    1       25     1
684   684    1       19     0
8157 8157    1       18     1
1481 1481    1       54     1
1556 1556    1       60     0
2882 2882    1       24     1
5535 5535    1       18     0
> 
```

* Now, we are going to look at the marginal distributions of our variables.
* Let's begin with the dependent variable, recidivism (measured over a 70-month follow-up period):

```r
# recidivism (within 70 months)

table(df$recid,exclude=NULL)
table(df$recid)/nrow(df)
```

* Here are the results:

```rout
> # recidivism (within 70 months)
> 
> table(df$recid,exclude=NULL)

   0    1 
5800 3527 
> table(df$recid)/nrow(df)

        0         1 
0.6218505 0.3781495 
> 
```

* Next, we turn to the distribution of males/females in the data and the joint distribution of sex and recidivism:

```r
# sex distribution

table(df$male,exclude=NULL)
sd <- table(df$recid,df$male,exclude=NULL)
sd

py1x1 <- sd[2,2]/(sd[1,2]+sd[2,2])
py1x1
py1x0 <- sd[2,1]/(sd[1,1]+sd[2,1])
py1x0
py1x1-py1x0
```

* This what we get:

```rout
> # sex distribution
> 
> table(df$male,exclude=NULL)

   0    1 
 469 8858 
> sd <- table(df$recid,df$male,exclude=NULL)
> sd
   
       0    1
  0  367 5433
  1  102 3425
> 
> py1x1 <- sd[2,2]/(sd[1,2]+sd[2,2])
> py1x1
[1] 0.3866561
> py1x0 <- sd[2,1]/(sd[1,1]+sd[2,1])
> py1x0
[1] 0.217484
> py1x1-py1x0
[1] 0.1691721
> 
```

* Now, we estimate a bivariate logistic regression model to corroborate what we see in the table above:


```r
# estimate bivariate logistic regression model

summary(glm(recid~1+male,data=df,family=binomial(link="logit")))

logit.m <- -1.2804+1*0.8190
exp(logit.m)/(1+exp(logit.m))

logit.f <- -1.2804+0*0.8190
exp(logit.f)/(1+exp(logit.f))
```

* These are the logistic regression results:

```rout
> # estimate bivariate logistic regression model
> 
> summary(glm(recid~1+male,data=df,family=binomial(link="logit")))

Call:
glm(formula = recid ~ 1 + male, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.9888  -0.9888  -0.9888   1.3786   1.7468  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.2804     0.1119 -11.439  < 2e-16 ***
male          0.8190     0.1140   7.182 6.88e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12312  on 9325  degrees of freedom
AIC: 12316

Number of Fisher Scoring iterations: 4

> 
> logit.m <- -1.2804+1*0.8190
> exp(logit.m)/(1+exp(logit.m))
[1] 0.3866538
> 
> logit.f <- -1.2804+0*0.8190
> exp(logit.f)/(1+exp(logit.f))
[1] 0.2174821
> 
```

* Now, we turn to the age distribution in the dataset. We start by opening a plot window and generating a barplot showing the age distribution of the persons released from North Carolina prisons in 1978:

```r
# open a 1-row, 3-column plot window

par(mfrow=c(1,3))

# age distribution

table(df$ageyears,exclude=NULL)
median(df$ageyears)
mean(df$ageyears)
barplot(table(df$ageyears),
  xlab="Age (in years)",
  ylab="Number of People")
```

* Here are the results:

```rout
> # open a 1-row, 3-column plot window
> 
> par(mfrow=c(1,3))
> 
> # age distribution
> 
> table(df$ageyears,exclude=NULL)

 16  17  18  19  20  21  22  23  24  25  26  27  28  29  30 
 19 161 492 480 624 599 580 468 537 443 432 338 415 292 324 
 31  32  33  34  35  36  37  38  39  40  41  42  43  44  45 
254 234 179 187 167 177 132 152 117 119  93 113 102  85  75 
 46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
 90  72  86  62  78  61  57  50  44  49  55  34  34  25  21 
 61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
 18  19  11  16   7   5  13   5   3   1   3   5   3   4   2 
 77  78 
  2   2 
> median(df$ageyears)
[1] 26
> mean(df$ageyears)
[1] 29.32787
> barplot(table(df$ageyears),
+   xlab="Age (in years)",
+   ylab="Number of People")
> 
```

* Next, we show how age is related to the fraction of persons who were observed to recidivate within 70 months:

```r
df$age.factor <- factor(df$ageyears,levels=16:78)
table(df$recid,df$age.factor,exclude=NULL)

fail.age <- table(df$recid,df$age.factor)[2,]
fail.age
n.age <- (table(df$recid,df$age.factor)[1,]+table(df$recid,df$age.factor)[2,])
n.age

pfail.age <- fail.age/n.age
pfail.age

plot(x=16:78,y=pfail.age,
  type="l",lty=1,lwd=2,
  xlab="Age at Release (in years)",
  ylab="Proportion Observed to Fail Within 70 Months")
abline(h=seq(from=0,to=1,by=0.1),lty=2,lwd=0.5)
abline(v=seq(from=0,to=80,by=10),lty=2,lwd=0.5)
```

* Here are the tabled results:

```rout
> df$age.factor <- factor(df$ageyears,levels=16:78)
> table(df$recid,df$age.factor,exclude=NULL)
   
     16  17  18  19  20  21  22  23  24  25  26  27  28  29
  0   9  78 242 246 338 362 339 311 313 279 262 235 251 173
  1  10  83 250 234 286 237 241 157 224 164 170 103 164 119
   
     30  31  32  33  34  35  36  37  38  39  40  41  42  43
  0 208 152 149 123 133 111 118  85 108  81  80  66  84  67
  1 116 102  85  56  54  56  59  47  44  36  39  27  29  35
   
     44  45  46  47  48  49  50  51  52  53  54  55  56  57
  0  58  52  64  52  60  43  54  42  39  37  35  36  39  27
  1  27  23  26  20  26  19  24  19  18  13   9  13  16   7
   
     58  59  60  61  62  63  64  65  66  67  68  69  70  71
  0  25  18  18  18  13   7  12   7   5  11   5   0   1   2
  1   9   7   3   0   6   4   4   0   0   2   0   3   0   1
   
     72  73  74  75  76  77  78
  0   5   2   4   2   0   2   2
  1   0   1   0   0   0   0   0
> 
> fail.age <- table(df$recid,df$age.factor)[2,]
> fail.age
 16  17  18  19  20  21  22  23  24  25  26  27  28  29  30 
 10  83 250 234 286 237 241 157 224 164 170 103 164 119 116 
 31  32  33  34  35  36  37  38  39  40  41  42  43  44  45 
102  85  56  54  56  59  47  44  36  39  27  29  35  27  23 
 46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
 26  20  26  19  24  19  18  13   9  13  16   7   9   7   3 
 61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
  0   6   4   4   0   0   2   0   3   0   1   0   1   0   0 
 76  77  78 
  0   0   0 
> n.age <- (table(df$recid,df$age.factor)[1,]+table(df$recid,df$age.factor)[2,])
> n.age
 16  17  18  19  20  21  22  23  24  25  26  27  28  29  30 
 19 161 492 480 624 599 580 468 537 443 432 338 415 292 324 
 31  32  33  34  35  36  37  38  39  40  41  42  43  44  45 
254 234 179 187 167 177 132 152 117 119  93 113 102  85  75 
 46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
 90  72  86  62  78  61  57  50  44  49  55  34  34  25  21 
 61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
 18  19  11  16   7   5  13   5   3   1   3   5   3   4   2 
 76  77  78 
  0   2   2 
> 
> pfail.age <- fail.age/n.age
> pfail.age
       16        17        18        19        20        21 
0.5263158 0.5155280 0.5081301 0.4875000 0.4583333 0.3956594 
       22        23        24        25        26        27 
0.4155172 0.3354701 0.4171322 0.3702032 0.3935185 0.3047337 
       28        29        30        31        32        33 
0.3951807 0.4075342 0.3580247 0.4015748 0.3632479 0.3128492 
       34        35        36        37        38        39 
0.2887701 0.3353293 0.3333333 0.3560606 0.2894737 0.3076923 
       40        41        42        43        44        45 
0.3277311 0.2903226 0.2566372 0.3431373 0.3176471 0.3066667 
       46        47        48        49        50        51 
0.2888889 0.2777778 0.3023256 0.3064516 0.3076923 0.3114754 
       52        53        54        55        56        57 
0.3157895 0.2600000 0.2045455 0.2653061 0.2909091 0.2058824 
       58        59        60        61        62        63 
0.2647059 0.2800000 0.1428571 0.0000000 0.3157895 0.3636364 
       64        65        66        67        68        69 
0.2500000 0.0000000 0.0000000 0.1538462 0.0000000 1.0000000 
       70        71        72        73        74        75 
0.0000000 0.3333333 0.0000000 0.3333333 0.0000000 0.0000000 
       76        77        78 
      NaN 0.0000000 0.0000000 
> 
```

* Next, we calculate a logistic regression model measuring the relationship between age-at-release and recidivism risk. Note that this code will add a purple smooth curve to the age-recidivism plot below:

```r
age.model <- glm(recid~1+ageyears,data=df,family=binomial(link="logit"))
summary(age.model)
x.age <- seq(from=16,to=78,by=1)
logit.age <- 0.250539-0.025866*x.age
pred.age <- exp(logit.age)/(1+exp(logit.age))
lines(x=x.age,y=pred.age,lty=1,lwd=2,col="purple")
```

* Here are the logistic regression results:

```rout
> age.model <- glm(recid~1+ageyears,data=df,family=binomial(link="logit"))
> summary(age.model)

Call:
glm(formula = recid ~ 1 + ageyears, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1089  -1.0145  -0.8766   1.3153   1.9054  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.250539   0.065699   3.813 0.000137 ***
ageyears    -0.025866   0.002181 -11.860  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12221  on 9325  degrees of freedom
AIC: 12225

Number of Fisher Scoring iterations: 4

> x.age <- seq(from=16,to=78,by=1)
> logit.age <- 0.250539-0.025866*x.age
> pred.age <- exp(logit.age)/(1+exp(logit.age))
> lines(x=x.age,y=pred.age,lty=1,lwd=2,col="purple")
```

* And, then we calculate the derivative of the logistic response function (a model that only includes age):

```r
# calculate the derivative of the logistic response function at the median
# derivative of the logistic response function is b*p*(1-p) (King, 1989:109)

data.frame(x.age,logit.age,pred.age)

b <- -0.025866
p <- 0.3960438
derivative <- b*p*(1-p)
derivative
```

* Here are the results:

```rout
> # calculate the derivative of the logistic response function at the median
> # derivative of the logistic response function is b*p*(1-p) (King, 1989:109)
> 
> data.frame(x.age,logit.age,pred.age)
   x.age logit.age  pred.age
1     16 -0.163317 0.4592613
2     17 -0.189183 0.4528448
3     18 -0.215049 0.4464440
4     19 -0.240915 0.4400609
5     20 -0.266781 0.4336975
6     21 -0.292647 0.4273560
7     22 -0.318513 0.4210382
8     23 -0.344379 0.4147462
9     24 -0.370245 0.4084818
10    25 -0.396111 0.4022471
11    26 -0.421977 0.3960438
12    27 -0.447843 0.3898737
13    28 -0.473709 0.3837387
14    29 -0.499575 0.3776406
15    30 -0.525441 0.3715808
16    31 -0.551307 0.3655612
17    32 -0.577173 0.3595833
18    33 -0.603039 0.3536487
19    34 -0.628905 0.3477589
20    35 -0.654771 0.3419152
21    36 -0.680637 0.3361191
22    37 -0.706503 0.3303720
23    38 -0.732369 0.3246751
24    39 -0.758235 0.3190296
25    40 -0.784101 0.3134367
26    41 -0.809967 0.3078975
27    42 -0.835833 0.3024131
28    43 -0.861699 0.2969845
29    44 -0.887565 0.2916126
30    45 -0.913431 0.2862983
31    46 -0.939297 0.2810424
32    47 -0.965163 0.2758457
33    48 -0.991029 0.2707089
34    49 -1.016895 0.2656327
35    50 -1.042761 0.2606176
36    51 -1.068627 0.2556643
37    52 -1.094493 0.2507732
38    53 -1.120359 0.2459447
39    54 -1.146225 0.2411793
40    55 -1.172091 0.2364772
41    56 -1.197957 0.2318389
42    57 -1.223823 0.2272644
43    58 -1.249689 0.2227540
44    59 -1.275555 0.2183078
45    60 -1.301421 0.2139260
46    61 -1.327287 0.2096085
47    62 -1.353153 0.2053554
48    63 -1.379019 0.2011666
49    64 -1.404885 0.1970421
50    65 -1.430751 0.1929817
51    66 -1.456617 0.1889853
52    67 -1.482483 0.1850527
53    68 -1.508349 0.1811836
54    69 -1.534215 0.1773778
55    70 -1.560081 0.1736350
56    71 -1.585947 0.1699549
57    72 -1.611813 0.1663371
58    73 -1.637679 0.1627811
59    74 -1.663545 0.1592867
60    75 -1.689411 0.1558533
61    76 -1.715277 0.1524805
62    77 -1.741143 0.1491678
63    78 -1.767009 0.1459147
> 
> b <- -0.025866
> p <- 0.3960438
> derivative <- b*p*(1-p)
> derivative
[1] -0.006186969
> 
```

* Finally, we create a boxplot showing the relationship between age at the time of release (in years) and sex (male/female):

```r
# joint age x gender distribution

boxplot(df$ageyears~df$male,
  xlab="Sex",
  ylab="Age (in years)",
  names=c("Females","Males"))
```

* The resulting 1x3 plotspace is here:

<p align="left">
<img src="/gfiles/age-recid.png" width="800px">
</p>

* Now, we estimate a logistic regresssion analysis with both sex and age as predictor variables. Here is the R code:

```r
# now let's estimate a logistic regression model using age and 
# sex as predictor variables.

m1 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
summary(m1)
logLik(m1)
```

and here are the results:

```rout
> # now let's estimate a logistic regression model using age and 
> # sex as predictor variables.
> 
> m1 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
> summary(m1)

Call:
glm(formula = recid ~ 1 + male + ageyears, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1276  -1.0304  -0.8503   1.2970   2.0321  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.555808   0.126760  -4.385 1.16e-05 ***
male         0.860076   0.114624   7.503 6.22e-14 ***
ageyears    -0.026411   0.002188 -12.071  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12156  on 9324  degrees of freedom
AIC: 12162

Number of Fisher Scoring iterations: 4

> logLik(m1)
'log Lik.' -6078.234 (df=3)
> 
```

* We now ask what this model implies about the risk of failure for males and females.
* The answer depends on what the value of age is.
* As an example, let's set the age to the median value of 26.

```r
logit.male <- -0.555808+1*0.860076-0.026411*26
pmale <- exp(logit.male)/(1+exp(logit.male))

logit.female <- -0.555808+0*0.860076-0.026411*26
pfemale <- exp(logit.female)/(1+exp(logit.female))

pmale-pfemale
```

* Here is what we get:

```rout
> logit.male <- -0.555808+1*0.860076-0.026411*26
> pmale <- exp(logit.male)/(1+exp(logit.male))
> 
> logit.female <- -0.555808+0*0.860076-0.026411*26
> pfemale <- exp(logit.female)/(1+exp(logit.female))
> 
> pmale-pfemale
[1] 0.1815417
> 
```

* For further insight, we can evaluate the function at the first and third quartiles of the age distribution:

```r
# first quartile of the age distribution

quantile(df$ageyears,0.25)

logit.male <- -0.555808+1*0.860076-0.026411*21
pmale <- exp(logit.male)/(1+exp(logit.male))

logit.female <- -0.555808+0*0.860076-0.026411*21
pfemale <- exp(logit.female)/(1+exp(logit.female))

pmale-pfemale

# third quartile of the age distribution

quantile(df$ageyears,0.75)

logit.male <- -0.555808+1*0.860076-0.026411*34
pmale <- exp(logit.male)/(1+exp(logit.male))

logit.female <- -0.555808+0*0.860076-0.026411*34
pfemale <- exp(logit.female)/(1+exp(logit.female))

pmale-pfemale
```

* Here are the results:

```rout
> quantile(df$ageyears,0.25)
25% 
 21 
> 
> logit.male <- -0.555808+1*0.860076-0.026411*21
> pmale <- exp(logit.male)/(1+exp(logit.male))
> 
> logit.female <- -0.555808+0*0.860076-0.026411*21
> pfemale <- exp(logit.female)/(1+exp(logit.female))
> 
> pmale-pfemale
[1] 0.1899451
> 
> # third quartile of the age distribution
> 
> quantile(df$ageyears,0.75)
75% 
 34 
> 
> logit.male <- -0.555808+1*0.860076-0.026411*34
> pmale <- exp(logit.male)/(1+exp(logit.male))
> 
> logit.female <- -0.555808+0*0.860076-0.026411*34
> pfemale <- exp(logit.female)/(1+exp(logit.female))
> 
> pmale-pfemale
[1] 0.1663648
> 
```

* Now, we ask what this model implies about the risk of failure as a function of age? Of course, the answer depends partially on what we assume about the 
value of sex (male/female). Here is the R code (which also generates a plot displayed below):

```r
# generate the age sequence

age <- seq(from=16,to=78)

# let's start with the females

logit.age.female <- -0.555808+0*0.860076-0.026411*age
p.age.female <- exp(logit.age.female)/(1+exp(logit.age.female))

# now, the males

logit.age.male <- -0.555808+1*0.860076-0.026411*age
p.age.male <- exp(logit.age.male)/(1+exp(logit.age.male))

# here is the plot

plot(x=age,y=p.age.male,type="l",lty=1,lwd=2,col="blue",ylim=c(0,1))
lines(x=age,y=p.age.female,lty=1,lwd=2,col="red")
abline(h=c(0,0.2,0.4,0.6,0.8,1.0),lty=3,lwd=0.5)
abline(v=c(20,30,40,50,60,70,80),lty=3,lwd=0.5)
```

* Here is the plot:

<p align="left">
<img src="/gfiles/age-plot.png" width="800px">
</p>

* Note that this is different from estimating an interaction effect.
* If we want to allow the logits to have different age slopes for males and females, then we need to add another parameter to the model 

```r
m2 <- glm(recid~1+male+ageyears+male*ageyears,data=df,family=binomial(link="logit"))
summary(m2)
logLik(m2)
```

* Here are the results:

```rout
> m2 <- glm(recid~1+male+ageyears+male*ageyears,data=df,family=binomial(link="logit"))
> summary(m2)

Call:
glm(formula = recid ~ 1 + male + ageyears + male * ageyears, 
    family = binomial(link = "logit"), data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1266  -1.0302  -0.8512   1.2975   2.1093  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)  
(Intercept)   -0.366302   0.383101  -0.956   0.3390  
male           0.665119   0.388916   1.710   0.0872 .
ageyears      -0.033536   0.013853  -2.421   0.0155 *
male:ageyears  0.007317   0.014030   0.522   0.6020  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12156  on 9323  degrees of freedom
AIC: 12164

Number of Fisher Scoring iterations: 4

> logLik(m2)
'log Lik.' -6078.094 (df=4)
> 
```

* Note that this is a 4-parameter model (compared to the three parameter model we considered earlier. Here is the log-likelihood ratio test:

```r
logLik(m1)
logLik(m2)

ts <- -2*(-6078.234-(-6078.094))
ts
1-pchisq(q=ts,df=1)
```

and the results are:

```rout
> logLik(m1)
'log Lik.' -6078.234 (df=3)
> logLik(m2)
'log Lik.' -6078.094 (df=4)
> 
> ts <- -2*(-6078.234-(-6078.094))
> ts
[1] 0.28
> 1-pchisq(q=ts,df=1)
[1] 0.5967012
> 
```

* Based on this evidence, we conclude that the constrained model (m1) is more consistent with the data. However, we can still interpret the 
interaction effect (between sex and age). Here is some R code:

```r
# let's start with the females

logit.age.female <- -0.366302+0*0.665119-0.033536*age+0.007317*0*age
p.age.female <- exp(logit.age.female)/(1+exp(logit.age.female))

# now, the males

logit.age.male <- -0.366302+1*0.665119-0.033536*age+0.007317*1*age
p.age.male <- exp(logit.age.male)/(1+exp(logit.age.male))

# let's add the expected failure rates from the interaction model to 
# the plot - the lines from model 2 are dashed.

lines(x=age,y=p.age.male,type="l",lty=2,lwd=2,col="blue",ylim=c(0,1))
lines(x=age,y=p.age.female,lty=2,lwd=2,col="red")
```

* The code above generates some additional information to add to the plotspace:

<p align="left">
<img src="/gfiles/age-plot2.png" width="800px">
</p>

* We can also check to see whether the derivatives of the response function / age are equal (comparing males and females) at a particular age point say, age 20.

```r
data.frame(age,p.age.male,p.age.female)

# first, the females

b <- -0.033536
p <- 0.26172501
derivative <- b*p*(1-p)
derivative

# second, the males

b <- -0.033536+0.007317
p <- 0.4438471
derivative <- b*p*(1-p)
derivative
```

* Here are the results:

```rout
> data.frame(age,p.age.male,p.age.female)
   age p.age.male p.age.female
1   16  0.4698648   0.28845943
2   17  0.4633394   0.28162527
3   18  0.4568265   0.27489047
4   19  0.4503284   0.26825658
5   20  0.4438471   0.26172501
6   21  0.4373849   0.25529698
7   22  0.4309439   0.24897359
8   23  0.4245262   0.24275576
9   24  0.4181338   0.23664429
10  25  0.4117688   0.23063981
11  26  0.4054332   0.22474284
12  27  0.3991289   0.21895372
13  28  0.3928579   0.21327271
14  29  0.3866220   0.20769990
15  30  0.3804230   0.20223527
16  31  0.3742629   0.19687869
17  32  0.3681432   0.19162991
18  33  0.3620656   0.18648857
19  34  0.3560319   0.18145421
20  35  0.3500435   0.17652626
21  36  0.3441020   0.17170407
22  37  0.3382089   0.16698690
23  38  0.3323656   0.16237392
24  39  0.3265735   0.15786422
25  40  0.3208337   0.15345682
26  41  0.3151477   0.14915068
27  42  0.3095164   0.14494468
28  43  0.3039412   0.14083766
29  44  0.2984229   0.13682839
30  45  0.2929627   0.13291560
31  46  0.2875615   0.12909796
32  47  0.2822200   0.12537412
33  48  0.2769393   0.12174268
34  49  0.2717199   0.11820221
35  50  0.2665626   0.11475124
36  51  0.2614681   0.11138831
37  52  0.2564369   0.10811189
38  53  0.2514695   0.10492046
39  54  0.2465665   0.10181250
40  55  0.2417282   0.09878643
41  56  0.2369550   0.09584071
42  57  0.2322471   0.09297377
43  58  0.2276049   0.09018403
44  59  0.2230285   0.08746993
45  60  0.2185181   0.08482989
46  61  0.2140738   0.08226235
47  62  0.2096956   0.07976575
48  63  0.2053836   0.07733854
49  64  0.2011377   0.07497916
50  65  0.1969578   0.07268610
51  66  0.1928437   0.07045782
52  67  0.1887955   0.06829282
53  68  0.1848127   0.06618960
54  69  0.1808952   0.06414671
55  70  0.1770428   0.06216266
56  71  0.1732550   0.06023603
57  72  0.1695315   0.05836540
58  73  0.1658721   0.05654937
59  74  0.1622761   0.05478655
60  75  0.1587434   0.05307560
61  76  0.1552732   0.05141517
62  77  0.1518652   0.04980395
63  78  0.1485189   0.04824066
> 
> # first, the females
> 
> b <- -0.033536
> p <- 0.26172501
> derivative <- b*p*(1-p)
> derivative
[1] -0.006479995
> 
> # second, the males
> 
> b <- -0.033536+0.007317
> p <- 0.4438471
> derivative <- b*p*(1-p)
> derivative
[1] -0.006472078
> 
```

* Note: if we want to check on the sampling distribution of the difference between these two derivatives, we can use the bootstrap:

```r
library(boot)

dd <- function(data, indices) 
  {
    d <- data[indices,]
    ms <- glm(recid~1+male+ageyears+male*ageyears,
      data=d,family=binomial(link="logit"))
    pyf.int <- coef(ms)[1]
    pyf.bm <- coef(ms)[2]
    pyf.ba <- coef(ms)[3]
    pyf.pr <- coef(ms)[4]
    logit.f <- pyf.int+pyf.bm*0+pyf.ba*20+pyf.pr*20*0
    p.f <- exp(logit.f)/(1+exp(logit.f))
    dvf <- pyf.ba*p.f*(1-p.f)
    logit.m <- pyf.int+pyf.bm*1+pyf.ba*20+pyf.pr*20*1
    p.m <- exp(logit.m)/(1+exp(logit.m))
    dvm <- (pyf.ba+pyf.pr)*p.m*(1-p.m)
    return(c(dvf,dvm))
  }

dboot <- boot(data=df,statistic=dd,R=3000)
dvf <- dboot$t[,1]
dvm <- dboot$t[,2]
delta <- dvm-dvf
quantile(delta,0.025)
quantile(delta,0.975)

# display a summary of the results
par(mfrow=c(1,2))
hist(delta)
boxplot(dvf,dvm,
  xlab="Sex",
  ylab="d_p(fail)/d_age at Age 20",
  names=c("Females","Males"))
```

* Here are the results:

```rout
> library(boot)
> 
> dd <- function(data, indices) 
+   {
+     d <- data[indices,]
+     ms <- glm(recid~1+male+ageyears+male*ageyears,
+       data=d,family=binomial(link="logit"))
+     pyf.int <- coef(ms)[1]
+     pyf.bm <- coef(ms)[2]
+     pyf.ba <- coef(ms)[3]
+     pyf.pr <- coef(ms)[4]
+     logit.f <- pyf.int+pyf.bm*0+pyf.ba*20+pyf.pr*20*0
+     p.f <- exp(logit.f)/(1+exp(logit.f))
+     dvf <- pyf.ba*p.f*(1-p.f)
+     logit.m <- pyf.int+pyf.bm*1+pyf.ba*20+pyf.pr*20*1
+     p.m <- exp(logit.m)/(1+exp(logit.m))
+     dvm <- (pyf.ba+pyf.pr)*p.m*(1-p.m)
+     return(c(dvf,dvm))
+   }
> 
> dboot <- boot(data=df,statistic=dd,R=3000)
> dvf <- dboot$t[,1]
> dvm <- dboot$t[,2]
> delta <- dvm-dvf
> quantile(delta,0.025)
      2.5% 
-0.0045852 
> quantile(delta,0.975)
      97.5% 
0.006692414 
> 
> # display a summary of the results
> par(mfrow=c(1,2))
> hist(delta)
> boxplot(dvf,dvm,
+   xlab="Sex",
+   ylab="d_p(fail)/d_age at Age 20",
+   names=c("Females","Males"))
> 
```

<p align="left">
<img src="/gfiles/deriv-plot.png" width="800px">
</p>

* One last point about the logistic regression model: we can code the likelihood function and use a general hill-climbing algorithm to maximize that function.

```r
# first we rerun the standard logistic regression analysis

m3 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
summary(m3)
logLik(m3)

library(maxLik)

ll4 <- function(parms)
  {
    a <- parms[1]
    b <- parms[2]
    c <- parms[3]
 
    logit <- a+b*df$male+c*df$ageyears

    py0 <- 1/(1+exp(logit))
    py1 <- exp(logit)/(1+exp(logit))

    pmf <- df$recid*py1+(1-df$recid)*py0
    lpmf <- log(pmf)
    return(lpmf)
  }

m4 <- maxLik(ll4,start=c(-0.32937423,0.40384020,-0.0328402402),
  method="BHHH",finalHessian="BHHH")
summary(m4)
```

* Here are the results:

```rout
> # first we rerun the standard logistic regression analysis
> 
> m3 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
> summary(m3)

Call:
glm(formula = recid ~ 1 + male + ageyears, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1276  -1.0304  -0.8503   1.2970   2.0321  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.555808   0.126760  -4.385 1.16e-05 ***
male         0.860076   0.114624   7.503 6.22e-14 ***
ageyears    -0.026411   0.002188 -12.071  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12156  on 9324  degrees of freedom
AIC: 12162

Number of Fisher Scoring iterations: 4

> logLik(m3)
'log Lik.' -6078.234 (df=3)
> 
> library(maxLik)
Loading required package: miscTools

Please cite the 'maxLik' package as:
Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.

If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
https://r-forge.r-project.org/projects/maxlik/
> 
> ll4 <- function(parms)
+   {
+     a <- parms[1]
+     b <- parms[2]
+     c <- parms[3]
+  
+     logit <- a+b*df$male+c*df$ageyears
+ 
+     py0 <- 1/(1+exp(logit))
+     py1 <- exp(logit)/(1+exp(logit))
+ 
+     pmf <- df$recid*py1+(1-df$recid)*py0
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m4 <- maxLik(ll4,start=c(-0.32937423,0.40384020,-0.0328402402),
+   method="BHHH",finalHessian="BHHH")
> summary(m4)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -6078.234 
3  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
[1,] -0.555804   0.126262  -4.402 1.07e-05 ***
[2,]  0.860075   0.114838   7.489 6.92e-14 ***
[3,] -0.026411   0.002169 -12.177  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

##### Assignment Due Thursday 10/14/21

Use the 1980 North Carolina dataset to: (1) conduct exploratory data analysis looking at marginal distributions and bivariate relationships; (2) estimate a logistic regression model with main effects for age and sex (interpreting your results); (3) check to see whether the age effects vary between sex groups using plots, a likelihood ratio test, and derivative comparison (including an assessment of the sampling distribution of the difference between the male and female derivatives at age 22); and (4) confirm that you can estimate a model writing down your own likelihood function that is comparable to the model estimated by the glm() function.


### Lesson 7 - Thursday 10/14/21

* Today, we turn our attention to the multinomial logistic regression model (sometimes also called the multinomial logit model). This model is appropriate when we have a categorical outcome variable. It is also a generalization of the binary logistic regression model we have previously considered. The dataset we will consider is based on a sample of nearly 70,000 auto accidents and comes from Agresti (1997). Measures included in the data are: 

* *y* - 1=level of injury (1=not injured, 2=injured but not transported by EMS, 3=injured and transported by EMS, 4=injured and hospitalized but survived, and 5=fatal injury
* *sb* - no = person not wearing a seatbelt at time of accident, yes = person was wearing a seatbelt at the time of the accident
* *ur* - u = accident occurred in an urban area, r = accident occurred in a rural area
* *sx* - m = male, f = female

* The outcome variable *y* could be construed as either a categorical or an ordinal variable. Multinomial logistic regression models can be estimated with either type of variable. There are some restrictions on the use of specialized ordinal level variables which we will discuss next week. For today, we will treat the outcome as a categorical variable with 5 categories.

* We begin our analysis by reading the dataset into memory:

```r
df <- read.csv(file="sb.csv",sep=",",header=T)

# we can select a simple random sample of 25 cases
# to look at the dataset

df[sample(nrow(df),size=25,replace=F), ]
```

* Here is our output (you will get a different sample of cases):

```rout
> df <- read.csv(file="sb.csv",sep=",",header=T)
> 
> # we can select a simple random sample of 25 cases
> # to look at the dataset
> 
> df[sample(nrow(df),size=25,replace=F), ]
      y  sb ur sx
67601 1 yes  r  m
32933 1  no  u  m
47646 1 yes  u  m
49569 1 yes  u  m
36416 1  no  u  m
4037  1  no  u  f
62798 1 yes  r  m
63082 1 yes  r  m
38785 1  no  u  m
67798 1 yes  r  m
37626 1  no  u  m
19387 1 yes  u  f
35081 1  no  u  m
12076 1 yes  u  f
19161 1 yes  u  f
7413  2  no  u  f
14650 1 yes  u  f
6523  1  no  u  f
63810 1 yes  r  m
48465 1 yes  u  m
34531 1  no  u  m
8392  1 yes  u  f
31545 3 yes  r  f
39366 1  no  u  m
1658  1  no  u  f
> 
```

* Next, we look at a crosstable of the outcome variable and seatbelt usage at the time of the accident:

```r
# multinomial logit 
# outcome variable: response (y)
# independent variable: seatbelt use (sb)

table(df$y,df$sb,exclude=NULL)
```

* Here is the resulting table:

```rout
> # multinomial logit 
> # outcome variable: response (y)
> # independent variable: seatbelt use (sb)
> 
> table(df$y,df$sb,exclude=NULL)
   
       no   yes
  1 27037 35383
  2   525   377
  3  2706  1753
  4   534   241
  5   100    38
> 
```

* To aid in interpreting the table, we can convert it to column percentage form:

```r
sb.table <- table(df$y,df$sb,exclude=NULL)
sb.table
col.pct <- prop.table(sb.table,margin=2)
col.pct
```

* Here is our output:

```rout
> sb.table <- table(df$y,df$sb,exclude=NULL)
> sb.table
   
       no   yes
  1 27037 35383
  2   525   377
  3  2706  1753
  4   534   241
  5   100    38
> col.pct <- prop.table(sb.table,margin=2)
> col.pct
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> 
```

* It looks like seatbelt usage coincides with lower injury risk.
* Let's convert our seatbelt usage variable into a numeric form.
* We also need to verify that the conversion worked properly.

```r
# independent variable

df$sbn <- rep(NA,nrow(df))
df$sbn[df$sb=="yes"] <- 1
df$sbn[df$sb=="no"] <- 0

table(df$sb,df$sbn,exclude=NULL)
```

* Here are the results:

```rout
> # independent variable
> 
> df$sbn <- rep(NA,nrow(df))
> df$sbn[df$sb=="yes"] <- 1
> df$sbn[df$sb=="no"] <- 0
> 
> table(df$sb,df$sbn,exclude=NULL)
     
          0     1
  no  30902     0
  yes     0 37792
> 
```

* Next, let's recode the outcome variable into a set of 5 dummy variables:

```r
df$y1 <- rep(NA,nrow(df))
df$y1[df$y==1] <- 1
df$y1[df$y==2 | df$y==3 | df$y==4 | df$y==5] <- 0
table(df$y1,df$y,exclude=NULL)

df$y2 <- rep(NA,nrow(df))
df$y2[df$y==2] <- 1
df$y2[df$y==1 | df$y==3 | df$y==4 | df$y==5] <- 0
table(df$y2,df$y,exclude=NULL)

df$y3 <- rep(NA,nrow(df))
df$y3[df$y==3] <- 1
df$y3[df$y==1 | df$y==2 | df$y==4 | df$y==5] <- 0
table(df$y3,df$y,exclude=NULL)

df$y4 <- rep(NA,nrow(df))
df$y4[df$y==4] <- 1
df$y4[df$y==1 | df$y==2 | df$y==3 | df$y==5] <- 0
table(df$y4,df$y,exclude=NULL)

df$y5 <- rep(NA,nrow(df))
df$y5[df$y==5] <- 1
df$y5[df$y==1 | df$y==2 | df$y==3 | df$y==4] <- 0
table(df$y5,df$y,exclude=NULL)
```

* Here are the results:

```rout
> df$y1 <- rep(NA,nrow(df))
> df$y1[df$y==1] <- 1
> df$y1[df$y==2 | df$y==3 | df$y==4 | df$y==5] <- 0
> table(df$y1,df$y,exclude=NULL)
   
        1     2     3     4     5
  0     0   902  4459   775   138
  1 62420     0     0     0     0
> 
> df$y2 <- rep(NA,nrow(df))
> df$y2[df$y==2] <- 1
> df$y2[df$y==1 | df$y==3 | df$y==4 | df$y==5] <- 0
> table(df$y2,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420     0  4459   775   138
  1     0   902     0     0     0
> 
> df$y3 <- rep(NA,nrow(df))
> df$y3[df$y==3] <- 1
> df$y3[df$y==1 | df$y==2 | df$y==4 | df$y==5] <- 0
> table(df$y3,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902     0   775   138
  1     0     0  4459     0     0
> 
> df$y4 <- rep(NA,nrow(df))
> df$y4[df$y==4] <- 1
> df$y4[df$y==1 | df$y==2 | df$y==3 | df$y==5] <- 0
> table(df$y4,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902  4459     0   138
  1     0     0     0   775     0
> 
> df$y5 <- rep(NA,nrow(df))
> df$y5[df$y==5] <- 1
> df$y5[df$y==1 | df$y==2 | df$y==3 | df$y==4] <- 0
> table(df$y5,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902  4459   775     0
  1     0     0     0     0   138
> 
```

* Now, we are ready to estimate the parameters of the multinomial logistic regression model. Here is the model where we have the 5-category outcome defined above and a single independent variable - seat belt use at the time of the accident.

```r
library(maxLik)

ll1 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- parms[2]
    a3 <- parms[3]
    b3 <- parms[4]
    a4 <- parms[5]
    b4 <- parms[6]
    a5 <- parms[7]
    b5 <- parms[8]

    l2 <- a2+b2*df$sbn
    l3 <- a3+b3*df$sbn
    l4 <- a4+b4*df$sbn
    l5 <- a5+b5*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m1 <- maxLik(ll1,start=c(-3.57239423,-0.13240324,
                         -3.57239423,-0.13240324,
                         -3.57239423,-0.13240324,
                         -3.57239423,-0.13240324),
  method="BHHH",finalHessian="BHHH")
summary(m1)
```

* Here is the output I got:

```rout
> library(maxLik)
Loading required package: miscTools

Please cite the 'maxLik' package as:
Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.

If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
https://r-forge.r-project.org/projects/maxlik/
> 
> ll1 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- parms[2]
+     a3 <- parms[3]
+     b3 <- parms[4]
+     a4 <- parms[5]
+     b4 <- parms[6]
+     a5 <- parms[7]
+     b5 <- parms[8]
+ 
+     l2 <- a2+b2*df$sbn
+     l3 <- a3+b3*df$sbn
+     l4 <- a4+b4*df$sbn
+     l5 <- a5+b5*df$sbn
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m1 <- maxLik(ll1,start=c(-3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324),
+   method="BHHH",finalHessian="BHHH")
> summary(m1)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 15 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26013.69 
8  free parameters
Estimates:
     Estimate Std. error  t value  Pr(> t)    
[1,] -3.94156    0.04407  -89.448  < 2e-16 ***
[2,] -0.60018    0.06799   -8.828  < 2e-16 ***
[3,] -2.30173    0.02016 -114.158  < 2e-16 ***
[4,] -0.70317    0.03171  -22.178  < 2e-16 ***
[5,] -3.92457    0.04370  -89.808  < 2e-16 ***
[6,] -1.06462    0.07802  -13.645  < 2e-16 ***
[7,] -5.59979    0.10018  -55.895  < 2e-16 ***
[8,] -1.23661    0.19074   -6.483 8.97e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Based on the parameter estimates in this model, we are now able to calculate the predicted probability distribution of *y* implied by those estimates:

```r
# first, save the parameter estimates

a2 <- coef(m1)[1]
b2 <- coef(m1)[2]
a3 <- coef(m1)[3]
b3 <- coef(m1)[4]
a4 <- coef(m1)[5]
b4 <- coef(m1)[6]
a5 <- coef(m1)[7]
b5 <- coef(m1)[8]

# second, calculate the logits

l2n <- a2+b2*0
l2y <- a2+b2*1

l3n <- a3+b3*0
l3y <- a3+b3*1

l4n <- a4+b4*0
l4y <- a4+b4*1

l5n <- a5+b5*0
l5y <- a5+b5*1

# third, calculate p(y=?|sb=no) and p(y=?|sb=yes)

py1n <- 1/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py1y <- 1/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py2n <- exp(l2n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py2y <- exp(l2y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py3n <- exp(l3n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py3y <- exp(l3y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py4n <- exp(l4n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py4y <- exp(l4y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py5n <- exp(l5n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py5y <- exp(l5y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

# display the results

c(py1n,py2n,py3n,py4n,py5n)
c(py1y,py2y,py3y,py4y,py5y)
```

* Here are the results:

```rout
> # display the results
> 
> c(py1n,py2n,py3n,py4n,py5n)
[1] 0.874927189 0.016989192 0.087567148 0.017280435 0.003236037
> c(py1y,py2y,py3y,py4y,py5y)
[1] 0.936256354 0.009975656 0.046385479 0.006377011 0.001005500
> 
```

* Next, we can compare these calculations to what we saw in the table we examined earlier:

```r
prop.table(table(df$y,df$sb),margin=2)
```

* which yields:

```rout
> prop.table(table(df$y,df$sb),margin=2)
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> 
```

* A question that now arises is whether the differences between these two columns are large enough for us to conclude that seat belt use is a significant predictor of the outcome distribution. To answer this question, we can construct a log-likelihood ratio test.
* The first step of the test is to estimate an intercept-only model:

```r
ll2 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- 0
    a3 <- parms[2]
    b3 <- 0
    a4 <- parms[3]
    b4 <- 0
    a5 <- parms[4]
    b5 <- 0

    l2 <- a2+b2*df$sbn
    l3 <- a3+b3*df$sbn
    l4 <- a4+b4*df$sbn
    l5 <- a5+b5*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m2 <- maxLik(ll2,start=c(-3.94156,-2.30173,-3.92457,-5.59979),
  method="BHHH",finalHessian="BHHH")
summary(m2)
```

* Here is a summary of the parameter estimates from the intercept-only model (model 2):

```rout
> ll2 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- 0
+     a3 <- parms[2]
+     b3 <- 0
+     a4 <- parms[3]
+     b4 <- 0
+     a5 <- parms[4]
+     b5 <- 0
+ 
+     l2 <- a2+b2*df$sbn
+     l3 <- a3+b3*df$sbn
+     l4 <- a4+b4*df$sbn
+     l5 <- a5+b5*df$sbn
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m2 <- maxLik(ll2,start=c(-3.94156,-2.30173,-3.92457,-5.59979),
+   method="BHHH",finalHessian="BHHH")
> summary(m2)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26413.26 
4  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] -4.23703    0.03354 -126.34  <2e-16 ***
[2,] -2.63896    0.01550 -170.24  <2e-16 ***
[3,] -4.38878    0.03614 -121.43  <2e-16 ***
[4,] -6.11439    0.08522  -71.75  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* To see what these parameter estimates imply about the probability distribution of *y*, we can perform the following calculations:


```r
# save the parameter estimates

a2 <- coef(m2)[1]
a3 <- coef(m2)[2]
a4 <- coef(m2)[3]
a5 <- coef(m2)[4]

# calculate logits

l2 <- a2
l3 <- a3
l4 <- a4
l5 <- a5

# calculate cell probabilities 

py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

# display the results

c(py1,py2,py3,py4,py5)
```

* We get these results:


```rout
> # display the results
> 
> c(py1,py2,py3,py4,py5)
[1] 0.908667424 0.013130695 0.064911055 0.011281917 0.002008909
> 
```

* Now, we can check this against the frequency distribution in the raw data:

```r
# check against the frequency table

table(df$y)/nrow(df)
```

which yields:

```rout
> # check against the frequency table
> 
> table(df$y)/nrow(df)

          1           2           3           4           5 
0.908667424 0.013130696 0.064911055 0.011281917 0.002008909 
> 
```

* Notice that the constrained model generates "compromise" estimates of each cell probabilities.
* We are now ready to calculate the log-likelihood ratio test:

```r
# log-likelihood ratio test

# note that this is a comparison of an 8-parameter model 
# and a 4-parameter model. Here is the log-likelihood ratio test:

logLik(m1)
logLik(m2)

ts <- -2*(-26413.26-(-26013.69))
ts
1-pchisq(q=ts,df=4)
```

* Here are the results:

```rout
> # log-likelihood ratio test
> 
> # note that this is a comparison of an 8-parameter model 
> # and a 4-parameter model. Here is the log-likelihood ratio test:
> 
> logLik(m1)
[1] -26013.69
attr(,"df")
[1] 8
> logLik(m2)
[1] -26413.26
attr(,"df")
[1] 4
> 
> ts <- -2*(-26413.26-(-26013.69))
> ts
[1] 799.14
> 1-pchisq(q=ts,df=4)
[1] 0
> 
```

* Based on this evidence, we conclude that the model adjusting the distribution of *y* for seat belt use is more consistent with the data.
* Let us now turn to the issue of whether our conclusion is altered in any way by further adjusting for urban/rural geography.
* We begin by recoding urban/rural into a numeric variable (we will call it "urban") for analysis:

```r
df$urban <- rep(NA,nrow(df))
df$urban[df$ur=="u"] <- 1
df$urban[df$ur=="r"] <- 0
table(df$ur,df$urban,exclude=NULL)
```

* Here are the results:

```rout
> df$urban <- rep(NA,nrow(df))
> df$urban[df$ur=="u"] <- 1
> df$urban[df$ur=="r"] <- 0
> table(df$ur,df$urban,exclude=NULL)
   
        0     1
  r 25523     0
  u     0 43171
> 
```

* We now estimate model 3 which is an elaboration of model 1 -- allowing for main effects of seat belt use and urban rural geography:

```r
ll3 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- parms[2]
    c2 <- parms[3]
    a3 <- parms[4]
    b3 <- parms[5]
    c3 <- parms[6]
    a4 <- parms[7]
    b4 <- parms[8]
    c4 <- parms[9]
    a5 <- parms[10]
    b5 <- parms[11]
    c5 <- parms[12]

    l2 <- a2+b2*df$sbn+c2*df$urban
    l3 <- a3+b3*df$sbn+c3*df$urban
    l4 <- a4+b4*df$sbn+c4*df$urban
    l5 <- a5+b5*df$sbn+c5*df$urban

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m3 <- maxLik(ll3,start=c(-3.94156,-0.60018,0.00372,
                               -2.30173,-0.70317,0.00372,
                               -3.92457,-1.06462,0.00372,
                               -5.5998,-1.2366,0.00372),
  method="BHHH",finalHessian="BHHH")
summary(m3)
```

* Here are the results:

```rout
> ll3 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- parms[2]
+     c2 <- parms[3]
+     a3 <- parms[4]
+     b3 <- parms[5]
+     c3 <- parms[6]
+     a4 <- parms[7]
+     b4 <- parms[8]
+     c4 <- parms[9]
+     a5 <- parms[10]
+     b5 <- parms[11]
+     c5 <- parms[12]
+ 
+     l2 <- a2+b2*df$sbn+c2*df$urban
+     l3 <- a3+b3*df$sbn+c3*df$urban
+     l4 <- a4+b4*df$sbn+c4*df$urban
+     l5 <- a5+b5*df$sbn+c5*df$urban
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m3 <- maxLik(ll3,start=c(-3.94156,-0.60018,0.00372,
+                                -2.30173,-0.70317,0.00372,
+                                -3.92457,-1.06462,0.00372,
+                                -5.5998,-1.2366,0.00372),
+   method="BHHH",finalHessian="BHHH")
> summary(m3)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 5 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -25587.14 
12  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
 [1,] -3.75859    0.05976 -62.895  < 2e-16 ***
 [2,] -0.60474    0.06805  -8.887  < 2e-16 ***
 [3,] -0.29589    0.06799  -4.352 1.35e-05 ***
 [4,] -1.90206    0.02547 -74.691  < 2e-16 ***
 [5,] -0.71430    0.03182 -22.445  < 2e-16 ***
 [6,] -0.70252    0.03126 -22.472  < 2e-16 ***
 [7,] -3.30597    0.05150 -64.193  < 2e-16 ***
 [8,] -1.08394    0.07817 -13.867  < 2e-16 ***
 [9,] -1.22625    0.07584 -16.169  < 2e-16 ***
[10,] -4.81938    0.11129 -43.305  < 2e-16 ***
[11,] -1.26318    0.19085  -6.619 3.63e-11 ***
[12,] -1.77132    0.19980  -8.865  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* We can calculate a log-likelihood ratio test comparing model 1 (simpler 8-parameter model) to model 3 (more complicated 12-parameter model). Here is the R code for this comparison:

```r
# compare model 1 to model 3

logLik(m1)
logLik(m3)

ts <- -2*(-26013.69-(-25587.14))
ts
1-pchisq(q=ts,df=4)
```

* Here are the results:

```rout
> # compare model 1 to model 3
> 
> logLik(m1)
[1] -26013.69
attr(,"df")
[1] 8
> logLik(m3)
[1] -25587.14
attr(,"df")
[1] 12
> 
> ts <- -2*(-26013.69-(-25587.14))
> ts
[1] 853.1
> 1-pchisq(q=ts,df=4)
[1] 0
> 
```

* Based on this evidence, we conclude that urban rural geography does coincide with the outcome variable. We can judge the magnitude of this effect by calculating the predicted probability distribution of *y* conditional on both seat belt use and urban/rural geography:

```r
# calculate predicted probabilities based on model 3
# first, save the parameter estimates

a2 <- coef(m3)[1]
b2 <- coef(m3)[2]
c2 <- coef(m3)[3]

a3 <- coef(m3)[4]
b3 <- coef(m3)[5]
c3 <- coef(m3)[6]

a4 <- coef(m3)[7]
b4 <- coef(m3)[8]
c4 <- coef(m3)[9]

a5 <- coef(m3)[10]
b5 <- coef(m3)[11]
c5 <- coef(m3)[12]

# calculate predicted probabilities

# 1. p(y=?|sb=no,rural)

l2.00 <- a2+b2*0+c2*0
l3.00 <- a3+b3*0+c3*0
l4.00 <- a4+b4*0+c4*0
l5.00 <- a5+b5*0+c5*0

py1.00 <- 1/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py2.00 <- exp(l2.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py3.00 <- exp(l3.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py4.00 <- exp(l4.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py5.00 <- exp(l5.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))

# 2. p(y=?|sb=no,urban)

l2.01 <- a2+b2*0+c2*1
l3.01 <- a3+b3*0+c3*1
l4.01 <- a4+b4*0+c4*1
l5.01 <- a5+b5*0+c5*1

py1.01 <- 1/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py2.01 <- exp(l2.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py3.01 <- exp(l3.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py4.01 <- exp(l4.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py5.01 <- exp(l5.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))

# 3. p(y=?|sb=yes,rural)

l2.10 <- a2+b2*1+c2*0
l3.10 <- a3+b3*1+c3*0
l4.10 <- a4+b4*1+c4*0
l5.10 <- a5+b5*1+c5*0

py1.10 <- 1/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py2.10 <- exp(l2.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py3.10 <- exp(l3.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py4.10 <- exp(l4.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py5.10 <- exp(l5.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))

# 4. p(y=?|sb=yes,urban)

l2.11 <- a2+b2*1+c2*1
l3.11 <- a3+b3*1+c3*1
l4.11 <- a4+b4*1+c4*1
l5.11 <- a5+b5*1+c5*1

py1.11 <- 1/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py2.11 <- exp(l2.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py3.11 <- exp(l3.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py4.11 <- exp(l4.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py5.11 <- exp(l5.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))

# display the results

c(py1.00,py2.00,py3.00,py4.00,py5.00)
c(py1.01,py2.01,py3.01,py4.01,py5.01)
c(py1.10,py2.10,py3.10,py4.10,py5.10)
c(py1.11,py2.11,py3.11,py4.11,py5.11)
```

* Here are the results:

```rout
> # display the results
> 
> c(py1.00,py2.00,py3.00,py4.00,py5.00)
[1] 0.82148091 0.01915419 0.12261549 0.03011857 0.00663084
> c(py1.01,py2.01,py3.01,py4.01,py5.01)
[1] 0.906282555 0.015719050 0.067005312 0.009748677 0.001244405
> c(py1.10,py2.10,py3.10,py4.10,py5.10)
[1] 0.90868689 0.01157297 0.06639678 0.01126944 0.00207392
> c(py1.11,py2.11,py3.11,py4.11,py5.11)
[1] 0.9526584084 0.0090253548 0.0344800396 0.0034663331 0.0003698641
> 
```

* Let's think about what this information is telling us. 
* First, p(y=1|sb=no, rural) = 0.821.
* Second, p(y=1|sb=no, urban) = 0.906
* These results suggest that accidents in urban areas are less likely to involve injuries compared to those that occurred in rural areas.
* Similar results are evident when seat belts are used: p(y=1|sb=yes,rural) = 0.909 and p(y=1|sb=yes,urban) = 0.953.
* This model does impose an important constraint on the analysis: it assumes there is no interaction between seat belt use and urban/rural geography.
* We can relax this constraint to see if a more general model is warranted.

```r
# now relax the constraint that seatbelt usage has the
# same coefficient for urban and rural persons

ll4 <- function(parms)
  {

    a2 <- parms[1]
    b2 <- parms[2]
    c2 <- parms[3]
    d2 <- parms[4]
    a3 <- parms[5]
    b3 <- parms[6]
    c3 <- parms[7]
    d3 <- parms[8]
    a4 <- parms[9]
    b4 <- parms[10]
    c4 <- parms[11]
    d4 <- parms[12]
    a5 <- parms[13]
    b5 <- parms[14]
    c5 <- parms[15]
    d5 <- parms[16]

    l2 <- a2+b2*df$sbn+c2*df$urban+d2*df$urban*df$sbn
    l3 <- a3+b3*df$sbn+c3*df$urban+d3*df$urban*df$sbn
    l4 <- a4+b4*df$sbn+c4*df$urban+d4*df$urban*df$sbn
    l5 <- a5+b5*df$sbn+c5*df$urban+d5*df$urban*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m4 <- maxLik(ll4,start=c(-3.75859,-0.60474,-0.29589,0.00328402,
                               -1.90206,-0.71430,0.70252,0.00328402,
                               -3.30597,-1.08394,-1.22625,0.00328402,
                               -4.81938,-1.26318,-1.77132,0.00328402),
  method="BHHH",finalHessian="BHHH")
summary(m4)
```

* Here are the results:

```rout
> summary(m4)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 5 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -25586.08 
16  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
 [1,] -3.77919    0.06913 -54.664  < 2e-16 ***
 [2,] -0.55616    0.10397  -5.349 8.84e-08 ***
 [3,] -0.26053    0.08973  -2.903  0.00369 ** 
 [4,] -0.08555    0.13752  -0.622  0.53389    
 [5,] -1.88675    0.02848 -66.254  < 2e-16 ***
 [6,] -0.75145    0.04449 -16.890  < 2e-16 ***
 [7,] -0.73347    0.04056 -18.083  < 2e-16 ***
 [8,]  0.07654    0.06367   1.202  0.22932    
 [9,] -3.29584    0.05467 -60.288  < 2e-16 ***
[10,] -1.11361    0.09735 -11.439  < 2e-16 ***
[11,] -1.25256    0.09161 -13.672  < 2e-16 ***
[12,]  0.08091    0.16337   0.495  0.62042    
[13,] -4.81443    0.11517 -41.802  < 2e-16 ***
[14,] -1.27758    0.21869  -5.842 5.16e-09 ***
[15,] -1.78703    0.23449  -7.621 2.52e-14 ***
[16,]  0.05251    0.44800   0.117  0.90670    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* We now conduct a log-likelihood ratio test comparing model 3 (more restrictive 12-parameter model) to model 4 (more general 16-parameter model):

```r
# compare model 4 to model 3

logLik(m3)
logLik(m4)

ts <- -2*(-25587.14-(-25586.08))
ts
1-pchisq(q=ts,df=4)
```

* The results of this test are not statistically significant. The more general 16-parameter model did not significantly improve on the more restrictive 12-parameter model:

```rout
> # compare model 4 to model 3
> 
> logLik(m3)
[1] -25587.14
attr(,"df")
[1] 12
> logLik(m4)
[1] -25586.08
attr(,"df")
[1] 16
> 
> ts <- -2*(-25587.14-(-25586.08))
> ts
[1] 2.12
> 1-pchisq(q=ts,df=4)
[1] 0.713699
> 
```

* We can use the parameter estimates from the more general model to calculate the probability distribution of y allowing for the possibility of an interaction:


```r
# calculate predicted probabilities based on model 4
# (allowing for the possibility that there is an
# interaction between urban/rural and seatbelt usage

a2 <- coef(m4)[1]
b2 <- coef(m4)[2]
c2 <- coef(m4)[3]
d2 <- coef(m4)[4]

a3 <- coef(m4)[5]
b3 <- coef(m4)[6]
c3 <- coef(m4)[7]
d3 <- coef(m4)[8]

a4 <- coef(m4)[9]
b4 <- coef(m4)[10]
c4 <- coef(m4)[11]
d4 <- coef(m4)[12]

a5 <- coef(m4)[13]
b5 <- coef(m4)[14]
c5 <- coef(m4)[15]
d5 <- coef(m4)[16]

# 1. p(y=?|sb=no,rural)

l2.00 <- a2+b2*0+c2*0+d2*0*0
l3.00 <- a3+b3*0+c3*0+d3*0*0
l4.00 <- a4+b4*0+c4*0+d4*0*0
l5.00 <- a5+b5*0+c5*0+d5*0*0

py1.00.i <- 1/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py2.00.i <- exp(l2.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py3.00.i <- exp(l3.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py4.00.i <- exp(l4.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py5.00.i <- exp(l5.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))

# 2. p(y=?|sb=no,urban)

l2.01 <- a2+b2*0+c2*1+d2*0*1
l3.01 <- a3+b3*0+c3*1+d3*0*1
l4.01 <- a4+b4*0+c4*1+d4*0*1
l5.01 <- a5+b5*0+c5*1+d5*0*1

py1.01.i <- 1/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py2.01.i <- exp(l2.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py3.01.i <- exp(l3.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py4.01.i <- exp(l4.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py5.01.i <- exp(l5.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))

# 3. p(y=?|sb=yes,rural)

l2.10 <- a2+b2*1+c2*0+d2*1*0
l3.10 <- a3+b3*1+c3*0+d3*1*0
l4.10 <- a4+b4*1+c4*0+d4*1*0
l5.10 <- a5+b5*1+c5*0+d5*1*0

py1.10.i <- 1/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py2.10.i <- exp(l2.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py3.10.i <- exp(l3.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py4.10.i <- exp(l4.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py5.10.i <- exp(l5.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))

# 4. p(y=?|sb=yes,urban)

l2.11 <- a2+b2*1+c2*1+d2*1*1
l3.11 <- a3+b3*1+c3*1+d3*1*1
l4.11 <- a4+b4*1+c4*1+d4*1*1
l5.11 <- a5+b5*1+c5*1+d5*1*1

py1.11.i <- 1/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py2.11.i <- exp(l2.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py3.11.i <- exp(l3.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py4.11.i <- exp(l4.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py5.11.i <- exp(l5.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
```

* Then, we can compare the two probability distributions:

```r
# re-display the results - without interaction

c(py1.00,py2.00,py3.00,py4.00,py5.00)
c(py1.01,py2.01,py3.01,py4.01,py5.01)
c(py1.10,py2.10,py3.10,py4.10,py5.10)
c(py1.11,py2.11,py3.11,py4.11,py5.11)

# display the results - with interaction

c(py1.00.i,py2.00.i,py3.00.i,py4.00.i,py5.00.i)
c(py1.01.i,py2.01.i,py3.01.i,py4.01.i,py5.01.i)
c(py1.10.i,py2.10.i,py3.10.i,py4.10.i,py5.10.i)
c(py1.11.i,py2.11.i,py3.11.i,py4.11.i,py5.11.i)
```

* Here are the results:

```rout
> # re-display the results - without interaction
> 
> c(py1.00,py2.00,py3.00,py4.00,py5.00)
[1] 0.82148091 0.01915419 0.12261549 0.03011857 0.00663084
> c(py1.01,py2.01,py3.01,py4.01,py5.01)
[1] 0.906282555 0.015719050 0.067005312 0.009748677 0.001244405
> c(py1.10,py2.10,py3.10,py4.10,py5.10)
[1] 0.90868689 0.01157297 0.06639678 0.01126944 0.00207392
> c(py1.11,py2.11,py3.11,py4.11,py5.11)
[1] 0.9526584084 0.0090253548 0.0344800396 0.0034663331 0.0003698641
> 
> # display the results - with interaction
> 
> c(py1.00.i,py2.00.i,py3.00.i,py4.00.i,py5.00.i)
[1] 0.819971993 0.018729215 0.124277963 0.030369333 0.006651497
> c(py1.01.i,py2.01.i,py3.01.i,py4.01.i,py5.01.i)
[1] 0.907167797 0.015968371 0.066029986 0.009601561 0.001232286
> c(py1.10.i,py2.10.i,py3.10.i,py4.10.i,py5.10.i)
[1] 0.909909910 0.011917429 0.065049301 0.011066184 0.002057175
> c(py1.11.i,py2.11.i,py3.11.i,py4.11.i,py5.11.i)
[1] 0.9519307881 0.0088204264 0.0352817040 0.0035872546 0.0003798269
> 
```

* We can see from these results that there is barely any difference in the two sets of probability distributions. 
* It is also worth noting that the results from the more general model are identical to what we see in the frequency tables:

```r
table(df$y,df$sb,df$ur)
```

which yields:

```rout
> table(df$y,df$sb,df$ur)
, ,  = r

   
       no   yes
  1  9369 12827
  2   214   168
  3  1420   917
  4   347   156
  5    76    29

, ,  = u

   
       no   yes
  1 17668 22556
  2   311   209
  3  1286   836
  4   187    85
  5    24     9

> 
```

* Consider, for example, the following calculation based on rural persons who did not wear a seatbelt:

```rout
> 9369/(9369+214+1420+347+76)
[1] 0.819972
> 
```

which is exactly the number we see in the interaction model. So, the simpler model slightly (but not significantly) distorts the joint distribution of these three variables.

##### Assignment Due Thursday 10/21/21

For this week's assignment, you are asked to consider estimating a multinomial logit model for urban/rural geography (by itself) compared to an intercept-only model. You should interpret the results of the urban/rural geography coefficients. Next, you should estimate and interpret a main-effects model adjusting for both sex and urban/rural geography. Does this model significantly improve on the urban/rural geography model? Finally, you are asked to estimate and interpret an interaction model adjusting for both sex and urban/rural geography. Consider whether this interactive model or the main-effects only model is more consistent with the data. Construct and interpret the estimates within the context of the predicted probability distribution of *y* from both the main-effects only model and the 2-way interaction model. Comment on the similarities/differences between the two distributions. Write a 1-2 paragraph essay where you summarize and narratively interpret your results. 


### Lesson 8 - Thursday 10/21/21

* We continue our analysis of the Agresti (1997) auto accident dataset based on a sample of nearly 70,000 auto accidents. As a reminder from last week, the measures included in the data are: 

* *y* - 1=level of injury (1=not injured, 2=injured but not transported by EMS, 3=injured and transported by EMS, 4=injured and hospitalized but survived, and 5=fatal injury
* *sb* - no = person not wearing a seatbelt at time of accident, yes = person was wearing a seatbelt at the time of the accident
* *ur* - u = accident occurred in an urban area, r = accident occurred in a rural area
* *sx* - m = male, f = female

* The outcome variable *y* could be construed as either a categorical or an ordinal variable. Multinomial logistic regression models can be estimated with either type of variable. There are some restrictions on the use of specialized ordinal level variables which we will discuss today. 

* We begin today's work by reconsidering the multinomial logit model from last week. Then, we transition to the ordinal logistic regression model.

* Here is a recently published reading for regression analysis based on nominal/ordinal level data similar to what we have been studying ([article link](https://onlinelibrary.wiley.com/doi/pdf/10.1002/pmrj.12622)). An older paper covering the issue of proportional odds testing is [here](https://www.jstor.org/stable/2532457?seq=1#metadata_info_tab_contents).

* Recall from last week that we estimated a multinomial logistic regression model with *y* as the outcome and the numeric version of *sb* as a predictor variable:

```r
df <- data.frame(y,sb,ur,sx)
# we can select a simple random sample of 25 cases
# to look at the dataset

df[sample(nrow(df),size=25,replace=F), ]

# multinomial logit 
# outcome variable: response (y)
# reference category is y=1
# independent variable: seatbelt use (sb)

table(df$y,df$sb,exclude=NULL)
prop.table(table(df$y,df$sb),margin=2)

df$sbn <- rep(NA,nrow(df))
df$sbn[df$sb=="yes"] <- 1
df$sbn[df$sb=="no"] <- 0

# recode outcome variable into a set of dummy variables

df$y1 <- rep(NA,nrow(df))
df$y1[df$y==1] <- 1
df$y1[df$y==2 | df$y==3 | df$y==4 | df$y==5] <- 0
table(df$y1,df$y,exclude=NULL)

df$y2 <- rep(NA,nrow(df))
df$y2[df$y==2] <- 1
df$y2[df$y==1 | df$y==3 | df$y==4 | df$y==5] <- 0
table(df$y2,df$y,exclude=NULL)

df$y3 <- rep(NA,nrow(df))
df$y3[df$y==3] <- 1
df$y3[df$y==1 | df$y==2 | df$y==4 | df$y==5] <- 0
table(df$y3,df$y,exclude=NULL)

df$y4 <- rep(NA,nrow(df))
df$y4[df$y==4] <- 1
df$y4[df$y==1 | df$y==2 | df$y==3 | df$y==5] <- 0
table(df$y4,df$y,exclude=NULL)

df$y5 <- rep(NA,nrow(df))
df$y5[df$y==5] <- 1
df$y5[df$y==1 | df$y==2 | df$y==3 | df$y==4] <- 0
table(df$y5,df$y,exclude=NULL)

library(maxLik)

ll1 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- parms[2]
    a3 <- parms[3]
    b3 <- parms[4]
    a4 <- parms[5]
    b4 <- parms[6]
    a5 <- parms[7]
    b5 <- parms[8]

    l2 <- a2+b2*df$sbn
    l3 <- a3+b3*df$sbn
    l4 <- a4+b4*df$sbn
    l5 <- a5+b5*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m1 <- maxLik(ll1,start=c(-3.57239423,-0.13240324,
                         -3.57239423,-0.13240324,
                         -3.57239423,-0.13240324,
                         -3.57239423,-0.13240324),
  method="BHHH",finalHessian="BHHH")
summary(m1)
```

* Here are the results of that analysis:

```rout
> df <- data.frame(y,sb,ur,sx)
> # we can select a simple random sample of 25 cases
> # to look at the dataset
> 
> df[sample(nrow(df),size=25,replace=F), ]
      y  sb ur sx
42933 1 yes  u  m
35423 1  no  u  m
20367 3 yes  u  f
16622 1 yes  u  f
19836 1 yes  u  f
35125 1  no  u  m
5726  1  no  u  f
5871  1  no  u  f
2266  1  no  u  f
25249 1 yes  r  f
37051 1  no  u  m
22655 1  no  r  f
52262 1 yes  u  m
5171  1  no  u  f
28874 1 yes  r  f
12909 1 yes  u  f
11530 1 yes  u  f
1920  1  no  u  f
22752 1  no  r  f
22051 1  no  r  f
10723 1 yes  u  f
48402 1 yes  u  m
23905 2  no  r  f
45077 1 yes  u  m
23845 1  no  r  f
> 
> # multinomial logit 
> # outcome variable: response (y)
> # reference category is y=1
> # independent variable: seatbelt use (sb)
> 
> table(df$y,df$sb,exclude=NULL)
   
       no   yes
  1 27037 35383
  2   525   377
  3  2706  1753
  4   534   241
  5   100    38
> prop.table(table(df$y,df$sb),margin=2)
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> 
> df$sbn <- rep(NA,nrow(df))
> df$sbn[df$sb=="yes"] <- 1
> df$sbn[df$sb=="no"] <- 0
> 
> # recode outcome variable into a set of dummy variables
> 
> df$y1 <- rep(NA,nrow(df))
> df$y1[df$y==1] <- 1
> df$y1[df$y==2 | df$y==3 | df$y==4 | df$y==5] <- 0
> table(df$y1,df$y,exclude=NULL)
   
        1     2     3     4     5
  0     0   902  4459   775   138
  1 62420     0     0     0     0
> 
> df$y2 <- rep(NA,nrow(df))
> df$y2[df$y==2] <- 1
> df$y2[df$y==1 | df$y==3 | df$y==4 | df$y==5] <- 0
> table(df$y2,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420     0  4459   775   138
  1     0   902     0     0     0
> 
> df$y3 <- rep(NA,nrow(df))
> df$y3[df$y==3] <- 1
> df$y3[df$y==1 | df$y==2 | df$y==4 | df$y==5] <- 0
> table(df$y3,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902     0   775   138
  1     0     0  4459     0     0
> 
> df$y4 <- rep(NA,nrow(df))
> df$y4[df$y==4] <- 1
> df$y4[df$y==1 | df$y==2 | df$y==3 | df$y==5] <- 0
> table(df$y4,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902  4459     0   138
  1     0     0     0   775     0
> 
> df$y5 <- rep(NA,nrow(df))
> df$y5[df$y==5] <- 1
> df$y5[df$y==1 | df$y==2 | df$y==3 | df$y==4] <- 0
> table(df$y5,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902  4459   775     0
  1     0     0     0     0   138
> 
> library(maxLik)
Loading required package: miscTools

Please cite the 'maxLik' package as:
Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.

If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
https://r-forge.r-project.org/projects/maxlik/
> 
> ll1 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- parms[2]
+     a3 <- parms[3]
+     b3 <- parms[4]
+     a4 <- parms[5]
+     b4 <- parms[6]
+     a5 <- parms[7]
+     b5 <- parms[8]
+ 
+     l2 <- a2+b2*df$sbn
+     l3 <- a3+b3*df$sbn
+     l4 <- a4+b4*df$sbn
+     l5 <- a5+b5*df$sbn
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m1 <- maxLik(ll1,start=c(-3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324),
+   method="BHHH",finalHessian="BHHH")
> summary(m1)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 15 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26013.69 
8  free parameters
Estimates:
     Estimate Std. error  t value  Pr(> t)    
[1,] -3.94156    0.04407  -89.448  < 2e-16 ***
[2,] -0.60018    0.06799   -8.828  < 2e-16 ***
[3,] -2.30173    0.02016 -114.158  < 2e-16 ***
[4,] -0.70317    0.03171  -22.178  < 2e-16 ***
[5,] -3.92457    0.04370  -89.808  < 2e-16 ***
[6,] -1.06462    0.07802  -13.645  < 2e-16 ***
[7,] -5.59979    0.10018  -55.895  < 2e-16 ***
[8,] -1.23661    0.19074   -6.483 8.97e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Next, we use a canned procedure in R to verify that we can get the same results:

```r
library(nnet)
mlogit <- multinom(y~1+sbn,data=df)
summary(mlogit)
logLik(mlogit)
```

which gives us the following output (please compare to output above):

```rout
> library(nnet)
> 
> mlogit <- multinom(y~1+sbn,data=df)
# weights:  15 (8 variable)
initial  value 110558.727957 
iter  10 value 26273.408126
iter  20 value 26016.606470
final  value 26013.686582 
converged
> summary(mlogit)
Call:
multinom(formula = y ~ 1 + sbn, data = df)

Coefficients:
  (Intercept)        sbn
2   -3.941561 -0.6001812
3   -2.301734 -0.7031680
4   -3.924569 -1.0646196
5   -5.599792 -1.2366376

Std. Errors:
  (Intercept)        sbn
2  0.04406523 0.06798918
3  0.02016272 0.03170564
4  0.04369955 0.07802104
5  0.10018477 0.19074012

Residual Deviance: 52027.37 
AIC: 52043.37 
> logLik(mlogit)
'log Lik.' -26013.69 (df=8)
> 
```

* Now, let's post-process the results of this analysis and verify that we get the same predicted distribution of *y* that we obtained last week:

```r
a2 <- coef(mlogit)[1,1]
b2 <- coef(mlogit)[1,2]
a3 <- coef(mlogit)[2,1]
b3 <- coef(mlogit)[2,2]
a4 <- coef(mlogit)[3,1]
b4 <- coef(mlogit)[3,2]
a5 <- coef(mlogit)[4,1]
b5 <- coef(mlogit)[4,2]

l2n <- a2+b2*0
l2y <- a2+b2*1

l3n <- a3+b3*0
l3y <- a3+b3*1

l4n <- a4+b4*0
l4y <- a4+b4*1

l5n <- a5+b5*0
l5y <- a5+b5*1

py1n <- 1/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py1y <- 1/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py2n <- exp(l2n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py2y <- exp(l2y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py3n <- exp(l3n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py3y <- exp(l3y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py4n <- exp(l4n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py4y <- exp(l4y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py5n <- exp(l5n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py5y <- exp(l5y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

# display the results

no.vector <- c(py1n,py2n,py3n,py4n,py5n)
no.vector
yes.vector <- c(py1y,py2y,py3y,py4y,py5y)
yes.vector
yes.vector-no.vector
```

* Here are the results:

```rout
> # display the results
> 
> no.vector <- c(py1n,py2n,py3n,py4n,py5n)
> no.vector
[1] 0.874927140 0.016989227 0.087567212 0.017280386 0.003236036
> yes.vector <- c(py1y,py2y,py3y,py4y,py5y)
> yes.vector
[1] 0.936256334 0.009975648 0.046385520 0.006377022 0.001005475
> yes.vector-no.vector
[1]  0.061329195 -0.007013578 -0.041181692 -0.010903364 -0.002230561
> 
```

which tell us that wearing a seat belt is indeed associated with better outcomes throughout the range of the distribution of *y*.

* Now, we turn to the ordinal logistic regression analysis. We begin this analysis by estimating the statistical model using the standard R implementation of ordered logit:

```r
# ordered logit 
# outcome variable: response (y)
# independent variable: seatbelt use (sb)

library(MASS)
ol <- polr(as.factor(y)~1+sbn,data=df,Hess=T)
summary(ol)
logLik(ol)
```

which yields the following results:

```rout
> ol <- polr(as.factor(y)~1+sbn,data=df,Hess=T)
> summary(ol)
Call:
polr(formula = as.factor(y) ~ 1 + sbn, data = df, Hess = T)

Coefficients:
     Value Std. Error t value
sbn -0.746    0.02716  -27.47

Intercepts:
    Value    Std. Error t value 
1|2   1.9426   0.0172   113.0388
2|3   2.1138   0.0179   118.0737
3|4   3.9633   0.0349   113.4066
4|5   5.8666   0.0859    68.3261

Residual Deviance: 52047.97 
AIC: 52057.97 
> logLik(ol)
'log Lik.' -26023.99 (df=5)
> 
```

* As we have come to expect with these models, straightforward interpretation of the coefficients is difficult. Nonetheless, we can see that there is now a single coefficient for seat belt use, it has a negative sign (indicating that wearing a seatbelt is associated with lower values of the dependent variable, *y*), and it is statistically significant.

* Notice also that the log-likelihood value for this model (-26023.99 with 5 parameter estimates) is lower than the log-likelihood value for the multinomial logistic regression model we estimated earlier (-26013.69 with 8 parameter estimates). We will need to determine whether the additional complexity of the multinomial logistic regression model is justified given the observed data.

* To see how this model works, we now write our own log-likelihood function. Since this is a fairly complex log-likelihood function, we should adopt a more principled method for finding the start values:

```r
# first, we identify the starting values

table(df$y)
cumsum(table(df$y))
cumsum(table(df$y))/nrow(df)
z <- runif(n=1000000,min=0,max=1)
logistic <- log(z/(1-z))
par(mfrow=c(1,2))
hist(logistic,prob=T,ylim=c(0,0.28),
  xlab="Standard Logistic Distribution Support")
lines(density(logistic),lty=1,lwd=2)
plot(ecdf(logistic),xlab="Standard Logistic Distribution Support",
                    ylab="Cumulative Distribution")
quantile(logistic,0.909)
quantile(logistic,0.922)
quantile(logistic,0.987)
quantile(logistic,0.998)

cor.test(df$sbn,df$y)
```

* Here are the results:

```rout
> # first, we identify the starting values
> 
> table(df$y)

    1     2     3     4     5 
62420   902  4459   775   138 
> cumsum(table(df$y))
    1     2     3     4     5 
62420 63322 67781 68556 68694 
> cumsum(table(df$y))/nrow(df)
        1         2         3         4         5 
0.9086674 0.9217981 0.9867092 0.9979911 1.0000000 
> z <- runif(n=1000000,min=0,max=1)
> logistic <- log(z/(1-z))
> par(mfrow=c(1,2))
> hist(logistic,prob=T,ylim=c(0,0.28),
+   xlab="Standard Logistic Distribution Support")
> lines(density(logistic),lty=1,lwd=2)
> plot(ecdf(logistic),xlab="Standard Logistic Distribution Support",
+                     ylab="Cumulative Distribution")
> quantile(logistic,0.909)
   90.9% 
2.295095 
> quantile(logistic,0.922)
   92.2% 
2.462995 
> quantile(logistic,0.987)
   98.7% 
4.315628 
> quantile(logistic,0.998)
   99.8% 
6.159764 
> 
> cor.test(df$sbn,df$y)

	Pearson's product-moment correlation

data:  df$sbn and df$y
t = -28.157, df = 68692, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.11420258 -0.09941706
sample estimates:
       cor 
-0.1068157 
```

* This process also generates the following chart which can be useful for visualizing the placement of the starting values:

<p align="left">
<img src="/gfiles/logistic.png" width="600px">
</p>

* Next, we use the information above to initiate the log-likelihood maximization process:

```r
library(maxLik)

ll2 <- function(parms)
  {
    a1 <- parms[1]
    a2 <- parms[2]
    a3 <- parms[3]
    a4 <- parms[4]
    b  <- parms[5]

    l1 <- a1-b*df$sbn
    l2 <- a2-b*df$sbn
    l3 <- a3-b*df$sbn
    l4 <- a4-b*df$sbn

    py.le1 <- exp(l1)/(1+exp(l1))
    py.le2 <- exp(l2)/(1+exp(l2))
    py.le3 <- exp(l3)/(1+exp(l3))
    py.le4 <- exp(l4)/(1+exp(l4))

    py1 <- py.le1
    py2 <- py.le2-py.le1
    py3 <- py.le3-py.le2
    py4 <- py.le4-py.le3
    py5 <- 1-py.le4

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m2 <- maxLik(ll2,start=c(2.304902,
                         2.469170,
                         4.330374,
                         6.184924,
                        -0.106816),
  method="BHHH",finalHessian="BHHH")
summary(m2)
```

which yields:

```rout
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26023.99 
5  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,]  1.94278    0.01718  113.08  <2e-16 ***
[2,]  2.11406    0.01785  118.46  <2e-16 ***
[3,]  3.96337    0.03449  114.93  <2e-16 ***
[4,]  5.86630    0.08560   68.53  <2e-16 ***
[5,] -0.74611    0.02723  -27.40  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Notice that these results are virtually identical to those we obtained from the polr() function.
* Now that we have established the equality of the two procedures, we check the model fit:

```r
# check the model fit

a1 <- coef(m2)[1]
a2 <- coef(m2)[2]
a3 <- coef(m2)[3]
a4 <- coef(m2)[4]
b  <- coef(m2)[5]

l1.no <- a1
l2.no <- a2
l3.no <- a3
l4.no <- a4

l1.yes <- a1-b*1
l2.yes <- a2-b*1
l3.yes <- a3-b*1
l4.yes <- a4-b*1

py.le1.no <- exp(l1.no)/(1+exp(l1.no))
py.le2.no <- exp(l2.no)/(1+exp(l2.no))
py.le3.no <- exp(l3.no)/(1+exp(l3.no))
py.le4.no <- exp(l4.no)/(1+exp(l4.no))

py.le1.yes <- exp(l1.yes)/(1+exp(l1.yes))
py.le2.yes <- exp(l2.yes)/(1+exp(l2.yes))
py.le3.yes <- exp(l3.yes)/(1+exp(l3.yes))
py.le4.yes <- exp(l4.yes)/(1+exp(l4.yes))

py1.no <- py.le1.no
py2.no <- py.le2.no-py.le1.no
py3.no <- py.le3.no-py.le2.no
py4.no <- py.le4.no-py.le3.no
py5.no <- 1-py.le4.no

py1.yes <- py.le1.yes
py2.yes <- py.le2.yes-py.le1.yes
py3.yes <- py.le3.yes-py.le2.yes
py4.yes <- py.le4.yes-py.le3.yes
py5.yes <- 1-py.le4.yes

no.vector <- c(py1.no,py2.no,py3.no,py4.no,py5.no)
no.vector 
yes.vector <- c(py1.yes,py2.yes,py3.yes,py4.yes,py5.yes)
yes.vector 
yes.vector-no.vector

rdata <- prop.table(table(df$y,df$sb),margin=2)
rdata
rdata[,2]-rdata[,1]
```

which yields:

```rout
> no.vector <- c(py1.no,py2.no,py3.no,py4.no,py5.no)
> no.vector 
[1] 0.874657564 0.017604807 0.089092905 0.015819392 0.002825332
> yes.vector <- c(py1.yes,py2.yes,py3.yes,py4.yes,py5.yes)
> yes.vector 
[1] 0.936368150 0.009474008 0.045228852 0.007587204 0.001341785
> yes.vector-no.vector
[1]  0.061710587 -0.008130798 -0.043864053 -0.008232188 -0.001483547
> 
> rdata <- prop.table(table(df$y,df$sb),margin=2)
> rdata
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> rdata[,2]-rdata[,1]
           1            2            3            4            5 
 0.061329161 -0.007013535 -0.041181669 -0.010903424 -0.002230533 
> 
```

* Comparing the expected distribution of y|sb=yes and y|sb=no to the values in the observed data, we see some minor differences but nothing of any particular consequence.
* Overall, this model looks like it fits the data reasonably well. 
* Still, there is a systematic way to check on this that goes beyond merely a visual comparison of the two distributions.
* The next step is to calculate a model that allows the logistic regression slopes for seat belt use to vary depending on where we are along the support of the standard logistic probability distribution:

```r
ll3 <- function(parms)
  {
    a1 <- parms[1]
    a2 <- parms[2]
    a3 <- parms[3]
    a4 <- parms[4]
    b1 <- parms[5]
    b2 <- parms[6]
    b3 <- parms[7]
    b4 <- parms[8]

    l1 <- a1-b1*df$sbn
    l2 <- a2-b2*df$sbn
    l3 <- a3-b3*df$sbn
    l4 <- a4-b4*df$sbn

    py.le1 <- exp(l1)/(1+exp(l1))
    py.le2 <- exp(l2)/(1+exp(l2))
    py.le3 <- exp(l3)/(1+exp(l3))
    py.le4 <- exp(l4)/(1+exp(l4))

    py1 <- py.le1
    py2 <- py.le2-py.le1
    py3 <- py.le3-py.le2
    py4 <- py.le4-py.le3
    py5 <- 1-py.le4

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m3 <- maxLik(ll3,start=c(1.94278,
                         2.11406,
                         3.96337,
                         5.86630,
                        -0.74611,
                        -0.74611,
                        -0.74611,
                        -0.74611),
  method="BHHH",finalHessian="BHHH")
summary(m3)
```

which yields the following results:

```rout
> summary(m3)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 3 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26013.69 
8  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  1.94524    0.01720  113.12  < 2e-16 ***
[2,]  2.11047    0.01832  115.19  < 2e-16 ***
[3,]  3.86580    0.04013   96.33  < 2e-16 ***
[4,]  5.73016    0.10016   57.21  < 2e-16 ***
[5,] -0.74178    0.02719  -27.29  < 2e-16 ***
[6,] -0.75734    0.02925  -25.89  < 2e-16 ***
[7,] -1.03543    0.07226  -14.33  < 2e-16 ***
[8,] -1.17110    0.19072   -6.14 8.23e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* As it turns out, this model is saturated (meaning it fits the data exactly; note that the log-likelihood value for this model is exactly the same log-likelihood from the multinomial logistic regression model. In fact, this model is just a different (but mathematically equivalent) parameterization of the multinomial model. To see this, let's formally check the fit:

```r
# check the model fit

a1 <- coef(m3)[1]
a2 <- coef(m3)[2]
a3 <- coef(m3)[3]
a4 <- coef(m3)[4]
b1 <- coef(m3)[5]
b2 <- coef(m3)[6]
b3 <- coef(m3)[7]
b4 <- coef(m3)[8]

l1.no <- a1
l2.no <- a2
l3.no <- a3
l4.no <- a4

l1.yes <- a1-b1*1
l2.yes <- a2-b2*1
l3.yes <- a3-b3*1
l4.yes <- a4-b4*1

py.le1.no <- exp(l1.no)/(1+exp(l1.no))
py.le2.no <- exp(l2.no)/(1+exp(l2.no))
py.le3.no <- exp(l3.no)/(1+exp(l3.no))
py.le4.no <- exp(l4.no)/(1+exp(l4.no))

py.le1.yes <- exp(l1.yes)/(1+exp(l1.yes))
py.le2.yes <- exp(l2.yes)/(1+exp(l2.yes))
py.le3.yes <- exp(l3.yes)/(1+exp(l3.yes))
py.le4.yes <- exp(l4.yes)/(1+exp(l4.yes))

py1.no <- py.le1.no
py2.no <- py.le2.no-py.le1.no
py3.no <- py.le3.no-py.le2.no
py4.no <- py.le4.no-py.le3.no
py5.no <- 1-py.le4.no

py1.yes <- py.le1.yes
py2.yes <- py.le2.yes-py.le1.yes
py3.yes <- py.le3.yes-py.le2.yes
py4.yes <- py.le4.yes-py.le3.yes
py5.yes <- 1-py.le4.yes

no.vector <- c(py1.no,py2.no,py3.no,py4.no,py5.no)
no.vector 
yes.vector <- c(py1.yes,py2.yes,py3.yes,py4.yes,py5.yes)
yes.vector 
yes.vector-no.vector

rdata <- prop.table(table(df$y,df$sb),margin=2)
rdata
rdata[,2]-rdata[,1]
```

* Here are the results:

```rout
> no.vector <- c(py1.no,py2.no,py3.no,py4.no,py5.no)
> no.vector 
[1] 0.874927189 0.016989192 0.087567148 0.017280435 0.003236036
> yes.vector <- c(py1.yes,py2.yes,py3.yes,py4.yes,py5.yes)
> yes.vector 
[1] 0.936256352 0.009975656 0.046385478 0.006377011 0.001005503
> yes.vector-no.vector
[1]  0.061329162 -0.007013535 -0.041181670 -0.010903424 -0.002230533
> 
> rdata <- prop.table(table(df$y,df$sb),margin=2)
> rdata
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> rdata[,2]-rdata[,1]
           1            2            3            4            5 
 0.061329161 -0.007013535 -0.041181669 -0.010903424 -0.002230533 
> 
```

* So, we have now verified that this ordinal logistic regression model with different slopes is: (1) a saturated model meaning it fits the data exactly; and (2) it can be used as the basis for a comparison to the ordinal logistic regression model with a constrained slope.

* Since this unconstrained model fits the data exactly, we can conduct log-likelihood ratio, AIC, and BIC model selection exercises to see which model is preferred. Here is the code:

```r
# log-likelihood ratio test

logLik(m2)
logLik(m3)
-2*(-26023.99-(-26013.69))
1-pchisq(q=20.6,df=3)

# Akaike Information Criterion (AIC)
# choose model that maximizes AIC

-26013.69-8
-26023.99-5

# Bayesian Information Criterion (BIC)
# choose model that maximizes BIC

-26013.69-(8/2)*log(68694)
-26023.99-(5/2)*log(68694)
```

which yields the following results:

```r
> # log-likelihood ratio test
> 
> logLik(m2)
[1] -26023.99
attr(,"df")
[1] 5
> logLik(m3)
[1] -26013.69
attr(,"df")
[1] 8
> -2*(-26023.99-(-26013.69))
[1] 20.6
> 1-pchisq(q=20.6,df=3)
[1] 0.0001274577
> 
> # Akaike Information Criterion (AIC)
> # choose model that maximizes AIC
> 
> -26013.69-8
[1] -26021.69
> -26023.99-5
[1] -26028.99
> 
> # Bayesian Information Criterion (BIC)
> # choose model that maximizes BIC
> 
> -26013.69-(8/2)*log(68694)
[1] -26058.24
> -26023.99-(5/2)*log(68694)
[1] -26051.83
> 
```

* Based on these results, the log-likelihood ratio test indicates that the multinomial model is preferred while both AIC and BIC point us toward the constrained ordinal logistic regression model. What do you think is going on here?

* A commonly used approach for checking on the proportional slopes/odds assumption we have just investigated is the Brant test. Here is the "canned implementation" of that test in R:
 
```r
library(brant)
brant(ol)
```

which yields the following output:

```rout
> library(brant)
> brant(ol)
-------------------------------------------- 
Test for	X2	df	probability 
-------------------------------------------- 
Omnibus		19.78	3	0
sbn		19.78	3	0
-------------------------------------------- 

H0: Parallel Regression Assumption holds
>
```

* Note the similarity of this test and the log-likelihood ratio test conducted above.
* As we can see, there is little practical difference in the conclusions we have drawn between the two models.

#### Assignment Due Thursday 10/28/21

* Divide the data frame up into separate urban and rural datasets. Here is how you can do that in R:

```r
rural.df <- subset(df,ur=="r")
urban.df <- subset(df,ur=="u")
write.csv(rural.df,file="ruraldf.csv")
write.csv(urban.df,file="urbandf.csv")
```

* Use a multinomial logistic regression program to estimate the relationship between seat belt use and injury separately in urban and rural places. Interpret what you see in these two analyses.
* Use an ordinal logistic regression program to estimate the relationship between seat belt use and injury separately in urban and rural places. Interpret your results. How (if at all) do these results differ from those you obtained from the multinomial analysis?
* Calculate the observed and expected frequencies for the two models. Comment on your findings.
* Using the various model selection criteria discussed in class (log-likelihood ratio test, AIC, and BIC) what conclusions do you reach about using the multinomial or ordinal statistical model within each group (urban/rural)?

### Lesson 9 - Thursday 10/28/21

* We now turn our attention to the problem of analyzing counted outcome data. Counted data can generally be grouped into 3 categories: (1) bounded counts; (2) unbounded counts; and (3) censored counts. We will discuss each of these categories.
* For today's class, we will consider the problem of bounded count data. Here is a reading that addresses the issue ([link](https://link.springer.com/article/10.1007/s10940-017-9346-9)).
* Let's begin by reading in a dataset (emailed to you) with 1000 cases and 2 variables (x and y):

```R
df <- read.csv(file="df.csv",header=T,sep=",")
head(df)
tail(df)
```

* Here is the output:

```Rout
> df <- read.csv(file="df.csv",header=T,sep=",")
> head(df)
  X          x y
1 1 -1.1155665 3
2 2 -0.8795643 5
3 3 -0.7568539 2
4 4 -0.2033934 4
5 5 -1.1694296 0
6 6  0.2639083 8
> tail(df)
        X           x y
995   995 -1.73083611 2
996   996  0.03160026 9
997   997  0.18312608 6
998   998 -0.51101843 7
999   999  0.53383795 7
1000 1000 -0.44075654 5
> 
```

* Next, we create a table showing the distribution of *df$y*:

```R
table(df$y,exclude=NULL)
```

which gives us:


```Rout
> table(df$y,exclude=NULL)

  0   1   2   3   4   5   6   7   8   9  10  11  12 
 17  40  54 109 103 124 100 121 108  88  74  43  19 
> 
```

* Notice that these counts are bounded (range of 0-12).
* What kinds of variables might take this form? (discussion)
* Here is a scatterplot of *df$x* and *df$y*:

```R
plot(x=df$x,y=df$y)
```

which gives us this plot:

<p align="left">
<img src="/gfiles/xy-scatterplot.png" width="600px">
</p>

* Now, let's construct a statistical model of this relationship.

```
m1 <- glm(cbind(y,12-y)~1+x,data=df,family=binomial(link="logit"))
summary(m1)
logLik(m1)
```

* Here is the output:


```Rout
> m1 <- glm(cbind(y,12-y)~1+x,data=df,family=binomial(link="logit"))
> summary(m1)

Call:
glm(formula = cbind(y, 12 - y) ~ 1 + x, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.0461  -0.6559  -0.0394   0.6634   3.9342  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.002481   0.019914   0.125    0.901    
x           0.998452   0.024688  40.443   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 3139.9  on 999  degrees of freedom
Residual deviance: 1097.8  on 998  degrees of freedom
AIC: 3731.8

Number of Fisher Scoring iterations: 4

> logLik(m1)
'log Lik.' -1863.885 (df=2)
> 
```

* So, we can see that we have an intercept estimate of approximately zero and a regression coefficient estimate of approximately 1. How do we interpret these estimates?

* Let's further consider this question by writing down the likelihood function:

```R
library(maxLik)

ll2 <- function(parms)
  {
   a <- parms[1]
   b <- parms[2]
   p <- exp(a+b*df$x)/(1+exp(a+b*df$x))
   pt1 <- factorial(12)
   pt2 <- factorial(df$y)*factorial(12-df$y)
   pt3 <- p^df$y
   pt4 <- (1-p)^(12-df$y)
   pmf <- pt1/pt2*pt3*pt4
   lpmf <- log(pmf)
   return(lpmf)
  }

m2 <- maxLik(ll2,start=c(0.02384042,1.02384023),
             method="BHHH",finalHessian="BHHH")
summary(m2)
```

which yields the following results:

```rout
> library(maxLik)
> 
> ll2 <- function(parms)
+   {
+    a <- parms[1]
+    b <- parms[2]
+    p <- exp(a+b*df$x)/(1+exp(a+b*df$x))
+    pt1 <- factorial(12)
+    pt2 <- factorial(df$y)*factorial(12-df$y)
+    pt3 <- p^df$y
+    pt4 <- (1-p)^(12-df$y)
+    pmf <- pt1/pt2*pt3*pt4
+    lpmf <- log(pmf)
+    return(lpmf)
+   }
> 
> m2 <- maxLik(ll2,start=c(0.02384042,1.02384023),
+              method="BHHH",finalHessian="BHHH")
> summary(m2)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 3 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1863.885 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] 0.002481   0.019715   0.126     0.9    
[2,] 0.998451   0.025243  39.553  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Next, we extract the parameter estimates form this model:

```R
a <- coef(m2)[1]
a
b <- coef(m2)[2]
b
```

which we then use to plot the functional form of the relationship between *df$x* and *df$y*:

```R
p <- exp(a+b*df$x)/(1+exp(a+b*df$x))
plot(x=df$x,y=p)
```

which yields:

<p align="left">
<img src="/gfiles/fform.png" width="600px">
</p>

* Next, we check on the model's fit. First, we calculate the expected frequencies:

```R
# calculate expected cell frequencies

e0  <- sum(choose(12,0)*p^0*(1-p)^(12-0))
e1  <- sum(choose(12,1)*p^1*(1-p)^(12-1))
e2  <- sum(choose(12,2)*p^2*(1-p)^(12-2))
e3  <- sum(choose(12,3)*p^3*(1-p)^(12-3))
e4  <- sum(choose(12,4)*p^4*(1-p)^(12-4))
e5  <- sum(choose(12,5)*p^5*(1-p)^(12-5))
e6  <- sum(choose(12,6)*p^6*(1-p)^(12-6))
e7  <- sum(choose(12,7)*p^7*(1-p)^(12-7))
e8  <- sum(choose(12,8)*p^8*(1-p)^(12-8))
e9  <- sum(choose(12,9)*p^9*(1-p)^(12-9))
e10 <- sum(choose(12,10)*p^10*(1-p)^(12-10))
e11 <- sum(choose(12,11)*p^11*(1-p)^(12-11))
e12 <- sum(choose(12,12)*p^12*(1-p)^(12-12))

# collect expected frequencies into a vector

e <- c(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
e
```

which yields:

```Rout
> e <- c(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
> e
 [1]  15.42094  40.75232  67.83955  90.60545 106.18798 114.69966
 [7] 117.21441 114.38691 106.40160  92.89133  72.51787  44.88954
[13]  16.19243
> 
```

* Then, we collect the observed frequencies:

```R
# collect observed frequencies

table(df$y)

o0  <- table(df$y)[1]
o1  <- table(df$y)[2]
o2  <- table(df$y)[3]
o3  <- table(df$y)[4]
o4  <- table(df$y)[5]
o5  <- table(df$y)[6]
o6  <- table(df$y)[7]
o7  <- table(df$y)[8]
o8  <- table(df$y)[9]
o9  <- table(df$y)[10]
o10 <- table(df$y)[11]
o11 <- table(df$y)[12]
o12 <- table(df$y)[13]

# put the observed frequencies into a vector

o <- c(o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12)
o

```

which yields:

```Rout
> table(df$y)

  0   1   2   3   4   5   6   7   8   9  10  11  12 
 17  40  54 109 103 124 100 121 108  88  74  43  19 
> 
> o0  <- table(df$y)[1]
> o1  <- table(df$y)[2]
> o2  <- table(df$y)[3]
> o3  <- table(df$y)[4]
> o4  <- table(df$y)[5]
> o5  <- table(df$y)[6]
> o6  <- table(df$y)[7]
> o7  <- table(df$y)[8]
> o8  <- table(df$y)[9]
> o9  <- table(df$y)[10]
> o10 <- table(df$y)[11]
> o11 <- table(df$y)[12]
> o12 <- table(df$y)[13]
> 
> # put the observed frequencies into a vector
> 
> o <- c(o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12)
> o
  0   1   2   3   4   5   6   7   8   9  10  11  12 
 17  40  54 109 103 124 100 121 108  88  74  43  19 
> 
```

* So, the test for model fit is:

```R
data.frame(o,e,o-e)
chi.sq <- (o-e)^2/e
sum(chi.sq)
dof <- 13-2
dof
pval <- 1-pchisq(sum(chi.sq),dof)
pval
```

which gives:

```Rout
> data.frame(o,e,o-e)
     o         e       o...e
0   17  15.42094   1.5790594
1   40  40.75232  -0.7523185
2   54  67.83955 -13.8395493
3  109  90.60545  18.3945451
4  103 106.18798  -3.1879848
5  124 114.69966   9.3003380
6  100 117.21441 -17.2144075
7  121 114.38691   6.6130861
8  108 106.40160   1.5984039
9   88  92.89133  -4.8913307
10  74  72.51787   1.4821315
11  43  44.88954  -1.8895428
12  19  16.19243   2.8075695
> chi.sq <- (o-e)^2/e
> sum(chi.sq)
[1] 11.37183
> dof <- 13-2
> dof
[1] 11
> pval <- 1-pchisq(sum(chi.sq),dof)
> pval
[1] 0.4126589
> 
```

* Now, we replot the joint distribution of *df$x* and *df$y*:

```R
plot(x=df$x,y=df$y)
```

and we now add the expected distribution of *y* conditional on different values of *x* to the plotspace:

```R
# calculate E(y|x=xstar) for range of xstar values

eyx <- vector()
xstar <- vector()

for (i in seq(from=1,to=1000,by=1))
  {
    # rescale 1 to 1000 --> -4 to 4

    lower <- -4
    upper <- 4
    min.i <- 1
    max.i <- 1000
    xstar[i] <- ((upper-lower)*(i-min.i)/(max.i-min.i))+lower

    pstar <- exp(a+b*xstar[i])/(1+exp(a+b*xstar[i]))
    p0  <- choose(12,0)*pstar^0*(1-pstar)^(12-0)
    p1  <- choose(12,1)*pstar^1*(1-pstar)^(12-1)
    p2  <- choose(12,2)*pstar^2*(1-pstar)^(12-2)
    p3  <- choose(12,3)*pstar^3*(1-pstar)^(12-3)
    p4  <- choose(12,4)*pstar^4*(1-pstar)^(12-4)
    p5  <- choose(12,5)*pstar^5*(1-pstar)^(12-5)
    p6  <- choose(12,6)*pstar^6*(1-pstar)^(12-6)
    p7  <- choose(12,7)*pstar^7*(1-pstar)^(12-7)
    p8  <- choose(12,8)*pstar^8*(1-pstar)^(12-8)
    p9  <- choose(12,9)*pstar^9*(1-pstar)^(12-9)
    p10 <- choose(12,10)*pstar^10*(1-pstar)^(12-10)
    p11 <- choose(12,11)*pstar^11*(1-pstar)^(12-11)
    p12 <- choose(12,12)*pstar^12*(1-pstar)^(12-12)
    eyx[i] <- p0*0+p1*1+p2*2+p3*3+p4*4+p5*5+p6*6+
              p7*7+p8*8+p9*9+p10*10+p11*11+p12*12
 }

# annotate the plot with a curve showing the relationship
# between xstar and E(y|x=xstar) implied by the statistical model

lines(x=xstar,y=eyx,pch=19,lty=1,lwd=3,col="blue")
```

and the resulting plot is:

<p align="left">
<img src="/gfiles/eyx-plot.png" width="600px">
</p>

##### Assignment Due Thursday 11/4/21

For this week's assignment, use the homework dataset I will send to you to study the relationship between *x* and *y* where *y* is a bounded count variable whose values range from 0 to 8. Assess the model fit and interpret the relationship between $x$ and $y$.

### Lesson 10 - Thursday 11/4/21

* For tonight's class, we turn to the issue of unbounded event count data.
* A good deal of criminological data takes the form of event counts so we will spend some time on this topic.
* Let's begin by reading in tonight's data set which consists of a continuous independent variable, *x*, a binary independent variable, *z*, and an unbounded event count outcome, *y*.
* Here is an exploratory data analysis:

```R
df <- read.csv(file="df.csv",sep=",",header=T)
table(df$y)

par(mfrow=c(2,2))
boxplot(df$y~df$z)
boxplot(df$x~df$z)

df.z0 <- subset(df,z==0)
df.z1 <- subset(df,z==1)

plot(x=df.z0$x,y=df.z0$y)
plot(x=df.z1$x,y=df.z1$y)
```

and here is the resulting plot:

<p align="left">
<img src="/gfiles/eda-plot.png" width="600px">
</p>

along with the marginal distribution of *y*:

```Rout
> table(df$y)

  0   1   2   3   4   5   6   7   8 
410 305 162  75  28  15   2   2   1 
> 
```

* Next, we estimate a linear regression model with outcome *y*, regressed on *x*, *z*, and the product of *x* and *z*:

```R
ls <- lm(y~1+x+z+x*z,data=df)
summary(ls)
logLik(ls)
```

and here is the output:


```Rout
> ls <- lm(y~1+x+z+x*z,data=df)
> summary(ls)

Call:
lm(formula = y ~ 1 + x + z + x * z, data = df)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.3967 -0.7028 -0.1697  0.5315  5.2266 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.87117    0.04942  17.629  < 2e-16 ***
x            0.42037    0.04991   8.422  < 2e-16 ***
z            0.37298    0.07081   5.267  1.7e-07 ***
x:z          0.22875    0.07113   3.216  0.00134 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.072 on 996 degrees of freedom
Multiple R-squared:  0.2512,	Adjusted R-squared:  0.2489 
F-statistic: 111.4 on 3 and 996 DF,  p-value: < 2.2e-16

> logLik(ls)
'log Lik.' -1486.748 (df=5)
> 
```

* Now, we calculate our own version of this estimator using the method of maximum likelihood:

```R
library(maxLik)

ls.ml <- function(parms)
  {
   a <- parms[1]
   b1 <- parms[2]
   b2 <- parms[3]
   b3 <- parms[4]
   sigma <- parms[5]
   yhat <- a+b1*df$x+b2*df$z+b3*df$x*df$z
   pt1 <- 1/(sigma*sqrt(2*pi))
   pt2 <- ((df$y-yhat)/sigma)^2
   pt3 <- exp(-1/2*pt2)
   pdf <- pt1*pt3
   lpdf <- log(pdf)
   return(lpdf)
  }

ls.nlm <- maxLik(ls.ml,start=c(0.83243243,0.42943023,0.3203243,0.2342402,1.03240320),
             method="BHHH",finalHessian="BHHH")
summary(ls.nlm)
```

and here is our output:


```Rout
> library(maxLik)
> 
> ls.ml <- function(parms)
+   {
+    a <- parms[1]
+    b1 <- parms[2]
+    b2 <- parms[3]
+    b3 <- parms[4]
+    sigma <- parms[5]
+    yhat <- a+b1*df$x+b2*df$z+b3*df$x*df$z
+    pt1 <- 1/(sigma*sqrt(2*pi))
+    pt2 <- ((df$y-yhat)/sigma)^2
+    pt3 <- exp(-1/2*pt2)
+    pdf <- pt1*pt3
+    lpdf <- log(pdf)
+    return(lpdf)
+   }
> 
> ls.nlm <- maxLik(ls.ml,start=c(0.83243243,0.42943023,0.3203243,0.2342402,1.03240320),
+              method="BHHH",finalHessian="BHHH")
> summary(ls.nlm)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 18 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1486.748 
5  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  0.87099    0.06170  14.117  < 2e-16 ***
[2,]  0.42048    0.05922   7.101 1.24e-12 ***
[3,]  0.37305    0.07866   4.742 2.11e-06 ***
[4,]  0.22851    0.07453   3.066  0.00217 ** 
[5,]  1.07011    0.02128  50.291  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Now that we have both of these results in hand, let's do some post-processing to assess fit.
* We begin by calculating some key quantities:

```R
ls.int <- coef(ls)[1]
ls.b1 <- coef(ls)[2]
ls.b2 <- coef(ls)[3]
ls.b3 <- coef(ls)[4]

ls.yhat <- ls.int+ls.b1*df$x+ls.b2*df$z+ls.b3*df$x*df$z
ls.u <- df$y-ls.yhat
rss <- sum(ls.u^2)
ncases <- nrow(df)
rmse <- sqrt((1/(ncases-2))*rss)
rmse
```

and here is the output:

```Rout
> ls.int <- coef(ls)[1]
> ls.b1 <- coef(ls)[2]
> ls.b2 <- coef(ls)[3]
> ls.b3 <- coef(ls)[4]
> 
> ls.yhat <- ls.int+ls.b1*df$x+ls.b2*df$z+ls.b3*df$x*df$z
> ls.u <- df$y-ls.yhat
> rss <- sum(ls.u^2)
> ncases <- nrow(df)
> rmse <- sqrt((1/(ncases-2))*rss)
> rmse
[1] 1.071233
> 
```

* Now, let's see what happens when we calculate expected cell frequencies based on this model:

```R
lsn3 <- vector()
lsn2 <- vector()
lsn1 <- vector()
ls0 <- vector()
ls1 <- vector()
ls2 <- vector()
ls3 <- vector()
ls4 <- vector()
ls5 <- vector()
ls6 <- vector()
ls7 <- vector()
ls8 <- vector()
ls9 <- vector()
ls10 <- vector()
ls11 <- vector()
ls12 <- vector()

for(i in 1:ncases){
  lsn3[i] <- pnorm(-2.5,mean=ls.yhat[i],sd=rmse)-pnorm(-3.5,mean=ls.yhat[i],sd=rmse)
  lsn2[i] <- pnorm(-1.5,mean=ls.yhat[i],sd=rmse)-pnorm(-2.5,mean=ls.yhat[i],sd=rmse)
  lsn1[i] <- pnorm(-0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-1.5,mean=ls.yhat[i],sd=rmse)
  ls0[i] <- pnorm(0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-0.5,mean=ls.yhat[i],sd=rmse)
  ls1[i] <- pnorm(1.5,mean=ls.yhat[i],sd=rmse)-pnorm(0.5,mean=ls.yhat[i],sd=rmse)
  ls2[i] <- pnorm(2.5,mean=ls.yhat[i],sd=rmse)-pnorm(1.5,mean=ls.yhat[i],sd=rmse)
  ls3[i] <- pnorm(3.5,mean=ls.yhat[i],sd=rmse)-pnorm(2.5,mean=ls.yhat[i],sd=rmse)
  ls4[i] <- pnorm(4.5,mean=ls.yhat[i],sd=rmse)-pnorm(3.5,mean=ls.yhat[i],sd=rmse)
  ls5[i] <- pnorm(5.5,mean=ls.yhat[i],sd=rmse)-pnorm(4.5,mean=ls.yhat[i],sd=rmse)
  ls6[i] <- pnorm(6.5,mean=ls.yhat[i],sd=rmse)-pnorm(5.5,mean=ls.yhat[i],sd=rmse)
  ls7[i] <- pnorm(7.5,mean=ls.yhat[i],sd=rmse)-pnorm(6.5,mean=ls.yhat[i],sd=rmse)
  ls8[i] <- pnorm(8.5,mean=ls.yhat[i],sd=rmse)-pnorm(7.5,mean=ls.yhat[i],sd=rmse)
  ls9[i] <- pnorm(9.5,mean=ls.yhat[i],sd=rmse)-pnorm(8.5,mean=ls.yhat[i],sd=rmse)
  ls10[i] <- pnorm(10.5,mean=ls.yhat[i],sd=rmse)-pnorm(9.5,mean=ls.yhat[i],sd=rmse)
  ls11[i] <- pnorm(11.5,mean=ls.yhat[i],sd=rmse)-pnorm(10.5,mean=ls.yhat[i],sd=rmse)
  ls12[i] <- pnorm(12.5,mean=ls.yhat[i],sd=rmse)-pnorm(11.5,mean=ls.yhat[i],sd=rmse)
  }

sum(lsn3)
sum(lsn2)
sum(lsn1)
sum(ls0)
sum(ls1)
sum(ls2)
sum(ls3)
sum(ls4)
sum(ls5)
sum(ls6)
sum(ls7)
sum(ls8)
sum(ls9)
sum(ls10)
sum(ls11)
sum(ls12)

lspvec <- c(sum(lsn3),sum(lsn2),sum(lsn1),sum(ls0),sum(ls1),
            sum(ls2),sum(ls3),sum(ls4),sum(ls5),sum(ls6),
            sum(ls7),sum(ls8),sum(ls9),sum(ls10),sum(ls11),sum(ls12))
sum(lspvec)

# calculate a chi-square goodness of fit test

yvals <- seq(from=-3,to=12,by=1)
yvec <- c(0,0,0,410,305,162,75,28,15,2,2,1,0,0,0,0)
data.frame(yvals,yvec,lspvec)
chisq.ls <- sum((yvec-lspvec)^2/lspvec)
chisq.ls
df.ls <- 16-5 
pvalue.ls <- 1-pchisq(chisq.ls,df.ls)
pvalue.ls
```

* Here is the output:

```Rout
> lsn3 <- vector()
> lsn2 <- vector()
> lsn1 <- vector()
> ls0 <- vector()
> ls1 <- vector()
> ls2 <- vector()
> ls3 <- vector()
> ls4 <- vector()
> ls5 <- vector()
> ls6 <- vector()
> ls7 <- vector()
> ls8 <- vector()
> ls9 <- vector()
> ls10 <- vector()
> ls11 <- vector()
> ls12 <- vector()
> 
> for(i in 1:ncases){
+   lsn3[i] <- pnorm(-2.5,mean=ls.yhat[i],sd=rmse)-pnorm(-3.5,mean=ls.yhat[i],sd=rmse)
+   lsn2[i] <- pnorm(-1.5,mean=ls.yhat[i],sd=rmse)-pnorm(-2.5,mean=ls.yhat[i],sd=rmse)
+   lsn1[i] <- pnorm(-0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-1.5,mean=ls.yhat[i],sd=rmse)
+   ls0[i] <- pnorm(0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-0.5,mean=ls.yhat[i],sd=rmse)
+   ls1[i] <- pnorm(1.5,mean=ls.yhat[i],sd=rmse)-pnorm(0.5,mean=ls.yhat[i],sd=rmse)
+   ls2[i] <- pnorm(2.5,mean=ls.yhat[i],sd=rmse)-pnorm(1.5,mean=ls.yhat[i],sd=rmse)
+   ls3[i] <- pnorm(3.5,mean=ls.yhat[i],sd=rmse)-pnorm(2.5,mean=ls.yhat[i],sd=rmse)
+   ls4[i] <- pnorm(4.5,mean=ls.yhat[i],sd=rmse)-pnorm(3.5,mean=ls.yhat[i],sd=rmse)
+   ls5[i] <- pnorm(5.5,mean=ls.yhat[i],sd=rmse)-pnorm(4.5,mean=ls.yhat[i],sd=rmse)
+   ls6[i] <- pnorm(6.5,mean=ls.yhat[i],sd=rmse)-pnorm(5.5,mean=ls.yhat[i],sd=rmse)
+   ls7[i] <- pnorm(7.5,mean=ls.yhat[i],sd=rmse)-pnorm(6.5,mean=ls.yhat[i],sd=rmse)
+   ls8[i] <- pnorm(8.5,mean=ls.yhat[i],sd=rmse)-pnorm(7.5,mean=ls.yhat[i],sd=rmse)
+   ls9[i] <- pnorm(9.5,mean=ls.yhat[i],sd=rmse)-pnorm(8.5,mean=ls.yhat[i],sd=rmse)
+   ls10[i] <- pnorm(10.5,mean=ls.yhat[i],sd=rmse)-pnorm(9.5,mean=ls.yhat[i],sd=rmse)
+   ls11[i] <- pnorm(11.5,mean=ls.yhat[i],sd=rmse)-pnorm(10.5,mean=ls.yhat[i],sd=rmse)
+   ls12[i] <- pnorm(12.5,mean=ls.yhat[i],sd=rmse)-pnorm(11.5,mean=ls.yhat[i],sd=rmse)
+   }
> 
> sum(lsn3)
[1] 1.516445
> sum(lsn2)
[1] 15.7885
> sum(lsn1)
[1] 83.11883
> sum(ls0)
[1] 223.3485
> sum(ls1)
[1] 313.3888
> sum(ls2)
[1] 236.9566
> sum(ls3)
[1] 99.36013
> sum(ls4)
[1] 23.27357
> sum(ls5)
[1] 2.96932
> sum(ls6)
[1] 0.1968668
> sum(ls7)
[1] 0.006459338
> sum(ls8)
[1] 0.0001008159
> sum(ls9)
[1] 7.27185e-07
> sum(ls10)
[1] 2.374561e-09
> sum(ls11)
[1] 3.455125e-12
> sum(ls12)
[1] 1.887379e-15
> 
> lspvec <- c(sum(lsn3),sum(lsn2),sum(lsn1),sum(ls0),sum(ls1),
+             sum(ls2),sum(ls3),sum(ls4),sum(ls5),sum(ls6),
+             sum(ls7),sum(ls8),sum(ls9),sum(ls10),sum(ls11),sum(ls12))
> sum(lspvec)
[1] 999.9241
> 
> # calculate a chi-square goodness of fit test
> 
> yvals <- seq(from=-3,to=12,by=1)
> yvec <- c(0,0,0,410,305,162,75,28,15,2,2,1,0,0,0,0)
> data.frame(yvals,yvec,lspvec)
   yvals yvec       lspvec
1     -3    0 1.516445e+00
2     -2    0 1.578850e+01
3     -1    0 8.311883e+01
4      0  410 2.233485e+02
5      1  305 3.133888e+02
6      2  162 2.369566e+02
7      3   75 9.936013e+01
8      4   28 2.327357e+01
9      5   15 2.969320e+00
10     6    2 1.968668e-01
11     7    2 6.459338e-03
12     8    1 1.008159e-04
13     9    0 7.271850e-07
14    10    0 2.374561e-09
15    11    0 3.455125e-12
16    12    0 1.887379e-15
> chisq.ls <- sum((yvec-lspvec)^2/lspvec)
> chisq.ls
[1] 10884.87
> df.ls <- 16-5 
> pvalue.ls <- 1-pchisq(chisq.ls,df.ls)
> pvalue.ls
[1] 0
> 
```

* Now, we are going to turn our attention to a regression model based on the Poisson probability distribution:

```R
m1 <- glm(y~1+x+z+x*z,data=df,family=poisson(link="log"))
summary(m1)
logLik(m1)
```

* here is our output:

```Rout
> m1 <- glm(y~1+x+z+x*z,data=df,family=poisson(link="log"))
> summary(m1)

Call:
glm(formula = y ~ 1 + x + z + x * z, family = poisson(link = "log"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3029  -1.0628  -0.2553   0.5330   2.9728  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.26778    0.05220  -5.130 2.90e-07 ***
x            0.57092    0.05523  10.338  < 2e-16 ***
z            0.38266    0.07104   5.387 7.17e-08 ***
x:z         -0.08638    0.06954  -1.242    0.214    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1470.7  on 999  degrees of freedom
Residual deviance: 1116.1  on 996  degrees of freedom
AIC: 2544.4

Number of Fisher Scoring iterations: 5

> logLik(m1)
'log Lik.' -1268.199 (df=4)
> 
```

* Notice that this model has one fewer parameter estimates than the normal linear regression model.
* Now, we consider the maximum likelihood estimator in detail:

```R
library(maxLik)

ll2 <- function(parms)
  {
   a <- parms[1]
   b1 <- parms[2]
   b2 <- parms[3]
   b3 <- parms[4]
   lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)
   pt1 <- exp(-lmb)
   pt2 <- lmb^df$y
   pt3 <- factorial(df$y)
   pmf <- pt1*pt2/pt3
   lpmf <- log(pmf)
   return(lpmf)
  }

m2 <- maxLik(ll2,start=c(0.87117,0.42037,0.37298,0.22875),
             method="BHHH",finalHessian="BHHH")
summary(m2)
```

* Here is the output:


```Rout
> library(maxLik)
> 
> ll2 <- function(parms)
+   {
+    a <- parms[1]
+    b1 <- parms[2]
+    b2 <- parms[3]
+    b3 <- parms[4]
+    lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)
+    pt1 <- exp(-lmb)
+    pt2 <- lmb^df$y
+    pt3 <- factorial(df$y)
+    pmf <- pt1*pt2/pt3
+    lpmf <- log(pmf)
+    return(lpmf)
+   }
> 
> m2 <- maxLik(ll2,start=c(0.87117,0.42037,0.37298,0.22875),
+              method="BHHH",finalHessian="BHHH")
> summary(m2)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 11 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1268.199 
4  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,] -0.26778    0.05210  -5.140 2.75e-07 ***
[2,]  0.57092    0.05415  10.544  < 2e-16 ***
[3,]  0.38266    0.07026   5.446 5.15e-08 ***
[4,] -0.08638    0.06790  -1.272    0.203    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Here is the fit analysis:

```R
# check on the model's fit

a <- coef(m2)[1]
a 
b1 <- coef(m2)[2]
b1 
b2 <- coef(m2)[3]
b2 
b3 <- coef(m2)[4]
b3 

lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)

e0  <- sum(exp(-lmb)*lmb^0/factorial(0))
e1  <- sum(exp(-lmb)*lmb^1/factorial(1))
e2  <- sum(exp(-lmb)*lmb^2/factorial(2))
e3  <- sum(exp(-lmb)*lmb^3/factorial(3))
e4  <- sum(exp(-lmb)*lmb^4/factorial(4))
e5  <- sum(exp(-lmb)*lmb^5/factorial(5))
e6  <- sum(exp(-lmb)*lmb^6/factorial(6))
e7  <- sum(exp(-lmb)*lmb^7/factorial(7))
e8  <- sum(exp(-lmb)*lmb^8/factorial(8))
e9  <- sum(exp(-lmb)*lmb^9/factorial(9))
e10  <- sum(exp(-lmb)*lmb^10/factorial(10))
e11  <- sum(exp(-lmb)*lmb^11/factorial(11))
e12  <- sum(exp(-lmb)*lmb^12/factorial(12))

table(df$y)
yvals <- seq(from=0,to=12,by=1)
yvec <- c(410,305,162,75,28,15,2,2,1,0,0,0,0)
e <- c(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
sum(e)

data.frame(yvals,yvec,e,yvec-e)
chi.sq <- (yvec-e)^2/e
sum(chi.sq)
dof <- 13-4
dof
pval <- 1-pchisq(sum(chi.sq),dof)
pval
```

* Here are the results:

```rout
> # check on the model's fit
> 
> a <- coef(m2)[1]
> a 
[1] -0.2677798
> b1 <- coef(m2)[2]
> b1 
[1] 0.5709187
> b2 <- coef(m2)[3]
> b2 
[1] 0.3826645
> b3 <- coef(m2)[4]
> b3 
[1] -0.08638074
> 
> lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)
> 
> e0  <- sum(exp(-lmb)*lmb^0/factorial(0))
> e1  <- sum(exp(-lmb)*lmb^1/factorial(1))
> e2  <- sum(exp(-lmb)*lmb^2/factorial(2))
> e3  <- sum(exp(-lmb)*lmb^3/factorial(3))
> e4  <- sum(exp(-lmb)*lmb^4/factorial(4))
> e5  <- sum(exp(-lmb)*lmb^5/factorial(5))
> e6  <- sum(exp(-lmb)*lmb^6/factorial(6))
> e7  <- sum(exp(-lmb)*lmb^7/factorial(7))
> e8  <- sum(exp(-lmb)*lmb^8/factorial(8))
> e9  <- sum(exp(-lmb)*lmb^9/factorial(9))
> e10  <- sum(exp(-lmb)*lmb^10/factorial(10))
> e11  <- sum(exp(-lmb)*lmb^11/factorial(11))
> e12  <- sum(exp(-lmb)*lmb^12/factorial(12))
> 
> table(df$y)

  0   1   2   3   4   5   6   7   8 
410 305 162  75  28  15   2   2   1 
> yvals <- seq(from=0,to=12,by=1)
> yvec <- c(410,305,162,75,28,15,2,2,1,0,0,0,0)
> e <- c(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
> sum(e)
[1] 999.9991
> 
> data.frame(yvals,yvec,e,yvec-e)
   yvals yvec            e     yvec...e
1      0  410 4.019905e+02  8.009510936
2      1  305 3.142258e+02 -9.225784756
3      2  162 1.639520e+02 -1.952041649
4      3   75 7.285088e+01  2.149118800
5      4   28 2.960919e+01 -1.609193223
6      5   15 1.128183e+01  3.718174039
7      6    2 4.063157e+00 -2.063156662
8      7    2 1.386217e+00  0.613783429
9      8    1 4.481976e-01  0.551802387
10     9    0 1.373807e-01 -0.137380732
11    10    0 3.994996e-02 -0.039949962
12    11    0 1.103420e-02 -0.011034197
13    12    0 2.898916e-03 -0.002898916
> chi.sq <- (yvec-e)^2/e
> sum(chi.sq)
[1] 4.019964
> dof <- 13-4
> dof
[1] 9
> pval <- 1-pchisq(sum(chi.sq),dof)
> pval
[1] 0.9100937
> 
```

* Here is some code to help us interpret the results:

```R
xstar <- seq(from=-3,to=3,by=0.1)
zstar <- c(0,1)
xz <- expand.grid(xstar,zstar)
xz

xz$xst <- xz$Var1
xz$zst <- xz$Var2
xz$lambda <- exp(a+b1*xz$x+b2*xz$z+b3*xz$xst*xz$zst)
plot(x=xstar,y=xz$lambda[1:61],ylab="E(lambda|x=xstar)",type="l",lty=1,lwd=2,col="red")
lines(x=xstar,y=xz$lambda[62:122],lty=1,lwd=2,col="blue")
```

* Here are the results:

```Rout
> xstar <- seq(from=-3,to=3,by=0.1)
> zstar <- c(0,1)
> xz <- expand.grid(xstar,zstar)
> xz
    Var1 Var2
1   -3.0    0
2   -2.9    0
3   -2.8    0
4   -2.7    0
5   -2.6    0
6   -2.5    0
7   -2.4    0
8   -2.3    0
9   -2.2    0
10  -2.1    0
11  -2.0    0
12  -1.9    0
13  -1.8    0
14  -1.7    0
15  -1.6    0
16  -1.5    0
17  -1.4    0
18  -1.3    0
19  -1.2    0
20  -1.1    0
21  -1.0    0
22  -0.9    0
23  -0.8    0
24  -0.7    0
25  -0.6    0
26  -0.5    0
27  -0.4    0
28  -0.3    0
29  -0.2    0
30  -0.1    0
31   0.0    0
32   0.1    0
33   0.2    0
34   0.3    0
35   0.4    0
36   0.5    0
37   0.6    0
38   0.7    0
39   0.8    0
40   0.9    0
41   1.0    0
42   1.1    0
43   1.2    0
44   1.3    0
45   1.4    0
46   1.5    0
47   1.6    0
48   1.7    0
49   1.8    0
50   1.9    0
51   2.0    0
52   2.1    0
53   2.2    0
54   2.3    0
55   2.4    0
56   2.5    0
57   2.6    0
58   2.7    0
59   2.8    0
60   2.9    0
61   3.0    0
62  -3.0    1
63  -2.9    1
64  -2.8    1
65  -2.7    1
66  -2.6    1
67  -2.5    1
68  -2.4    1
69  -2.3    1
70  -2.2    1
71  -2.1    1
72  -2.0    1
73  -1.9    1
74  -1.8    1
75  -1.7    1
76  -1.6    1
77  -1.5    1
78  -1.4    1
79  -1.3    1
80  -1.2    1
81  -1.1    1
82  -1.0    1
83  -0.9    1
84  -0.8    1
85  -0.7    1
86  -0.6    1
87  -0.5    1
88  -0.4    1
89  -0.3    1
90  -0.2    1
91  -0.1    1
92   0.0    1
93   0.1    1
94   0.2    1
95   0.3    1
96   0.4    1
97   0.5    1
98   0.6    1
99   0.7    1
100  0.8    1
101  0.9    1
102  1.0    1
103  1.1    1
104  1.2    1
105  1.3    1
106  1.4    1
107  1.5    1
108  1.6    1
109  1.7    1
110  1.8    1
111  1.9    1
112  2.0    1
113  2.1    1
114  2.2    1
115  2.3    1
116  2.4    1
117  2.5    1
118  2.6    1
119  2.7    1
120  2.8    1
121  2.9    1
122  3.0    1
> 
> xz$xst <- xz$Var1
> xz$zst <- xz$Var2
> xz$lambda <- exp(a+b1*xz$x+b2*xz$z+b3*xz$xst*xz$zst)
> plot(x=xstar,y=xz$lambda[1:61],ylab="E(lambda|x=xstar)",type="l",lty=1,lwd=2,col="red")
> lines(x=xstar,y=xz$lambda[62:122],lty=1,lwd=2,col="blue")
> 
```

and a plot:

<p align="left">
<img src="/gfiles/poisson-plot.png" width="600px">
</p>
