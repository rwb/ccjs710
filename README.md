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
* Reading for next week's class is Section 1.3 and Chapter 2 of Maddala (1983; [here](http://www.microlinkcolleges.net/elib/files/undergraduate/Economics/BOOK%20MADDALA%20limityed%20dependent%20var..pdf).
