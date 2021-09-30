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

* Tonight's topic: multinomial logit and event count models.
