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

#### Lesson 1 - Thursday 9/2/21

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
subset(df.all,abs(all.likelihood-max(all.likelihood))<0.01)
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
> subset(df.all,abs(all.likelihood-max(all.likelihood))<0.01)
    theta all.likelihood
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
# slope for tangent line -- y = a + b*x

slope.line <- (logpi1-logpi0)/(theta1-theta0)
slope.line

# intercept for line -- a = y - b*x

int.line <- -2.841473-slope.line*thetad
int.line

# draw a line connecting the two points through plotspace

abline(a=int.line,b=slope.line,lty=1,lwd=1,col="blue")
```

* Here are the results:

```r
> # slope for tangent line -- y = a + b*x
> 
> slope.line <- (logpi1-logpi0)/(theta1-theta0)
> slope.line
[1] 5.505374e-05
> 
> # intercept for line -- a = y - b*x
> 
> int.line <- -2.841473-slope.line*thetad
> int.line
[1] -2.841483
> 
> # draw a line connecting the two points through plotspace
> 
> abline(a=int.line,b=slope.line,lty=1,lwd=1,col="blue")
```

<p align="left">
<img src="/gfiles/likelihood-plot.png" width="800px">
</p>

* A key issue that arises in maximum likelihood estimation is studying the curvature of the log-likelihood function to obtain the Fisher information which can, in turn, be used to calculate the variances of the maximum likelihood estimate:

```r
# use finite difference approximation to calculate 
# second derivative of log-likelihood function
# this yields the observed Fisher information:

j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
j
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
> # use finite difference approximation to calculate 
> # second derivative of log-likelihood function
> # this yields the observed Fisher information:
> 
> j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
> j
[1] 2097.966
> (logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2
[1] -2097.966
> 
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

* Please note that *j* is a positive number. Since *j* is the opposite sign of the second derivative of the log-likelihood function, this indicates that the log-likelihood function is concave down at its maximum.
* Next, we turn to the issue of using a ratio of likelihoods to test the hypothesis of equal failure rates between the two groups.
* We begin by imposing the equality constraint that both groups have the same failure rates. 
* Then, we calculate the likelihood for each group subject to the constraint that the failure rate is a constant value of 0.182

```r
# the value of theta = 0.182 maximizes the likelihood function
# note also that 57/313 = 0.182

# calculate likelihood function for each group
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
> # the value of theta = 0.182 maximizes the likelihood function
> # note also that 57/313 = 0.182
> 
> # calculate likelihood function for each group
> # holding theta constant at 0.182
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
# conditional on treat treatment is 0.109

p1.treat <- choose(n.treat,n.fail.treat)
p2.treat <- theta^(n.fail.treat)
p3.treat <- (1-theta)^(n.treat-n.fail.treat)
treat.likelihood <- p1.treat*p2.treat*p3.treat
treat.log.likelihood <- log(treat.likelihood)
df.treat <- data.frame(theta,treat.likelihood)
subset(df.treat,abs(treat.likelihood-max(treat.likelihood))<0.01)
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
> # conditional on treat treatment is 0.109
> 
> p1.treat <- choose(n.treat,n.fail.treat)
> p2.treat <- theta^(n.fail.treat)
> p3.treat <- (1-theta)^(n.treat-n.fail.treat)
> treat.likelihood <- p1.treat*p2.treat*p3.treat
> treat.log.likelihood <- log(treat.likelihood)
> df.treat <- data.frame(theta,treat.likelihood)
> subset(df.treat,abs(treat.likelihood-max(treat.likelihood))<0.01)
    theta treat.likelihood
98  0.097        0.1236199
99  0.098        0.1250755
100 0.099        0.1264038
101 0.100        0.1276031
102 0.101        0.1286719
103 0.102        0.1296095
104 0.103        0.1304151
105 0.104        0.1310888
106 0.105        0.1316307
107 0.106        0.1320415
108 0.107        0.1323222
109 0.108        0.1324741
110 0.109        0.1324988
111 0.110        0.1323984
112 0.111        0.1321751
113 0.112        0.1318314
114 0.113        0.1313701
115 0.114        0.1307944
116 0.115        0.1301074
117 0.116        0.1293126
118 0.117        0.1284138
119 0.118        0.1274147
120 0.119        0.1263192
121 0.120        0.1251316
122 0.121        0.1238559
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
subset(df.control,abs(control.likelihood-max(control.likelihood))<0.01)
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
> subset(df.control,abs(control.likelihood-max(control.likelihood))<0.01)
    theta control.likelihood
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
222 0.221         0.06258163
223 0.222         0.06187740
224 0.223         0.06110520
225 0.224         0.06026827
226 0.225         0.05936999
227 0.226         0.05841389
228 0.227         0.05740366
229 0.228         0.05634306
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

* Please note that the constrained model is in the numerator
and the free model is in the denominator:

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

* Now, we take a more conventional approach to the problem by
creating a rectangular data set:

```r
# create individual level dataset

yarr <- c(rep(1,10),rep(0,92-10))
yctl <- c(rep(1,47),rep(0,221-47))
y <- c(yarr,yctl)
arr <- c(rep(1,92),rep(0,221))
df <- data.frame(arr,y)
df

# crosstable (outcome on rows; treatment on columns)

ct <- table(df$y,df$arr,exclude=NULL)
ct

# conditional probabilities

py1arr0 <- ct[2,1]/(ct[1,1]+ct[2,1])
py1arr0
py1arr1 <- ct[2,2]/(ct[1,2]+ct[2,2])
py1arr1
```

* Here are the results:

```rout
> # create individual level dataset
> 
> yarr <- c(rep(1,10),rep(0,92-10))
> yctl <- c(rep(1,47),rep(0,221-47))
> y <- c(yarr,yctl)
> arr <- c(rep(1,92),rep(0,221))
> df <- data.frame(arr,y)
> df
    arr y
1     1 1
2     1 1
3     1 1
4     1 1
5     1 1
*
*
*
308   0 0
309   0 0
310   0 0
311   0 0
312   0 0
313   0 0
>
> # crosstable (outcome on rows; treatment on columns)
> 
> ct <- table(df$y,df$arr,exclude=NULL)
> ct
   
      0   1
  0 174  82
  1  47  10
> 
> # conditional probabilities
> 
> py1arr0 <- ct[2,1]/(ct[1,1]+ct[2,1])
> py1arr0
[1] 0.2126697
> py1arr1 <- ct[2,2]/(ct[1,2]+ct[2,2])
> py1arr1
[1] 0.1086957
> 
```

* We can use this information to calculate the relative risk
and odds ratio for these data:

```r
# relative risk and odds ratio

ct.rr <- py1arr1/py1arr0
ct.rr
ct.or.num <- py1arr1/(1-py1arr1)
ct.or.den <- py1arr0/(1-py1arr0)
ct.or <- ct.or.num/ct.or.den
ct.or
```
* And the results are:

```rout
> # relative risk and odds ratio
> 
> ct.rr <- py1arr1/py1arr0
> ct.rr
[1] 0.5111008
> ct.or.num <- py1arr1/(1-py1arr1)
> ct.or.den <- py1arr0/(1-py1arr0)
> ct.or <- ct.or.num/ct.or.den
> ct.or
[1] 0.451479
> 
```

* Next, we estimate the constrained and free logistic regression models:

```r
# constrained logistic regression model

constmodel <- glm(y~1,data=df,family=binomial(link="logit"))
summary(constmodel)
pfailconst <- exp(-1.5021)/(1+exp(-1.5021))
pfailconst
logLik(constmodel)

# free logistic regression model

freemodel <- glm(y~1+arr,data=df,family=binomial(link="logit"))
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
> freemodel <- glm(y~1+arr,data=df,family=binomial(link="logit"))
> summary(freemodel)

Call:
glm(formula = y ~ 1 + arr, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6915  -0.6915  -0.6915  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.3089     0.1644  -7.962 1.69e-15 ***
arr          -0.7952     0.3731  -2.131   0.0331 *  
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
relative risk and odds ratio statistics:

```r
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

#### Assignment Due Thursday 9/9/21

* Conduct a parallel analysis using the treatment-as-delivered data from the Minneapolis study. Here are the data you should use:

```r
n.treat <- 135
n.fail.treat <- 18

n.control <- 178
n.fail.control <- 39
```
