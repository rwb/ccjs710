### CCJS 710-0101 Limited Dependent Variables

* Meeting time: Thursday 4-6:45
* Classroom: LeFrak 2207
* Instructor: Bobby Brame (rbrame@umd.edu)
* Office: LeFrak 2139
* Standing office hours: Thursday 2-3:30 or by appointment.
* Prerequisite: Must have completed an approved doctoral level statistics course.
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
* 9/9: differences between proportions, relative risk statistics, odds ratios
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

* A key issue that arises in maximum likelihood estimation is studying the curvature of the log-likelihood function to obtain the Fisher information which can, in turn, be used to calculate the variances of the maximum likelihood estimate:

```r
# use finite difference approximation to calculate 
# second derivative of log-likelihood function
# this yields the observed Fisher information:

j <- -(logpi1-2*logpid+logpi0)/((theta1-theta0)/2)^2

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
