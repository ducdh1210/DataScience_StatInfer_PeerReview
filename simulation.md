
Coursera Statistical Inference Project
========================================================
### Data Simulation
For our simulation, we are going to sample 40 random variables from the exponential distribution in 1000 simulation runs. Set lambda = 0.2 for all siumulations.


```r
library(ggplot2)
set.seed(25)
lambda <- 0.2
expectedMean <- expectedSd <- 1/lambda
numSamples <- 40
numSims <- 1000
sampleData <- matrix(rexp(numSamples * numSims, lambda), ncol = numSamples)
means <- apply(sampleData, 1, mean)
variances <- apply(sampleData, 1, var)
```
### Distribution of Sample Means
Let's first look at the distribution of our sample means

Answer 1) 

If we plot a histogram of the sample means we see that they are centered around the theoretical mean, which is of 1/0. 2 = 5. 

We also plot a normal distribution curve with a mean of 1/lambda and a standard deviation of (1/lambda)/sqrt(numSamples). It shows that our simulated variance of sample means is very close to theorital variance of sample means.

Answer 3) 

We also see that the distribution of our sample means is approximately normal. It conforms to Central Limit Theorem. 

![plot of chunk plotSampleMeans](figure/plotSampleMeans.png) 

```
## [1] "Our simulated mean of sample means =  4.99851106119285"
```

```
## [1] "Our theoritical mean of sample means =  5"
```

```
## [1] "Our simulated variance of sample means =  0.661985767399141"
```

```
## [1] "Our theoritical variance of sample means = 0.625"
```

### Distribution of Sample Variances
Answer 2)
The plot of the distribution of sample variancess shows that they are centered around (1/lambda)^2 = 25 as expected.
We also compute the mean of the sample variance. It value is close to 25, which proves that simulated mean of sample variance can be used as an estimate of theoretiacal variance of the distribution


```r
df <- as.data.frame(variances)
plot <- ggplot(df, aes(x=variances)) 
plot <- plot + geom_histogram(aes(y=..density..), colour="gray", fill="blue", binwidth=3)
plot <- plot + geom_vline(xintercept = (1/lambda)^2)
print(plot)
```

![plot of chunk plotSamplVariances](figure/plotSamplVariances.png) 

```r
print(paste("Our simulated means of sample variance = ", mean(variances)))
```

```
## [1] "Our simulated means of sample variance =  25.1075650995983"
```

```r
print(paste("Our theoritical variance of the distribution = ", 25))
```

```
## [1] "Our theoritical variance of the distribution =  25"
```

### Confidence Interval
Finally we calculate the 95% confidence interval for our sample means and show that the interval covers over 95% of our samples..
It shows that 95% confidence interval is covered from 3.3994 to 6.5976

```r
interval <- meanOfMeans + c(-1, 1) * qt(.975, numSamples - 1) * (1/lambda) / sqrt(numSamples)
covered <- sapply(means, function(mean)
{
      interval[1] < mean & interval[2] > mean 
})

print(paste("Our confidence interval =", toString(round(interval, 4))))
```

```
## [1] "Our confidence interval = 3.3994, 6.5976"
```

```r
print(paste(sum(covered) / numSims * 100, "% of our sample means are covered by this interval.", sep=""))
```

```
## [1] "95.2% of our sample means are covered by this interval."
```








