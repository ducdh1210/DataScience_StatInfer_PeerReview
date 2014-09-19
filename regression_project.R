library(ggplot2)

#Data Simulation
set.seed(25)
lambda <- 0.2
expectedMean <- expectedSd <- 1/lambda
numSamples <- 40
numSims <- 1000
sampleData <- matrix(rexp(numSamples * numSims, lambda), ncol = numSamples)
means <- apply(sampleData, 1, mean)
variances <- apply(sampleData, 1, var)

# Distribution of sample mean
meanOfMeans <- mean(means)
df <- as.data.frame(means)
plot <- ggplot(df, aes(x=means)) 
plot <- plot + geom_histogram(aes(y=..density..), colour="gray", fill="#daf0dd", binwidth=0.1)
plot <- plot + stat_function(fun=dnorm, args=list(mean=expectedMean, sd=expectedSd/sqrt(numSamples)))
plot <- plot + geom_vline(xintercept = meanOfMeans)
print(plot)

print(paste("Our variance of sample means= ", var(means)))
print(paste("Our expected variance of sample means =", expectedSd^2/numSamples))

# Distribution of sample variance
df <- as.data.frame(variances)
plot <- ggplot(df, aes(x=variances)) 
plot <- plot + geom_histogram(aes(y=..density..), colour="gray", fill="#daf0dd", binwidth=3)
plot <- plot + geom_vline(xintercept = (1/lambda)^2)
print(plot)

# Confidence Interval
interval <- meanOfMeans + c(-1, 1) * qt(.975, numSamples - 1) * (1/lambda) / sqrt(numSamples)
covered <- sapply(means, function(mean)
{
  interval[1] < mean & interval[2] > mean 
})

print(paste("Our confidence interval =", toString(round(interval, 4))))
print(paste(sum(covered) / numSims * 100, "% of our sample means are covered by this interval.", sep=""))
