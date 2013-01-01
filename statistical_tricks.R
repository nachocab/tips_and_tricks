# boxplot: box-and-whisker plot, the data is broken up into quartiles: the box contains the middle 50% of the Ct values for each gene, the black bar is the median, the whiskers represent the top and bottom 25% of the observations. Circles represent outliers.

# standard error (of the sample mean): THINK ACCURACY - THE SD OF THE sample mean (x_bar), it describes the accuracy of the sample mean as an estimate of the population mean. an estimate of how close is my sample mean to the population mean (should decrease by increasing sample size): standard error of the mean (i.e., of using the sample mean as a method of estimating the population mean) is the standard deviation of those sample means over all possible samples (of a given size) drawn from the population.
    The sample mean has 95% prob of being within 2 se of the pop mean
# standard deviation (of the sample): THINK VARIABILITY - DESCRIBES THE SPREAD OF VALUES IN THE SAMPLE: how much do the individuals within the sample differ from the sample mean (unaffected by sample size) => it varies from sample to sample but stays the same on average when the sample size increases
    A new value has 95% prob of being within 2 sds of the sample mean

# The following four quantities have an intimate relationship: Given any three, we can determine the fourth: sample size, effect size,
# significance level: = P(Type I error) = probability of finding an effect that is not there
# power: = 1 - P(Type II error) = probability of finding an effect that is there

# marginal distribution of the statistics: the individual distribution of each statistic

# residual variance: unexplained variation, it's used to calculate the standard error of the estimate => sum of square differences between the y-value of each ordered pair in the regression line and each corresponding predicted y-value

# hyperparameter: parameter of a prior distribution (as opposed to a parameter of the model for the system being studied)

# statistic: a function of the (observed) data => therefore, also a random variable (X denotes a random variable corresponding to the observed data)

# estimator: a statistic used to infer the value of an unkown parameter in a statistical model. Ex: Parameter is a, estimator is â. If data is symbolized by random var X, the estimator is symbolized as a fn of that rand var â(X)
#              An estimator is judged by looking at its properties: unbiasedness, mean square error (MSE), consistency, asymptotic distribution...

# error of an estimator: e(x) = â(x) - a => the error depends both on the estimator (â) and on the sample data (x)

# error (of a sample): difference between the sample and the true function value (unobservable, based on the population).
# residual (of a sample): difference between the sample and the estimated function value (based on the sample).

# error vs residual => statistical error: diff between the height of each man in a random sample of n people and the (unobservable) population mean => the of errors is almost surely not zero => they are independent
#                      residual: diff between the height of each man and the sample mean => which means that the sum of residuals within a random sample is 0 => residuals are not independent

# z-score: measured in number of standard deviations above or below the mean => standardized statistical errors (based on population) => z = (x-MEAN)/SD
# t-statistic: ratio of the departure of an estimated parameter from its notional value and its SE

# expected value of the estimator: E(â(X))

# mean squared error: expected value of the squared errors => probability-weighted average over all samples of the squared errors => MSE(â) = E[(â(X)-a)^2] => how far on average are the collection of estimates from the parameter being estimated?
#                       MSE(â) = var(â) + (B(â))^2, if unbiased => MSE(â) == var(â)

# standard error of an estimator (â): sqrt(var(â)) == sd(â) => an estimate of the sd of â (an estimator of the parameter a)

# sampling deviation: d(x) = â(x) - E(â(X))

# variance: expected value of the squared sampling deviations => var(â) = E[(â-E(â))^2] => how far on average are the collection of estimates from the expected value of the estimates?

# difference between MSE and variance => bull's eye: parameter, throwing arrows: estimator, arrows: estimates of the estimator => high variance means the arrows are dispersed, low variance means the arrows are clustered. High MSE means the average distance between the arrows and the bull's eye is high, low MSE means the average distance between the arrows and the bull's eye is low. You can have high variance, low MSE arrows, and viceversa.

# bias (of an estimator): B(â) = E(â) - a = E(â-a) => difference between the average of the collection of estimates and the parameter, also == expected value of the error. High bias means the average position of the arrows is off-target, they may be dispersed (high variance) or clustered (low variance)

# unbiased: an estimator is unbiased iff B(â) = E(â-a) = 0 (you can't have a biased estimate, only biased estimators; the error of a single estimate is not the same as the bias an estimator: you can have an estimate with high error from an unbiased estimator)

# estimate: a realization of an estimator, which is a statistic, and therefore a random variable

# robust estimator: estimator that is resistant to errors in the results, produced from outliers and other small departures from model assumptions (usually, normality). Ex: median is a robust measure (estimator) of central tendency, the mean is not; the mean absolute deviation (MAD) and IQR are robust measure of statistical dispersion, the sd and range are not.

# TPR: sensitivity (hit rate, recall) => TPR = TP/P = TP/(TP+FN)
# SPC: specificity (TNR) = TN/N = TN/(FP+TN) = 1-FPR
# FPR: (false alarm rate, fall-out) => FPR = FP/N = FP/(FP+TN)

# ACC: accuracy = (TP + TN)/all
# PPV: precision (positive predicted value) = TP/(TP+FP)

# FDR: false discovery rate = FP/(FP+TP)
# NPV: negative predicted value = TN/(TN+FN)




# sleep dataset
sleep.wide <- data.frame(ID=1:10, group1=sleep$extra[1:10], group2=sleep$extra[11:20])

# Student t-test assumes equal variances - test whether two samples are drawn from populations with different means
t.test(extra ~ group, sleep, var.equal=TRUE) # long format
t.test(sleep.wide$group1, sleep.wide$group2, var.equal=TRUE) # t = -1.8608, df = 18, p-value = 0.07919

# Welch t-test doesn't assume equal variances - test whether two samples are drawn from populations with different means. How significant is the difference between two groups of observations?
t.test(extra ~ group, sleep)
t.test(sleep.wide$group1, sleep.wide$group2) # t = -1.8608, df = 17.776, p-value = 0.07939

# Paired-sample t-test - For observations before and after a treatment, or of two matched subjects with different treatments.
t.test(extra ~ group, sleep, paired=TRUE)
t.test(sleep.wide$group1, sleep.wide$group2, paired=TRUE) # t = -4.0621, df = 9, p-value = 0.002833

# Paired-sample t-test is the same as testing whether the difference between each pair of observations has a population mean of 0.
t.test(sleep.wide$group1 - sleep.wide$group2, mu=0, var.equal=TRUE) # t = -4.0621, df = 9, p-value = 0.002833

# One sample t-test - Comparing a group against an expecteed population mean
t.test(sleep$extra, mu=0) # t = 3.413, df = 19, p-value = 0.002918

# linear regression
lm(output~input)