---
title: "Power & Sample Size: One-Way ANOVA"
author: "Shauna Rakshe"
date: "6/8/2022"
output: 
  html_document:
    theme: lumen
    keep_md: true
---


```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.1.3
```

```
## Warning: package 'tidyr' was built under R version 4.1.3
```

```r
library(pwr)
```

```
## Warning: package 'pwr' was built under R version 4.1.3
```

```r
library(broom)
```

# Recap: 2 sample t-test

If we want to compare the means of two independent groups using a 2-sample t-test, there are four parameters to consider when calculating power or sample size:

* Effect size $\Delta$: the difference in means

* $\alpha$: the Type I error rate (false positives)

* Power: the probability of correctly rejecting the null hypothesis

* Sample size n: the number of individual observations in each group

We can specify any three of these parameters and solve for the last one.  

# Effect size

With a two-sample t test, effect size is the difference between the two means. If the difference **between** the means of the two groups is big compared to the variation **within** a group, it's easy to see that the groups have different means.

$$\Delta = \mu_1 - \mu_2$$
This is hard to generalize to more than two means!

Think of the difference between the distribution of the test statistic under H_0 and the distribution under H_A.  

Describe the shift in distribution with a "noncentrality parameter", $\lambda$:
$$H_0 \sim t_{df}$$
$$H_A \sim t_{df, \lambda}$$

For our 2 sample t test:
$$\lambda = \frac{\mu_1 - \mu_2}{\sqrt{\sigma_1^2/n_1 + \sigma_2^2/n_2}}$$
With only 2 means to compare, it makes more sense to think of $\Delta$.  But for more complicated situations, we'll need to use $\lambda$ to calculate power or sample size.

![](PSS_ANOVA_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```
## 
##      Two-sample t test power calculation 
## 
##               n = 10
##               d = 1.538462
##       sig.level = 0.05
##           power = 0.9017519
##     alternative = two.sided
## 
## NOTE: n is number in *each* group
```

```
## 
##      Two-sample t test power calculation 
## 
##               n = 10
##               d = 0.7692308
##       sig.level = 0.05
##           power = 0.3703017
##     alternative = two.sided
## 
## NOTE: n is number in *each* group
```

![](PSS_ANOVA_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# Comparing more than two group means at once

What happens if we want to compare more than two means?  

* We could do a bunch of 2 sample t tests. But this would inflate the false positive rate.

* We can use ANOVA to test the equality of all the means at once.

* ANOVA is a generalization of the 2 sample t test to multiple groups.  The idea is the same: we'll compare variation **between** groups to variation **within** groups.

  + ANOVA won't identify which means are different.  This will only tell us if the means are the same, or if at least one is different from the others.
  
  + If we find evidence that not all the means are the same, we still have to compare the means pairwise to find out **which ones are different**
  
## How does ANOVA work?

See if the variance **between** groups is large compared to the variance **within** groups:

Break the total variation into two parts: 

  -Variation between a point and the mean of its group
  
  -Variation between the mean of the group and the overall (grand) mean

$$F = \frac{\textrm{variance between groups}}{\textrm{variance within groups}} = \frac{MSG}{MSE}$$

Say we're comparing blood biomarker levels in mice.  

  -Three different treatment groups
  
  -Does the average biomarker level differ between groups?



```
## Warning: `fun.y` is deprecated. Use `fun` instead.
```

```
## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.
```

![Comparing the means of 3 groups](PSS_ANOVA_files/figure-html/unnamed-chunk-3-1.png)

## A very basic ANOVA model:

- The response variable is continuous.

- We're comparing responses across fixed groups or categories.

- The variances within each group are equal.

- The individuals within each group and across groups are all independent.

- Within each group, the responses follow a roughly normal distribution.

We'll start by looking at one-way ANOVA.  This means we are looking at the mean response across **one category** (eg Treatment Group = Treatment 1, Treatment 2, or Placebo).

$$y_{ij} = \mu + \alpha_i + \epsilon$$
cell mean = grand mean + treatment effect + error

## Common pitfalls

- Violations of ANOVA model assumptions regarding response data distributions

  - Variances within the groups aren't roughly equal. 
  - Responses within each group aren't roughly normally distributed. 
  - Severe violations? You might have to use a nonparametric model.
  
Nonparametric models generally have much less power, so the sample size you need will be much bigger.

- Individuals aren't independent

  - Individuals in each group are measured twice, pre- and post-treatment.
  - If this is your situation, you need repeated-measures ANOVA. 

This changes the sample size calculations!

## One-way ANOVA example

Let's go back to our biomarker-level-in-mice experiment. We have three treatments to compare.  How many mice will we need to get 80% power for our experiment?

$$H_0: \mu_A = \mu_B = \mu_C$$
$$H_A: \textrm{At least one mean is different from the others.}$$
$$\alpha = 0.05$$

Just like in the 2 sample t test case, we need to specify 3 parameters and solve for the last:

- Effect size

- $\alpha$: the Type I error rate (false positives)

- Power: the probability of correctly rejecting the null hypothesis

- Sample size n: the number of individual observations in each group

**If we want to find n, we need to specify effect size. But how?**

# Effect size for ANOVA: Cohen's f

Many people use the effect size called Cohen's f for ANOVA power/sample size calculations.

$$f = 0.1 ~~\textrm{small effect}$$
$$f = 0.25 ~~\textrm{medium effect}$$
$$f = 0.4 ~~\textrm{large effect}$$

But this is just a rule of thumb.  

**How do you know what's meaningful for your experiment?**

![Power for different effect sizes as a function of n per group](PSS_ANOVA_files/figure-html/unnamed-chunk-4-1.png)


What is Cohen's f quantifying?

It's the square root of the variance in the means divided by the total variance.

$$f = \sqrt{\frac{\sigma^2_m}{\sigma^2}}$$

Why do we care?  It's related to the noncentrality parameter for the ANOVA F statistic:

$$\lambda = \frac{\sum_{i=1}^{G} n_i(\mu_i - \mu)^2}{\sigma^2}
=N \frac{\sigma^2_m}{\sigma^2} = Nf^2$$

**We can estimate a maximum and a minimum value for $\sigma^2_m$ from a difference in means.**

You probably have an idea what a meaningful difference in means would be for your experiment!

## Effect size part 2

Say we'd be interested if any two means were different by amount $\Delta$.

- Use $\Delta$ to calculate the minimum variance in means, $\sigma^2_m$ 
- Then calculate a minimum Cohen's f
- Repeat to find the maximum variance in means and maximum Cohen's f

**The minimum $\sigma_m$ occurs if two means differ by exactly $\Delta$, and all the other means are halfway between.**

The grand mean $\mu$ will be zero.

Assume all groups have an equal size. 

$$\sigma^2_m = \sum_{i=1}^{G}(\frac{n_i}{N})(\mu_i - \mu)^2 =\frac{n}{N}\sum_{i=1}^{G}(\mu_i - \mu)^2 $$

$$\sigma^2_m(min) = \frac{n}{N}\sum_{i = 1}^{G}(\frac{\Delta^2}{4}+\frac{\Delta^2}{4})=\frac{n}{N}(\frac{\Delta^2}{4}+\frac{\Delta^2}{4})=\frac{n}{N}\frac{\Delta^2}{2}=\frac{\Delta^2}{2G}$$

Then you can calculate a minimum Cohen's f:

$$f = \sqrt{\frac{\sigma_m^2(min)}{\sigma^2}}=\sqrt{\frac{\Delta^2}{2G\sigma^2}}$$

And the noncentrality parameter for one-way ANOVA becomes:
$$\lambda =N \frac{\sigma^2_m}{\sigma^2}=\frac{N\Delta^2}{2G\sigma^2}$$

## Takeaways for power/sample size calculations

Remember that $\lambda$ quantifies the difference between the distribution under H0 and the difference under HA.

**The bigger $\lambda$ is, the more power you will have.**

$$\lambda =\frac{N\Delta^2}{2G\sigma^2}$$
$\lambda$ is larger when:

- N (total number of observations) goes up

- The difference between at least one pair of means goes up

- $\sigma^2$ (overall variation in the data) goes down

Just like we got a minimum Cohen's f value for a $\Delta$ of interest, we can also calculate a maximum Cohen's f value, and get a range of power estimates.


```r
cohensfminmax <- function(Delta, sd, G){
  #calculate min cohen's f
sdm.min <- Delta/sqrt(2*G)
fmin <- sdm.min/sd

#calculate max cohen's f
#formula is different for odd vs even numbers of G
sdm.max <- ifelse(
  #check if even
  (G%%2)==0,
  #formula for even G
  Delta/2,
  #formula for odd G
  (Delta*sqrt(G^2-1))/(2*G)
)
fmax <- sdm.max/sd

#Make power curves
cohensf <- seq(fmin, fmax, by = (fmax-fmin)/5)
n <- c(seq(2, 10, by = 1), seq(12, 20, by = 2))

powerdata <- crossing(cohensf, n) %>%
  rowwise() %>%
  mutate(power = pwr::pwr.anova.test(
    f=cohensf,
    k=G,
    n = n,
    sig.level = 0.05
  ) %>% tidy() %>% pull(power) )
  
ggplot(powerdata, aes(x = n, y = power, color = factor(cohensf))) + 
  geom_line() + 
  theme_minimal() + 
  labs(
    x = "Number in each group (n)",
    y = "Power"
  )
}

cohensfminmax(Delta = 3, sd = 0.9, G =3)
```

![](PSS_ANOVA_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# But which means are different?

Maybe we found that at least one mean is different from the others.  YAY!  But...which mean is it?

We have to compare each pair of means to see where the difference is.
Mean of treatment 1 = mean of treatment 2?
Mean of treatment 1 = mean of treatment 3?
Mean of treatment 2 = mean of treatment 3?

As the number of group level means we compare goes up, the number of pairwise comparisons increases fast.  **If we perform each pairwise comparison with alpha = 0.05, the overall error rate will be much higher than 5%**.  

Control the overall (familywise) error rate by reducing the alpha level for each individual test.  
The simplest (Bonferroni) method is to divide alpha by the number of tests to be performed. Lots of methods exist!

If you're only interested in control vs each treatment, you can decide not to do the other comparisons.
**Make sure to power the comparisons you're most interested in.** 

# One-way ANOVA example:

Example: Mice getting two treatments compared to placebo, looking at blood levels of biomarker after 2 weeks (a continuous measure). Group 1 is Treatment A.  Group 2 is Treatment B.  Group 3 is Placebo.  We estimate that the maximum difference in means that we will detect is 3 units. Let's say we expect a standard deviation of 0.89 units from previous experimental work. We want equal numbers of mice in each group and 80% power for a test with Type 1 error rate 0.05.
$$G = 3\\
\alpha = 0.05\\
power = 0.80\\
\mu_1 = 1.5\\
\mu_2 = -1.5\\
\mu_3 = 0$$

Our F statistic will be $F = \frac{MSTr}{MSE}$, which will follow different F distributions under the null and alternative hypotheses. Our goal is to be able to distinguish between these two distributions with 80% power.  

If our F statistic is greater than the critical value, we'll reject H_0_.  So we need the F statistic under the H_A_ distribution to be greater than that critical value. 

If we have G groups with n mice each, then our total number of mice will be nG = N.  With 3 groups, the degrees of freedom of our F distributions will be df1 = G - 1 = 2 and df2 = nG - G = 3n - 3.

What will lambda be? 
$$\lambda = \frac{\sum n_i (\mu_i - \mu)^2}{\sigma^2} = \frac{1}{0.8}[1.5^2n+(-1.5)^2n + 0^2n]\\
=\frac{4.5n}{0.8}=5.625n$$

$$\textrm{Power = Pr(reject Ho | Ha is true)}\\
= Pr(F_{\textrm{under Ha}} \geq F^*)\\
= Pr(F_{G-1, N-G, \lambda} \geq F^*)\\
= Pr(F_{2, 3n-3, 5.625n} \geq F^*)$$

Let's see what kind of power we'd get with n = 3!


```r
G <- 3 #specify number of groups
n <- 3 #specify number in each group

df1 <- G-1 
df2 <- G*n - G 
lambda <- 5.625*n
alpha <- 0.05

#Find the critical value for the distribution under the null hypothesis
(Fcrit <- qf(alpha, G-1, G*n - G, lower.tail = F))
```

```
## [1] 5.143253
```

```r
#Find the probability of this critical value under Ha
pf(Fcrit, G-1, G*n-G, ncp = lambda, lower.tail = F)
```

```
## [1] 0.8069789
```


```r
#plot distributions

pdfdata <- tibble(fvar = seq(0, n*G*3, by = 0.1)) %>%
  mutate(
    #distribution under the null hypothesis
         H0 = df(fvar, df1 = df1, df2=df2),
  #distribution under the alternative hypothesis
         HA = df(fvar, df1 = df1, df2 = df2,                  ncp = lambda)) %>%
  pivot_longer(cols = c(H0, HA),
               names_to = "hypothesis",
               values_to = "pdf")

ggplot(pdfdata, aes(x = fvar, y = pdf,
                    color = hypothesis,
                    linetype = hypothesis)) +
  geom_line() + 
  geom_area(data = pdfdata %>% 
              filter(hypothesis == "HA"),
            aes(y=ifelse(fvar > Fcrit, pdf, 0),
                fill = "lightyellow")) +
  geom_area(data = pdfdata %>%
              filter(hypothesis == "H0"),
            aes(y=ifelse(fvar < Fcrit, pdf, 0),
                fill = "lightgrey")) + 
  geom_vline(xintercept = Fcrit, color = "red") + 
  theme_minimal() + 
  scale_color_viridis_d() +
  labs(title = "Distribution under H0 vs HA",
       subtitle = "G = 3, n = 3, lambda = 5.625n",
       x = "Value of F statistic")
```

![](PSS_ANOVA_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


The probability of getting a value at least as extreme as the critical value, given that the null hypothesis is false so that our results follow this non-central distribution, is 0.807.  This corresponds to 80.7% power!

**With a maximum difference in means of 3 (means: -1.5, 1.5, 0), n per group of 3, 3 groups, and a type I error rate of 0.05, we expect to have power of about 80.7%.**

What if our maximum difference in means was still 3, n per group was still 3, there were still 3 groups and the same type I error rate, but our means were arranged differently?

$$G = 3\\
\alpha = 0.05\\
\mu_1 = 1.5\\
\mu_2 = -1.5\\
\mu_3 = 1.5$$

$$\lambda = \frac{\sum n_i (\mu_i - \mu)^2}{\sigma^2} = \frac{1}{0.8}[1.5^2n+(-1.5)^2n + 1.5^2n]\\
=\frac{6.75n}{0.8}=8.438n$$


```r
G <- 3 #specify number of groups
n <- 3 #specify number in each group

df1 <- G-1 
df2 <- G*n - G 
lambda <- 8.4375*n
alpha <- 0.05

#Find the critical value for the distribution under the null hypothesis
(Fcrit <- qf(alpha, G-1, G*n - G, lower.tail = F))
```

```
## [1] 5.143253
```

```r
#Find the probability of this critical value under Ha
pf(Fcrit, G-1, G*n-G, ncp = lambda, lower.tail = F)
```

```
## [1] 0.9346705
```

**With this pattern of means, the power rises to 93.4%.**

You can see that for any given maximum difference in means, there are multiple ways for those means to be arranged.  The minimum value of lambda occurs when two means are set at the maximum difference apart ($\Delta/2$ and $-\Delta/2$), and all the others are equal to zero.  The maximum value of lambda occurs when all of the means are set at $\Delta/2$ and $-\Delta/2$, with roughly equal frequency.


## Example with effect size

We have 3 groups with n = 3 apiece.  The maximum difference in means that we expect to see is 3 and we expect the standard deviation to be about 0.89.

First pattern of means: 1.5, -1.5, 0.  (This will give the minimum lambda and minimum Cohen's f).
$$G = 3\\
\alpha = 0.05\\
power = 0.80\\
\mu_1 = 1.5\\
\mu_2 = -1.5\\
\mu_3 = 0$$

Second pattern of means: 1.5, -1.5, 1.5.  (This will give the maximum lambda and maximum Cohen's f).
$$G = 3\\
\alpha = 0.05\\
power = 0.80\\
\mu_1 = 1.5\\
\mu_2 = -1.5\\
\mu_3 = 1.5$$


```r
#specify parameters
Delta <- 3
sd <- 0.89
G <- 3

#calculate min cohen's f
sdm.min <- Delta/sqrt(2*G)
fmin <- sdm.min/sd
fmin   #1.376
```

```
## [1] 1.376118
```

```r
#calculate max cohen's f
#formula is different for odd vs even numbers of G
sdm.max <- ifelse(
  #check if even
  (G%%2)==0,
  #formula for even G
  Delta/2,
  #formula for odd G
  (Delta*sqrt(G^2-1))/(2*G)
)
fmax <- sdm.max/sd
fmax #1.589
```

```
## [1] 1.589004
```

```r
pwr.anova.test(k=3, n=3, f=1.376) #power = 81.1%
```

```
## 
##      Balanced one-way analysis of variance power calculation 
## 
##               k = 3
##               n = 3
##               f = 1.376
##       sig.level = 0.05
##           power = 0.8108169
## 
## NOTE: n is number in each group
```

```r
pwr.anova.test(k=3, n=3, f=1.589) #power = 90.8%
```

```
## 
##      Balanced one-way analysis of variance power calculation 
## 
##               k = 3
##               n = 3
##               f = 1.589
##       sig.level = 0.05
##           power = 0.9078715
## 
## NOTE: n is number in each group
```



# References


Huang, Yibi.  "Chapter 7: Power & Sample Size Calculation for ANOVA."  http://www.stat.uchicago.edu/~yibi/teaching/stat222/2017/Lectures/C07.pdf

PASS software documentation. "One-Way Analysis of Variance F-Tests Using Effect Size." "One Way Analysis of Variance Assuming Equal Variances (F-Tests)."

Ryan, Thomas P. *Sample Size Determination and Power*.  Hoboken: John Wiley & Sons, Inc., 2013.

