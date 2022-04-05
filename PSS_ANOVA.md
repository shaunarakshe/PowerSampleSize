---
title: "PSS_ANOVA"
author: "Shauna Rakshe"
date: "4/4/2022"
output: 
  html_document:
    theme: lumen
    toc: yes
    toc_float:
      collapsed: yes
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

# Introduction/Recap

Recap of 2-sample t test PSS considerations.  Introduce noncentrality for t test?

# One-way ANOVA model

What happens if we want to compare more than two means?  
* We could do a bunch of 2 sample t tests. But this would inflate the false positive rate (more on this later).
* We can use ANOVA to test the equality of all the means at once!

## ANOVA basic idea

*Need to check with Dr. Minier how much people are likely to know already/how much depth she wants on this*

*Need graphic: scatterplot overlaid with box plots for 3 or more groups*

ANOVA (Analysis of Variance) compares the means of several different groups.  If the variation **between** groups is large compared to the variation **within** groups, the means of groups are likely to really be different.  On the other hand, if the variation **within** groups is nearly as large as the variation **between** groups, any calculated difference in means is likely due to chance.

A basic ANOVA model:
* The response variable is continuous.
* We're comparing responses across fixed groups or categories.
* The variances within each group are equal.
* The individuals within each group and across groups are all independent.

We'll start by looking at one-way ANOVA.  This means we are looking at the mean response across **one category** (eg Treatment Group = Treatment 1, Treatment 2, or Placebo).

Common pitfalls:
* Variances within the groups aren't equal. For now, let's assume we're using a mouse model with an established genotype, so we can expect the variance within each group of mice to be roughly similar.  If you expect wildly different variances between different groups for any reason, *tell your statistician early!* *Do we need to talk about this at all?  I saw some info about ANOVA power calcs with unequal variances, but I'm not sure if this is a common thing.*
* Individuals aren't independent
  * Individuals in each group are measured twice, pre- and post-treatment.
  * If this is your situation, you need repeated-measures ANOVA.  (We'll talk about this later today.) This changes the sample size calculations, so be sure that you discuss your research question and experimental design with your statistician!
  
Our hypotheses for ANOVA are:
H_0_: All the means are equal. 
H_A_: Not all the means are equal.  At least one mean is different from the others.

We also want to specify our Type I and Type II error rates.  Remember, Type I error is the probability that we will reject the null hypothesis given that it's actually true (a false positive), while Type II error is the probability that we'll fail to reject the null hypothesis when it's actually false (a false negative).  Here we will set the conventional values of alpha (Type I error rate) = 0.05 and beta (type II error rate) = 0.20, so power = 1- beta = 0.80.
$$H_0: \mu_A = \mu_B = \mu_C\\
H_A: \textrm{At least one mean is different from the others.}\\
\alpha = 0.05\\
\beta = 0.20\\
power = 1- \beta = 0.80$$

Say that we have G groups (in our example here, G = 3) with N total individuals in all the groups combined.  Our test statistic for rejecting (or failing to reject) the null hypothesis will follow the F distribution with G-1 numerator degrees of freedom and N-G denominator degrees of freedom.  We'll reject H_0_ if our F statistic is greater than the critical value:
$$F = \frac{MST}{MSE}=\frac{\frac{SST}{df1}}{\frac{SSE}{df2}} \gt F_{G-1, N-G, \alpha}$$
*show a picture of the F distribution, especially of how it changes with different degrees of freedom in numerator and denominator?  Maybe also say something about how the F distribution changes depending on the num and denom degrees of freedom, and how this makes sample calcs complicated. I think this might be a good place for a very simple shiny app -- sliders for G and N to change the shape of the F distribution.  Also, I feel like I usually see k instead of G, do you have a preference?*


```r
#example df curve with df1 = 10 and df2 = 20
df1 <- 10  
df2 <- 20 

ggplot(data.frame(x = c(0, 3)), aes(x = x)) + 
  stat_function(fun = df, 
                args = list(df1 = df1, df2 = df2))
```

![](PSS_ANOVA_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#for our example, 3 in each group
G <- 3 #specify number of groups
N <- 12 #specify total number of experimental units

df1 <- G-1 
df2 <- N-G 

ggplot(data.frame(x = c(0, 3)), aes(x = x)) + 
  stat_function(fun = df, 
                args = list(df1 = df1, df2 = df2)) 
```

![](PSS_ANOVA_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
#for our example, 3 in each group
G <- 3 #specify number of groups
N <- 21 #specify total number of experimental units

df1 <- G-1 
df2 <- N-G 

ggplot(data.frame(x = c(0, 3)), aes(x = x)) + 
  stat_function(fun = df, 
                args = list(df1 = df1, df2 = df2))
```

![](PSS_ANOVA_files/figure-html/unnamed-chunk-1-3.png)<!-- -->



If the alternative hypothesis is true, the F distribution won't be the same as what we'd expect under the null hypothesis.  How will it change?  The difference between the F distribution we'd expect under the alternative hypothesis (=there is a difference between the means of different groups) and the null hypothesis (=all the means are the same) is captured by the noncentrality parameter lambda.





# Two-way ANOVA model

# Interactions?

# Multiple comparisons considerations

# ANCOVA

# Unbalanced ANOVA

Do you ever run into anybody who wants to plan an unbalanced ANOVA?  We might be able to leave this out.

