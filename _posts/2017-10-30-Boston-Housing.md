What is Linear Regression?
==========================

Linear regression is a technique used to model the relationship between
the dependent X variable and the independent Y variable. In the case
here, we will be trying to build a model that describes the relationship
of housing features to predict housing price.

To quickly go over the math.

A linear model can be described as:

*y* = *a* \* *b**x*

Where:

y = average value of response variable

a = y intercept

b = coefficient

x = value of explanatory variable

If we had multiple predictors we can adjust this formula to be:

*y* = *a* \* *b*<sub>1</sub>*x* + *b*<sub>2</sub>*x* + *b*<sub>*n*</sub>*x*

Where *n* is the number of explanatory variables.

I'll go more into these formulas as we interpret the final model we
build.

Reading in the data.
--------------------

    library(MASS)
    df = Boston
    summary(df)

    ##       crim                zn             indus            chas        
    ##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
    ##  1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
    ##  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
    ##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
    ##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
    ##  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
    ##       nox               rm             age              dis        
    ##  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
    ##  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
    ##  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
    ##  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
    ##  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
    ##  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
    ##       rad              tax           ptratio          black       
    ##  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
    ##  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
    ##  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
    ##  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
    ##  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
    ##  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
    ##      lstat            medv      
    ##  Min.   : 1.73   Min.   : 5.00  
    ##  1st Qu.: 6.95   1st Qu.:17.02  
    ##  Median :11.36   Median :21.20  
    ##  Mean   :12.65   Mean   :22.53  
    ##  3rd Qu.:16.95   3rd Qu.:25.00  
    ##  Max.   :37.97   Max.   :50.00

Below is a definition of all the column labels.

    crim
    per capita crime rate by town.

    zn
    proportion of residential land zoned for lots over 25,000 sq.ft.

    indus
    proportion of non-retail business acres per town.

    chas
    Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).

    nox
    nitrogen oxides concentration (parts per 10 million).

    rm
    average number of rooms per dwelling.

    age
    proportion of owner-occupied units built prior to 1940.

    dis
    weighted mean of distances to five Boston employment centres.

    rad
    index of accessibility to radial highways.

    tax
    full-value property-tax rate per \$10,000.

    ptratio
    pupil-teacher ratio by town.

    black
    1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.

    lstat
    lower status of the population (percent).

    medv
    median value of owner-occupied homes in \$1000s.

Exploring the Data
------------------

    library(corrplot)
    df_cor = cor(df)
    corrplot.mixed(df_cor)

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-3-1.png)

We can see some features are highly correlated with each other, this is
called multicollinearity. This could be a problem later on when we try
to build models because small changes in either predictor will cause
large changes in the models.

    library(ggplot2)
    qplot(x=medv,data=df,geom='histogram')

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-4-1.png)

This plot is a histogram of the `medv` column which is the median price
of the house. We can the data is just ever-so-slightly skewed to the
right, meaning it is not normally distributed around the mean, but
distributed slightly below the mean. One of the assumptions of linear
regression is that the data is normally distributed around the mean. So,
to fix this, we can apply a log transformation to the price column.

    qplot(x=log(medv),data=df,geom='histogram')

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-5-1.png)
We can see that by doing the log transform of the median value, we now
have a slight left skew in the data, but we have reduced the large
outliers to the far right of the plot. Since the difference doesn't seem
to be that large, we can use the log transform since it looks 'more
normal' then the original data.

Building our first model
------------------------

We have to start somewhere. We know that we want to model the data
around median value so let's start by comparing all the variables to it.

    model1 = lm(log(medv)~., data= df)
    summary(model1)

    ## 
    ## Call:
    ## lm(formula = log(medv) ~ ., data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.73361 -0.09747 -0.01657  0.09629  0.86435 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.1020423  0.2042726  20.081  < 2e-16 ***
    ## crim        -0.0102715  0.0013155  -7.808 3.52e-14 ***
    ## zn           0.0011725  0.0005495   2.134 0.033349 *  
    ## indus        0.0024668  0.0024614   1.002 0.316755    
    ## chas         0.1008876  0.0344859   2.925 0.003598 ** 
    ## nox         -0.7783993  0.1528902  -5.091 5.07e-07 ***
    ## rm           0.0908331  0.0167280   5.430 8.87e-08 ***
    ## age          0.0002106  0.0005287   0.398 0.690567    
    ## dis         -0.0490873  0.0079834  -6.149 1.62e-09 ***
    ## rad          0.0142673  0.0026556   5.373 1.20e-07 ***
    ## tax         -0.0006258  0.0001505  -4.157 3.80e-05 ***
    ## ptratio     -0.0382715  0.0052365  -7.309 1.10e-12 ***
    ## black        0.0004136  0.0001075   3.847 0.000135 ***
    ## lstat       -0.0290355  0.0020299 -14.304  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1899 on 492 degrees of freedom
    ## Multiple R-squared:  0.7896, Adjusted R-squared:  0.7841 
    ## F-statistic: 142.1 on 13 and 492 DF,  p-value: < 2.2e-16

    par(mfrow=c(2,2))
    plot(model1)

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-7-1.png)
There are a few things I want to dive into here. First, let's start with
the long list of numbers above. The first column is titled 'Estimate'.
If we look back at our equation for multiple linear regression:

*y* = *a* \* *b*<sub>1</sub>*x* + *b*<sub>2</sub>*x* + *b*<sub>*n*</sub>*x*

The first number in the Estimate list is titled 'Intercept', this would
correspond to the 'a' value in the equation. Each subsequent number
would correspond to a new 'b' value. Where 'x' would be a variable we
were trying to predict. To interpret this estimate column, we say, it is
the change in response based on a 1-unit change in the explanatory
variable, holding all other variables constant. For example, let's look
at crime: `crim        -0.0102715` This says, holding all other
variables constant, we can except a decrease in price of
$log(-0.0102715). Generally speaking, more crime equals less a house
will cost. The next value would be the statistical t-value which can be
used to calculate the p-value which will tell you if the variable is
significant. We only want those variables that will add something to the
model, not those that are insignificant.

One way to pick the best model is to use AIC. Taken from Wikipedia,

"The Akaike information criterion (AIC) is a measure of the relative
quality of statistical models for a given set of data. Given a
collection of models for the data, AIC estimates the quality of each
model, relative to each of the other models. Hence, AIC provides a means
for model selection"

You can see why this method is very useful, it does all the hard work
for you.

    library(MASS)
    step = stepAIC(model1,direction = 'both')

    ## Start:  AIC=-1667.19
    ## log(medv) ~ crim + zn + indus + chas + nox + rm + age + dis + 
    ##     rad + tax + ptratio + black + lstat
    ## 
    ##           Df Sum of Sq    RSS     AIC
    ## - age      1    0.0057 17.755 -1669.0
    ## - indus    1    0.0362 17.786 -1668.2
    ## <none>                 17.749 -1667.2
    ## - zn       1    0.1643 17.914 -1664.5
    ## - chas     1    0.3088 18.058 -1660.5
    ## - black    1    0.5339 18.283 -1654.2
    ## - tax      1    0.6235 18.373 -1651.7
    ## - nox      1    0.9351 18.684 -1643.2
    ## - rad      1    1.0413 18.791 -1640.3
    ## - rm       1    1.0637 18.813 -1639.7
    ## - dis      1    1.3639 19.113 -1631.7
    ## - ptratio  1    1.9270 19.676 -1617.0
    ## - crim     1    2.1995 19.949 -1610.1
    ## - lstat    1    7.3809 25.130 -1493.2
    ## 
    ## Step:  AIC=-1669.03
    ## log(medv) ~ crim + zn + indus + chas + nox + rm + dis + rad + 
    ##     tax + ptratio + black + lstat
    ## 
    ##           Df Sum of Sq    RSS     AIC
    ## - indus    1    0.0363 17.791 -1670.0
    ## <none>                 17.755 -1669.0
    ## + age      1    0.0057 17.749 -1667.2
    ## - zn       1    0.1593 17.914 -1666.5
    ## - chas     1    0.3138 18.069 -1662.2
    ## - black    1    0.5431 18.298 -1655.8
    ## - tax      1    0.6205 18.376 -1653.7
    ## - nox      1    0.9645 18.720 -1644.3
    ## - rad      1    1.0356 18.791 -1642.3
    ## - rm       1    1.1452 18.900 -1639.4
    ## - dis      1    1.5471 19.302 -1628.8
    ## - ptratio  1    1.9224 19.677 -1619.0
    ## - crim     1    2.1988 19.954 -1612.0
    ## - lstat    1    8.1949 25.950 -1479.0
    ## 
    ## Step:  AIC=-1670
    ## log(medv) ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
    ##     black + lstat
    ## 
    ##           Df Sum of Sq    RSS     AIC
    ## <none>                 17.791 -1670.0
    ## + indus    1    0.0363 17.755 -1669.0
    ## + age      1    0.0058 17.786 -1668.2
    ## - zn       1    0.1451 17.936 -1667.9
    ## - chas     1    0.3399 18.131 -1662.4
    ## - black    1    0.5344 18.326 -1657.0
    ## - tax      1    0.6139 18.405 -1654.8
    ## - nox      1    0.9350 18.726 -1646.1
    ## - rad      1    1.0088 18.800 -1644.1
    ## - rm       1    1.1171 18.909 -1641.2
    ## - dis      1    1.7385 19.530 -1624.8
    ## - ptratio  1    1.8862 19.678 -1621.0
    ## - crim     1    2.2229 20.014 -1612.4
    ## - lstat    1    8.1604 25.952 -1481.0

    step$anova

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## log(medv) ~ crim + zn + indus + chas + nox + rm + age + dis + 
    ##     rad + tax + ptratio + black + lstat
    ## 
    ## Final Model:
    ## log(medv) ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
    ##     black + lstat
    ## 
    ## 
    ##      Step Df    Deviance Resid. Df Resid. Dev       AIC
    ## 1                              492   17.74938 -1667.194
    ## 2   - age  1 0.005723781       493   17.75510 -1669.031
    ## 3 - indus  1 0.036264380       494   17.79137 -1669.999

    step_model= lm(log(medv) ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                     black + lstat,data=df)

    summary(step_model)

    ## 
    ## Call:
    ## lm(formula = log(medv) ~ crim + zn + chas + nox + rm + dis + 
    ##     rad + tax + ptratio + black + lstat, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.73400 -0.09460 -0.01771  0.09782  0.86290 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.0836823  0.2030491  20.112  < 2e-16 ***
    ## crim        -0.0103187  0.0013134  -7.856 2.49e-14 ***
    ## zn           0.0010874  0.0005418   2.007 0.045308 *  
    ## chas         0.1051484  0.0342285   3.072 0.002244 ** 
    ## nox         -0.7217440  0.1416535  -5.095 4.97e-07 ***
    ## rm           0.0906728  0.0162807   5.569 4.20e-08 ***
    ## dis         -0.0517059  0.0074420  -6.948 1.18e-11 ***
    ## rad          0.0134457  0.0025405   5.293 1.82e-07 ***
    ## tax         -0.0005579  0.0001351  -4.129 4.28e-05 ***
    ## ptratio     -0.0374259  0.0051715  -7.237 1.77e-12 ***
    ## black        0.0004127  0.0001071   3.852 0.000133 ***
    ## lstat       -0.0286039  0.0019002 -15.053  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1898 on 494 degrees of freedom
    ## Multiple R-squared:  0.7891, Adjusted R-squared:  0.7844 
    ## F-statistic: 168.1 on 11 and 494 DF,  p-value: < 2.2e-16

We can see from the first command, that it's going through our initial
model and removing one variable and recalculating the AIC, which is a
rough measure of model goodness. Looking at the second the command, it
shows us what our first model was and what the final model. So if we
look at the summary of the new linear model with the predictors given to
us from the stepAIC, we can see that now we have removed all predictors
that are not significant and are adjusted R-square value has gone up,
slightly.

But there is still some work we can do.

Diagonstics to our new model
----------------------------

The car package has a nice function called variable inflation factor.
This is a measure of multicollinearity, or

"a phenomenon in which two or more predictor variables in a multiple
regression model are highly correlated, meaning that one can be linearly
predicted from the others with a substantial degree of accuracy." -
Wikipedia

This can be a major problem and we need to make sure that we are
accounting for it.

    library(car)
    vif(step_model)

    ##     crim       zn     chas      nox       rm      dis      rad      tax 
    ## 1.789704 2.239229 1.059819 3.778011 1.834806 3.443420 6.861126 7.272386 
    ##  ptratio    black    lstat 
    ## 1.757681 1.341559 2.581984

There is some discussion over the cutoff point of when to exclude the
predictor for multicollinearity. A good rule of thumb is if VIF &gt; 5,
you should exclude it from your analysis. Looking above, we have 2
variables we should remove, rad and tax.

    model2 = lm(log(medv)~crim+zn+chas+nox+dis+ptratio+black+lstat,data=df)
    summary(model2)

    ## 
    ## Call:
    ## lm(formula = log(medv) ~ crim + zn + chas + nox + dis + ptratio + 
    ##     black + lstat, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.70805 -0.12147 -0.01373  0.10405  0.82894 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.7010244  0.1421502  33.071  < 2e-16 ***
    ## crim        -0.0080105  0.0012719  -6.298 6.65e-10 ***
    ## zn           0.0013826  0.0005565   2.484 0.013311 *  
    ## chas         0.1302984  0.0363485   3.585 0.000371 ***
    ## nox         -0.7322562  0.1352610  -5.414 9.63e-08 ***
    ## dis         -0.0578514  0.0077684  -7.447 4.25e-13 ***
    ## ptratio     -0.0376285  0.0048687  -7.729 6.06e-14 ***
    ## black        0.0002958  0.0001120   2.642 0.008505 ** 
    ## lstat       -0.0353753  0.0017219 -20.545  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2025 on 497 degrees of freedom
    ## Multiple R-squared:  0.7584, Adjusted R-squared:  0.7545 
    ## F-statistic:   195 on 8 and 497 DF,  p-value: < 2.2e-16

    plot(model2)

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-10-1.png)![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-10-2.png)![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-10-3.png)![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-10-4.png)

So we can see this new model isn't modeling the data as well as our old
model, but all of the predictors are still significant and we have
removed the issue of multicollinearity.

    #multicollinearity?
    vif(model2)

    ##     crim       zn     chas      nox      dis  ptratio    black    lstat 
    ## 1.473811 2.074443 1.049493 3.024847 3.294717 1.367971 1.286382 1.861558

So what could be the problem? Let's start by going over some diagnostic
tests we can do.

First, the mean of the residuals should be equal to 0.

    # Assumption - mean of the residuals is = 0 
    mean(model2$residuals)

    ## [1] 4.05783e-18

    plot(model2$residuals)
    abline(h=0,col='red')

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Next, we should look at some outliers.

    library(car)
    outlierTest(model2)

    ##     rstudent unadjusted p-value Bonferonni p
    ## 413 4.270130         2.3419e-05     0.011850
    ## 372 4.052312         5.8859e-05     0.029782

The outliers test shows us that we have 2 points that should be removed.

    df2=df[-c(413,372),,drop=T]# drop the points
    row.names(df2)=1:nrow(df2)

    model3 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
                  ptratio + black + lstat,data=df2)
    par(mfrow=c(2,2))
    plot(model3)

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    summary(model3)

    ## 
    ## Call:
    ## lm(formula = log(medv) ~ crim + zn + chas + nox + rm + dis + 
    ##     ptratio + black + lstat, data = df2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.70878 -0.09807 -0.00993  0.08974  0.77161 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.6741127  0.1896496  19.373  < 2e-16 ***
    ## crim        -0.0084826  0.0011748  -7.220 1.97e-12 ***
    ## zn           0.0007490  0.0005193   1.442 0.149874    
    ## chas         0.1199066  0.0335829   3.570 0.000391 ***
    ## nox         -0.6147940  0.1258689  -4.884 1.40e-06 ***
    ## rm           0.1123588  0.0157923   7.115 3.96e-12 ***
    ## dis         -0.0447458  0.0073176  -6.115 1.97e-09 ***
    ## ptratio     -0.0333834  0.0045429  -7.348 8.36e-13 ***
    ## black        0.0004409  0.0001050   4.197 3.20e-05 ***
    ## lstat       -0.0289046  0.0018797 -15.377  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1869 on 494 degrees of freedom
    ## Multiple R-squared:  0.7937, Adjusted R-squared:  0.7899 
    ## F-statistic: 211.1 on 9 and 494 DF,  p-value: < 2.2e-16

With the removed outliers, we can see our model has increased in it's
Adjusted R-Square value! But we lost a predictor as being significant.
Let's try the stepAIC function on this new model with the new data.

    model3_step=stepAIC(model3,df2,direction = 'both')

    ## Start:  AIC=-1680.97
    ## log(medv) ~ crim + zn + chas + nox + rm + dis + ptratio + black + 
    ##     lstat
    ## 
    ##           Df Sum of Sq    RSS     AIC
    ## <none>                 17.247 -1681.0
    ## - zn       1    0.0726 17.320 -1680.8
    ## - chas     1    0.4451 17.692 -1670.1
    ## - black    1    0.6151 17.862 -1665.3
    ## - nox      1    0.8329 18.080 -1659.2
    ## - dis      1    1.3054 18.552 -1646.2
    ## - rm       1    1.7673 19.014 -1633.8
    ## - crim     1    1.8202 19.067 -1632.4
    ## - ptratio  1    1.8853 19.132 -1630.7
    ## - lstat    1    8.2551 25.502 -1485.8

    model3_step$anov

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## log(medv) ~ crim + zn + chas + nox + rm + dis + ptratio + black + 
    ##     lstat
    ## 
    ## Final Model:
    ## log(medv) ~ crim + zn + chas + nox + rm + dis + ptratio + black + 
    ##     lstat
    ## 
    ## 
    ##   Step Df Deviance Resid. Df Resid. Dev       AIC
    ## 1                        494   17.24699 -1680.969

    summary(model3_step)

    ## 
    ## Call:
    ## lm(formula = log(medv) ~ crim + zn + chas + nox + rm + dis + 
    ##     ptratio + black + lstat, data = df2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.70878 -0.09807 -0.00993  0.08974  0.77161 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.6741127  0.1896496  19.373  < 2e-16 ***
    ## crim        -0.0084826  0.0011748  -7.220 1.97e-12 ***
    ## zn           0.0007490  0.0005193   1.442 0.149874    
    ## chas         0.1199066  0.0335829   3.570 0.000391 ***
    ## nox         -0.6147940  0.1258689  -4.884 1.40e-06 ***
    ## rm           0.1123588  0.0157923   7.115 3.96e-12 ***
    ## dis         -0.0447458  0.0073176  -6.115 1.97e-09 ***
    ## ptratio     -0.0333834  0.0045429  -7.348 8.36e-13 ***
    ## black        0.0004409  0.0001050   4.197 3.20e-05 ***
    ## lstat       -0.0289046  0.0018797 -15.377  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1869 on 494 degrees of freedom
    ## Multiple R-squared:  0.7937, Adjusted R-squared:  0.7899 
    ## F-statistic: 211.1 on 9 and 494 DF,  p-value: < 2.2e-16

    par(mfrow=c(2,2))
    plot(model3_step)

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-15-1.png)

We can see that there is no difference in the models between model3 and
step\_model3 so we can continue with either. The R-squared values are
equal and there is no difference in variables. We can test for outliers
in this new model now.

    library(car)
    outlierTest(model3)

    ##     rstudent unadjusted p-value Bonferonni p
    ## 369 4.303254         2.0307e-05     0.010235
    ## 372 4.266323         2.3833e-05     0.012012

So we have 2 new outlier points in model 3, so we can remove them and
re-run the model with the new data.

    df3=df2[-c(371,400,373),,drop=T]
    row.names(df3)=1:nrow(df3)
    model4 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
                     tax + ptratio + black + lstat,data=df3)
    summary(model4)

    ## 
    ## Call:
    ## lm(formula = log(medv) ~ crim + zn + chas + nox + rm + dis + 
    ##     tax + ptratio + black + lstat, data = df3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.70835 -0.10150 -0.00960  0.08772  0.79154 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.624e+00  1.899e-01  19.086  < 2e-16 ***
    ## crim        -7.880e-03  1.217e-03  -6.473 2.34e-10 ***
    ## zn           7.611e-04  5.218e-04   1.459  0.14530    
    ## chas         1.062e-01  3.335e-02   3.185  0.00154 ** 
    ## nox         -5.614e-01  1.376e-01  -4.079 5.27e-05 ***
    ## rm           1.154e-01  1.550e-02   7.445 4.38e-13 ***
    ## dis         -4.340e-02  7.194e-03  -6.033 3.18e-09 ***
    ## tax         -6.026e-05  8.344e-05  -0.722  0.47055    
    ## ptratio     -3.255e-02  4.937e-03  -6.594 1.11e-10 ***
    ## black        4.396e-04  1.048e-04   4.195 3.23e-05 ***
    ## lstat       -2.858e-02  1.870e-03 -15.288  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1831 on 490 degrees of freedom
    ## Multiple R-squared:  0.7971, Adjusted R-squared:  0.7929 
    ## F-statistic: 192.4 on 10 and 490 DF,  p-value: < 2.2e-16

    par(mfrow=c(2,2))
    plot(model4)

![](Boston_notebook_files/figure-markdown_strict/unnamed-chunk-17-1.png)

We can see that we again have increased the power of our model but lost
significance on another variable. We can run the stepAIC and see if we
can remove it. Spoiler alert, we can. So to save you from the boredom of
more code. I'll just tell you we can remove them. We now that we have a
model that we can use.

With this final model, we have a Adjusted R-square value of 0.7929 which
means that the model can account for ~80% of the data, which isn't that
bad! We have a pretty good model for assessing the median value of a
house.

Next: Diagnostic tests for confirming the goodness of our model. These
tests range from outliers, to non-normality, to multi-collinearity. A
single test from above can not make or break a model but rather give you
insight to the model as a whole. No model will be perfect because data
is often messy and requires massaging, so what we want is a model that
can best explain the data.
