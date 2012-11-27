This is the analysis for the spread1 by using the method outlined in part 
16.2.5


> g <- glm(parkinson$status ~ parkinson$spread1, family = binomial)
> summary(g)

Call:
glm(formula = parkinson$status ~ parkinson$spread1, family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.62075   0.01234   0.21992   0.51823   1.71295  

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)        15.8608     2.3924   6.630 3.37e-11 ***
parkinson$spread1   2.3966     0.3734   6.417 1.39e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 130.15  on 193  degrees of freedom
AIC: 134.15

Number of Fisher Scoring iterations: 6

> mean(parkinson$spread1)
[1] -5.684397
> mean(parkinson$spread1[parkinson$status == 0])
[1] -6.759264
> mean(parkinson$spread1[parkinson$status == 1])
[1] -5.33342
> zero <- which(parkinson$status == 0)
> one <- which(parkinson$status == 1)
> 1/(1+exp(-(15.8608+2.3966*-6.759246)))
[1] 0.416196 <--- ALMOST!
> 1/(1+exp(-(15.8608+2.3966*-5.33342)))
[1] 0.9560066 <---- GREAT VALUE!

//////// SPREAD2 analysis ///////////


> y <- function(x,y,t) {1/(1+exp(-(x+y*t))) }
> g <- glm(parkinson$status ~ parkinson$spread2, family = binomial)
> summary(g)

Call:
glm(formula = parkinson$status ~ parkinson$spread2, family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.34813   0.09858   0.42402   0.70699   1.76286  

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)        -2.4283     0.6196  -3.919 8.89e-05 ***
parkinson$spread2  17.5354     3.1569   5.555 2.78e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 170.40  on 193  degrees of freedom
AIC: 174.4

Number of Fisher Scoring iterations: 5

> mean(parkinson$spread2)
[1] 0.2265103
> mean(parkinson$spread2[zero])
[1] 0.160292
> mean(parkinson$spread2[one])
[1] 0.2481327
> y(-2.4283,17.5354, 0.160292)
[1] 0.5944722
> y(-2.4283,17.5354, 0.5944722)
[1] 0.9996633

///////////// PPE ANALYSIS /////////////

> g <- glm(parkinson$status ~ parkinson$PPE, family = binomial)
> summary(g)

Call:
glm(formula = parkinson$status ~ parkinson$PPE, family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.81009   0.00478   0.19873   0.54610   1.71619  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -4.2214     0.7854  -5.375 7.65e-08 ***
parkinson$PPE  32.2906     5.1819   6.231 4.62e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 131.57  on 193  degrees of freedom
AIC: 135.57

Number of Fisher Scoring iterations: 6

> mean(parkinson$PPE)
[1] 0.2065516
> mean(parkinson$PPE[zero])
[1] 0.1230171
> mean(parkinson$PPE[one])
[1] 0.2338282
> y(-4.2214,32.2906,0.1230171)
[1] 0.438044
> y(-4.2214,32.2906,0.2338282)
[1] 0.9654122


/////////// NHR ANALYSIS //////////

> g <- glm(parkinson$status ~ parkinson$NHR, family = binomial)
> summary(g)

Call:
glm(formula = parkinson$status ~ parkinson$NHR, family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-3.08266   0.00423   0.67699   0.83106   0.94919  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)   
(Intercept)     0.4717     0.2523   1.869  0.06160 . 
parkinson$NHR  39.8604    14.8783   2.679  0.00738 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 202.94  on 193  degrees of freedom
AIC: 206.94

Number of Fisher Scoring iterations: 6

> mean(parkinson$NHR[zero])
[1] 0.01148271
> mean(parkinson$NHR[one])
[1] 0.02921095
> y(0.4717,39.8604,0.01148271)
[1] 0.7169546
> y(0.4717,39.8604,0.02921095)
[1] 0.8369981


