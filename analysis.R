#//////// INIT PROCESS ////////

parkinson <- read.csv('parkinsons.data', header=TRUE)
y <- function(x,y,t) {1/(1+exp(-(x+y*t))) }
zero <- which(parkinson$status == 0)
one <- which(parkinson$status == 1)

#//////// MDVP.Fo.Hz. ////////

g <- glm(parkinson$status ~ parkinson$MDVP.Fo.Hz., family = binomial)
summary(g)

str <- "Call:
glm(formula = parkinson$status ~ parkinson$MDVP.Fo.Hz., family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.1740   0.3574   0.4926   0.7070   1.2718  

Coefficients:
                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)            4.706084   0.775185   6.071 1.27e-09 ***
parkinson$MDVP.Fo.Hz. -0.022051   0.004446  -4.960 7.05e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 189.29  on 193  degrees of freedom
AIC: 193.29

Number of Fisher Scoring iterations: 4"

mean(parkinson$MDVP.Fo.Hz.[zero])
#[1] 181.9378
mean(parkinson$MDVP.Fo.Hz.[one])
#[1] 145.1808

y(4.706084,-0.022051,181.9378)
#[1] 0.6668947
y(4.706084,-0.022051,145.1808)
#[1] 0.8182747

#//////// MDVP.Fhi.Hz. ////////

g <- glm(parkinson$status ~ parkinson$MDVP.Fhi.Hz., family = binomial)
summary(g)

str <- "Call:
glm(formula = parkinson$status ~ parkinson$MDVP.Fhi.Hz., family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8233   0.6365   0.6804   0.7449   1.3084  

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)             1.871388   0.386236   4.845 1.26e-06 ***
parkinson$MDVP.Fhi.Hz. -0.003694   0.001670  -2.212    0.027 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 212.81  on 193  degrees of freedom
AIC: 216.81

Number of Fisher Scoring iterations: 4"

mean(parkinson$MDVP.Fhi.Hz.[zero])
#[1] 223.6368
mean(parkinson$MDVP.Fhi.Hz.[one])
#[1] 188.4415
y(1.871388,-0.003694,223.6368)
#[1] 0.6668947
y(1.871388,-0.003694,188.4415)
#[1] 0.8182747

#//////// MDVP.Flo.Hz. ////////

g <- glm(parkinson$status ~ parkinson$MDVP.Flo.Hz., family = binomial)
summary(g)

str <- "Call:
glm(formula = parkinson$status ~ parkinson$MDVP.Flo.Hz., family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0877   0.4531   0.5451   0.6736   1.3160  

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)             3.476248   0.536249   6.483 9.02e-11 ***
parkinson$MDVP.Flo.Hz. -0.019075   0.003937  -4.845 1.27e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 191.47  on 193  degrees of freedom
AIC: 195.47

Number of Fisher Scoring iterations: 4"

mean(parkinson$MDVP.Flo.Hz.[zero])
#[1] 145.2073
mean(parkinson$MDVP.Flo.Hz.[one])
#[1] 106.8936
y(3.476248,-0.019075,145.2073)
#[1] 0.6696094
y(3.476248,-0.019075,106.8936)
#[1] 0.8080288

#//////// MDVP.Jitter.Abs. ////////

g <- glm(parkinson$status ~ parkinson$MDVP.Jitter.Abs., family = binomial)
summary(g)

str <- "Call:
glm(formula = parkinson$status ~ parkinson$MDVP.Jitter.Abs., 
    family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.92964   0.00099   0.44182   0.81060   1.34642  

Coefficients:
                             Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   -1.0556     0.4089  -2.582  0.00983 ** 
parkinson$MDVP.Jitter.Abs. 66665.3255 13494.8481   4.940 7.81e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 173.78  on 193  degrees of freedom
AIC: 177.78

Number of Fisher Scoring iterations: 6"

mean(parkinson$MDVP.Jitter.Abs.[zero])
#[1] 2.3375e-05
mean(parkinson$MDVP.Jitter.Abs.[one])
#[1] 5.068027e-05
y(-1.0556,66665.3255,2.3375e-05)
#[1] 0.6230941
y(-1.0556,66665.3255,5.068027e-05)
#[1] 0.9107654

#//////// SPREAD1 analysis ///////////

g <- glm(parkinson$status ~ parkinson$spread1, family = binomial)
summary(g)

str <- "Call:
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

Number of Fisher Scoring iterations: 6"

mean(parkinson$spread1[zero])
#[1] -6.759264
mean(parkinson$spread1[one])
#[1] -5.33342
y(15.8608,2.3966,-6.759246)
#[1] 0.416196 <--- ALMOST!
y(15.8608,2.3966,-5.33342)
#[1] 0.9560066 <---- GREAT VALUE!

#//////// SPREAD2 analysis ///////////

g <- glm(parkinson$status ~ parkinson$spread2, family = binomial)
summary(g)

mean(parkinson$spread2)
#[1] 0.2265103
mean(parkinson$spread2[zero])
#[1] 0.160292
mean(parkinson$spread2[one])
#[1] 0.2481327
y(-2.4283,17.5354, 0.160292)
#[1] 0.5944722
y(-2.4283,17.5354, 0.5944722)
#[1] 0.9996633

#///////////// PPE ANALYSIS /////////////

g <- glm(parkinson$status ~ parkinson$PPE, family = binomial)
summary(g)

str <- "Call:
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
"

mean(parkinson$PPE)
#[1] 0.2065516
mean(parkinson$PPE[zero])
#[1] 0.1230171
mean(parkinson$PPE[one])
#[1] 0.2338282
y(-4.2214,32.2906,0.1230171)
#[1] 0.438044
y(-4.2214,32.2906,0.2338282)
#[1] 0.9654122


#/////////// NHR ANALYSIS //////////

g <- glm(parkinson$status ~ parkinson$NHR, family = binomial)
summary(g)

str <- "Call:
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
"

mean(parkinson$NHR[zero])
#[1] 0.01148271
mean(parkinson$NHR[one])
#[1] 0.02921095
y(0.4717,39.8604,0.01148271)
#[1] 0.7169546
y(0.4717,39.8604,0.02921095)
#[1] 0.8369981

