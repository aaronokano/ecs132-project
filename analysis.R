#//////// INIT PROCESS ////////

parkinson <- read.csv('parkinsons.data', header=TRUE)
logit <- function(x,y,t) {1/(1+exp(-(x+y*t))) }
logit2 <- function(x,y,z,t1,t2) {1/(1+exp(-(x+y*t1+z*t2))) }
logit3 <- function(x,y,z,w,t1,t2,t3) {1/(1+exp(-(x+y*t1+z*t2 + w*t3))) }
zero <- which(parkinson$status == 0)
one <- which(parkinson$status == 1)
crossvalglm <- function( response, predictor, predictor2, predictor3 ) {
  v <- sample( 1:(length(response) - 1), (length( response ) - 1) * 0.5 )
  notv <- setdiff( 1:(length(response) - 1), v )
  model <- glm( response[v] ~ predictor[v] + predictor2[v] + predictor3[v], family = binomial )
  cor( response[notv], mapply( logit3, model$coefficients[1], model$coefficients[2],
                              model$coefficients[3], model$coefficients[4], predictor[notv],
                              predictor2[notv], predictor3[notv]) )^2
}

crossvalglm2 <- function( response, predictor, predictor2 ) {
  v <- sample( 1:(length(response) - 1), (length( response ) - 1) * 0.5 )
  notv <- setdiff( 1:(length(response) - 1), v )
  model <- glm( response[v] ~ predictor[v] + predictor2[v], family = binomial )
  cor( response[notv], mapply( logit2, model$coefficients[1], model$coefficients[2],
                              model$coefficients[3], predictor[notv],
                              predictor2[notv]) )^2
}

crossvalglm3 <- function( response, predictor ) {
  v <- sample( 1:(length(response) - 1), (length( response ) - 1) * 0.5 )
  notv <- setdiff( 1:(length(response) - 1), v )
  model <- glm( response[v] ~ predictor[v], family = binomial )
  cor( response[notv], mapply( logit, model$coefficients[1], model$coefficients[2],
                              predictor[notv]
                              ) )^2
}

mean( sapply( 1:1000, function(n) crossvalglm2( parkinson$status,
                                               parkinson$PPE, parkinson$DFA *
                                               parkinson$RPDE) ) )


#//////// MDVP.Fo.Hz. ////////

str <- "glm(formula = parkinson$status ~ parkinson$MDVP.Fo.Hz., family = binomial)

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

logit(4.706084,-0.022051,181.9378)
#[1] 0.6668947 <- NOT GOOD
logit(4.706084,-0.022051,145.1808)
#[1] 0.8182747

#//////// MDVP.Fhi.Hz. ////////

str <- "glm(formula = parkinson$status ~ parkinson$MDVP.Fhi.Hz., family = binomial)

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
logit(1.871388,-0.003694,223.6368)
#[1] 0.6668947 <- NOT GOOD
logit(1.871388,-0.003694,188.4415)
#[1] 0.8182747

#//////// MDVP.Flo.Hz. ////////

str <- "glm(formula = parkinson$status ~ parkinson$MDVP.Flo.Hz., family = binomial)

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
logit(3.476248,-0.019075,145.2073)
#[1] 0.6696094 <- NOT GOOD
logit(3.476248,-0.019075,106.8936)
#[1] 0.8080288

#//////// MDVP.Jitter.Abs. ////////

str <- "glm(formula = parkinson$status ~ parkinson$MDVP.Jitter.Abs., 
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
logit(-1.0556,66665.3255,2.3375e-05)
#[1] 0.6230941 <- NOT GOOD
logit(-1.0556,66665.3255,5.068027e-05)
#[1] 0.9107654

#//////// spread1 analysis ///////////

str <- "glm(formula = parkinson$status ~ parkinson$spread1, family = binomial)

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
logit(15.8608,2.3966,-6.759246)
#[1] 0.416196 <--- ALMOST!
logit(15.8608,2.3966,-5.33342)
#[1] 0.9560066 <---- GREAT VALUE!

#//////// spread2 ///////////

str <- "glm(formula = parkinson$status ~ parkinson$spread2, family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.34813   0.09858   0.42402   0.70699   1.76286  

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)        -2.4283     0.6196  -3.919 8.89e-05 ***
parkinson$spread2  17.5354     3.1569   5.555 2.78e-08 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 170.40  on 193  degrees of freedom
AIC: 174.4

Number of Fisher Scoring iterations: 5"

mean(parkinson$spread2)
#[1] 0.2265103
mean(parkinson$spread2[zero])
#[1] 0.160292
mean(parkinson$spread2[one])
#[1] 0.2481327
logit(-2.4283,17.5354, 0.160292)
#[1] 0.5944722
logit(-2.4283,17.5354, 0.5944722)
#[1] 0.9996633

#///////////// PPE /////////////

str <- "glm(formula = parkinson$status ~ parkinson$PPE, family = binomial)

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
logit(-4.2214,32.2906,0.1230171)
#[1] 0.438044 <- ALMOST
logit(-4.2214,32.2906,0.2338282)
#[1] 0.9654122

#Confidence Interval
(22.13408 , 42.44712)

#/////////// NHR //////////

str <- "glm(formula = parkinson$status ~ parkinson$NHR, family = binomial)

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
logit(0.4717,39.8604,0.01148271)
#[1] 0.7169546 <- NOT GOOD
logit(0.4717,39.8604,0.02921095)
#[1] 0.8369981

#Confidence Interval
(10.69893 , 69.02187)

#////MDVP.Shimmer.dB ///

str <- "glm(formula = parkinson$status ~ parkinson$MDVP.Shimmer.dB., 
    family = binomial)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.65929   0.00326   0.34324   0.78365   1.34462  

Coefficients:
                           Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  -1.497      0.510  -2.934  0.00334 ** 
parkinson$MDVP.Shimmer.dB.   12.353      2.681   4.608 4.07e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 173.49  on 193  degrees of freedom
AIC: 177.49

Number of Fisher Scoring iterations: 6"


mean(parkinson$MDVP.Shimmer.dB.[zero]) 
#[1] 0.1629583
mean(parkinson$MDVP.Shimmer.dB.[one])
#[1] 0.3212041
logit(-1.497,12.353,0.1629583)
#[1] 0.6262175 <- NOT GOOD
logit(-1.497,12.353,0.3212041)
#[1] 0.9220717

#Confidence Interval
(7.09824, 17.60776)


#//// HNR ///////
str <- "glm(formula = parkinson$status ~ parkinson$HNR, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2409   0.1279   0.5052   0.7538   1.4748  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)     7.0326     1.3243   5.310 1.09e-07 ***
parkinson$HNR  -0.2576     0.0553  -4.658 3.19e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 187.55  on 193  degrees of freedom
AIC: 191.55

Number of Fisher Scoring iterations: 5"

mean(parkinson$HNR[zero])
#[1] 24.67875
mean(parkinson$HNR[one])
#[1] 20.97405
logit(7.0326,-0.2576,24.67875)
#[1]  0.662701 <- NOT GOOD
logit(7.0326,-0.2576,20.97405)
#[1] 0.8361264

#Confidence Interval
(-0.365988, -0.149212)

#///// RPDE ////

str <- "glm(formula = parkinson$status ~ parkinson$RPDE, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2648   0.3762   0.5411   0.7802   1.3864  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -2.4316     0.8531  -2.850  0.00437 ** 
parkinson$RPDE   7.4058     1.8059   4.101 4.12e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 198.53  on 193  degrees of freedom
AIC: 202.53

Number of Fisher Scoring iterations: 4"

mean(parkinson$RPDE[zero])
#0.4425519
mean(parkinson$RPDE[one])
#0.5168159
logit(-2.4316,7.4058, 0.4425519)
#0.699696 #<---- NOT GOOD
logit(-2.4316,7.4058, 0.5168159)
#0.8015222

#Confidence Interval
(3.866236 , 10.945364)

#////////// DFA ////////

str <- "glm(formula = parkinson$status ~ parkinson$DFA, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0150   0.4386   0.6410   0.7780   1.2959  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)   
(Intercept)     -6.151      2.291  -2.685  0.00726 **
parkinson$DFA   10.233      3.248   3.150  0.00163 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 217.65  on 194  degrees of freedom
Residual deviance: 206.93  on 193  degrees of freedom
AIC: 210.93

Number of Fisher Scoring iterations: 4"

mean(parkinson$DFA[zero])
#[1] 0.6957156
mean(parkinson$DFA[one])
#[1] 0.7254079
logit(-6.151,10.233,0.6957156)
#[1] 0.7247721 <- NOT GOOD
logit(-6.151,10.233,0.7254079)
#[1] 0.7811019 

#Confidence Interval
(3.86692 , 16.59908)
