Casebase June 18th (3 Questions)
================

    ## Warning in k * e: Recycling array of length 1 in array-vector arithmetic is deprecated.
    ##   Use c() or as.vector() instead.

    ## Warning in k * e: Recycling array of length 1 in array-vector arithmetic is deprecated.
    ##   Use c() or as.vector() instead.

| death |    d.time |         V1 |         V2 |         V3 |         V4 |         V5 |
|------:|----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
|     0 | 0.0132678 | -1.3967724 | -1.8320955 | -1.3479005 | -0.9866042 | -0.6397470 |
|     1 | 0.0135560 | -0.4507505 | -1.2605280 |  0.0163649 | -0.5483301 |  0.4295402 |
|     0 | 0.1647498 | -0.5448065 | -2.1214950 | -2.6451039 | -3.5516801 | -1.4325106 |
|     1 | 0.3246748 | -1.5972992 | -1.7999827 | -0.4485799 | -2.3650921 | -2.9519653 |
|     0 | 0.9142944 | -0.9686751 | -3.6473056 | -0.9238270 |  1.5005274 | -3.1240724 |
|     1 | 0.1229913 |  0.0494438 | -0.6250478 | -2.2808895 | -1.7030362 |  0.5397049 |

The Generated Data

``` r
head(x_train)
```

    ##              V1         V2        V3          V4         V5        V6
    ## 758 -0.01977640  0.3736821 1.4983265 -1.53236158  0.8280106 1.2726331
    ## 659  1.52023119  1.6392526 1.1276025 -0.07341659 -0.5282388 1.9684596
    ## 946 -0.01834635  0.6912945 0.5015561 -0.42242888  0.3165624 1.0218067
    ## 353 -0.27005275 -0.6333812 0.2831906  0.70656387  1.2633599 0.2833530
    ## 706  1.22482891 -0.4662355 0.6811660  1.07515537 -0.3959277 0.7951504
    ## 271  1.71487526  1.7528646 0.6928033 -0.37086681 -0.3191993 0.2212617
    ##             V7         V8         V9        V10        V11        V12
    ## 758  0.6521816 -0.2285522 1.94666617 -0.5037643  0.3641398  0.2200194
    ## 659  1.7117778  0.6565627 0.34873950  0.3674779  0.8312269  0.2150864
    ## 946 -0.9197271 -1.1114080 0.02487401 -3.5240082 -0.2858132  1.2632933
    ## 353 -1.1516578 -2.3039778 0.10256837 -1.2968994  2.2878048 -2.4847318
    ## 706  0.4740976 -0.9190230 0.27662648  1.4113455 -2.0033110 -0.5247139
    ## 271  0.5970892  2.0311382 1.66897249  1.1915739  1.7338416  0.9538601
    ##             V13         V14        V15         V16        V17        V18
    ## 758  2.97112497  0.04306023  0.8472070  0.06402779  1.0552753  0.2401622
    ## 659 -1.00845041 -0.18169336  2.5550786  0.96215695  1.2118666  1.9923745
    ## 946 -2.46617968  0.73690850  0.7239741  0.06032303 -1.7610726  0.1627908
    ## 353 -1.64132004  0.52083998 -0.3097582  0.10676709 -1.6372265 -0.6242967
    ## 706 -0.17303677 -0.59456555  1.8041131 -0.36627654 -0.5611821  0.5918119
    ## 271 -0.08579067  0.71947279 -1.7959585 -0.46275826  0.2307932 -0.3012117
    ##            V19        V20
    ## 758  1.5835471 -0.6248536
    ## 659  1.6161834  0.6059964
    ## 946 -0.3910859  0.5495322
    ## 353  1.2304464  0.1352528
    ## 706 -0.2001744  0.1291000
    ## 271  1.5506065  0.9354353

``` r
head(y_train)
```

    ##     death      d.time
    ## 758     1 0.003110306
    ## 659     0 0.133753549
    ## 946     0 0.809418596
    ## 353     1 0.033504359
    ## 706     1 0.382277164
    ## 271     0 4.558215315

``` r
head(y_train_2)
```

    ## [1] 0.003110306  0.133753549+ 0.809418596+ 0.033504359  0.382277164 
    ## [6] 4.558215315+

``` r
head(x_test)
```

    ##            V1         V2          V3          V4         V5         V6
    ## 1  -1.3967724 -1.8320955 -1.34790046 -0.98660422 -0.6397470 -1.3304706
    ## 2  -0.4507505 -1.2605280  0.01636489 -0.54833013  0.4295402  0.9070205
    ## 22 -1.7066542 -1.6516059 -0.37897115 -1.94791767 -3.3141746 -2.0694854
    ## 25  0.1837185 -0.8397371  0.27414109 -1.22109935  0.3144348  0.6210323
    ## 26 -3.5448790 -1.6296287 -2.68170240 -2.33035575 -0.3738484 -2.8962277
    ## 32  1.0609322  1.6561353  0.94803703  0.02716139  1.7617738  1.9015794
    ##            V7         V8          V9        V10        V11        V12
    ## 1  -1.5355248 -2.4543334 -0.32529635  1.0952792  1.5344285 -0.5685063
    ## 2   0.7758785  0.1586082  1.58741979 -0.8370476 -0.3873850 -0.6199339
    ## 22 -1.5497312 -2.1323812  0.03502865 -0.9955968  0.6046084 -0.0133803
    ## 25 -0.4079956  1.6251989  0.76967601  2.6464593  1.8736877  1.3363753
    ## 26 -0.9045846 -1.3150425 -1.28027910 -2.2605449 -2.4259674 -1.3882253
    ## 32  1.2411291  1.3244445  1.21403759  0.7502845  0.2371234  1.0867710
    ##           V13        V14         V15        V16        V17         V18
    ## 1  -0.9869267 -2.2463667 -1.74635791 -2.1901456 -1.9284718 -0.11962460
    ## 2   0.5803676 -1.9418380  0.06008967 -0.7999502 -1.5581148 -0.50407446
    ## 22 -1.7798211 -1.4923716 -1.75915193 -3.6123766 -2.4300852 -2.51962479
    ## 25  1.4784031 -0.4496949  2.08698334  0.4095645 -0.9382461  1.67884200
    ## 26  0.2071036 -2.9824127 -4.36142197 -2.6742722 -3.5005979  0.02727076
    ## 32  3.3944976  1.3632566  2.62576645  0.4314251  1.1864806  0.55300619
    ##           V19         V20
    ## 1  -0.5580743  1.19161412
    ## 2  -1.8231107 -1.71532269
    ## 22 -0.7675056 -3.05629721
    ## 25  0.7252411  0.01794203
    ## 26 -1.2124322  0.03241006
    ## 32  0.8207121  3.21024731

## 1. Get the Casebase Concordance

How can we get the risk score predictions from the casebase model? The
predict function in the casebase package is drawn from glmnet with
family binomial.

The problem with the prediction function is that it asks for a newx of
21 variables when I have 20 X’s.

``` r
# (4) pen_cb (ridge)
ridge_pen_cb <- fitSmoothHazard.fit(x = x_train, y = y_train,
                    family = "glmnet",
                    time = "d.time",
                    event = "death",
                    formula_time = ~ log(d.time), # how hazard depends on time
                    alpha = 0, # ridge
                    ratio = 10, # ratio of size of base series to case series
                    standardize = TRUE,
                    penalty.factor = c(0, rep(1, ncol(x_train))) #0 and then number of 1's is number of columns in x
                    )

# predict(ridge_pen_cb, newx = x_test, type = "response")
x_test %>% ncol()
```

    ## [1] 20

``` r
coef(ridge_pen_cb) %>% head()
```

    ## 6 x 1 sparse Matrix of class "dgCMatrix"
    ##                     s1
    ## (Intercept) -0.7718764
    ## log(d.time) -0.2834753
    ## V1           0.4931499
    ## V2          -0.5162726
    ## V3           0.4602844
    ## V4          -0.4044314

predict adds log time in front of it, feed newx with log(d.time) in
front. it’s part of the logistic regression model. it will give you a
predicted risk score for a certain follow up time.

what is the formula for the predicted probability? what’s the cumulative
incidence function for a given covariate profile? write out that
formula, and double check that to get the CIF (F_hat (t)). F_hat (t) is
given a time t and covariate X, what is your predicted prob.

### sampleCaseBase function

``` r
# create a survival object from the dataset
data <- as.data.frame(cbind(y_train, x_train))
survObj <- Surv(data[["d.time"]],
     data$death,
     type = "right")

n <- nrow(survObj) # number of subjects
B <- sum(survObj[, "time"]) # sum of all the times from all subjects
c <- sum(survObj[, "status"]) # number of cases (sum of all failures 1's)
ratio = 10
b <- ratio * c # size of the base series
offset <- log(B/b)

# select person-moments from individuals proportional to their total follow-up time
prob_select <- survObj[, "time"]/B # all individual's time/total_time (the prob of selecting that individ's time out of all other individ's time)
which_pm <- sample(n, b, replace = TRUE, prob = prob_select) # sample b bases out of n subjects with replacement, each n having a unique prob_select 
bSeries <- as.matrix(survObj[which_pm,]) # get all times and status of all n subjects chosen (chosen with repeats)
bSeries[, "status"] <- 0 # turn all status' into 0's
bSeries[, "time"] <- bSeries[, "time"] * runif(b) # multiply all times with a random number from Unif(0, 1) distribution

# combine base series with covariate data
selectTimeEvent <- !(colnames(data) %in% c("d.time", "death")) # logical vector of TRUE if X col and FALSE if not
bSeries <- cbind(bSeries,
      subset(data, select = selectTimeEvent)[which_pm, ,
                                             drop = FALSE]) # combine base series time and status with corresponding X's from original data (use drop = FALSE to ensure it is a dataframe)

# rename columns
names(bSeries)[names(bSeries) == "status"] <- "death"
names(bSeries)[names(bSeries) == "time"] <- "d.time"

# get case series
cSeries <- data[which(subset(data, select = (names(data) == "death")) != 0),] # get all rows from original data with cases (which function returns the row positions of all the death = 1, then take all the original data columns of those rows)

# combine case and base series
cbSeries <- rbind(cSeries, bSeries)

# add offset to the data
cbSeries <- cbind(cbSeries, rep_len(offset, nrow(cbSeries))) # rep_length just replicates the one offset value for all the rows 
names(cbSeries)[ncol(cbSeries)] <- "offset" # rename offset col
class(cbSeries) <- c("cbData", class(cbSeries)) # set class to be both cbData and data.frame

head(cbSeries)
```

    ##     death      d.time          V1         V2         V3         V4         V5
    ## 758     1 0.003110306 -0.01977640  0.3736821  1.4983265 -1.5323616  0.8280106
    ## 353     1 0.033504359 -0.27005275 -0.6333812  0.2831906  0.7065639  1.2633599
    ## 706     1 0.382277164  1.22482891 -0.4662355  0.6811660  1.0751554 -0.3959277
    ## 280     1 0.217314516 -2.94620738 -3.0412593 -3.1780980 -4.0399342 -3.5980669
    ## 773     1 0.166967675 -0.08047593  1.4018180 -0.5122788 -0.5614625  0.9345282
    ## 615     1 0.027934578 -0.06128434 -1.6691642 -0.8975126 -1.8856801 -1.5989837
    ##             V6         V7         V8          V9        V10        V11
    ## 758  1.2726331  0.6521816 -0.2285522  1.94666617 -0.5037643  0.3641398
    ## 353  0.2833530 -1.1516578 -2.3039778  0.10256837 -1.2968994  2.2878048
    ## 706  0.7951504  0.4740976 -0.9190230  0.27662648  1.4113455 -2.0033110
    ## 280 -2.6932264 -1.0662647 -2.8885815 -1.23731453 -3.6342930 -3.2100835
    ## 773 -1.8716410 -0.1824107 -0.8558827  1.31095590 -2.1225090 -0.9398336
    ## 615 -2.6827900 -0.5173234 -2.4556388  0.08822874 -0.6783693 -2.0290819
    ##            V12        V13         V14        V15         V16        V17
    ## 758  0.2200194  2.9711250  0.04306023  0.8472070  0.06402779  1.0552753
    ## 353 -2.4847318 -1.6413200  0.52083998 -0.3097582  0.10676709 -1.6372265
    ## 706 -0.5247139 -0.1730368 -0.59456555  1.8041131 -0.36627654 -0.5611821
    ## 280 -1.5560581 -3.7751741 -5.12609208 -4.2950453 -1.94469244 -4.9402040
    ## 773 -0.4103777 -2.8928239 -1.14581069  0.2207291  0.49685213 -0.4233195
    ## 615 -1.5441438 -1.1898789 -2.07929478 -2.0131183 -0.78301525 -1.0061523
    ##            V18        V19        V20    offset
    ## 758  0.2401622  1.5835471 -0.6248536 -1.532332
    ## 353 -0.6242967  1.2304464  0.1352528 -1.532332
    ## 706  0.5918119 -0.2001744  0.1291000 -1.532332
    ## 280 -3.0461870 -2.9977882 -4.9589474 -1.532332
    ## 773  1.0534363  0.1125925 -1.9210555 -1.532332
    ## 615 -0.5197611 -0.3115921 -1.4217089 -1.532332

### fitSmoothHazard.fit using sampleCaseBase

``` r
# call sampleCaseBase
sampleData <- cbSeries # after running the sampleCaseBase function, it returns sampleData

# format everything into matrices and expand variables that need to be
sample_event <- as.matrix(sampleData[, "death"]) # get the death col of 0s and 1s from cbSeries
sample_time <- model.matrix(update(~ log(d.time), ~ . -1), sampleData) # output a column that is the log of d.time in sampleData (only do this if the family = "glmnet" or "gbm") # else, model.matrix(~ log(d.time), sampleData)
sample_time_x <- cbind(sample_time,
                       as.matrix(sampleData[, !names(sampleData) %in% c("death", "d.time", "offset")])) # get all the X col and add log(d.time) col
sample_offset <- sampleData$offset

# fit a binomial model (assume no competing risks) (cv.glmnet_offset_hack will do this)

# out <- cv.glmnet_offset_hack(sample_time_x, sample_event,
#                              family = "binomial",
#                              offset = sample_offset, ...) # fit this if glmnet, if glm: glm.fit(sample_time_x, sample_event, family = binomial(), offset = sample_offset)
```

### cv.glmnet_offset_hack

``` r
offset <- sample_offset
# checks if the offset values are the same in the offset vector
if (diff(range(offset)) > 1e-06) {
    stop("Glmnet is only available with constant offset",
      call. = FALSE
    )
  }

offset_value <- unique(offset)[1]

# 1. Fit without offset
out <- cv.glmnet(x = sample_time_x, y = sample_event,
                 family = "binomial",
                 offset = sample_offset) # fit a binomial model (logistic regression) without offset

# 2. Fix the intercept
out$glmnet.fit$a0 <- out$glmnet.fit$a0 - offset_value # a0 returns a list of intercept values for each lambda value (strength of the penalty), we fix the intercept by subtracting all intercept values with the offset value

print(out)
```

    ## 
    ## Call:  cv.glmnet(x = sample_time_x, y = sample_event, offset = sample_offset,      family = "binomial") 
    ## 
    ## Measure: Binomial Deviance 
    ## 
    ##       Lambda Index Measure      SE Nonzero
    ## min 0.000135    71  0.4169 0.01308      21
    ## 1se 0.003189    37  0.4287 0.01484      20

### fitSmoothHazard.fit after cv.glmnet_offset_hack

``` r
# adding additional arguments
out$originalData <- list(
  "x" = x_train,
  "y" = y_train
)
out$typeEvents <- sort(unique(y_train[, "death"]))
out$timeVar <- "d.time"
out$eventVar <- "death"
out$matrix.fit <- TRUE
out$formula_time <- ~ log(d.time)
out$offset <- sample_offset

# create new class
class(out) <- c("singleEventCB", class(out))

print(out)
```

    ## 
    ## Call:  cv.glmnet(x = sample_time_x, y = sample_event, offset = sample_offset,      family = "binomial") 
    ## 
    ## Measure: Binomial Deviance 
    ## 
    ##       Lambda Index Measure      SE Nonzero
    ## min 0.000135    71  0.4169 0.01308      21
    ## 1se 0.003189    37  0.4287 0.01484      20

``` r
set.seed(124)
# sanity checks of logistic regression in cv.glmnet
data(BinomialExample)
x <- BinomialExample$x # x is all the X's
y <- BinomialExample$y # y is 0's or 1's

# reduce X to 2 columns
x_2 <- x[,1:2]

# fit a simple binomial model
fit <- cv.glmnet(x = x_2, y = y, family = "binomial")

# betas for linear predictors
coef(fit, s = "lambda.min")
```

    ## 3 x 1 sparse Matrix of class "dgCMatrix"
    ##                    s1
    ## (Intercept) 0.2616969
    ## V1          .        
    ## V2          0.3841829

``` r
# check predict link: compare the function to get linear predictors with manual method
predict(fit, newx = c(1, 2), type = "link", s = "lambda.min")
```

    ##      lambda.min
    ## [1,]   1.030063

``` r
0.26169 + 0.3841829*2
```

    ## [1] 1.030056

``` r
# check predict response: compare the function to get the fitted probability of p_i or P(Y_i = 1) with manual method based off of linear pred
predict(fit, newx = c(1,2), type = "response", s = "lambda.min")
```

    ##      lambda.min
    ## [1,]   0.736928

``` r
1/(1 + exp(-predict(fit, newx = c(1, 2), type = "link", s = "lambda.min")))
```

    ##      lambda.min
    ## [1,]   0.736928

``` r
# check predict class: if fitted prob in predict response is > 0.5 it should be class 1
predict(fit, newx = c(1,2), type = "class", s = "lambda.min")
```

    ##      lambda.min
    ## [1,] "1"

$$
\\log\\left\\{\\frac{P(Y_i=1)}{1-P(Y_i=1)}\\right\\} = \\beta_0 + \\beta_1 X\_{i1} + \\cdots + \\beta_pX\_{ip}, \\quad i = 1,2,\\cdots, n
$$

where *P*(*Y*<sub>*i*</sub>=1) is also denoted as *p*<sub>*i*</sub>.

$$
p_i = \\frac{1}{1 + \\exp{\\{-\\boldsymbol{X_i}\\boldsymbol{\\beta}\\}}}
$$

## Solution: (1) Get the Casebase Concordance

How can we get the risk score predictions from the casebase model? The
predict function in the casebase package is drawn from glmnet with
family binomial.

The problem with the prediction function is that it asks for a newx of
21 variables when I have 20 X’s.

``` r
# (4) pen_cb (ridge)
ridge_pen_cb <- fitSmoothHazard.fit(x = x_train, y = y_train,
                    family = "glmnet",
                    time = "d.time",
                    event = "death",
                    formula_time = ~ log(d.time), # how hazard depends on time
                    alpha = 0, # ridge
                    ratio = 10, # ratio of size of base series to case series
                    standardize = TRUE,
                    penalty.factor = c(0, rep(1, ncol(x_train))) #0 and then number of 1's is number of columns in x
                    )

# solution: add log(d.time) as a first column to x_test
x_test_cb <- cbind(log(y_test[,"d.time"]), x_test)

# for each subject, this gives the probability that y=1 (death = 1)
predict(ridge_pen_cb, newx = x_test_cb, type = "response", s = "lambda.min") %>% head()
```

    ##    lambda.min
    ## 1   0.6788556
    ## 2   0.9323192
    ## 22  0.6527386
    ## 25  0.6963031
    ## 26  0.2702730
    ## 32  0.6201952

Now, get the concordance for a casebase model given that we can get the
risk scores/predicted probs.

``` r
y_testing <- cbind(y_test[,2], y_test[,1])
colnames(y_testing) <- c("time", "status")

# ridge_pen_cb concordance
pred <- predict(ridge_pen_cb, newx = x_test_cb, type = "response", s = "lambda.min")
df <- as.data.frame(cbind(pred, y_testing))
colnames(df) <- c("pred", "time", "status")
ridge_cb_conc <- conc(df)
ridge_cb_conc
```

    ## [1] 0.9126661

Question: Should I use *P*(*Y*<sub>*i*</sub>=1) as the risk score to
compute concordance? Or should I use the linear predictor? I can get
both.

Notes:

## 2. Figure out the Score function

Why is it that the Score function (from riskRegression package) cannot
take in a glmnet object? (Need this to get the Brier Score)

How does the Score function compute the Brier Score mathematically?
(it’s different from my method)

``` r
# (2) pen_cox method (ridge)
ridge_pen_cox <- cv.glmnet(x = x_train, y = y_train_2, family = "cox", alpha = 0) # cv does cross validation, in this case it selects the lambda/model that maximizes the partial likelihood, because it is cox

# brier_ridge_pen_cox <- Score(list("Ridge Pen Cox" = ridge_pen_cox),
#       data = test_df,
#       formula = Hist(d.time, death != 0) ~ 1, summary = NULL,
#       se.fit = FALSE, metrics = "brier", contrasts = FALSE,
#       times = times)
```

time-dependent Brier Score somehow, not just a dichotomous outcome.

## 3. Getting the Brier Score for Penalized Cox Using My Method

How can we get prediction probabilities from a glmnet object with the
predict function?

``` r
# (2) pen_cox method (ridge)
ridge_pen_cox <- cv.glmnet(x = x_train, y = y_train_2, family = "cox", alpha = 0) # cv does cross validation, in this case it selects the lambda/model that maximizes the partial likelihood, because it is cox

pred <- predict(ridge_pen_cox, newx = x_test, s = "lambda.min", type = "response")
head(pred)
```

    ##             1
    ## 1   1.3072307
    ## 2  13.4014417
    ## 22  2.8842421
    ## 25  3.5357405
    ## 26  0.3844236
    ## 32  2.8510931

how to take the linear predictors to get the predicted probabilities.
you need the CIF, which you get from the baseline hazard\*e^(BX’s).

don’t pass the variable, but you pass the coefficients init = c(“coef
for every variable”) max_iter = …

package to get predicted probs from glmnet:
<https://www.rdocumentation.org/packages/c060/versions/0.2-9/topics/predictProb.coxnet>

## Solution: (3) Getting the Brier Score for Penalized Cox Using My Method

How can we get prediction probabilities from a glmnet object with the
predict function?

``` r
# (2) pen_cox method (ridge)
ridge_pen_cox <- cv.glmnet(x = x_train, y = y_train_2, family = "cox", alpha = 0) # cv does cross validation, if we set s = "lambda.min", we are able to select the lambda that maximizes the partial likelihood (because it is cox) for predictions or coefficients

y_test_cox <- y_test[,c(2,1)]
colnames(y_test_cox) <- c("time", "status")

# predictProb(ridge_pen_cox, response = y_test_cox, x = x_test)
# 
# pred <- predict(ridge_pen_cox, newx = x_test, s = "lambda.min", type = "response")
# head(pred)
```

``` r
# predictProb.coxnet <- predictProb.glmnet <- function (object, response, x, times, complexity,  ...) 
# {
#     #require(glmnet)    
#     lp       <- as.numeric(predict(object, newx=data.matrix(x),s=complexity, type="link"))
#     basesurv <- basesurv(object$response,object$linear.predictor, sort(unique(times)))
#     p        <- exp(exp(lp) %*% -t(basesurv$cumBaseHaz))
#     
#     if (NROW(p) != NROW(x) || NCOL(p) != length(times)) 
#         stop("Prediction failed")
#     p
# }
# 
# predictProb(ridge_pen_cox, response = y_test_cox, x = x_test)
# 
# predict(ridge_pen_cox, newx = x_test, s = "lambda.min", type = "link")
# 
# basesurv(ridge_pen_cox$y_test_cox, ridge_pen_cox$linear.predictor, sort(unique(times)))
```

The hazard for individual *i* at time *t* for a Cox model.

*h*<sub>*i*</sub>(*t*) = *h*<sub>0</sub>(*t*) \* *e**x**p*(*β*<sub>1</sub>*x*<sub>*i*1</sub>+*β*<sub>2</sub>*x*<sub>*i*2</sub>+...+*β*<sub>*k*</sub>*x*<sub>*i**k*</sub>)

Question: Mathematically, how do we get the survival probabilities given
a Cox model (with Betas)?

Notes: Clean everything and restart R, try predictProb.glmnet

Once you finish all the concordance and brier score. Finish the
simulation and write about it and illustrate. By mid-july you should
have that written out. When Gaby comes back, move to another simulation
setting. 2 weeks for simulation, then 2 weeks for writing, then that
will be mid-August and 2 weeks for cleaning and evaluating.
