Casebase June 16th (3 Questions)
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

## 3. Getting the Brier Score for Penalized Cox Using My Method

How can we get prediction probabilities from a glmnet object with the
predict function?

``` r
# (2) pen_cox method (ridge)
ridge_pen_cox <- cv.glmnet(x = x_train, y = y_train_2, family = "cox", alpha = 0) # cv does cross validation, in this case it selects the lambda/model that maximizes the partial likelihood, because it is cox

pred <- predict(ridge_pen_cox, newx = x_test, s = "lambda.min", type = "link")
head(pred)
```

    ##             1
    ## 1   0.2679109
    ## 2   2.5953623
    ## 22  1.0592621
    ## 25  1.2629227
    ## 26 -0.9560102
    ## 32  1.0477025
