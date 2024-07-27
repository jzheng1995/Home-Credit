# Introduction

Now that we have our data set up we can continue on analysis and model
training.

## Set up

### Libraries

``` r
library(tidyverse)
```

    ## Warning: package 'tidyr' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(here)
```

    ## here() starts at /Users/psyc/Documents/GitHub/home credit

``` r
library(tools)
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.1.0 ──
    ## ✔ broom        1.0.4     ✔ rsample      1.1.1
    ## ✔ dials        1.2.0     ✔ tune         1.1.1
    ## ✔ infer        1.0.7     ✔ workflows    1.1.3
    ## ✔ modeldata    1.1.0     ✔ workflowsets 1.0.1
    ## ✔ parsnip      1.1.0     ✔ yardstick    1.2.0
    ## ✔ recipes      1.0.6

    ## Warning: package 'infer' was built under R version 4.3.2

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()
    ## • Learn how to get started at https://www.tidymodels.org/start/

``` r
library(doParallel)
```

    ## Loading required package: foreach
    ## 
    ## Attaching package: 'foreach'
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when
    ## 
    ## Loading required package: iterators
    ## Loading required package: parallel

``` r
library(h2o)
```

    ## 
    ## ----------------------------------------------------------------------
    ## 
    ## Your next step is to start H2O:
    ##     > h2o.init()
    ## 
    ## For H2O package documentation, ask for help:
    ##     > ??h2o
    ## 
    ## After starting H2O, you can use the Web UI at http://localhost:54321
    ## For more information visit https://docs.h2o.ai
    ## 
    ## ----------------------------------------------------------------------
    ## 
    ## 
    ## Attaching package: 'h2o'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     day, hour, month, week, year
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     cor, sd, var
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     &&, %*%, %in%, ||, apply, as.factor, as.numeric, colnames,
    ##     colnames<-, ifelse, is.character, is.factor, is.numeric, log,
    ##     log10, log1p, log2, round, signif, trunc

### Import data

``` r
# quick rds save/load
rdsread <- function(name, envr = globalenv()){
  assign(x = name, read_rds(str_c(name,".rds")),envir = envr)
}
quickrds <- function(x){
  saveRDS(get(x),str_c(x, ".rds"))
}
```

``` r
rdsread("train_joined_tbl")
rdsread("test_joined_tbl")
```

### Train test split

We’ll split our data into train and test sets.

``` r
library(rsample)

variables <- c('year.n','month.n','week.n','education_927M','age','empl_industry_691L', 'familystate_447L', 'incometype_1044T','mainoccupationinc_384A','target')
predictors <- variables[variables != "target"]

home_train <- train_joined_tbl %>% select(all_of(variables))
home_test <- test_joined_tbl %>% select(all_of(predictors ))
```

I’ll use a random forest model as a pretty good out-of-box performer. We
have several hyperparameters to optimize, however. They include number
of predictor columns sampled (mtry), minimum observations per leaf
(min_row), maximum tree depth (max_depth), and percent of observations
sampled (sample_rate). The h2o package will use a random search of a
combination of our hyperparameter grid and will stop if the RMSE does
not improve by a specified amount.

``` r
library(h2o)
# convert training data to h2o object
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# set the response column 
train_h2o <- as.h2o(home_train)
response <- "target"

# number of features
n_features <- length(setdiff(names(home_train), "target"))

# set the predictor names
predictors <- setdiff(colnames(home_train), response)

h2o_rf1 <- h2o.randomForest(
    x = predictors, 
    y = response,
    training_frame = train_h2o, 
    ntrees = n_features * 10,
    seed = 123
)
saveRDS(h2o_rf1,"h2o_rf1.rds")
# hyperparameter grid
hyper_grid <- list(
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

# random grid search strategy
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*5      # or stop search after 5 min.
)
h2o.shutdown(prompt = FALSE)
# h2o setup
h2o.no_progress()
h2o.init(max_mem_size = "5g")
train_h2o <- as.h2o(home_train)

 # perform grid search
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors, 
  y = response, 
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = n_features * 10,
  seed = 123,
  stopping_metric = "RMSE",   
  stopping_rounds = 10,           # stop if last 10 trees added 
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)

# Assess hyperparameter configurations
summary(random_grid, show_stack_traces = TRUE)

random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
# Save computationally expensive objects to load later
saveRDS(random_grid,"random_grid.rds")
saveRDS(random_grid_perf,"random_grid_perf.rds")
```

We can examine the models that were ran and see exactly what
hyperparameter combinations were examined. The output shows us that the
final model had a mse of 0.03, a 0.0001 improvement over the last
iteration. We can also see that there was a failed iteration.

``` r
random_grid_perf %>% summary() 
```

    ## H2O Grid Details
    ## ================
    ## 
    ## Grid ID: rf_random_grid 
    ## Used hyper parameters: 
    ##   -  max_depth 
    ##   -  min_rows 
    ##   -  mtries 
    ##   -  sample_rate 
    ## Number of models: 5 
    ## Number of failed models: 1 
    ## 
    ## Hyper-Parameter Search Summary: ordered by increasing mse
    ##   max_depth min_rows  mtries sample_rate              model_ids     mse
    ## 1  30.00000 10.00000 1.00000     0.55000 rf_random_grid_model_3 0.03022
    ## 2  20.00000  3.00000 1.00000     0.80000 rf_random_grid_model_1 0.03023
    ## 3  30.00000 10.00000 2.00000     0.63200 rf_random_grid_model_5 0.03032
    ## 4  20.00000  5.00000 2.00000     0.80000 rf_random_grid_model_4 0.03033
    ## 5  20.00000 10.00000 3.00000     0.63200 rf_random_grid_model_6 0.03040
    ## Failed models
    ## -------------
    ##  max_depth min_rows mtries sample_rate status_failed
    ##         30      5.0      0         0.8          FAIL
    ##                                                                                                                                                                                                                                                                              msgs_failed
    ##  "Illegal argument(s) for DRF model: rf_random_grid_model_2.  Details: ERRR on field: _mtries: mtries must be -1 (converted to sqrt(features)) or -2 (All features) or >= 1 but it is 0\nERRR on field: _mtries: Computed mtries should be -1 or -2 or in interval [1,10[ but it is 0\n"
    ## H2O Grid Summary
    ## ================
    ## 
    ## Grid ID: rf_random_grid 
    ## Used hyper parameters: 
    ##   -  max_depth 
    ##   -  min_rows 
    ##   -  mtries 
    ##   -  sample_rate 
    ## Number of models: 5 
    ##   -  rf_random_grid_model_3 
    ##   -  rf_random_grid_model_1 
    ##   -  rf_random_grid_model_5 
    ##   -  rf_random_grid_model_4 
    ##   -  rf_random_grid_model_6 
    ## 
    ## Number of failed models: 1 
    ##   -  Illegal argument(s) for DRF model: rf_random_grid_model_2.  Details: ERRR on field: _mtries: mtries must be -1 (converted to sqrt(features)) or -2 (All features) or >= 1 but it is 0
    ## ERRR on field: _mtries: Computed mtries should be -1 or -2 or in interval [1,10[ but it is 0
    ## 
    ## Note: To see exception stack traces please pass parameter `show_stack_traces = T` to this function.

## Model validation

### Preprocessing setup

Set up tidymodels framework to preprocess and use train data
information.

``` r
library(recipes)
library(workflows)

home_recipe <- recipe(target ~ ., data = home_train) 
# recipe
home_blueprint <- home_recipe  %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_impute_knn(-all_outcomes())


home_prep <- prep(home_blueprint, training = home_train)

# near zero variance
caret::nearZeroVar(home_train, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
```

    ##   rowname freqRatio percentUnique zeroVar  nzv
    ## 1  target  30.80937   0.000131005   FALSE TRUE

### Set up random forest workflow

``` r
# cross validation folds
cv_folds <-
 vfold_cv(home_train, 
          v = 5, 
          strata = "target",
          set.seed = 123) 

# random forest specification
rf_spec <- 
  rand_forest(
    mtry = 2,
    min_n = 10
  ) %>% 
  set_engine("ranger", importance = "impurity", num.threads = 4) %>% 
  set_mode("classification")

# workflow
rf_wflow_tuned <-
 workflow() %>%
 add_recipe(home_blueprint) %>% 
 add_model(rf_spec) 
```

### Cross-validated Randomforest

Test model with informed hyperparameters using cross-validation.

``` r
# Use multiple cores
all_cores <- parallel::detectCores(logical = FALSE)
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# Run cross-validated random forest
rf_res <-
  rf_wflow_tuned %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas,
      accuracy,roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE, allow_par = TRUE, parallel_over = "everything",verbose = TRUE)
    )

# Assess overall performance
 rf_res %>%  collect_metrics(summarize = TRUE)
 
# Observe individual model performance
compare_cv_rf <- rf_res %>%  collect_metrics(summarize = FALSE) 

saveRDS(rf_res, "rf_res.rds")
saveRDS(compare_cv_rf, "compare_cv_rf.rds")
```

``` r
rdsread("rf_res")
```

``` r
rf_res %>%  collect_metrics(summarize = TRUE)
```

    ## # A tibble: 7 × 6
    ##   .metric   .estimator  mean     n   std_err .config             
    ##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
    ## 1 accuracy  binary     0.969     5 0.000108  Preprocessor1_Model1
    ## 2 f_meas    binary     0.984     5 0.0000560 Preprocessor1_Model1
    ## 3 precision binary     0.969     5 0.000108  Preprocessor1_Model1
    ## 4 recall    binary     1         5 0         Preprocessor1_Model1
    ## 5 roc_auc   binary     0.632     5 0.00146   Preprocessor1_Model1
    ## 6 sens      binary     1         5 0         Preprocessor1_Model1
    ## 7 spec      binary     0         5 0         Preprocessor1_Model1

``` r
rf_res %>%  collect_metrics(summarize = FALSE) 
```

    ## # A tibble: 35 × 5
    ##    id    .metric   .estimator .estimate .config             
    ##    <chr> <chr>     <chr>          <dbl> <chr>               
    ##  1 Fold1 recall    binary         1     Preprocessor1_Model1
    ##  2 Fold1 precision binary         0.969 Preprocessor1_Model1
    ##  3 Fold1 f_meas    binary         0.984 Preprocessor1_Model1
    ##  4 Fold1 accuracy  binary         0.969 Preprocessor1_Model1
    ##  5 Fold1 sens      binary         1     Preprocessor1_Model1
    ##  6 Fold1 spec      binary         0     Preprocessor1_Model1
    ##  7 Fold1 roc_auc   binary         0.628 Preprocessor1_Model1
    ##  8 Fold2 recall    binary         1     Preprocessor1_Model1
    ##  9 Fold2 precision binary         0.969 Preprocessor1_Model1
    ## 10 Fold2 f_meas    binary         0.984 Preprocessor1_Model1
    ## # ℹ 25 more rows

## Prediction

Run informed model on test data and obtain predicted probabilities for
`target` class.

``` r
# preprocess recipe 
preprocess_home <- recipe(target ~ ., data = home_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(-all_outcomes())


# fit train data with tuned workflow
home_rf_fit <- rf_wflow_tuned %>% 
  fit(home_train)
# extract recipe
wf_extracted <- home_rf_fit %>% extract_recipe()
# extract model
home_final_tuned <- home_rf_fit %>% extract_fit_parsnip()

# preprocess test data
home_test_processed <- wf_extracted %>% bake(new_data = home_test)

# predict test data using extracted model
pred_home <- predict(home_final_tuned, new_data = home_test_processed)

# predicted class probability 
pred_probs <- predict(home_final_tuned, new_data = home_test_processed, type = "prob")
# compare class prediction and prediction probability
df_new_pre_processed <- cbind(pred_home, pred_probs)
df_new_pre_processed

saveRDS(df_new_pre_processed, "df_new_pre_processed.rds")
```

``` r
df_new_pre_processed
```

    ##    .pred_class   .pred_0    .pred_1
    ## 1            0 0.9534976 0.04650240
    ## 2            0 0.9418981 0.05810194
    ## 3            0 0.9582279 0.04177215
    ## 4            0 0.9802462 0.01975378
    ## 5            0 0.9802462 0.01975378
    ## 6            0 0.9802462 0.01975378
    ## 7            0 0.9802462 0.01975378
    ## 8            0 0.9802462 0.01975378
    ## 9            0 0.9802462 0.01975378
    ## 10           0 0.9802462 0.01975378
