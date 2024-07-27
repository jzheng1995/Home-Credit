## Introduction

This is primarily an ETL document for the 2024 home credit competition
data on Kaggle. This will also be a first attempt at 1) a kaggle
competition, 2) approaching this dataset, and 3) documenting the
bottom-up process for public review. This will primarily be a reference
for myself on how to improve on all these aspects. The goal of this
competition is to predict `target` class from all the provided features.

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
library(DBI)
library(RSQLite)
library(reticulate)
library(tools)
library(data.tree)
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 4.3.2

``` r
con <- dbConnect(SQLite(), "mydb")
```

``` r
# quick rds save/load
rdsread <- function(name, envr = globalenv()){
  assign(x = name, read_rds(str_c(name,".rds")),envir = envr)
}
quickrds <- function(x){
  saveRDS(get(x),str_c(x, ".rds"))
}
```

### File import

When I first approached these data, I attempted to read the all the csv
files into my R environment through `read_csv()`. As it turns out, the
data contained millions of rows for multiple files and were clearly too
big to import for my laptop memory. My roundabout strategy in
circumventing this problem was creating a local relational database
(SQLite), trim the data down on the database side, and then import a
data table that’s hopefully small enough to work with.

To do all this, I planned on reading each csv data file into a local
database. I’m not a fan of manually writing all my file names so first I
set up all the folder paths to read into a local SQLite database.

``` r
# folder paths
train_folder_path <- here("home-credit-credit-risk-model-stability","csv_files","train") %>% str_c(.,"/") 
test_folder_path <- here("home-credit-credit-risk-model-stability","csv_files","test") %>% str_c(.,"/") 

# file paths
train_names <- here("home-credit-credit-risk-model-stability","csv_files","train") %>% list.files()
test_names <- here("home-credit-credit-risk-model-stability","csv_files","test") %>% list.files()

# test/train full file paths
train_paths <- str_c(train_folder_path, train_names)
test_paths <- str_c(test_folder_path, test_names)

# Names for each data table
test_names_noext <- test_names %>% file_path_sans_ext()
train_names_noext <- train_names %>% file_path_sans_ext()
```

I then iterated the creation of a database table for each csv by running
a `dbWriteTable()` for each filepath.

``` r
# Import all csv files into one SQL database

# for each csv file in train folder, read into mydb
for (i in seq(length(train_paths))){
  dbWriteTable(con, train_names_noext[i], train_paths[i], overwrite = TRUE)
}

# for each csv file in test folder, read into mydb
for (i in seq(length(test_paths))){
  dbWriteTable(con, test_names_noext[i], test_paths[i], overwrite = TRUE)
}
```

## Overview

### SQL overview

Since the data is relatively unknown at this point, here are some useful
SQL/r functions to extract some useful overviews. We’ll perform these on
the `train` subset of csv files.

-   `dbListFields`: DBI function to examine column names
-   `"PRAGMA table_info"`: SQL statement to examine column data type

``` r
# table fields
train_fields <- tibble(table_name = train_names_noext) %>% mutate(
  fields = map(table_name, ~dbListFields(con, .)),
  info = map(table_name, ~dbGetQuery(con, str_c("PRAGMA table_info(",.,")")))
)
# show
train_fields %>% select(info) %>% unnest(info) %>% select(name, type) %>%  head()
```

    ## # A tibble: 6 × 2
    ##   name                     type   
    ##   <chr>                    <chr>  
    ## 1 case_id                  INTEGER
    ## 2 actualdpd_943P           REAL   
    ## 3 annuity_853A             REAL   
    ## 4 approvaldate_319D        TEXT   
    ## 5 byoccupationinc_3656910L REAL   
    ## 6 cancelreason_3545846M    TEXT

-   `dbListTables`: DBI function to examine existing table names

``` r
# table names
datatables <- dbListTables(con)
# original table names
original_table_names <- tibble(table = datatables) %>%  
  # filter out future added table names
  filter(str_detect(table, "test|train"),
  str_detect(table, "id|ID|_U|joined", negate = TRUE)) 
# show
original_table_names %>% head() 
```

    ## # A tibble: 6 × 1
    ##   table                   
    ##   <chr>                   
    ## 1 test_applprev_1_0       
    ## 2 test_applprev_1_1       
    ## 3 test_applprev_1_2       
    ## 4 test_applprev_2         
    ## 5 test_base               
    ## 6 test_credit_bureau_a_1_0

There are 1139 columns across 70 tables. Some recurring columns are
`case_id`, `num_group1`, and `num_group2`. `case_id` is clearly an
unique row identifier for each person. Based on the Kaggle data
description, `num_group1` and `num_group2` are meant to index `case_id`
when there are multiple row entries per `case_id` (e.g. historical loan
contracts).

### File naming patterns

We can also examine the structure of the files with a `data.tree` below.

``` r
# set up nodes
homecredit<- Node$new("home-credit-credit-risk-model-stability")
csvfiles <- homecredit$AddChild("csv_files")
train <- csvfiles$AddChild("train")
test <- csvfiles$AddChild("test")

# add train nodes
for (i in (original_table_names %>% filter(str_detect(table, "train")) %>% pull(table))){
  train$AddChild(i)}
# add test nodes
for (i in (original_table_names %>% filter(str_detect(table, "test")) %>% pull(table))){
  test$AddChild(i)}
# show
homecredit %>% 
  as.data.frame() %>% 
  as.matrix() %>% 
  print(quote=FALSE)
```

    ##       levelName                              
    ##  [1,] home-credit-credit-risk-model-stability
    ##  [2,]  °--csv_files                          
    ##  [3,]      ¦--train                          
    ##  [4,]      ¦   ¦--train_applprev_1_0         
    ##  [5,]      ¦   ¦--train_applprev_1_1         
    ##  [6,]      ¦   ¦--train_applprev_2           
    ##  [7,]      ¦   ¦--train_base                 
    ##  [8,]      ¦   ¦--train_credit_bureau_a_1_0  
    ##  [9,]      ¦   ¦--train_credit_bureau_a_1_1  
    ## [10,]      ¦   ¦--train_credit_bureau_a_1_2  
    ## [11,]      ¦   ¦--train_credit_bureau_a_1_3  
    ## [12,]      ¦   ¦--train_credit_bureau_a_2_0  
    ## [13,]      ¦   ¦--train_credit_bureau_a_2_1  
    ## [14,]      ¦   ¦--train_credit_bureau_a_2_10 
    ## [15,]      ¦   ¦--train_credit_bureau_a_2_2  
    ## [16,]      ¦   ¦--train_credit_bureau_a_2_3  
    ## [17,]      ¦   ¦--train_credit_bureau_a_2_4  
    ## [18,]      ¦   ¦--train_credit_bureau_a_2_5  
    ## [19,]      ¦   ¦--train_credit_bureau_a_2_6  
    ## [20,]      ¦   ¦--train_credit_bureau_a_2_7  
    ## [21,]      ¦   ¦--train_credit_bureau_a_2_8  
    ## [22,]      ¦   ¦--train_credit_bureau_a_2_9  
    ## [23,]      ¦   ¦--train_credit_bureau_b_1    
    ## [24,]      ¦   ¦--train_credit_bureau_b_2    
    ## [25,]      ¦   ¦--train_debitcard_1          
    ## [26,]      ¦   ¦--train_deposit_1            
    ## [27,]      ¦   ¦--train_features             
    ## [28,]      ¦   ¦--train_other_1              
    ## [29,]      ¦   ¦--train_person_1             
    ## [30,]      ¦   ¦--train_person_2             
    ## [31,]      ¦   ¦--train_static_0_0           
    ## [32,]      ¦   ¦--train_static_0_1           
    ## [33,]      ¦   ¦--train_static_cb_0          
    ## [34,]      ¦   ¦--train_tax_registry_a_1     
    ## [35,]      ¦   ¦--train_tax_registry_b_1     
    ## [36,]      ¦   °--train_tax_registry_c_1     
    ## [37,]      °--test                           
    ## [38,]          ¦--test_applprev_1_0          
    ## [39,]          ¦--test_applprev_1_1          
    ## [40,]          ¦--test_applprev_1_2          
    ## [41,]          ¦--test_applprev_2            
    ## [42,]          ¦--test_base                  
    ## [43,]          ¦--test_credit_bureau_a_1_0   
    ## [44,]          ¦--test_credit_bureau_a_1_1   
    ## [45,]          ¦--test_credit_bureau_a_1_2   
    ## [46,]          ¦--test_credit_bureau_a_1_3   
    ## [47,]          ¦--test_credit_bureau_a_1_4   
    ## [48,]          ¦--test_credit_bureau_a_2_0   
    ## [49,]          ¦--test_credit_bureau_a_2_1   
    ## [50,]          ¦--test_credit_bureau_a_2_10  
    ## [51,]          ¦--test_credit_bureau_a_2_11  
    ## [52,]          ¦--test_credit_bureau_a_2_2   
    ## [53,]          ¦--test_credit_bureau_a_2_3   
    ## [54,]          ¦--test_credit_bureau_a_2_4   
    ## [55,]          ¦--test_credit_bureau_a_2_5   
    ## [56,]          ¦--test_credit_bureau_a_2_6   
    ## [57,]          ¦--test_credit_bureau_a_2_7   
    ## [58,]          ¦--test_credit_bureau_a_2_8   
    ## [59,]          ¦--test_credit_bureau_a_2_9   
    ## [60,]          ¦--test_credit_bureau_b_1     
    ## [61,]          ¦--test_credit_bureau_b_2     
    ## [62,]          ¦--test_debitcard_1           
    ## [63,]          ¦--test_deposit_1             
    ## [64,]          ¦--test_features              
    ## [65,]          ¦--test_other_1               
    ## [66,]          ¦--test_person_1              
    ## [67,]          ¦--test_person_2              
    ## [68,]          ¦--test_static_0_0            
    ## [69,]          ¦--test_static_0_1            
    ## [70,]          ¦--test_static_0_2            
    ## [71,]          ¦--test_static_cb_0           
    ## [72,]          ¦--test_tax_registry_a_1      
    ## [73,]          ¦--test_tax_registry_b_1      
    ## [74,]          °--test_tax_registry_c_1

We see that all the original csv files are organized by a prefix `test_`
or `train_`. These can be further related by numeral suffixes that
denote either 1) related tables split by columns (`train_person_1`,
`train_person_2`) or 2) related tables split by rows
(`train_credit_bureau_a_2_1`,`train_credit_bureau_a_2_2`,…,`train_credit_bureau_a_2_8`).

In addition, a `feature_definitions.csv` file was provided that mapped
column names to feature definitions. For future reference, we create a
tibble to map feature definitions to column names to table names.

``` r
# import feature definitions
features <- here("home-credit-credit-risk-model-stability","feature_definitions.csv") %>% read_csv(show_col_types = FALSE)

# table fields mapped to feature description
train_features <- train_fields %>% unnest(everything()) %>% left_join(features, by = c("fields" = "Variable")) %>% select(fields, everything())
train_features %>% select(fields, table_name,Description ) %>% head()
```

``` watch-out
## # A tibble: 6 × 3
##   fields                   table_name         Description                       
##   <chr>                    <chr>              <chr>                             
## 1 case_id                  train_applprev_1_0 <NA>                              
## 2 actualdpd_943P           train_applprev_1_0 Days Past Due (DPD) of previous c…
## 3 annuity_853A             train_applprev_1_0 Monthly annuity for previous appl…
## 4 approvaldate_319D        train_applprev_1_0 Approval Date of Previous Applica…
## 5 byoccupationinc_3656910L train_applprev_1_0 Applicant's income from previous …
## 6 cancelreason_3545846M    train_applprev_1_0 Application cancellation reason.
```

### Base_train

The goal of this competition is to predict `target`, which can be found
in `train_base`. The definition of `target`, found on a [forum
Q&A](https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability/discussion/477074),
is defined as:

    ...unpaid payment (one is enough) in certain time period. There is also some time tolerance (e.g. one day late is not default) and amount tolerance (if client paid $100 instead of $100.10) ). 

Here’s some information on `train_base`.

-   **Column names, type**

    ``` r
    # Base training dataset - summary
    # table columns
    base_columns <- dbGetQuery(con, "PRAGMA table_info(train_base)")
    base_columns %>% head() 
    ```

        ##   cid          name    type notnull dflt_value pk
        ## 1   0       case_id INTEGER       0         NA  0
        ## 2   1 date_decision    TEXT       0         NA  0
        ## 3   2         MONTH INTEGER       0         NA  0
        ## 4   3      WEEK_NUM INTEGER       0         NA  0
        ## 5   4        target INTEGER       0         NA  0

-   **Number of rows**

    ``` r
    # table rows
    base_rows <- dbGetQuery(con, "SELECT COUNT(*) FROM train_base")
    base_rows %>% head() 
    ```

        ##   COUNT(*)
        ## 1  1526659

-   **Number of rows per participant**

    ``` r
    # rows per participant
    base_rows_per_id <- dbGetQuery(con, "SELECT case_id, COUNT(*) FROM train_base GROUP BY case_id")
    base_rows_per_id %>% head()
    ```

        ##   case_id COUNT(*)
        ## 1       0        1
        ## 2       1        1
        ## 3       2        1
        ## 4       3        1
        ## 5       4        1
        ## 6       5        1

    ``` r
    # sum of rows
    row_per_users <- dbGetQuery(con, "SELECT row_count, count(*) as user_count
    FROM 
    (SELECT count(case_id) AS row_count 
    FROM train_base
    GROUP BY case_id)
    GROUP BY row_count")

    row_per_users %>% head() 
    ```

        ##   row_count user_count
        ## 1         1    1526659

Now we have an idea of the main table we’ll be working with. Since the
prediction `target` is only available for the `case_id` present in
`train_base`, the first step for reducing the data load is to filter for
the same `case_id` in all the other supplemental tables. Before we do
that, however, I want to simplify the number of tables I have by joining
tables split by rows. This will reduce the number of table `LEFT JOIN`s
I will have to perform later on.

### Joining split tables

The split tables we’re looking for share the same `columns` but
different `rows`. We want to 1) identify tables sharing the same columns
, 2) identify the shared naming scheme that tbese split tables use, and
3) join the split table rows by the common naming scheme. Here are the
steps to set this up:

-   Create a tibble with original table names mapped to all column names

``` r
# renew datatable
datatables <- dbListTables(con)
# Filter original tables
original_fields <- tibble(table = datatables) %>% filter(
  str_detect(table, "test|train"),
  str_detect(table, "id|ID|_U|joined", negate = TRUE)
) %>% mutate(
  fields = map(table, ~dbListFields(con, .)),
  info = map(table, ~dbGetQuery(con, str_c("PRAGMA table_info(",.,")")))
) %>% select(table, fields) %>% unnest(fields)
```

-   First extract common naming scheme using regex. We then map the
    naming scheme to original table name and its respective number of
    columns. We expect that for any set tables sharing a common scheme,
    the set should share the same number of columns.

``` r
# map extracted scheme to original table name, # of columns
split_datasets_revised <- original_fields %>% select(table) %>% unique() %>% 
  mutate(table_name_extract =str_extract(table,"[A-Za-z_]*_\\d(?=_\\d)" )) %>% 
  select(table, table_name_extract) %>% mutate(
  ncol = map(table, ~dbGetQuery(con, str_c("PRAGMA table_info(",.,")")) %>% nrow()) ) %>% unnest(ncol)
split_datasets_revised %>% head()
```

    ## # A tibble: 6 × 3
    ##   table                    table_name_extract      ncol
    ##   <chr>                    <chr>                  <int>
    ## 1 test_applprev_1_0        test_applprev_1           41
    ## 2 test_applprev_1_1        test_applprev_1           41
    ## 3 test_applprev_1_2        test_applprev_1           41
    ## 4 test_applprev_2          <NA>                       6
    ## 5 test_base                <NA>                       4
    ## 6 test_credit_bureau_a_1_0 test_credit_bureau_a_1    79

-   Set up the join query for split tables. Since these are vertical
    rowbinding, we use `UNION` statements to create new joined tables
    suffxied with `_U`.

``` r
# joined table set names
split_dataset_names <- split_datasets_revised$table_name_extract %>% unique() %>% discard(is.na)
# create UNION/create table query for every split tables
union_query_revised <- 
  # extract unique shared table names
  split_datasets_revised %>% select(table_name_extract) %>% na.omit() %>% unique() %>% 
  # join to identify split tables
  left_join(split_datasets_revised,by = join_by(table_name_extract)) %>%
  # add # of split tables per group
  group_by(table_name_extract) %>% add_tally() %>% 
  # iterate SELECT statements for SQL query
  mutate(queries = str_c("SELECT * FROM ", table)) %>% 
  # create single query for each table
  select(table_name_extract, queries) %>% group_by(table_name_extract) %>% nest(nested_query = queries) %>% 
  # create combined table
  mutate(
         combined_query = nested_query[[1]] %>% pull(queries) %>% paste0(., collapse = " "),
         union_query = str_replace_all(combined_query,"(\\d )(?=SELECT)","\\1UNION " ),
         write_query = str_c("CREATE TABLE IF NOT EXISTS ",table_name_extract,"_U"," AS ", union_query))

# examine the queries
union_query_revised %>% select(table_name_extract,write_query) %>% head() 
```

    ## # A tibble: 6 × 2
    ## # Groups:   table_name_extract [6]
    ##   table_name_extract      write_query                                           
    ##   <chr>                   <chr>                                                 
    ## 1 test_applprev_1         CREATE TABLE IF NOT EXISTS test_applprev_1_U AS SELEC…
    ## 2 test_credit_bureau_a_1  CREATE TABLE IF NOT EXISTS test_credit_bureau_a_1_U A…
    ## 3 test_credit_bureau_a_2  CREATE TABLE IF NOT EXISTS test_credit_bureau_a_2_U A…
    ## 4 test_static_0           CREATE TABLE IF NOT EXISTS test_static_0_U AS SELECT …
    ## 5 train_applprev_1        CREATE TABLE IF NOT EXISTS train_applprev_1_U AS SELE…
    ## 6 train_credit_bureau_a_1 CREATE TABLE IF NOT EXISTS train_credit_bureau_a_1_U …

``` r
quickrds("split_dataset_names")
```

``` r
# SQL - union 
# execute each query
union_query_revised$write_query %>% walk(~dbExecute(con,.))
```

## Subset supplemental data

Since the main focus of the operation is predicting `target` from
`train_base`, we can reduce our operations to just data that’s available
for `case_id`s that are present in `train_base`. This can be done by
using `INNER JOIN` and filtering out data from all supplemental sources
with `train_base$case_id`.

-   First subset `case_id` column from base tables

``` r
# create IDs to filter out relevant IDs in data tables
dbExecute(con,"CREATE TABLE IF NOT EXISTS train_id AS SELECT case_id FROM train_base")
dbExecute(con,"CREATE TABLE IF NOT EXISTS test_id AS SELECT case_id FROM test_base")
```

-   Some of supplemental tables have multiple rows per participant. The
    key for the main participant data is the value 0 for numgroup_1 and
    numgroup_2. Here we set up a numgroup tibble for future mapping. (1
    = 0 mapping, 0 = other number)

``` r
# numgroup index
numgroup_tibble <- original_fields %>% 
  filter(str_detect(fields, "num_group")) %>% 
  pivot_wider(names_from = fields, values_from = fields, values_fn = ~1, values_fill = 0) 

numgroup_tibble %>% head()
```

    ## # A tibble: 6 × 3
    ##   table                    num_group1 num_group2
    ##   <chr>                         <dbl>      <dbl>
    ## 1 test_applprev_1_0                 1          0
    ## 2 test_applprev_1_1                 1          0
    ## 3 test_applprev_1_2                 1          0
    ## 4 test_applprev_2                   1          1
    ## 5 test_credit_bureau_a_1_0          1          0
    ## 6 test_credit_bureau_a_1_1          1          0

-   Create query to `INNER JOIN` and filter tables by
    `train_base$case_id`, `numgroup` and save filtered tables with a
    `_id` suffix.

    ``` r
      # renew data table
    datatables <- dbListTables(con)

      # filter data by id
    id_join_query_revised <- tibble(tables = datatables) %>% 
      # filter for original tables,joined tables 
      filter(
    # filter out split table names
    str_detect(tables,str_c(str_c(split_dataset_names,"_\\d"), collapse = "|"),
               negate = TRUE))%>% 
      filter(str_detect(tables, "id|ID|joined|base", negate = TRUE)) %>% 
      filter(str_detect(tables, "train|test")) %>% 
      # create query
      mutate(
      query = case_when(
    str_detect(tables, "train") ~ str_c("CREATE TABLE IF NOT EXISTS ",tables, "_id"," AS SELECT * FROM ",tables,
                " INNER JOIN train_id ON ", tables),
    str_detect(tables, "test") ~ str_c("CREATE TABLE IF NOT EXISTS ",tables, "_id"," AS SELECT * FROM ",tables,
                " INNER JOIN test_id ON ", tables)
      ),
      query = case_when(
    str_detect(tables, "train") ~ str_c(query, ".case_id = train_id.case_id"),
    str_detect(tables, "test") ~ str_c(query, ".case_id = test_id.case_id")
      )
    )
      # filter rows by num_group
    id_join_query_num <- id_join_query_revised %>% left_join(numgroup_tibble , by = c("tables" = "table")) %>% mutate(
      query = case_when(
    num_group1 == 1 & num_group2 == 0 ~ str_c(query, " AND num_group1 = 0"),
    num_group1 == 1 & num_group2 == 1  ~ str_c(query, " AND num_group1 = 0 AND num_group2 = 0"),
    .default = query
      )
    )
      # show queries
    id_join_query_num$query %>% head()
    ```

        ## [1] "CREATE TABLE IF NOT EXISTS test_applprev_1_U_id AS SELECT * FROM test_applprev_1_U INNER JOIN test_id ON test_applprev_1_U.case_id = test_id.case_id"                                                     
        ## [2] "CREATE TABLE IF NOT EXISTS test_applprev_2_id AS SELECT * FROM test_applprev_2 INNER JOIN test_id ON test_applprev_2.case_id = test_id.case_id AND num_group1 = 0 AND num_group2 = 0"                     
        ## [3] "CREATE TABLE IF NOT EXISTS test_credit_bureau_a_1_U_id AS SELECT * FROM test_credit_bureau_a_1_U INNER JOIN test_id ON test_credit_bureau_a_1_U.case_id = test_id.case_id"                                
        ## [4] "CREATE TABLE IF NOT EXISTS test_credit_bureau_a_2_U_id AS SELECT * FROM test_credit_bureau_a_2_U INNER JOIN test_id ON test_credit_bureau_a_2_U.case_id = test_id.case_id"                                
        ## [5] "CREATE TABLE IF NOT EXISTS test_credit_bureau_b_1_id AS SELECT * FROM test_credit_bureau_b_1 INNER JOIN test_id ON test_credit_bureau_b_1.case_id = test_id.case_id AND num_group1 = 0"                   
        ## [6] "CREATE TABLE IF NOT EXISTS test_credit_bureau_b_2_id AS SELECT * FROM test_credit_bureau_b_2 INNER JOIN test_id ON test_credit_bureau_b_2.case_id = test_id.case_id AND num_group1 = 0 AND num_group2 = 0"

``` r
# execute
id_join_query_num$query %>% walk(~dbExecute(con,.))
```

## Joining features

### Feature subset

To facilitate a quick analysis we’re going to pick out some variables
that I think are theoretically relevant in predicting `target`:

1.  `birth_259D`: Birth date
2.  `education_927M`: Education
3.  `empl_employedfrom_271D`: Employment date
4.  `empl_employedtotal_800L`: Employment length
5.  `empl_industry_691L`: Job industry
6.  `familystate_447L`: Family status
7.  `incometype_1044TL`: Salary type
8.  `mainoccupationinc_384A`: Income amount
9.  `riskassesment_940T`: Normalized risk - assessed by credit bureau

### Join SQL

-   We can use a tibble to map the selected features to their respective
    tables.

    ``` r
    # highlight features of interest and identify within datatables

    # pick features of interest and rename
    revised_feature_list <- list(
      base = c("case_id"),
      person_1_id = c("birth_259D", #birthdate
               "education_927M", # education
               "empl_employedfrom_271D", # employ date
               "empl_employedtotal_800L", # employ length
               "empl_industry_691L", # job industry
               "familystate_447L", # relationship
               "incometype_1044T", # salary type
               "mainoccupationinc_384A"# income amount
               ), 
      static_cb_0_id = c("riskassesment_940T" #risk assessment - normalized
                           )) 
    # link features of interest to data tables
    revised_features_tbl <- 
      # set up variables as tibble
      tibble(table = names(revised_feature_list),
         train_table = str_c("train_", table),
         test_table = str_c("test_", table),
       features = revised_feature_list ) %>% unnest(features)
    ```

-   We’ll then set up the query parts using the mapped tibble.

    ``` r
    # create query for joining relevant features to train_base
    train_join <- revised_features_tbl %>% filter(str_detect(features, "case_id", negate = TRUE)) %>% na.omit() %>% 
      # table.column query
      mutate(
      table_column = str_c(train_table,".", features)) %>% 
      select(train_table, table_column) %>% 
      mutate(
    join_string = str_c("LEFT JOIN ", train_table, " ON "),
    id_string = str_c("train_base.case_id = ",train_table, ".case_id"),
    join_id_string = str_c(join_string, id_string)
      ) %>% unique()

    # create query for joining relevant features to train_test
    test_join <- revised_features_tbl %>% filter(str_detect(features, "case_id", negate = TRUE)) %>% na.omit() %>% 
      # table.column query
      mutate(
      table_column = str_c(test_table,".", features)) %>% 
      select(test_table, table_column) %>% 
      mutate(
    join_string = str_c("LEFT JOIN ", test_table, " ON "),
    id_string = str_c("test_base.case_id = ",test_table, ".case_id"),
    join_id_string = str_c(join_string, id_string)
      ) %>% unique()
    ```

    -   `train_base` set up

    ``` r
      train_join %>% select(table_column, join_id_string) %>% head()
    ```

        ## # A tibble: 6 × 2
        ##   table_column                              join_id_string                      
        ##   <chr>                                     <chr>                               
        ## 1 train_person_1_id.birth_259D              LEFT JOIN train_person_1_id ON trai…
        ## 2 train_person_1_id.education_927M          LEFT JOIN train_person_1_id ON trai…
        ## 3 train_person_1_id.empl_employedfrom_271D  LEFT JOIN train_person_1_id ON trai…
        ## 4 train_person_1_id.empl_employedtotal_800L LEFT JOIN train_person_1_id ON trai…
        ## 5 train_person_1_id.empl_industry_691L      LEFT JOIN train_person_1_id ON trai…
        ## 6 train_person_1_id.familystate_447L        LEFT JOIN train_person_1_id ON trai…

    -   `test_base` set up

    ``` r
      test_join %>% select(table_column, join_id_string) %>% head()
    ```

        ## # A tibble: 6 × 2
        ##   table_column                             join_id_string                       
        ##   <chr>                                    <chr>                                
        ## 1 test_person_1_id.birth_259D              LEFT JOIN test_person_1_id ON test_b…
        ## 2 test_person_1_id.education_927M          LEFT JOIN test_person_1_id ON test_b…
        ## 3 test_person_1_id.empl_employedfrom_271D  LEFT JOIN test_person_1_id ON test_b…
        ## 4 test_person_1_id.empl_employedtotal_800L LEFT JOIN test_person_1_id ON test_b…
        ## 5 test_person_1_id.empl_industry_691L      LEFT JOIN test_person_1_id ON test_b…
        ## 6 test_person_1_id.familystate_447L        LEFT JOIN test_person_1_id ON test_b…

-   The parts can be strung together for the final query.

    ``` r
    # create the final train join query

    # all join TABLE on TABLE
    train_join_list <- train_join$join_id_string %>% unique() %>%  str_c(collapse = " ")
    # all TABLE.COLUMNS
    train_table_column_join <- train_join$table_column %>% str_c(collapse = ", ") 
    # full query
    train_join_query <- train_table_column_join   %>% 
      # select train_base columns 
      str_c("SELECT train_base.*, ",.) %>% 
      # 
      str_c(., " FROM train_base") %>% 
      # left join table statements
      str_c(., " ",train_join_list) %>% 
      # save as new table
      str_c("CREATE TABLE IF NOT EXISTS train_joined AS ",.)

    # create the final train join query

    # all join TABLE on TABLE
    test_join_list <- test_join$join_id_string %>% unique() %>%  str_c(collapse = " ")
    # all TABLE.COLUMNS
    test_table_column_join <- test_join$table_column %>% str_c(collapse = ", ") 
    # full query
    test_join_query <- test_table_column_join   %>% 
      # select test_base columns 
      str_c("SELECT test_base.*, ",.) %>% 
      # 
      str_c(., " FROM test_base") %>% 
      # left join table statements
      str_c(., " ",test_join_list) %>% 
      # save as new table
      str_c("CREATE TABLE IF NOT EXISTS test_joined AS ",.)
    ```

    -   `train_base` join query

    ``` r
    train_join_query 
    ```

        ## [1] "CREATE TABLE IF NOT EXISTS train_joined AS SELECT train_base.*, train_person_1_id.birth_259D, train_person_1_id.education_927M, train_person_1_id.empl_employedfrom_271D, train_person_1_id.empl_employedtotal_800L, train_person_1_id.empl_industry_691L, train_person_1_id.familystate_447L, train_person_1_id.incometype_1044T, train_person_1_id.mainoccupationinc_384A, train_static_cb_0_id.riskassesment_940T FROM train_base LEFT JOIN train_person_1_id ON train_base.case_id = train_person_1_id.case_id LEFT JOIN train_static_cb_0_id ON train_base.case_id = train_static_cb_0_id.case_id"

    -   `test_base` join query

    ``` r
    test_join_query
    ```

        ## [1] "CREATE TABLE IF NOT EXISTS test_joined AS SELECT test_base.*, test_person_1_id.birth_259D, test_person_1_id.education_927M, test_person_1_id.empl_employedfrom_271D, test_person_1_id.empl_employedtotal_800L, test_person_1_id.empl_industry_691L, test_person_1_id.familystate_447L, test_person_1_id.incometype_1044T, test_person_1_id.mainoccupationinc_384A, test_static_cb_0_id.riskassesment_940T FROM test_base LEFT JOIN test_person_1_id ON test_base.case_id = test_person_1_id.case_id LEFT JOIN test_static_cb_0_id ON test_base.case_id = test_static_cb_0_id.case_id"

``` r
# join tables with selected features
dbExecute(con, test_join_query)
dbExecute(con, train_join_query)
```

## Data analysis

Now that we have a dataset with all our relevant features, we can move
into data anaysis and prediction. First we need to define our variable
types across our test and train data.

``` r
# read train table
train_joined_df <- dbGetQuery(con, "SELECT * FROM train_joined") %>% tibble()
"train_joined_df" %>% quickrds()
# read test table
test_joined_df <- dbGetQuery(con, "SELECT * FROM test_joined") %>% tibble()
"test_joined_df" %>% quickrds()
```

-   Import and define variable types.
    -   \`
    -   `categorical` variables (e.g., )
    -   `numeric`:

    ``` r
    train_joined_df<- read_rds("train_joined_df.rds")
    # define train table variables
    train_joined_tbl <- train_joined_df %>%   
      # convert date to numeric
      mutate(
      date_decision = as_date(date_decision),
      birth_259D = as_date(birth_259D),
      age = time_length(difftime(date_decision, birth_259D), "years"), 
      month.n = month(date_decision),
      year.n = year(date_decision),
      week.n = WEEK_NUM %>% as.numeric())  %>%
      # label numeric, factors
      mutate(
    across(c(matches("mainoccupationinc_384A|riskassesment_940T|
                     age|\\.n")), 
           ~ as.numeric(.)),
    across(c(matches("education_927M|empl_employedtotal_800L|empl_industry_691L|
                     familystate_447L|incometype_1044T|target|familystate_447L")), 
           ~ as.factor(.))) 
    "train_joined_tbl" %>% quickrds()
    ```

    ``` r
    test_joined_df<- read_rds("test_joined_df.rds")

    # define test table variables
    test_joined_tbl <- test_joined_df %>%   
      # convert date to numeric
      mutate(
      date_decision = as_date(date_decision),
      birth_259D = as_date(birth_259D),
      age = time_length(difftime(date_decision, birth_259D), "years"),
      month.n = month(date_decision),
      year.n = year(date_decision),
      week.n = WEEK_NUM %>% as.numeric()) %>%
      # label numeric, factors
      mutate(
    across(c(matches("mainoccupationinc_384A|riskassesment_940T|
                     age|\\.n")), 
           ~ as.numeric(.)),
    across(c(matches("education_927M|empl_employedtotal_800L|empl_industry_691L|
                     familystate_447L|incometype_1044T|target|familystate_447L")), 
           ~ as.factor(.))) 
    "test_joined_tbl" %>% quickrds()
    ```

    -   `train` df

    ``` r
    train_joined_tbl %>% glimpse()
    ```

        ## Rows: 1,526,659
        ## Columns: 18
        ## $ case_id                 <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, …
        ## $ date_decision           <date> 2019-01-03, 2019-01-03, 2019-01-04, 2019-01-0…
        ## $ MONTH                   <int> 201901, 201901, 201901, 201901, 201901, 201901…
        ## $ WEEK_NUM                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
        ## $ target                  <fct> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
        ## $ birth_259D              <date> 1986-07-01, 1957-08-01, 1974-12-01, 1993-08-0…
        ## $ education_927M          <fct> P97_36_170, P97_36_170, P97_36_170, P33_146_17…
        ## $ empl_employedfrom_271D  <chr> "2017-09-15", "2008-10-29", "2010-02-15", "201…
        ## $ empl_employedtotal_800L <fct> MORE_FIVE, MORE_FIVE, MORE_FIVE, MORE_FIVE, MO…
        ## $ empl_industry_691L      <fct> OTHER, OTHER, OTHER, OTHER, OTHER, OTHER, EDUC…
        ## $ familystate_447L        <fct> MARRIED, DIVORCED, MARRIED, MARRIED, MARRIED, …
        ## $ incometype_1044T        <fct> SALARIED_GOVT, SALARIED_GOVT, EMPLOYED, EMPLOY…
        ## $ mainoccupationinc_384A  <dbl> 10800, 10000, 14000, 10000, 24000, 64000, 2000…
        ## $ riskassesment_940T      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
        ## $ age                     <dbl> 32.50924, 61.42368, 44.09309, 25.42368, 25.007…
        ## $ month.n                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
        ## $ year.n                  <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019…
        ## $ week.n                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

    -   `test` df

    ``` r
    test_joined_tbl %>% glimpse()
    ```

        ## Rows: 10
        ## Columns: 17
        ## $ case_id                 <int> 57543, 57549, 57551, 57552, 57569, 57630, 5763…
        ## $ date_decision           <date> 2020-10-06, 2020-10-06, 2020-10-06, 2020-10-0…
        ## $ MONTH                   <int> 202010, 202010, 202010, 202010, 202010, 202010…
        ## $ WEEK_NUM                <int> 92, 92, 92, 92, 92, 92, 92, 92, 92, 92
        ## $ birth_259D              <date> 1996-08-01, 1992-03-01, 1990-08-01, NA, NA, NA…
        ## $ education_927M          <fct> P97_36_170, P97_36_170, P97_36_170, NA, NA, N…
        ## $ empl_employedfrom_271D  <chr> "2018-02-15", "2019-05-04", "2019-01-15", NA, …
        ## $ empl_employedtotal_800L <fct> MORE_ONE, MORE_ONE, MORE_FIVE, NA, NA, NA, NA,…
        ## $ empl_industry_691L      <fct> OTHER, OTHER, OTHER, NA, NA, NA, NA, NA, NA, NA
        ## $ familystate_447L        <fct> MARRIED, SINGLE, MARRIED, NA, NA, NA, NA, NA, …
        ## $ incometype_1044T        <fct> EMPLOYED, EMPLOYED, EMPLOYED, NA, NA, NA, NA, …
        ## $ mainoccupationinc_384A  <dbl> 36000, 15000, 24000, NA, NA, NA, NA, NA, NA, NA
        ## $ riskassesment_940T      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
        ## $ age                     <dbl> 24.18070, 28.59959, 30.18207, NA, NA, NA, NA, …
        ## $ month.n                 <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
        ## $ year.n                  <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020…
        ## $ week.n                  <dbl> 92, 92, 92, 92, 92, 92, 92, 92, 92, 92

### Summary statistics

Here’s a look at some summary statistics.

### Nominal variables

``` r
library(summarytools)
```

    ## 
    ## Attaching package: 'summarytools'

    ## The following object is masked from 'package:tibble':
    ## 
    ##     view

``` r
# factor level proportions
train_joined_tbl %>% select(where(is.factor)) %>% freq()
```

    ## Frequencies  
    ## train_joined_tbl$target  
    ## Type: Factor  
    ## 
    ##                  Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- --------- --------- -------------- --------- --------------
    ##           0   1478665     96.86          96.86     96.86          96.86
    ##           1     47994      3.14         100.00      3.14         100.00
    ##        <NA>         0                               0.00         100.00
    ##       Total   1526659    100.00         100.00    100.00         100.00
    ## 
    ## train_joined_tbl$education_927M  
    ## Type: Factor  
    ## 
    ##                        Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------------- --------- --------- -------------- --------- --------------
    ##          a55475b1    798046    52.274         52.274    52.274         52.274
    ##       P106_81_188     54661     3.580         55.854     3.580         55.854
    ##       P157_18_172       631     0.041         55.896     0.041         55.896
    ##        P17_36_170      5481     0.359         56.255     0.359         56.255
    ##       P33_146_175    258589    16.938         73.193    16.938         73.193
    ##        P97_36_170    409251    26.807        100.000    26.807        100.000
    ##              <NA>         0                              0.000        100.000
    ##             Total   1526659   100.000        100.000   100.000        100.000
    ## 
    ## train_joined_tbl$empl_employedtotal_800L  
    ## Type: Factor  
    ## 
    ##                           Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## -------------------- --------- --------- -------------- --------- --------------
    ##       (Empty string)    998344     65.39          65.39     65.39          65.39
    ##             LESS_ONE     30467      2.00          67.39      2.00          67.39
    ##            MORE_FIVE    371321     24.32          91.71     24.32          91.71
    ##             MORE_ONE    126527      8.29         100.00      8.29         100.00
    ##                 <NA>         0                               0.00         100.00
    ##                Total   1526659    100.00         100.00    100.00         100.00
    ## 
    ## train_joined_tbl$empl_industry_691L  
    ## Type: Factor  
    ## 
    ##                              Freq     % Valid   % Valid Cum.     % Total   % Total Cum.
    ## ----------------------- --------- ----------- -------------- ----------- --------------
    ##          (Empty string)   1004423    65.79223       65.79223    65.79223       65.79223
    ##             AGRICULTURE      5288     0.34638       66.13861     0.34638       66.13861
    ##             ARMY_POLICE        14     0.00092       66.13952     0.00092       66.13952
    ##               ART_MEDIA       245     0.01605       66.15557     0.01605       66.15557
    ##                CATERING      3508     0.22978       66.38536     0.22978       66.38536
    ##       CHARITY_RELIGIOUS        25     0.00164       66.38699     0.00164       66.38699
    ##               EDUCATION     30346     1.98774       68.37473     1.98774       68.37473
    ##                 FINANCE      3150     0.20633       68.58106     0.20633       68.58106
    ##                  GAMING        96     0.00629       68.58735     0.00629       68.58735
    ##              GOVERNMENT     35440     2.32141       70.90876     2.32141       70.90876
    ##                  HEALTH     13026     0.85324       71.76200     0.85324       71.76200
    ##               INSURANCE       217     0.01421       71.77621     0.01421       71.77621
    ##                      IT       279     0.01828       71.79449     0.01828       71.79449
    ##                  LAWYER       564     0.03694       71.83143     0.03694       71.83143
    ##           MANUFACTURING      9035     0.59182       72.42325     0.59182       72.42325
    ##               MARKETING       239     0.01566       72.43890     0.01566       72.43890
    ##                  MINING      3582     0.23463       72.67353     0.23463       72.67353
    ##                   OTHER    386837    25.33880       98.01233    25.33880       98.01233
    ##              POST_TELCO       612     0.04009       98.05241     0.04009       98.05241
    ##             REAL_ESTATE      3680     0.24105       98.29346     0.24105       98.29346
    ##             RECRUITMENT        85     0.00557       98.29903     0.00557       98.29903
    ##                 TOURISM       513     0.03360       98.33263     0.03360       98.33263
    ##                   TRADE     20696     1.35564       99.68827     1.35564       99.68827
    ##          TRANSPORTATION      4318     0.28284       99.97111     0.28284       99.97111
    ##                 WELNESS       441     0.02889      100.00000     0.02889      100.00000
    ##                    <NA>         0                                0.00000      100.00000
    ##                   Total   1526659   100.00000      100.00000   100.00000      100.00000
    ## 
    ## train_joined_tbl$familystate_447L  
    ## Type: Factor  
    ## 
    ##                                Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ------------------------- --------- --------- -------------- --------- --------------
    ##            (Empty string)    798046     52.27          52.27     52.27          52.27
    ##                  DIVORCED     19296      1.26          53.54      1.26          53.54
    ##       LIVING_WITH_PARTNER      8142      0.53          54.07      0.53          54.07
    ##                   MARRIED    484846     31.76          85.83     31.76          85.83
    ##                    SINGLE    183334     12.01          97.84     12.01          97.84
    ##                   WIDOWED     32995      2.16         100.00      2.16         100.00
    ##                      <NA>         0                               0.00         100.00
    ##                     Total   1526659    100.00         100.00    100.00         100.00
    ## 
    ## train_joined_tbl$incometype_1044T  
    ## Type: Factor  
    ## 
    ##                                    Freq      % Valid   % Valid Cum.      % Total   % Total Cum.
    ## ----------------------------- --------- ------------ -------------- ------------ --------------
    ##                      EMPLOYED    298158    19.530098      19.530098    19.530098      19.530098
    ##                   HANDICAPPED         1     0.000066      19.530164     0.000066      19.530164
    ##                 HANDICAPPED_2      7371     0.482819      20.012983     0.482819      20.012983
    ##                 HANDICAPPED_3      5258     0.344412      20.357395     0.344412      20.357395
    ##                         OTHER     11436     0.749087      21.106482     0.749087      21.106482
    ##       PRIVATE_SECTOR_EMPLOYEE    490562    32.133043      53.239525    32.133043      53.239525
    ##             RETIRED_PENSIONER    311028    20.373115      73.612640    20.373115      73.612640
    ##                 SALARIED_GOVT    373646    24.474752      98.087392    24.474752      98.087392
    ##                  SELFEMPLOYED     29199     1.912608     100.000000     1.912608     100.000000
    ##                          <NA>         0                                 0.000000     100.000000
    ##                         Total   1526659   100.000000     100.000000   100.000000     100.000000

### Numeric variables

``` r
# numeric variable summaries
train_joined_tbl %>% select(where(is.numeric)) %>% descr()
```

    ## Descriptive Statistics  
    ## train_joined_tbl  
    ## N: 1526659  
    ## 
    ##                            age      case_id   mainoccupationinc_384A        MONTH      month.n
    ## ----------------- ------------ ------------ ------------------------ ------------ ------------
    ##              Mean        44.56   1286076.57                 57707.48    201936.29         6.43
    ##           Std.Dev        14.03    718946.59                 33348.30        44.74         3.51
    ##               Min        20.96         0.00                     0.00    201901.00         1.00
    ##                Q1        32.74    766197.00                 36000.00    201906.00         3.00
    ##            Median        42.81   1357358.00                 50000.00    201910.00         7.00
    ##                Q3        56.20   1739023.00                 70000.00    202001.00         9.00
    ##               Max        76.04   2703454.00                200000.00    202010.00        12.00
    ##               MAD        16.72    721155.91                 29652.00         7.41         4.45
    ##               IQR        23.46    972825.00                 34000.00        95.00         6.00
    ##                CV         0.31         0.56                     0.58         0.00         0.55
    ##          Skewness         0.27         0.14                     1.66         0.87        -0.05
    ##       SE.Skewness         0.00         0.00                     0.00         0.00         0.00
    ##          Kurtosis        -1.03        -0.59                     3.71        -1.22        -1.23
    ##           N.Valid   1526659.00   1526659.00               1526659.00   1526659.00   1526659.00
    ##         Pct.Valid       100.00       100.00                   100.00       100.00       100.00
    ## 
    ## Table: Table continues below
    ## 
    ##  
    ## 
    ##                     riskassesment_940T     WEEK_NUM       week.n       year.n
    ## ----------------- -------------------- ------------ ------------ ------------
    ##              Mean                 0.23        40.77        40.77      2019.30
    ##           Std.Dev                 0.98        23.80        23.80         0.46
    ##               Min                -3.67         0.00         0.00      2019.00
    ##                Q1                -0.23        23.00        23.00      2019.00
    ##            Median                 0.37        40.00        40.00      2019.00
    ##                Q3                 0.97        55.00        55.00      2020.00
    ##               Max                 2.12        91.00        91.00      2020.00
    ##               MAD                 0.89        25.20        25.20         0.00
    ##               IQR                 1.20        32.00        32.00         1.00
    ##                CV                 4.32         0.58         0.58         0.00
    ##          Skewness                -0.83         0.30         0.30         0.88
    ##       SE.Skewness                 0.01         0.00         0.00         0.00
    ##          Kurtosis                 0.25        -0.65        -0.65        -1.23
    ##           N.Valid             53560.00   1526659.00   1526659.00   1526659.00
    ##         Pct.Valid                 3.51       100.00       100.00       100.00

### Summary

We see that `riskassesment_940T` has 96.5% missing entries so it may be
best to leave it out for now.
