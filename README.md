
<!-- README.md is generated from README.Rmd. Please edit that file -->
 

<p align="center">
![Phenotools](inst/rstudio/templates/phenotools.png)
</p>
 

The goal of the **phenotools** package is to facilitate efficient and reproducible use of phenotypic data from MoBa and linked registry sources in the TSD environment.

Please contact <laurie.hannigan@bristol.ac.uk> with bugs, feedback, or development ideas.  

Installation
------------

You can install the latest working version as follows...

``` r
install.packages("//tsd-evs/p471/data/durable/common/software/phenotools_0.1.0.zip", 
                 repos=NULL,
                 type = "binary")
```

At present, any missing dependencies need to be installed manually from the TSD CRAN copy, i.e.,

``` r
install.packages('dplyr',
                 repos = "file://tsd-evs/shared/R/cran")
```

You can also install from source if needed.

  \#\# Overview and project set-up

The **phenotools** package primarily provides functions to assist you at the beginning and end of your analytic workflow; specifically, in the respective areas of data preparation, and analysis documentation and reporting.

![Phenotools in TSD reproducible workflow.](inst/rstudio/templates/phenotools_workflow.jpg)

These functions are outlined below. However, to make full usage of the phenotools package, it is advisable to begin by using it to set up your analytic project with a pre-defined structure using the `initialise_project` function:

``` r
initialise_project(path="C:/Users/p471-lauriejh/newproject",
                   template_filepath="N:/data/durable/common/new_project_template/.")
#> Initialising...
#> 
#> Project initialised successfully. Open via Rstudio>File>Open project...
#> or open the >yourproject<.Rproj file from Windows Explorer.
```

This creates a project directory in your specified location, populated with sub-directories and files from a template - shown here in the "Files" pane in Rstudio after opening the new project:

<p align="center">
<img src="inst/rstudio/templates/newproj_filestr.png" alt="Phenotools" width="250" />

</p>
Using this template helps to keep consistency across analytic projects, making collaboration and reproducible working easier. Moreover, some of phenotools' reporting functions make use of this structure to find relevant files and combine them for export, so deviating too far from this core structure will restrict the extent to which the package can help you at the other end.

  \#\# Data preparation with phenotools

The main workhorse of the phenotools package is the `curate_dataset` function. The purpose of this function is to curate an analysis-ready dataset based on the variables, from MoBa and other supported sources, you request. This means that psychometric scales will be coded up for you, and derived variables such as BMI computed and returned along with specific demographic variables and family-level IDs.

The first step is to ascertain what variables are available and what names you need to refer to them by in your request. This is done using the `available_variables` function:

``` r
available_variables(source = "moba")
#> Values from column:var_name are valid inputs for curate_dataset()
#> # A tibble: 60 x 5
#>    measure subscale   questionnaire var_name     source
#>    <chr>   <chr>      <chr>         <chr>        <chr> 
#>  1 BMI     mother     Q1            bmi_mat_q1   moba  
#>  2 BMI     father     Q1            bmi_pat_q1   moba  
#>  3 SCL     anxiety    Q3            scl_anx_q3   moba  
#>  4 SCL     depression Q3            scl_dep_q3   moba  
#>  5 SCL     <NA>       Q4_6months    scl_full_6m  moba  
#>  6 SCL     anxiety    Q4_6months    scl_anx_6m   moba  
#>  7 SCL     depression Q4_6months    scl_dep_6m   moba  
#>  8 SCL     <NA>       Q5_18months   scl_full_18m moba  
#>  9 SCL     anxiety    Q5_18months   scl_anx_18m  moba  
#> 10 SCL     depression Q5_18months   scl_dep_18m  moba  
#> # ... with 50 more rows
```

To get a dataset with a variable that you want, you need to quote its var\_name as the input for the `variables_required` option in the `curate_dataset` function:

``` r
mydata <- curate_dataset(variables_required="scl_anx_q3",
                         pheno_data_root_dir="N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                         PDB="2306",
                         completion_threshold=0.5,
                         return_items=FALSE,
                         consistent_items=FALSE,
                         transformations=NULL)
#> Checking inputs...
#> 
#> Processing MoBa variables...
#> 
#> Loading data from questionnaire Q3, which is number 1 of 1
#> 
#> Processing MoBa scale variables. These are: 
#> 
#> scl_anx_q3
#> 
#> Expect a wait of up to:
#> 
#> 10.7 seconds (0.2 mins)
#> 
#> for your requested scales...
#> 
#> Processing of MoBa scale variables complete.
#> 
#> Curating the final dataset(s)...
#> 
#> Dataset curation complete; outputting as data.frame.

head(mydata)
#> # A tibble: 6 x 6
#>   preg_id BARN_NR m_id    f_id    birth_yr scl_anx_q3
#>   <chr>     <dbl> <chr>   <chr>      <dbl>      <dbl>
#> 1 1             1 M100499 F027449     2005          2
#> 2 2             1 M080431 F034439     2007          0
#> 3 3             1 M041879 F010847     2005          1
#> 4 4             1 M073267 F006456     2003          0
#> 5 5             1 M021417 <NA>        2000          0
#> 6 6             1 M018960 F057876     2006         NA
```

Other important inputs here are the root directory of the MoBa phenotypic data (SPSS format) and PDB code for the project you are working in. Both default to the correct values for the p471 TSD project.

Likely, you will want a dataset with multiple phenotypes; `variables_required` needs the var\_names for your variables as a concatenated string:

``` r

##Not run
mydata <- curate_dataset(variables_required=c("scl_anx_q3","bmi_pat_q1"))
```

Alternatively, you can use columns in the dataframe produced by the `available_variables` function to get a list of variables and input that:

``` r

phenovars <- available_variables(source = "moba")
#> Values from column:var_name are valid inputs for curate_dataset()

head(phenovars)
#> # A tibble: 6 x 5
#>   measure subscale   questionnaire var_name    source
#>   <chr>   <chr>      <chr>         <chr>       <chr> 
#> 1 BMI     mother     Q1            bmi_mat_q1  moba  
#> 2 BMI     father     Q1            bmi_pat_q1  moba  
#> 3 SCL     anxiety    Q3            scl_anx_q3  moba  
#> 4 SCL     depression Q3            scl_dep_q3  moba  
#> 5 SCL     <NA>       Q4_6months    scl_full_6m moba  
#> 6 SCL     anxiety    Q4_6months    scl_anx_6m  moba

myphenovars <- dplyr::filter(phenovars,
                            stringr::str_detect(measure,"SCL"),
                            stringr::str_detect(subscale,"depression"))

myphenovars$var_name
#> [1] "scl_dep_q3"   "scl_dep_6m"   "scl_dep_18m"  "scl_dep_5yr" 
#> [5] "scl_dep_3yr"  "scl_dep_8yr"  "scl_dep_far"  "scl_dep_far2"
```

``` r
##Not run
mydata <- curate_dataset(variables_required=myphenovars$var_name)
```

In the above cases `curate_dataset` returns a data.frame with the processed scale scores and other variables. However, if you set `{r echo=T} return_items = TRUE`, then the result of `curate_dataset` will be a list, where the first element is the scale-level dataset, and the second element is the item-level dataset.

``` r

mydata <- curate_dataset(variables_required=c("scl_anx_q3","bmi_pat_q1"),
                         return_items = T)
#> Checking inputs...
#> 
#> Processing MoBa variables...
#> 
#> Loading data from questionnaire Q1, which is number 1 of 2
#> 
#> Loading data from questionnaire Q3, which is number 2 of 2
#> 
#> Processing MoBa scale variables. These are: 
#> 
#> scl_anx_q3
#> 
#> Expect a wait of up to:
#> 
#> 13.1 seconds (0.2 mins)
#> 
#> for your requested scales...
#> 
#> Processing of MoBa scale variables complete.
#> 
#> Processing non-scale MoBa vars. These are: 
#> 
#> bmi_pat_q1
#> Processing BMI variable 1 of 1
#> 
#> Processing of BMI variables is complete.
#> 
#> Processing of non-scale MoBa variables is complete.
#> 
#> Curating the final dataset(s)...
#> 
#> Dataset curation complete; outputting as data.frame. You requested that
#> scale items be returned (using return_items=TRUE), so output is a list, of which the
#> first element ("scales") is your scale-level dataset, and the second ("items")
#> is your item-level dataset, with "_raw" and "_coded" (i.e., numeric) versions of all items
#> for each scale.

str(mydata)
#> List of 2
#>  $ scales:Classes 'tbl_df', 'tbl' and 'data.frame':  114143 obs. of  9 variables:
#>   ..$ preg_id        : chr [1:114143] "1" "2" "3" "4" ...
#>   ..$ BARN_NR        : num [1:114143] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..- attr(*, "format.spss")= chr "F12.0"
#>   .. ..- attr(*, "display_width")= int 12
#>   ..$ m_id           : chr [1:114143] "M100499" "M080431" "M041879" "M073267" ...
#>   ..$ f_id           : chr [1:114143] "F027449" "F034439" "F010847" "F006456" ...
#>   ..$ birth_yr       : num [1:114143] 2005 2007 2005 2003 2000 ...
#>   .. ..- attr(*, "label")= chr "FAAR"
#>   .. ..- attr(*, "format.spss")= chr "F5.0"
#>   ..$ scl_anx_q3     : num [1:114143] 2 0 1 0 0 NA 0 2 0 1 ...
#>   ..$ pat_q1_bmi     : num [1:114143] 23.4 26.3 23.4 32.8 26.6 ...
#>   ..$ pat_q1_heightcm: num [1:114143] 179 170 173 178 194 NA 173 185 176 185 ...
#>   ..$ pat_q1_weightkg: num [1:114143] 75 76 70 104 100 NA 65 NA 80 90 ...
#>  $ items :Classes 'tbl_df', 'tbl' and 'data.frame':  114143 obs. of  13 variables:
#>   ..$ preg_id                   : chr [1:114143] "1" "2" "3" "4" ...
#>   ..$ BARN_NR                   : num [1:114143] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..- attr(*, "format.spss")= chr "F12.0"
#>   .. ..- attr(*, "display_width")= int 12
#>   ..$ m_id                      : chr [1:114143] "M100499" "M080431" "M041879" "M073267" ...
#>   ..$ f_id                      : chr [1:114143] "F027449" "F034439" "F010847" "F006456" ...
#>   ..$ birth_yr                  : num [1:114143] 2005 2007 2005 2003 2000 ...
#>   .. ..- attr(*, "label")= chr "FAAR"
#>   .. ..- attr(*, "format.spss")= chr "F5.0"
#>   ..$ scl_anx_q3_i1_CC1202_raw  : chr [1:114143] "Not bothered" "Not bothered" "Not bothered" "Not bothered" ...
#>   ..$ scl_anx_q3_i2_CC1203_raw  : chr [1:114143] "A little bothered" "Not bothered" "Not bothered" "Not bothered" ...
#>   ..$ scl_anx_q3_i3_CC1208_raw  : chr [1:114143] "A little bothered" "Not bothered" "A little bothered" "Not bothered" ...
#>   ..$ scl_anx_q3_i4_CC1209_raw  : chr [1:114143] "Not bothered" "Not bothered" "Not bothered" "Not bothered" ...
#>   ..$ scl_anx_q3_i1_CC1202_coded: num [1:114143] 0 0 0 0 0 NA 0 0 0 1 ...
#>   ..$ scl_anx_q3_i2_CC1203_coded: num [1:114143] 1 0 0 0 0 NA 0 1 0 0 ...
#>   ..$ scl_anx_q3_i3_CC1208_coded: num [1:114143] 1 0 1 0 NA NA 0 1 0 0 ...
#>   ..$ scl_anx_q3_i4_CC1209_coded: num [1:114143] 0 0 0 0 NA NA 0 0 0 0 ...

myscaledata <- mydata[["scales"]]
myitemdata <- mydata[["items"]]

head(myscaledata)
#> # A tibble: 6 x 9
#>   preg_id BARN_NR m_id  f_id  birth_yr scl_anx_q3 pat_q1_bmi
#>   <chr>     <dbl> <chr> <chr>    <dbl>      <dbl>      <dbl>
#> 1 1             1 M100~ F027~     2005          2       23.4
#> 2 2             1 M080~ F034~     2007          0       26.3
#> 3 3             1 M041~ F010~     2005          1       23.4
#> 4 4             1 M073~ F006~     2003          0       32.8
#> 5 5             1 M021~ <NA>      2000          0       26.6
#> 6 6             1 M018~ F057~     2006         NA       NA  
#> # ... with 2 more variables: pat_q1_heightcm <dbl>, pat_q1_weightkg <dbl>
head(myitemdata)
#> # A tibble: 6 x 13
#>   preg_id BARN_NR m_id  f_id  birth_yr scl_anx_q3_i1_C~ scl_anx_q3_i2_C~
#>   <chr>     <dbl> <chr> <chr>    <dbl> <chr>            <chr>           
#> 1 1             1 M100~ F027~     2005 Not bothered     A little bother~
#> 2 2             1 M080~ F034~     2007 Not bothered     Not bothered    
#> 3 3             1 M041~ F010~     2005 Not bothered     Not bothered    
#> 4 4             1 M073~ F006~     2003 Not bothered     Not bothered    
#> 5 5             1 M021~ <NA>      2000 Not bothered     Not bothered    
#> 6 6             1 M018~ F057~     2006 <NA>             <NA>            
#> # ... with 6 more variables: scl_anx_q3_i3_CC1208_raw <chr>,
#> #   scl_anx_q3_i4_CC1209_raw <chr>, scl_anx_q3_i1_CC1202_coded <dbl>,
#> #   scl_anx_q3_i2_CC1203_coded <dbl>, scl_anx_q3_i3_CC1208_coded <dbl>,
#> #   scl_anx_q3_i4_CC1209_coded <dbl>
```

See the help pages for `curate_dataset` for more options.

 

Analysis documentation and reporting with phenotools (in development)
---------------------------------------------------------------------

Functions to assist with analysis documentation and reporting are currently in development. These include:

-   `dataset_report()` which creates an Rmarkdown file for specified output with a summary report of the dataset, including descriptive statistics, psychometric properties of scales and attrition analyses for longitudinal data

-   `synthesise_data()` and `simulate_data()` which make and save synthetic or simulated datasets for export with analysis code based on dataset properties and/or structure

-   `dataprep_code()` which produces an R file with the code used to generate your dataset for export
