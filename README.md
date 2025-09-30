# easylca <img src="man/figures/logo.png" align="right" height="139" alt="" />
<!-- badges: start -->
  [![R-CMD-check](https://github.com/markusradke/easylca/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/markusradke/easylca/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`easylca` is an R package designed to make conducting latent class analyses with mixed indicators (LCAs) with *Mplus 8.4* more accessible. *Mplus* is a powerful commercial statistical software package developed by Muthen & Muthen ([check out their website here](https://www.statmodel.com)). It is based on text file input and output, and employs its own syntax for model formulation. With `easylca`, however, there is no need to worry about learning the *Mplus* syntax.

The `easylca` package is an easy-to-use wrapper around the `MplusAutomation` package by Hallquist et al. ([check out their package here](https://cran.r-project.org/package=MplusAutomation)) which communicates with *Mplus* from within R. However, `easylca` in addition provides functions that ensure data compatibility and can automatically create and run even the most complex LCAs involving censored, zero-inflated, count or negative binomially distributed variables. Furthermore, `easylca` includes functions that generate HTML reports for data inspection, model selection and model profile inspection.

## Installation 

You can install the development version of `easylca` from [GitHub](https://github.com/) with:

``` r
devtools::install_github("markusradke/easylca")
```

> [!IMPORTANT]
> Be aware: *MPlus 8.4* must to be installed on your computer in order to use `easylca` and is *not* installed together with this package.


`easylca` also depends on the `rhdf5` package. Depending on your environment you may need to install the `BiocManager` package manager first to install the `rhdf5` package. If required, please follow the two commands below to complete installation. 

```r 
install.packages("BiocManager")
BiocManager::install("rhdf5")
```

## How to conduct LCA with `easylca`
Follow these steps to conduct a (mixed mode) latent class analysis (LCA) using `easylca`: 
1. Prepare your data for an analysis with Mplus. Please note that all discrete variables must be stored in **integer**-vectors within your data frame with values greater or equal 1 and with fewer than 10 different levels. All continuous variables must be stored in **numeric**/**double** vectors. For the analysis with *Mplus*, variable names also should not start with a number and must consists of a maximum of 8 characters.
```r
# This is our ready-made example data set:
titanic_passengers 
```

2. Inspect the relevant statistics for on the data for model definition with an HTML report that is created in the current working directory.
```r 
generate_data_diagnosis_report(titanic_passengers)
```

3. Now, you can define your analysis and instruct easylca on how to model the variables in the data set.
```r
titanic_settings <- define_lca(frame = titanic_passengers,
                               analysis_name = 'titanic',
                               id_variable = 'id',
                               nclasses = 3,
                               nominal = c('port', 'pasclass'),
                               categorical = c('survived', 'isfem', 'nsibsp', 'nparchi'),
                               starts = 80,
                               cores = 16)

```

4. Run your initial analysis with the settings you just created. 
```r
# Attention: This may take quite a while...
titanic_lca_results <- perform_lca(titanic_settings)
```

If you are unable to finish the analysis for any reason, you can use the `read_models()` command to retrieve the intermediate results from the folder structure created by `easylca` for the analysis. 
For now, there is also an off-the-shelf example results object already prepared for your convenience: `titanic_lca_results`.

5. Inspect the LCA results for different model types and numbers of classes using an HTML report that is created in the current working directory. This report is very helpful for class enumeration because it shows the necessary information criteria (e.g., *BIC* and *saBIC*) as well as information on the *[Vuong-Lo-Mendell-Rubin test](https://www.jstor.org/stable/2673445)* results (if the test was conducted during the modeling process).
```r
# You can try this command with the ready-made titanic_lca_results object from `easylca`
generate_model_selection_report(titanic_lca_results)
```

6.   The best solution should be found at least two times with different random start values. If not all models were replicated, try increasing the number of random starting values for the analysis. For your convenience, this function also offers a loop option that will rerun the analysis until all the requested models are replicated.
```r
# Attention again: This also may take quite a while (or even longer)...
rerun_lca(titanic_lca_results)
```

7. After deciding on a specific model, you can view its estimated class profiles in another HTML report created in the current working directory.
```r
generate_model_report(titanic_lca_resutls, 2, 6)
```


8. Extract the model parameters and class assignments for the observations from the results object of perform_lca() or rerun_lca() for further analysis or visualization.
```r
# for example
titanic_lca_results$summary # overview of different models
titanic_lca_results$models[[2]][[6]]$parameters # model parameters of model type 2, 6 classes
titanic_lca_results$models[[2]][[6]]$savedata # class assignements
```

Please refer to the corresponding help functions (e.g., `?define_lca()`) to see a reference of all function arguments.


