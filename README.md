# easylca <img src="man/figures/logo.png" align="right" height="139" alt="" />

`easylca` is designed to render conducting latent class analyses with mixed indicators (LCAs) with *Mplus 8.4* more accessible. *Mplus* is a powerful commercial statistical software package developed by Muthen & Muthen ([check out their website here](www.statmodel.com)). With `easylca`, there is no need to worry about learning the *Mplus* syntax.

The `easylca` package wraps itself around the `MplusAutomation` package which communicates with *Mplus* from within R. However, `easylca` in addition provides functions that ensure data compatibility and can automatically create and run even the most complex LCAs involving censored, zero-inflated, count or negative binomially distributed variables. Furthermore, `easylca` includes functions that generate HTML reports for data inspection, model selection and model profile inspection.

**Be aware: *MPlus 8.4* must to be installed on your computer in order to use package and is *not* installed together with `easylca`.**

`easylca` also depends on the `rhdf5` package. Depending on your environment you may need to install the `BiocManager` package manager first to install the `rhdf5` package.
If required, please follow these two commands to complete installation:
    ``install.packages("BiocManager");
    BiocManager::install("rhdf5")``
  
Follow these steps to conduct an LCA using `easylca`: 
1. Inspect your data using the `generate_data_diagnosis_report()` function (this generates an HTML report in the current working directory).
2. Define your analysis with `define_lca()`.
3. Run your initial analysis with `perform_lca()`. 

If you are unable to complete the analysis for any reason, you can use the `read_models()` command to read the intermediate results from the folder structure created by `easylca` for the analysis.

4. Inspect the results and select a model using the `generate_model_selection_report()` command (this generates an HTML report in the current working directory).
5. Inspect the profiles of individual models with the `generate_model_report()` command (this generates an HTML report in the current working directory).
6. Use `rerun_lca()` to increase the number of random starting values for the analysis if the best solution for some models was not replicated (see `generate_model_selection_report()`). For convenience, this function also offers a loop option that will rerun the analysis until all the requested models have replicated 
