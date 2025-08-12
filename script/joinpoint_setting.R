
## joinpoint setting for number

run_opt_rate = run_options(model="ln",
                           max_joinpoints=5,
                           model_selection_method = 'permutation test',
                           ci_method = 'parametric',
                           dependent_variable_type = 'crude rate',
                           n_cores=parallel::detectCores())

## joinpoint setting for rate
run_opt_number = run_options(model="ln",
                             max_joinpoints=5,
                             model_selection_method = 'permutation test',
                             ci_method = 'parametric',
                             dependent_variable_type = 'count',
                             n_cores=parallel::detectCores())

## export options
export_opt = export_options(aapc_full_range  = TRUE,
                            export_aapc = TRUE,
                            aapc_start_range1 = 1990,
                            aapc_end_range1 = 1999,
                            aapc_start_range2 = 1999,
                            aapc_end_range2 = 2009,
                            aapc_start_range3 = 2009,
                            aapc_end_range3 = 2019)
export_opt_new <- paste0(
  export_opt,
  "\nAAPC Start Range4=2019",
  "\nAAPC End Range4=2021",
  "\nAAPC Start Range5=1990",
  "\nAAPC End Range5=2019"
)

rm(export_opt)
