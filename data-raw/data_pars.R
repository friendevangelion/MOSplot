data_pars <- load_parameters(file="data-raw/CP003879.pars.txt")

devtools::use_data(data_pars, overwrite = TRUE)