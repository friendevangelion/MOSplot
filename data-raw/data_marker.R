data_marker <- load_parameters(file="data-raw/CP003879.marker.tab", sep="\t", stringsAsFactors=F)
devtools::use_data(data_marker, overwrite = TRUE)