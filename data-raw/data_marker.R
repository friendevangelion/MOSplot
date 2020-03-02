data_marker <- read.table(file="data-raw/CP003879.marker.tab", sep="\t", stringsAsFactors=F, header=T)

devtools::use_data(data_marker, overwrite = TRUE)