data_gc <- read.table("data-raw/CP003879.genomeGC.tab", header=T, sep="\t", stringsAsFactors=F)

devtools::use_data(data_gc, overwrite = TRUE)