data_gene <- read.table("data-raw/CP003879.codingGene.tab", header=T, sep="\t", stringsAsFactors=F, comment.char="", quote="")
devtools::use_data(data_gene, overwrite = TRUE)