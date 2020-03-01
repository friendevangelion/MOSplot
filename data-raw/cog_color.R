cog_color <- read.table("data-raw/COG.dark.color.config", header=T, sep="\t", stringsAsFactors=F, comment.char="")
devtools::use_data(cog_color, overwrite = TRUE)