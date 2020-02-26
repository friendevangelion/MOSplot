#' test
#'
pars <- load_parameters(file="data-raw/CP003879.pars.txt")
cog_color <- read.table("data-raw/COG.dark.color.config",
                        sep="\t", stringsAsFactors=F, comment.char="")
data_gc <- read.table("data-raw/CP003879.genomeGC.tab",
                      header=T, sep="\t", stringsAsFactors=F)
data_gc <- mapping_data(data=data_gc, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))
data_gene <- read.table("data-raw/CP003879.codingGene.tab",
                        header=T, sep="\t", stringsAsFactors=F, comment.char="", quote="")
data_gene <- data_gene[,-7]
data_gene <- mapping_data(data=data_gene, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))
data_gene <- mapping_data(data=data_gene, col_name="COGcolor", ori_tag=cog_color[,1], mapping_tag=cog_color[,2], other_tag="#BBBBBB")
pdf("~/test.pdf")
mapping_bg(pars)
col_plot <- rep("#525252", nrow(data_gc))
col_plot[which(data_gc$GCcontent<0.37)] <- "#969696"
mapping_shape(mos_pars=pars, shape="b",
              panel=1, frag=data_gc$SeqID,
              x.st=data_gc$startPosition+300, x.en=data_gc$endPosition-300,
              y.st=data_gc$GCcontent, y.en=0.37, y.range=c(0.22,0.52),
              fill=col_plot, fill.alpha=0.9)
mapping_shape(mos_pars=pars, shape="p",
              panel=1, frag=data_gc$SeqID,
              x=(data_gc$startPosition+data_gc$endPosition)/2,
              y=data_gc$GCskew, y.range=c(-0.65,0.65),
              col="#88419D", size=0.1)
mapping_shape(mos_pars=pars, shape="l",
              panel=1, frag=data_gc$SeqID,
              x=(data_gc$startPosition+data_gc$endPosition)/2,
              y=data_gc$GCskew, y.range=c(-0.65,0.65),
              col="#88419D", size=0.2)
data_plot <- data_gene[which(data_gene$Strand=="+"),]
mapping_shape(mos_pars=pars, shape="a",
              panel=2, frag=data_plot$SeqID,
              x.st=data_plot$Start, x.en=data_plot$End,
              y.st=0.02, y.en=0.49, y.range=c(0,1),
              col="#FFFFFF", col.alpha=0.8,
              fill=data_plot$COGcolor, fill.alpha=0.8,
              arrow.size=300, arrow.type=1)
data_plot <- data_gene[which(data_gene$Strand=="-"),]
mapping_shape(mos_pars=pars, shape="a",
              panel=2, frag=data_plot$SeqID,
              x.st=data_plot$Start, x.en=data_plot$End,
              y.st=0.51, y.en=0.98, y.range=c(0,1),
              col="#FFFFFF", col.alpha=0.8,
              fill=data_plot$COGcolor, fill.alpha=0.8,
              arrow.size=-300, arrow.type=1)
dev.off()
