library(MOSplot)

# please modify the output full path in pdf("test.pdf")
updated_pars <- update_parameters(pars)
updated_gc <- mapping_data(data=data_gc, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))
updated_gene <- mapping_data(data=data_gene, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))
updated_gene <- mapping_data(data=updated_gene, col_name="COGcolor", ori_tag=cog_color$categories, mapping_tag=cog_color$hex, other_tag="#BBBBBB")
pdf("~/test.pdf")
mapping_bg(updated_pars)
col_plot <- rep("#525252", nrow(updated_gc))
col_plot[which(updated_gc$GCcontent<0.37)] <- "#969696"
mapping_shape(mos_pars=updated_pars, shape="b",
              panel=1, frag=updated_gc$SeqID,
              x.st=updated_gc$startPosition+300,
              x.en=updated_gc$endPosition-300,
              y.st=updated_gc$GCcontent, y.en=0.37, y.range=c(0.22,0.52),
              fill=col_plot, fill.alpha=0.9)
mapping_shape(mos_pars=updated_pars, shape="p",
              panel=1, frag=updated_gc$SeqID,
              x=(updated_gc$startPosition+updated_gc$endPosition)/2,
              y=updated_gc$GCskew, y.range=c(-0.65,0.65),
              col="#252525", size=0.1)
mapping_shape(mos_pars=updated_pars, shape="l",
              panel=1, frag=updated_gc$SeqID,
              x=(updated_gc$startPosition+updated_gc$endPosition)/2,
              y=updated_gc$GCskew, y.range=c(-0.65,0.65),
              col="#525252", size=0.2)
data_plot <- updated_gene[which(updated_gene$Strand=="+"),]
data_plot <- data_plot[nrow(data_plot):1,]
mapping_shape(mos_pars=updated_pars, shape="a",
              panel=2, frag=data_plot$SeqID,
              x.st=data_plot$Start, x.en=data_plot$End,
              y.st=0.02, y.en=0.49, y.range=c(0,1),
              col="#FFFFFF", col.alpha=0.8, size=0.2,
              fill=data_plot$COGcolor, fill.alpha=0.8,
              arrow.size=500, arrow.type=1)
data_plot <- rev(updated_gene[which(updated_gene$Strand=="-"),])
mapping_shape(mos_pars=updated_pars, shape="a",
              panel=2, frag=data_plot$SeqID,
              x.st=data_plot$Start, x.en=data_plot$End,
              y.st=0.51, y.en=0.98, y.range=c(0,1),
              col="#FFFFFF", col.alpha=0.8, size=0.2,
              fill=data_plot$COGcolor, fill.alpha=0.8,
              arrow.size=-500, arrow.type=1)
mapping_marker(mos_pars=updated_pars, shape="r", panel=2, frag=1,
               x.st=data_marker$Start, x.en=data_marker$End,
               y.st=rep(0, nrow(data_marker)), y.en=rep(1, nrow(data_marker)),
               y.range=c(0,1), col="#A63603", col.alpha=0.95, fill.alpha=0, size=1)
mapping_marker(mos_pars=updated_pars, shape="p", panel=1, frag=1,
               x.st=c((672600+677200)/2, (2719000+2723800)/2, (3840000+3844600)/2),
               y.st=rep(0.5, 3), y.range=c(0,1), col="#A63603", col.alpha=0.95, fill.alpha=0, size=2, plwd=1)
mapping_marker(mos_pars=updated_pars, shape="t", panel=1, frag=1,
               x.st=c((672600+677200)/2, (2719000+2723800)/2, (3840000+3844600)/2),
               y.st=rep(0.5, 3), y.range=c(0,1), fill="#A63603", tri=c(-1,8,15))

mapping_text(mos_pars=updated_pars, panel=2, frag=1,
            x=(data_marker$Start+data_marker$End)/2, y=rep(-2, nrow(data_marker)), y.range=c(0,1), texts=data_marker$GI, angle=0, cex=1, col="red")
dev.off()
