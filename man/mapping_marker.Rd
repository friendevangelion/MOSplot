\name{mapping_marker}
\alias{mapping_marker}
\title{Mapping Marker on Spiral Graph}
\description{
Add marker on spiral graph with given x-axis and y-axis coordinate by coordinate transformation. Markers contain point, cover region, shade and triangle on x-axis.
}
\usage{
mapping_marker(mos_pars=NULL, shape=NULL, panel=NULL, frag=NULL, x.st=NULL, x.en=NULL, y.st=NULL, y.en=NULL, y.range=NULL, col=NULL, col.alpha=NULL, fill=NULL, fill.alpha=NULL, size=NULL, others=NULL, tri=NULL, plwd=0.1, pch=21)
}
\arguments{
\item{mos_pars}{arguments list: generated in function load_parameters.}
\item{shape}{character: can be "point"/"p", "region"/"r", "shade"/"s", "triangle"/"t".}
\item{x.st}{numeric vector: x coordinates.}
\item{x.en}{numeric vector: x coordinates.}
\item{y.st}{numeric vector: y coordinates.}
\item{y.en}{numeric vector: y coordinates.}
\item{y.range}{vector contains two numeric elements: c(lower bound of data, upper bound of data).}
\item{col}{character (color) vector: color for point, line and border of bar and arrow.}
\item{col.alpha}{numeric vector: alpha for col.}
\item{fill}{character (color) vector: color to fill bar and arrow.}
\item{fill.alpha}{numeric vector: alpha for fill.}
\item{size}{numeric vector: size for point, line and border of bar and arrow.}
\item{tri}{numeric vector: contains arrow position, arrow length and arrow angle.}
\item{plwd}{numeric: see 'lwd' in function 'points{graphics}'.}
\item{pch}{integer: see in function 'points{graphics}'.}
}
\details{
If "point"/"p" is selected as 'shape', parameters 'panel', 'frag', 'x.st' and 'y.st' must be given. 'panel' and 'frag' can be a single integer or an array. 'x.st' and 'y.st' have to be equal in length. If 'panel' or 'frag' is a numeric array, it should be equal with 'x.st' and 'y.st' in length.


If "region"/"r" is selected as 'shape', parameters 'panel', 'frag', 'x.st', 'x.en', 'y.st' and 'y.en' must be given. 'panel' and 'frag' can be a single integer or an array. 'y.st' and 'y.en' can be a single number or an array. 'x.st', 'x.en' have to be equal in length. If 'panel', 'frag', 'y.st' or 'y.en' is a numeric array, it should be equal with 'x.st' and 'x.en' in length.


If "shade"/"s" is selected as 'shape', parameters 'panel', 'frag', 'x.st' and 'x.en' must be given. 'panel' and 'frag' can be a single integer or an array. 'x.st', 'x.en' have to be equal in length. If 'panel' or 'frag' is a numeric array, it should be equal with 'x.st' and 'x.en' in length.


If "triangle"/"t" is selected as 'shape', parameters 'panel', 'frag' and 'x.st' must be given. 'panel' and 'frag' can be a single integer or an array. 'If 'panel' or 'frag' is a numeric array, it should be equal with 'x.st' in length. 'tri' is a numeric vector contains three element. For example, tri=c(1,6,15), in which, 1 means triangle marker is at the top of the panels, 6 means the total height of triangle is 6 and 15 means the degree of the triangle is 15*2 = 30 degree.


'y.range' can be set as y.range=c(min,max). The data of y, y.st or y.en will be limited to min or max, if the value is out of the given range.


'col', 'col.alpha', 'fill' and 'fill.alpha' can be a single value or an array. If 'col', 'col.alpha', 'fill' or 'fill.alpha' is an array, it should be equal with your data in length.
}
\examples{
updated_pars <- update_parameters(data_pars)
mapping_bg(updated_pars)
updated_gc <- mapping_data(data=data_gc, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))
updated_gene <- mapping_data(data=data_gene, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))
updated_gene <- mapping_data(data=updated_gene, col_name="COGcolor", ori_tag=cog_color$categories, mapping_tag=cog_color$hex, other_tag="#BBBBBB")
col_plot <- rep("#525252", nrow(updated_gc))
col_plot[which(updated_gc$GCcontent<0.37)] <- "#969696"
pdf("demo.pdf")
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
dev.off()
}
