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
hello()
}
