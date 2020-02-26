\name{mapping_shape}
\alias{mapping_shape}
\title{Mapping Geometry Element on Spiral Graph}
\description{
Add geometry elements on spiral graph with given x-axis and y-axis coordinate by coordinate transformation. Geometry elements contain dot, line, bar and arrow-bar.
}
\usage{
mapping_shape(mos_pars=NULL, shape=NULL, panel=NULL, frag=NULL, x=NULL, x.st=NULL, x.en=NULL, y=NULL, y.st=NULL, y.en=NULL, y.range=NULL, col=NULL, col.alpha=NULL, fill=NULL, fill.alpha=NULL, size=NULL, arrow.size=NULL, arrow.type=-1, pch=19)
}
\arguments{
\item{mos_pars}{arguments list: generated in function load_parameters.}
\item{shape}{character: can be "point"/"p", "line"/"l", "bar"/"b", "arrow"/"a".}
\item{panel}{integer vector: panel ID (panel_y_height), please use function mapping_data convert panel name to panel ID.}
\item{frag}{integer vector: fragment ID (panel_x_frags), please use function mapping_data convert fragment name to fragment ID.}
\item{x}{numeric vector: x coordinates of points in the point and line plot.}
\item{x.st}{numeric vector: x coordinates of points in the bar and arrow plot.}
\item{x.en}{numeric vector: x coordinates of points in the bar and arrow plot.}
\item{y}{numeric vector: y coordinates of points in the point and line plot.}
\item{y.st}{numeric vector: y coordinates of points in the bar and arrow plot.}
\item{y.en}{numeric vector: y coordinates of points in the bar and arrow plot.}
\item{y.range}{vector contains two numeric elements: c(lower bound of data, upper bound of data).}
\item{col}{character (color) vector: color for point, line and border of bar and arrow.}
\item{col.alpha}{numeric vector: alpha for col.}
\item{fill}{character (color) vector: color to fill bar and arrow.}
\item{fill.alpha}{numeric vector: alpha for fill.}
\item{size}{numeric vector: size for point, line and border of bar and arrow.}
\item{arrow.size}{numeric: arrow size.}
\item{arrow.type}{numeric: arrow type, 1 refers to outer-arrow, -1 refers to inner-arrow.}
\item{pch}{integer: see in function 'points{graphics}'.}
}
\details{
If "point"/"p" or "line"/"l" is selected as 'shape', parameters 'panel', 'frag', 'x' and 'y' must be given. 'panel' and 'frag' can be a single integer or an array. 'x' and 'y' have to be equal in length. If 'panel' or 'frag' is a numeric array, it should be equal with 'x' and 'y' in length.


If "bar"/"b" is selected as 'shape', parameters 'panel', 'frag', 'x.st', 'x.en', 'y.st' and 'y.en' must be given. 'panel' and 'frag' can be a single integer or an array. 'y.st' and 'y.en' can be a single number or an array. 'x.st', 'x.en' have to be equal in length. If 'panel', 'frag', 'y.st' or 'y.en' is a numeric array, it should be equal with 'x.st' and 'x.en' in length.


If "arrow"/"a" is selected as 'shape', parameters 'panel', 'frag', 'x.st', 'x.en', 'y.st' and 'y.en' must be given. 'panel' and 'frag' can be a single integer or an array. 'y.st' or 'y.en' must be a number. 'x.st', 'x.en' have to be equal in length. If 'panel' or 'frag' is a numeric array, it should be equal with 'x.st' and 'x.en' in length. 'arrow.size' can be both positive and negative which assigns the direction of the arrow-bar.


'y.range' can be set as y.range=c(min,max). The data of y, y.st or y.en will be limited to 
min or max, if the value is out of the given range.


'col', 'col.alpha', 'fill' and 'fill.alpha' can be a single value or an array. If 'col', 'col.alpha', 'fill' or 'fill.alpha' is an array, it should be equal with your data in length.
}
\examples{
}
