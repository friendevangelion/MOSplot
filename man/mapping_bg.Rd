\name{mapping_bg}
\alias{mapping_bg}
\title{Create a Spiral Canvas with Given Parameter List}
\description{
Create empty plot and add spiral background, x-axis, y-axis, tag and seperator with given spiral parameters loaded with function 'load_parameters'.
}
\usage{
mapping_bg(mos_pars=NULL)
}
\arguments{
\item{mos_pars}{arguments list: generated in function load_parameters.}
}
\examples{
library(MOSplot)
updated_pars <- update_parameters(mos_pars=pars)
pdf("demo.pdf")
# NOTE: only updated parameters can be used to plot background/shapes/markers
mapping_bg(updated_pars)
dev.off()
}
