\name{update_parameters}
\alias{update_parameters}
\title{Update Spiral Graph Parameters}
\description{
Check necessary parameters and update spiral coordinate.
}
\usage{
update_parameters(mos_pars=NULL)
}
\arguments{
\item{mos_pars}{arguments list: generated in function load_parameters.}
}
\examples{
library(MOSplot)
pars <- load_parameters(data_pars$data_pars, isFile=F)
updated_pars <- update_parameters(mos_pars=pars)

# You can compare str(updated_pars) with str(pars) to check the difference.
names(updated_pars)[which(!(names(updated_pars) %in% names(pars)))]
}