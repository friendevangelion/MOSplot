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
updated_pars <- update_parameters(mos_pars=data_pars)

# You can compare str(updated_pars) with str(pars) to check the difference.
names(updated_pars)[which(!(names(updated_pars) %in% names(pars)))]
}