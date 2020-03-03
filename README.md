# MOSplot
A Package For Creating MOSplot (mosquito coil plot) / Spiral Graph

## INSTALLATION
All 'MOSplot' resources are uploaded to https://github.com/friendevangelion/MOSplot. You can obtain package 'MOSplot' from github:

```
# install.packages("devtools")
library("devtools")
devtools::install_github("friendevangelion/MOSplot")
```

or

```
# install.packages("remotes")
library("remotes")
install_github("friendevangelion/MOSplot")
```

## USAGE
The detail usage is contained in the manual file. Or you can simply check help message of all the functions in 'MOSplot' by using '?function()' in R.

```
mapping_data()        Convert Data to Other Corresponding Value
load_parameters()     Load Spiral Graph Parameters
update_parameters()   Update Spiral Graph Parameters
mapping_bg()          Create a Spiral Canvas with Given Parameter List
mapping_shape()       Mapping Geometry Element on Spiral Graph
mapping_marker()      Mapping Marker on Spiral Graph
```

## FAQ 

Here are some questions on the installation of MOSplot: 

Q1. Error caused by dependency 'plotrix'.

```
ERROR: dependency 'plotrix' is not available for package 'MOSplot'
* removing 'XXX/R-X.X.X/library/MOSplot'
In R CMD INSTALL
Error: Failed to install 'MOSplot' from GitHub:
```

A1. MOSplot requires only package 'plotrix' for plotting bending text. This error caused by the your ancient R. You can upgrade your R or install antique plotrix :)

```
# check the plotrix old version list and select one matched with your R version
library("remotes")
install_version("plotrix", version = "3.7",repos = "http://cran.us.r-project.org")
```

## NEWS

### version 1.1.0

Update test dataset in Rdata/Rda format.

Add a function update_parameters().

Increase some check points.

### version 1.0.0

Initial version.
