\name{load_parameters}
\alias{load_parameters}
\title{Load Spiral Graph Parameters}
\description{
Load spiral graph parameters including graph layout setting, panel setting, grid setting, tag setting and geometric element setting from a config file. This file contains two columns with TAB separator - first is parameter name and second is parameter value. 
}
\usage{
load_parameters(file="", what="", comment.char="#", sep="\t", quote = if(identical(sep, "\n")) "" else "'\"", skip=0, nlines=0)
}
\arguments{
\item{file}{character: your input file contains all graph parameters.}
\item{what}{character (default is ""): see 'scan {base}'.}
\item{comment.char}{character (default is "#"): see 'scan {base}'.}
\item{sep}{character (default is "\\t"): separator between column in graph parameters file.}
\item{skip}{numeric (default is 0): see 'scan {base}'.}
\item{nline}{numeric (default is 0): see 'scan {base}'.}
}
\details{
All the parameters can be seen in the demo test of this package.

Here is a list of important parameters:



(basis setting)

global_circ_num (integer) # the number of coils you want to display

global_init_angle (numeric) # inital angle, set if global_circ_num is empty, -1 means empty

global_start_angle (numeric) # start angle

global_h_zoom (numeric) # figure zoom, 2*pi*global_h_zoom>global_h_total

global_h_total (numeric) # height for plot, -1 means empty

global_directory (boolean) # 1 means inside is max and 0 means outside is max



(panel setting)

tag_data_arc (boolean) # whether data tag is bended

tag_data (character array) # panel tag, NULL means empty

tag_data_col (character color) # panel tag color

tag_data_size (numeric) # panel tag size

tag_data_offrate (numeric) # distance between panel to tag

panel_x_frag (numeric array) # data points of every panel

panel_x_texture (integer) # data points to draw background

panel_x_block (integer) # data points per block in panel

panel_x_blank (integer) # data points between panels

panel_y_height (numeric array) # panel height

panel_y_blank (numeric) # blank between panels

panel_grid_flag (numeric array) # panel grid, NULL means empty

panel_grid_sub_flag (numeric array) # panel sub grid, NULL means empty

panel_color (character color) # panel background color

panel_border_color (character color) # panel background border color

panel_border_size (numeric) # panel background border size

grid_y_color (character color) # panel grid y color

grid_y_width (numeric) # panel grid y width

grid_x_color (character color) # block grid x color

grid_x_width (numeric) # block grid x width



(x tag setting)

tag_x_arc (boolean) # whether x tag is bended

tag_x_size (numeric) # x tag size

tag_x_col (character color) # x tag color

tag_x_offsize (numeric) # distance for x tag

tag_x_span (numeric) # point displayed per tag_x_span

tag_x_list (character array) # x tag list, NULL means empty

tag_x_pos (integer array) # x tag position, NULL means empty

tag_x_add (numeric) # set if tag_x_list and tag_x_pos is empty

tag_x_digit (integer) # use scientific notation, set if tag_x_list and tag_x_pos is empty

tag_x_unit (character) # set if tag_x_list and tag_x_pos is empty



(y tag setting)

tag_y_arc (boolean) # whether y tag is bended

tag_y_size (numeric) # y tag size

tag_y_offsize (numeric) # distance for y tag

tag_y_col (character color) # y tag color

tag_y_zoom (numeric) # y tag zoom ratio

tag_y_st (character array) # y tag list

tag_y_mid (character array) # y tag list

tag_y_en (character array) # y tag list



(other setting)

point_span (integer) # point displayed per point_span

point_size (numeric) 

point_col (character color) 

point_alpha (numeric) 

line_span (integer) 

line_size (numeric) 

line_col (character color) 

line_alpha (numeric) 

bar_span (integer) 

bar_line_col (character color) 

bar_line_alpha (numeric) 

bar_line_size (numeric) 

bar_fill_col (character color) 

bar_fill_alpha (numeric)
}
\examples{
library(MOSplot)
# Please set your working directory to 'data_raw' of MOSplot.
data_pars <- load_parameters(file="CP003879.pars.txt")

# You can use str(data_pars) to see the data structure.
str(data_pars)

# After your graph parameters are loaded without errors,
# graph parameters should be updated with function 'update_parameters'.
}
