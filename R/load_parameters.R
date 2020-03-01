#' Title
#'
#' @param file character: your input file contains all graph parameters.
#' @param what character (default is ""): see 'scan {base}'.
#' @param comment.char character (default is "#"): see 'scan {base}'.
#' @param sep character (default is "\\t"): separator between column in graph parameters file.
#' @param skip numeric (default is 0): see 'scan {base}'.
#' @param nline numeric (default is 0): see 'scan {base}'.
#'
#' @return a list
#' @export
#'
#' @examples
#'
load_parameters <- function(file="", what="", comment.char="#", sep="\t", quote = if(identical(sep, "\n")) "" else "'\"", skip=0, nlines=0) {
  ori_data <- scan(file=file, what=what, comment.char=comment.char, sep="\n", quote=quote, skip=skip, nlines=nlines)
  tmp_data <- strsplit(ori_data, split=sep)
  mos_pars <- list()
  mos_pars <- lapply(tmp_data, tail, n=-1)
  names(mos_pars) <- lapply(tmp_data, head, n=1)
  # basis setting
  if ("global_circ_num" %in% names(mos_pars)) { mos_pars$global_circ_num = as.numeric(mos_pars$global_circ_num) }
  if ("global_init_angle" %in% names(mos_pars)) { mos_pars$global_init_angle = as.numeric(mos_pars$global_init_angle) }
  if ("global_start_angle" %in% names(mos_pars)) { mos_pars$global_start_angle = as.numeric(mos_pars$global_start_angle) }
  if ("global_h_zoom" %in% names(mos_pars)) { mos_pars$global_h_zoom = as.numeric(mos_pars$global_h_zoom) }
  if ("global_h_total" %in% names(mos_pars)) { mos_pars$global_h_total = as.numeric(mos_pars$global_h_total) }
  if ("global_directory" %in% names(mos_pars)) { mos_pars$global_directory = as.numeric(mos_pars$global_directory) }
  # panel setting
  if ("tag_data_arc" %in% names(mos_pars)) { mos_pars$tag_data_arc = as.numeric(mos_pars$tag_data_arc) }
  if ("tag_data" %in% names(mos_pars)) { if (mos_pars$tag_data=="NULL") { mos_pars$tag_data = NULL } else { mos_pars$tag_data = unlist(strsplit(mos_pars$tag_data,split=",")) } }
  if ("tag_data_col" %in% names(mos_pars)) { mos_pars$tag_data_col = paste("#",mos_pars$tag_data_col,sep="") }
  if ("tag_data_size" %in% names(mos_pars)) { mos_pars$tag_data_size = as.numeric(mos_pars$tag_data_size) }
  if ("tag_data_offrate" %in% names(mos_pars)) { mos_pars$tag_data_offrate = as.numeric(mos_pars$tag_data_offrate) }
  if ("panel_x_frag" %in% names(mos_pars)) { mos_pars$panel_x_frag = as.numeric(unlist(strsplit(mos_pars$panel_x_frag,split=","))) }
  if ("panel_x_texture" %in% names(mos_pars)) { mos_pars$panel_x_texture = as.numeric(mos_pars$panel_x_texture) }
  if ("panel_x_block" %in% names(mos_pars)) { mos_pars$panel_x_block = as.numeric(mos_pars$panel_x_block) }
  if ("panel_x_blank" %in% names(mos_pars)) { mos_pars$panel_x_blank = as.numeric(mos_pars$panel_x_blank) }
  if ("panel_y_height" %in% names(mos_pars)) { mos_pars$panel_y_height = as.numeric(unlist(strsplit(mos_pars$panel_y_height,split=","))) }
  if ("panel_y_blank" %in% names(mos_pars)) { mos_pars$panel_y_blank = as.numeric(mos_pars$panel_y_blank) }
  if ("panel_grid_flag" %in% names(mos_pars)) { if (mos_pars$panel_grid_flag=="NULL") { mos_pars$panel_grid_flag = NULL } else { mos_pars$panel_grid_flag = as.numeric(unlist(strsplit(mos_pars$panel_grid_flag,split=","))) } }
  if ("panel_grid_sub_flag" %in% names(mos_pars)) { if (mos_pars$panel_grid_sub_flag=="NULL") { mos_pars$panel_grid_sub_flag = NULL } else { mos_pars$panel_grid_sub_flag = as.numeric(unlist(strsplit(mos_pars$panel_grid_sub_flag,split=","))) } }
  if ("panel_color" %in% names(mos_pars)) { if (mos_pars$panel_color=="NULL") { mos_pars$panel_color = NULL } else { mos_pars$panel_color = paste("#",mos_pars$panel_color,sep="") } }
  if ("panel_border_color" %in% names(mos_pars)) { if (mos_pars$panel_border_color=="NULL") { mos_pars$panel_border_color = NULL } else { mos_pars$panel_border_color = paste("#",mos_pars$panel_border_color,sep="") } }
  if ("panel_border_size" %in% names(mos_pars)) { mos_pars$panel_border_size = as.numeric(mos_pars$panel_border_size) }
  if ("grid_y_color" %in% names(mos_pars)) { mos_pars$grid_y_color = paste("#",mos_pars$grid_y_color,sep="") }
  if ("grid_y_width" %in% names(mos_pars)) { mos_pars$grid_y_width = as.numeric(mos_pars$grid_y_width) }
  if ("grid_x_color" %in% names(mos_pars)) { mos_pars$grid_x_color = paste("#",mos_pars$grid_x_color,sep="") }
  if ("grid_x_width" %in% names(mos_pars)) { mos_pars$grid_x_width = as.numeric(mos_pars$grid_x_width) }
  # x tag setting
  if ("tag_x_arc" %in% names(mos_pars)) { mos_pars$tag_x_arc = as.numeric(mos_pars$tag_x_arc) }
  if ("tag_x_size" %in% names(mos_pars)) { mos_pars$tag_x_size = as.numeric(mos_pars$tag_x_size) }
  if ("tag_x_col" %in% names(mos_pars)) { mos_pars$tag_x_col = paste("#",mos_pars$tag_x_col,sep="") }
  if ("tag_x_offsize" %in% names(mos_pars)) { mos_pars$tag_x_offsize = as.numeric(mos_pars$tag_x_offsize) }
  if ("tag_x_span" %in% names(mos_pars)) { mos_pars$tag_x_span = as.numeric(mos_pars$tag_x_span) }
  if ("tag_x_digit" %in% names(mos_pars)) { mos_pars$tag_x_digit = as.numeric(mos_pars$tag_x_digit) }
  if ("tag_x_list" %in% names(mos_pars)) { if (mos_pars$tag_x_list=="NULL") { mos_pars$tag_x_list = NULL } else { mos_pars$tag_x_list = unlist(strsplit(mos_pars$tag_x_list,split=",")) } }
  if ("tag_x_pos" %in% names(mos_pars)) { if (mos_pars$tag_x_pos=="NULL") { mos_pars$tag_x_pos = NULL } else { mos_pars$tag_x_pos = unlist(strsplit(mos_pars$tag_x_pos,split=",")) } }
  if ("tag_x_add" %in% names(mos_pars)) { mos_pars$tag_x_add = as.numeric(mos_pars$tag_x_add) }
  if ("tag_x_init" %in% names(mos_pars)) { mos_pars$tag_x_init = unlist(strsplit(mos_pars$tag_x_init,split=",")) }
  if ("tag_x_unit" %in% names(mos_pars)) { mos_pars$tag_x_unit = as.character(mos_pars$tag_x_unit) }
  # y tag setting
  if ("tag_y_arc" %in% names(mos_pars)) { mos_pars$tag_y_arc = as.numeric(mos_pars$tag_y_arc) }
  if ("tag_y_size" %in% names(mos_pars)) { mos_pars$tag_y_size = as.numeric(mos_pars$tag_y_size) }
  if ("tag_y_offsize" %in% names(mos_pars)) { mos_pars$tag_y_offsize = as.numeric(mos_pars$tag_y_offsize) }
  if ("tag_y_col" %in% names(mos_pars)) { mos_pars$tag_y_col = paste("#",mos_pars$tag_y_col,sep="") }
  if ("tag_y_zoom" %in% names(mos_pars)) { mos_pars$tag_y_zoom = as.numeric(mos_pars$tag_y_zoom) }
  if ("tag_y_st" %in% names(mos_pars)) { if (mos_pars$tag_y_st=="NULL") { mos_pars$tag_y_st = NULL } else { mos_pars$tag_y_st = unlist(strsplit(mos_pars$tag_y_st,split=",")) } }
  if ("tag_y_mid" %in% names(mos_pars)) { if (mos_pars$tag_y_mid=="NULL") { mos_pars$tag_y_mid = NULL } else { mos_pars$tag_y_mid = unlist(strsplit(mos_pars$tag_y_mid,split=",")) } }
  if ("tag_y_en" %in% names(mos_pars)) { if (mos_pars$tag_y_en=="NULL") { mos_pars$tag_y_en = NULL } else { mos_pars$tag_y_en = unlist(strsplit(mos_pars$tag_y_en,split=",")) } }
  # other setting
  if ("point_span" %in% names(mos_pars)) { mos_pars$point_span = as.numeric(mos_pars$point_span) }
  if ("point_size" %in% names(mos_pars)) { mos_pars$point_size = as.numeric(mos_pars$point_size) }
  if ("point_col" %in% names(mos_pars)) { mos_pars$point_col = paste("#",mos_pars$point_col,sep="") }
  if ("point_alpha" %in% names(mos_pars)) { mos_pars$point_alpha = as.numeric(mos_pars$point_alpha) }
  if ("line_span" %in% names(mos_pars)) { mos_pars$line_span = as.numeric(mos_pars$line_span) }
  if ("line_size" %in% names(mos_pars)) { mos_pars$line_size = as.numeric(mos_pars$line_size) }
  if ("line_col" %in% names(mos_pars)) { mos_pars$line_col = paste("#",mos_pars$line_col,sep="") }
  if ("line_alpha" %in% names(mos_pars)) { mos_pars$line_alpha = as.numeric(mos_pars$line_alpha) }
  if ("bar_span" %in% names(mos_pars)) { mos_pars$bar_span = as.numeric(mos_pars$bar_span) }
  if ("bar_line_col" %in% names(mos_pars)) { if (mos_pars$bar_line_col=="NULL") { mos_pars$bar_line_col = NULL } else { mos_pars$bar_line_col = paste("#",mos_pars$bar_line_col,sep="") } }
  if ("bar_line_alpha" %in% names(mos_pars)) { mos_pars$bar_line_alpha = as.numeric(mos_pars$bar_line_alpha) }
  if ("bar_line_size" %in% names(mos_pars)) { mos_pars$bar_line_size = as.numeric(mos_pars$bar_line_size) }
  if ("bar_fill_col" %in% names(mos_pars)) { if (mos_pars$bar_fill_col=="NULL") { mos_pars$bar_fill_col = NULL } else { mos_pars$bar_fill_col = paste("#",mos_pars$bar_fill_col,sep="") } }
  if ("bar_fill_alpha" %in% names(mos_pars)) { mos_pars$bar_fill_alpha = as.numeric(mos_pars$bar_fill_alpha) }
  ### validation ###
  error_flag = 0
  #
  if (("global_circ_num" %in% names(mos_pars)) & ("global_init_angle" %in% names(mos_pars))) {
    if (mos_pars$global_circ_num<0 & mos_pars$global_init_angle<0) { error_flag = 1; warning("Both global_circ_num & global_init_angle have not been set!") }
  } else { error_flag = 1; warning('Both global_circ_num & global_init_angle are not illegal!') }
  if (!("global_init_angle" %in% names(mos_pars))) { mos_pars$global_init_angle = -1 } else { if (mos_pars$global_init_angle<1e-6 & mos_pars$global_init_angle>0) { warning("global_init_angle is better larger than 0.000001") } }
  if (!("global_start_angle" %in% names(mos_pars))) { mos_pars$global_start_angle = 1035 }
  if (("global_h_zoom" %in% names(mos_pars)) & ("global_h_total" %in% names(mos_pars))) {
    if (mos_pars$global_h_zoom<0 & mos_pars$global_h_total<0) { error_flag = 1; warning("Both global_h_zoom & global_h_total have not been set!") }
  } else { error_flag = 1; warning('Both global_h_zoom & global_h_total are not illegal!') }
  if (!("global_directory" %in% names(mos_pars))) { mos_pars$global_directory = 1 }
  #
  if (!("tag_data_arc" %in% names(mos_pars))) { mos_pars$tag_data_arc = 1 }
  if (!("tag_data" %in% names(mos_pars))) { mos_pars$tag_data = NULL }
  if (!("tag_data_col" %in% names(mos_pars))) { mos_pars$tag_data_col = "#999999" }
  if (!("tag_data_size" %in% names(mos_pars))) { mos_pars$tag_data_size = 1.2 }
  if (!("tag_data_offrate" %in% names(mos_pars))) { mos_pars$tag_data_offrate = 0.8 }
  if (!("panel_x_frag" %in% names(mos_pars))) { error_flag = 1; warning("panel_x_frag has not been set!") } else { if (length(mos_pars$panel_x_frag)<0) { warning("panel_x_frag is not illegal!") } }
  if (!("panel_x_texture" %in% names(mos_pars))) { mos_pars$panel_x_texture = NULL }
  if (!("panel_x_block" %in% names(mos_pars))) { mos_pars$panel_x_block = NULL }
  if (!("panel_x_blank" %in% names(mos_pars))) { mos_pars$panel_x_blank = 0 }
  if (!("panel_y_height" %in% names(mos_pars))) { error_flag = 1; warning("panel_y_height has not been set!") } else { if (length(mos_pars$panel_y_height)<0) { warning("panel_y_height is not illegal!") } }
  if (!("panel_y_blank" %in% names(mos_pars))) { mos_pars$panel_x_blank = 0.5 }
  if (!("panel_grid_flag" %in% names(mos_pars))) { if (("panel_y_height" %in% names(mos_pars))) { mos_pars$panel_grid_flag = rep(0, length(mos_pars$panel_y_height%in% names(mos_pars))) } }
  if (!("panel_grid_sub_flag" %in% names(mos_pars))) { if (("panel_y_height" %in% names(mos_pars))) { mos_pars$panel_grid_sub_flag = rep(0, length(mos_pars$panel_y_height%in% names(mos_pars))) } }
  if (!("panel_color" %in% names(mos_pars))) { mos_pars$panel_x_block = "#BDBDBD" }
  if (!("panel_border_color" %in% names(mos_pars))) { mos_pars$panel_border_color = "#999999" }
  if (!("panel_border_size" %in% names(mos_pars))) { mos_pars$panel_border_size = 0.4 }
  if (!("grid_y_color" %in% names(mos_pars))) { mos_pars$grid_y_color = "#E6E6E6" }
  if (!("grid_y_width" %in% names(mos_pars))) { mos_pars$grid_y_width = 0.1 }
  if (!("grid_x_color" %in% names(mos_pars))) { mos_pars$grid_x_color = "#E6E6E6" }
  if (!("grid_x_width" %in% names(mos_pars))) { mos_pars$grid_x_width = 0.1 }
  if (!is.null(mos_pars$panel_x_texture)) { mos_pars$panel_x_texture = sum(mos_pars$panel_x_frag)/100 }
  if (!is.null(mos_pars$panel_x_block)) { mos_pars$panel_x_block = 10^floor(log(mos_pars$panel_x_texture, base=10))*4 }
  if (!is.null(mos_pars$tag_data)) {
    if (length(mos_pars$tag_data)!=length(mos_pars$panel_x_frag)) { warning("tag_data is not in same length with panel_x_frag, tag_data will be set as NULL!"); mos_pars$tag_data = NULL; }
  }
  if (("panel_y_height" %in% names(mos_pars))) {
    if (("panel_grid_flag" %in% names(mos_pars))) { if (length(mos_pars$panel_y_height)!=length(mos_pars$panel_grid_flag) & mode(mos_pars$panel_grid_flag)!="logical") { error_flag = 1; warning("panel_y_height & panel_grid_flag are not in same length!") } }
    if (("panel_grid_sub_flag" %in% names(mos_pars))) { if (length(mos_pars$panel_y_height)!=length(mos_pars$panel_grid_sub_flag) & mode(mos_pars$panel_grid_sub_flag)!="logical") { error_flag = 1; warning("panel_y_height & panel_grid_sub_flag are not in same length!") } }
  }
  #
  if (!("tag_x_arc" %in% names(mos_pars))) { mos_pars$tag_x_arc = 1 }
  if (!("tag_x_size" %in% names(mos_pars))) { mos_pars$tag_x_size = 1.8 }
  if (!("tag_x_col" %in% names(mos_pars))) { mos_pars$tag_x_col = "#999999" }
  if (!("tag_x_offsize" %in% names(mos_pars))) { mos_pars$tag_x_offsize = 4 }
  if (!("tag_x_span" %in% names(mos_pars))) { mos_pars$tag_x_span = 5 }
  if (!("tag_x_add" %in% names(mos_pars))) { mos_pars$tag_x_add = 0 }
  if (!("tag_x_init" %in% names(mos_pars))) { if (length(mos_pars$panel_x_frag)>0) { mos_pars$tag_x_init = rep(0, length(mos_pars$panel_x_frag)) } }
  if (!("tag_x_digit" %in% names(mos_pars))) { mos_pars$tag_x_digit = 0 }
  if (!("tag_x_unit" %in% names(mos_pars))) { mos_pars$tag_x_unit = "" }
  if (("tag_x_list" %in% names(mos_pars)) & ("tag_x_pos" %in% names(mos_pars))) {
    if (length(mos_pars$tag_x_list)!=length(mos_pars$tag_x_pos)) {
      error_flag = 1; warning("tag_x_list & tag_x_pos are not in same length!")
    }
  }
  #
  if (!("tag_y_arc" %in% names(mos_pars))) { mos_pars$tag_y_arc = 1 }
  if (!("tag_y_size" %in% names(mos_pars))) { mos_pars$tag_y_size = 1 }
  if (!("tag_y_offsize" %in% names(mos_pars))) { mos_pars$tag_y_offsize = 0 }
  if (!("tag_y_col" %in% names(mos_pars))) { mos_pars$tag_y_col = "#999999" }
  if (!("tag_y_st" %in% names(mos_pars))) { mos_pars$tag_y_st = NULL }
  if (!("tag_y_mid" %in% names(mos_pars))) { mos_pars$tag_y_mid = NULL }
  if (!("tag_y_en" %in% names(mos_pars))) { mos_pars$tag_y_en= NULL }
  if (("panel_y_height" %in% names(mos_pars))) {
    if (("tag_y_st" %in% names(mos_pars))) { if (length(mos_pars$panel_y_height)!=length(mos_pars$tag_y_st) & mode(mos_pars$tag_y_st)!="logical") { error_flag = 1; warning("panel_y_height & tag_y_st are not in same length!") } }
    if (("tag_y_mid" %in% names(mos_pars))) { if (length(mos_pars$panel_y_height)!=length(mos_pars$tag_y_mid) & mode(mos_pars$tag_y_mid)!="logical") { error_flag = 1; warning("panel_y_height & tag_y_mid are not in same length!") } }
    if (("tag_y_en" %in% names(mos_pars))) { if (length(mos_pars$panel_y_height)!=length(mos_pars$tag_y_en) & mode(mos_pars$tag_y_en)!="logical") { error_flag = 1; warning("panel_y_height & tag_y_mid are not in same length!") } }
  }
  #
  if (!("point_span" %in% names(mos_pars))) { mos_pars$point_span = 20 }
  if (!("point_size" %in% names(mos_pars))) { mos_pars$point_size = 0.01 }
  if (!("point_col" %in% names(mos_pars))) { mos_pars$point_col = "#000000" }
  if (!("point_alpha" %in% names(mos_pars))) { mos_pars$point_alpha = 0.9 }
  if (!("line_span" %in% names(mos_pars))) { mos_pars$line_span = 4 }
  if (!("line_size" %in% names(mos_pars))) { mos_pars$line_size = 0.1 }
  if (!("line_col" %in% names(mos_pars))) { mos_pars$line_col = "#000000" }
  if (!("line_alpha" %in% names(mos_pars))) { mos_pars$line_alpha = 0.9 }
  if (!("bar_span" %in% names(mos_pars))) { mos_pars$bar_span = 1 }
  if (!("bar_line_col" %in% names(mos_pars))) { mos_pars$bar_line_col = NULL }
  if (!("bar_line_alpha" %in% names(mos_pars))) { mos_pars$bar_line_alpha = 0.8 }
  if (!("bar_line_size" %in% names(mos_pars))) { mos_pars$bar_line_size = 0.1 }
  if (!("bar_fill_col" %in% names(mos_pars))) { mos_pars$bar_fill_col = "#525252" }
  if (!("bar_fill_alpha" %in% names(mos_pars))) { mos_pars$bar_fill_alpha = 0.6 }
  #
  if (!error_flag) {
    pi = 3.1415926536
    mos_pars$validate_level = 1
    # global_h_total validation
    if (mos_pars$global_h_total<0) {
      mos_pars$global_h_total = sum(mos_pars$panel_y_height)+(length(mos_pars$panel_y_height)-1)*mos_pars$panel_y_blank
    } else if (sum(mos_pars$panel_y_height)+(length(mos_pars$panel_y_height)-1)*mos_pars$panel_y_blank!=mos_pars$global_h_total) {
      warning ("Total height of plot setting is different with panel height setting!")
    }
    # global_h_zoom validation
    if (2*pi*mos_pars$global_h_zoom<mos_pars$global_h_total) {
      warning ("2*pi*global_h_zoom should be larger than global_h_total!")
      mos_pars$global_h_zoom = floor((sum(mos_pars$panel_y_height)+(length(mos_pars$panel_y_height)-1)*mos_pars$panel_y_blank)/2/pi)+1
      show (paste("global_h_zoom is modified to ",mos_pars$global_h_zoom,"!", sep=""))
    }
    # delta_angle setting
    if (mos_pars$global_circ_num>0) {
      mos_pars$global_init_angle = 180
      delta_angle = 180
      total_data = sum(mos_pars$panel_x_frag) + length(mos_pars$panel_x_frag)*mos_pars$panel_x_blank - mos_pars$panel_x_blank
      spiral_length = integrate(function(x)sqrt(x^2+1),mos_pars$global_start_angle/180*pi,(mos_pars$global_start_angle+mos_pars$global_circ_num*360)/180*pi)$value
      init_length = integrate(function(x)sqrt(x^2+1),mos_pars$global_start_angle/180*pi,(mos_pars$global_start_angle+mos_pars$global_init_angle)/180*pi)$value
      while (spiral_length/init_length<total_data | spiral_length/init_length>total_data*1.05) {
        init_length = integrate(function(x)sqrt(x^2+1),mos_pars$global_start_angle/180*pi,(mos_pars$global_start_angle+mos_pars$global_init_angle)/180*pi)$value
        if (spiral_length/init_length<total_data) { mos_pars$global_init_angle = mos_pars$global_init_angle - delta_angle/2 }
        else if (spiral_length/init_length>total_data*1.05) { mos_pars$global_init_angle = mos_pars$global_init_angle + delta_angle/2 }
        delta_angle = delta_angle/2
      }
      show (paste("global_init_angle had been reset to ", mos_pars$global_init_angle, "!", sep=""))
      if (mos_pars$global_init_angle<1e-6) { show ("global_init_angle is better larger than 0.000001, please enlarge global_circ_num!") }
    }
    # panel_st panel_en frag_st frag_en setting
    mos_pars$panel_coord_auto = 1
    if (mos_pars$panel_coord_auto) {
      tri = matrix(0, length(mos_pars$panel_y_height), length(mos_pars$panel_y_height))
      tri[lower.tri(tri)] = 1
      mos_pars$panel_st = as.vector(tri %*% mos_pars$panel_y_height) + seq(0,length(mos_pars$panel_y_height)-1)*mos_pars$panel_y_blank
      tri = matrix(1, length(mos_pars$panel_y_height), length(mos_pars$panel_y_height))
      tri[upper.tri(tri)] = 0
      mos_pars$panel_en = as.vector(tri %*% mos_pars$panel_y_height) + seq(0,length(mos_pars$panel_y_height)-1)*mos_pars$panel_y_blank
    }
    mos_pars$block_coord_auto = 1
    if (mos_pars$block_coord_auto) {
      tri = matrix(0, length(mos_pars$panel_x_frag), length(mos_pars$panel_x_frag))
      tri[lower.tri(tri)] = 1
      mos_pars$frag_st = as.vector(tri %*% mos_pars$panel_x_frag) + seq(0,length(mos_pars$panel_x_frag)-1)*mos_pars$panel_x_blank + 1
      tri = matrix(1, length(mos_pars$panel_x_frag), length(mos_pars$panel_x_frag))
      tri[upper.tri(tri)] = 0
      mos_pars$frag_en = as.vector(tri %*% mos_pars$panel_x_frag) + seq(0,length(mos_pars$panel_x_frag)-1)*mos_pars$panel_x_blank + 1
    }
    return (mos_pars)
  } else {
    return (list())
  }
}
