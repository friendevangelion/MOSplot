#' Title
#'
#' @param mos_pars arguments list: generated in function load_parameters.
#'
#' @return a list
#' @export
#'
#' @examples
#'
update_parameters <- function(mos_pars=NULL, baseline_init=T) {
  if (is.null(mos_pars)) {
    stop ("mos_pars is NULL!")
  }
  # basis setting
  if (!("global_init_angle" %in% names(mos_pars))) { stop ("Error found in global_init_angle! Please recheck global_circ_num, global_init_angle and global_start_angle and run load_parameters again!") }
  if (!("global_start_angle" %in% names(mos_pars))) { stop ("Error found in global_start_angle! Please recheck global_start_angle and run load_parameters again!") }
  if (!("global_h_zoom" %in% names(mos_pars))) { stop ("Error found in global_h_zoom! Please recheck global_h_zoom and run load_parameters again!") }
  if (!("global_h_total" %in% names(mos_pars))) { stop ("Error found in global_h_total! Please recheck global_h_total and run load_parameters again!") }
  if (!("global_directory" %in% names(mos_pars))) { stop ("Error found in global_directory! Please recheck global_directory and run load_parameters again!") }
  # panel setting
  if (!("tag_data_arc" %in% names(mos_pars))) { stop ("Error found in tag_data_arc! Please recheck tag_data_arc and run load_parameters again!") }
  if (!("panel_x_frag" %in% names(mos_pars))) { stop ("Error found in panel_x_frag! Please recheck panel_x_frag and run load_parameters again!") }
  if (!("panel_x_texture" %in% names(mos_pars))) { stop ("Error found in panel_x_texture! Please recheck panel_x_texture and run load_parameters again!") }
  if (!("panel_x_block" %in% names(mos_pars))) { stop ("Error found in panel_x_block! Please recheck panel_x_block and run load_parameters again!") }
  if (!("panel_x_blank" %in% names(mos_pars))) { stop ("Error found in panel_x_blank! Please recheck panel_x_blank and run load_parameters again!") }
  if (!("panel_y_height" %in% names(mos_pars))) { stop ("Error found in panel_y_height! Please recheck panel_y_height and run load_parameters again!") }
  if (!("panel_y_blank" %in% names(mos_pars))) { stop ("Error found in panel_y_blank! Please recheck panel_y_blank and run load_parameters again!") }
  if (!("panel_grid_flag" %in% names(mos_pars))) { stop ("Error found in panel_grid_flag! Please recheck panel_grid_flag and run load_parameters again!") }
  if (!("panel_grid_sub_flag" %in% names(mos_pars))) { stop ("Error found in panel_grid_sub_flag! Please recheck panel_grid_sub_flag and run load_parameters again!") }
  if (!("panel_color" %in% names(mos_pars))) { stop ("Error found in panel_color! Please recheck panel_color and run load_parameters again!") }
  if (!("panel_border_color" %in% names(mos_pars))) { stop ("Error found in panel_border_color! Please recheck panel_border_color and run load_parameters again!") }
  if (!("panel_border_size" %in% names(mos_pars))) { stop ("Error found in panel_border_size! Please recheck panel_border_size and run load_parameters again!") }
  if (!("grid_y_color" %in% names(mos_pars))) { stop ("Error found in grid_y_color! Please recheck grid_y_color and run load_parameters again!") }
  if (!("grid_y_width" %in% names(mos_pars))) { stop ("Error found in grid_y_width! Please recheck grid_y_width and run load_parameters again!") }
  if (!("grid_x_color" %in% names(mos_pars))) { stop ("Error found in grid_x_color! Please recheck grid_x_color and run load_parameters again!") }
  if (!("grid_x_width" %in% names(mos_pars))) { stop ("Error found in grid_x_width! Please recheck grid_x_width and run load_parameters again!") }
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
  if (length(mos_pars$panel_x_blank)==1) { if (length(mos_pars$panel_x_frag)==1) { mos_pars$panel_x_blank = 0 } else { mos_pars$panel_x_blank = rep(mos_pars$panel_x_blank, length(mos_pars$panel_x_frag)-1) } }
  total_data = sum(mos_pars$panel_x_frag) + sum(mos_pars$panel_x_blank)
  if (length(mos_pars$panel_x_blank)==length(mos_pars$panel_x_frag)) { total_data = total_data - mos_pars$panel_x_blank[length(mos_pars$panel_x_frag)] }
  # delta_angle setting
  if (mos_pars$global_circ_num>0) {
    mos_pars$global_init_angle = 180
    delta_angle = 180
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
    mos_pars$frag_st = as.vector(tri %*% mos_pars$panel_x_frag) + as.vector(tri %*% c(mos_pars$panel_x_blank, 0)) + 1
    tri = matrix(1, length(mos_pars$panel_x_frag), length(mos_pars$panel_x_frag))
    tri[upper.tri(tri)] = 0
    mos_pars$frag_en = as.vector(tri %*% mos_pars$panel_x_frag) + as.vector(tri %*% c(0, mos_pars$panel_x_blank)) + 1
  }
  # init baseline
  if (baseline_init) {
    theta_angle = mos_pars$global_start_angle/180*pi
    dl = theta_angle*mos_pars$global_init_angle/180*pi
    mos_pars$baseline = rep(0, total_data+1)
    i = 0
    while (i<=total_data+1) {
      mos_pars$baseline[i] = theta_angle
      dt = dl/theta_angle
      theta_angle = theta_angle+dt
      i = i+1
    }
    mos_pars$size = max(c(mos_pars$global_h_zoom*mos_pars$baseline*cos(mos_pars$baseline),
                          mos_pars$global_h_zoom*mos_pars$baseline*sin(mos_pars$baseline))) +
    mos_pars$global_h_total+mos_pars$tag_x_offsize+mos_pars$tag_x_size
  }
  return (mos_pars)
}

