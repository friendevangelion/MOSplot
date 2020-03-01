#' Title
#'
#' @param mos_pars arguments list: generated in function load_parameters.
#'
#' @return a list
#' @export
#'
#' @examples
#'
update_parameters <- function(mos_pars=NULL) {
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
  # init baseline
  theta_angle = mos_pars$global_start_angle/180*pi
  dl = theta_angle*mos_pars$global_init_angle/180*pi
  total_data = sum(mos_pars$panel_x_frag) + length(mos_pars$panel_x_frag)*mos_pars$panel_x_blank - mos_pars$panel_x_blank
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
  return (mos_pars)
}
