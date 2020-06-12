#' Title
#'
#' @param mos_pars arguments list: generated in function load_parameters.
#' @param panel integer vector: panel ID (panel_y_height), please use function mapping_data convert panel name to panel ID.
#' @param frag integer vector: fragment ID (panel_x_frags), please use function mapping_data convert fragment name to fragment ID.
#' @param x numeric vector: x coordinates.
#' @param y numeric vector: y coordinates.
#' @param y.range vector contains two numeric elements: c(lower bound of data, upper bound of data).
#' @param texts character vector: texts list.
#' @param angle numeric or character: can be "off", "auto" or -180~180.
#' @param ... pass to the function 'text'.
#'
#' @return add text on spiral figure
#' @export
#'
#' @examples
#'
mapping_text <- function(mos_pars=NULL, panel=NULL, frag=NULL, x=NULL, y=NULL, y.range=NULL, texts=NULL, text_arc=T, angle="auto", ...) {
  pi = 3.1415926
  h = mos_pars$global_h_total/2
  
  if (is.null(mos_pars)) { stop ("mos_pars is NULL!") }
  if (!("baseline" %in% names(mos_pars))) { stop ("There is no 'baseline' tag in your parameters, maybe there is something wrong in function 'load_parameters' or 'update_parameters'!") }
  else {
    if (length(mos_pars$baseline)<sum(mos_pars$panel_x_frag) + length(mos_pars$panel_x_frag)*mos_pars$panel_x_blank - mos_pars$panel_x_blank) {
      stop ("The length of 'baseline' is not match with the given parameters 'panel_x_frag' and 'panel_x_blank', maybe there is something wrong in function 'load_parameters' or 'update_parameters'!")
    }
  }
  if (is.null(x)) { stop ("x is not given!") }
  if (is.null(y)) { stop ("y is not given!") }
  if (length(x)!=length(y)) { stop ("x coordinates & y coordinates are not in the same length!") }
  if (is.null(panel) & length(mos_pars$panel_y_height)==1) { panel = rep(1,length(x)) }
  if (!is.null(panel)) { if (length(panel)==1) { panel = rep(panel,length(x)) } else if (length(x)!=length(panel)) { stop ("panel ID & your data are not in the same length!") } }
  if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(y)) }
  if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x)) } else if (length(x)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
  if (is.null(y.range)) { y.range = c(min(y), max(y)) }
  if (is.null(texts)) { stop ("texts is not given!") }
  if (length(texts)!=length(x) & length(texts)!=1) { stop ("Parameter texts is illegal!") }
  if (is.null(y.range)) { y.range = c(min(y), max(y)) }
  if (length(which(y<y.range[1]))>0) { y[which(y<y.range[1])] = y.range[1] }
  if (length(which(y>y.range[2]))>0) { y[which(y>y.range[2])] = y.range[2] }
  y = (y-y.range[1])/(y.range[2]-y.range[1])
  xtemp = x
  ytemp = y
  basetemp = mos_pars$baseline[xtemp+mos_pars$frag_st[frag]]
  ptemp = panel[seq(1,length(panel),by=mos_pars$point_span)]
  if (text_arc) {
    if (mos_pars$global_directory) {
      rtemp = mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[ptemp]-ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp])
    } else {
      rtemp = mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[ptemp]+ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp])
    }
    for (i in seq(1, length(texts))) {
      arctext(x=texts[i], radius=rtemp[i], middle=basetemp[i], clockwise=F, ...)
    }
  } else {
    if (mos_pars$global_directory) {
      plot_x <- (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[ptemp]-ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp)
      plot_y <- (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[ptemp]-ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp)
    } else {
      plot_x <- (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[ptemp]+ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp)
      plot_y <- (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[ptemp]+ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp)
    }
    if (angle=="off") {
      text(x=plot_x, y=plot_y, labels=texts, ...)
    } else if (angle=="auto") {
      for (i in seq(1, length(texts))) {
        text(x=plot_x[i], y=plot_y[i], labels=texts[i], srt=basetemp[i]/pi*180+90, ...)
      }
    } else {
      for (i in seq(1, length(texts))) {
        text(x=plot_x[i], y=plot_y[i], labels=texts[i], srt=basetemp[i]/pi*180+90+angle, ...)
      }
    }
  }
}
