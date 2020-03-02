#' Title
#'
#' @param mos_pars arguments list: generated in function load_parameters.
#' @param shape character: can be "point"/"p", "line"/"l", "bar"/"b", "arrow"/"a".
#' @param panel integer vector: panel ID (panel_y_height), please use function mapping_data convert panel name to panel ID.
#' @param frag integer vector: fragment ID (panel_x_frags), please use function mapping_data convert fragment name to fragment ID.
#' @param x numeric vector: x coordinates of points in the point and line plot.
#' @param x.st numeric vector: x coordinates of points in the bar and arrow plot.
#' @param x.en numeric vector: x coordinates of points in the bar and arrow plot.
#' @param y numeric vector: y coordinates of points in the point and line plot.
#' @param y.st numeric vector: y coordinates of points in the bar and arrow plot.
#' @param y.en numeric vector: y coordinates of points in the bar and arrow plot.
#' @param y.range vector contains two numeric elements: c(lower bound of data, upper bound of data).
#' @param col character (color) vector: color for point, line and border of bar and arrow.
#' @param col.alpha numeric vector: alpha for col.
#' @param fill character (color) vector: color to fill bar and arrow.
#' @param fill.alpha numeric vector: alpha for fill.
#' @param size numeric vector: size for point, line and border of bar and arrow.
#' @param arrow.size numeric: arrow size.
#' @param arrow.type numeric: arrow type, 1 refers to outer-arrow, -1 refers to inner-arrow.
#'
#' @return add elements on spiral figure
#' @export
#'
#' @examples
#'
mapping_shape <- function(mos_pars=NULL, shape=NULL, panel=NULL, frag=NULL, x=NULL, x.st=NULL, x.en=NULL, y=NULL, y.st=NULL, y.en=NULL, y.range=NULL, col=NULL, col.alpha=NULL, fill=NULL, fill.alpha=NULL, size=NULL, arrow.size=NULL, arrow.type=-1, pch=19) {

  col2hex <- function(col, alpha) { rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255) }
  h = mos_pars$global_h_total/2

  if (is.null(mos_pars)) { stop ("mos_pars is NULL!") }
  if (!("baseline" %in% names(mos_pars))) { stop ("There is no 'baseline' tag in your parameters, maybe there is something wrong in function 'load_parameters' or 'update_parameters'!") }
  else {
    if (length(mos_pars$baseline)<sum(mos_pars$panel_x_frag) + length(mos_pars$panel_x_frag)*mos_pars$panel_x_blank - mos_pars$panel_x_blank) {
      stop ("The length of 'baseline' is not match with the given parameters 'panel_x_frag' and 'panel_x_blank', maybe there is something wrong in function 'load_parameters' or 'update_parameters'!")
    }
  }
  if (is.null(shape)) { stop ("shape is NULL!") }
  if (is.null(panel) & length(mos_pars$panel_y_height)>1) { stop ("panel ID is not given!") }
  if (is.null(frag) & length(mos_pars$panel_x_frag)>1) { stop ("fragment ID is not given!") }

  if (shape=="point" | shape=="p") {
    # error check for point
    if (is.null(x)) { stop ("x coordinates is not given!") }
    if (is.null(y)) { stop ("y coordinates is not given!") }
    if (length(x)!=length(y)) { stop ("x coordinates & y coordinates are not in the same length!") }
    if (is.null(panel) & length(mos_pars$panel_y_height)==1) { panel = rep(1,length(x)) }
    if (!is.null(panel)) { if (length(panel)==1) { panel = rep(panel,length(x)) } else if (length(x)!=length(panel)) { stop ("panel ID & your data are not in the same length!") } }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(y)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x)) } else if (length(x)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    # reform data & plot
    if (is.null(y.range)) { y.range = c(min(y), max(y)) }
    if (length(which(y<y.range[1]))>0) { y[which(y<y.range[1])] = y.range[1] }
    if (length(which(y>y.range[2]))>0) { y[which(y>y.range[2])] = y.range[2] }
    y = (y-y.range[1])/(y.range[2]-y.range[1])
    xtemp = x[seq(1,length(x),by=mos_pars$point_span)]
    ytemp = y[seq(1,length(y),by=mos_pars$point_span)]
    ptemp = panel[seq(1,length(panel),by=mos_pars$point_span)]
    ftemp = frag[seq(1,length(frag),by=mos_pars$point_span)]
    if (length(col)>1) { col = col[seq(1,length(col),by=mos_pars$point_span)] }
    if (length(col.alpha)>1) { col.alpha = col.alpha[seq(1,length(col.alpha),by=mos_pars$point_span)] }
    if (length(size)>1) { stemp = size[seq(1,length(size),by=mos_pars$point_span)] }
    basetemp = mos_pars$baseline[xtemp+mos_pars$frag_st[ftemp]]
    if (mos_pars$global_directory) {
      plot_x <- (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[ptemp]-ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp)
      plot_y <- (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[ptemp]-ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp)
    } else {
      plot_x <- (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[ptemp]+ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp)
      plot_y <- (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[ptemp]+ytemp*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp)
    }
    if (is.null(col)) { col = mos_pars$point_col }
    if (is.null(col.alpha)) { col.alpha = mos_pars$point_alpha }
    plot_col = col2hex(col, col.alpha*255)
    if (is.null(size)) { plot_size = mos_pars$point_size } else { plot_size = size }
    points(x=plot_x, y=plot_y, col=plot_col, cex=plot_size, lwd=NA, pch=pch)
  }

  if (shape=="line" | shape=="l") {
    # error check for point
    if (is.null(x)) { stop ("x coordinates is not given!") }
    if (is.null(y)) { stop ("y coordinates is not given!") }
    if (length(x)!=length(y)) { stop ("x coordinates & y coordinates are not in the same length!") }
    if (is.null(panel) & length(mos_pars$panel_y_height)==1) { panel = rep(1,length(x)) }
    if (!is.null(panel)) { if (length(panel)==1) { panel = rep(panel,length(x)) } else if (length(x)!=length(panel)) { stop ("panel ID & your data are not in the same length!") } }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(y)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x)) } else if (length(x)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    # reform data & plot
    if (is.null(y.range)) { y.range = c(min(y), max(y)) }
    if (length(which(y<y.range[1]))>0) { y[which(y<y.range[1])] = y.range[1] }
    if (length(which(y>y.range[2]))>0) { y[which(y>y.range[2])] = y.range[2] }
    y = (y-y.range[1])/(y.range[2]-y.range[1])
    plot_panel = unique(panel)
    plot_frag = unique(frag)
    for (k in seq(1,length(plot_frag))) {
      for (i in seq(1, length(plot_panel))) {
        xtemp = x[which(panel==plot_panel[i] & frag==plot_frag[k])]
        ytemp = y[which(panel==plot_panel[i] & frag==plot_frag[k])]
        if (length(col)>1) { ctemp = col[which(panel==plot_panel[i] & frag==plot_frag[k])] }
        if (length(col.alpha)>1) { catemp = col.alpha[which(panel==plot_panel[i] & frag==plot_frag[k])] }
        if (length(size)>1) { stemp = size[which(panel==plot_panel[i] & frag==plot_frag[k])] }
        xtemp = xtemp[seq(1,length(x),by=mos_pars$line_span)]
        ytemp = ytemp[seq(1,length(y),by=mos_pars$line_span)]
        if (length(col)>1) { ctemp = ctemp[seq(1,length(col),by=mos_pars$point_span)] } else { ctemp = col }
        if (length(col.alpha)>1) { catemp = catemp[seq(1,length(col.alpha),by=mos_pars$point_span)] } else { catemp = col.alpha }
        if (length(size)>1) { stemp = stemp[seq(1,length(size),by=mos_pars$point_span)] } else { stemp = size }
        basetemp = mos_pars$baseline[xtemp+mos_pars$frag_st[plot_frag[k]]]
        if (mos_pars$global_directory) {
          plot_x <- (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[plot_panel[i]]-ytemp*(mos_pars$panel_en[plot_panel[i]]-mos_pars$panel_st[plot_panel[i]]))*cos(basetemp)
          plot_y <- (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[plot_panel[i]]-ytemp*(mos_pars$panel_en[plot_panel[i]]-mos_pars$panel_st[plot_panel[i]]))*sin(basetemp)
        } else {
          plot_x <- (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[plot_panel[i]]+ytemp*(mos_pars$panel_en[plot_panel[i]]-mos_pars$panel_st[plot_panel[i]]))*cos(basetemp)
          plot_y <- (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[plot_panel[i]]+ytemp*(mos_pars$panel_en[plot_panel[i]]-mos_pars$panel_st[plot_panel[i]]))*sin(basetemp)
        }
        if (is.null(col)) { ctemp = mos_pars$line_col }
        if (is.null(col.alpha)) { catemp = mos_pars$line_alpha }
        plot_col = col2hex(ctemp, catemp*255)
        if (is.null(size)) { plot_size = mos_pars$line_size } else { plot_size = stemp }
        lines(x=plot_x, y=plot_y, col=plot_col, lwd=plot_size)
      }
    }
  }

  if (shape=="bar" | shape=="b") {
    # error check for point
    if (is.null(x.st)) { stop ("x.st coordinates is not given!") }
    if (is.null(x.en)) { stop ("x.en coordinates is not given!") }
    if (is.null(y.st)) { stop ("y.st coordinates is not given!") }
    if (is.null(y.en)) { stop ("y.en coordinates is not given!") }
    if (length(x.st)!=length(x.en)) { stop ("x.st coordinates & x.en coordinates are not in the same length!") }
    if (length(x.st)!=length(y.st)) { stop ("x coordinates & y coordinates are not in the same length!") }
    if (!is.null(y.en)) { if (length(y.st)!=length(y.en) & length(y.en)!=1) { stop ("y.st coordinates & y.en coordinates are not in the same length!") } }
    if (is.null(panel) & length(mos_pars$panel_y_height)==1) { panel = rep(1,length(x.st)) }
    if (!is.null(panel)) { if (length(panel)==1) { panel = rep(panel,length(x.st)) } else if (length(x.st)!=length(panel)) { stop ("panel ID & your data are not in the same length!") } }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(y.st)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x.st)) } else if (length(x.st)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x.st)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x.st)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(fill)) { if (length(x.st)!=length(fill) & length(fill)!=1) { stop ("Parameter fill is illegal!") } }
    if (!is.null(fill.alpha)) { if (length(x.st)!=length(fill.alpha) & length(fill.alpha)!=1) { stop ("Parameter fill.alpha is illegal!") } }
    if (!is.null(fill) & !is.null(fill.alpha)) { if (length(fill)!=length(fill.alpha) & length(fill)!=1 & length(fill.alpha)!=1) { stop ("fill & fill.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x.st)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    # reform data & plot
    if (length(y.st)==1) { y.st = rep(y.st, length(x.st)) }
    if (length(y.en)==1) { y.en = rep(y.en, length(x.st)) }
    if (is.null(y.range)) { y.range = c(min(c(y.st,y.en)), max(c(y.st,y.en))) }
    if (length(which(y.st<y.range[1]))>0) { y.st[which(y.st<y.range[1])] = y.range[1] }
    if (length(which(y.st>y.range[2]))>0) { y.st[which(y.st>y.range[2])] = y.range[2] }
    if (length(which(y.en<y.range[1]))>0) { y.en[which(y.en<y.range[1])] = y.range[1] }
    if (length(which(y.en>y.range[2]))>0) { y.en[which(y.en>y.range[2])] = y.range[2] }
    y.st = (y.st-y.range[1])/(y.range[2]-y.range[1])
    y.en = (y.en-y.range[1])/(y.range[2]-y.range[1])
    x.st = x.st[seq(1,length(x.st),by=mos_pars$bar_span)]
    x.en = x.en[seq(1,length(x.en),by=mos_pars$bar_span)]
    y.st = y.st[seq(1,length(y.st),by=mos_pars$bar_span)]
    y.en = y.en[seq(1,length(y.en),by=mos_pars$bar_span)]
    ptemp = panel[seq(1,length(panel),by=mos_pars$bar_span)]
    ftemp = frag[seq(1,length(frag),by=mos_pars$bar_span)]
    basetemp.st = mos_pars$baseline[x.st+mos_pars$frag_st[ftemp]]
    basetemp.en = mos_pars$baseline[x.en+mos_pars$frag_st[ftemp]]
    if (length(col)>1) { col = col[seq(1,length(col),by=mos_pars$bar_span)] }
    if (length(col.alpha)>1) { col.alpha = col.alpha[seq(1,length(col.alpha),by=mos_pars$bar_span)] }
    if (length(fill)>1) { fill = fill[seq(1,length(fill),by=mos_pars$bar_span)] }
    if (length(fill.alpha)>1) { fill.alpha = fill.alpha[seq(1,length(fill.alpha),by=mos_pars$bar_span)] }
    if (length(size)>1) { stemp = size[seq(1,length(size),by=mos_pars$bar_span)] } else if (length(size)==1) { stemp = size }
    if (mos_pars$global_directory) {
      plot_x11 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_x12 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x22 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x21 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_y11 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
      plot_y12 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y22 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y21 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
    } else {
      plot_x11 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_x12 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x22 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x21 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_y11 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
      plot_y12 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y22 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y21 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
    }
    if (is.null(col)) { col = mos_pars$bar_line_col }
    if (is.null(col.alpha)) { col.alpha = mos_pars$bar_line_alpha }
    if (!is.null(col) & !is.null(col.alpha)) { plot_col = col2hex(col, col.alpha*255) } else { plot_col = NULL }
    if (is.null(fill)) { fill = mos_pars$bar_fill_col }
    if (is.null(fill.alpha)) { fill.alpha = mos_pars$bar_fill_alpha }
    if (!is.null(fill) & !is.null(fill.alpha)) { plot_fill = col2hex(fill, fill.alpha*255) } else { plot_fill = NULL }
    if (is.null(size)) { plot_size = mos_pars$point_size } else { plot_size = stemp }
    for (i in seq(1,min(length(plot_x11),length(plot_x12),length(plot_x21),length(plot_x22)))) {
      temp = cbind(c(plot_x11[i], plot_x12[i], plot_x22[i], plot_x21[i]),c(plot_y11[i], plot_y12[i], plot_y22[i], plot_y21[i]))
      if (!is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[i] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[i] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[i] } else { pborder = plot_col }
        polygon(temp, col=pfill, lwd=psize, border=pborder )
      }
      if (is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[i] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[i] } else { psize = plot_size }
        polygon(temp, col=pfill, lwd=psize, border=NA )
      }
      if (!is.null(plot_col) & is.null(plot_fill)) {
        if (length(plot_size)>1) { psize = plot_size[i] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[i] } else { pborder = plot_col }
        polygon(temp, col=NA, lwd=psize, border=pborder )
      }
    }
  }

  if (shape=="arrow" | shape=="a") {
    # error check for point
    if (is.null(x.st)) { stop ("x.st coordinates is not given!") }
    if (is.null(x.en)) { stop ("x.en coordinates is not given!") }
    if (is.null(y.st)) { stop ("y.st coordinates is not given!") }
    if (is.null(y.en)) { stop ("y.en coordinates is not given!") }
    if (length(y.st)>1) { stop ("y.st coordinates is illegal!") }
    if (length(y.en)>1) { stop ("y.en coordinates is illegal!") }
    if (y.st>1 | y.st<0 | y.en>1 | y.en<0) { stop ("Please set y.st & y.en to [0,1]!") }
    if (is.null(arrow.size)) { stop ("arrow.size is not given!") } else if (length(arrow.size)>1) { stop ("arrow.size is illegal!") }
    x.st = x.st[seq(1,length(x.st),by=mos_pars$bar_span)]
    x.en = x.en[seq(1,length(x.en),by=mos_pars$bar_span)]
    ptemp = panel[seq(1,length(panel),by=mos_pars$bar_span)]
    ftemp = frag[seq(1,length(frag),by=mos_pars$bar_span)]
    if (arrow.size>0 & arrow.type>0) {
      basetemp.st = mos_pars$baseline[x.st+mos_pars$frag_st[ftemp]]
      basetemp.en = mos_pars$baseline[x.en+mos_pars$frag_st[ftemp]]
      templist = x.en+arrow.size+mos_pars$frag_st[frag]
      if (length(which(templist>length(mos_pars$baseline)))>0) { templist[which(templist>length(mos_pars$baseline))] = length(mos_pars$baseline) }
      basetemp.ar = mos_pars$baseline[templist]
    }
    if (arrow.size<0 & arrow.type>0) {
      basetemp.st = mos_pars$baseline[x.st+mos_pars$frag_st[ftemp]]
      basetemp.en = mos_pars$baseline[x.en+mos_pars$frag_st[ftemp]]
      templist = x.st+arrow.size+mos_pars$frag_st[frag]
      if (length(which(templist<0))>0) { templist[which(templist<0)] = 1 }
      basetemp.ar = mos_pars$baseline[templist]
    }
    if (arrow.size>0 & arrow.type<0) {
      basetemp.st = mos_pars$baseline[x.st+mos_pars$frag_st[ftemp]]
      basetemp.ar = mos_pars$baseline[x.en+mos_pars$frag_st[ftemp]]
      basetemp.en = mos_pars$baseline[x.en-arrow.size+mos_pars$frag_st[ftemp]]
      if (length(which(x.en-arrow.size<x.st))>0) {
        basetemp.en[which(x.en-arrow.size<x.st)] = basetemp.st[which(x.en-arrow.size<x.st)]
      }
    }
    if (arrow.size<0 & arrow.type<0) {
      basetemp.st = mos_pars$baseline[x.st-arrow.size+mos_pars$frag_st[ftemp]]
      basetemp.ar = mos_pars$baseline[x.st+mos_pars$frag_st[ftemp]]
      basetemp.en = mos_pars$baseline[x.en+mos_pars$frag_st[ftemp]]
      if (length(which(x.st-arrow.size>x.en))>0) {
        basetemp.st[which(x.st-arrow.size>x.en)] = basetemp.en[which(x.st-arrow.size>x.en)]
      }
    }
    if (length(col)>1) { col = col[seq(1,length(col),by=mos_pars$bar_span)] }
    if (length(col.alpha)>1) { col.alpha = col.alpha[seq(1,length(col.alpha),by=mos_pars$bar_span)] }
    if (length(fill)>1) { fill = fill[seq(1,length(fill),by=mos_pars$bar_span)] }
    if (length(fill.alpha)>1) { fill.alpha = fill.alpha[seq(1,length(fill.alpha),by=mos_pars$bar_span)] }
    if (length(size)>1) { stemp = size[seq(1,length(size),by=mos_pars$bar_span)] } else if (length(size)==1) { stemp = size }
    if (mos_pars$global_directory) {
      plot_x11 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_x12 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x22 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x21 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_y11 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
      plot_y12 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y22 <- (mos_pars$global_h_zoom*basetemp.en-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y21 <- (mos_pars$global_h_zoom*basetemp.st-h+mos_pars$panel_en[ptemp]-y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
      plot_xar <- (mos_pars$global_h_zoom*basetemp.ar-h+mos_pars$panel_en[ptemp]-(y.st+y.en)/2*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.ar)
      plot_yar <- (mos_pars$global_h_zoom*basetemp.ar-h+mos_pars$panel_en[ptemp]-(y.st+y.en)/2*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.ar)
    } else {
      plot_x11 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_x12 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x22 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.en)
      plot_x21 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.st)
      plot_y11 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
      plot_y12 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.st*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y22 <- (mos_pars$global_h_zoom*basetemp.en+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.en)
      plot_y21 <- (mos_pars$global_h_zoom*basetemp.st+h-mos_pars$panel_en[ptemp]+y.en*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.st)
      plot_xar <- (mos_pars$global_h_zoom*basetemp.ar+h-mos_pars$panel_en[ptemp]+(y.st+y.en)/2*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*cos(basetemp.ar)
      plot_yar <- (mos_pars$global_h_zoom*basetemp.ar+h-mos_pars$panel_en[ptemp]+(y.st+y.en)/2*(mos_pars$panel_en[ptemp]-mos_pars$panel_st[ptemp]))*sin(basetemp.ar)
    }
    if (is.null(col)) { col = mos_pars$bar_line_col }
    if (is.null(col.alpha)) { col.alpha = mos_pars$bar_line_alpha }
    if (!is.null(col) & !is.null(col.alpha)) { plot_col = col2hex(col, col.alpha*255) } else { plot_col = NULL }
    if (is.null(fill)) { fill = mos_pars$bar_fill_col }
    if (is.null(fill.alpha)) { fill.alpha = mos_pars$bar_fill_alpha }
    if (!is.null(fill) & !is.null(fill.alpha)) { plot_fill = col2hex(fill, fill.alpha*255) } else { plot_fill = NULL }
    if (is.null(size)) { plot_size = mos_pars$point_size } else { plot_size = stemp }
    for (i in seq(1,min(length(plot_x11),length(plot_x12),length(plot_x21),length(plot_x22)))) {
      if (arrow.size>0) {
        temp = cbind(c(plot_x11[i], plot_x12[i], plot_xar[i], plot_x22[i], plot_x21[i]),c(plot_y11[i], plot_y12[i], plot_yar[i], plot_y22[i], plot_y21[i]))
      } else {
        temp = cbind(c(plot_x11[i], plot_x12[i], plot_x22[i], plot_x21[i], plot_xar[i]),c(plot_y11[i], plot_y12[i], plot_y22[i], plot_y21[i], plot_yar[i]))
      }
      if (!is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[i] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[i] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[i] } else { pborder = plot_col }
        polygon(temp, col=pfill, lwd=psize, border=pborder )
      }
      if (is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[i] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[i] } else { psize = plot_size }
        polygon(temp, col=pfill, lwd=psize, border=NA )
      }
      if (!is.null(plot_col) & is.null(plot_fill)) {
        if (length(plot_size)>1) { psize = plot_size[i] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[i] } else { pborder = plot_col }
        polygon(temp, col=NA, lwd=psize, border=pborder )
      }
    }
  }
}

