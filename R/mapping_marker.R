#' Title
#'
#' @param mos_pars arguments list: generated in function load_parameters.
#' @param shape character: can be "point"/"p", "region"/"r", "shade"/"s", "triangle"/"t".
#' @param x.st numeric vector: x coordinates.
#' @param x.en numeric vector: x coordinates.
#' @param y.st numeric vector: y coordinates.
#' @param y.en numeric vector: y coordinates.
#' @param y.range vector contains two numeric elements: c(lower bound of data, upper bound of data).
#' @param col character (color) vector: color for border.
#' @param col.alpha numeric vector: alpha for col.
#' @param fill character (color) vector: color to fill.
#' @param fill.alpha numeric vector: alpha for fill.
#' @param size numeric vector: size for border.
#' @param tri numeric vector: contains arrow position, arrow length and arrow angle.
#'
#' @return add elements on spiral figure
#' @export
#'
#' @examples
#'
mapping_marker <- function(mos_pars=NULL, shape=NULL, panel=NULL, frag=NULL, x.st=NULL, x.en=NULL, y.st=NULL, y.en=NULL, y.range=NULL, col=NULL, col.alpha=NULL, fill=NULL, fill.alpha=NULL, size=NULL, others=NULL, tri=NULL, plwd=0.1, pch=21) {
  col2hex <- function(col, alpha) { rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255) }
  pi = 3.1415926
  h = mos_pars$global_h_total/2
  
  if (is.null(mos_pars)) { stop ("mos_pars is NULL!") }
  if (!("baseline" %in% names(mos_pars))) { stop ("There is no 'baseline' tag in your parameters, maybe there is something wrong in function 'load_parameters' or 'update_parameters'!") }
  else {
    if (length(mos_pars$baseline)<sum(mos_pars$panel_x_frag) + length(mos_pars$panel_x_frag)*mos_pars$panel_x_blank - mos_pars$panel_x_blank) {
      stop ("The length of 'baseline' is not match with the given parameters 'panel_x_frag' and 'panel_x_blank', maybe there is something wrong in function 'load_parameters' or 'update_parameters'!")
    }
  }
  if (is.null(shape)) { stop ("shape is NULL!") }
  if (is.null(x.st)) { stop ("x.st is not given!") }
  if (shape=="point" | shape=="p") {
    if (is.null(panel) & length(mos_pars$panel_y_height)>1) { stop ("panel ID is not given!") }
    if (is.null(frag) & length(mos_pars$panel_x_frag)>1) { stop ("fragment ID is not given!") }
    if (is.null(y.st)) { stop ("y.st is not given!") }
    if (is.null(y.range)) { warning ('y.range is set to [min(y), max(y)]!') }
    if (length(x.st)!=length(x.st)) { stop ("x coordinates & y coordinates are not in the same length!") }
    if (is.null(panel) & length(mos_pars$panel_y_height)==1) { panel = rep(1,length(x.st)) }
    if (!is.null(panel)) { if (length(panel)==1) { panel = rep(panel,length(x.st)) } else if (length(x.st)!=length(panel)) { stop ("panel ID & your data are not in the same length!") } }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(x.st)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x.st)) } else if (length(x.st)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x.st)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x.st)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x.st)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    # reform data & plot
    if (is.null(y.range)) { y.range = c(min(y.st), max(y.st)) }
    if (length(which(y.st<y.range[1]))>0) { y.st[which(y.st<y.range[1])] = y.range[1] }
    if (length(which(y.st>y.range[2]))>0) { y.st[which(y.st>y.range[2])] = y.range[2] }
    y.st = (y.st-y.range[1])/(y.range[2]-y.range[1])
    xtemp = x.st
    ytemp = y.st
    ptemp = panel
    ftemp = frag
    stemp = size
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
    if (is.null(size)) { plot_size = mos_pars$point_size } else { plot_size = stemp }
    points(x=plot_x, y=plot_y, col=plot_col, cex=plot_size, lwd=plwd, pch=pch)
  }

  if (shape=="region" | shape=="r") {
    if (is.null(panel) & length(mos_pars$panel_y_height)>1) { stop ("panel ID is not given!") }
    if (is.null(frag) & length(mos_pars$panel_x_frag)>1) { stop ("fragment ID is not given!") }
    if (is.null(x.en)) { stop ("x.en is not given!") }
    if (is.null(y.st)) { stop ("y.st is not given!") }
    if (is.null(y.en)) { stop ("y.en is not given!") }
    if (length(x.st)!=length(x.en) | length(x.st)!=length(x.st) | length(x.st)!=length(y.en)) { stop ("x coordinates & y coordinates are not in the same length!") }
    if (is.null(y.range)) { warning ('y.range is set to [min(y), max(y)]!') }
    if (is.null(panel) & length(mos_pars$panel_y_height)==1) { panel = rep(1,length(x.st)) }
    if (!is.null(panel)) { if (length(panel)==1) { panel = rep(panel,length(x.st)) } else if (length(x.st)!=length(panel)) { stop ("panel ID & your data are not in the same length!") } }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(x.st)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x.st)) } else if (length(x.st)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x.st)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x.st)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x.st)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    # reform data & plot
    if (is.null(y.range)) { y.range = c(min(c(y.st,y.en)), max(c(y.st,y.en))) }
    if (length(which(y.en<y.range[1]))>0) { y.en[which(y.en<y.range[1])] = y.range[1] }
    if (length(which(y.en>y.range[2]))>0) { y.en[which(y.en>y.range[2])] = y.range[2] }
    y.st = (y.st-y.range[1])/(y.range[2]-y.range[1])
    y.en = (y.en-y.range[1])/(y.range[2]-y.range[1])
    for (j in seq(1, length(x.st))) {
      basetemp = mos_pars$baseline[c(seq(mos_pars$frag_st[frag[j]]+x.st[j], mos_pars$frag_st[frag[j]]+x.en[j], by=mos_pars$panel_x_texture), mos_pars$frag_st[frag[j]]+x.en[j])]
      if (mos_pars$global_directory) {
        temp = rbind(cbind((mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[panel[j]]-y.st[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*cos(basetemp),
                           (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[panel[j]]-y.st[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*sin(basetemp)),
                     cbind(rev((mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[panel[j]]-y.en[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*cos(basetemp)),
                           rev((mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[panel[j]]-y.en[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*sin(basetemp))))
      } else {
        temp = rbind(cbind((mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[panel[j]]+y.st[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*cos(basetemp),
                           (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[panel[j]]+y.st[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*sin(basetemp)),
                     cbind(rev((mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[panel[j]]+y.en[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*cos(basetemp)),
                           rev((mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[panel[j]]+y.en[j]*(mos_pars$panel_en[panel[j]]-mos_pars$panel_st[panel[j]]))*sin(basetemp))))
      }
      if (is.null(col)) { col = mos_pars$bar_line_col }
      if (is.null(col.alpha)) { col.alpha = mos_pars$bar_line_alpha }
      if (!is.null(col) & !is.null(col.alpha)) { plot_col = col2hex(col, col.alpha*255) } else { plot_col = NULL }
      if (is.null(fill)) { fill = mos_pars$bar_fill_col }
      if (is.null(fill.alpha)) { fill.alpha = mos_pars$bar_fill_alpha }
      if (!is.null(fill) & !is.null(fill.alpha)) { plot_fill = col2hex(fill, fill.alpha*255) } else { plot_fill = NULL }
      if (is.null(size)) { plot_size = mos_pars$line_size } else { plot_size = size }
      if (!is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[j] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[j] } else { pborder = plot_col }
        polygon(temp, col=pfill, lwd=psize, border=pborder )
      }
      if (is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[j] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        polygon(temp, col=pfill, lwd=psize, border=NA )
      }
      if (!is.null(plot_col) & is.null(plot_fill)) {
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[j] } else { pborder = plot_col }
        polygon(temp, col=NA, lwd=psize, border=pborder )
      }
    }
  }

  if (shape=="shade" | shape=="s") {
    if (is.null(frag) & length(mos_pars$panel_x_frag)>1) { stop ("fragment ID is not given!") }
    if (is.null(x.en)) { stop ("x.en is not given!") }
    if (length(x.st)!=length(x.en)) { stop ("x coordinates is not in the same length!") }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(x.st)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x.st)) } else if (length(x.st)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x.st)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x.st)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x.st)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    # reform data & plot
    if (is.null(col)) { col = mos_pars$bar_line_col }
    if (is.null(col.alpha)) { col.alpha = mos_pars$bar_line_alpha }
    if (!is.null(col) & !is.null(col.alpha)) { plot_col = col2hex(col, col.alpha*255) } else { plot_col = NULL }
    if (is.null(fill)) { fill = mos_pars$bar_fill_col }
    if (is.null(fill.alpha)) { fill.alpha = mos_pars$bar_fill_alpha }
    if (!is.null(fill) & !is.null(fill.alpha)) { plot_fill = col2hex(fill, fill.alpha*255) } else { plot_fill = NULL }
    if (is.null(size)) { plot_size = mos_pars$line_size } else { plot_size = stemp }
    for (j in seq(1, length(x.st))) {
      basetemp = mos_pars$baseline[c(seq(mos_pars$frag_st[frag[j]]+x.st[j], mos_pars$frag_st[frag[j]]+x.en[j], by=mos_pars$panel_x_texture), mos_pars$frag_st[frag[j]]+x.en[j])]
      temp = rbind(cbind((mos_pars$global_h_zoom*basetemp-h)*cos(basetemp),
                         (mos_pars$global_h_zoom*basetemp-h)*sin(basetemp)),
                   cbind(rev((mos_pars$global_h_zoom*basetemp+h)*cos(basetemp)),
                         rev((mos_pars$global_h_zoom*basetemp+h)*sin(basetemp))))
      if (!is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[j] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[j] } else { pborder = plot_col }
        polygon(temp, col=pfill, lwd=psize, border=pborder )
      }
      if (is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[j] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        polygon(temp, col=pfill, lwd=psize, border=NA )
      }
      if (!is.null(plot_col) & is.null(plot_fill)) {
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[j] } else { pborder = plot_col }
        polygon(temp, col=NA, lwd=psize, border=pborder )
      }
    }
  }

  if (shape=="triangle" | shape=="t") {
    if (is.null(frag) & length(mos_pars$panel_x_frag)>1) { stop ("fragment ID is not given!") }
    if (is.null(frag) & length(mos_pars$panel_x_frag)==1) { frag = rep(1,length(x.st)) }
    if (!is.null(frag)) { if (length(frag)==1) { frag = rep(frag,length(x.st)) } else if (length(x.st)!=length(frag)) { stop ("fragment ID & your data are not in the same length!") } }
    if (!is.null(col)) { if (length(x.st)!=length(col) & length(col)!=1) { stop ("Parameter col is illegal!") } }
    if (!is.null(col.alpha)) { if (length(x.st)!=length(col.alpha) & length(col.alpha)!=1) { stop ("Parameter col.alpha is illegal!") } }
    if (!is.null(col) & !is.null(col.alpha)) { if (length(col)!=length(col.alpha) & length(col)!=1 & length(col.alpha)!=1) { stop ("col & col.alpha are not in the same length!") } }
    if (!is.null(size)) { if (length(x.st)!=length(size) & length(size)!=1) { stop ("Parameter size is illegal!") } }
    if (is.null(tri)) { stop ("arrow parameters are not given!") }
    # reform data & plot
    if (is.null(col)) { col = mos_pars$bar_line_col }
    if (is.null(col.alpha)) { col.alpha = mos_pars$bar_line_alpha }
    if (!is.null(col) & !is.null(col.alpha)) { plot_col = col2hex(col, col.alpha*255) } else { plot_col = NULL }
    if (is.null(fill)) { fill = mos_pars$bar_fill_col }
    if (is.null(fill.alpha)) { fill.alpha = mos_pars$bar_fill_alpha }
    if (!is.null(fill) & !is.null(fill.alpha)) { plot_fill = col2hex(fill, fill.alpha*255) } else { plot_fill = NULL }
    if (is.null(size)) { plot_size = mos_pars$line_size } else { plot_size = size }
    for (j in seq(1, length(x.st))) {
      basetemp = mos_pars$baseline[mos_pars$frag_st[frag[j]]+x.st[j]]
      if (tri[1]>0) {
        x0 = (mos_pars$global_h_zoom*basetemp-h)*cos(basetemp)
        y0 = (mos_pars$global_h_zoom*basetemp-h)*sin(basetemp)
        x1 = x0+tri[2]/cos(tri[3]/180*pi)*cos(basetemp-tri[3]/180*pi-pi)
        y1 = y0+tri[2]/cos(tri[3]/180*pi)*sin(basetemp-tri[3]/180*pi-pi)
        x2 = x0+tri[2]/cos(tri[3]/180*pi)*cos(basetemp+tri[3]/180*pi-pi)
        y2 = y0+tri[2]/cos(tri[3]/180*pi)*sin(basetemp+tri[3]/180*pi-pi)
      } else {
        x0 = (mos_pars$global_h_zoom*basetemp+h)*cos(basetemp)
        y0 = (mos_pars$global_h_zoom*basetemp+h)*sin(basetemp)
        x1 = x0+tri[2]/cos(tri[3]/180*pi)*cos(basetemp-tri[3]/180*pi)
        y1 = y0+tri[2]/cos(tri[3]/180*pi)*sin(basetemp-tri[3]/180*pi)
        x2 = x0+tri[2]/cos(tri[3]/180*pi)*cos(basetemp+tri[3]/180*pi)
        y2 = y0+tri[2]/cos(tri[3]/180*pi)*sin(basetemp+tri[3]/180*pi)
      }
      if (!is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[j] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[j] } else { pborder = plot_col }
        polygon(c(x0,x1,x2),c(y0,y1,y2), col=pfill, lwd=psize, border=pborder )
      }
      if (is.null(plot_col) & !is.null(plot_fill)) {
        if (length(plot_fill)>1) { pfill = plot_fill[j] } else { pfill = plot_fill }
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        polygon(c(x0,x1,x2),c(y0,y1,y2), col=pfill, lwd=psize, border=NA )
      }
      if (!is.null(plot_col) & is.null(plot_fill)) {
        if (length(plot_size)>1) { psize = plot_size[j] } else { psize = plot_size }
        if (length(plot_col)>1) { pborder = plot_col[j] } else { pborder = plot_col }
        polygon(c(x0,x1,x2),c(y0,y1,y2), col=NA, lwd=psize, border=pborder )
      }
    }
  }
}
