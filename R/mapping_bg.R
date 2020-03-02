#' Title
#'
#' @param mos_pars arguments list: generated in function load_parameters.
#'
#' @return a new spiral figure
#' @export
#'
#' @examples
#'
mapping_bg <- function(mos_pars=NULL) {
  if (is.null(mos_pars)) {
    stop ("mos_pars is NULL!")
  }
  if (!("baseline" %in% names(mos_pars))) { stop ("There is no 'baseline' tag in your parameters, maybe there is something wrong in function 'load_parameters' or 'update_parameters'!") }
  else {
    if (length(mos_pars$baseline)<sum(mos_pars$panel_x_frag) + length(mos_pars$panel_x_frag)*mos_pars$panel_x_blank - mos_pars$panel_x_blank) {
      stop ("The length of 'baseline' is not match with the given parameters 'panel_x_frag' and 'panel_x_blank', maybe there is something wrong in function 'load_parameters' or 'update_parameters'!")
    }
  }
  pi = 3.1415926
  h = mos_pars$global_h_total/2
  par(mar=c(0, 0, 0, 0))
  plot(c(-mos_pars$size, mos_pars$size), c(-mos_pars$size, mos_pars$size), type="n", ann=F, bty="n", xaxt="n", yaxt="n")
  # init panel
  for (k in seq(1, length(mos_pars$panel_x_frag))) {
    basetemp = mos_pars$baseline[seq(mos_pars$frag_st[k], mos_pars$frag_en[k], by=mos_pars$panel_x_texture)]
    if (basetemp[length(basetemp)]!=mos_pars$baseline[mos_pars$frag_en[k]]) { basetemp = c(basetemp, mos_pars$baseline[mos_pars$frag_en[k]]) }
    for (i in seq(1, length(mos_pars$panel_y_height))) {
      if (mos_pars$global_directory) {
        temp = rbind(cbind((mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i])*cos(basetemp),
                           (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i])*sin(basetemp)),
                     cbind(rev(mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[i])*cos(rev(basetemp)),
                           rev(mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_en[i])*sin(rev(basetemp))))
      } else {
        temp = rbind(cbind((mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i])*cos(basetemp),
                           (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i])*sin(basetemp)),
                     cbind(rev(mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[i])*cos(rev(basetemp)),
                           rev(mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_en[i])*sin(rev(basetemp))))
      }
      if (!is.null(mos_pars$panel_color)) { polygon(temp, border = NA, col = mos_pars$panel_color) }
      if (!is.null(mos_pars$panel_border_color)) { polygon(temp, border = mos_pars$panel_border_color, lwd = mos_pars$panel_border_size, col = NA) }
    }
  }
  # plot x-axis separator
  for (k in seq(1, length(mos_pars$panel_x_frag))) {
    for (i in seq(0, floor(mos_pars$panel_x_frag[k]/mos_pars$panel_x_block))) {
      j = i*mos_pars$panel_x_block+mos_pars$frag_st[k]
      polygon(c((mos_pars$global_h_zoom*mos_pars$baseline[j]-h)*cos(mos_pars$baseline[j])+mos_pars$grid_x_width/2*sin(mos_pars$baseline[j]), (mos_pars$global_h_zoom*mos_pars$baseline[j]-h)*cos(mos_pars$baseline[j])-mos_pars$grid_x_width/2*sin(mos_pars$baseline[j]),
                (mos_pars$global_h_zoom*mos_pars$baseline[j]+h)*cos(mos_pars$baseline[j])-mos_pars$grid_x_width/2*sin(mos_pars$baseline[j]), (mos_pars$global_h_zoom*mos_pars$baseline[j]+h)*cos(mos_pars$baseline[j])+mos_pars$grid_x_width/2*sin(mos_pars$baseline[j])),
              c((mos_pars$global_h_zoom*mos_pars$baseline[j]-h)*sin(mos_pars$baseline[j])-mos_pars$grid_x_width/2*cos(mos_pars$baseline[j]), (mos_pars$global_h_zoom*mos_pars$baseline[j]-h)*sin(mos_pars$baseline[j])+mos_pars$grid_x_width/2*cos(mos_pars$baseline[j]),
                (mos_pars$global_h_zoom*mos_pars$baseline[j]+h)*sin(mos_pars$baseline[j])+mos_pars$grid_x_width/2*cos(mos_pars$baseline[j]), (mos_pars$global_h_zoom*mos_pars$baseline[j]+h)*sin(mos_pars$baseline[j])-mos_pars$grid_x_width/2*cos(mos_pars$baseline[j])),
              border = NA, col = mos_pars$grid_x_color)
    }
  }
  # plot y-axis separator
  for (k in seq(1, length(mos_pars$panel_x_frag))) {
    basetemp = mos_pars$baseline[seq(mos_pars$frag_st[k], mos_pars$frag_en[k], by=mos_pars$panel_x_texture)]
    if (basetemp[length(basetemp)]!=mos_pars$baseline[mos_pars$frag_en[k]]) { basetemp = c(basetemp, mos_pars$baseline[mos_pars$frag_en[k]]) }
    for (i in seq(1, length(mos_pars$panel_grid_flag))) {
      if (mos_pars$panel_grid_flag[i]) {
        if (mos_pars$global_directory) {
          temp = rbind(cbind((mos_pars$global_h_zoom*basetemp-h+(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2-mos_pars$grid_y_width*1)*cos(basetemp),
                             (mos_pars$global_h_zoom*basetemp-h+(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2-mos_pars$grid_y_width*1)*sin(basetemp)),
                       cbind(rev(mos_pars$global_h_zoom*basetemp-h+(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2+mos_pars$grid_y_width*1)*cos(rev(basetemp)),
                             rev(mos_pars$global_h_zoom*basetemp-h+(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2+mos_pars$grid_y_width*1)*sin(rev(basetemp))))
        } else {
          temp = rbind(cbind((mos_pars$global_h_zoom*basetemp+h-(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2+mos_pars$grid_y_width*1)*cos(basetemp),
                             (mos_pars$global_h_zoom*basetemp+h-(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2+mos_pars$grid_y_width*1)*sin(basetemp)),
                       cbind(rev(mos_pars$global_h_zoom*basetemp+h-(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2-mos_pars$grid_y_width*1)*cos(rev(basetemp)),
                             rev(mos_pars$global_h_zoom*basetemp+h-(mos_pars$panel_st[i]+mos_pars$panel_en[i])/2-mos_pars$grid_y_width*1)*sin(rev(basetemp))))
        }
        polygon(temp, border = NA, col = mos_pars$grid_y_color)
      }
      if (mos_pars$panel_grid_sub_flag[i]) {
        if (mos_pars$global_directory) {
          temp = rbind(cbind((mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4-mos_pars$grid_y_width/2)*cos(basetemp),
                             (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4-mos_pars$grid_y_width/2)*sin(basetemp)),
                       cbind(rev(mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4+mos_pars$grid_y_width/2)*cos(rev(basetemp)),
                             rev(mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4+mos_pars$grid_y_width/2)*sin(rev(basetemp))))
          polygon(temp, border = NA, col = mos_pars$grid_y_color)
          temp = rbind(cbind((mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4-mos_pars$grid_y_width/2)*cos(basetemp),
                             (mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4-mos_pars$grid_y_width/2)*sin(basetemp)),
                       cbind(rev(mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4+mos_pars$grid_y_width/2)*cos(rev(basetemp)),
                             rev(mos_pars$global_h_zoom*basetemp-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4+mos_pars$grid_y_width/2)*sin(rev(basetemp))))
        } else {
          temp = rbind(cbind((mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4+mos_pars$grid_y_width/2)*cos(basetemp),
                             (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4+mos_pars$grid_y_width/2)*sin(basetemp)),
                       cbind(rev(mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4-mos_pars$grid_y_width/2)*cos(rev(basetemp)),
                             rev(mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*1/4-mos_pars$grid_y_width/2)*sin(rev(basetemp))))
          polygon(temp, border = NA, col = mos_pars$grid_y_color)
          temp = rbind(cbind((mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4+mos_pars$grid_y_width/2)*cos(basetemp),
                             (mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4+mos_pars$grid_y_width/2)*sin(basetemp)),
                       cbind(rev(mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4-mos_pars$grid_y_width/2)*cos(rev(basetemp)),
                             rev(mos_pars$global_h_zoom*basetemp+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*3/4-mos_pars$grid_y_width/2)*sin(rev(basetemp))))
        }
        polygon(temp, border = NA, col = mos_pars$grid_y_color)
      }
    }
  }
  # plot x-tag
  if (is.null(mos_pars$tag_x_list) | is.null(mos_pars$tag_x_pos)) {
    if (!is.null(mos_pars$tag_x_add)) {
      if (mos_pars$tag_x_add>0) {
        for (k in seq(1, length(mos_pars$panel_x_frag))) {
          for (i in seq(0, floor(mos_pars$panel_x_frag[k]/mos_pars$panel_x_block/mos_pars$tag_x_span))) {
            j = i*mos_pars$panel_x_block*mos_pars$tag_x_span+mos_pars$frag_st[k]
            text = as.character(round(i*mos_pars$tag_x_add*mos_pars$panel_x_block*mos_pars$tag_x_span*10^mos_pars$tag_x_digit+mos_pars$tag_x_init[k]))
            tag_x_int = substr(text, 1, nchar(text)-mos_pars$tag_x_digit)
            mos_pars$tag_x_frac = substr(text, nchar(text)-mos_pars$tag_x_digit+1, nchar(text))
            if (tag_x_int=="") { tag_x_int = "0" }
            if (mos_pars$tag_x_frac=="0") { mos_pars$tag_x_frac = paste(rep("0",mos_pars$tag_x_digit), sep="", collapse="")}
            if (mos_pars$tag_x_digit) {
              tag = paste(tag_x_int, ".", mos_pars$tag_x_frac, mos_pars$tag_x_unit, sep="")
            } else {
              tag = paste(tag_x_int, mos_pars$tag_x_unit, sep="")
            }
            if (mos_pars$global_directory) {
              if (mos_pars$tag_x_arc) {
                arctext(x=tag, radius=mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize,
                        middle=mos_pars$baseline[j], cex=mos_pars$tag_x_size, col=mos_pars$tag_x_col, clockwise=F)
              } else {
                text((mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize)*cos(mos_pars$baseline[j]),
                     (mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize)*sin(mos_pars$baseline[j]),
                     label=tag, cex=mos_pars$tag_x_size, col=mos_pars$tag_x_col, srt=mos_pars$baseline[j]/pi*180+90)
              }
            } else {
              if (mos_pars$tag_x_arc) {
                arctext(x=tag, radius=mos_pars$global_h_zoom*mos_pars$baseline[j]-h-mos_pars$tag_x_offsize,
                        middle=mos_pars$baseline[j], cex=mos_pars$tag_x_size, col=mos_pars$tag_x_col, clockwise=T)
              } else {
                text((mos_pars$global_h_zoom*mos_pars$baseline[j]-h-mos_pars$tag_x_offsize)*cos(mos_pars$baseline[j]),
                     (mos_pars$global_h_zoom*mos_pars$baseline[j]-h-mos_pars$tag_x_offsize)*sin(mos_pars$baseline[j]),
                     label=tag, cex=mos_pars$tag_x_size, col=mos_pars$tag_x_col, srt=mos_pars$baseline[j]/pi*180-90)
              }
            }
          }
        }
      } else {
        warning ("tag_x_add is not illegal! There will be no x tag!")
      }
    } else {
      warning ("tag_x_add is NULL! There will be no x tag!")
    }
  } else {
    for (t in seq(1, length(mos_pars$tag_x_list))) {
      j = mos_pars$frag_st[mos_pars$tag_x_frag[t]]-1+mos_pars$tag_x_pos[t]
      if (mos_pars$global_directory) {
        if (mos_pars$tag_x_arc) {
          arctext(x=tag, radius=mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize,
                  middle=mos_pars$baseline[j], cex=mos_pars$tag_x_list[t], col=mos_pars$tag_x_col, clockwise=F)
        } else {
          text((mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize)*cos(mos_pars$baseline[j]),
               (mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize)*sin(mos_pars$baseline[j]),
               label=tag, cex=mos_pars$tag_x_list[t], col=mos_pars$tag_x_col, srt=mos_pars$baseline[j]/pi*180+90)
        }
      } else {
        if (mos_pars$tag_x_arc) {
          arctext(x=tag, radius=mos_pars$global_h_zoom*mos_pars$baseline[j]+h+mos_pars$tag_x_offsize,
                  middle=mos_pars$baseline[j], cex=mos_pars$tag_x_list[t], col=mos_pars$tag_x_col, clockwise=T)
        } else {
          text((mos_pars$global_h_zoom*mos_pars$baseline[j]-h-mos_pars$tag_x_offsize)*cos(mos_pars$baseline[j]),
               (mos_pars$global_h_zoom*mos_pars$baseline[j]-h-mos_pars$tag_x_offsize)*sin(mos_pars$baseline[j]),
               label=tag, cex=mos_pars$tag_x_list[t], col=mos_pars$tag_x_col, srt=mos_pars$baseline[j]/pi*180-90)
        }
      }
    }
  }
  # plot y-tag
  if (is.null(mos_pars$tag_y_st) | is.null(mos_pars$tag_y_en)) {
    warning ("tag_y_st or tag_y_en is NULL! There will be no y tag!")
  } else {
    for (i in seq(1, length(mos_pars$panel_y_height))) {
      if (mos_pars$global_directory) {
        if (mos_pars$tag_y_arc) {
          if (mos_pars$tag_y_en[i]!="NA") { arctext(x=mos_pars$tag_y_en[i], radius=mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((1-1)*mos_pars$tag_y_zoom/2+(1-mos_pars$tag_y_zoom)/2),
                                                    center=c(mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1])),
                                                    end=mos_pars$baseline[1], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, clockwise=F) }
          if (mos_pars$tag_y_mid[i]!="NA") { arctext(x=mos_pars$tag_y_mid[i], radius=mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((2-1)*mos_pars$tag_y_zoom/2+(1-mos_pars$tag_y_zoom)/2),
                                                    center=c(mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1])),
                                                    end=mos_pars$baseline[1], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, clockwise=F) }
          if (mos_pars$tag_y_st[i]!="NA") { arctext(x=mos_pars$tag_y_st[i], radius=mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((3-1)*mos_pars$tag_y_zoom/2+(1-mos_pars$tag_y_zoom)/2),
                                                    center=c(mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1])),
                                                    end=mos_pars$baseline[1], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, clockwise=F) }
        } else {
          if (mos_pars$tag_y_en[i]!="NA") { text((mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((1-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*cos(mos_pars$baseline[1])+mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),
                                                 (mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((1-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*sin(mos_pars$baseline[1])-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1]),
                                                 label=mos_pars$tag_y_en[i], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, srt=mos_pars$baseline[1]/pi*180+90) }
          if (mos_pars$tag_y_mid[i]!="NA") { text((mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((2-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*cos(mos_pars$baseline[1])+mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),
                                                 (mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((2-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*sin(mos_pars$baseline[1])-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1]),
                                                 label=mos_pars$tag_y_mid[i], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, srt=mos_pars$baseline[1]/pi*180+90) }
          if (mos_pars$tag_y_st[i]!="NA") { text((mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((3-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*cos(mos_pars$baseline[1])+mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),
                                                 (mos_pars$global_h_zoom*mos_pars$baseline[1]-h+mos_pars$panel_st[i]+(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((3-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*sin(mos_pars$baseline[1])-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1]),
                                                 label=mos_pars$tag_y_st[i], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, srt=mos_pars$baseline[1]/pi*180+90) }
        }
      } else {
        if (mos_pars$tag_y_arc) {
          if (mos_pars$tag_y_en[i]!="NA") { arctext(x=mos_pars$tag_y_en[i], radius=mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((1-1)*mos_pars$tag_y_zoom/2+(1-mos_pars$tag_y_zoom)/2),
                                                    center=c(mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1])),
                                                    end=mos_pars$baseline[1], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, clockwise=T) }
          if (mos_pars$tag_y_mid[i]!="NA") { arctext(x=mos_pars$tag_y_mid[i], radius=mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((2-1)*mos_pars$tag_y_zoom/2+(1-mos_pars$tag_y_zoom)/2),
                                                     center=c(mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1])),
                                                     end=mos_pars$baseline[1], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, clockwise=T) }
          if (mos_pars$tag_y_st[i]!="NA") { arctext(x=mos_pars$tag_y_st[i], radius=mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((3-1)*mos_pars$tag_y_zoom/2+(1-mos_pars$tag_y_zoom)/2),
                                                    center=c(mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),-mos_pars$tag_y_offsize*cos(mos_pars$baseline[1])),
                                                    end=mos_pars$baseline[1], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, clockwise=T) }
        } else {
          if (mos_pars$tag_y_en[i]!="NA") { text((mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((1-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*cos(mos_pars$baseline[1])-mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),
                                                 (mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((1-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*sin(mos_pars$baseline[1])+mos_pars$tag_y_offsize*cos(mos_pars$baseline[1]),
                                                 label=mos_pars$tag_y_en[i], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, srt=mos_pars$baseline[1]/pi*180-90) }
          if (mos_pars$tag_y_mid[i]!="NA") { text((mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((2-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*cos(mos_pars$baseline[1])-mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),
                                                  (mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((2-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*sin(mos_pars$baseline[1])+mos_pars$tag_y_offsize*cos(mos_pars$baseline[1]),
                                                  label=mos_pars$tag_y_mid[i], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, srt=mos_pars$baseline[1]/pi*180-90) }
          if (mos_pars$tag_y_st[i]!="NA") { text((mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((3-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*cos(mos_pars$baseline[1])-mos_pars$tag_y_offsize*sin(mos_pars$baseline[1]),
                                                 (mos_pars$global_h_zoom*mos_pars$baseline[1]+h-mos_pars$panel_st[i]-(mos_pars$panel_en[i]-mos_pars$panel_st[i])*((3-1)*mos_pars$tag_y_zoom/2)+(1-mos_pars$tag_y_zoom)/2)*sin(mos_pars$baseline[1])+mos_pars$tag_y_offsize*cos(mos_pars$baseline[1]),
                                                 label=mos_pars$tag_y_st[i], cex=mos_pars$tag_y_size, col=mos_pars$tag_y_col, srt=mos_pars$baseline[1]/pi*180-90) }
        }
      }
    }
  }
}
