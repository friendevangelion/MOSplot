#' Title
#'
#' @param data data.frame: your input genomic data.
#' @param col_name character: column name.
#' @param ori_tag vector: a list of values contained in your input genomic data.
#' @param mapping_tag vector: a list of replaced values.
#' @param other_tag any basic data type or NULL (default is NULL): values contained in ori_tag but not in your input data.
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
mapping_data <- function(data=NULL, col_name=NULL, ori_tag=NULL, mapping_tag=NULL, other_tag=NULL) {
  if (is.null(data) | is.null(col_name) | is.null(ori_tag) | is.null(mapping_tag)) {
    stop ("There is some NULL input in your paramters!")
  }
  if (length(ori_tag)!=length(mapping_tag)) {
    stop ("ori_tag and mapping_tag must be in same length!")
  }
  if (mode(data)!="list") {
    stop ("Input data must be in data.frame or list format!")
  } else {
    if (!(col_name %in% names(data))) {
      stop (paste("There is no ", col_name, " in your input data!"))
    } else {
      tmp_data <- mapping_tag[match(data[[col_name]], ori_tag)]
      tmp_na_list <- which(is.na(tmp_data))
      if (length(tmp_na_list)>0) {
        if (is.null(other_tag)) {
          tmp_data[tmp_na_list] <- data[[col_name]][tmp_na_list]
          show ("NA produced by unmapped data, replaced them by original data!")
        } else {
          tmp_data[tmp_na_list] <- other_tag
          show ("NA produced by unmapped data, replaced them by other_tag!")
        }
      }
      data[[col_name]] <- tmp_data
      return (data)
    }
  }
}
