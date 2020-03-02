#' A dataset for mapping COG categories to color: 
#'
#' @format A data frame with 27 rows and 3 variables:
#' \describe{
#'   \item{categories}{COG categories}
#'   \item{hex}{hex color value}
#'   \item{rgb}{rgb color value}
#' }
"cog_color"
#' A parameter list: 
#'
#' @format A list with several arguments needed in MOSplot:
#' \describe{
#' }
"data_pars"
#' A dataset of GC-content and GC-skew value of all sliding windows in Psychroflexus torquis ATCC 700755 genome: 
#'
#' @format A data frame with 21605 rows and 5 variables:
#' \describe{
#'   \item{SeqID}{sequence id}
#'   \item{startPosition}{start position of sliding window}
#'   \item{endPosition}{end position of sliding window}
#'   \item{GCcontent}{GC-content of sliding window}
#'   \item{GCskew}{GC-skew of sliding window}
#' }
"data_gc"
#' A dataset of genes of Psychroflexus torquis ATCC 700755 genome: 
#'
#' @format A data frame with 3746 rows and 7 variables:
#' \describe{
#'   \item{SeqID}{sequence id}
#'   \item{Source}{start position of sliding window}
#'   \item{endPosition}{end position of sliding window}
#'   \item{GCcontent}{GC-content of sliding window}
#'   \item{GCskew}{GC-skew of sliding window}
#' }
"data_gene"
#' A dataset of highlighted genomic islands: 
#'
#' @format A data frame with 3746 rows and 7 variables:
#' \describe{
#'   \item{SeqID}{sequence id}
#'   \item{Source}{start position of sliding window}
#'   \item{endPosition}{end position of sliding window}
#'   \item{GCcontent}{GC-content of sliding window}
#'   \item{GCskew}{GC-skew of sliding window}
#' }
"data_marker"
