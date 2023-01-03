#' @param z Vector. Specifies column for \code{z} axis. This column can be broken into intervals 
#' using \code{zBr} and \code{zBrMethod} for numerical variables and using \code{zLevel} for 
#' categorical variables. Subsets of data column corresponding to \code{z} column breaks are 
#' then plotted with different colors. Input methods:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. } 
#' } 
#' First found match is used when multivalued vector is provided.
