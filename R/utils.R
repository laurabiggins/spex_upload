#' process_metadata
#' 
#' Takes in metadata file and produces a list containing the original meta file
#' along with some summary information.
#'
#' @param meta_dataset imported metadata file
#' @param main_sample_name column name for the sample names
#' @param other_cols the other columns that contain metadata that we want to use
#'
#' @return list of 3 
#' * sample names
#' * nested list of counts of each variable
#' * whole metadata file
#' @export
#'
#' @examples
process_metadata <- function(meta_dataset, main_sample_name = "sample_name", other_cols = NA){
  
  meta_processed <- list(
    sample_names = meta_dataset[[main_sample_name]],
    meta_summary = list(
      sample_name = dplyr::count(meta_dataset, .data[[main_sample_name]])
    ),
    meta_all = meta_dataset
  )
  
  if(length(other_cols) > 0 & is.character(other_cols)){
    for (i in other_cols) {
      meta_processed$meta_summary[[i]] = dplyr::count(meta_dataset, .data[[i]])
    }
  }
  meta_processed
}
