# This file contains custom helper functions

#' create_id()
#'
#' Helper function. Create custom id variable for each participant
#' @param var character vector: list of variables to be concatenated to form the new id variable
#' @param df dataframe: the dataframe for which to create a new id variable
#' @return character vector
#' @keywords internal
#' @noRd
#' @examples
#' create_id(df, c('first_name, last_name')

create_id <- function(df, var = c("first_name", "last_name")) {
    # check that var contains valid variable names from df var %in% colnames(df)
    
    # concatenate create a new column `x` with the three columns collapsed together
    out <- do.call(paste0, df[var])
    # Remove extra white spaces
    out <- gsub(" ", "", out)
    # Remove extra hyphens
    out <- gsub("-", "", out)
    # Change to lowercase id_name
    out <- tolower(out)
    
    return(out)
} 
