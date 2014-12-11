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


#' load_csv()
#'
#' Helper function. Set default options for the read.csv function
#' @param file character vector: the name of the file which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd(). Tilde-expansion is performed where supported. This can be a compressed file (see file).
#' @return data frame
#' @keywords internal
#' @noRd
#' @examples
#' load_csv('./data/my_csv_file.csv')

load_csv <- function(file) {
  out <- read.csv(file, , header = TRUE, stringsAsFactors = FALSE, na.strings = "")
  
  return(out)
}
