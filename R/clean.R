#' clean_eto()
#'
#' Helper function. Clean the eto dataframe.
#' @param df dataframe: from eto_file input
#' @return dataframe
#' @keywords internal
#' @noRd

clean_eto <- function(df) {
  # Remove empty rows
  df <- dplyr::filter(df, is.na(id) == FALSE)
  # Replace missing USI by 0
  df$usi[is.na(df$usi)] <- 0
  # Create the id_name variable: Will serve as the main key to merge datasets
  df$id_name <- create_id(df, c("first_name", "last_name"))
  # Format dates variables
  df$start <- lubridate::mdy(df$start)
  df$start <- as.character(df$start)
  df$exit <- lubridate::mdy(df$exit)
  df$exit <- as.character(df$exit)
  # add a "source"database" variable to identify where the data was extracted from
  df$database <- "eto"
  
  return(df)  
}


#' clean_pro()
#'
#' Helper function. Clean the pro dataframe.
#' @param df dataframe: from pro_file input
#' @return dataframe
#' @keywords internal
#' @noRd

clean_pro <- function(df) {
  # select and rename variables
  df <- dplyr::select(df,
                      usi = USI,
                      first_name = STUDENT_FIRST_NAME,
                      last_name = STUDENT_LAST_NAME, 
                      dob = BIRTH_DATE,
                      start = ENROLL_DATE,
                      exit = EXIT_DATE)
  # Replace missing USI by 0
  df$usi[is.na(df$usi)] <- 0
  # Create the id_name variable: Will serve as the main key to merge datasets
  df$id_name <- create_id(df, c("first_name", "last_name"))
  # Format dates variables
  df$start <- lubridate::mdy(df$start)
  df$start <- as.character(df$start)
  df$exit <- lubridate::mdy(df$exit)
  df$exit <- as.character(df$exit)
  # add a "source"database" variable to identify where the data was extracted from
  df$database <- "proactive"
  # Ensure the exit var is of type character (can be loaded as logical, if the var is empty)
  df$exit <- as.character(df$exit)
  
  return(df)  
}


#' clean_log()
#'
#' Helper function. Clean the proactive attendance log.
#' @param df dataframe: from log input
#' @return dataframe
#' @keywords internal
#' @noRd

clean_log <- function(df) {
  # Remove irrelevant rows
  keep <- stringr::str_detect(df[,1], 'ERROR:')
  df <- dplyr::filter(df, keep)
  
  # Extract row numbers
  df$error_row <- stringr::str_extract(df[ ,1], "\\#[0-9]{1,4}\\:")
  df$error_row <- stringr::str_replace(df$error_row, "#", "")
  df$error_row <- stringr::str_replace(df$error_row, ":", "")
  
  # Extract error message
  df$error_info <- stringr::str_extract(df[ ,1], "ERROR: Row #(.*)")
  df$error_info <- stringr::str_replace(df$error_info, "(.*)ERROR: Row #", "")
  df$error_info <- stringr::str_replace(df$error_info, "(.*)[0-9]: ", "")
  
  # Adjust error_row values
  df$error_row <- as.numeric(df$error_row) - 1
  
  # Select relevant columns
  df <- dplyr::select(df, error_row:error_info)
  
  return(df)  
}


#' clean_attedance()
#'
#' Helper function. Clean the eto attendance file.
#' @param df dataframe: from eto_file input
#' @return dataframe
#' @keywords internal
#' @noRd

clean_attendance <- function(df) {
  # Rename columns
  df <- dplyr::select(df,
                      usi = USI,
                      first_name = First.Name,
                      last_name = Last.Name,
                      date = Date.of.Attendance,
                      att_type = Attendance.Type,
                      att_class = Attendance.Classification
                      )
  # Add columns containing row number
  df$error_row <- row.names(df)
  
  return(df)  
}