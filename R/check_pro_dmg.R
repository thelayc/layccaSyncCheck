#' check_pro_dmg()
#'
#' This function takes two .csv files: 1) ProActive full report, and 2) "[LAYC CA] Students usi report" as inputs, and return a list of records that are not in sync.
#' @param pro_file character vector: the name of the file containing data from ProActive. Can be a full or relative path to the .csv file.
#' @param eto_file character vector: the name of the file containing data from ETO. Can be a full or relative path to the .csv file.
#' @return data frame
#' @export
#' @examples
#' check_pro_dmg(pro_file = './data/proactive.csv', eto_file = './data/eto.csv')

check_pro_dmg <- function(pro_file = 'proactive.csv', eto_file = 'eto_usi.csv') {
  # Load .csv files
  pro <- load_csv(pro_file)
  eto <- load_csv(eto_file)
  
  # Clean datasets
  eto <- clean_eto(eto)
  pro <- clean_pro(pro)
  
  # Compare names
  names <- compare_names(pro = pro, eto = eto)
  
  return(list(pro, eto))
  
}


#' clean_eto()
#'
#' Helper function. Clean the eto dataframe.
#' @param df dataframe: from eto_file input
#' @return dataframe
#' @keywords internal
#' @noRd
#' @examples
#' clean_eto(df = eto)

clean_eto <- function(df) {
  # Remove empty rows
  df <- dplyr::filter(df, is.na(id) == FALSE)
  # Replace missing USI by 0
  df$usi[is.na(df$usi)] <- 0
  # Create the id_name variable: Will serve as the main key to merge datasets
  df$id_name <- create_id(df, c("first_name", "last_name"))
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
#' @examples
#' clean_pro(df = pro)

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
  # add a "source"database" variable to identify where the data was extracted from
  df$database <- "proactive"
  # Ensure the exit var is of type character (can be loaded as logical, if the var is empty)
  df$exit <- as.character(df$exit)
  
  return(df)  
}


#' compare_names()
#'
#' Helper function. Compare students names in both database
#' @param pro dataframe: proactive dataframe
#' @param eto dataframe: eto dataframe
#' @return dataframe
#' @keywords internal
#' @noRd
#' @examples
#' compare_names(pro = pro, eto = eto)

compare_names <- function(pro = pro, eto = eto) {
  # identify names that are in eto, but not in proactive
  eto_only <- dplyr::anti_join(eto, pro, by = "id_name")
  # identify names that are in proactive, but not in eto
  pro_only <- dplyr::anti_join(pro, eto, by = "id_name")
  # Combine both dataframe
  out <- rbind_list(eto_only, pro_only)
  # Sort dataframe by names
  out <- dplyr::arrange(out, last_name, first_name)
  
  return(out)  
}