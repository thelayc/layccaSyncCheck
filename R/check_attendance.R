#' check_attendance()
#'
#' This function takes two files: 1) ProActive attendance upload log (text file), and 2) "[LAYC CA] DCPCSB attendance report" as inputs, and return a list of attendance records that were not processed correctly during the upload.
#' @param log character vector: the name of the file containing the upload log from ProActive. Can be a full or relative path to the .txt file.
#' @param eto_file character vector: the name of the file containing attendance data from ETO. Can be a full or relative path to the .csv file.
#' @return data frame
#' @export
#' @examples
#' check_attendance(log = './data/upload_log.txt', att_file = './data/attendance_upload.csv')

check_attendance <- function(log = 'upload_log.txt', att_file = 'attendance_upload.csv') {
  # Load .csv files
  log <- data.frame(readLines(log), stringsAsFactors = FALSE)
  att <- load_csv(att_file)
  
  # Clean datasets
  log <- clean_log(log)
  att <- clean_attendance(att)
  
  # Identify problematic attendance records in original .csv file
  out <- dplyr::left_join(log, att, by = 'error_row')
  
  # Save results in a .csv file
  if(!file.exists('output')) {dir.create('output')}
  write.csv(out, file = 'output/attendance_check.csv', na = "", row.names = FALSE)
  
  message("Check your working directory: Your results were saved in the output folder") 
}