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
  
  
  log$Error_Row<-as.integer(as.vector(unlist(log[1])))
  att$row<-c(1:nrow(att))
  att_log<-inner_join(att,log,by=c("row"="Error_Row"))
  att_log$idName<-paste0(att_log$First.Name,att_log$Last.Name)
  
  error_names<-tolower(unique(att_log$idName))
  error_names<-as.data.frame(error_names)
  names(error_names)="error_names"
  
  
  
  
  
  
  # Clean datasets
  eto <- clean_eto(eto)
  pro <- clean_pro(pro)
  
  # Compare names
  names <- compare(pro = pro, eto = eto, var = 'id_name')
  # Compare usi
  usi <- compare(pro = pro, eto = eto, var = 'usi')
  # Compare enrollment dates
  start <- compare(pro = pro, eto = eto, var = 'start')
  # Compare exit dates
  exit <- compare(pro = pro, eto = eto, var = 'exit')
  
  # Combine dataframes
  out <- dplyr::rbind_list(names, usi, start, exit)
  
  # Save results in a .csv file
  if(!file.exists('output')) {dir.create('output')}
  write.csv(out, file = 'output/pro_demographics.csv', na = "", row.names = FALSE)
  
  message("Check your working directory: Your results were saved in the output folder") 
}