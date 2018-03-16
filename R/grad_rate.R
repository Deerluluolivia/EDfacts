#' Get Raw Graduation Rate
#'
#' @param end_year four digit number represents the end of school year
#'
#' @return dataframe for a specific graduation rate
#' @export

get_raw_grad_rate <- function(end_year){

  #build url
  grate_url <-ifelse(end_year == 2015
                     ,paste0(
                       'https://www2.ed.gov/about/inits/ed/edfacts/data-files/acgr-release2-lea-sy', end_year-1, '-', substr(end_year, 3, 4), '.csv')
                     ,paste0(
                       'https://www2.ed.gov/about/inits/ed/edfacts/data-files/acgr-lea-sy', end_year-1, '-', substr(end_year, 3, 4), '.csv'))

  print(grate_url)
  #download
  tname <- tempfile(pattern = "grate", tmpdir = tempdir(), fileext = ".csv")
  tdir <- tempdir()
  downloader::download(grate_url, dest = tname, mode = "wb")

  #read file
  read_csv(tname) %>%
    janitor::clean_names() %>%
    mutate(end_year = end_year)
}


tidy_grad_rate <- function(df) {
 df <- as.data.frame(df)
 #get rid of the ending year in variable names, eg. _1011
 names(df) <-  gsub('_\\d\\d\\d\\d$', '', names(df))

 #get rid of "date_cur"
 df$date_cur <- NULL

 #deal with suppression rules. GE is >=, LE is <=, GT is >, LT is <, PS is suppressed
 rate_cols <- grepl('_rate', names(df))
 constant_df <- df[, !rate_cols]
 rate_df <- df[, rate_cols]
 cal_mean <- function(x)mean(as.numeric(x))

 rate_df <- rate_df %>%
   mutate_all(
     function(x) {
#for greater than/ less than, drop the string, leave the number
       x <- gsub('GE|LE|GT|LT', '', x)
#for suppressed "PS", replac with NA
       x <- gsub('PS', NA_character_, x)
       x
     }
   )
#for range value 10-19, 15-20, calculate the mean of the range
 rate_df$all_rate <-  unlist(map(strsplit(rate_df$all_rate, "-"), cal_mean))
 rate_df$mam_rate <-  unlist(map(strsplit(rate_df$mam_rate, "-"), cal_mean))
 rate_df$mas_rate <-  unlist(map(strsplit(rate_df$mas_rate, "-"), cal_mean))
 rate_df$mbl_rate <-  unlist(map(strsplit(rate_df$mbl_rate, "-"), cal_mean))
 rate_df$mhi_rate <-  unlist(map(strsplit(rate_df$mhi_rate, "-"), cal_mean))
 rate_df$mtr_rate <-  unlist(map(strsplit(rate_df$mtr_rate, "-"), cal_mean))
 rate_df$mwh_rate <-  unlist(map(strsplit(rate_df$mwh_rate, "-"), cal_mean))
 rate_df$cwd_rate <-  unlist(map(strsplit(rate_df$cwd_rate, "-"), cal_mean))
 rate_df$ecd_rate <-  unlist(map(strsplit(rate_df$ecd_rate, "-"), cal_mean))
 rate_df$lep_rate <-  unlist(map(strsplit(rate_df$lep_rate, "-"), cal_mean))

#bind rate variables and count variables togather
 df <- bind_cols(constant_df, rate_df)

 return(df)
}

#combine the get_raw and tidy function into one function
fetch_grad_rate <- function(end_year) {
  get_raw_grad_rate(end_year) %>%
    tidy_grad_rate()
}

