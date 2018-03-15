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

 rate_df <- rate_df %>%
   mutate_all(
     function(x) {
       #for greater than/ less than, drop the string, leave the number
       x <- gsub('GE|LE|GT|LT', '', x)
       x <- gsub('PS', NA_character_, x)
       x
     }
   )

 df <- bind_cols(constant_df, rate_df)

 extract_mean <- function(df, s) {
   df <- df %>% separate(s, "-", into=c("first", "second"))

   df$first <- as.numeric(df$first)
   df$second <- as.numeric(df$second)

   df$mean <- ifelse(!is.na(df$second),
                     (df$first + df$second)/2,
                     df$first)
   df %>% select(-first, -second)
 }
 extract_mean(df, 'all_rate')

  df %>%
    mutate_at(
      c('all_rate'),
      extract_mean
    )

 #calculate the midpoint for suppressed data with a range, eg. 70-75
 df[rate,] <- lapply(df[rate,], extract.mean)

 return(df)
}


fetch_grad_rate <- function(end_year) {
  get_raw_grad_rate(end_year) %>%
    tidy_grad_rate()
}

