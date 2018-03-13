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
  gsub('_\\d\\d\\d\\d$', '', names(df))
}


fetch_grad_rate <- function(end_year) {
  get_raw_grad_rate(end_year) %>%
    tidy_grad_rate()
}
