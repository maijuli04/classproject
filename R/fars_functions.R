#' Read in a .csv file and format as a tibble dataframe
#'
#' @param filename The path name for the file you wish to read in.
#' @return A tibble dataframe of the read in data.
#' @note Will return a message if the file cannot be found.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
   if(!file.exists(filename))
      stop("file '", filename, "' does not exist")
   data <- suppressMessages({
      readr::read_csv(filename, progress = FALSE)
   })
   dplyr::tbl_df(data)
}


#' Create a filename and set the year to be used in the name "accident_date.csv.bz2".
#'
#' @param year The year to be referenced in the file name
#' @return A character string of the filename.
make_filename <- function(year) {
   year <- as.integer(year)
   sprintf("accident_%d.csv.bz2", year)
}


#' Read in the data from the Fatality Analysis Reporting System for the years specified.
#'
#' @param years A vector of years
#' @return Mutiple tibble dataframes of data for the requested years.
#' @details For each year, the fears_read_years function will search for a corresponding file with the filename formatted as "accident_date.csv.bz2", insterting the year for date. If the file does not exist, it will return an error indicate the year is invalid.
#' @importFrom dplyr mutate select
#' @import magrittr
#' @examples
#' \dontrun{
#' these_years <- c(2013,2014,2015)
#' fars_read_years(these_years)}
fars_read_years <- function(years) {
   lapply(years, function(year) {
      file <- make_filename(year)
      tryCatch({
         dat <- fars_read(file)
         dplyr::mutate(dat, year = year) %>%
            dplyr::select(MONTH, year)
      }, error = function(e) {
         warning("invalid year: ", year)
         return(NULL)
      })
   })
}


#' Create a summary table of the number of accidents per year, according to the Fatality #' Analysis Report System for the years specified.
#'
#' @param years A vector of years
#' @return A table of counts of accidents for each year requested by month.
#' @details The function will read in all data files for the years requested and produce a summary table for the number of accidents that occurred each month for the years specified.
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' these_years <- c(2013,2014,2015)
#' fars_summarize_years(these_years)}
fars_summarize_years <- function(years) {
   dat_list <- fars_read_years(years)
   dplyr::bind_rows(dat_list) %>%
      dplyr::group_by(year, MONTH) %>%
      dplyr::summarize(n = n()) %>%
      tidyr::spread(year, n)
}

#' Plot accident locations on a US map using data from the Fatality Analysis Report System.
#'
#' @param state.num The state identification number used by the Fatality Analysis Report System.
#' @param year The year for which the data are to be plotted.
#' @return A state map with points indicate accident locations for the specified year.
#' @details The function will search and retrive the requested data from the Fatality Analysis Report System.  An error will be returned if there state number is invalid.  If there is no accident data to report for the specified year, this will be reported without a map.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#'  fars_map_state(42,2013)}
fars_map_state <- function(state.num, year) {
   filename <- make_filename(year)
   data <- fars_read(filename)
   state.num <- as.integer(state.num)

   if(!(state.num %in% unique(data$STATE)))
      stop("invalid STATE number: ", state.num)
   data.sub <- dplyr::filter(data, STATE == state.num)
   if(nrow(data.sub) == 0L) {
      message("no accidents to plot")
      return(invisible(NULL))
   }
   is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
   is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
   with(data.sub, {
      maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                xlim = range(LONGITUD, na.rm = TRUE))
      graphics::points(LONGITUD, LATITUDE, pch = 46)
   })
}
