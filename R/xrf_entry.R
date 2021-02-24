#'@title Graphical User Interface for XRF data entry
#'
#'@description This package will help you to enter XRF data easily to the database.
#'@param xrf_entry Launch the XRF data entry interface.
#'@export
#'@keywords
#'@seealso
#'@return
#'@aliases
#'@examples xrf_entry()
xrf_entry <- function() {
  shiny::runApp(system.file("application", package="XRFdatabase"))
}
