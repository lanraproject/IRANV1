run_app <- function() {
  appDir <- system.file("app", package = "IRANV1")
  
  if (appDir == "") {
    stop("Aplikasi tidak ditemukan")
  }
  
  shiny::runApp(appDir)
}
