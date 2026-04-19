check_update <- function() {
  try({
    # ambil versi lokal
    local_version <- as.character(utils::packageVersion("IRANV1"))
    
    # ambil versi dari DESCRIPTION di GitHub
    url <- "https://raw.githubusercontent.com/lanraproject/IRANV1/main/DESCRIPTION"
    desc <- readLines(url, warn = FALSE)
    
    remote_version <- sub("Version: ", "", desc[grep("^Version:", desc)])
    
    if (length(remote_version) > 0 && local_version != remote_version) {
      message("🚨 Versi baru tersedia!")
      message("Versi Anda: ", local_version)
      message("Versi terbaru: ", remote_version)
      message("Silakan update dengan:")
      message('remotes::install_github("lanraproject/IRANV1")')
    }
    
  }, silent = TRUE)
}
