check_update <- function() {
  try({
    # ambil versi lokal
    local_version <- as.character(utils::packageVersion("IRANV1"))
    
    # ambil versi dari GitHub (DESCRIPTION)
    url <- "https://raw.githubusercontent.com/lanraproject/IRANV1/main/DESCRIPTION"
    desc <- readLines(url, warn = FALSE)
    
    remote_version <- sub("Version: ", "", desc[grep("^Version:", desc)])
    
    if (length(remote_version) > 0 && local_version != remote_version) {
      stop("Versi Anda sudah usang.\nSilakan update dulu:\nremotes::install_github('lanraproject/IRANV1')")
    }
    
  }, silent = TRUE)
}
