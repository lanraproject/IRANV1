check_update <- function() {
  try({
    local_version <- as.character(utils::packageVersion("IRANV1"))

    url <- "https://raw.githubusercontent.com/lanraproject/IRANV1/main/DESCRIPTION"
    desc <- readLines(url, warn = FALSE)
    
    remote_version_line <- desc[grep("^Version:", desc)]

    if (length(remote_version_line) == 0) return()

remote_version <- sub("Version: ", "", remote_version_line)
    
    if (length(remote_version) > 0 && local_version != remote_version) {
      stop("Versi Anda sudah usang.\nSilakan update dulu:\nremotes::install_github('lanraproject/IRANV1')")
    }
    
  }, silent = TRUE)
}
