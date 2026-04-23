# install sebagai paket
install.packages("remotes")
remotes::install_github("lanraproject/IRANV1") #pasang paket IRANV1
IRANV1::run_app() #jalankan paket

# jalankan langsung melalui GitHub
shiny::runGitHub("IRANV1", "lanraproject") #lebih ringkas
