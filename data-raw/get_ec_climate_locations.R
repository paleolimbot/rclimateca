

if(file.exists("data-raw/eclocs.csv")) unlink("data-raw/eclocs.csv")
download.file("ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv",
              "data-raw/eclocs.csv")
ecclimatelocs <- read.csv("data-raw/eclocs.csv", stringsAsFactors = FALSE, skip=3, check.names = FALSE)
devtools::use_data(ecclimatelocs, overwrite = TRUE)
unlink("data-raw/eclocs.csv")
