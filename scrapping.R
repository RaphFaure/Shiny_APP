install.packages("tidyverse")
library(tidyverse)

chemin = "C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\2023"
fichiers_csv = list.files(path = chemin, pattern = "\\.csv", full.names = TRUE)

fichier = read_csv("C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\2023\\INMET_SE_SP_A747_PRADOPOLIS_01-01-2023_A_31-10-2023.csv", ";")
print(fichier)

latt = c() 
long = c()
altitude = c()
