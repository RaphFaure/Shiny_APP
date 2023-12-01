
library(readr)
library(dplyr)
library(lubridate)
library(leaflet)
library(sf)

path_dossier <- "C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/2023" #path to data
fichiers_csv <- list.files(path = path_dossier, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE) #list which contains the list of .csv files containing data
n = length(fichiers_csv)
m = nchar (path_dossier) #save length 

##First step: separate the CSV files into two distinct CSV files. Indeed, R cannot deal with the current files because they are in two parts. 


for (i in 1:n) { 
  
  #the script will repeat operations for each file 
  
  path_fichier <- fichiers_csv[i] 
  data = read_delim(path_fichier, delim = ",", locale = locale(encoding = "ISO-8859-1")) #the encoding is not 'UTF-8' because files use Brazilian words
  p = nchar(path_fichier)
  name_fichier = substr(path_fichier, m+2, p) #Extraction of the name of the ith file
  
  #creation of news paths 
  
  new_name_meteo = paste0("C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/2023_meteo/",name_fichier) 
  new_name_geo = paste0("C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/2023_geo/",name_fichier) 
  
  #splitting the former file
  
  data_meteo <- data[-(0:7), ] 
  data_geo <- data[(1:6),] 
  
  #rename columns of the second file
  
  colnames(data_meteo) <- data_meteo[1, ]
  data_meteo <- data_meteo[-1, ] #delete the former first line (which was containing the columns names)
  
  #there is now the value colnames() "Data;Hora UTC;PRECIPITAÇÃO TOTAL, HORÁRIO (mm);PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB);PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB);PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB);RADIACAO GLOBAL (Kj/m²);TEMPERATURA DO AR - BULBO SECO, HORARIA (°C);TEMPERATURA DO PONTO DE ORVALHO (°C);TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C);TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C);TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C);TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C);UMIDADE REL. MAX. NA HORA ANT. (AUT) (%);UMIDADE REL. MIN. NA HORA ANT. (AUT) (%);UMIDADE RELATIVA DO AR, HORARIA (%);VENTO, DIREÇÃO HORARIA (gr) (° (gr));VENTO, RAJADA MAXIMA (m/s);VENTO, VELOCIDADE HORARIA (m/s);"
  #why ? both files are not yet like dataframe but more like series. Indeed, I figured out that these .csv files use two separators (firstly : ',' then ';' so these two new files are like new .csv files using ";" as seperator 
  #resolving this probleme involves to split each row to build a dataframe (HERE WE GO)
  
  #spliting each row and building a new dataframe
  
  data_meteo = data.frame(do.call("rbind", strsplit(data_meteo$"Data;Hora UTC;PRECIPITAÇÃO TOTAL, HORÁRIO (mm);PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB);PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB);PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB);RADIACAO GLOBAL (Kj/m²);TEMPERATURA DO AR - BULBO SECO, HORARIA (°C);TEMPERATURA DO PONTO DE ORVALHO (°C);TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C);TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C);TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C);TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C);UMIDADE REL. MAX. NA HORA ANT. (AUT) (%);UMIDADE REL. MIN. NA HORA ANT. (AUT) (%);UMIDADE RELATIVA DO AR, HORARIA (%);VENTO, DIREÇÃO HORARIA (gr) (° (gr));VENTO, RAJADA MAXIMA (m/s);VENTO, VELOCIDADE HORARIA (m/s);", ";", fixed = TRUE)))
  names= unlist(strsplit("Data;Hora UTC;PRECIPITAÇÃO TOTAL, HORÁRIO (mm);PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB);PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB);PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB);RADIACAO GLOBAL (Kj/m²);TEMPERATURA DO AR - BULBO SECO, HORARIA (°C);TEMPERATURA DO PONTO DE ORVALHO (°C);TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C);TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C);TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C);TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C);UMIDADE REL. MAX. NA HORA ANT. (AUT) (%);UMIDADE REL. MIN. NA HORA ANT. (AUT) (%);UMIDADE RELATIVA DO AR, HORARIA (%);VENTO, DIREÇÃO HORARIA (gr) (° (gr));VENTO, RAJADA MAXIMA (m/s);VENTO, VELOCIDADE HORARIA (m/s);",";", fixed=TRUE))
  colnames(data_meteo) <- names 
  name = colnames(data_geo)[1]
  
  #now for the first part 
  
  data_geo = data.frame(do.call("rbind", strsplit(data_geo[[name]], ";", fixed = TRUE)))
  names=unlist(strsplit(name, ";", fixed = TRUE))
  nouvelle_ligne <- data.frame(t(names)) #getting column names
  colnames(nouvelle_ligne) <- colnames(data_geo)
  data_geo <- rbind(nouvelle_ligne, data_geo) #add column names
  data_geo = t(data_geo) #transposing 
  colnames(data_geo) <- data_geo[1, ]
  data_meteo <- as.data.frame(data_meteo)
  data_geo <- as.data.frame(data_geo)
  
  #I want to delete the useless columns 
  
  data_meteo <- as.data.frame(data_meteo)
  data_meteo <- select(data_meteo,colnames(data)[1], colnames(data)[2], colnames(data)[3], colnames(data)[8], colnames(data)[9], colnames(data)[10], colnames(data)[11], colnames(data)[12], colnames(data)[13])
  data_meteo$`PRECIPITAÃÃO TOTAL, HORÃRIO (mm)` <- as.numeric(data_meteo$`PRECIPITAÃÃO TOTAL, HORÃRIO (mm)`)
  
  # I convert the date column to date format
  
  data_meteo$Data <- as.Date(data_meteo$Data, format = "%Y-%m-%d")  
  
  # I calculate the average of temperatures per hours. 
  
  colonnes <- c(4,5,6,7,8,9)
  sapply(data_meteo[, colonnes], class) # I convert the columns to number format
  data_meteo[, colonnes] <- lapply(data_meteo[, colonnes], function(x) as.numeric(as.character(x)))
  Temp_mean = (rowMeans(data_meteo[,colonnes], na.rm = TRUE))/10 
  data_meteo <- data.frame(data_meteo, Temp_mean) #new dataframe 
  data <- select(data, colnames(data)[1],colnames(data)[3],colnames(data)[10]) #I delete the previous temp columns
  
  #I calculate the daily average of temperatures and the sum of the daily precipitation. 
  
  colnames(data)[2] <- 'Precipitation'
  data <- data %>%
    group_by(Data) %>%
    summarise(Precip_daily = sum(Precipitation, na.rm = TRUE), Temp_daily = mean(Temp_mean, na.rm = TRUE)) 
  
  #I want to create now a dataframe including both files 
  
  ville <- data_geo$`ESTACAO:`[2]
  lat <- data_geo$`LATITUDE:`[2]
  long <- data_geo$`LONGITUDE:`[2]
  reg <- data_geo$`REGIAO:`[2]
  data_meteo <- data_meteo %>% mutate(city = ville, Lat = lat, Long = long, Reg = reg )
  
  #I can now save the new files 
  
  write_csv(data_meteo, new_name_meteo 
  )   
  write_csv(data_geo, new_name_geo
  )}

#The Second Part
#I want to create a bigger dataframe which will contain each files



#Getting back files 

path_dossier_meteo <- "C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/2023_meteo"

fichiers_csv_meteo <- list.files(path = path_dossier_meteo, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)


n = length(fichiers_csv)
m = nchar (path_dossier)

path_fichier_meteo <- fichiers_csv_meteo[1]

#Initialization

data_meteo <- read_delim(path_fichier_meteo, delim = ",", locale = locale(encoding = "ISO-8859-1"))
big_data = data_meteo

for (i in 2:n) {
  
  path_fichier_meteo <- fichiers_csv_meteo[i]
  
  data_meteo <- read_delim(path_fichier_meteo, delim = ",", locale = locale(encoding = "ISO-8859-1"))
  
  big_data = rbind.data.frame(big_data,data_meteo)
  
  
}

#I save now the new dataframe

write_csv(big_data, "C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/data")

#As you can see, the new dataframe is quite big
#To simplify what follows, I will work on a date range of three days


data <- read_delim("C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/data", delim = ",", locale = locale(encoding = "ISO-8859-1"))

data <- filter(data, Data >= "2023-10-29", Data <= "2023-10-31")

write_csv(data, "C:/Users/FX506/Desktop/CS/césure/2e partie/etude_de_cas_PSR/data")

#I want to plot on my map of Brazil, the borders of the regions. For this, I need to retrieve a GeoJSON containing them. 
#Once retrieved, I will merge the data file and the new file on the 'Reg' column to have a new file.

regions_br <- st_read("C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states_.geojson")
colnames(regions_br)[3] <- "Reg"
new_data <- merge(data, regions_br, by = 'Reg' ) 

#I save it as Geojson file 

st_write(new_data, "C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states-new_.geojson", driver = "GeoJSON", append = FALSE)
file <- st_read("C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states-new_.geojson")
file <- st_make_valid(file)
file <- st_simplify(file, preserveTopology = TRUE)
st_write(new_data, "C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states-new-simp_.geojson", driver = "GeoJSON", append = FALSE)