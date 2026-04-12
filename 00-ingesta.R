#voy a adquirir los datos - Y guardadrlos en una variable llamada datos
library(googlesheets4)
url="https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=1481484702#gid=1481484702"
gs4_deauth()

# Leo el archivo y solo lee la pagina 2
datos <- read_sheet(url, skip = 1,sheet=2)

#Leo los datos
str(datos)