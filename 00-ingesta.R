#install.packages(c("tidyverse", "janitor", "scales", "summarytools"))
# Carga de librerías
library(tidyverse)    
library(janitor)     
library(scales)      
library(summarytools) 
library(googlesheets4)
library(scales)
# Configuración de acceso
gs4_deauth()
url="https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=1481484702#gid=1481484702"
# Ingesta de datos
datos <- read_sheet(url, skip = 1,sheet=2)