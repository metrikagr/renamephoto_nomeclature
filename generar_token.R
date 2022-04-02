gs4_auth(email = "tu_email@gmail.com")
library(tidyverse)
library(googledrive)


googledrive::drive_auth()
googledrive::drive_token()->acceso #almacena el valor del token

## Load token and credentials to gdrive (pucp credentials)
## Crear una carpeta token
saveRDS(acceso,"token/token_acceso.rds")
drive_auth(token = readRDS("token/token_acceso.rds"))
