library(tidyverse)
library(googledrive)
source("utils.R")

### Login y recibir token
#googledrive::drive_auth()
#googledrive::drive_token()->acceso

### Load token and credentials to gdrive (pucp credentials)
#saveRDS(acceso,"token/acceso_go_pucp.rds")
#drive_auth(token = readRDS("token/acceso_go.rds"))
drive_auth(token = readRDS("token/acceso_go_pucp.rds"))

#### leer los datos
link_listado_huancv2006 <- "https://docs.google.com/spreadsheets/d/1coMHN8rcLTrl-yPFZPVvWjxzuQNYW5ZqbwyoemFCg9Q/edit#gid=1402243987"
lista_codigo_hancv2006 <- googlesheets4::read_sheet(ss = link_listado_huancv2006)

### Crear el directorio para hacer el bacup y la copia de imagenes
create_dir(path = "Fotos2006_Huancavelica/papas AZULADO 88-123", dirname = "papas AZULADO 88-123_copia")

### leer el sheet 
hancv2006_link_sheet <- "https://docs.google.com/spreadsheets/d/1qeikwV2LNF2PDzWtPNyh-cP_6XDN4dVdExPtPb17yrs/edit?usp=sharing"
hancv2006_link_datos <- gsheet::gsheet2tbl(hancv2006_link_sheet)

#### Generar backup y copiar fotos en la carpeta 
copy_photos_drive(from = "papas AZULADO 88-123",
                  to = "papas AZULADO 88-123/papas AZULADO 88-123_copia",
                  pho_new = ".jpg")

### Renombrar las imagenes #########
lista_folder_hvelica_2006 <- drive_ls(path = "papas AZULADO 88-123/papas AZULADO 88-123_copia")

dt_lista_hvelica_2006 <- lista_folder_hvelica_2006 %>% 
                                 mutate(
                                   CODIGO_FOTOS = gsub(x = name, pattern = "\\s.*",replacement = ""),
                                   plant_part = str_remove_all(gsub(x = name, pattern = ".*\\s",replacement = ""),
                                                               pattern = "\\..*")
                                 )

dt_lista_hvelica_2006 <- left_join(dt_lista_hvelica_2006,lista_codigo_hancv2006)
                            

### obtener tabla del listado fotos de variedades y sus partes #########
#dt_lista_variedades_hvelica2006 <- get_photo_parts(lista_folder_hvelica_2006$name, sep="\\s.*")

### Metadatos #####################
crop_abbrev <- "pt"
group <- ""
group_abbrev <- "" 
variety <- ""
country_abbrev <- "pe" 
site_abbrev <- "hvelica"
year <- "2006"
plant_part <- ""


dt_lista_hvelica_2006  <- dt_lista_hvelica_2006   %>% 
                                mutate(
                                  nomenclature = paste0(crop_abbrev, group_abbrev,
                                                        Nombre_completo,"_",CODIGO_FOTOS,"_",country_abbrev, site_abbrev,
                                                        year, "_",plant_part, ".jpg")
                                ) %>% 
                                mutate(
                                  nomenclature =
                                    str_replace_all(nomenclature, pattern = "[[:space:]]",
                                                    replacement = "-")
                                )

writexl::write_xlsx(dt_lista_hvelica_2006,"dt_lista_hvelica_2006.xlsx")

### Renombrar los archivos por sus nomenclaturas.
for(i in 1:nrow(dt_lista_hvelica_2006)){
  drive_rename(
    file = lista_folder_hvelica_2006$id[i],
    name = dt_lista_hvelica_2006$nomenclature[i],
    overwrite = TRUE
  )
}
