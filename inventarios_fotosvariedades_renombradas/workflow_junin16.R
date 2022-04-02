#gs4_auth(email = "obacc07@gmail.com")
library(tidyverse)
library(googledrive)
source("utils.R")


#googledrive::drive_auth()
#googledrive::drive_token()->acceso

### Load token and credentials to gdrive (pucp credentials)
#saveRDS(acceso,"token/acceso_go_pucp.rds")
#drive_auth(token = readRDS("token/acceso_go.rds"))
drive_auth(token = readRDS("token/acceso_go_pucp.rds"))


# Rename Fotos Junin 2016 -------------------------------------------------

### Crear el directorio para hacer el bacup y la copia de imagenes
#create_dir(path = "Project-GIZ/Fotos", dirname = "Rename_Fotos_Junin2016")

### Listado del inventario de Junin
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                      filter(folder == "Fotos_Junin2016")  %>% 
                                      mutate(
                                        crop = "potato", crop_abbrev= "pt",
                                        country = "Peru", country_abbrev = "pe",
                                        site = "Junin", site_abbrev = "jun",
                                        year = "2016",group_abbrev=""
                                      ) %>% 
                                      mutate(
                                        nomenclature = paste0(
                                                              crop_abbrev, group_abbrev,
                                                              country_abbrev, "_", year,
                                                              site_abbrev,"_",
                                                              nombre_variedad, "_",plant_part
                                                              )
                                      ) %>% 
                                      mutate(
                                        nomenclature =
                                          str_replace_all(nomenclature, pattern = "[[:space:]]",
                                                          replacement = "-")
                                      )


 

### nombre de la variedad -sin repeticiones
variedad <- unique(inventario_photos_catalogo$nombre_variedad)

### Listar archivo de una carpeta o folder
dt_fotos_lista <- drive_ls(path = "Fotos_Junin2016")
### quitar de la lista elementos que no son fotos
dt_fotos_lista <- dt_fotos_lista %>% 
                                  dplyr::filter(tools::file_ext(name)!="") %>% 
                                  dplyr::filter(!stringr::str_detect(name, ".xlsx")) %>% 
                                  mutate(raw_photoname = name)

join_data <- left_join(inventario_photos_catalogo, dt_fotos_lista)

variety_photos <- vector(mode = "list", length = length(variedad))

for(i in 1:nrow(variedad)){

  print(variedad[i])
  
  create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Junin2016",
             dirname = variedad[i])
  
  variety_photos[[i]] <- join_data %>% dplyr::filter(nombre_variedad==variedad[i])
  
  print(variety_photos[[i]])
  print(variety_photos[[i]]$nomenclature)
  
    # 
    for(j in 1:nrow(variety_photos[[i]])){
          drive_cp(
            file = variety_photos[[i]]$id[j],
            path = file.path("Project-GIZ/Fotos/Rename_Fotos_Junin2016", variedad[i], "/"),
            name = paste0(variety_photos[[i]]$nomenclature[j], ".", variety_photos[[i]]$formato[j]),
            overwrite = TRUE
          )
    }
    # 
}

####### Creacion del archivo inventario junin 2016 

link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"

# Guardar inventario de imagenes de junin ---------------------------------

inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
  filter(folder == "Fotos_Junin2016")  %>% 
  mutate(
    crop = "potato", crop_abbrev= "pt",
    country = "Peru", country_abbrev = "pe",
    site = "Junin", site_abbrev = "jun",
    year = "2016",group_abbrev=""
  ) %>% 
  mutate(
    nomenclature = paste0(
      crop_abbrev, group_abbrev,
      country_abbrev, "_", year,
      site_abbrev,"_",
      nombre_variedad, "_",plant_part
    )
  ) %>% 
  mutate(
    nomenclature =
      str_replace_all(nomenclature, pattern = "[[:space:]]",
                      replacement = "-")
  )

names(inventario_photos_catalogo)[2] <- "Nombre"  
inventario_photos_catalogo$Nombre <- tolower(inventario_photos_catalogo$Nombre)

db_catalogo_variedades <- googlesheets4::read_sheet(link_db_catalogo_variedades,skip = 1)
db_catalogo_variedades <- db_catalogo_variedades %>% filter(Departamento == "Junin")
db_catalogo_variedades$Nombre <- tolower(db_catalogo_variedades$Nombre)

out <- left_join(inventario_photos_catalogo, db_catalogo_variedades) %>% 
        relocate(Orden) %>% 
        mutate(Nombre = toupper(Nombre))


writexl::write_xlsx(out, "inventarios/inv_ren_junin_2016.xlsx")





