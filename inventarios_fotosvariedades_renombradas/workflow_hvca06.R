
# Huancavelica ------------------------------------------------------------
library(tidyverse)
library(googledrive)
source("utils.R")

#create_dir(path = "Project-GIZ/Fotos", dirname = "Rename_Fotos_Junin2016")

#googledrive::drive_auth()
#googledrive::drive_token()->acceso

### Load token and credentials to gdrive (pucp credentials)
#saveRDS(acceso,"token/acceso_go_pucp.rds")
#drive_auth(token = readRDS("token/acceso_go.rds"))
drive_auth(token = readRDS("token/acceso_go_pucp.rds"))


# Rename Fotos Junin 2016 amarillo -------------------------------------------------

### Crear el directorio para hacer el bacup y la copia de imagenes
#create_dir(path = "Project-GIZ/Fotos", dirname = "Rename_Fotos_Junin2016")

### Listado del inventario de Huancavelica 2006
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                filter(folder == "Fotos2006_Huancavelica")  %>% 
                                mutate(
                                        crop = "potato", crop_abbrev= "pt",
                                        country = "Peru", country_abbrev = "pe",
                                        site = "Huancavelica", site_abbrev = "hvca",
                                        year = "2006",group_abbrev=""
                                      ) 

### Data Dictionary of Catalogues 
lista_hvca06_link <- "https://docs.google.com/spreadsheets/d/1coMHN8rcLTrl-yPFZPVvWjxzuQNYW5ZqbwyoemFCg9Q/edit#gid=1402243987"
hvca06_codigo_fotos <- googlesheets4::read_sheet(lista_hvca06_link)

### Datos de los archivos y sus IDs en GDrive

##papa azul
lista_drive_hvca06_ptazul <- drive_ls(path = "Project-GIZ/Fotos/Fotos2006_Huancavelica/papas AZULADO 88-123")
lista_drive_hvca06_ptazul <- lista_drive_hvca06_ptazul %>% 
                                      dplyr::filter(tools::file_ext(name)!="") %>% 
                                      dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif"))
lista_drive_hvca06_ptazul <- lista_drive_hvca06_ptazul %>% mutate(raw_photoname = name)
join_ptazul <- inventario_photos_catalogo %>% filter(sub_folder== "papas AZULADO 88-123") %>% left_join(., lista_drive_hvca06_ptazul) 
join_ptazul <- left_join(join_ptazul, hvca06_codigo_fotos)
join_ptazul <- join_ptazul %>% mutate(nombre_variedad = Nombre_completo) %>% 
                               mutate(
                                    crop = "potato", crop_abbrev= "pt",
                                    country = "Peru", country_abbrev = "pe",
                                    site = "Huancavelica", site_abbrev = "hvca",
                                    year = "2006", group_abbrev=""
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
variedad <- unique(join_ptazul$nombre_variedad)

create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006",
           dirname ="papas_azulado")

variety_photos <- vector(mode = "list", length = length(variedad))

for(i in 1:length(variedad)){
  
  print(variedad[i])
  
  create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_azulado",
             dirname = variedad[i])
  
  
  variety_photos[[i]] <- join_ptazul %>% 
                              dplyr::filter(nombre_variedad==variedad[i])
                              
  print(variety_photos[[i]])
  print(variety_photos[[i]]$nomenclature)
  
  # 
  for(j in 1:nrow(variety_photos[[i]])){
    drive_cp(
      file = variety_photos[[i]]$id[j],
      path = file.path("Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_azulado", variedad[i], "/"),
      name = paste0(variety_photos[[i]]$nomenclature[j], ".", variety_photos[[i]]$formato[j]),
      overwrite = TRUE
    )
  }
  # 
}

# Renombre Huancavelica 2006 - Marron -------------------------------------

### Listado del inventario de Junin
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                    filter(folder == "Fotos2006_Huancavelica")  %>% 
                                    mutate(
                                      crop = "potato", crop_abbrev= "pt",
                                      country = "Peru", country_abbrev = "pe",
                                      site = "Huancavelica", site_abbrev = "hvca",
                                      year = "2006",group_abbrev=""
                                    ) 

### Data Dictionary of Catalogues 
lista_hvca06_link <- "https://docs.google.com/spreadsheets/d/1coMHN8rcLTrl-yPFZPVvWjxzuQNYW5ZqbwyoemFCg9Q/edit#gid=1402243987"
hvca06_codigo_fotos <- googlesheets4::read_sheet(lista_hvca06_link)

### Datos de los archivos y sus IDs en GDrive

##papa marron
lista_drive_hvca06_ptmarron <- drive_ls(path = "Project-GIZ/Fotos/Fotos2006_Huancavelica/papas MARRON 52-87")
lista_drive_hvca06_ptmarron <- lista_drive_hvca06_ptmarron %>% 
                                      dplyr::filter(tools::file_ext(name)!="") %>% 
                                      dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif"))
lista_drive_hvca06_ptmarron <- lista_drive_hvca06_ptmarron %>% mutate(raw_photoname = name)
join_ptmarron <- inventario_photos_catalogo %>% filter(sub_folder== "papas MARRON 52-87") %>% left_join(., lista_drive_hvca06_ptmarron) 
join_ptmarron <- left_join(join_ptmarron, hvca06_codigo_fotos)
join_ptmarron <- join_ptmarron %>% mutate(nombre_variedad = Nombre_completo) %>% 
                              mutate(
                                crop = "potato", crop_abbrev= "pt",
                                country = "Peru", country_abbrev = "pe",
                                site = "Huancavelica", site_abbrev = "hvca",
                                year = "2006", group_abbrev=""
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
variedad <- unique(join_ptmarron$nombre_variedad)

variety_photos <- vector(mode = "list", length = length(variedad))

for(i in 1:36){
  
  print(variedad[i])
  
  create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_marron",
             dirname = variedad[i])
  
  
  variety_photos[[i]] <- join_ptmarron %>% 
    dplyr::filter(nombre_variedad==variedad[i])
  
  print(variety_photos[[i]])
  print(variety_photos[[i]]$nomenclature)
  
  # 
  for(j in 1:nrow(variety_photos[[i]])){
    drive_cp(
      file = variety_photos[[i]]$id[j],
      path = file.path("Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_marron", variedad[i], "/"),
      name = paste0(variety_photos[[i]]$nomenclature[j], ".", variety_photos[[i]]$formato[j]),
      overwrite = TRUE
    )
  }
  # 
}





# Renombre Huancavelica 2006 - Naranja -------------------------------------

### Listado del inventario de Junin
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                    filter(folder == "Fotos2006_Huancavelica")  %>% 
                                    mutate(
                                      crop = "potato", crop_abbrev= "pt",
                                      country = "Peru", country_abbrev = "pe",
                                      site = "Huancavelica", site_abbrev = "hvca",
                                      year = "2006",group_abbrev=""
                                    ) 

### Data Dictionary of Catalogues 
lista_hvca06_link <- "https://docs.google.com/spreadsheets/d/1coMHN8rcLTrl-yPFZPVvWjxzuQNYW5ZqbwyoemFCg9Q/edit#gid=1402243987"
hvca06_codigo_fotos <- googlesheets4::read_sheet(lista_hvca06_link)

### Datos de los archivos y sus IDs en GDrive

##papa 
lista_drive_hvca06_ptnar <- drive_ls(path = "Project-GIZ/Fotos/Fotos2006_Huancavelica/papas NARANJA 160-195")
lista_drive_hvca06_ptnar <- lista_drive_hvca06_ptnar %>% 
                                  dplyr::filter(tools::file_ext(name)!="") %>% 
                                  dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif"))
lista_drive_hvca06_ptnar <- lista_drive_hvca06_ptnar %>% mutate(raw_photoname = name)
join_ptnar <- inventario_photos_catalogo %>% filter(sub_folder== "papas NARANJA 160-195") %>% left_join(., lista_drive_hvca06_ptnar) 
join_ptnar <- left_join(join_ptnar, hvca06_codigo_fotos)
join_ptnar <- join_ptnar %>% mutate(nombre_variedad = Nombre_completo) %>% 
                            mutate(
                              crop = "potato", crop_abbrev= "pt",
                              country = "Peru", country_abbrev = "pe",
                              site = "Huancavelica", site_abbrev = "hvca",
                              year = "2006", group_abbrev=""
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
variedad <- unique(join_ptnar$nombre_variedad)

variety_photos <- vector(mode = "list", length = length(variedad))

for(i in 35:36){
  
  print(variedad[i])
  
  create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_naranja",
             dirname = variedad[i])
  
  
  variety_photos[[i]] <- join_ptnar %>% 
    dplyr::filter(nombre_variedad==variedad[i])
  
  print(variety_photos[[i]])
  print(variety_photos[[i]]$nomenclature)
  
  # 
  for(j in 1:nrow(variety_photos[[i]])){
    drive_cp(
      file = variety_photos[[i]]$id[j],
      path = file.path("Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_naranja", variedad[i], "/"),
      name = paste0(variety_photos[[i]]$nomenclature[j], ".", variety_photos[[i]]$formato[j]),
      overwrite = TRUE
    )
  }
  # 
}


# Renombre Huancavelica 2006 - ROJIZO -------------------------------------


### Listado del inventario de Junin
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                      filter(folder == "Fotos2006_Huancavelica")  %>% 
                                      mutate(
                                        crop = "potato", crop_abbrev= "pt",
                                        country = "Peru", country_abbrev = "pe",
                                        site = "Huancavelica", site_abbrev = "hvca",
                                        year = "2006",group_abbrev=""
                                      ) 

### Data Dictionary of Catalogues 
lista_hvca06_link <- "https://docs.google.com/spreadsheets/d/1coMHN8rcLTrl-yPFZPVvWjxzuQNYW5ZqbwyoemFCg9Q/edit#gid=1402243987"
hvca06_codigo_fotos <- googlesheets4::read_sheet(lista_hvca06_link)

##get number of varieties
dt_varieties <- hvca06_codigo_fotos %>% filter(colores_catalogo == "Rojo")
ch1 <- dt_varieties$Nombre_completo
length(ch1)

### Datos de los archivos y sus IDs en GDrive

##papa 
lista_drive_hvca06_ptrojo <- drive_ls(path = "Project-GIZ/Fotos/Fotos2006_Huancavelica/papas ROJIZO 124-159")
lista_drive_hvca06_ptrojo <- lista_drive_hvca06_ptrojo %>% 
                                      dplyr::filter(tools::file_ext(name)!="") %>% 
                                      dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif"))
lista_drive_hvca06_ptrojo <- lista_drive_hvca06_ptrojo %>% mutate(raw_photoname = name)
join_ptrojo <- inventario_photos_catalogo %>% filter(sub_folder== "papas ROJIZO 124-159") %>% left_join(., lista_drive_hvca06_ptrojo) 
join_ptrojo <- left_join(join_ptrojo, hvca06_codigo_fotos) %>% filter(!is.na(nombre_variedad))
join_ptrojo <- join_ptrojo %>% mutate(nombre_variedad = Nombre_completo)
                               mutate(
                                 crop = "potato", crop_abbrev= "pt",
                                 country = "Peru", country_abbrev = "pe",
                                 site = "Huancavelica", site_abbrev = "hvca",
                                 year = "2006", group_abbrev=""
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
#Quitar NA -- solo para la carpeta rojizo                             
join_ptrojo <- join_ptrojo  %>% filter(!is.na(nombre_variedad))                             
                               
                               
### nombre de la variedad -sin repeticiones
variedad <- unique(join_ptrojo$nombre_variedad) 

variety_photos <- vector(mode = "list", length = length(variedad))

for(i in 34:36){
  
  print(variedad[i])
  
  create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Huancavelica2006/papas_rojo",
             dirname = variedad[i])
  
  
  variety_photos[[i]] <- join_ptrojo %>% 
                              dplyr::filter(nombre_variedad==variedad[i])
  
  print(variety_photos[[i]])
  print(variety_photos[[i]]$nomenclature)
  
  # 
  for(j in 1:nrow(variety_photos[[i]])){
    drive_cp(
      file = variety_photos[[i]]$id[j],
      path = file.path("Project-GIZ/Fotos/Rename_Fotos_Huancavelica2021", variedad[i], "/"),
      name = paste0(variety_photos[[i]]$nomenclature[j], ".", variety_photos[[i]]$formato[j]),
      overwrite = TRUE
    )
  }
  # 
}


#### Creacion del archivo inventario huancavelica 2006 excel


# Inventario de imagenes renombradas huancavelica 2006 -------------------------------------
link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"

library(tidyverse)
### Listado del inventario de Junin
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
  filter(folder == "Fotos2006_Huancavelica")  %>% 
  mutate(
    crop = "potato", crop_abbrev= "pt",
    country = "Peru", country_abbrev = "pe",
    site = "Huancavelica", site_abbrev = "hvca",
    year = "2006",group_abbrev="",
    codigo = tolower(codigo)
  ) 
names(inventario_photos_catalogo)[2] <- "Nombre"


### Listado de fotos Vilma Huancavelica 2006
lista_hvca06_link <- "https://docs.google.com/spreadsheets/d/1coMHN8rcLTrl-yPFZPVvWjxzuQNYW5ZqbwyoemFCg9Q/edit#gid=1402243987"
hvca06_codigo_fotos <- googlesheets4::read_sheet(lista_hvca06_link) %>% mutate(codigo = tolower(codigo))

#Match entre nombre de variedad y codigo
out <- left_join(inventario_photos_catalogo, hvca06_codigo_fotos) %>% 
  mutate(Nombre = Nombre_completo) %>% 
  mutate(
    nomenclature = paste0(
      crop_abbrev, group_abbrev,
      country_abbrev, "_", year,
      site_abbrev,"_",
      Nombre, "_",plant_part
    )
  ) %>% 
  mutate(
    nomenclature =
      str_replace_all(nomenclature, pattern = "[[:space:]]",
                      replacement = "-")
  ) %>% 
  mutate(Nombre = tolower(Nombre)) %>%
  select(-18)

#Leyendo la bd variedades de papa 
db_catalogo_variedades <- googlesheets4::read_sheet(link_db_catalogo_variedades,skip = 1)
db_catalogo_variedades <- db_catalogo_variedades %>% filter(Departamento == "Huancavelica", Año ==2006)
db_catalogo_variedades$Nombre <- tolower(db_catalogo_variedades$Nombre)

#Join entre el inventario de imagenes renombradas huancavelica 2006 y las BD variedades
join_ren_inv <- left_join(out, db_catalogo_variedades) %>% 
  relocate(Orden) %>% 
  mutate(Nombre = toupper(Nombre)) %>% 
  arrange(Nombre) %>%
  mutate(nomenclature = 
           case_when(
             sub_folder == "papas AZULADO 88-123" ~ paste0(nomenclature,"_azulado"),
             sub_folder == "papas MARRON 52-87" ~ paste0(nomenclature,"_marron"),
             sub_folder == "papas NARANJA 160-195" ~ paste0(nomenclature,"_naranja"),
             sub_folder == "papas ROJIZO 124-159" ~ paste0(nomenclature,"_rojizo")
           )
  )

writexl::write_xlsx(join_ren_inv, "inventarios/inv_ren_hcva_2006.xlsx")




