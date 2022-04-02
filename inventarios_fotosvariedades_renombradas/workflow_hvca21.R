library(tidyverse)
library(googledrive)
library(writexl)
source("utils.R")

#googledrive::drive_auth()
#googledrive::drive_token()->acceso

### Load token and credentials to gdrive (pucp credentials)
#saveRDS(acceso,"token/acceso_go_pucp.rds")
#drive_auth(token = readRDS("token/acceso_go.rds"))
drive_auth(token = readRDS("token/acceso_go_pucp.rds"))

# Rename Fotos Junin 2016 amarillo -------------------------------------------------

### Crear el directorio para hacer el bacup y la copia de imagenes
#create_dir(path = "Project-GIZ/Fotos", dirname = "Rename_Fotos_Junin2016")


### Listado del inventario de Junin
inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                          filter(folder == "Fotos2021_Huancavelica")  %>% 
                                          mutate(
                                            crop = "potato", crop_abbrev= "pt",
                                            country = "Peru", country_abbrev = "pe",
                                            site = "Huancavelica", site_abbrev = "hvca",
                                            year = "2021",group_abbrev=""
                                          ) 

### Data Dictionary of Catalogues 
lista_hvca21_link <- "https://docs.google.com/spreadsheets/d/10aRmd4bKheN8IxdoT0lDQ49wrjAfE5R6JHBlcmXwUDI/edit#gid=90106869"
hvca21_codigo_fotos <- googlesheets4::read_sheet(lista_hvca21_link,sheet = 2)


dt_lista_hvelica21 <- drive_ls(path = "Fotos2021_Huancavelica")
folder_hvelica21 <- sort(dt_lista_hvelica21$name)
join_hvelica21 <- dt_lista_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
raw_photo_name_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
plant_part <- vector(mode="list", length = length(folder_hvelica21))
codigo <- vector(mode="list", length = length(folder_hvelica21))
inventario_photos_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
sub_folder <- vector(mode="character", length = length(folder_hvelica21))

# Creacion de folders

# for(i in 1:length(folder_hvelica21)){
#   
#     create_dir(path = "Project-GIZ/Fotos/Rename_Fotos_Huancavelica2021",
#              dirname = folder_hvelica21[i])
#   
# }  

#

for(i in seq.int(folder_hvelica21)){
  
  dt_lista_hvelica21_subfolder[[i]] <- drive_ls(path = file.path("Project-GIZ/Fotos/Fotos2021_Huancavelica", folder_hvelica21[i]))
  dt_lista_hvelica21_subfolder[[i]] <- dt_lista_hvelica21_subfolder[[i]] %>% 
                                            dplyr::filter(tools::file_ext(name)!="") %>% 
                                            dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                            dplyr::filter(!str_detect(name, "ICONOS time coccion"))
  
  dt_lista_hvelica21_subfolder[[i]] <- dt_lista_hvelica21_subfolder[[i]] %>% mutate(raw_photoname = name)
  
  join_hvelica21[[i]] <- inventario_photos_catalogo %>% filter(sub_folder== folder_hvelica21[i]) %>% left_join(.,dt_lista_hvelica21_subfolder[[i]])
  join_hvelica21[[i]] <- left_join(join_hvelica21[[i]], hvca21_codigo_fotos)
  
  join_hvelica21[[i]] <- join_hvelica21[[i]] %>% mutate(nombre_variedad = Nombre_completo) %>%
                                                 mutate(plant_part = tipo) %>% 
                                                 mutate(
                                                    crop = "potato", crop_abbrev= "pt",
                                                    country = "Peru", country_abbrev = "pe",
                                                    site = "Huancavelica", site_abbrev = "hvca",
                                                    year = "2021", group_abbrev=""
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
  
  
  writexl::write_xlsx(x = join_hvelica21[[i]], path = file.path("lista_data","hvca21", paste0("variedad-" ,i,".xlsx")))

}  

saveRDS(join_hvelica21, "lista_data/join_hvelica21.rds")

###Renombre de los datos

join_hvelica21 <- readRDS(file = "lista_data/join_hvelica21.rds")
join_invt_hvelica21 <- data.table::rbindlist(join_hvelica21,fill = TRUE) %>% as.data.frame(stringsAsFactors=FALSE)
join_invt_hvelica21 <- join_invt_hvelica21 %>% mutate(nombre_variedad == sub_folder) %>% 
                                               mutate(Nombre_completo == sub_folder)    


writexl::write_xlsx(join_invt_hvelica21,path = "lista_data/hvca21/tbl_variedad_hvelica2021.xlsx")

variedad <- unique(join_invt_hvelica21$nombre_variedad)

variety_photos <- vector(mode = "list", length = length(variedad))


for(i in 63:69){
  
 print(variedad[i])
  
 variety_photos[[i]] <- join_invt_hvelica21 %>% dplyr::filter(nombre_variedad==variedad[i])
                                           
  
  #print(variety_photos[[i]])
  #print(variety_photos[[i]]$nomenclature)
  
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


########### PARTE 2

rest_variedad <- setdiff(dt_lista_hvelica21$name,unique(join_invt_hvelica21$sub_folder))

dt_lista_hvelica21 <- drive_ls(path = "Fotos2021_Huancavelica") %>% 
                               filter(name %in% rest_variedad)

lista_hvca21_link <- "https://docs.google.com/spreadsheets/d/10aRmd4bKheN8IxdoT0lDQ49wrjAfE5R6JHBlcmXwUDI/edit#gid=90106869"
hvca21_codigo_fotos <- googlesheets4::read_sheet(lista_hvca21_link,sheet = 2)



folder_hvelica21 <- sort(dt_lista_hvelica21$name)
join_hvelica21 <- dt_lista_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
raw_photo_name_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
plant_part <- vector(mode="list", length = length(folder_hvelica21))
codigo <- vector(mode="list", length = length(folder_hvelica21))
inventario_photos_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
sub_folder <- vector(mode="character", length = length(folder_hvelica21))

inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
                                          filter(folder == "Fotos2021_Huancavelica")  %>% 
                                          mutate(
                                            crop = "potato", crop_abbrev= "pt",
                                            country = "Peru", country_abbrev = "pe",
                                            site = "Huancavelica", site_abbrev = "hvca",
                                            year = "2021",group_abbrev=""
                                          ) 


for(i in seq.int(folder_hvelica21)){
  
  dt_lista_hvelica21_subfolder[[i]] <- drive_ls(path = file.path("Project-GIZ/Fotos/Fotos2021_Huancavelica", folder_hvelica21[i]))
  dt_lista_hvelica21_subfolder[[i]] <- dt_lista_hvelica21_subfolder[[i]] %>% 
    dplyr::filter(tools::file_ext(name)!="") %>% 
    dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
    dplyr::filter(!str_detect(name, "ICONOS time coccion"))
  
  dt_lista_hvelica21_subfolder[[i]] <- dt_lista_hvelica21_subfolder[[i]] %>% mutate(raw_photoname = name)
  
  join_hvelica21[[i]] <- hvca21_codigo_fotos %>% filter(Nombre_completo == folder_hvelica21[i]) %>% left_join(.,dt_lista_hvelica21_subfolder[[i]])
  #join_hvelica21[[i]] <- left_join(join_hvelica21[[i]], hvca21_codigo_fotos)
  
  join_hvelica21[[i]] <- join_hvelica21[[i]] %>% mutate(nombre_variedad = Nombre_completo) %>%
                                                 mutate(plant_part = tipo) %>% 
                                                 mutate(
                                                   crop = "potato", crop_abbrev= "pt",
                                                   country = "Peru", country_abbrev = "pe",
                                                   site = "Huancavelica", site_abbrev = "hvca",
                                                   year = "2021", group_abbrev=""
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
  
  
  writexl::write_xlsx(x = join_hvelica21[[i]], path = file.path("lista_data","hvca21", paste0("variedad-" ,i,".xlsx")))
  
  
}  
join_hvelica21_pt2 <- join_hvelica21
saveRDS(join_hvelica21_pt2, "lista_data/join_hvelica21_pt2.rds")

###Renombre de los datos

join_hvelica21_pt2 <- readRDS(file = "lista_data/join_hvelica21_pt2.rds")
join_invt_hvelica21_pt2 <- data.table::rbindlist(join_hvelica21_pt2, fill = TRUE) %>% as.data.frame(stringsAsFactors=FALSE)
join_invt_hvelica21_pt2 <- join_invt_hvelica21_pt2 %>% mutate(nombre_variedad == sub_folder)    


writexl::write_xlsx(join_invt_hvelica21_pt2,path = "lista_data/hvca21/tbl_variedad_hvelica2021_pt2.xlsx")





variedad <- unique(join_invt_hvelica21_pt2$nombre_variedad)

variety_photos <- vector(mode = "list", length = length(variedad))

for(i in 57:length(variedad)){
  
  print(variedad[i])
  variety_photos[[i]] <- join_invt_hvelica21_pt2 %>% dplyr::filter(nombre_variedad==variedad[i])
  variety_photos[[i]]$formato <- "jpg"
  
  #print(variety_photos[[i]])
  #print(variety_photos[[i]]$nomenclature)
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



#### Creacion del archivo excel del inventario huancavelica 2021


library(googledrive)
library(tidyverse)
library(googlesheets4)

tbl_inventario_hcva21_p1 <- readRDS("lista_data/join_hvelica21.rds") 
tbl_inventario_hcva21_p2 <- readRDS("lista_data/join_hvelica21_pt2.rds") 
inventario_hcva21 <- c(tbl_inventario_hcva21_p1, tbl_inventario_hcva21_p2)
tbl_inventario_hcva21 <- data.table::rbindlist(inventario_hcva21, fill=TRUE,use.names = TRUE) %>% 
  as.data.frame() 

#tabla con las variades que no estaban mapeadas
tbl_inventario_hcva21_extras <- tbl_inventario_hcva21 %>% filter(is.na(nombre_variedad))
tbl_inventario_hcva21_extras <- tbl_inventario_hcva21_extras %>% mutate(nombre_variedad = sub_folder)
tbl_inventario_hcva21_extras <- tbl_inventario_hcva21_extras %>% 
  mutate(
    plant_part = case_when(
      str_detect(raw_photoname,"-T")~ "tuberculo",
      str_detect(raw_photoname,"-F")~ "flor",
      str_detect(raw_photoname,"-H")~ "hoja",
      str_detect(raw_photoname,".tiff")~ "molecular",
      str_detect(raw_photoname,"-B")~ "brote",
      str_detect(raw_photoname,"-P")~ "planta"
    ) 
  ) %>% 
  mutate(
    nombre = nombre_variedad,
    Nombre_completo = nombre_variedad,
    tipo = plant_part,
    nomenclature = paste0(
      crop_abbrev, group_abbrev,
      country_abbrev, "_", year,
      site_abbrev,"_",
      nombre, "_",plant_part
    )
  ) %>% 
  mutate(
    nomenclature =
      str_replace_all(nomenclature, pattern = "[[:space:]]",
                      replacement = "-")
  )



#Quitar filas vacias y quedarse con las completas
tbl_inventario_hcva21 <- tbl_inventario_hcva21 %>% 
  filter(!is.na(nombre_variedad))

#inventario de huancavelica completo 2021
tbl_inventario_hcva21_total <- data.table::rbindlist(list(tbl_inventario_hcva21, tbl_inventario_hcva21_extras),fill = TRUE) %>% as.data.frame()
#names(tbl_inventario_hcva21_total)[2] <- "Nombre"  
tbl_inventario_hcva21_total <- tbl_inventario_hcva21_total %>% 
  mutate(
    nombre_utf8 =  str_replace_all(nombre_variedad,"[[:punct:]]", "") %>% 
      stringi::stri_trans_general(., "latin-ascii") %>% 
      gsub("´", replacement = "",.) %>% toupper()
    
  )%>% 
  arrange(nombre_utf8) %>% 
  mutate(nombre_utf8 = 
           case_when(
             nombre_utf8 == "OCCE WAYRU" ~ "OCCE WUAYRO",
             nombre_utf8 == "QELLU OKU NATA" ~ "QELLU UKU NATA",
             nombre_utf8 == "SIRINA CATALOGO" ~ "SIRINA",   
             nombre_utf8 == "WAMANPA UMAN CATALOGO" ~ "WAMANPA UMAN",
             nombre_utf8 == "WANCA LLICLLA" ~ "WANKA LLICLLA",
             nombre_utf8 == "WISTOPA RUNTUN" ~ "WISTUPA RUNTUN",
             nombre_utf8 == "YANA LLUNCHUY WAQACHI" ~ "YANA LLUMCHUY WAQACHI", 
             nombre_utf8 == "YANA OCCA" ~ "YANA OCA",             
             nombre_utf8 == "YANA WAKAPA QALLUN" ~ "YANA WACAPA QALLUN",
             nombre_utf8 == "YURAC RIPRAN" ~ "YURAQ RIPRAN",
             TRUE ~ nombre_utf8
           )
  )



link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"
db_catalogo_variedades <- googlesheets4::read_sheet(link_db_catalogo_variedades)
db_catalogo_variedades <- db_catalogo_variedades %>% 
  filter(Departamento == "Huancavelica", `Año de publicacion` == "2021" ) %>% 
  mutate(
    nombre_utf8 =  str_replace_all(Nombre, "[[:punct:]]", "") %>% 
      stringi::stri_trans_general(., "latin-ascii") %>% 
      gsub("´", replacement = "",.) %>% toupper()
    
  ) %>% 
  arrange(nombre_utf8)




#db_catalogo_variedades$Nombre <- tolower(db_catalogo_variedades$Nombre)


out <- left_join(tbl_inventario_hcva21_total, db_catalogo_variedades) %>% 
  relocate(Orden)
#mutate(Nombre = toupper(Nombre)) 

out <- out %>% mutate(nombre_variedad = Nombre)          

writexl::write_xlsx(out, "inventarios/inv_ren_hcva_2021.xlsx")



