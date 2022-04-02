###########################################################################
# Creacion de la tabla de datos de imagenes --------------------------------
###########################################################################

library(googledrive)
library(googlesheets4)
library(tidyverse)

source("utils.R")

#googledrive::drive_auth()
#googledrive::drive_token()->acceso

### Load token and credentials to gdrive (pucp credentials)
#saveRDS(acceso,"token/acceso_go_pucp.rds")
#drive_auth(token = readRDS("token/acceso_go.rds"))
drive_auth(token = readRDS("token/acceso_go_pucp.rds"))



#Cargar la tabla de variedades#############
db_catalogo_variedades <- "https://docs.google.com/spreadsheets/d/1cGinGZxLZT0fHdoEC5U3E8Yh-LAYhTUlXKJGUxMjnKs/edit#gid=101581762"


#Listar las imagenes de los folderes de imagenes - datos crudos ###

###------------------------------------------------------------------------
##Imagenes: Junin 2006 
###------------------------------------------------------------------------

### Listar archivo de una carpeta o folder
dt_lista_junin16 <- drive_ls(path = "Fotos_Junin2016")
### quitar de la lista elementos que no son fotos
dt_lista_junin16 <- dt_lista_junin16 %>% dplyr::filter(tools::file_ext(name)!="") 
#nombre original de las fotos
raw_photo_name_junin16 <- dt_lista_junin16$name

# GET 
Nombre_variedad <- get_photo_parts(dt_lista_junin16$name, sep="-")$Nombre
plant_part <- get_photo_parts(dt_lista_junin16$name, sep="-")$plant_part %>% 
              str_replace_all(., "\\..[A-z]+","") %>% 
              str_replace_all(., " copy", "") %>% 
              str_replace_all(., " copia", "") %>% 
              str_replace_all(., " UP", "") %>% 
              tolower()

formato <- "jpg"            
folder <- "Fotos_Junin2016"
sub_folder <- ""
codigo <- ""
#inventario de fotos - variedades de junin 2016
inventario_photos_junin16 <- data.frame(
                                       raw_photoname= raw_photo_name_junin16, 
                                       nombre_variedad = Nombre_variedad ,
                                       plant_part = plant_part,
                                       folder = folder,
                                       sub_folder = sub_folder,
                                       formato = formato,
                                       codigo = codigo
                                      )

writexl::write_xlsx(inventario_photos_junin16, "inventario_junin16.xlsx")


###------------------------------------------------------------------------
##Imagenes: Huancavelica 2016
###------------------------------------------------------------------------

### Listar archivo de una carpeta o folder
dt_lista_hvelica06 <- drive_ls(path = "Fotos2006_Huancavelica")

##### Listar archivos de un subfolder papas AZULADO 88-123
dt_lista_hvelica06_ptazul88123 <- drive_ls(path = "papas AZULADO 88-123")
dt_lista_hvelica06_ptazul88123 <- dt_lista_hvelica06_ptazul88123 %>% 
                                    dplyr::filter(tools::file_ext(name)!="") %>% 
                                    dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif"))


#nombre original de las fotos
raw_photo_name_hvelica06_ptazul88123 <- dt_lista_hvelica06_ptazul88123$name

plant_part <- raw_photo_name_hvelica06_ptazul88123 %>% 
                gsub(x = ., pattern = "Flor Planta", "Flor") %>% 
                gsub(x = ., pattern = ".*\\s",replacement = "") %>% 
                str_remove_all(., pattern = "\\..*") %>% 
                gsub( "-.*", "",  .) %>%  
                tolower()              

Nombre_variedad <- ""
formato <- "jpg"            
folder <- "Fotos2006_Huancavelica"
sub_folder <- "papas AZULADO 88-123"
codigo <-  raw_photo_name_hvelica06_ptazul88123 %>%  gsub(x = ., pattern = "\\s.*",replacement = "")


inventario_photos_hvelica06_ptazul8812 <- data.frame(
                                           raw_photoname= raw_photo_name_hvelica06_ptazul88123, 
                                           nombre_variedad = Nombre_variedad ,
                                           plant_part = plant_part,
                                        folder = folder,
                                        sub_folder = sub_folder,
                                        formato = formato,
                                        codigo = codigo
                                      )

writexl::write_xlsx(inventario_photos_hvelica06_ptazul8812, "inventario_hvelica06_ptazul8812.xlsx")

##### Listar archivos de un subfolder papas MARRON 52-87
dt_lista_hvelica06_ptmarron5287 <- drive_ls(path = "papas MARRON 52-87") 


dt_lista_hvelica06_ptmarron5287 <- dt_lista_hvelica06_ptmarron5287 %>% 
                                          dplyr::filter(tools::file_ext(name)!="") %>% 
                                          dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                          dplyr::filter(!str_detect(name, "ICONOS time coccion"))


raw_photo_name_hvelica06_ptmarron5287 <- dt_lista_hvelica06_ptmarron5287$name
plant_part <- raw_photo_name_hvelica06_ptmarron5287 %>% 
                      gsub(x = ., pattern = "Flor Planta", "Flor") %>% 
                      gsub(x = ., pattern = ".*\\s",replacement = "") %>% 
                      str_remove_all(., pattern = "\\..*") %>% 
                      gsub( "-.*", "",  .) %>%  
                      tolower()              

codigo <-  raw_photo_name_hvelica06_ptmarron5287 %>%  gsub(x = ., pattern = "\\s.*",replacement = "")
Nombre_variedad <- ""
formato <- "jpg"            
folder <- "Fotos2006_Huancavelica"
sub_folder <- "papas MARRON 52-87"


inventario_photos_hvelica06_ptmarron5287 <- data.frame(
                                                  raw_photoname= raw_photo_name_hvelica06_ptmarron5287, 
                                                  nombre_variedad = Nombre_variedad ,
                                                  plant_part = plant_part,
                                                  folder = folder,
                                                  sub_folder = sub_folder,
                                                  formato = formato,
                                                  codigo = codigo
                                            )

writexl::write_xlsx(inventario_photos_hvelica06_ptmarron5287, "inventario_photos_hvelica06_ptmarron5287.xlsx")



##### Listar archivos de un subfolder papas NARANJAS 160-195


dt_lista_hvelica06_ptnarj160195 <- drive_ls(path = "papas NARANJA 160-195")
dt_lista_hvelica06_ptnarj160195 <- dt_lista_hvelica06_ptnarj160195 %>% 
                                      dplyr::filter(tools::file_ext(name)!="") %>% 
                                      dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                      dplyr::filter(!str_detect(name, "ICONOS time coccion"))


raw_photo_name_hvelica06_ptnarj160195 <- dt_lista_hvelica06_ptnarj160195$name
plant_part <- raw_photo_name_hvelica06_ptnarj160195 %>% 
                      gsub(x = ., pattern = "Flor Planta", "Flor") %>% 
                      gsub(x = ., pattern = ".*\\s",replacement = "") %>% 
                      str_remove_all(., pattern = "\\..*") %>% 
                      gsub( "-.*", "",  .) %>%  
                      tolower()              

codigo <-  raw_photo_name_hvelica06_ptnarj160195  %>%  gsub(x = ., pattern = "\\s.*",replacement = "")
Nombre_variedad <- ""
formato <- "jpg"            
folder <- "Fotos2006_Huancavelica"
sub_folder <- "papas NARANJA 160-195"


inventario_photos_hvelica06_ptnarj160195 <- data.frame(
                                                raw_photoname= raw_photo_name_hvelica06_ptnarj160195, 
                                                nombre_variedad = Nombre_variedad ,
                                                plant_part = plant_part,
                                                folder = folder,
                                                sub_folder = sub_folder,
                                                formato = formato,
                                                codigo = codigo
                                              )

writexl::write_xlsx(inventario_photos_hvelica06_ptnarj160195, "inventario_photos_hvelica06_ptnarj160195.xlsx")



##### Listar archivos de un subfolder papas NARANJAS 124-159

dt_lista_hvelica06_ptrojo124159 <- drive_ls(path = "papas ROJIZO 124-159")
dt_lista_hvelica06_ptrojo124159 <- dt_lista_hvelica06_ptrojo124159 %>% 
                                        dplyr::filter(tools::file_ext(name)!="") %>% 
                                        dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                        dplyr::filter(!str_detect(name, "ICONOS time coccion"))


raw_photo_name_hvelica06_ptrojo124159<- dt_lista_hvelica06_ptrojo124159$name
plant_part <- raw_photo_name_hvelica06_ptrojo124159 %>% 
                    gsub(x = ., pattern = "Flor Planta", "Flor") %>% 
                    gsub(x = ., pattern = ".*\\s",replacement = "") %>% 
                    str_remove_all(., pattern = "\\..*") %>% 
                    gsub( "-.*", "",  .) %>%  
                    tolower()              

codigo <-  raw_photo_name_hvelica06_ptrojo124159 %>% gsub(x = ., pattern = "\\s.*",replacement = "")
Nombre_variedad <- ""
formato <- "jpg"            
folder <- "Fotos2006_Huancavelica"
sub_folder <- "papas ROJIZO 124-159"


inventario_photos_hvelica06_ptrojo124159 <- data.frame(
                                                raw_photoname= raw_photo_name_hvelica06_ptrojo124159, 
                                                nombre_variedad = Nombre_variedad ,
                                                plant_part = plant_part,
                                                folder = folder,
                                                sub_folder = sub_folder,
                                                formato = formato,
                                                codigo = codigo
                                            )

writexl::write_xlsx(inventario_photos_hvelica06_ptrojo124159, "inventario_photos_hvelica06_ptrojo124159.xlsx")


###------------------------------------------------------------------------
##Imagenes: Huancavelica 2021
###------------------------------------------------------------------------

dt_lista_hvelica21 <- drive_ls(path = "Fotos2021_Huancavelica")
folder_hvelica21 <- sort(dt_lista_hvelica21$name)
dt_lista_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
raw_photo_name_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
plant_part <- vector(mode="list", length = length(folder_hvelica21))
codigo <- vector(mode="list", length = length(folder_hvelica21))
inventario_photos_hvelica21_subfolder <- vector(mode="list", length = length(folder_hvelica21))
sub_folder <- vector(mode="character", length = length(folder_hvelica21))

for(i in seq.int(folder_hvelica21)){

  
  dt_lista_hvelica21_subfolder[[i]] <- drive_ls(path = folder_hvelica21[i])
  dt_lista_hvelica21_subfolder[[i]] <- dt_lista_hvelica21_subfolder[[i]] %>% 
                                        dplyr::filter(tools::file_ext(name)!="") %>% 
                                        dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                        dplyr::filter(!str_detect(name, "ICONOS time coccion"))
  
  
  raw_photo_name_hvelica21_subfolder[[i]] <- dt_lista_hvelica21_subfolder[[i]]$name
  plant_part[[i]] <- ""         
  
  codigo[[i]] <-  raw_photo_name_hvelica21_subfolder[[i]] %>% gsub(x = ., pattern = "\\s.*",replacement = "")
  Nombre_variedad <- ""
  formato <- "jpg"            
  folder <- "Fotos2021_Huancavelica"
  sub_folder <- folder_hvelica21[i]
  
  
  inventario_photos_hvelica21_subfolder[[i]] <- data.frame(
                                                  raw_photoname= raw_photo_name_hvelica21_subfolder[[i]], 
                                                  nombre_variedad = Nombre_variedad ,
                                                  plant_part = plant_part[[i]],
                                                  folder = folder,
                                                  sub_folder = folder_hvelica21[i],
                                                  formato = formato,
                                                  codigo = codigo[[i]]
                                               )  
}

library(data.table)
inventario_photos_hvelica21 <- data.table::rbindlist(inventario_photos_hvelica21_subfolder,use.names = TRUE,fill = TRUE) %>% 
                               as.data.frame()      

writexl::write_xlsx(inventario_photos_hvelica21, "inventario_hvelica21.xlsx")



###------------------------------------------------------------------------
##Imagenes: Chugay-Pataz 2021
###------------------------------------------------------------------------

dt_lista_chupataz <- drive_ls(path = "Fotos_Chugay-Pataz") %>% 
                          dplyr::filter(!str_detect(name, ".psd"))

folder_chupataz <- sort(dt_lista_chupataz$name)
                       
dt_lista_chupataz_subfolder <- vector(mode="list", length = length(folder_chupataz))
raw_photo_name_chupataz_subfolder <- vector(mode="list", length = length(folder_chupataz))
plant_part <- vector(mode="list", length = length(folder_chupataz))
codigo <- vector(mode="list", length = length(folder_chupataz))
inventario_photos_chupataz_subfolder <- vector(mode="list", length = length(folder_chupataz))
sub_folder <- vector(mode="character", length = length(folder_chupataz))

for(i in seq.int(folder_chupataz)){
  
  print(i)
  
  dt_lista_chupataz_subfolder[[i]] <- drive_ls(path = folder_chupataz[i])      
                                      
  dt_lista_chupataz_subfolder[[i]] <- dt_lista_chupataz_subfolder[[i]] %>% 
    dplyr::filter(tools::file_ext(name)!="") %>% 
    dplyr::filter(str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
    dplyr::filter(!str_detect(name, "ICONOS time coccion"))
  
  
  raw_photo_name_chupataz_subfolder[[i]] <- dt_lista_chupataz_subfolder[[i]]$name
  plant_part[[i]] <- raw_photo_name_chupataz_subfolder[[i]] %>% 
                          gsub(x = ., pattern = "\\s.*",replacement = "") %>% 
                          tolower()  
                          
  
  codigo[[i]] <-  ""
  Nombre_variedad <- ""
  formato <- "jpg"            
  folder <- "Fotos_Chugay-Pataz"
  sub_folder <- folder_chupataz[i]
  
  
  inventario_photos_chupataz_subfolder[[i]] <- data.frame(
    raw_photoname= raw_photo_name_chupataz_subfolder[[i]], 
    nombre_variedad = Nombre_variedad ,
    plant_part = plant_part[[i]],
    folder = folder,
    sub_folder = folder_chupataz[i],
    formato = formato,
    codigo = codigo[[i]]
  )  
}

library(data.table)
inventario_photos_chupataz <- data.table::rbindlist(inventario_photos_chupataz_subfolder,use.names = TRUE,fill = TRUE) %>% 
                                as.data.frame()      


writexl::write_xlsx(inventario_photos_chupataz, "inventario_photos_chupataz.xlsx")


# Tabla maestra de todas las imagenes del catalogo ------------------------

inv_junin16 <- readxl::read_xlsx("inventario_junin16.xlsx")

inv_hvelica21 <- readxl::read_xlsx("inventario_hvelica21.xlsx")

inv_hvelica8812 <- readxl::read_xlsx("inventario_hvelica06_ptazul8812.xlsx")

inv_hvelica5287 <- readxl::read_xlsx("inventario_photos_hvelica06_ptmarron5287.xlsx")

inv_hvelica16095 <- readxl::read_xlsx("inventario_photos_hvelica06_ptnarj160195.xlsx")

inv_hvelica125159 <- readxl::read_xlsx("inventario_photos_hvelica06_ptrojo124159.xlsx")

inv_chupataz <- readxl::read_xlsx("inventario_photos_chupataz.xlsx")

a1 <- rbind(inv_junin16, inv_hvelica8812, inv_hvelica125159,inv_hvelica16095,
      inv_hvelica21, inv_hvelica5287, inv_chupataz) 

writexl::write_xlsx(a1, "invetario_photos_catalogo_papa.xlsx")




