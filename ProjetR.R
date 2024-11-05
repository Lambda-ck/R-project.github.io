base_brevets <- data.frame(
  firm_name = character(),     # Nom de l'entreprise
  n_patents = integer(),       # Nombre de brevets
  ipc_main_code = character(), # Code de la classe IPC principale
  ipc_main_desc = character(), # Description de la classe IPC principale
  ipc_second_code = character(), # Code de la seconde classe IPC
  ipc_second_desc = character(), # Description de la seconde classe IPC
  addr_city_main = character(), # Ville principale de l'entreprise
  addr_dept_main = character(), # Département principal de l'entreprise
  stringsAsFactors = FALSE
)

### Manipulation des descriptions liées aux IPC

files <- list.files(pattern = "^EN_ipc.*\\.txt$")#on importe tous les fichiers IPC voulus
data_list <- lapply(files, read.table, sep = "\t", header = FALSE, stringsAsFactors = FALSE)#on les mets en data frame

# On combine tous les data frames de la liste en un seul data frame
ipc_combined <- do.call(rbind, data_list)

# On nomme les colonnes
colnames(ipc_combined) <- c("IPC_code14", "Description")

#head(ipc_combined)

# On extrait les 4 premiers caractères pour obtenir le code IPC4
ipc_combined$IPC_code4 <- substr(ipc_combined$IPC_code14, 1, 4)

#il reste cependant les codes à 1,2 et 3 caractères, on les enlève donc
ipc_combined <- ipc_combined[nchar(ipc_combined$IPC_code4) == 4, ]

#head(ipc_combined)

# On garde une ligne unique pour chaque IPC_code4 avec sa première description associée
ipc_unique <- ipc_combined[!duplicated(ipc_combined$IPC_code4), c("IPC_code4", "Description")]

head(ipc_unique)

#rendez vous à la fin du code pour utiliser ces ipc_unique en les reliant aux entreprises.



### Manipulation des Informations des entreprises


data_OECD<-read.csv("202202_EPO_App_reg_small.txt", sep = ",", stringsAsFactors = FALSE) #importation

#head(data_OECD)
#str(data_OECD)        # Affiche la structure du dataframe
#summary(data_OECD)    # Affiche le résumé statistique de chaque colonne

data_OECD$year<-as.numeric(substr(data_OECD$app_nbr, 3, 6))  # Extraire l'année
data_OECD_filtered<-subset(data_OECD, ctry_code == "FR" & year >= 2010 & year <= 2020) #en respectant les conditions

#head(data_OECD_filtered)

data_IPC<-read.csv("202202_EPO_IPC_small.txt", sep = ",", stringsAsFactors = FALSE) #importation

#head(data_IPC)

data_IPC$IPC_code4<- substr(data_IPC$IPC, 1, 4) #on prend que les IPC4

#head(data_IPC)

data_combined<- merge(data_OECD_filtered, data_IPC, by = "appln_id", all.x = TRUE)#on combine les données des deux data frame par la colonne appln_id
#head(data_combined)

data_combined<- subset(data_combined, select = -IPC)#on enlève la colonne IPC qui est en trop
#head(data_combined)



### Remplacer les variantes qu'on connaît grâce aux exemples de la consigne :

# On convertit en minuscules les noms d'entreprise pour eviter les confusions
data_combined$app_name <- tolower(data_combined$app_name) 

data_combined$app_name <- gsub(",? sa| inc\\.| sarl|\\(société par actions simplifiée\\)|\\(s\\.?a\\.?s\\.?\\)", "", data_combined$app_name, ignore.case = TRUE)
#j'ai trouvé ,? et \\ à l'aide de chat gpt.

# Supprimer les accents (aussi grâce à chat gpt):
data_combined$app_name <- iconv(data_combined$app_name, from = "UTF-8", to = "ASCII//TRANSLIT")

#supprimer les tirets du 6 et du 8 (idem avec chat gpt):
data_combined$app_name <- gsub("[-–]", "", data_combined$app_name)

#supprimer les virgules
data_combined$app_name <- gsub(",", "", data_combined$app_name)


data_combined$app_name <- trimws(data_combined$app_name) #trimws supprime les espaces en début et fin de nom



all_companies <- unique(data_combined$app_name) #on prend la liste des companies uniques

for(company in all_companies){
  #on stocke les données de la companie actuelle
  company_data <- subset(data_combined, app_name == company)
  #on a donc pour la companie actuelle un nombre de brevets égal au nombre de lignes de company_data.
  # Nombre de brevets
  n_patents <- nrow(company_data)
  
  most_frequent_city<- names(which.max(table(company_data$city)))  # Ville la plus fréquente
  #table(city) trouve le nombre d'occurences de chaque ville et which.max trouve LE plus fréquent.
  
  most_frequent_dept <- names(which.max(table(company_data$reg_code))) #département le plus fréquent
  
  most_frequent_ipc <- names(which.max(table(company_data$IPC_code4))) #IPC le plus fréquent
  
  #On utilise "sort" pour trier les IPC par occurences 
  second_most_frequent_ipc <- sort(table(company_data$IPC_code4), decreasing = TRUE)[2]
  second_most_frequent_ipc <- names(second_most_frequent_ipc)# second IPC le plus fréquent
  
  
  ###On relie maintenant les codes IPC aux descriptions :
  
  # Recherche des descriptions pour les IPC principaux et secondaires de chaque entreprise
  ipc_main_desc <- ipc_unique$Description[ipc_unique$IPC_code4 == most_frequent_ipc]
  ipc_second_desc <- ipc_unique$Description[ipc_unique$IPC_code4 == second_most_frequent_ipc]
  
  #Ajout des résultats au data frame
  base_brevets <- rbind(base_brevets, data.frame(
    firm_name = company,
    n_patents = n_patents,
    ipc_main_code = most_frequent_ipc,
    ipc_main_desc = ifelse(length(ipc_main_desc) > 0, ipc_main_desc, NA),#on verifie qu'il y a bien une description partout
    ipc_second_code = second_most_frequent_ipc,
    ipc_second_desc = ifelse(length(ipc_second_desc) > 0, ipc_second_desc, NA),#idem ici 
    addr_city_main = most_frequent_city,
    addr_dept_main = most_frequent_dept,
    stringsAsFactors = FALSE
  ))
}
edit(base_brevets)


