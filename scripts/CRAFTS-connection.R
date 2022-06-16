# Installing the packages -----------------------------------------------------------------
install.packages("httr")
install.packages("jsonlite")
install.packages("tibble")
install.packages("tidyr")
install.packages("purrr")
install.packages("stringr")
install.packages("dplyr")

# Loading packages --------------------------------------------------------------------
library(httr)
library(jsonlite)
library(tibble)
library(tidyr)
library(purrr) 
library(stringr)   
library(dplyr)

# Settings API -------------------------------------------------------------------------
token <- "XXXXX"
auth <- add_headers('Authorization' = paste("bearer", token))

# Functions queries -------------------------------------------------------

parametric_query <- function(query, auth, typeQuery) {
  res <-
    GET(query , auth)
  if (http_status(res)$category == "Success") {
    output <- switch(typeQuery,
                     "parametric"={
                       result_json <- (content(res))$results$bindings
                       
                       # formatting table
                       result_txt <- content(res, "text")
                       result_json_2 <- fromJSON(result_txt, flatten = TRUE)
                       result_df_2 <- as.data.frame(result_json_2$results$bindings)
                       
                       # result_json <- content(res)
                       # result_df <- as.data.frame(result_json$results$bindings)
                       return(result_df_2)
                     },
                     "resource_out_1"={
                       result_txt <- content(res, "text")
                       result_json_2 <- fromJSON(result_txt,flatten = TRUE,  simplifyDataFrame = FALSE)
                       result_df_2 <- as.data.frame(result_json_2)
                    
                       return(result_df_2)
                     },
                     "resource_out_2"={
                       result_json <- content(res)
                       
                       data_raw <- enframe(unlist(result_json))
                       rgx_split <- "\\."
                       n_cols_max <-
                         data_raw %>%
                         pull(name) %>% 
                         str_split(rgx_split) %>% 
                         map_dbl(~length(.)) %>% 
                         max()
                       nms_sep <- paste0("name", 1:n_cols_max)
                       data_sep <-
                         data_raw %>% 
                         separate(name, into = nms_sep, sep = rgx_split, fill = "right")
                       return(data_sep)
                     },
                     "multiresource"={
                       result_txt <- content(res, "text")
                       result_json <- content(res)
                       
                       result_json_2 <- fromJSON(result_txt,flatten = TRUE)
                       result_df_2 <- as.data.frame(result_json_2)
                    
                       data_raw <- enframe(unlist(result_json))
                       return(data_raw)
                       },
                     {
                       return ("This kind of query doesn't exist. Possible values: parametric, resource_out_1, resource_out_2 or multiresource.")
                     }
                    )
   
  } else{
    return(http_status(res))
  }
}

# Type of Queries -----------------------------------------------------------------
#parametric: to parametric queries defined in CRAFTS API. URL like /query? 
#resource_out_1: to resource queries defined in CRAFTS API. URL like /resource? 
#resource_out_2: same as before but returns another view of data (we can choose) 
#multiresource: to multi resources queries defined in CRAFTS API. URL like /resources? 


# Get all arboretum
get_all_arb <- "https://crafts.gsic.uva.es/apis/virfor/query?id=arboreta"

# Get all provenances from AR01, AR02 and AR03
get_provs_AR <-
  "https://crafts.gsic.uva.es/apis/virfor/query?id=arbProvInfos&arboretum=http://reinfforce.iefc.net/data/arboretum/AR01&arboretum=http://reinfforce.iefc.net/data/arboretum/AR02&arboretum=http://reinfforce.iefc.net/data/arboretum/AR03E"

# Get info from specific provenance
get_info_prov  <-
  "https://crafts.gsic.uva.es/apis/virfor/resource?id=Provenance&iri=http://reinfforce.iefc.net/data/provenance/ACERR_PSE-ALPS"

# Get info from 3 different arboretum
get_info_multiAR  <-
  "https://crafts.gsic.uva.es/apis/virfor/resources?id=Arboretum&iris=http://reinfforce.iefc.net/data/arboretum/AR01&iris=http://reinfforce.iefc.net/data/arboretum/AR02&iris=http://reinfforce.iefc.net/data/arboretum/AR03"


# Launch some queries to retrieve data ----------------------------------------------------------------
all_arb <- parametric_query(get_all_arb,auth,"parametric")
provs_AR <- parametric_query(get_all_arb,auth,"parametric")

info_prov<- parametric_query(get_info_prov,auth,"resource_out_1")
info_prov2 <- parametric_query(get_info_prov,auth,"resource_out_2")

info_multiAR <- parametric_query(get_info_multiAR,auth,"multiresource")

