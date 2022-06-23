# Installing the packages -----------------------------------------------------------------
install.packages("httr")
install.packages("jsonlite")
install.packages("tibble")
install.packages("tidyr")
install.packages("purrr")
install.packages("stringr")
install.packages("dplyr")
install.packages("whisker")

# Loading packages --------------------------------------------------------------------
library(httr)
library(jsonlite)
library(tibble)
library(tidyr)
library(purrr)
library(stringr)
library(dplyr)
library(whisker) # Mustache, templates

# Settings API -------------------------------------------------------------------------
token <- "XXXXX"
auth <- add_headers('Authorization' = paste("bearer", token))

url_api <- "https://crafts.gsic.uva.es/apis/virfor"
prefix_rff <- "http://reinfforce.iefc.net/data/"

# Query templates
uri_param_query <- paste0(url_api, "/query?id={{nameQuery}}")
uri_param_query_type <-
  paste0(url_api, "/query?id={{nameQuery}}&type={{{type}}}")

#"https://crafts.gsic.uva.es/apis/virfor/resource?id={{}}}&{{}}"
#"https://crafts.gsic.uva.es/apis/virfor/resources?id={{}}&iris={{}}"


# Functions queries -------------------------------------------------------
parametric_query <- function(query, auth, typeQuery) {
  res <-
    GET(query , auth)
  if (http_status(res)$category == "Success") {
    output <- switch(
      typeQuery,
      "parametric" = {
        result_json <- (content(res))$results$bindings
        
        # formatting table
        result_txt <- content(res, "text")
        result_json_2 <-
          fromJSON(result_txt, flatten = TRUE)
        result_df_2 <-
          as.data.frame(result_json_2$results$bindings)
        
        # result_json <- content(res)
        # result_df <- as.data.frame(result_json$results$bindings)
        return(result_df_2)
      },
      "resource_out_1" = {
        result_txt <- content(res, "text")
        result_json_2 <-
          fromJSON(result_txt,
                   flatten = TRUE,
                   simplifyDataFrame = FALSE)
        result_df_2 <- as.data.frame(result_json_2)
        
        return(result_df_2)
      },
      "resource_out_2" = {
        result_json <- content(res)
        
        data_raw <- enframe(unlist(result_json))
        rgx_split <- "\\."
        n_cols_max <-
          data_raw %>%
          pull(name) %>%
          str_split(rgx_split) %>%
          map_dbl( ~ length(.)) %>%
          max()
        nms_sep <- paste0("name", 1:n_cols_max)
        data_sep <-
          data_raw %>%
          separate(name,
                   into = nms_sep,
                   sep = rgx_split,
                   fill = "right")
        return(data_sep)
      },
      "resource_out_json" = {
        result_json <- content(res)
        return(result_json)
      },
      "multiresource" = {
        result_txt <- content(res, "text")
        result_json <- content(res)
        
        result_json_2 <-
          fromJSON(result_txt, flatten = TRUE)
        result_df_2 <- as.data.frame(result_json_2)
        
        data_raw <- enframe(unlist(result_json))
        return(data_raw)
      },
      {
        return (
          "This kind of query doesn't exist. Possible values: parametric, resource_out_1, resource_out_2 or multiresource."
        )
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
#resource_out_json: same as before but returns data in JSON format
#multiresource: to multi resources queries defined in CRAFTS API. URL like /resources?

# Examples of Queries

# Get all arboretum
nameQuery <- "arboreta"
get_all_arb <- whisker.render(uri_param_query)

# Get all taxa
nameQuery <- "taxa"
get_all_taxa <- whisker.render(uri_param_query)

# Get all provenances
nameQuery <- "provenances"
get_all_prov <- whisker.render(uri_param_query)

# Get all families
param <-
  list(nameQuery = "taxa", type = "https://datos.iepnb.es/def/sector-publico/medio-ambiente/ifn/Family")
get_all_families <- whisker.render(uri_param_query_type, param)

# Get all provenances from AR01, AR02 and AR03
get_provs_AR <-
  "https://crafts.gsic.uva.es/apis/virfor/query?id=arbProvInfos&arboretum=http://reinfforce.iefc.net/data/arboretum/AR01&arboretum=http://reinfforce.iefc.net/data/arboretum/AR02&arboretum=http://reinfforce.iefc.net/data/arboretum/AR03"

# Get info from specific provenance
get_info_prov  <-
  "https://crafts.gsic.uva.es/apis/virfor/resource?id=Provenance&iri=http://reinfforce.iefc.net/data/provenance/ACERR_PSE-ALPS"

# Get info from 3 different arboretum
get_info_multiAR  <-
  "https://crafts.gsic.uva.es/apis/virfor/resources?id=Arboretum&iris=http://reinfforce.iefc.net/data/arboretum/AR01&iris=http://reinfforce.iefc.net/data/arboretum/AR02&iris=http://reinfforce.iefc.net/data/arboretum/AR03"

# Get all info from one provenance inside an arboretum: AR01-ACERR_PSE-WALE
get_info_arbProv <-
  "https://crafts.gsic.uva.es/apis/virfor/resource?id=ArbProvInfo&iri=http://reinfforce.iefc.net/data/arbProvenance/AR01-ACERR_PSE-WALE"

get_info_AR01 <-
  "https://crafts.gsic.uva.es/apis/virfor/resource?id=Arboretum&iri=http://reinfforce.iefc.net/data/arboretum/AR01"

## Use cases

# Launch some queries to retrieve data ----------------------------------------------------------------

# 1. Get all arboretum
all_arb <- parametric_query(get_all_arb, auth, "parametric")
# 2. Get all provenances from AR01, AR02 and AR03
provs_AR <- parametric_query(get_provs_AR, auth, "parametric")
# 3. Get info from specific provenance: different output formats
info_prov <- parametric_query(get_info_prov, auth, "resource_out_1")
info_prov2 <- parametric_query(get_info_prov, auth, "resource_out_2")
# 4. Get Get info from 3 different arboretum
info_multiAR <-
  parametric_query(get_info_multiAR, auth, "multiresource")
# 5. Get country and climatic measures AR01
info_AR01 <- parametric_query(get_info_AR01, auth, "resource_out_json")

# Get info about AR01
country <- info_AR01$country$label$en
ADI <- info_AR01$lastADIInSquareRootDegreesCelsiusDayPerMilimeter
Pmean_mm <- info_AR01$lastAnnualPrecipitationInMilimeters
Tmean_degrees <- info_AR01$lastMeanAnnualTempInDegreesCelsius


# Get growths and survivals from one arboretum (AR01)

growths = data.frame()
survivals = data.frame()

for (prov in info_AR01$arbProvInfo) {
  get_info_arbProv <-
    paste0("https://crafts.gsic.uva.es/apis/virfor/resource?id=ArbProvInfo&iri=",
           prov)
  info_provAR01 <-
    parametric_query(get_info_arbProv, auth, "resource_out_json")
  
  # Get survivals
  if (!is.null(info_provAR01$survivalRates)) {
    iri <- info_provAR01$iri
    value <- info_provAR01$survivalRates$value
    survivals <-
      rbind(survivals, data.frame(data.frame(iri), data.frame(value)))
  }
  #Get growths
  for (tree in info_provAR01$trees) {
    iri <- tree$iri
    value <- tree$lastAnnualGrowthRateInCentimetersPerYear
    growths <-
      rbind(growths, data.frame(data.frame(iri), data.frame(value)))
  }
}

rm(iri, value, prov, tree)


# Get growths and survivals from multiple arboretum (AR01, AR02 and AR03)
growths_multi = data.frame()
survivals_multi = data.frame()


for (prov in provs_AR$arbprovinfo.value) {
  get_info_arbProv <-
    paste0("https://crafts.gsic.uva.es/apis/virfor/resource?id=ArbProvInfo&iri=",
           prov)
  info_provARXX <-
    parametric_query(get_info_arbProv, auth, "resource_out_json")
  
  # Get survivals
  if (!is.null(info_provARXX$survivalRates)) {
    iri <- info_provARXX$iri
    value <- info_provARXX$survivalRates$value
    survivals_multi <-
      rbind(survivals_multi, data.frame(data.frame(iri), data.frame(value)))
  }
  #Get growths
  if (!is.null(info_provARXX$trees)) {
    for (tree in info_provARXX$trees) {
      if (typeof(tree) != "list") {
        iri <- info_provARXX$trees$iri
        value <-
          info_provARXX$trees$lastAnnualGrowthRateInCentimetersPerYear
      } else{
        iri <- tree$iri
        value <- tree$lastAnnualGrowthRateInCentimetersPerYear
      }
      growths_multi <-
        rbind(growths_multi, data.frame(data.frame(iri), data.frame(value)))
    }
  }
}
rm(iri, value, prov, tree)

