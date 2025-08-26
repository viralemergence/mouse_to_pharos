source("packages.R")



#### Project metadata

#  create metadata from template
# wddsWizard::use_wdds_template(template_file = "project_metadata_template.csv",folder = "data",file_name = "project_metadata.csv")

project_metadata_raw <- readr::read_csv("data/project_metadata.csv")


## turn empty strings into NAs in the group field
project_metadata_groups <- project_metadata_raw |>
  dplyr::mutate(Group = dplyr::case_when(
    Group != "" ~ Group,
    TRUE ~ NA
  ))

## use `fill` to complete the items column and `mutate` to make groups a little
## more ergonomic

project_metadata_filled <- tidyr::fill(data = project_metadata_groups, Group)

# get ids for components of a group.
project_metadata_ids <- project_metadata_filled |>
  dplyr::mutate(
    entity_id = stringr::str_extract(string = Group, pattern = "[0-9]{1,}"),
    # make sure that there are no NA entity IDs
    entity_id = dplyr::case_when(
      is.na(entity_id) ~ "1",
      TRUE ~ entity_id
    )
  ) |>
  # drop entity ids from group field and convert to camel case
  dplyr::mutate(
    Group = stringr::str_replace_all(string = Group, pattern = " [0-9]{1,}", replacement = ""),
    Group = snakecase::to_lower_camel_case(Group)
  )

project_metadata_ids

## split dataframe by Group for further processing #####

project_metadata_list <- split(project_metadata_ids, project_metadata_ids$Group)


# The `get_entity` function creates standard entities that will be easier to transform json

project_metadata_list_entities <- purrr::map(project_metadata_list, function(x) {
  if (all(x$entity_id == "1")) {
    out <- get_entity(x)
    return(out)
  }
  
  x_list <- split(x, x$entity_id)
  names(x_list) <- NULL
  out <- purrr::map(x_list, get_entity)
  return(out)
})

# make json ####

project_metadata_prepped <- prep_for_json(project_metadata_list_entities) 

project_metadata_json <- project_metadata_prepped |>
  jsonlite::toJSON(pretty = TRUE)


schema <- wdds_json(version = "latest", file = "schemas/project_metadata.json")

project_validator <- jsonvalidate::json_validator(schema, engine = "ajv")

project_validation <- project_validator(project_metadata_json, verbose = TRUE)

## check for errors!

errors <- attributes(project_validation)

###### data

dat_pancov_share <- readr::read_csv("data/dat_pancov_share.csv",col_types = readr::cols(lat = "d", lon = "d",.default = readr::col_character()), trim_ws = TRUE)
str(dat_pancov_share)


wddsWizard::disease_data_required_fields

## rename and add collection info

dat_pancov_renamed <- dat_pancov_share |>
  dplyr::rename( "sampleID" = "...1" ,
                 "animalID" = "Tag_No" ,# assumption
                "detectionOutcome" = "PanCov_Gel_Result_Final",
                "longitude" = "lon",
                "latitude" = "lat",
                "recapture" = "Recapture"
                ) |>
    dplyr::mutate(detectionTarget = "Coronaviridae",
         detectionMethod = "PCR",
         sampleCollectionMethod = "swab",
         sampleCollectionBodyPart = "mouth, anus",
         forwardPrimerSequence = "GGTTGGGAYTAYCCHAARTGYGA, CCRTCATCAGAHARWATCAT, CCRTCATCACTHARWATCAT, GAYTAYCCHAARTGTGAYAGA,GAYTAYCCHAARTGTGAYMGH ",
         primerCitation = "https://doi.org/10.3390/v13040599",
         liveCapture = TRUE,
         hostIdentification = "Peromyscus leucopus")



names(dat_pancov_renamed)

### get parasite ids

parasite_ids <- dat_pancov_renamed |> 
  dplyr::filter(PanCoV_Lineage != "Negative") |>
  dplyr::mutate(parasiteIdentification = stringr::str_split(string =PanCoV_Lineage,pattern = " "),
         parasiteIdentification = purrr::map_chr(parasiteIdentification,function(x){
          y  <- x[[2]]
          out <- stringr::str_remove_all(string = y, pattern = "\\(|\\)")
          return(out)
         })
         ) |>
  dplyr::select(sampleID,parasiteIdentification)

dat_pancov_id <- dplyr::left_join(dat_pancov_renamed,parasite_ids,"sampleID")


## format dates --- what kind of date?

date_df <- dat_pancov_id$Date |>
  stringr::str_split("/") |>
  purrr::map_df(function(x){
    data.frame( "collectionDay" = as.numeric(x[2]), 
                "collectionMonth" = as.numeric(x[1]),
                "collectionYear" = as.numeric(x[3]) )
  })

dat_pancov_dates <- cbind(dat_pancov_id,date_df) |>
  dplyr::select(-c(Date,PanCoV_Lineage))

## check for required fields
all(wddsWizard::disease_data_required_fields %in% names(dat_pancov_dates))


dat_pancov_prepped <- wddsWizard::prep_data(dat_pancov_dates)

## wrap the prepped data in list
dat_pancov_json <- dat_pancov_prepped |>
  jsonlite::toJSON(pretty = TRUE)


# validate data

schema <- wddsWizard::wdds_json(version = "latest", file = "schemas/disease_data.json")

dd_validator <- jsonvalidate::json_validator(schema, engine = "ajv")

dd_validation <- dd_validator(dat_pancov_json, verbose = TRUE)

## check for errors!

errors <- attributes(dd_validation)
errors


#### put it all together

data_package <- list(
  disease_data = dat_pancov_prepped,
  project_metadata = project_metadata_prepped
)

# check that all required fields are in the data

req_field_check <- wddsWizard::schema_required_fields %in% names(data_package)


# convert to json

data_package_json <- jsonlite::toJSON(data_package, pretty = TRUE)

### validate json

schema <- wdds_json(version = "latest", file = "wdds_schema.json")

wdds_validator <- jsonvalidate::json_validator(schema, engine = "ajv")

project_validation <- wdds_validator(data_package_json, verbose = TRUE)

if (project_validation) {
  print("Your data package is valid! 🎊 ")
} else {
  errors <- attributes(project_validation)
  errors$errors
}


data_package_json

project_metadata_prepped$creators
dat_pancov_prepped

readr::write_csv(dat_pancov_dates,"outputs/wdds_compliant_pancov.csv")

