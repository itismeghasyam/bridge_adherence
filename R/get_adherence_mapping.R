###########################################
#' Script to get adherence mapping of a study
#' Author: aryton.tediarjo@sagebase.org, meghasyam@sagebase.org
##########################################
library(bridgeclient)
library(synapser)
library(tidyverse)
source("R/bridge_helpers.R")
synapser::synLogin()


### Get adherance per date (with datetime)


#' log in to bridge using bridgeclient
bridgeclient::bridge_login(
    study = "mobile-toolbox",
    credentials_file = ".bridge_creds")

#' output reference in synapse
OUTPUT_REF <- list(
    filename = "bridge_mtb_adherence_eventStream.tsv",
    parent_id = "syn26253351",
    git_url = "https://github.com/Sage-Bionetworks/bridge_adherence/blob/main/R/get_adherence_mapping.R"
)

#' Function to get studies mapping
#' @return bridge study name and their id 
get_studies_mapping <- function(maxOffset = 100){
    
    offset_list = seq(0,maxOffset,100)
    
    study_list <- lapply(offset_list, function(offset_){
        print(offset_)
        bridgeclient_get_studies(offset = as.numeric(offset_)) %>%
            .$items %>% 
            purrr::map_dfr(function(x){
                tibble::tibble(
                    name = x$name, 
                    id = x$id)}) %>%
            dplyr::select(
                study_name = name,
                study_id = id)
    }) %>% data.table::rbindlist(fill = T)
    
}

#' Function to get user enrollments using study id
#' @param data dataframe with study_id
#' @return return bridge enrollment response
get_user_enrollments <- function(data){
    data %>%
        dplyr::mutate(enrollments = purrr::map(
            study_id, bridgeclient::get_all_enrollments))
}

#' Function to get userID and externalIDs of enrollment
#' @param data dataframe with enrollment API return
#' @return return mapped user id and externalId
get_user_ids <- function(data){
    data %>% 
        dplyr::mutate(user_ids = purrr::map(enrollments, function(enrollment){
            enrollment %>% 
                purrr::map_dfr(function(x){
                    tibble::tibble(user_id = x$participant$identifier,
                                   external_id = x$externalId)
                })
        })) %>% 
        tidyr::unnest(user_ids, names_repair = "minimal")
}

#' Function to get Bridge Adherence
#' @param data dataframe with study_id and user_id
#' @return Bridge adherence data
get_adherence <- function(data){
    data %>% 
        dplyr::mutate(adherence = purrr::pmap(
            select(., study_id, user_id), bridgeclient_get_adherence))
}

#' Function to get metadata of adherence data
#' e.g: activeOnly, adherencePercent, timestamp, clientTimeZone
#' @param data data with adherence dataframe
#' @return dataframe with adherence metadata
get_adherence_metadata <- function(data){
    data %>% 
        dplyr::mutate(adherence_metadata = purrr::map(
            adherence, 
            function(adherence_df){
                adherence_df %>% 
                    tibble::enframe() %>% 
                    dplyr::filter(name != "dayRangeOfAllStreams",
                                  name != "streams") %>%
                    tidyr::spread(name, value) %>%
                    as.data.frame() %>% 
                    purrr::flatten() %>% 
                    as.data.frame()
                    # dplyr::rowwise() %>%
                    # dplyr::mutate_all(unlist) %>%
                    # dplyr::ungroup()
                    
                    
                    })) %>%
        tidyr::unnest(adherence_metadata)
}

#' Function to get adherence streams endpoint
#' @param data data with adherence dataframe
#' @return dataframe with session information from bridge
get_adherence_streams <- function(data){
    #' Helper function to parse stream contents
    #' by day entries and build it as a dataframe
    parse_adherence_streams <- function(lst) {
        lst$streams %>% 
            purrr::map(function(stream_content){
                stream_content$byDayEntries %>%
                    unname() %>%
                    unlist(recursive = FALSE) %>% 
                    tibble::as_tibble_col() %>% 
                    dplyr::mutate(value = purrr::map(value, function(stream_data){
                        stream_data %>%
                            unlist(recursive = TRUE) %>%
                            tibble::as_tibble_row(.name_repair = "minimal")
                    })) %>%
                    tidyr::unnest(
                        value, keep_empty = TRUE, 
                        names_repair = "minimal")}) %>% 
            purrr::reduce(plyr::rbind.fill)
    }
    
    tryCatch({
        data %>% 
            dplyr::mutate(streams = purrr::map(adherence, parse_adherence_streams)) %>% 
            tidyr::unnest(streams, names_sep = "_")
    }, error = function(e){
        data %>% 
            dplyr::mutate(streams = NA_character_)
    })
}


### Get all studies and subset to studies we want
study_list <- get_studies_mapping(maxOffset = 100) %>%
    dplyr::filter(study_id %in% c(
        'cxhnxd', # WUSTL Mobile Toolbox 
        'htshxm', # HNRP Mobile Toolbox
        'hktrrx', # The Mobile Toolbox for Monitoring Cognitive Function
        'pmbfzc' # Emory Healthy Aging Study
    )) 

### Get user enrollments in studies and user 
study_user_ids <- study_list %>% 
    get_user_enrollments() %>% 
    get_user_ids()

### User activity per user
# sub_id <- study_user_ids %>% 
#     dplyr::filter(user_id == '24ftAlI1NGgQWjRqo27lUKpP')

user_activity <- study_user_ids %>% 
    dplyr::select(study_id, user_id, external_id) %>% 
    unique() 

user_activity_temp <- apply(user_activity,1,function(x){
    curr_activity <- bridgeclient_get_activityevents(x[['study_id']], x[['user_id']])
    curr_activity_df <-  curr_activity$items %>%
        data.table::rbindlist(fill=TRUE) %>% 
        dplyr::mutate(user_id = x[['user_id']],
                      # study_id = x[['study_id']],
                      external_id = x[['external_id']]) %>% 
        dplyr::select(eventId, user_id, external_id) %>% 
        dplyr::mutate(val = T) %>% 
        tidyr::spread(eventId,val)
    
    # curr_activity_df[is.na(curr_activity_df)] <- FALSE
    
    return(curr_activity_df)
}) %>% data.table::rbindlist(fill = T)

### add a column for session start -> is their sum of all assessment type columns >0 ??
assesment_cols <- colnames(user_activity_temp)[grepl('assessment', colnames(user_activity_temp))]

user_activity_temp <- user_activity_temp %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(nAssessmentsDone = sum(c_across(all_of(assesment_cols)), na.rm = T )) %>% 
    dplyr::ungroup()

### save to synapse
readr::write_tsv(user_activity_temp, 'bridge_mtb_assessments.tsv')
file <- synapser::File('bridge_mtb_assessments.tsv', 
                       parent = OUTPUT_REF$parent_id)

activity <- synapser::Activity(
    # executed = OUTPUT_REF$git_url,
    # name = "fetch bridge data",
    description = "Get assessments completion details from Bridge Activity events"
)
synStore(file, activity = activity)
unlink('bridge_mtb_assessments.tsv')


### Get adherence record per user
user_adherence <- study_user_ids %>% 
    get_adherence()

### Get adherence metadata and streams
user_adherence_metadata <- user_adherence %>% 
    get_adherence_metadata() %>% 
    get_adherence_streams() %>% 
    dplyr::select(-enrollments)

### Write required columns to output
main_output <- user_adherence_metadata %>% 
    dplyr::select(-type, -enrollments, -adherence)

readr::write_tsv(main_output,OUTPUT_REF$filename)



### save to synapse
file <- synapser::File(OUTPUT_REF$filename, 
                       parent = OUTPUT_REF$parent_id)

activity <- synapser::Activity(
    executed = OUTPUT_REF$git_url,
    name = "fetch bridge data",
    description = "normalize Bridge Adherence eventStreams"
)
synStore(file, activity = activity)
unlink(OUTPUT_REF$filename)


#' main <- function(){
#'     #' get adherence 
#'     adherence_mapping <- get_studies_mapping() %>%
#'         dplyr::filter(study_id %in% c(
#'             'cxhnxd', # WUSTL Mobile Toolbox 
#'             'htshxm', # HNRP Mobile Toolbox
#'             'fmqcjv' # Mobile Toolbox Study 
#'         )) %>% 
#'         get_user_enrollments() %>% 
#'         get_user_ids() %>%
#'         get_adherence() %>% 
#'         get_adherence_metadata() %>% 
#'         get_adherence_streams() %>% 
#'         dplyr::select(-type, -enrollments, -adherence) %>% 
#'         readr::write_tsv(OUTPUT_REF$filename)
#'     
#'     #' save to synapse
#'     file <- synapser::File(OUTPUT_REF$filename, 
#'                           parent = OUTPUT_REF$parent_id)
#'     
#'     activity <- synapser::Activity(
#'         executed = OUTPUT_REF$git_url,
#'         name = "fetch bridge data",
#'         description = "normalize Bridge Adherence eventStreams"
#'     )
#'     synStore(file, activity = activity)
#'     unlink(OUTPUT_REF$filename)
#' }
#' 
#' main()
