###########################################
# Script to change dataGroups in Bridge for sage-mpower-2
# Author: meghasyam@sagebase.org
# Some of the functions have been repurposed from Aryton Tediarjo's Bridge Adherence script
##########################################

##########################################
## Required libraries
##########################################
library(bridgeclient)
library(synapser)
library(tidyverse)
synapser::synLogin()

##########################################
## Required functions
##########################################
get_new_dataGroups <- function(dataGroups, study_id){
  ## Add new dataGroups to participants, as below
  # - studies:  "@HOME PD", "Udall Superusers"   should have dataGroup "show_3_cogntive" added.
  # -  studies "Sage", and those missing a study  should have  "show_8_cognitive" added.
  # - and people in the    "Stepwise", "Test" studies should have nothing changed.
  
  new_dataGroups <- dataGroups # default is no change 
  
  if(study_id %in% c('at-home-pd', 'Udall-superusers')){
    # @HOME PD or Udall-superusers users have show_3_cognitive added to their dataGroups
      new_dataGroups <- append(dataGroups, 'show_3_cognitive')
  }
  
  if(study_id == 'sage' || is.na(study_id)){
    # Sage users or those missing a study_id have show_8_cognitive added to their dataGroups
      new_dataGroups <- append(dataGroups, 'show_8_cognitive')
  }
  
  # Stepwise and Test users have no change as they will not pass through the above if statements
  
  return(new_dataGroups)
}

# update participant info on Bridge using POST
bridgeclient_POST_participant <- function(user_id, updated_participant_info){
  # from https://developer.sagebridge.org/swagger-ui/index.html#/Participants/updateParticipant
  bridgeclient:::bridgePOST(
    glue::glue("v3/participants/{user_id}"), body = updated_participant_info)
}

# get all studies in the App, accesible to the person logged in
bridgeclient_get_studies <- function(){
  bridgeclient:::bridgeGET("v5/studies?offsetBy=0&pageSize=100&includeDeleted=false")
}

#' Function to get studies mapping
#' @return bridge study name and their id 
get_studies_mapping <- function(){
  bridgeclient_get_studies() %>%
    .$items %>% 
    purrr::map_dfr(function(x){
      tibble::tibble(
        name = x$name, 
        id = x$id)}) %>%
    dplyr::select(
      study_name = name,
      study_id = id)
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

##########################################
## Login, Download data and Set up
##########################################
#' log in to bridge using bridgeclient
bridgeclient::bridge_login(
  study = "sage-mpower-2",
  email = '',
  password = '')

### Get all studies and subset to studies we want
study_list <- get_studies_mapping() 

### Get user enrollments in studies and user ids 
study_user_ids <- study_list %>% 
  get_user_enrollments() %>% 
  get_user_ids()

## All users without the enrollments list
study_users <- study_user_ids %>% 
  dplyr::select(-enrollments) 

## Remove users with Test and Stepwise study_name 
study_users_filtered <- study_users %>% 
  dplyr::filter(!(study_name %in% c('Test','Stepwise'))) %>% 
  dplyr::mutate(row_num = dplyr::row_number()) %>% # for position in participant_info list below
  dplyr::mutate(row_num = as.numeric(row_num))

##########################################
## Update dataGroups
##########################################

## Get updated study participant info, by updating dataGroups

#################### uncomment the below to actually update the dataGroups

study_users_filtered_info <- apply(study_users_filtered,1,function(x){
  participant_info <- bridgeclient::get_participant(user_id = x[['user_id']])
  
  ### uncomment the below to actually update the dataGroups
  # participant_info$dataGroups <- get_new_dataGroups(participant_info$dataGroups, x[['study_id']])
  participant_info
}) 


### TEST ZONE

aa <- lapply(study_users_filtered_info, function(x){
  x[['dataGroups']] %>% 
    as.data.frame() %>% 
    t() %>%
    `colnames<-`('dataGroups') %>%
    as.data.frame() %>% 
    dplyr::mutate(user_id = x[['id']])
}) %>% data.table::rbindlist(fill = T) %>% 
  dplyr::left_join(study_users_filtered) %>% 
  dplyr::filter(dataGroups %in% c('show_3_cognitive',
                                  'show_8_cognitive')) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataGroup_valid = isDataGroupValid(dataGroups,study_id)) %>% 
  dplyr::ungroup()

isDataGroupValid <- function(dataGroups, study_id){
  if(study_id %in% c('at-home-pd', 'Udall-superusers')){
    if(dataGroups == 'show_3_cognitive'){
      return(T)
    }else{
      return(F)
    }
  }
  
  if(study_id == 'sage' || is.na(study_id)){
    # Sage users or those missing a study_id have show_8_cognitive added to their dataGroups
    if(dataGroups == 'show_8_cognitive'){
      return(T)
    }else{
      return(F)
    }
  }
  
}



##########################################
## POST updates to Bridge
##########################################
## user no 1078, i,e user_id == 'wzE1KiYmxwprHZwv_Q_ROdIE', is Alx
## Trying to POST an update to this participant acc to see if it works
# aa <- bridgeclient_POST_participant('wzE1KiYmxwprHZwv_Q_ROdIE', study_users_info[[1078]])

#################### Uncomment inside the loop to update all users
udpate_reports <- apply(study_users_filtered, 1, function(x_user){
  
  # for debugging and progress update
  print(x_user[['user_id']])
  print(as.numeric(x_user[['row_num']]))
  
  ### uncomment below to update all users
  # bridgeclient_POST_participant(user_id = x_user[['user_id']],
  #                               study_users_filtered_info[[as.numeric(x_user[['row_num']])]])

  # study_users_filtered_info[[as.numeric(x_user[['row_num']])]]
  
})

## Looks good to me. I see an updated dataGroups for 'wzE1KiYmxwprHZwv_Q_ROdIE', 'show_3_cognitive' now shows up
