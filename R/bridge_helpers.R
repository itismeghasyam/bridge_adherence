bridgeclient_get_activityevents <- function(study_id,user_id){
    bridgeclient:::bridgeGET(
        glue::glue("/v5/studies/{study_id}/participants/{user_id}/activityevents"))
}


bridgeclient_get_studies <- function(){
    bridgeclient:::bridgeGET("v5/studies?offsetBy=0&pageSize=100&includeDeleted=false")
}

bridgeclient_get_adherence <- function(study_id,user_id){
  bridgeclient:::bridgeGET(
    glue::glue("/v5/studies/{study_id}/participants/{user_id}/adherence/eventstream"))
}
