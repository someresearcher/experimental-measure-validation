library(stringr)
library(anytime)
library(dplyr)
library(tidyr)
library(readr)
library(kableExtra)


resultsPath = "./results"

loadData <- function(path, subPath) {
  return(read.csv2(paste(path,subPath,".csv", sep=""), sep=",", check.names = FALSE))
}

getRequestTraceNameFromMeasures <- function(nameFromExperiment) {
  return(
   switch(nameFromExperiment, 
           "Add Product 2 to Cart"="Add Product To Cart",
           "Add Product to Cart"="Add Product To Cart",
           "Home"="Index Page",
           "List Products"="Show Category",
           "List Products with different page"="Show Category",
           "Login"="User Login",
           "Logout"="User Logout",             
           "Look at Product"="Product Page",
           "Add Product 2 to Cart - discrete"="Add Product To Cart",
           "Add Product to Cart - discrete"="Add Product To Cart",
           "Home - discrete"="Index Page",
           "List Products - discrete"="Show Category",
           "List Products with different page - discrete"="Show Category",
           "Login - discrete"="User Login",
           "Logout - discrete"="User Logout",             
           "Look at Product - discrete"="Product Page",
           "n/a"
    )
  )
}


coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}


addMeasures <- function(resultSet, resultsPath, subPath) {
  measures <- read.csv2(paste(resultsPath, subPath, "-model-measures.csv", sep=""))
  systemMeasures <- measures %>% 
    filter(entityType == "system") %>% 
    filter(measureKey %in% c("serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel", "ratioOfCachedDataAggregates", "")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  requestTraceMeasures <- measures %>% 
    filter(entityType == "requestTrace") %>% 
    filter(measureKey %in% c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  resultSet <- resultSet %>% left_join(systemMeasures, by=join_by(entityType == entityType))
  resultSet <- resultSet %>% mutate(requestTraceName = unlist(lapply(label, getRequestTraceNameFromMeasures))) %>% coalesce_join(requestTraceMeasures, by=join_by(requestTraceName == entityName), join=dplyr::left_join)
  resultSet <- resultSet %>% mutate("entityName" = coalesce(entityName, requestTraceName)) %>% relocate("entityName", .after="entityType") %>% select (-c("requestTraceName"))
  return(resultSet)
}

combinedLoad <- function(path) {
  rawResultSet <- loadData(resultsPath, path)
  rawResultSet <- rawResultSet %>% select (-c("mean(latency) [ms]", "median(latency) [ms]", "90th_percentile(latency) [ms]", "mean(receivedBytes) [byte]", "median(receivedBytes) [byte]", "90th_percentile(receivedBytes) [byte]", "mean(sentBytes) [byte]")) %>%
    filter(!str_detect(label, "- discrete")) %>%
    relocate("entityType", .before="mean(elapsed) [ms]") %>%
    relocate("label", .before="entityType") %>%
    relocate("architectureVariation", .before="label" )
  
  rawResultSet["success_rate [%]"] <- as.numeric(unlist(rawResultSet["success_rate [%]"])) * 100
  
  # TODO find a better, more generic solution
  rawResultSet["mean(elapsed) [ms]"] <- as.numeric(unlist(rawResultSet["mean(elapsed) [ms]"]))
  rawResultSet["90th_percentile(elapsed) [ms]"] <- as.numeric(unlist(rawResultSet["90th_percentile(elapsed) [ms]"]))
  rawResultSet["throughput [requests / s]"] <- as.numeric(unlist(rawResultSet["throughput [requests / s]"]))
  
  combinedResultSet <- addMeasures( rawResultSet, resultsPath, path )
  
  combinedResultSet <- combinedResultSet %>%
    mutate(architectureVariation=str_remove(architectureVariation, "teastore-private-nlb-")) %>% 
    mutate(entityName = ifelse(entityType == "system", "teaStore", entityName))
  
  combinedResultSet["serviceReplicationLevel"] <- as.numeric(unlist(combinedResultSet["serviceReplicationLevel"]))
  combinedResultSet["medianServiceReplication"] <- as.numeric(unlist(combinedResultSet["medianServiceReplication"]))
  combinedResultSet["smallestReplicationValue"] <- as.numeric(unlist(combinedResultSet["smallestReplicationValue"]))
  combinedResultSet["storageReplicationLevel"] <- as.numeric(unlist(combinedResultSet["storageReplicationLevel"]))
  combinedResultSet["ratioOfCachedDataAggregates"] <- as.numeric(unlist(combinedResultSet["ratioOfCachedDataAggregates"]))
  combinedResultSet["dataReplicationAlongRequestTrace"] <- as.numeric(unlist(combinedResultSet["dataReplicationAlongRequestTrace"]))
  
  combinedResultSet <- combinedResultSet %>% mutate_if(is.numeric, ~round(., 2))
  
  return(combinedResultSet)
}

#teastorePrivateNlbOriginal <- loadData(resultsPath, "/original/teastore-private-nlb-original")
#teastorePrivateNlbOriginal <- addMeasures(teastorePrivateNlbOriginal, "/original/teastore-private-nlb-original")
teastorePrivateNlbOriginal <- combinedLoad("/original/teastore-private-nlb-original")

teastorePrivateNlbHighreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-highreplication")
teastorePrivateNlbLowreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-lowreplication")
teastorePrivateNlbNoreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-noreplication")
teastorePrivateNlbOnlyuireplication  <- combinedLoad("/serviceReplication/teastore-private-nlb-onlyuireplication")
teastorePrivateNlbOnlyupstreamreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-onlyupstreamreplication")
teastorePrivateNlbWithfailuresHighreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-withfailures-highreplication")
teastorePrivateNlbWithfailuresLowreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-withfailures-lowreplication")
teastorePrivateNlbWithfailuresNoreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-withfailures-noreplication")
teastorePrivateNlbWithfailuresOnlyuireplication <- combinedLoad("/serviceReplication/teastore-private-nlb-withfailures-onlyuireplication")
teastorePrivateNlbWithfailuresOnlyupstreamreplication <- combinedLoad("/serviceReplication/teastore-private-nlb-withfailures-onlyupstreamreplication")

teastorePrivateNlbNoreplicationWithcaching <- combinedLoad("/verticalReplication/teastore-private-nlb-noreplication-withcaching")
teastorePrivateNlbWithfailuresWithcaching <- combinedLoad("/verticalReplication/teastore-private-nlb-withfailures-withcaching")
teastorePrivateNlbNoreplicationNocaching  <- combinedLoad("/verticalReplication/teastore-private-nlb-noreplication-nocaching")
teastorePrivateNlbNoreplicationWithsomecaching  <- combinedLoad("/verticalReplication/teastore-private-nlb-noreplication-withsomecaching")
teastorePrivateNlbWithfailuresNocaching    <- combinedLoad("/verticalReplication/teastore-private-nlb-withfailures-nocaching")
teastorePrivateNlbWithfailuresWithsomecaching  <- combinedLoad("/verticalReplication/teastore-private-nlb-withfailures-withsomecaching")

teastorePrivateNlbRdsSingleHighreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-single-highreplication")
teastorePrivateNlbRdsSingleLowreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-single-lowreplication")
teastorePrivateNlbRdsSingleNocaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-single-nocaching")
teastorePrivateNlbRdsSingleNoreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-single-noreplication")
teastorePrivateNlbRdsSingleWithcaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-single-withcaching")
teastorePrivateNlbRdsSingleWithsomecaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-single-withsomecaching")
teastorePrivateNlbRdsThreeHighreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-three-highreplication")
teastorePrivateNlbRdsThreeLowreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-three-lowreplication")
teastorePrivateNlbRdsThreeNocaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-three-nocaching")
teastorePrivateNlbRdsThreeNoreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-three-noreplication")
teastorePrivateNlbRdsThreeWithcaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-three-withcaching")
teastorePrivateNlbRdsThreeWithsomecaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-three-withsomecaching")
teastorePrivateNlbRdsTwoHighreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-two-highreplication")
teastorePrivateNlbRdsTwoLowreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-two-lowreplication")
teastorePrivateNlbRdsTwoNocaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-two-nocaching")
teastorePrivateNlbRdsTwoNoreplication <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-two-noreplication")
teastorePrivateNlbRdsTwoWithcaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-two-withcaching")
teastorePrivateNlbRdsTwoWithsomecaching <- combinedLoad("/horizontalDataReplication/teastore-private-nlb-rds-two-withsomecaching")

teastorePrivateNlbHighreplicationNocaching <- combinedLoad("/combined/teastore-private-nlb-highreplication-nocaching")
teastorePrivateNlbHighreplicationWithcaching <- combinedLoad("/combined/teastore-private-nlb-highreplication-withcaching")
teastorePrivateNlbLowreplicationNocaching <- combinedLoad("/combined/teastore-private-nlb-lowreplication-nocaching")
teastorePrivateNlbLowreplicationWithcaching <- combinedLoad("/combined/teastore-private-nlb-lowreplication-withcaching")
teastorePrivateNlbWithfailuresHighreplicationNocaching <- combinedLoad("/combined/teastore-private-nlb-withfailures-highreplication-nocaching")
teastorePrivateNlbWithfailuresHighreplicationWithcaching <- combinedLoad("/combined/teastore-private-nlb-withfailures-highreplication-withcaching")
teastorePrivateNlbWithfailuresLowreplicationNocaching <- combinedLoad("/combined/teastore-private-nlb-withfailures-lowreplication-nocaching")
teastorePrivateNlbWithfailuresLowreplicationWithcaching <- combinedLoad("/combined/teastore-private-nlb-withfailures-lowreplication-withcaching")


allResults <- rbind(teastorePrivateNlbOriginal,teastorePrivateNlbHighreplication,teastorePrivateNlbLowreplication,teastorePrivateNlbNoreplication,teastorePrivateNlbOnlyuireplication,teastorePrivateNlbOnlyupstreamreplication,teastorePrivateNlbWithfailuresHighreplication,teastorePrivateNlbWithfailuresLowreplication,teastorePrivateNlbWithfailuresNoreplication,teastorePrivateNlbWithfailuresOnlyuireplication,teastorePrivateNlbWithfailuresOnlyupstreamreplication,teastorePrivateNlbNoreplicationWithcaching,teastorePrivateNlbWithfailuresWithcaching,teastorePrivateNlbNoreplicationNocaching ,teastorePrivateNlbNoreplicationWithsomecaching,teastorePrivateNlbWithfailuresNocaching,teastorePrivateNlbWithfailuresWithsomecaching,teastorePrivateNlbRdsSingleHighreplication,teastorePrivateNlbRdsSingleLowreplication,teastorePrivateNlbRdsSingleNocaching,teastorePrivateNlbRdsSingleNoreplication,teastorePrivateNlbRdsSingleWithcaching,teastorePrivateNlbRdsSingleWithsomecaching,teastorePrivateNlbRdsThreeHighreplication,teastorePrivateNlbRdsThreeLowreplication,teastorePrivateNlbRdsThreeNocaching ,teastorePrivateNlbRdsThreeNoreplication ,teastorePrivateNlbRdsThreeWithcaching,teastorePrivateNlbRdsThreeWithsomecaching,teastorePrivateNlbRdsTwoHighreplication,teastorePrivateNlbRdsTwoLowreplication,teastorePrivateNlbRdsTwoNocaching,teastorePrivateNlbRdsTwoNoreplication,teastorePrivateNlbRdsTwoWithcaching ,teastorePrivateNlbRdsTwoWithsomecaching,teastorePrivateNlbHighreplicationNocaching,teastorePrivateNlbHighreplicationWithcaching,teastorePrivateNlbLowreplicationNocaching,teastorePrivateNlbLowreplicationWithcaching,teastorePrivateNlbWithfailuresHighreplicationNocaching,teastorePrivateNlbWithfailuresHighreplicationWithcaching ,teastorePrivateNlbWithfailuresLowreplicationNocaching,teastorePrivateNlbWithfailuresLowreplicationWithcaching)


investigateServiceReplicationTimeBehaviour <- function(data) {
  result <- data %>%
    select(-c("success_rate [%]", "median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]", "storageReplicationLevel", "ratioOfCachedDataAggregates","dataReplicationAlongRequestTrace" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(serviceReplicationLevel, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis1a <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbHighreplication,teastorePrivateNlbLowreplication ,teastorePrivateNlbNoreplication,teastorePrivateNlbOnlyuireplication,teastorePrivateNlbOnlyupstreamreplication))
hypothesis1b <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleHighreplication,teastorePrivateNlbRdsSingleLowreplication,teastorePrivateNlbRdsSingleNoreplication))
hypothesis1c <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsTwoHighreplication,teastorePrivateNlbRdsTwoLowreplication,teastorePrivateNlbRdsTwoNoreplication))
hypothesis1d <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsThreeHighreplication,teastorePrivateNlbRdsThreeLowreplication,teastorePrivateNlbRdsThreeNoreplication))
hypothesis1e <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbHighreplicationNocaching,teastorePrivateNlbLowreplicationNocaching,teastorePrivateNlbNoreplicationNocaching))
hypothesis1f <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbHighreplicationWithcaching,teastorePrivateNlbLowreplicationWithcaching,teastorePrivateNlbNoreplicationWithcaching))

investigateServiceReplicationAvailability <- function(data) {
  result <- data %>%
    select(-c("mean(elapsed) [ms]","median(elapsed) [ms]","90th_percentile(elapsed) [ms]","median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]","throughput [requests / s]", "storageReplicationLevel", "ratioOfCachedDataAggregates","dataReplicationAlongRequestTrace" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(serviceReplicationLevel, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis2a <- investigateServiceReplicationAvailability(rbind(teastorePrivateNlbWithfailuresHighreplication,teastorePrivateNlbWithfailuresLowreplication,teastorePrivateNlbWithfailuresNoreplication,teastorePrivateNlbWithfailuresOnlyuireplication,teastorePrivateNlbWithfailuresOnlyupstreamreplication))
hypothesis2b <- investigateServiceReplicationAvailability(rbind(teastorePrivateNlbWithfailuresHighreplicationNocaching,teastorePrivateNlbWithfailuresLowreplicationNocaching,teastorePrivateNlbWithfailuresNocaching))
hypothesis2c <- investigateServiceReplicationAvailability(rbind(teastorePrivateNlbWithfailuresHighreplicationWithcaching,teastorePrivateNlbWithfailuresLowreplicationWithcaching,teastorePrivateNlbWithfailuresWithcaching))

investigateHorizontalReplicationTimeBehaviour <- function(data) {
  result <- data %>%
    select(-c("success_rate [%]", "median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "ratioOfCachedDataAggregates","dataReplicationAlongRequestTrace" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(storageReplicationLevel, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis3a <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleNoreplication,teastorePrivateNlbRdsTwoNoreplication,teastorePrivateNlbRdsThreeNoreplication))
hypothesis3b <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleLowreplication,teastorePrivateNlbRdsTwoLowreplication,teastorePrivateNlbRdsThreeLowreplication))
hypothesis3c <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleHighreplication,teastorePrivateNlbRdsTwoHighreplication,teastorePrivateNlbRdsThreeHighreplication))
hypothesis3d <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleNocaching,teastorePrivateNlbRdsTwoNocaching,teastorePrivateNlbRdsThreeNocaching))
hypothesis3e <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleWithcaching,teastorePrivateNlbRdsTwoWithcaching,teastorePrivateNlbRdsThreeWithcaching))
hypothesis3f <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleWithsomecaching,teastorePrivateNlbRdsTwoWithsomecaching,teastorePrivateNlbRdsThreeWithsomecaching))

investigateVerticalReplicationTimeBehaviour <- function(data) {
  result <- data %>%
    select(-c("success_rate [%]", "median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel")) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(ratioOfCachedDataAggregates,dataReplicationAlongRequestTrace,, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis4a <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbNoreplicationWithcaching,teastorePrivateNlbNoreplicationNocaching,teastorePrivateNlbNoreplicationWithsomecaching))
hypothesis4b <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleNocaching,teastorePrivateNlbRdsSingleWithsomecaching,teastorePrivateNlbRdsSingleWithcaching))
hypothesis4c <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsTwoNocaching,teastorePrivateNlbRdsTwoWithsomecaching,teastorePrivateNlbRdsTwoWithcaching))
hypothesis4d <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsThreeNocaching,teastorePrivateNlbRdsThreeWithsomecaching,teastorePrivateNlbRdsThreeWithcaching))
hypothesis4e <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbLowreplicationNocaching,teastorePrivateNlbLowreplicationWithcaching,teastorePrivateNlbLowreplication))
hypothesis4f <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbHighreplicationNocaching,teastorePrivateNlbHighreplicationWithcaching,teastorePrivateNlbHighreplication))

investigateVerticalReplicationAvailability <- function(data) {
  result <- data %>%
    select(-c("mean(elapsed) [ms]","median(elapsed) [ms]","90th_percentile(elapsed) [ms]","median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]","throughput [requests / s]", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(ratioOfCachedDataAggregates,dataReplicationAlongRequestTrace,, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis5a <- investigateVerticalReplicationAvailability(rbind(teastorePrivateNlbWithfailuresWithcaching,teastorePrivateNlbWithfailuresNocaching,teastorePrivateNlbWithfailuresWithsomecaching,teastorePrivateNlbWithfailuresNoreplication))
hypothesis5b <- investigateVerticalReplicationAvailability(rbind(teastorePrivateNlbWithfailuresLowreplicationNocaching,teastorePrivateNlbWithfailuresLowreplicationWithcaching,teastorePrivateNlbWithfailuresLowreplication))
hypothesis5c <- investigateVerticalReplicationAvailability(rbind(teastorePrivateNlbWithfailuresHighreplicationNocaching,teastorePrivateNlbWithfailuresHighreplicationWithcaching,teastorePrivateNlbWithfailuresHighreplication))


# ----------------------

paperColumnNames <- c("Entity Type", "Name", "Variation", "mean rt [ms]", "median rt [ms]", "90p rt [ms]", "throughput [req/s]", "Service Replication Level", "Median Service Replication", "Smallest Replication Value")
paperColumnAlignment <- "lllccccccc"
paperTableStyling1<- function(kTable) {
  kTable %>%
    kable_styling(font_size = 11, bootstrap_options = "bordered", latex_options=c("scale_down", "HOLD_position")) %>%
    row_spec(0,bold=TRUE, hline_after=TRUE) %>% 
    column_spec(c(1:10), width = "5em", latex_valign = "m") %>%
    column_spec(3, width = "12em", latex_valign = "m") %>%
    column_spec(c(4:6), width= "3em", latex_valign = "m") %>%
    #column_spec(c(5:11), width= "5em", latex_valign = "m") %>%
    collapse_rows(columns=c(1,2,3))
}

paperTableStyling1(kable(hypothesis1a %>%
                        filter(!str_detect(label, "Add Product 2 to Cart")) %>%
                        filter(!(entityName %in% c("Index Page", "Product Page", "Show Category", "User Login", "User Logout"))) %>%
                        select(-c(label))
                        ,
                       label = NA,
                       caption = "Hypothesis 1: A higher service replication has a positive impact on time-behavior for the original TeaStore application",
                       col.names = paperColumnNames,
                       longtable = FALSE,
                       align = paperColumnAlignment,
                       format="latex", 
                       escape = FALSE)
)  %>% save_kable("./table1aForPaper.tex",float = FALSE)

paperTableStyling1(kable(hypothesis1f %>%
                          filter(entityType == "system") %>%
                          select(-c(label))
                        ,
                        label = NA,
                        caption = "Hypothesis 1: A higher service replication has a positive impact on time-behavior for the TeaStore application while using extensive caching.",
                        col.names = paperColumnNames,
                        longtable = FALSE,
                        align = paperColumnAlignment,
                        format="latex", 
                        escape = FALSE)
)  %>% save_kable("./table1fForPaper.tex",float = FALSE)


paperColumnNames <- c("Entity Type", "Name", "Variation", "success rate [\\%]", "Service Replication Level", "Median Service Replication", "Smallest Replication Value")
paperColumnAlignment <- "lllcccc"
paperTableStyling2<- function(kTable) {
  kTable %>%
    kable_styling(font_size = 11, bootstrap_options = "bordered", latex_options=c("scale_down", "HOLD_position")) %>%
    row_spec(0,bold=TRUE, hline_after=TRUE) %>% 
    column_spec(c(1:2), width = "3em", latex_valign = "m") %>%
    column_spec(3, width = "15em", latex_valign = "m") %>%
    column_spec(c(5:8), width= "8em", latex_valign = "m") %>%
    #column_spec(c(5:11), width= "5em", latex_valign = "m") %>%
    collapse_rows(columns=c(1,2,3))
}

paperTableStyling2(kable(hypothesis2a %>%
                           filter(entityType == "system") %>%
                           select(-c(label))
                         ,
                         label = NA,
                         caption = "Hypothesis 2: A higher service replication has a positive impact on availability for the original TeaStore application",
                         col.names = paperColumnNames,
                         longtable = FALSE,
                         align = paperColumnAlignment,
                         format="latex", 
                         escape = FALSE)
)  %>% save_kable("./table2aForPaper.tex",float = FALSE)


paperColumnNames <- c("Entity Type", "Name", "Variation", "mean rt [ms]", "median rt [ms]", "90p rt [ms]", "throughput [req/s]", "Storage Replication Level")
paperColumnAlignment <- "lllccccc"
paperTableStyling3<- function(kTable) {
  kTable %>%
    kable_styling(font_size = 11, bootstrap_options = "bordered", latex_options=c("scale_down", "HOLD_position")) %>%
    row_spec(0,bold=TRUE, hline_after=TRUE) %>% 
    column_spec(c(1:8), width = "5em", latex_valign = "m") %>%
    column_spec(3, width = "10em", latex_valign = "m") %>%
    column_spec(c(4:6), width= "3em", latex_valign = "m") %>%
    #column_spec(c(5:11), width= "5em", latex_valign = "m") %>%
    collapse_rows(columns=c(1,2,3))
}

paperTableStyling3(kable(hypothesis3a %>%
                           filter(!str_detect(label, "List Products with different page")) %>%
                           filter(!(entityName %in% c("Index Page", "Product Page", "Add Product To Cart", "User Login", "User Logout"))) %>%
                           select(-c(label))
                         ,
                         label = NA,
                         caption = "Hypothesis 3: A higher horizontal data replication has a positive impact on time-behavior for the TeaStore application while there is no service replication.",
                         col.names = paperColumnNames,
                         longtable = FALSE,
                         align = paperColumnAlignment,
                         format="latex", 
                         escape = FALSE)
)  %>% save_kable("./table3aForPaper.tex",float = FALSE)


paperColumnNames <- c("Entity Type", "Name", "Variation", "mean rt [ms]", "median rt [ms]", "90p rt [ms]", "throughput [req/s]", "Ratio of Cached Data Aggregates", "Data Replication along Request Trace")
paperColumnAlignment <- "lllcccccc"
paperTableStyling4<- function(kTable) {
  kTable %>%
    kable_styling(font_size = 11, bootstrap_options = "bordered", latex_options=c("scale_down", "HOLD_position")) %>%
    row_spec(0,bold=TRUE, hline_after=TRUE) %>% 
    column_spec(c(1:9), width = "5em", latex_valign = "m") %>%
    column_spec(3, width = "13em", latex_valign = "m") %>%
    column_spec(c(4:6), width= "3em", latex_valign = "m") %>%
    column_spec(8, width= "6em", latex_valign = "m") %>%
    column_spec(9, width= "8em", latex_valign = "m") %>%
    #column_spec(c(5:11), width= "5em", latex_valign = "m") %>%
    collapse_rows(columns=c(1,2,3))
}

paperTableStyling4(kable(hypothesis4a %>%
                           filter(!str_detect(label, "Add Product 2 to Cart")) %>%
                           filter(!(entityName %in% c("Index Page", "Product Page", "Show Category", "User Login", "User Logout"))) %>%
                           select(-c(label))
                         ,
                         label = NA,
                         caption = "Hypothesis 4: A higher vertical data replication has a positive impact on time-behavior for the TeaStore application while no service replication is used.",
                         col.names = paperColumnNames,
                         longtable = FALSE,
                         align = paperColumnAlignment,
                         format="latex", 
                         escape = FALSE)
)  %>% save_kable("./table4aForPaper.tex",float = FALSE)



paperColumnNames <- c("Entity Type", "Name", "Variation", "success rate [\\%]", "Ratio of Cached Data Aggregates", "Data Replication along Request Trace")
paperColumnAlignment <- "lllccc"
paperTableStyling5<- function(kTable) {
  kTable %>%
    kable_styling(font_size = 11, bootstrap_options = "bordered", latex_options=c("scale_down", "HOLD_position")) %>%
    row_spec(0,bold=TRUE, hline_after=TRUE) %>% 
    column_spec(c(1:2), width = "6em", latex_valign = "m") %>%
    column_spec(3, width = "12em", latex_valign = "m") %>%
    column_spec(c(4:5), width= "8em", latex_valign = "m") %>%
    column_spec(6, width= "10em", latex_valign = "m") %>%
    #column_spec(c(5:11), width= "5em", latex_valign = "m") %>%
    collapse_rows(columns=c(1,2,3))
}

paperTableStyling5(kable(hypothesis5a %>%
                           filter(!str_detect(label, "Add Product 2 to Cart")) %>%
                           filter(!(entityName %in% c("Index Page", "Product Page", "Show Category", "User Login", "User Logout"))) %>%
                           select(-c(label))
                         ,
                         label = NA,
                         caption = "Hypothesis 5: A higher vertical data replication has a positive impact on availability for the TeaStore application while there is no service replication.",
                         col.names = paperColumnNames,
                         longtable = FALSE,
                         align = paperColumnAlignment,
                         format="latex", 
                         escape = FALSE)
)  %>% save_kable("./table5aForPaper.tex",float = FALSE)

paperTableStyling5(kable(hypothesis5c %>%
                           filter(entityType == "system") %>%
                           select(-c(label))
                         ,
                         label = NA,
                         caption = "Hypothesis 5: A higher vertical data replication has a positive impact on availability for the TeaStore application while there is no service replication.",
                         col.names = paperColumnNames,
                         longtable = FALSE,
                         align = paperColumnAlignment,
                         format="latex", 
                         escape = FALSE)
)  %>% save_kable("./table5cForPaper.tex",float = FALSE)


