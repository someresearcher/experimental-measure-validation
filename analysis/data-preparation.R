library(stringr)
library(anytime)
library(dplyr)
library(tidyr)
library(readr)


loadData <- function(path, timestamp) {
  parentScope = parent.frame()
  parentScope$rawData <- read.csv2(paste(path,"/",timestamp,"_browse_run.csv", sep=""), sep=",")
  parentScope$homeOnly <-  read.csv2(paste(path,"/",timestamp,"_home_run.csv", sep=""), sep=",")
  parentScope$loginOnly <-  read.csv2(paste(path,"/",timestamp,"_login_run.csv", sep=""), sep=",")
  parentScope$listProductsOnly <-  read.csv2(paste(path,"/",timestamp,"_listProducts_run.csv", sep=""), sep=",")
  parentScope$lookAtProductOnly <-  read.csv2(paste(path,"/",timestamp,"_lookAtProduct_run.csv", sep=""), sep=",")
  parentScope$addProductToCartOnly <-  read.csv2(paste(path,"/",timestamp,"_addProductToCart_run.csv", sep=""), sep=",")
  parentScope$logoutOnly <-  read.csv2(paste(path,"/",timestamp,"_logout_run.csv", sep=""), sep=",")
}

prepare <- function(rawData) {
  rawData %>% filter(!str_detect(label, "-[:digit:]*$")) %>% 
    mutate(timestamp_iso = anytime(timeStamp/1000)) 
}

analyze <- function(data) {
  data %>%
    summarise("mean(elapsed) [ms]" = mean(elapsed, na.rm = T), 
              "median(elapsed) [ms]" = median(elapsed, na.rm = T), 
              "90th_percentile(elapsed) [ms]" = quantile(elapsed, probs=0.9, na.rm=T),
              "mean(latency) [ms]" = mean(Latency, na.rm = T), 
              "median(latency) [ms]" = median(Latency, na.rm = T), 
              "90th_percentile(latency) [ms]" = quantile(Latency, probs=0.9, na.rm=T),
              "mean(receivedBytes) [byte]" = mean(bytes, na.rm = T), 
              "median(receivedBytes) [byte]" = median(bytes, na.rm = T), 
              "90th_percentile(receivedBytes) [byte]" = quantile(bytes, probs=0.9, na.rm=T),
              "mean(sentBytes) [byte]" = mean(sentBytes, na.rm = T), 
              "median(sentBytes) [byte]" = median(sentBytes, na.rm = T), 
              "90th_percentile(sentBytes) [byte]" = quantile(sentBytes, probs=0.9, na.rm=T),
              "success_rate [%]" = sum(success == "true") / n(),
              "throughput [requests / s]" = n() / (sum(elapsed, na.rm = T) / 1000),
    )
}

loadAndAnalyze <- function(path, timestamp) {
  
  loadData(path, timestamp)
  
  overall <- rawData %>% prepare %>% analyze %>% mutate(label = "Overall", entityType="system")
  
  perRequestTrace <- rawData %>%
    prepare %>%
    group_by(label) %>%
    analyze %>%
    mutate(entityType = "requestTrace")
  
  homeOnlyResults <- homeOnly %>%
    prepare %>%
    analyze %>%
    mutate(label = "Home - discrete", entityType = "requestTrace")
  
  loginOnlyResults <- loginOnly %>%
    prepare %>%
    analyze %>%
    mutate(label = "Login - discrete", entityType = "requestTrace")
  
  listProductsOnlyResults <- listProductsOnly %>%
    prepare %>%
    analyze %>%
    mutate(label = "List Products - discrete", entityType = "requestTrace")
  
  lookAtProductOnlyResults <- lookAtProductOnly %>%
    prepare %>%
    analyze %>%
    mutate(label = "Look at Product - discrete", entityType = "requestTrace")
  
  addProductToCartOnlyResults <- addProductToCartOnly %>%
    prepare %>%
    analyze %>%
    mutate(label = "Add Product to Cart - discrete", entityType = "requestTrace")
  
  logoutOnlyResults <- logoutOnly %>%
    prepare %>%
    analyze %>%
    mutate(label = "Logout - discrete", entityType = "requestTrace")
  
  result <- rbind(overall, perRequestTrace, homeOnlyResults, loginOnlyResults, listProductsOnlyResults, lookAtProductOnlyResults, addProductToCartOnlyResults, logoutOnlyResults)
  
  result$architectureVariation <- tail(str_split_1(path, "/"), n=1)
  
  return(result)
}

resultsPath = "../../experiment-results"
getDataPath <- function(subPath) {
  return(paste(resultsPath, subPath ,sep=""))
}


outputPath = "./results"
aggregateData <- function(subPath, timestamp) {
  resultSet <- loadAndAnalyze(getDataPath(subPath), timestamp)
  
  dirPath <- paste(outputPath, paste(head(str_split_1(subPath, "/"), n=-1), collapse="/"), sep="")
  ifelse(!dir.exists(file.path(dirPath)), dir.create(file.path(dirPath), recursive = TRUE), FALSE)
  
  write.csv(resultSet, paste(outputPath, subPath, ".csv", sep=""), row.names = FALSE)
  return(resultSet)
}

teastorePrivateNlbOriginal <- aggregateData("/original/teastore-private-nlb-original", "20241107_113408")

teastorePrivateNlbHighreplication <- aggregateData("/serviceReplication/teastore-private-nlb-highreplication","20241107_221109")
teastorePrivateNlbLowreplication <- aggregateData("/serviceReplication/teastore-private-nlb-lowreplication", "20241107_141210")
teastorePrivateNlbNoreplication <- aggregateData("/serviceReplication/teastore-private-nlb-noreplication", "20241107_124908")
teastorePrivateNlbOnlyuireplication  <- aggregateData("/serviceReplication/teastore-private-nlb-onlyuireplication", "20241107_215531")
teastorePrivateNlbOnlyupstreamreplication <- aggregateData("/serviceReplication/teastore-private-nlb-onlyupstreamreplication", "20241107_214106")
teastorePrivateNlbWithfailuresHighreplication <- aggregateData("/serviceReplication/teastore-private-nlb-withfailures-highreplication", "20241107_225411")
teastorePrivateNlbWithfailuresLowreplication <- aggregateData("/serviceReplication/teastore-private-nlb-withfailures-lowreplication", "20241107_224021")
teastorePrivateNlbWithfailuresNoreplication <- aggregateData("/serviceReplication/teastore-private-nlb-withfailures-noreplication", "20241107_222743")
teastorePrivateNlbWithfailuresOnlyuireplication <- aggregateData("/serviceReplication/teastore-private-nlb-withfailures-onlyuireplication", "20241107_230841")
teastorePrivateNlbWithfailuresOnlyupstreamreplication <- aggregateData("/serviceReplication/teastore-private-nlb-withfailures-onlyupstreamreplication", "20241107_233558")

teastorePrivateNlbNoreplicationWithcaching <- aggregateData("/verticalReplication/teastore-private-nlb-noreplication-withcaching", "20241108_011740")
teastorePrivateNlbWithfailuresWithcaching <- aggregateData("/verticalReplication/teastore-private-nlb-withfailures-withcaching", "20241108_095831")
teastorePrivateNlbNoreplicationNocaching  <- aggregateData("/verticalReplication/teastore-private-nlb-noreplication-nocaching", "20241107_235241") 
teastorePrivateNlbNoreplicationWithsomecaching  <- aggregateData("/verticalReplication/teastore-private-nlb-noreplication-withsomecaching", "20241108_090816")
teastorePrivateNlbWithfailuresNocaching    <- aggregateData("/verticalReplication/teastore-private-nlb-withfailures-nocaching", "20241108_092438")
teastorePrivateNlbWithfailuresWithsomecaching  <- aggregateData("/verticalReplication/teastore-private-nlb-withfailures-withsomecaching", "20241108_101240")

teastorePrivateNlbRdsSingleHighreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-single-highreplication", "20241108_173532")
teastorePrivateNlbRdsSingleLowreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-single-lowreplication", "20241108_160732")
teastorePrivateNlbRdsSingleNocaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-single-nocaching", "20241108_175626")
teastorePrivateNlbRdsSingleNoreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-single-noreplication", "20241108_154901")
teastorePrivateNlbRdsSingleWithcaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-single-withcaching", "20241108_183052")
teastorePrivateNlbRdsSingleWithsomecaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-single-withsomecaching", "20241108_184627")
teastorePrivateNlbRdsThreeHighreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-three-highreplication", "20241108_211129")
teastorePrivateNlbRdsThreeLowreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-three-lowreplication", "20241108_212600")
teastorePrivateNlbRdsThreeNocaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-three-nocaching", "20241108_214015")
teastorePrivateNlbRdsThreeNoreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-three-noreplication", "20241108_215625")
teastorePrivateNlbRdsThreeWithcaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-three-withcaching", "20241108_221053")
teastorePrivateNlbRdsThreeWithsomecaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-three-withsomecaching", "20241108_222628")
teastorePrivateNlbRdsTwoHighreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-two-highreplication", "20241108_192348")
teastorePrivateNlbRdsTwoLowreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-two-lowreplication", "20241108_194148")
teastorePrivateNlbRdsTwoNocaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-two-nocaching", "20241108_200659")
teastorePrivateNlbRdsTwoNoreplication <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-two-noreplication", "20241108_202204")
teastorePrivateNlbRdsTwoWithcaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-two-withcaching", "20241108_203725")
teastorePrivateNlbRdsTwoWithsomecaching <- aggregateData("/horizontalDataReplication/teastore-private-nlb-rds-two-withsomecaching", "20241108_204847")

teastorePrivateNlbHighreplicationNocaching <- aggregateData("/combined/teastore-private-nlb-highreplication-nocaching", "20241108_104309")
teastorePrivateNlbHighreplicationWithcaching <- aggregateData("/combined/teastore-private-nlb-highreplication-withcaching", "20241108_110500")
teastorePrivateNlbLowreplicationNocaching <- aggregateData("/combined/teastore-private-nlb-lowreplication-nocaching", "20241108_111904")
teastorePrivateNlbLowreplicationWithcaching <- aggregateData("/combined/teastore-private-nlb-lowreplication-withcaching", "20241108_120008")
teastorePrivateNlbWithfailuresHighreplicationNocaching <- aggregateData("/combined/teastore-private-nlb-withfailures-highreplication-nocaching", "20241108_124056")
teastorePrivateNlbWithfailuresHighreplicationWithcaching <- aggregateData("/combined/teastore-private-nlb-withfailures-highreplication-withcaching", "20241108_133119")
teastorePrivateNlbWithfailuresLowreplicationNocaching <- aggregateData("/combined/teastore-private-nlb-withfailures-lowreplication-nocaching", "20241108_135336")
teastorePrivateNlbWithfailuresLowreplicationWithcaching <- aggregateData("/combined/teastore-private-nlb-withfailures-lowreplication-withcaching", "20241108_142347")

