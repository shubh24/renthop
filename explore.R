library(jsonlite)
library(data.table)
library(lubridate)
library(caTools)
library(caret)
library(e1071)
library(MLmetrics)
library(plyr)

ny_lat <- 40.785091
ny_lon <- -73.968285

library(h2o)
h2o.init()

df = fromJSON("train.json")
test = fromJSON("test.json")

frq_features = table(unlist(df$features))
top_features = names(frq_features[frq_features>1000]) 

rf_h2o = function(t1, t2){
  
    write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
    write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)
    
    feature_names = names(t1)
    feature_names = feature_names[! feature_names %in% c("created", "listing_id", "interest_level", "latitude", "longitude", "hour", "mday", "yday")]
    
    train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
    test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
    rf = h2o.randomForest(x = feature_names, y = "interest_level", training_frame = train_h2o, ntree = 100) 
    print(as.data.frame(h2o.varimp(rf))$variable)
    res = as.data.frame(predict(rf, test_h2o))
    
    return(res)
}

generate_df = function(df, train_flag){
    t1 <- data.table(bathrooms=unlist(df$bathrooms)
                     ,bedrooms=unlist(df$bedrooms)
                     ,building_id=as.factor(unlist(df$building_id))
                     ,created=as.POSIXct(unlist(df$created))
                     ,n_photos = as.numeric(sapply(df$photos, length))
                     ,n_description = log(as.numeric(sapply(df$description, nchar)))
                     # ,description=unlist(df$description) # parse errors
                     ,display_address=as.character(unlist(df$display_address)) # parse errors
                     ,latitude=unlist(df$latitude)
                     ,longitude=unlist(df$longitude)
                     ,distance_to_city=mapply(function(lon,lat) sqrt((lon - ny_lon)^2 + (lat - ny_lat)^2),
                                              df$longitude,
                                              df$latitude)
                     ,listing_id=unlist(df$listing_id)
                     ,manager_id=as.factor(unlist(df$manager_id))
                     ,price=unlist(df$price)
                     #,days_since = as.numeric(difftime(Sys.Date(), unlist(df$created)))
                     #,interest_level=as.factor(unlist(df$interest_level))
                     # ,street_adress=as.factor(unlist(df$street_address)) # parse errors
    )
    
    if (train_flag == 1){
      t1$interest_level = as.factor(unlist(df$interest_level))
    }
    
    t1[,":="(yday=yday(created)
             ,month=lubridate::month(created)
             ,mday=mday(created)
             ,wday=wday(created)
             ,hour=lubridate::hour(created))]

    t1$price_per_br = t1$price/t1$bedrooms
    
    t1$bathrooms = as.factor(t1$bathrooms)
    t1$bedrooms = as.factor(t1$bedrooms)
    t1$wday = as.factor(t1$wday)
    t1$month = as.factor(t1$month)
    
    t1$street_type = as.character(sapply(t1$display_address, function(x){substring(tolower(tail(strsplit(x, " ")[[1]], n = 1)), 1, 2)}))
    street_type = as.data.frame(table(as.factor(t1$street_type)))
    top_streets = street_type$Var1[street_type$Freq > 200]
    t1$street_type = ifelse(t1$street_type %in% top_streets, yes = as.character(t1$street_type), no = "other")
    t1$display_address = NULL
    
    t1$zero_building_id = as.factor(t1$building_id == 0)
    t1$zero_description = as.factor(t1$n_description == 0)
    t1$zero_photos = as.factor(t1$n_photos == 0)
    
    buildings = as.data.frame(table(t1$building_id))
    buildings = buildings[-(buildings$Var1 == 0),]
    
    t1$top10buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 10)$Var1, yes = 1, no = 0))
    t1$top20buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 20)$Var1, yes = 1, no = 0))
    t1$top50buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 50)$Var1, yes = 1, no = 0))
    t1$top100buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 100)$Var1, yes = 1, no = 0))
    t1$building_id = NULL
    
    managers = as.data.frame(table(t1$manager_id))

    t1$top10managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 10)$Var1, yes = 1, no = 0))
    t1$top20managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 20)$Var1, yes = 1, no = 0))
    t1$top50managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 50)$Var1, yes = 1, no = 0))
    t1$top100managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 100)$Var1, yes = 1, no = 0))
    t1$manager_id = NULL
    
    t1 = cbind(t1, t(sapply(df$features, function(x){as.numeric(top_features %in% x)})))

    for (i in c(1:25)){
      t1[[paste0("V", i)]] = as.factor(t1[[paste0("V", i)]])
    }
    
    return (t1)
}

validate = function(t1){
  set.seed(101) 
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  res_val = rf_h2o(t1_train, t1_test)
  print(confusionMatrix(table(res_val$predict, t1_test$interest_level)))
  MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(res_val[,c("high", "low", "medium")]))
  
}

t1 = generate_df(df, 1)
t2 = generate_df(test, 0)

validate(t1)

res = rf_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "rf_h2o_3.csv", row.names = FALSE)
