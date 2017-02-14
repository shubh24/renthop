library(jsonlite)
library(data.table)
library(lubridate)
library(caTools)
library(caret)
library(e1071)

ny_lat <- 40.785091
ny_lon <- -73.968285

library(h2o)
h2o.init()

df = fromJSON("train.json")

rf_h2o = function(t1, t2){
  
  write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
  write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)
  
  feature_names = names(t1)
  feature_names = feature_names[! feature_names %in% c("created", "listing_id", "interest_level", "latitude", "longitude", "hour")]
  
  train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
  test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
  rf = h2o.randomForest(x = feature_names, y = "interest_level", training_frame = train_h2o, ntree = 50) 
  print(as.data.frame(h2o.varimp(rf))$variable)
  res = as.data.frame(predict(rf, test_h2o))
  
  return(res)
}

t1 <- data.table(bathrooms=unlist(df$bathrooms)
                 ,bedrooms=unlist(df$bedrooms)
                 ,building_id=as.factor(unlist(df$building_id))
                 ,created=as.POSIXct(unlist(df$created))
                 ,n_photos = as.numeric(sapply(df$photos, length))
                 ,n_description = log(as.numeric(sapply(df$description, nchar)))
                 # ,description=unlist(df$description) # parse errors
                 # ,display_address=unlist(df$display_address) # parse errors
                 ,latitude=unlist(df$latitude)
                 ,longitude=unlist(df$longitude)
                 ,distance_to_city=mapply(function(lon,lat) sqrt((lon - ny_lon)^2 + (lat - ny_lat)^2),
                                          df$longitude,
                                          df$latitude)
                 ,listing_id=unlist(df$listing_id)
                 ,manager_id=as.factor(unlist(df$manager_id))
                 ,price=unlist(df$price),
                 days_since = as.numeric(difftime(Sys.Date(), unlist(df$created)))
                 ,interest_level=as.factor(unlist(df$interest_level))
                 ,street_adress=as.factor(unlist(df$street_address)) # parse errors
)

t1[,":="(yday=yday(created)
         ,month=lubridate::month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         ,hour=lubridate::hour(created))]

frq_features = table(unlist(df$features))
top_features = names(frq_features[frq_features>1000]) 

t1 = cbind(t1, t(sapply(df$features, function(x){as.numeric(top_features %in% x)})))
for (i in c(1:25)){
  t1[[paste0("V", i)]] = as.factor(t1[[paste0("V", i)]])
}
t1$bathrooms = as.factor(t1$bathrooms)
t1$bedrooms = as.factor(t1$bedrooms)

set.seed(101) 

sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
t1_train <- t1[sample, ]
t1_test <- t1[-sample, ]

res_val = rf_h2o(t1_train, t1_test)
confusionMatrix(table(res_val$predict, t1_test$interest_level))

test = fromJSON("test.json")

t2 <- data.table(bathrooms=unlist(test$bathrooms)
                 ,bedrooms=unlist(test$bedrooms)
                 ,building_id=as.factor(unlist(test$building_id))
                 ,created=as.POSIXct(unlist(test$created))
                 ,n_photos = as.numeric(sapply(test$photos, length))
                 ,n_description = as.numeric(sapply(test$description, nchar))
                 # ,description=unlist(test$description) # parse errors
                 # ,display_address=unlist(test$display_address) # parse errors
                 ,latitude=unlist(test$latitude)
                 ,longitude=unlist(test$longitude)
                 ,distance_to_city=mapply(function(lon,lat) sqrt((lon - ny_lon)^2 + (lat - ny_lat)^2),
                                          test$longitude,
                                          test$latitude)
                 ,listing_id=unlist(test$listing_id)
                 ,manager_id=as.factor(unlist(test$manager_id))
                 ,price=unlist(test$price)
                 #,interest_level=as.factor(unlist(test$interest_level))
                 # ,street_adress=unlist(test$street_address) # parse errors
)

t2[,":="(yday=yday(created)
         ,month=lubridate::month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         ,hour=lubridate::hour(created))]

t2 = cbind(t2, t(sapply(df$features, function(x){as.numeric(top_features %in% x)})))
t2$bathrooms = as.factor(t2$bathrooms)
t2$bedrooms = as.factor(t2$bedrooms)

res = rf_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "rf_h2o_1.csv", row.names = FALSE)

