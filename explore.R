library(jsonlite)
library(data.table)

df = fromJSON("train.json")

t1 <- data.table(bathrooms=unlist(df$bathrooms)
                 ,bedrooms=unlist(df$bedrooms)
                 ,building_id=as.factor(unlist(df$building_id))
                 ,created=as.POSIXct(unlist(df$created))
                 ,n_photos = as.numeric(sapply(df$photos, length))
                 ,n_description = as.numeric(sapply(df$description, nchar))
                 # ,description=unlist(df$description) # parse errors
                 # ,display_address=unlist(df$display_address) # parse errors
                 ,latitude=unlist(df$latitude)
                 ,longitude=unlist(df$longitude)
                 ,listing_id=unlist(df$listing_id)
                 ,manager_id=as.factor(unlist(df$manager_id))
                 ,price=unlist(df$price)
                 ,interest_level=as.factor(unlist(df$interest_level))
                 # ,street_adress=unlist(df$street_address) # parse errors
)

t1[,":="(yday=yday(created)
         ,month=month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         ,hour=hour(created))]


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
                 ,listing_id=unlist(test$listing_id)
                 ,manager_id=as.factor(unlist(test$manager_id))
                 ,price=unlist(test$price)
                 #,interest_level=as.factor(unlist(test$interest_level))
                 # ,street_adress=unlist(test$street_address) # parse errors
)

t2[,":="(yday=yday(created)
         ,month=month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         ,hour=hour(created))]

library(h2o)
h2o.init()

write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)

feature_names = names(t1)
feature_names = feature_names[! feature_names %in% c("ID", "created", "latitude", "longitude", "listing_id", "interest_level", "yday", "mday", "hour")]

train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
rf = h2o.randomForest(x = feature_names, y = "interest_level", training_frame = train_h2o, ntree = 50)
res = as.data.frame(predict(rf, test_h2o))
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "submission_rf_h2o.csv", row.names = FALSE)

