library(jsonlite)
library(data.table)
library(lubridate)
library(caTools)
library(caret)
library(e1071)
library(magrittr)
library(knitr)
library(stringr)
require(dplyr)
library(MLmetrics)
library(plyr)
library(xgboost)

ny_lat <- 40.785091
ny_lon <- -73.968285

library(h2o)
h2o.init()

df = fromJSON("train.json")
test = fromJSON("test.json")

# cv = read.csv("count_vec.csv")
# cv[cv > 1] = 1
# for (i in colnames(cv)){cv[[i]] = as.factor(cv[[i]])}
# cv_train <- cv[1:49352, ]
# cv_test <- cv[49353:124011, ]

nbd_train = read.csv("neighborhood_train.csv", stringsAsFactors = TRUE)
nbd_test = read.csv("neighborhood_test.csv", stringsAsFactors = TRUE)

feature = as.data.frame(table(tolower(unlist(df$features))))
feature$Var1 = as.character(feature$Var1)

keywords = c("24", "court", "wood", "roof", "outdoor", "garden", "park", "bath", "actual", "allowed", "air", "doorman", "balcony", "available", "pool", "gym", "wifi", "fan", "playroom", "subway", "concierge", "fire", "fitness", "dish", "garage", "granite", "high", "laundry", "live", "fee", "war", "private", "lounge", "short", "spacious", "stainless", "storage", "terrace", "valet", "washer", "yoga")

for (j in 1:length(keywords)){
  key = keywords[j]
  wood = feature %>% filter(str_detect(feature$Var1, key))
  feature = rbind(feature, c(key, sum(wood$Freq)))
  
  for(i in 1:nrow(wood)){
    feature = feature[!(feature$Var1 == wood$Var1[i]),]
  }
  feature$Freq = as.numeric(feature$Freq)
}

feature = feature$Var1[feature$Freq > 50]

# # Laundry in unit INCLUDE THIS!
# top_features %>%
#   filter(str_detect(top_features, paste(c('laundry', 'dryer', 'washer'), collapse="|"))) %>%
#   filter(!str_detect(top_features, "dishwasher")) %>%
#   kable(caption = "Laundry in unit")


rf_h2o = function(t1, t2){
  
    write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
    write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)
    
    feature_names = names(t1)
    feature_names = feature_names[! feature_names %in% c("created", "listing_id", "interest_level")]
    
    train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
    test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
    rf = h2o.randomForest(x = feature_names, y = "interest_level", training_frame = train_h2o, ntree = 200)
    print(as.data.frame(h2o.varimp(rf))$variable)
    res = as.data.frame(predict(rf, test_h2o))

    return(res)
}

gbm_h2o = function(t1, t2){
  
  write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
  write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)
  
  feature_names = names(t1)
  feature_names = feature_names[! feature_names %in% c("created", "listing_id", "interest_level")]
  
  train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
  test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
  
  gbm1 <- h2o.gbm(x = feature_names
                ,y = "interest_level"
                ,training_frame = train_h2o
                ,distribution = "multinomial"
                ,model_id = "gbm1"
                #,nfolds = 5
                ,ntrees = 10000
                ,learn_rate = 0.01
                ,max_depth = 5
                ,min_rows = 20
                ,sample_rate = 0.8
                ,score_tree_interval = 10
                ,col_sample_rate = 0.7
                ,stopping_rounds = 5
                ,stopping_metric = "logloss"
                ,stopping_tolerance = 1e-4
                ,seed=321)
  
  print(as.data.frame(h2o.varimp(gbm1))$variable)
  res = as.data.frame(predict(gbm1, test_h2o))
  
  return(res)
}

generate_df = function(df, train_flag){
    t1 <- data.table(bathrooms=unlist(df$bathrooms)
                     ,bedrooms=unlist(df$bedrooms)
                     ,building_id=as.factor(unlist(df$building_id))
                     ,created=as.POSIXct(unlist(df$created))
                     ,n_photos = as.numeric(sapply(df$photos, length))
                     ,n_description = log(as.numeric(sapply(df$description, nchar)))
                     ,n_features = as.numeric(sapply(df$features, length))
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
      # t1 = cbind(t1, cv_train)
      t1 = merge(t1, nbd_train, by = "listing_id")
    }
    else{
      # t1 = cbind(t1, cv_test)
      t1 = merge(t1, nbd_test, by = "listing_id")
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
    t1$mday = as.factor(t1$mday)
    t1$hour = as.factor(t1$hour)
    t1$yday = as.factor(t1$yday)
    
    t1$street_type = as.character(sapply(t1$display_address, function(x){substring(tolower(tail(strsplit(x, " ")[[1]], n = 1)), 1, 2)}))
    street_type = as.data.frame(table(as.factor(t1$street_type)))
    top_streets = street_type$Var1[street_type$Freq > 200]
    t1$street_type = as.factor(ifelse(t1$street_type %in% top_streets, yes = as.character(t1$street_type), no = "other"))
    t1$display_address = NULL
    
    t1$zero_building_id = as.factor(t1$building_id == 0)
    t1$zero_description = as.factor(t1$n_description == 0)
    t1$zero_photos = as.factor(t1$n_photos == 0)
    
    # buildings = as.data.frame(table(t1$building_id))
    # buildings = buildings[-(buildings$Var1 == 0),]
    # 
    # t1$top10buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 10)$Var1, yes = 1, no = 0))
    # t1$top20buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 20)$Var1, yes = 1, no = 0))
    # t1$top50buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 50)$Var1, yes = 1, no = 0))
    # t1$top100buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 100)$Var1, yes = 1, no = 0))
    # t1$building_id = NULL
    # 
    # managers = as.data.frame(table(t1$manager_id))
    # 
    # t1$top10managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 10)$Var1, yes = 1, no = 0))
    # t1$top20managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 20)$Var1, yes = 1, no = 0))
    # t1$top50managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 50)$Var1, yes = 1, no = 0))
    # t1$top100managers = as.factor(ifelse(as.character(t1$manager_id) %in% head(arrange(managers, desc(Freq)), n = 100)$Var1, yes = 1, no = 0))
    # t1$manager_id = NULL
    
    t1 = cbind(t1, t(sapply(df$features, function(x){as.numeric(str_detect(tolower(x), feature))})))

    for (i in c(1:length(feature))){ #can this be factored in above?
      t1[[paste0("V", str(i))]] = as.factor(t1[[paste0("V", str(i))]])
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
  print(MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(res_val[,c("high", "low", "medium")])))
 
  return (res_val[, c("high", "low", "medium")]) 
}

validate_gbm = function(t1){
  set.seed(101) 
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  res_val = gbm_h2o(t1_train, t1_test)
  print(confusionMatrix(table(res_val$predict, t1_test$interest_level)))
  print(MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(res_val[,c("high", "low", "medium")])))
  
  return (res_val[, c("high", "low", "medium")]) 
  
}

validate_xgb = function(train_xgb, train_y){
  set.seed(101) 
  
  sample <- sample.int(nrow(train_xgb), floor(.75*nrow(train_xgb)), replace = F)
  train_xgb_train <- train_xgb[sample, ]
  train_xgb_val <- train_xgb[-sample, ]
  
  train_y_train = train_y[1:nrow(train_xgb_train)]
  
  train_y_val = train_y[37015:length(train_y)]
  train_y_val[train_y_val == 0] = "low"
  train_y_val[train_y_val == 1] = "medium"
  train_y_val[train_y_val == 2] = "high"
  train_y_val = as.factor(train_y_val)
  
  pred_df_val = run_xgb(train_xgb_train, train_y_train, train_xgb_val)
  print(MultiLogLoss(y_true = train_y_val, y_pred = as.matrix(pred_df_val[,c("high", "low", "medium")])))
  
  return(pred_df_val[, c("high", "low", "medium")])
}

validate_ensemble = function(t1, train_xgb, train_y){
  
  rf_res = validate(t1)
  xgb_res = validate_xgb(train_xgb, train_y)
  
  set.seed(101) 
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_test <- t1[-sample, ]
  
  ensemble_res = (rf_res + xgb_res)/2
  MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(ensemble_res))
  
}
xgb = function(t1, train_flag){

  train_x = t1
  
  # train_x$yday = NULL
  # train_x$latitude = NULL
  # train_x$longitude = NULL
  
  if (train_flag == 1){
    train_x$interest_level = NULL
  }
  
  dmy <- dummyVars(" ~ .", data = train_x)
  t_xgb <- data.frame(predict(dmy, newdata = train_x))
  
  return(t_xgb)
}

get_train_y = function(t1){
  
  train_y = as.character(t1$interest_level)
  
  train_y[train_y == "low"] = 0
  train_y[train_y == "medium"] = 1
  train_y[train_y == "high"] = 2
  
  train_y = as.numeric(train_y)
  
  return(train_y)  
}

run_xgb = function(train_xgb, train_y, test_xgb){
  
  train_xgb$listing_id = NULL
  
  model = xgboost(data = as.matrix(train_xgb), 
                  label = train_y,
                  eta = 0.1,
                  gamma = 1,
                  max_depth = 6, 
                  nround=100, 
                  subsample = 1,
                  colsample_bytree = 0.7,
                  seed = 100,
                  eval_metric = "mlogloss",
                  objective = "multi:softprob",
                  num_class = 3,
                  missing = NaN,
                  silent = 1)
  
  # library(Ckmeans.1d.dp)
  # names <- dimnames(data.matrix(train_xgb[,-1]))[[2]]
  # importance_matrix = xgb.importance(names, model = model)
  # xgb.plot.importance(importance_matrix[1:50,])
  
  pred = predict(model,  as.matrix(test_xgb), missing=NaN)
  
  pred_matrix = matrix(pred, nrow = nrow(test_xgb), byrow = TRUE)
  
  pred_submission = cbind(test_xgb$listing_id, pred_matrix)
  colnames(pred_submission) = c("listing_id", "low", "medium", "high")
  
  pred_df = as.data.frame(pred_submission)
  return(pred_df)
  
}

t1 = generate_df(df, 1)
t2 = generate_df(test, 0)

#Validation (rf)
rf_val = validate(t1)

#Running xgboost
train_xgb = xgb(t1, 1)
test_xgb = xgb(t2, 0)
train_y = get_train_y(t1)
xgb_val = validate_xgb(train_xgb, train_y)
pred_df = run_xgb(train_xgb, train_y, test_xgb)
write.csv(pred_df, "xgb_submission.csv", row.names = FALSE)

#Validation (gbm)
gbm_val = validate_gbm(t1)
pred_df_gbm = gbm_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(pred_df_gbm$high), medium = as.vector(pred_df_gbm$medium), low = as.vector(pred_df_gbm$low))
write.csv(pred, "gbm_4.csv", row.names = FALSE)

#Running RF
res = rf_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "rf_h2o_3.csv", row.names = FALSE)