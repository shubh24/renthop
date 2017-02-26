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

#features to implement
#sea-facing/landmark coordinates
#sentiment analysis on desc
#adj/nouns usage
#population density!
#ensemble -- if both confident, take higher prob. if different, take higher prob. thus, always higher prob.

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
                ,ntrees = 100
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
                     ,n_words = sapply(strsplit(as.character(df$description), "\\s+"), length)
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
                     ,yday=as.factor(sapply(df$created, yday))
                     ,month=as.factor(sapply(df$created, lubridate::month))
                     ,mday=as.factor(sapply(df$created, mday))
                     ,wday=as.factor(sapply(df$created, wday))
                     ,hour=as.factor(sapply(df$created, lubridate::hour))
                     #,days_since = as.numeric(difftime(Sys.Date(), unlist(df$created)))
                     #,interest_level=as.factor(unlist(df$interest_level))
                     #,street_adress=as.character(unlist(df$street_address)) # parse errors
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
    
    t1$price_per_br = t1$price/t1$bedrooms
    
    # outliers <- t1[t1$longitude == 0 | t1$latitude == 0, ]
    # outliers <- paste(outliers$street_adress, ", new york")
    # 
    # outliers <- data.frame("street_address" = outliers)
    # coords <- sapply(as.character(outliers$street_address), function(x) geocode(x, source = "google")) %>%
    #   t %>%
    #   data.frame %>%
    #   cbind(outliers, .)
    # rownames(coords) <- 1:nrow(coords)
    # 
    # t1 = merge(t1, coords, by ="street_address")
    # 
    # t1$latitude[t1$longitude == 0 | t1$latitude == 0] = coords$lat
    # t1$longitude[t1$longitude == 0 | t1$latitude == 0] = coords$lon
    
    t1$zero_bedroom = as.factor(t1$bedrooms == 0)
    t1$bathrooms_whole = as.factor(as.integer(t1$bathrooms) == t1$bathrooms)
    t1$bed_bath_diff = t1$bedrooms - t1$bathrooms
      
    t1$bathrooms = as.factor(t1$bathrooms)
    t1$bedrooms = as.factor(t1$bedrooms)
    
    t1$street_type = as.character(sapply(t1$display_address, function(x){substring(tolower(tail(strsplit(x, " ")[[1]], n = 1)), 1, 2)}))
    street_type = as.data.frame(table(as.factor(t1$street_type)))
    top_streets = street_type$Var1[street_type$Freq > 200]
    t1$street_type = as.factor(ifelse(t1$street_type %in% top_streets, yes = as.character(t1$street_type), no = "other"))
  
    t1$east = as.factor(str_detect(tolower(t1$display_address), "east"))
    t1$west = as.factor(str_detect(tolower(t1$display_address), "west"))
    t1$north = as.factor(str_detect(tolower(t1$display_address), "north"))
    t1$south = as.factor(str_detect(tolower(t1$display_address), "south"))
    
    t1$display_address = NULL
    
    t1$zero_building_id = as.factor(t1$building_id == 0)
    # t1$zero_description = as.factor(t1$n_description == 0)
    t1$zero_photos = as.factor(t1$n_photos == 0)
    
    return (t1)
}

validate = function(t1){
  set.seed(101) 
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  res_val = rf_h2o(t1_train, t1_test)
  print(MLmetrics::ConfusionMatrix(y_pred = res_val$predict, y_true = t1_test$interest_level))
  print(MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(res_val[,c("high", "low", "medium")])))
 
  return (res_val[, c("high", "low", "medium")]) 
}

validate_gbm = function(t1){
  set.seed(101) 
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  res_val = gbm_h2o(t1_train, t1_test)
  print(MLmetrics::ConfusionMatrix(y_pred = res_val$predict, y_true = t1_test$interest_level))
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

validate_ensemble = function(t1){
  
  rf_res = validate(t1)
  gbm_res = validate_gbm(t1)
  
  #Ensemble code here.

  set.seed(101) 
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_test <- t1[-sample, ]
  
  # ensemble_res = data.frame("high" = mapply(function(x, y){max(x, y)}, rf_res$high, gbm_val$high), "medium" = mapply(function(x, y){max(x, y)}, rf_res$medium, gbm_val$medium),  "low" = mapply(function(x, y){max(x, y)}, rf_res$low, gbm_val$low))
  
  combined_res = cbind(rf_res, gbm_res)
  colnames(combined_res) = c("high_rf", "low_rf", "medium_rf", "high_gbm", "low_gbm", "medium_gbm")
  
  high_prob_rf = as.numeric(as.factor(str_detect(colnames(combined_res)[apply(combined_res, 1, which.max)], "rf")))
  high_prob_gbm = as.numeric(as.factor(str_detect(colnames(combined_res)[apply(combined_res, 1, which.max)], "gbm")))
  
  ensemble_res = data.frame("high" = (rf_res$high)*(high_prob_rf-1) + (gbm_val$high)*(high_prob_gbm-1), 
                            "medium" = (rf_res$medium)*(high_prob_rf-1) + (gbm_val$medium)*(high_prob_gbm-1),
                            "high" = (rf_res$low)*(high_prob_rf-1) + (gbm_val$low)*(high_prob_gbm-1)
                            )
      
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
# strdetect_df = data.frame()
# for (i in 1:nrow(t1)){
#   strdetect_df = rbind(strdetect_df, tryCatch(t(as.numeric(str_detect(tolower(df$features[i]), feature))), error = function(e){rep(0, length(feature))}))
# }
# 
strdetect_df = read.csv("strdetect_train.csv", stringsAsFactors = TRUE)
strdetect_df$X = NULL
for (i in c(1:44)){ #length(feature) instead of 44
  strdetect_df[[paste0("V", as.character(i))]] = as.factor(strdetect_df[[paste0("V", as.character(i))]])
}
t1 = cbind(t1, strdetect_df)

manager_df = t1[, c("manager_id", "interest_level")]  
manager_df = cbind(manager_df, model.matrix( ~ interest_level - 1, data = manager_df))
manager_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ manager_id, data = manager_df, FUN = sum)
manager_agg$count = rowSums(manager_agg[,c(2:4)])
manager_agg[, c(2:4)] = manager_agg[, c(2:4)]/manager_agg$count
manager_agg$manager_score = 2*manager_agg$interest_levelhigh + manager_agg$interest_levelmedium 
manager_agg$manager_score[manager_agg$count < 20] = 0
manager_agg$interest_levellow = NULL
manager_agg$interest_levelhigh = NULL
manager_agg$interest_levelmedium = NULL
manager_agg$count = NULL

t1 = merge(t1, manager_agg, by = "manager_id")
t1$manager_id = NULL

# nbd_count = aggregate(building_id ~ neighborhood, data = t1, FUN=function(x){length(unique(x))})
# colnames(nbd_count) = c("neighborhood", "building_count")
# nbd_count$building_count = as.factor(nbd_count$building_count > 10)
# t1 = merge(t1, nbd_count, by = "neighborhood")

building_df = t1[, c("building_id", "interest_level")]  
building_df = cbind(building_df, model.matrix( ~ interest_level - 1, data = building_df))
building_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ building_id, data = building_df, FUN = sum)
building_agg$count = rowSums(building_agg[,c(2:4)])
building_agg[, c(2:4)] = building_agg[, c(2:4)]/building_agg$count
building_agg$building_score = 2*building_agg$interest_levelhigh + building_agg$interest_levelmedium 
building_agg$building_score[building_agg$count < 20] = 0
building_agg$interest_levellow = NULL
building_agg$interest_levelhigh = NULL
building_agg$interest_levelmedium = NULL
building_agg$count = NULL

t1 = merge(t1, building_agg, by = "building_id")
t1$building_id = NULL

t1$medium_score = t1$building_score/t1$price

nbd_agg = aggregate(price ~ neighborhood, data = t1, FUN = mean)
colnames(nbd_agg) = c("neighborhood", "avg_price_nbd")

t1 = merge(t1, nbd_agg, by = "neighborhood")
t1$price_diff_from_mean = t1$price - t1$avg_price_nbd
t1$avg_price_nbd = NULL
  
t2 = generate_df(test, 0)
# strdetect_df_test = data.frame()
# for (i in 1:nrow(t2)){
#   strdetect_df_test = rbind(strdetect_df_test, tryCatch(t(as.numeric(str_detect(tolower(test$features[i]), feature))), error = function(e){rep(0, length(feature))}))
# }
# 
strdetect_df_test = read.csv("strdetect_test.csv", stringsAsFactors = TRUE)
for (i in c(1:44)){ #length(feature) instead of 44
  strdetect_df_test[[paste0("V", as.character(i))]] = as.factor(strdetect_df_test[[paste0("V", as.character(i))]])
}
strdetect_df_test$X = NULL
t2 = cbind(t2, strdetect_df_test)

t2 = left_join(t2, as.data.table(manager_agg), by = "manager_id")
t2$manager_score[is.na(t2$manager_score)] = mean(t2$manager_score, na.rm = TRUE)
t2$manager_id = NULL
t2 = left_join(t2, as.data.table(building_agg), by = "building_id")
t2$building_score[is.na(t2$building_score)] = mean(t2$building_score, na.rm = TRUE)
t2$building_id = NULL

t2 = left_join(t2, as.data.table(nbd_agg), by = "neighborhood")
t2$price_diff_from_mean[!is.na(t2$avg_price_nbd)] = t2$price[!is.na(t2$avg_price_nbd)] - t2$avg_price_nbd[!is.na(t2$avg_price_nbd)]
t2$price_diff_from_mean[is.na(t2$avg_price_nbd)] = 0
t2$avg_price_nbd = NULL

t2$medium_score = t2$building_score/t2$price

for (i in 1:44){
  
  variable_name = paste0("V", as.character(i))
  V1_df = cbind(t1[[variable_name]], t1[, c("interest_level")])
    
  colnames(V1_df) = c("V1", "interest_level")
  V1_df$V1 = as.numeric(V1_df$V1)
  
  V1_df = cbind(V1_df, model.matrix( ~ interest_level - 1, data = V1_df))
  V1_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ V1, data = V1_df, FUN = sum)
  V1_agg$count = rowSums(V1_agg[,c(2:4)])
  V1_agg[, c(2:4)] = V1_agg[, c(2:4)]/V1_agg$count
  V1_agg$V1_score = 2*V1_agg$interest_levelhigh + V1_agg$interest_levelmedium 
  V1_agg$V1_score[V1_agg$count[as.numeric(V1_agg$V1) == 1] < 20] = 0
  
  if (max(V1_agg$V1_score) < 0.4){
    t1[[variable_name]] = NULL
    t2[[variable_name]] = NULL
  }
}

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
write.csv(pred, "gbm_8.csv", row.names = FALSE)

#Running RF
res = rf_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "rf_h2o_3.csv", row.names = FALSE)