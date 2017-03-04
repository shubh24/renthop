library(jsonlite)
library(data.table)
library(lubridate)
library(caTools)
#library(caret)
library(e1071)
library(magrittr)
library(knitr)
library(stringr)
require(dplyr)
library(MLmetrics)
library(plyr)
#library(xgboost)
library(ggmap)
library(syuzhet)

#features to implement
#adj/nouns usage
#population density -- kind of locality

ny_lat <- 40.785091
ny_lon <- -73.968285

library(h2o)
h2o.init()

df = fromJSON("train.json")
test = fromJSON("test.json")

# cv = read.csv("count_vec.csv")
# cv[cv > 1] = 1
# cv$cv_count = rowSums(cv)
# for (i in colnames(cv)){cv[[i]] = as.factor(cv[[i]])}
# cv_train <- cv[1:49352, "cv_count"]
# cv_test <- cv[49353:124011, "cv_count"]

nbd_train = read.csv("neighborhood_train.csv", stringsAsFactors = TRUE)
nbd_test = read.csv("neighborhood_test.csv", stringsAsFactors = TRUE)

subway_train = read.csv("subway_train.csv", stringsAsFactors = TRUE)
subway_test = read.csv("subway_test.csv", stringsAsFactors = TRUE)

# feature = as.data.frame(table(tolower(unlist(df$features))))
# feature$Var1 = as.character(feature$Var1)
# 
# filtered_df = data.frame()
# 
keywords = c("24", "court", "wood", "roof", "outdoor", "garden", "parking", "bath", "actual", "allowed", "air", "doorman", "balcony", "available", "pool", "gym", "wifi", "fan", "playroom", "subway", "concierge", "fire", "fitness", "dish", "garage", "granite", "high", "laundry", "live", "no fee", "reduced fee", "war", "private", "lounge", "short", "spacious", "stainless", "storage", "terrace", "valet", "washer", "yoga")
# 
# for (j in 1:length(keywords)){
#   key = keywords[j]
# 
#   filtered = feature %>% filter(str_detect(feature$Var1, key))
#   # feature = rbind(feature, c(key, sum(filtered$Freq)))
#   
#   f_df = as.data.frame(cbind("Var1" = key, "Freq" = sum(filtered$Freq)))
#   
#   filtered_df = rbind(filtered_df, f_df)
#   
#   # for(i in 1:nrow(filtered)){
#   #   feature = feature[!(feature$Var1 == filtered$Var1[i]),]
#   # }
#   
#   filtered_df$Freq = as.numeric(filtered_df$Freq)
# }
# feature = as.vector(filtered_df$Var1[filtered_df$Freq > 50])

# feature = c("no fee", "furnish", "laundry", "outdoor", "parking", "allowed", "doorman", "elevator", "fitness", "storage")

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
    rf = h2o.randomForest(x = feature_names, y = "interest_level", training_frame = train_h2o, ntree = 1000)
    print(as.data.frame(h2o.varimp(rf))$variable)
    res = as.data.frame(predict(rf, test_h2o))

    return(res)
}

gbm_h2o = function(t1, t2){

  # write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
  # write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)
  # 
  # feature_names = names(t1)
  # feature_names = feature_names[!feature_names %in% c("created")]
  # feature_names = feature_names[!feature_names %in% c("listing_id")]
  # feature_names = feature_names[!feature_names %in% c("interest_level")]
  # 
  # train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
  # test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
  # 
  # ntrees_opts = c(2000)       # early stopping will stop earlier
  # max_depth_opts = seq(1,20)
  # min_rows_opts = c(1,5,10,20,50,100)
  # learn_rate_opts = seq(0.001,0.01,0.001)
  # sample_rate_opts = seq(0.3,1,0.05)
  # col_sample_rate_opts = seq(0.3,1,0.05)
  # col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
  # #nbins_cats_opts = seq(100,10000,100) # no categorical features
  # # in this dataset
  # 
  # hyper_params = list( ntrees = ntrees_opts,
  #                      max_depth = max_depth_opts,
  #                      min_rows = min_rows_opts,
  #                      learn_rate = learn_rate_opts,
  #                      sample_rate = sample_rate_opts,
  #                      col_sample_rate = col_sample_rate_opts,
  #                      col_sample_rate_per_tree = col_sample_rate_per_tree_opts
  #                      #,nbins_cats = nbins_cats_opts
  # )
  # 
  # 
  # # Search a random subset of these hyper-parmameters. Max runtime
  # # and max models are enforced, and the search will stop after we
  # # don't improve much over the best 5 random models.
  # search_criteria = list(strategy = "RandomDiscrete",
  #                        max_runtime_secs = 300,
  #                        max_models = 100,
  #                        stopping_metric = "logloss",
  #                        stopping_tolerance = 0.00001,
  #                        stopping_rounds = 5,
  #                        seed = 123456)
  # 
  # gbm_grid <- h2o.grid("gbm",
  #                      grid_id = "mygrid",
  #                      x = feature_names,
  #                      y = "interest_level",
  # 
  #                      # faster to use a 80/20 split
  #                      # training_frame = train_h2o,
  #                      # validation_frame = test_h2o,
  #                      # nfolds = 0,
  # 
  #                      # alternatively, use N-fold cross-validation:
  #                      training_frame = train_h2o,
  #                      nfolds = 5,
  # 
  #                      # Gaussian is best for MSE loss, but can try
  #                      # other distributions ("laplace", "quantile"):
  #                      distribution="multinomial",
  # 
  #                      # stop as soon as mse doesn't improve by
  #                      # more than 0.1% on the validation set,
  #                      # for 2 consecutive scoring events:
  #                      stopping_rounds = 2,
  #                      stopping_tolerance = 1e-3,
  #                      stopping_metric = "logloss",
  # 
  #                      # how often to score (affects early stopping):
  #                      score_tree_interval = 10,
  # 
  #                      ## seed to control the sampling of the
  #                      ## Cartesian hyper-parameter space:
  #                      seed = 123456,
  #                      hyper_params = hyper_params,
  #                      search_criteria = search_criteria)
  # 
  # gbm_sorted_grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "logloss")
  # print(gbm_sorted_grid)
# 
#   best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
#   summary(best_model)

  write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
  write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)

  feature_names = names(t1)
  feature_names = feature_names[!feature_names %in% c("created")]
  feature_names = feature_names[!feature_names %in% c("listing_id")]
  feature_names = feature_names[!feature_names %in% c("interest_level")]

  train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
  test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")

  gbm1 <- h2o.gbm(x = feature_names
                ,y = "interest_level"
                ,training_frame = train_h2o
                ,distribution = "multinomial"
                ,model_id = "gbm1"
                #,nfolds = 5
                ,ntrees = 1000
                ,learn_rate = 0.01
                ,max_depth = 6
                ,min_rows = 10
                ,sample_rate = 0.8
                ,score_tree_interval = 10
                ,col_sample_rate = 0.75
                ,stopping_rounds = 5
                ,stopping_metric = "logloss"
                ,stopping_tolerance = 1e-4
                ,seed=321)

  print(as.data.frame(h2o.varimp(gbm1)))
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
                     ,description=unlist(df$description) # parse errors
                     ,display_address=as.character(unlist(df$display_address)) # parse errors
                     ,latitude=unlist(df$latitude)
                     ,longitude=unlist(df$longitude)
                     ,distance_to_city=mapply(function(lon,lat) sqrt((lon - ny_lon)^2 + (lat - ny_lat)^2),
                                              df$longitude,
                                              df$latitude)
                     ,listing_id=unlist(df$listing_id)
                     ,manager_id=as.factor(unlist(df$manager_id))
                     ,price=unlist(df$price)
                     #,yday=as.factor(sapply(df$created, yday))
                     #,month=as.factor(sapply(df$created, lubridate::month))
                     ,mday=as.factor(sapply(df$created, mday))
                     ,wday=as.factor(sapply(df$created, wday))
                     ,hour=as.factor(sapply(df$created, lubridate::hour))
                     #,days_since = as.numeric(difftime(Sys.Date(), unlist(df$created)))
                     #,interest_level=as.factor(unlist(df$interest_level))
                     ,street_address=as.character(unlist(df$street_address)) # parse errors
    )
    
    if (train_flag == 1){
      t1$interest_level = as.factor(unlist(df$interest_level))
      #t1 = cbind(t1, cv_train)
      #names(t1)[names(t1) == "cv_train"] = "cv_count"
      t1 = merge(t1, nbd_train, by = "listing_id")
      t1 = merge(t1, subway_train, by = "listing_id")
    }
    else{
      #t1 = cbind(t1, cv_test)
      #names(t1)[names(t1) == "cv_test"] = "cv_count"
      t1 = merge(t1, nbd_test, by = "listing_id")
      t1 = merge(t1, subway_test, by = "listing_id")
    }
    
    # last_active may not be working because we don't know how the test data is split -- time/randomly etc
    manager_activity = t1[, c("listing_id", "manager_id", "created")]
    last_active_df = data.frame()

    for (i in 1:length(levels(t1$manager_id))){

        specific_manager_activity = data.frame(manager_activity$listing_id[as.character(manager_activity$manager_id) == as.character(levels(t1$manager_id)[i])], sort(manager_activity$created[as.character(manager_activity$manager_id) == as.character(levels(t1$manager_id)[i])]))
        colnames(specific_manager_activity) = c("listing_id", "created")

        specific_manager_activity$last_active = c(0, as.numeric(diff(as.Date(specific_manager_activity$created))))

        last_active_df = rbind(last_active_df, specific_manager_activity[, c("listing_id", "last_active")])
      }

    t1 = merge(as.data.table(t1), as.data.table(last_active_df), by = "listing_id")

    sentiment = get_nrc_sentiment(t1$description)
    t1$sentiment = sentiment$positive/(sentiment$positive + sentiment$negative)
    t1$sentiment[is.na(t1$sentiment)] = mean(t1$sentiment[!is.na(t1$sentiment)])
    t1$description = NULL
    
    t1$price_per_br = t1$price/t1$bedrooms        
    t1$price_per_ba = t1$price/t1$bathrooms
    
    outliers <- t1[t1$longitude == 0 | t1$latitude == 0, ]
    outliers_ny <- as.data.frame(cbind(outliers$listing_id, paste0(outliers$street_address, ", new york")))
    colnames(outliers_ny) = c("listing_id", "street_address")
    
    for (i in 1:nrow(outliers_ny)){
      coord = geocode(as.character(outliers_ny$street_address[i]), source = "google")      

      t1$latitude[as.character(t1$listing_id) == as.character(outliers_ny$listing_id[i])] = as.numeric(coord["lat"])
      t1$longitude[as.character(t1$listing_id) == outliers_ny$listing_id[i]] = as.numeric(coord["lon"])
    }
    t1$street_address = NULL
    
    t1$bathrooms_whole = as.factor(as.integer(t1$bathrooms) == t1$bathrooms)
    t1$bed_bath_diff = t1$bedrooms - t1$bathrooms
    #   
    # t1$one_bathroom = as.factor(t1$bathrooms == 1)
    # t1$two_bathrooms = as.factor(t1$bathrooms == 2)
    # t1$three_bathrooms = as.factor(t1$bathrooms == 3)
    # t1$four_bathrooms = as.factor(t1$bathrooms == 3)
    # t1$five_plus_bathrooms = as.factor(t1$bathrooms > 4)
    # t1$bathrooms = as.factor(t1$bathrooms)
    
    # t1$one_bedroom = as.factor(t1$bedrooms == 1)
    # t1$two_bedrooms = as.factor(t1$bedrooms == 2)
    # t1$three_bedrooms = as.factor(t1$bedrooms == 3)
    # t1$four_plus_bedrooms = as.factor(t1$bedrooms > 3)
    # t1$bedrooms = as.factor(t1$bedrooms)
    
    t1$street_number_provided = as.factor(grepl("\\d", t1$display_address))
    
    t1$street_type = as.character(sapply(t1$display_address, function(x){substring(tolower(tail(strsplit(x, " ")[[1]], n = 1)), 1, 2)}))
    street_type = as.data.frame(table(as.factor(t1$street_type)))
    top_streets = street_type$Var1[street_type$Freq > 200]
    t1$street_type = as.factor(ifelse(t1$street_type %in% top_streets, yes = as.character(t1$street_type), no = "other"))
  
    t1$east = as.factor(str_detect(tolower(t1$display_address), "east"))
    t1$west = as.factor(str_detect(tolower(t1$display_address), "west"))
    # t1$north = as.factor(str_detect(tolower(t1$display_address), "north"))
    # t1$south = as.factor(str_detect(tolower(t1$display_address), "south"))
    
    t1$display_address = NULL
    
    t1$zero_building_id = as.factor(t1$building_id == 0)
    # buildings = as.data.frame(table(t1$building_id))
    # buildings = buildings[-(buildings$Var1 == 0),]
    # t1$top10buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 10)$Var1, yes = 1, no = 0))
    # t1$top50buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 50)$Var1, yes = 1, no = 0))
    # t1$top100buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 100)$Var1, yes = 1, no = 0))
    t1$building_id = NULL
    
    # t1$zero_description = as.factor(t1$n_description == 0)
    t1$zero_photos = as.factor(t1$n_photos == 0)
    
    # t1$expensive = as.factor(t1$price > 7000)
    # t1$cheap = as.factor(t1$price < 1500)
    
    # t1$luxury_or_not = grepl("luxury", tolower(df$description))
    t1$studio = grepl("studio", tolower(df$description))
    t1$no_fee = grepl("no fee", tolower(df$description))
    
    t1$price = 1/t1$price
    t1$price_per_br = 1/t1$price_per_br
    t1$price_per_ba = 1/t1$price_per_ba
    
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

get_manager_scores = function(t1, t2){
  
  manager_df = t1[, c("manager_id", "interest_level")]  
  manager_df = cbind(manager_df, model.matrix( ~ interest_level - 1, data = manager_df))
  manager_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ manager_id, data = manager_df, FUN = sum)
  manager_agg$count = rowSums(manager_agg[,c(2:4)])
  manager_agg[, c(2:4)] = manager_agg[, c(2:4)]/manager_agg$count
  manager_agg$manager_score = 2*manager_agg$interest_levelhigh + 1*manager_agg$interest_levelmedium 
  manager_agg$manager_score[manager_agg$count < 3] = median(manager_agg$manager_score[manager_agg$count >= 3])
  manager_agg$interest_levellow = NULL
  manager_agg$interest_levelhigh = NULL
  manager_agg$interest_levelmedium = NULL
  manager_agg$count = NULL
  
  t1 = merge(t1, manager_agg, by = "manager_id")
  t1$manager_id = NULL
  
  t2 = left_join(t2, as.data.table(manager_agg), by = "manager_id")
  t2$manager_score[is.na(t2$manager_score)] = median(t2$manager_score, na.rm = TRUE)
  t2$manager_id = NULL
  
  return(list(t1, t2))
  
}

get_building_scores = function(t1, t2){
  building_df = t1[, c("building_id", "interest_level")]  
  building_df = cbind(building_df, model.matrix( ~ interest_level - 1, data = building_df))
  building_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ building_id, data = building_df, FUN = sum)
  building_agg$count = rowSums(building_agg[,c(2:4)])
  building_agg[, c(2:4)] = building_agg[, c(2:4)]/building_agg$count
  building_agg$building_score = 2*building_agg$interest_levelhigh + building_agg$interest_levelmedium 
  # building_agg$building_score[building_agg$count < 20] = 0
  building_agg$interest_levellow = NULL
  building_agg$interest_levelhigh = NULL
  building_agg$interest_levelmedium = NULL
  building_agg$count = NULL
  
  t1 = merge(t1, building_agg, by = "building_id")
  t1$building_id = NULL
  
  t2 = left_join(t2, as.data.table(building_agg), by = "building_id")
  t2$building_score[is.na(t2$building_score)] = median(t2$building_score, na.rm = TRUE)
  t2$building_id = NULL
  
  return(list(t1,t2))
}

get_nbd_scores = function(t1, t2){

  nbd_agg = aggregate(price ~ neighborhood + bedrooms, data = t1, FUN = median)
  colnames(nbd_agg)[colnames(nbd_agg) == "price"] = "median_price_nbd"
  t1 = merge(t1, nbd_agg, by = c("neighborhood", "bedrooms"))
  t1$price_diff_from_median = as.factor(t1$price > t1$median_price_nbd)
  t1$median_price_nbd = NULL
  
  t2 = left_join(t2, as.data.table(nbd_agg), by = c("neighborhood", "bedrooms"))
  t2$price_diff_from_median[!is.na(t2$median_price_nbd)] = t2$price[!is.na(t2$median_price_nbd)] - t2$median_price_nbd[!is.na(t2$median_price_nbd)]
  t2$price_diff_from_median[is.na(t2$median_price_nbd)] = 0
  t2$price_diff_from_median = as.factor(t2$price_diff_from_median > 0)
  t2$median_price_nbd = NULL
  
  neighborhood_df = t1[, c("neighborhood", "interest_level")]  
  neighborhood_df = cbind(neighborhood_df, model.matrix( ~ interest_level - 1, data = neighborhood_df))
  neighborhood_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ neighborhood, data = neighborhood_df, FUN = sum)
  neighborhood_agg$count = rowSums(neighborhood_agg[,c(2:4)])
  neighborhood_agg[, c(2:4)] = neighborhood_agg[, c(2:4)]/neighborhood_agg$count
  neighborhood_agg$neighborhood_score = 2*neighborhood_agg$interest_levelhigh + neighborhood_agg$interest_levelmedium 
  neighborhood_agg$neighborhood_score[neighborhood_agg$count < 3] = median(neighborhood_agg$neighborhood_score[neighborhood_agg$count >= 3])
  neighborhood_agg$interest_levellow = NULL
  neighborhood_agg$interest_levelhigh = NULL
  neighborhood_agg$interest_levelmedium = NULL
  neighborhood_agg$count = NULL
  
  t1 = merge(t1, neighborhood_agg, by = "neighborhood")
  t1$neighborhood = NULL
  
  t2 = left_join(t2, as.data.table(neighborhood_agg), by = "neighborhood")
  t2$neighborhood_score[is.na(t2$neighborhood_score)] = median(t2$neighborhood_score, na.rm = TRUE)
  t2$neighborhood = NULL
  
  return(list(t1, t2))
}

validate_gbm = function(t1){
  set.seed(101) 
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  # gbm_tuning(t1_train, t1_test)
  
  manager_res = get_manager_scores(t1_train, t1_test)
  t1_train = manager_res[[1]]
  t1_test = manager_res[[2]]
  
  t1$building_id = NULL
  # building_res = get_building_scores(t1_train, t1_test)
  # t1_train = building_res[[1]]
  # t1_test = building_res[[2]]
  
  # t1_train$medium_score = t1_train$building_score*t1_train$n_features/t1_train$price
  # t1_test$medium_score = t1_test$building_score*t1_test$n_features/t1_test$price

  nbd_res = get_nbd_scores(t1_train, t1_test)
  t1_train = nbd_res[[1]]
  t1_test = nbd_res[[2]]
  
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
                            "low" = (rf_res$low)*(high_prob_rf-1) + (gbm_val$low)*(high_prob_gbm-1)
                            )
  
  ensemble_res$predict = colnames(ensemble_res)[apply(ensemble_res, 1, which.max)]
  print(MLmetrics::ConfusionMatrix(y_pred = ensemble_res$predict, y_true = t1_test$interest_level))
  print(MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(ensemble_res[,c("high", "low", "medium")])))
  
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
                  nround=10000, 
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
#   print(i)
#   strdetect_df = rbind(strdetect_df, tryCatch(t(as.numeric(str_detect(tolower(df$features[i]), feature))), error = function(e){rep(0, length(feature))}))
# }
# write.csv(strdetect_df, "strdetect_train.csv", row.names = FALSE)
strdetect_df = read.csv("strdetect_train.csv", stringsAsFactors = TRUE)
# for (i in c(1:length(feature))){ #length(feature) instead of 44
#   strdetect_df[[paste0("V", as.character(i))]] = as.factor(strdetect_df[[paste0("V", as.character(i))]])
# }
# t1$imp_features = rowSums(strdetect_df)
t1$relevant_features = rowSums(strdetect_df)/t1$n_features
#Get the 44 features -- count of "high" featuers(ratio of high greater than regular high) against count of "low" features

# nbd_count = aggregate(building_id ~ neighborhood, data = t1, FUN=function(x){length(unique(x))})
# colnames(nbd_count) = c("neighborhood", "building_count")
# nbd_count$building_count = as.factor(nbd_count$building_count > 10)
# t1 = merge(t1, nbd_count, by = "neighborhood")

t2 = generate_df(test, 0)
# strdetect_df_test = data.frame()
# for (i in 1:nrow(t2)){
#   print(i)
#   strdetect_df_test = rbind(strdetect_df_test, tryCatch(t(as.numeric(str_detect(tolower(test$features[i]), feature))), error = function(e){rep(0, length(feature))}))
# }
# write.csv(strdetect_df_test, "strdetect_test.csv", row.names = FALSE)
# strdetect_df_test = read.csv("strdetect_test.csv", stringsAsFactors = TRUE)
# for (i in c(1:length(feature))){ 
#   strdetect_df_test[[paste0("V", as.character(i))]] = as.factor(strdetect_df_test[[paste0("V", as.character(i))]])
# }
# t2 = cbind(t2, strdetect_df_test)


# for (i in 1:length(feature)){
# 
#   variable_name = paste0("V", as.character(i))
#   V1_df = cbind(t1[[variable_name]], t1[, c("interest_level")])
# 
  # colnames(V1_df) = c("V1", "interest_level")
  # V1_df$V1 = as.numeric(V1_df$V1)
  # 
  # V1_df = cbind(V1_df, model.matrix( ~ interest_level - 1, data = V1_df))
  # V1_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ V1, data = V1_df, FUN = sum)
  # V1_agg$count = rowSums(V1_agg[,c(2:4)])
  # V1_agg[, c(2:4)] = V1_agg[, c(2:4)]/V1_agg$count
  # if (max(V1_agg$interest_levelhigh) > 0.1){
  #   print(feature[i])
  # }
#   # V1_agg$V1_score = 2*V1_agg$interest_levelhigh + V1_agg$interest_levelmedium
#   # V1_agg$V1_score[V1_agg$count[as.numeric(V1_agg$V1) == 1] < 20] = 0
# 
#   # if (max(V1_agg$V1_score) < 0.4){
#   #   t1[[variable_name]] = NULL
#   #   t2[[variable_name]] = NULL
#   # }
# }

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

manager_res = get_manager_scores(t1, t2)
t1 = manager_res[[1]]
t2 = manager_res[[2]]

t1$building_id = NULL
t2$building_id = NULL

nbd_res = get_nbd_scores(t1, t2)
t1 = nbd_res[[1]]
t2 = nbd_res[[2]]

pred_df_gbm = gbm_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(pred_df_gbm$high), medium = as.vector(pred_df_gbm$medium), low = as.vector(pred_df_gbm$low))
write.csv(pred, "gbm_18.csv", row.names = FALSE)

#Running RF
res = rf_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "rf_h2o_3.csv", row.names = FALSE)


for(i in 1:length(keywords)){
   variable_name = keywords[i]
   V1_df = cbind(check[[variable_name]], check[, c("t1$interest_level")])
   
   colnames(V1_df) = c("V1", "interest_level")
   V1_df$V1 = as.numeric(V1_df$V1)
     
   V1_df = cbind(V1_df, model.matrix( ~ interest_level - 1, data = V1_df))
   V1_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ V1, data = V1_df, FUN = sum)
   V1_agg$count = rowSums(V1_agg[,c(2:4)])
   V1_agg[, c(2:4)] = V1_agg[, c(2:4)]/V1_agg$count
   if (max(V1_agg$interest_levelhigh) > 0.1){
     print(feature[i])
   }
       
}