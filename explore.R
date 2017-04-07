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
library(ggmap)
library(syuzhet)
library(geosphere)
library(RecordLinkage)

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

town_train = read.csv("town_train.csv", stringsAsFactors = FALSE)
town_test = read.csv("town_test.csv", stringsAsFactors = FALSE)

subway_train = read.csv("subway_train.csv", stringsAsFactors = TRUE)
subway_test = read.csv("subway_test.csv", stringsAsFactors = TRUE)

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

# strdetect_df_test = data.frame()
# for (i in 1:nrow(t2)){
#   print(i)
#   strdetect_df_test = rbind(strdetect_df_test, tryCatch(t(as.numeric(str_detect(tolower(test$features[i]), feature))), error = function(e){rep(0, length(feature))}))
# }
# write.csv(strdetect_df_test, "strdetect_test.csv", row.names = FALSE)
strdetect_df_test = read.csv("strdetect_test.csv", stringsAsFactors = TRUE)
# for (i in c(1:length(feature))){ 
#   strdetect_df_test[[paste0("V", as.character(i))]] = as.factor(strdetect_df_test[[paste0("V", as.character(i))]])
# }
# t2 = cbind(t2, strdetect_df_test)

# feature = as.data.frame(table(tolower(unlist(df$features))))
# feature$Var1 = as.character(feature$Var1)
# 
# filtered_df = data.frame()
# 
# keywords = c("24", "court", "wood", "roof", "outdoor", "garden", "parking", "bath", "actual", "allowed", "air", "doorman", "balcony", "available", "pool", "gym", "wifi", "fan", "playroom", "subway", "concierge", "fire", "fitness", "dish", "garage", "granite", "high", "laundry", "live", "no fee", "reduced fee", "war", "private", "lounge", "short", "spacious", "stainless", "storage", "terrace", "valet", "washer", "yoga")
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
  # ntrees_opts = c(2500)       # early stopping will stop earlier
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
  #                        max_runtime_secs = 3600,
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
  #                      # nfolds = 5,
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
  # best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
  # summary(best_model)

  write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
  write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)

  feature_names = names(t1)
  feature_names = feature_names[!feature_names %in% c("created")]
  # feature_names = feature_names[!feature_names %in% c("listing_id")]
  feature_names = feature_names[!feature_names %in% c("interest_level")]

  train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
  test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")

  gbm1 <- h2o.gbm(x = feature_names
                ,y = "interest_level"
                ,training_frame = train_h2o
                ,distribution = "multinomial"
                ,model_id = "gbm1"
                # ,nfolds = 5
                ,ntrees = 1400
                # ,learn_rate = 0.004
                ,learn_rate = 0.01
                ,max_depth = 6
                ,min_rows = 10
                ,sample_rate = 0.7
                ,score_tree_interval = 10
                ,col_sample_rate = 0.5
                ,stopping_rounds = 5
                ,stopping_metric = "logloss"
                ,stopping_tolerance = 1e-4
                ,seed=100)

  print(as.data.frame(h2o.varimp(gbm1)))
  # print(h2o.performance(model = gbm1, newdata = test_h2o))
  
  res = as.data.frame(predict(gbm1, test_h2o))

  return(res)
}

dl_h2o = function(t1, t2){
  
  write.table(t1, gzfile('./t1.csv.gz'),quote=F,sep=',',row.names=F)
  write.table(t2, gzfile('./t2.csv.gz'),quote=F,sep=',',row.names=F)
  
  feature_names = names(t1)
  feature_names = feature_names[!feature_names %in% c("created")]
  # feature_names = feature_names[!feature_names %in% c("listing_id")]
  feature_names = feature_names[!feature_names %in% c("interest_level")]
  
  train_h2o = h2o.uploadFile("./t1.csv.gz", destination_frame = "train")
  test_h2o = h2o.uploadFile("./t2.csv.gz", destination_frame = "test")
  
  dl1 <- h2o.deeplearning(
    model_id = "dl_1", # (optional) assign a user-specified id to the model
    training_frame = train_h2o, 
    # validation_frame = valid, # validation dataset: used for scoring and early stopping
    x = feature_names,
    y = "interest_level",
    activation = "Rectifier", # default (a.k.a Relu)
    hidden = c(200, 200),    # default = 2 hidden layers with 200 neurons each
    epochs = 1, # How many times the dataset should be iterated
    variable_importances = TRUE # allows obtaining the variable importance, not enabled by default
  )
  
  
  print(as.data.frame(h2o.varimp(dl1)))
  res = as.data.frame(predict(dl1, test_h2o))
  
  return(res)
}

generate_df = function(df, train_flag){
    t1 <- data.table(bathrooms=unlist(df$bathrooms)
                     ,bedrooms=unlist(df$bedrooms)
                     ,building_id=as.factor(unlist(df$building_id))
                     ,created=as.POSIXct(unlist(df$created))
                     ,n_photos = as.numeric(sapply(df$photos, length))
                     ,n_words = sapply(strsplit(as.character(df$description), "\\s+"), length)
                     ,n_description = as.numeric(sapply(df$description, nchar))
                     ,n_features = as.numeric(sapply(df$features, length))
                     ,description=unlist(df$description) # parse errors
                     ,display_address=as.character(unlist(tolower(df$display_address))) # parse errors
                     ,latitude=unlist(df$latitude)
                     ,longitude=unlist(df$longitude)
                     ,distance_to_city=mapply(function(lon,lat) distm (c(lon, lat), c(ny_lon, ny_lat), fun = distHaversine)/1000,
                                              df$longitude,
                                              df$latitude)
                     ,listing_id=unlist(df$listing_id)
                     ,manager_id=as.factor(unlist(df$manager_id))
                     ,price=unlist(df$price)
                     ,month=as.numeric(sapply(df$created, lubridate::month))
                     ,mday=as.numeric(sapply(df$created, mday))
                     ,wday=as.factor(sapply(df$created, wday))
                     ,hour=as.numeric(sapply(df$created, lubridate::hour))
                     ,minute=as.numeric(sapply(df$created, lubridate::minute))
                     #,interest_level=as.factor(unlist(df$interest_level))
                     ,street_address=as.character(unlist(tolower(df$street_address))) # parse errors
    )
    
    if (train_flag == 1){
      t1$interest_level = as.factor(unlist(df$interest_level))
      #t1 = cbind(t1, cv_train)
      #names(t1)[names(t1) == "cv_train"] = "cv_count"
      t1 = merge(t1, nbd_train, by = "listing_id")
      t1 = merge(t1, town_train, by = "listing_id")
      t1 = merge(t1, subway_train, by = "listing_id")
      t1$relevant_features = rowSums(strdetect_df)/t1$n_features
      t1$relevant_features[is.na(t1$relevant_features)] = 0
    }
    else{
      #t1 = cbind(t1, cv_test)
      #names(t1)[names(t1) == "cv_test"] = "cv_count"
      t1 = merge(t1, nbd_test, by = "listing_id")
      t1 = merge(t1, town_test, by = "listing_id")
      t1 = merge(t1, subway_test, by = "listing_id")
      t1$relevant_features = rowSums(strdetect_df_test)/t1$n_features
    }
    
    t1$total_days = (t1$month - 4.0)*30 + t1$mday +  t1$hour/25.0
    # t1$slope_listing_days = (t1$listing_id - min(t1$listing_id))/t1$total_days
    t1$month = NULL
    
    # t1$caps_count = sapply(regmatches(as.vector(t1$description), gregexpr("[A-Z]", as.vector(t1$description), perl=TRUE)), length)
    # t1$caps_count = t1$caps_count/nchar(t1$description)
    # t1$bang_count = sapply(regmatches(as.vector(t1$description), gregexpr("!", as.vector(t1$description), perl=TRUE)), length)
    
    sentiment = get_nrc_sentiment(t1$description)
    t1$sentiment = sentiment$positive/(sentiment$positive + sentiment$negative)
    t1$sentiment[is.na(t1$sentiment)] = mean(t1$sentiment[!is.na(t1$sentiment)])
    t1$description = NULL
    
    t1$price_per_room = t1$price/(1 + t1$bedrooms + 0.5*t1$bathrooms)     
    t1$price_per_bedroom = t1$price/t1$bedrooms
    t1$price_per_bathroom = t1$price/t1$bathrooms
    t1$rooms = t1$bedrooms + t1$bathrooms
    
    # t1$ends_with_95 = as.factor(substr(as.character(t1$price), nchar(as.character(t1$price))-1, nchar(as.character(t1$price))) == "95")
    # t1$ends_with_99 = as.factor(substr(as.character(t1$price), nchar(as.character(t1$price))-1, nchar(as.character(t1$price))) == "99")
    # t1$ends_with_999 = as.factor(substr(as.character(t1$price), nchar(as.character(t1$price))-2, nchar(as.character(t1$price))) == "999")
    t1$number_of_9 = as.numeric(str_count(substr(as.character(t1$price), nchar(as.character(t1$price))-2, nchar(as.character(t1$price))), "9"))
    
    outliers <- t1[t1$longitude == 0 | t1$latitude == 0, ]
    outliers_ny <- as.data.frame(cbind(outliers$listing_id, paste0(outliers$street_address, ", new york")))
    colnames(outliers_ny) = c("listing_id", "street_address")
    
    for (i in 1:nrow(outliers_ny)){
      coord = geocode(as.character(outliers_ny$street_address[i]), source = "google")      

      t1$latitude[as.character(t1$listing_id) == as.character(outliers_ny$listing_id[i])] = as.numeric(coord["lat"])
      t1$longitude[as.character(t1$listing_id) == outliers_ny$listing_id[i]] = as.numeric(coord["lon"])
    }
    
    t1$street_display_sim = levenshteinSim(t1$street_address, t1$display_address)
    
    t1$street_address = NULL
    
    t1$bathrooms_whole = as.factor(as.integer(t1$bathrooms) == t1$bathrooms)
    t1$bed_bath_diff = t1$bedrooms - t1$bathrooms
    # t1$street_number_provided = as.factor(grepl("\\d", t1$display_address))
    # t1$phone_number_provided = as.factor(grepl("\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d", df$description))

    t1$street_type = as.character(sapply(t1$display_address, function(x){substring(tolower(tail(strsplit(x, " ")[[1]], n = 1)), 1, 2)}))
    street_type = as.data.frame(table(as.factor(t1$street_type)))
    top_streets = street_type$Var1[street_type$Freq > 200]
    t1$street_type = as.factor(ifelse(t1$street_type %in% top_streets, yes = as.character(t1$street_type), no = "other"))
  
    # t1$east = as.factor(str_detect(tolower(t1$display_address), "east"))
    # t1$west = as.factor(str_detect(tolower(t1$display_address), "west"))
    # t1$north = as.factor(str_detect(tolower(t1$display_address), "north"))
    # t1$south = as.factor(str_detect(tolower(t1$display_address), "south"))
    
    t1$zero_building_id = as.factor(t1$building_id == 0)
    # buildings = as.data.frame(table(t1$building_id))
    # buildings = buildings[-(buildings$Var1 == 0),]
    # t1$top10buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 10)$Var1, yes = 1, no = 0))
    # t1$top50buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 50)$Var1, yes = 1, no = 0))
    # t1$top100buildings = as.factor(ifelse(as.character(t1$building_id) %in% head(arrange(buildings, desc(Freq)), n = 100)$Var1, yes = 1, no = 0))
    
    # t1$building_id = NULL
    
    # t1$zero_description = as.factor(t1$n_description == 0)
    t1$zero_photos = as.factor(t1$n_photos == 0)
    
    # t1$expensive = as.factor(t1$price > 7000)
    # t1$cheap = as.factor(t1$price < 1500)
    
    # t1$luxury_or_not = grepl("luxury", tolower(df$description))
    # t1$studio = grepl("studio", tolower(df$description))
    t1$no_fee = grepl("no fee", tolower(df$features))
    t1$featured = grepl("featured", tolower(df$features))
    # t1$outdoor = grepl("outdoor", tolower(df$features))
  
    hot_keywords = c("manhattan","central park","subway","train","bikeway","columbus circle")
    hot_keywords_count = as.numeric(grepl(hot_keywords[1], tolower(df$description)))

    for (i in 2:length(hot_keywords)){
      keyword = hot_keywords[i]
      hot_keywords_count = hot_keywords_count + as.numeric(grepl(keyword, tolower(df$description)))
    }
    
    t1$hot_keywords_count = hot_keywords_count
    
    # t1$n_good = t1$n_features + t1$n_photos + t1$n_description + t1$number_of_9
    # t1$n_features = NULL
    # t1$n_photos = NULL
    # t1$n_description = NULL
    # t1$number_of_9 = NULL
    
    # t1$V2 = grepl("air", tolower(df$features))
    # t1$V3 = grepl("pool", tolower(df$features))
    # t1$V4 = grepl("wifi", tolower(df$features))
    # t1$V5 = grepl("playroom", tolower(df$features))
    # t1$V6 = grepl("garage", tolower(df$features))
    # t1$V7 = grepl("reduced fee", tolower(df$features))
    # t1$V8 = grepl("private", tolower(df$features))
    # t1$V9 = grepl("terrace", tolower(df$features))
    # t1$V10 = grepl("yoga", tolower(df$features))

    t1$town = as.factor(t1$town)
    
    t1$price = log(t1$price)
    t1$price_per_room = log(t1$price_per_room)
    t1$price_per_bedroom = log(t1$price_per_bedroom)
    t1$price_per_bathroom = log(t1$price_per_bathroom)
    
    t1$minutes = t1$hour*60 + t1$minute
    t1$hour = NULL
    t1$minute = NULL
    
    return (t1)
}

validate = function(t1){
  set.seed(101) 
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  manager_res = get_manager_scores(t1_train, t1_test)
  t1_train = manager_res[[1]]
  t1_test = manager_res[[2]]
  
  # t1$building_id = NULL
  building_res = get_building_scores(t1_train, t1_test)
  t1_train = building_res[[1]]
  t1_test = building_res[[2]]
  
  # t1_train$medium_score = t1_train$building_score*t1_train$n_features/t1_train$price
  # t1_test$medium_score = t1_test$building_score*t1_test$n_features/t1_test$price
  
  nbd_res = get_nbd_scores(t1_train, t1_test)
  t1_train = nbd_res[[1]]
  t1_test = nbd_res[[2]]
  
  res_val = rf_h2o(t1_train, t1_test)
  print(MLmetrics::ConfusionMatrix(y_pred = res_val$predict, y_true = t1_test$interest_level))
  print(MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(res_val[,c("high", "low", "medium")])))
 
  return (res_val[, c("high", "low", "medium")]) 
}

get_last_active = function(t1){
  
  manager_activity = data.frame("listing_id" = unlist(df$listing_id), "manager_id" = unlist(df$manager_id), "created" = unlist(df$created))
  manager_activity = rbind(manager_activity, data.frame("listing_id" = unlist(test$listing_id), "manager_id" = unlist(test$manager_id), "created" = unlist(test$created)))

  last_active_df = data.frame()
  
  for (i in 1:length(levels(t1$manager_id))){
    
    specific_manager_activity = data.frame(manager_activity$listing_id[as.character(manager_activity$manager_id) == as.character(levels(t1$manager_id)[i])], sort(manager_activity$created[as.character(manager_activity$manager_id) == as.character(levels(t1$manager_id)[i])]))
    colnames(specific_manager_activity) = c("listing_id", "created")
    
    specific_manager_activity$last_active = c(0, as.numeric(diff(as.Date(specific_manager_activity$created))))
    
    last_active_df = rbind(last_active_df, specific_manager_activity[, c("listing_id", "last_active")])
  }
  
  t1 = merge(as.data.table(t1), as.data.table(last_active_df), by = "listing_id")
  
  return(t1)
}

get_multi_town = function(t1, t2){
  
  # manager_df = t1[, c("manager_id", "town", "interest_level")]
  # manager_df = cbind(manager_df, model.matrix( ~ interest_level - 1, data = manager_df))
  # 
  # manager_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ manager_id + town, data = manager_df, FUN = sum)
  # manager_agg$manager_count = rowSums(manager_agg[,c(3:5)])
  # manager_agg[, c(3:5)] = manager_agg[, c(3:5)]/manager_agg$manager_count
  # 
  # manager_agg$manager_town_score = 2*manager_agg$interest_levelhigh + manager_agg$interest_levelmedium
  # manager_agg$manager_town_score[manager_agg$manager_count < 10] = median(manager_agg$manager_town_score[manager_agg$manager_count >= 10])
  # 
  # manager_agg$interest_levellow = NULL
  # manager_agg$interest_levelhigh = NULL 
  # manager_agg$interest_levelmedium = NULL
  # manager_agg$manager_count = NULL
  # 
  # t1 = merge(t1, manager_agg, by = c("manager_id", "town"))
  # t2 = left_join(t2, as.data.table(manager_agg), by = c("manager_id", "town"))
  # 
  # t2$manager_town_score[is.na(t2$manager_town_score)] = median(t2$manager_town_score)

  manager_df = rbind(t1[, c("manager_id", "town")], t2[, c("manager_id", "town")])
  manager_df$dummy = 1
  manager_town_agg = aggregate(dummy ~ manager_id + town, data = manager_df, FUN = sum)
  manager_town_agg$gt10 = as.numeric(manager_town_agg$dummy >= 50)
  
  manager_agg = aggregate(dummy ~ manager_id, data = manager_town_agg, FUN = sum)
  colnames(manager_agg) = c("manager_id", "dummy_sum")
  manager_agg$multi_town = as.factor(manager_agg$dummy_sum >= 2)
  manager_agg$dummy_sum = NULL
  
  t1 = merge(t1, manager_agg, by = "manager_id")
  t2 = left_join(t2, as.data.table(manager_agg), by = "manager_id")

  return(list(t1, t2))
}

get_street_opportunity = function(t1, t2){
 
  street_price = rbind(t1[, c("street_int_id", "bedrooms", "price", "neighborhood")], t2[, c("street_int_id", "bedrooms", "price", "neighborhood")])
  
  street_agg = aggregate(price ~ street_int_id + bedrooms + neighborhood, data = street_price, FUN = median)
  colnames(street_agg)[colnames(street_agg) == "price"] = "median_price_street"
  
  # nbd_price = rbind(t1[, c("neighborhood", "bedrooms", "price")], t2[, c("neighborhood", "bedrooms", "price")])
  # nbd_agg = aggregate(price ~ neighborhood + bedrooms, data = nbd_price, FUN = median)
  # colnames(nbd_agg)[colnames(nbd_agg) == "price"] = "median_price_nbd"
  # street_agg = merge(street_agg, nbd_agg, by = c("neighborhood", "bedrooms"))
  
  nbd_agg = aggregate(median_price_street ~ neighborhood, data = street_agg, FUN = median)
  colnames(nbd_agg)[colnames(nbd_agg) == "median_price_street"] = "median_price_street_nbd"
  
  street_agg = merge(street_agg, nbd_agg, by = "neighborhood")

  street_agg$street_opportunity = (street_agg$median_price_street - street_agg$median_price_street_nbd)/street_agg$median_price_street_nbd
  street_agg$median_price_street = NULL
  street_agg$median_price_street_nbd = NULL
  
  t1 = merge(t1, street_agg, by = c("street_int_id", "bedrooms", "neighborhood"))
  t2 = merge(t2, street_agg, by = c("street_int_id", "bedrooms", "neighborhood"))
  
  return (list(t1, t2))
}

get_manager_building_count = function(t1, t2){
  
  manager_df = unique(rbind(t1[, c("manager_id", "building_id")], t2[, c("manager_id", "building_id")]))
  manager_df$mb_count = 1
  
  manager_agg = aggregate(mb_count ~ manager_id, data = manager_df, FUN = sum)
  
  t1 = merge(t1, manager_agg, by = "manager_id")
  t2 = merge(t2, as.data.table(manager_agg), by = "manager_id")

  t1$building_id = NULL
  t2$building_id = NULL
  
  return (list(t1, t2))  
}

get_manager_address_count = function(t1, t2){
  
  manager_df = unique(rbind(t1[, c("manager_id", "street_int_id")], t2[, c("manager_id", "street_int_id")]))
  manager_df$ms_count = 1
  
  manager_agg = aggregate(ms_count ~ manager_id+street_int_id, data = manager_df, FUN = sum)
  
  t1 = merge(t1, manager_agg, by = c("manager_id", "street_int_id"))
  t2 = merge(t2, as.data.table(manager_agg), by = c("manager_id", "street_int_id"))
  
  return (list(t1, t2))  
  
}

get_manager_town_opp = function(t1, t2){

  manager_df = rbind(t1[, c("manager_id", "neighborhood", "bedrooms", "price")], t2[, c("manager_id", "neighborhood", "bedrooms", "price")])
  neighborhood_df = rbind(t1[, c("neighborhood", "bedrooms", "price")], t2[, c("neighborhood", "bedrooms", "price")])
  
  manager_agg = aggregate(price ~ manager_id + neighborhood + bedrooms, data = manager_df, FUN = median)
  colnames(manager_agg)[colnames(manager_agg) == "price"] = "manager_median_price"
  
  neighborhood_agg = aggregate(price ~ neighborhood + bedrooms, data = neighborhood_df, FUN = median)
  colnames(neighborhood_agg)[colnames(neighborhood_agg) == "price"] = "neighborhood_median_price"
  
  manager_agg = merge(manager_agg, neighborhood_agg, by = c("neighborhood", "bedrooms"))
  manager_agg$mtb_opportunity = (manager_agg$manager_median_price - manager_agg$neighborhood_median_price)/manager_agg$neighborhood_median_price
  
  manager_agg$neighborhood_median_price = NULL
  manager_agg$manager_median_price = NULL
  
  t1 = merge(t1, manager_agg, by = c("manager_id", "neighborhood", "bedrooms"))
  t2 = merge(t2, as.data.table(manager_agg), by = c("manager_id", "neighborhood", "bedrooms"))
  
  t2$mtb_opportunity[is.na(t2$mtb_opportunity)] = median(t2$mtb_opportunity, na.rm = TRUE)
  
  return (list(t1, t2))
}

get_manager_scores = function(t1, t2){

  manager_df = t1[, c("manager_id", "interest_level", "price", "bedrooms", "neighborhood")]
  manager_df = cbind(manager_df, model.matrix( ~ interest_level - 1, data = manager_df))

  global_high_medium = (sum(manager_df$interest_levelhigh) + sum(manager_df$interest_levelmedium))/nrow(t1)
  global_high = sum(manager_df$interest_levelhigh)/nrow(t1)
  global_medium = sum(manager_df$interest_levelmedium)/nrow(t1)
  
  manager_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ manager_id, data = manager_df, FUN = sum)
  manager_agg$manager_count = rowSums(manager_agg[,c(2:4)])
  manager_agg$lambda = 1/(1+exp((15 - manager_agg$manager_count)*2)) 
    
  manager_df$interest_level = NULL
  manager_df$interest_levelhigh = NULL
  manager_df$interest_levellow = NULL
  manager_df$interest_levelmedium = NULL
  
  manager_df = rbind(manager_df[,c("manager_id", "price", "bedrooms")], t2[, c("manager_id", "price", "bedrooms")])
  manager_price = aggregate(price ~ manager_id + bedrooms, data = manager_df, FUN = median)
  colnames(manager_price) = c("manager_id", "bedrooms", "manager_median_price")

  # manager_df$dummy = 1
  # manager_expertise = aggregate(dummy ~ manager_id + neighborhood, data = manager_df, FUN = sum)
  # manager_expertise_df = data.frame()
  #   
  # for(i in 1:length(levels(manager_expertise$manager_id))){
  #   specific_manager_expertise = manager_expertise[manager_expertise$manager_id == levels(manager_expertise$manager_id)[i],]
  #   expert_neighborhoods = as.vector(specific_manager_expertise$neighborhood[with(specific_manager_expertise, order(-dummy))][1:3])
  #   
  #   manager_expertise_df = rbind(manager_expertise_df, as.data.frame(cbind(as.character(levels(manager_expertise$manager_id)[i]), expert_neighborhoods[1], expert_neighborhoods[2], expert_neighborhoods[3]), stringsAsFactors = FALSE))  
  #   
  # }
  # colnames(manager_expertise_df) = c("manager_id", "nbd1","nbd2","nbd3")
  # 
  # check_expert_nbd = merge(manager_df[, c("manager_id", "neighborhood")], manager_expertise_df, by = "manager_id")
  # check_expert_nbd$neighborhood = as.character(check_expert_nbd$neighborhood)
  # 
  # tf = c()
  # check_expert_nbd$expert = apply(check_expert_nbd, 1, function(x){
  # 
  #   if (length(intersect(x[["neighborhood"]], c(x[["nbd1"]], x[["nbd2"]], x[["nbd3"]]))) > 0){
  #     tf = c(tf, TRUE)
  #   }
  #   else{tf = c(tf, FALSE)}
  # 
  #   return (tf)
  # })
  # check_expert_nbd$nbd1 = NULL
  # check_expert_nbd$nbd2 = NULL
  # check_expert_nbd$nbd3 = NULL
  
  # manager_agg$popular = as.factor(manager_agg$count > 80)
  # manager_agg$premium = as.factor(manager_agg$interest_levelhigh > 30)
  # manager_agg$first_timer = as.factor(manager_agg$count == 1)

  manager_agg[, c(2:4)] = manager_agg[, c(2:4)]/manager_agg$manager_count

  # manager_agg$high_medium_score = manager_agg$lambda*(manager_agg$interest_levelhigh+manager_agg$interest_levelmedium) + (1-manager_agg$lambda)*global_high_medium
  # manager_agg$medium_score = manager_agg$lambda*manager_agg$interest_levelmedium + (1-manager_agg$lambda)*global_medium
  manager_agg$high_score = manager_agg$lambda*manager_agg$interest_levelhigh + (1-manager_agg$lambda)*global_high
  manager_agg$lambda = NULL
  
  manager_agg$manager_score = 2*manager_agg$interest_levelhigh + manager_agg$interest_levelmedium
  manager_agg$manager_score[manager_agg$manager_count < 10] = median(manager_agg$manager_score[manager_agg$manager_count >= 10])

  manager_agg$interest_levellow = NULL
  manager_agg$interest_levelhigh = NULL
  manager_agg$interest_levelmedium = NULL

  t1 = merge(t1, manager_agg, by = "manager_id")
  t1 = merge(t1, manager_price, by = c("manager_id", "bedrooms"))
  # t1 = unique(left_join(t1, check_expert_nbd, by = c("manager_id", "neighborhood")))
  # t1$price_ratio_manager_median = t1$price/t1$manager_median_price
  
  t1$manager_opportunity = (t1$price - t1$manager_median_price)/t1$manager_median_price

  # t1$manager_opportunity_pr = (t1$price_per_room - t1$manager_median_price)/t1$manager_median_price
  t1$manager_median_price = NULL
  t1$manager_id = NULL

  t2 = left_join(t2, as.data.table(manager_agg), by = "manager_id")
  t2 = left_join(t2, as.data.table(manager_price), by = c("manager_id", "bedrooms"), copy = TRUE)
  # t2 = unique(left_join(t2, check_expert_nbd, by = c("manager_id", "neighborhood"), copy = TRUE))
  
  t2_manager_table = as.data.frame(table(t2$manager_id))
  colnames(t2_manager_table) = c("manager_id", "na_manager_count")
  t2 = merge(t2, t2_manager_table, by = "manager_id")
  t2$manager_count[is.na(t2$manager_count)] = t2$na_manager_count[is.na(t2$manager_count)]
  t2$na_manager_count = NULL
  
  # t2$manager_median_price[is.na(t2$manager_median_price)] = median(t2$manager_median_price, na.rm = TRUE) 
  # t2$price_ratio_manager_median = t2$price/t2$manager_median_price

  t2$manager_opportunity = (t2$price - t2$manager_median_price)/t2$manager_median_price
  
  # t2$manager_opportunity_pr = (t2$price_per_room - t2$manager_median_price)/t2$manager_median_price
  t2$manager_median_price = NULL
  
  t2$manager_score[is.na(t2$manager_score)] = median(t2$manager_score, na.rm = TRUE) #Leave it as NA and try!
  t2$high_score[is.na(t2$high_score)] = global_high
  # t2$medium_score[is.na(t2$medium_score)] = global_medium #Leave it as NA and try!
  # t2$high_medium_score[is.na(t2$high_medium_score)] = global_high_medium
  
  t2$manager_id = NULL

  return(list(t1, t2))
 
}

get_building_scores = function(t1, t2){

  building_df = t1[, c("building_id", "interest_level")]
  building_df = cbind(building_df, model.matrix( ~ interest_level - 1, data = building_df))
  
  global_high = sum(building_df$interest_levelhigh)/nrow(t1)
  global_high_medium = sum(building_df$interest_levelhigh + building_df$interest_levelmedium)/nrow(t1)
  
  building_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ building_id, data = building_df, FUN = sum)
  building_agg$building_count = rowSums(building_agg[,c(2:4)])
  building_agg$lambda = 1/(1+exp((2 - building_agg$building_count)*2)) 
  
  # building_agg$building_popular = as.factor(building_agg$count > 60)
  # building_agg$building_popular[building_agg$building_id == 0] = 0
  # building_agg$building_premium = as.factor(building_agg$interest_levelhigh > 15)
  # building_agg$building_premium[building_agg$building_id == 0] = 0

  building_agg[, c(2:4)] = building_agg[, c(2:4)]/building_agg$building_count
  
  # building_agg$building_score = 2*building_agg$interest_levelhigh + building_agg$interest_levelmedium
  # building_agg$building_score[building_agg$count < 20] = 0
  
  building_agg$building_high_score = building_agg$lambda*building_agg$interest_levelhigh + (1-building_agg$lambda)*global_high
  building_agg$building_high_medium_score = building_agg$lambda*(building_agg$interest_levelhigh + building_agg$interest_levelmedium) + (1-building_agg$lambda)*global_high_medium
  
  building_agg$building_high_score[building_agg$building_id == 0] = global_high
  building_agg$building_high_medium_score[building_agg$building_id == 0] = global_high_medium
  
  building_agg$lambda = NULL

  building_agg$interest_levellow = NULL
  building_agg$interest_levelhigh = NULL
  building_agg$interest_levelmedium = NULL

  t1 = merge(t1, building_agg, by = "building_id")
  # t1$building_score[t1$zero_building_id == TRUE] = NA
  # t1$building_id = NULL

  t2 = left_join(t2, as.data.table(building_agg), by = "building_id")
  t2$building_count[is.na(t2$building_count)] = 1
  t2$building_high_score[is.na(t2$building_high_score)] = global_high
  t2$building_high_medium_score[is.na(t2$building_high_medium_score)] = global_high_medium
  
  # t2$building_score[is.na(t2$building_score)] = median(t2$building_score, na.rm = TRUE)
  # t2$building_id = NULL
  
  # building_df = t1[, c("building_id", "bedrooms", "price")]
  # building_price = aggregate(price ~ building_id + bedrooms, data = building_df, FUN = median )
  # 
  # colnames(building_price) = c( "building_id", "bedrooms", "building_median_price")
  # 
  # t1 = merge(t1, building_price, by = c("building_id", "bedrooms"))
  # t1$building_opportunity = (t1$price - t1$building_median_price)/t1$building_median_price
  # t1$building_opportunity[t1$zero_building_id == TRUE] = NA
  # t1$building_median_price = NULL
  # 
  # t2 = left_join(t2, data.table(building_price), by = c("building_id", "bedrooms"), copy = TRUE)
  # t2$building_opportunity = (t2$price - t2$building_median_price)/t2$building_median_price
  # t2$building_opportunity[t2$zero_building_id == TRUE] = NA
  # t2$building_median_price = NULL
  # 
  # t1$building_id = NULL
  # t2$building_id = NULL

  return(list(t1,t2))
}

get_street_scores = function(t1, t2){
  
  street_price = rbind(t1[, c("street_int_id", "bedrooms", "price")], t2[, c("street_int_id", "bedrooms", "price")])
  
  street_agg = aggregate(price ~ street_int_id + bedrooms, data = street_price, FUN = median)
  colnames(street_agg)[colnames(street_agg) == "price"] = "median_price_street"

  street_df = t1[, c("street_int_id", "interest_level")]
  street_df = cbind(street_df, model.matrix( ~ interest_level - 1, data = street_df))
  
  global_high_medium = (sum(street_df$interest_levelhigh) + sum(street_df$interest_levelmedium))/nrow(t1)
  global_high = sum(street_df$interest_levelhigh)/nrow(t1)
  
  street_agg_score = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ street_int_id, data = street_df, FUN = sum)
  street_agg_score$street_count = rowSums(street_agg_score[,c(2:4)])
  street_agg_score$lambda = 1/(1+exp((15 - street_agg_score$street_count)*2)) 
  
  street_agg_score[, c(2:4)] = street_agg_score[, c(2:4)]/street_agg_score$street_count
  
  street_agg_score$high_medium_street_score = street_agg_score$lambda*(street_agg_score$interest_levelhigh+street_agg_score$interest_levelmedium) + (1-street_agg_score$lambda)*global_high_medium
  street_agg_score$high_street_score = street_agg_score$lambda*street_agg_score$interest_levelhigh + (1-street_agg_score$lambda)*global_high

  street_agg_score$lambda = NULL
  street_agg_score$interest_levellow = NULL
  street_agg_score$interest_levelhigh = NULL 
  street_agg_score$interest_levelmedium = NULL
  street_agg_score$interest_level = NULL
  
  t1 = merge(t1, street_agg, by = c("street_int_id", "bedrooms"))
  t1$street_opportunity = (t1$price - t1$median_price_street)/t1$median_price_street
  t1$median_price_street = NULL
  t1 = merge(t1, street_agg_score, by = c("street_int_id"))
  
  t2 = left_join(t2, as.data.table(street_agg), by = c("street_int_id", "bedrooms"))

  t2$street_opportunity = (t2$price - t2$median_price_street)/t2$median_price_street
  t2$median_price_street = NULL
  t2 = merge(t2, street_agg_score, by = c("street_int_id"))
  t2$high_street_score[is.na(t2$high_street_score)] = global_high
  t2$high_medium_street_score[is.na(t2$high_medium_street_score)] = global_high_medium
  
  
  # street_df = t1[, c("street_int_id", "interest_level")]
  # street_df = cbind(street_df, model.matrix( ~ interest_level - 1, data = street_df))
  # street_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ street_int_id, data = street_df, FUN = sum)
  # 
  # street_agg$count = rowSums(street_agg[,c(2:4)])
  # street_agg[, c(2:4)] = street_agg[, c(2:4)]/street_agg$count
  # street_agg$street_score = 2*street_agg$interest_levelhigh + street_agg$interest_levelmedium
  # street_agg$street_score[street_agg$count < 10] = median(street_agg$street_score[street_agg$count >= 10])
  # 
  # street_agg$interest_levellow = NULL
  # street_agg$interest_levelhigh = NULL
  # street_agg$interest_levelmedium = NULL
  # street_agg$count = NULL
  # 
  # t1 = merge(t1, street_agg, by = "street_int_id")
  # 
  # t2 = left_join(t2, as.data.table(street_agg), by = "street_int_id")
  # t2$street_score[is.na(t2$street_score)] = median(t2$street_score, na.rm = TRUE)
  
  return(list(t1, t2))
}

get_town_opportunity = function(t1, t2){
  
  town_price = rbind(t1[, c("town", "bedrooms", "price")], t2[, c("town", "bedrooms", "price")])
  
  town_agg = aggregate(price ~ town + bedrooms, data = town_price, FUN = median)
  colnames(town_agg)[colnames(town_agg) == "price"] = "median_price_town"
  
  t1 = left_join(t1, as.data.table(town_agg), by = c("town", "bedrooms"))
  t1$town_opportunity = (t1$price - t1$median_price_town)/t1$median_price_town
  t1$median_price_town = NULL
  
  t2 = left_join(t2, as.data.table(town_agg), by = c("town", "bedrooms"))
  t2$town_opportunity = (t2$price - t2$median_price_town)/t2$median_price_town
  t2$median_price_town = NULL
  
  return (list(t1, t2))  
}

get_nbd_scores = function(t1, t2){

  nbd_price = rbind(t1[, c("neighborhood", "bedrooms", "price")], t2[, c("neighborhood", "bedrooms", "price")])
  
  nbd_agg = aggregate(price ~ neighborhood + bedrooms, data = nbd_price, FUN = median)
  colnames(nbd_agg)[colnames(nbd_agg) == "price"] = "median_price_nbd"
  
  t1 = merge(t1, nbd_agg, by = c("neighborhood", "bedrooms"))
  t1$nbd_opportunity = (t1$price - t1$median_price_nbd)/t1$median_price_nbd
  t1$median_price_nbd = NULL
  
  t2 = left_join(t2, as.data.table(nbd_agg), by = c("neighborhood", "bedrooms"))
  t2$nbd_opportunity = (t2$price - t2$median_price_nbd)/t2$median_price_nbd
  t2$median_price_nbd = NULL
  # t2$price_diff_from_median[!is.na(t2$median_price_nbd)] = t2$price[!is.na(t2$median_price_nbd)] - t2$median_price_nbd[!is.na(t2$median_price_nbd)]
  # t2$price_diff_from_median[is.na(t2$median_price_nbd)] = 0
  # t2$price_ratio_with_median[!is.na(t2$median_price_nbd)] = t2$price[!is.na(t2$median_price_nbd)]/t2$median_price_nbd[!is.na(t2$median_price_nbd)]
  # t2$price_ratio_with_median[is.na(t2$median_price_nbd)] = 1
  
  neighborhood_df = t1[, c("neighborhood", "interest_level")]  
  neighborhood_df = cbind(neighborhood_df, model.matrix( ~ interest_level - 1, data = neighborhood_df))
  
  neighborhood_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ neighborhood, data = neighborhood_df, FUN = sum)
  
  neighborhood_agg$count = rowSums(neighborhood_agg[,c(2:4)])

  neighborhood_agg[, c(2:4)] = neighborhood_agg[, c(2:4)]/neighborhood_agg$count
  neighborhood_agg$neighborhood_score = 2*neighborhood_agg$interest_levelhigh + neighborhood_agg$interest_levelmedium 
  neighborhood_agg$neighborhood_score[neighborhood_agg$count < 20] = median(neighborhood_agg$neighborhood_score[neighborhood_agg$count >= 20])
  
  neighborhood_agg$interest_levellow = NULL
  neighborhood_agg$interest_levelhigh = NULL
  neighborhood_agg$interest_levelmedium = NULL
  neighborhood_agg$count = NULL
  
  t1 = merge(t1, neighborhood_agg, by = "neighborhood")
  t1$neighborhood = NULL
  
  t2 = left_join(t2, as.data.table(neighborhood_agg), by = "neighborhood")
  # t2$neighborhood_score[is.na(t2$neighborhood_score)] = median(t2$neighborhood_score, na.rm = TRUE)
  t2$neighborhood = NULL
  
  return(list(t1, t2))
}

get_split_score = function(t1, feature_name){
  
  positive_split = (nrow(t1[(t1$interest_level == "high") & t1[[feature_name]] == TRUE,])/nrow(t1_train[t1_train[[feature_name]] == TRUE,]))
  
  negative_split = (nrow(t1[(t1$interest_level == "high") & t1[[feature_name]] == FALSE,])/nrow(t1_train[t1_train[[feature_name]] == FALSE,]))
  
  return (list(positive_split, negative_split))
}

get_renthop_score = function(t1, t2){
  # features in renthop score -- quality of listing - zero_photos, zero_building_id, no_fee
  # manager quality -- logged in within 24hrs, recently posted listing(last_active), expert in nbd, manager reviews, manager count(first timer), phone_number_provided(registered?)
  # street number provided
  
  zero_photo_split = get_split_score(t1, "zero_photos")
  zero_photo_split_p = zero_photo_split[[1]]
  zero_photo_split_n = zero_photo_split[[2]]
  
  zero_building_id_split = get_split_score(t1, "zero_building_id")
  zero_building_id_split_p = zero_building_id_split[[1]]
  zero_building_id_split_n = zero_building_id_split[[2]]
  
  no_fee_split = get_split_score(t1, "no_fee")
  no_fee_split_p = no_fee_split[[1]]
  no_fee_split_n = no_fee_split[[2]]
  
  # last_active_split = get_split_score(t1, "last_active")
  expert_split = get_split_score(t1, "expert")
  expert_split_p = expert_split[[1]]
  expert_split_n = expert_split[[2]]
  
  # manager_count_split = get_split_score(t1, "manager_count")
  phone_number_provided_split = get_split_score(t1, "phone_number_provided")
  phone_number_provided_split_p = phone_number_provided_split[[1]]
  phone_number_provided_split_n = phone_number_provided_split[[2]]
  
  street_number_provided_split = get_split_score(t1, "street_number_provided")
  street_number_provided_split_p = street_number_provided_split[[1]]
  street_number_provided_split_n= street_number_provided_split[[2]]

  t1$renthop_score = (as.numeric(t1$no_fee)*no_fee_split_p + mod(as.numeric(t1$no_fee)+1, 2)*no_fee_split_n +
    # as.numeric(t1$last_active > 0)*last_active_split +
    as.numeric(t1$expert)*expert_split_p + mod(as.numeric(t1$expert)+1, 2)*expert_split_n +
    # as.numeric(t1$manager_count > 1)*manager_count_split +
    (as.numeric(t1$phone_number_provided) - 1)*phone_number_provided_split_p + mod(as.numeric(t1$phone_number_provided), 2)*phone_number_provided_split_n +
    (as.numeric(t1$street_number_provided) - 1)*street_number_provided_split_p + mod(as.numeric(t1$street_number_provided), 2)*street_number_provided_split_n + 
    (as.numeric(t1$zero_photos) - 1)*zero_photo_split_p + mod(as.numeric(t1$zero_photos), 2)*zero_building_id_split_n +
    (as.numeric(t1$zero_building_id) - 1)*zero_building_id_split_p + mod(as.numeric(t1$zero_building_id), 2)*zero_building_id_split_n)/6
    
  t2$renthop_score = (as.numeric(t2$no_fee)*no_fee_split_p + mod(as.numeric(t2$no_fee)+1, 2)*no_fee_split_n +
    # as.numeric(t2$last_active > 0)*last_active_split +
    as.numeric(t2$expert)*expert_split_p + mod(as.numeric(t2$expert)+1, 2)*expert_split_n +
    # as.numeric(t2$manager_count > 1)*manager_count_split +
    (as.numeric(t2$phone_number_provided) - 1)*phone_number_provided_split_p + mod(as.numeric(t2$phone_number_provided), 2)*phone_number_provided_split_n +
    (as.numeric(t2$street_number_provided) - 1)*street_number_provided_split_p + mod(as.numeric(t2$street_number_provided), 2)*street_number_provided_split_n + 
    (as.numeric(t2$zero_photos) - 1)*zero_photo_split_p + mod(as.numeric(t2$zero_photos), 2)*zero_building_id_split_n +
    (as.numeric(t2$zero_building_id) - 1)*zero_building_id_split_p + mod(as.numeric(t2$zero_building_id), 2)*zero_building_id_split_n)/6
  
  # t1$zero_photos = NULL
  # t1$zero_building_id = NULL
  # t1$no_fee = NULL
  # t1$expert = NULL
  # t1$phone_number_provided = NULL
  # t1$street_number_provided = NULL
  # 
  # t2$zero_photos = NULL
  # t2$zero_building_id = NULL
  # t2$no_fee = NULL
  # t2$expert = NULL
  # t2$phone_number_provided = NULL
  # t2$street_number_provided = NULL
  
  return (list(t1, t2))
}

get_bulk_listing = function(t1, t2){

    hour_df = rbind(t1[, c("listing_id", "created", "manager_id")], t2[, c("listing_id", "created", "manager_id")])
    hour_df$hour_serial = as.integer(difftime(hour_df$created, min(hour_df$created), units = "hours"))
    
    hour_df$dummy = 1
    hour_serial_df = aggregate(dummy ~ hour_serial + manager_id, data = hour_df, FUN = sum)
    hour_freq_df = aggregate(dummy ~ hour_serial, data = hour_df, FUN = sum)
    
    colnames(hour_serial_df) = c("hour_serial", "manager_id", "hour_manager_freq")
    colnames(hour_freq_df) = c("hour_serial", "hour_freq")
    
    hour_serial_df = merge(hour_serial_df, hour_freq_df, by = "hour_serial")
    hour_serial_df$hour_manager_ratio = hour_serial_df$hour_manager_freq/hour_serial_df$hour_freq

    hour_df = merge(hour_df, hour_serial_df[, c("manager_id", "hour_serial", "hour_manager_ratio")], by = c("manager_id", "hour_serial"))
    
    t1 = merge(t1, hour_df[, c("listing_id", "hour_manager_ratio")], by = "listing_id")
    t2 = merge(t2, hour_df[, c("listing_id", "hour_manager_ratio")], by = "listing_id")

    return(list(t1, t2))
}

get_hour_freq = function(t1, t2){

  hour_df = rbind(t1[, c("listing_id", "created", "neighborhood")], t2[, c("listing_id", "created", "neighborhood")])
  
  hour_df$hour_serial = as.integer(difftime(hour_df$created, min(hour_df$created), units = "hours"))
  
  # hour_serial_df = as.data.frame(table(hour_df$hour_serial))
  hour_df$dummy = 1
  hour_serial_df = aggregate(dummy ~ hour_serial + neighborhood, data = hour_df, FUN = sum)
  colnames(hour_serial_df) = c("hour_serial", "neighborhood", "hour_freq")

  hour_df = merge(hour_df, hour_serial_df, by = c("neighborhood", "hour_serial"))

  t1 = merge(t1, hour_df[, c("listing_id", "hour_freq")], by = "listing_id")
  t2 = merge(t2, hour_df[, c("listing_id", "hour_freq")], by = "listing_id")
  
  return(list(t1, t2))
}

get_time_scores = function(t1, t2){

  min_created = min(min(t1$created), min(t2$created))

  t1$day_serial = as.integer(difftime(t1$created, min_created, units = "days"))
  t2$day_serial = as.integer(difftime(t2$created, min_created, units = "days"))

  time_nbd_df = rbind(t1[, c("listing_id", "created", "neighborhood", "bedrooms", "price", "day_serial")], t2[, c("listing_id", "created",  "neighborhood", "bedrooms", "price", "day_serial")])

  day_median_price = aggregate(price ~ neighborhood + bedrooms + day_serial, data = time_nbd_df, FUN = median)
  colnames(day_median_price)[colnames(day_median_price) == "price"] = "day_median_price"

  t1 = merge(t1, day_median_price, by = c("neighborhood", "bedrooms", "day_serial"))
  t2 = merge(t2, day_median_price, by = c("neighborhood", "bedrooms", "day_serial"))

  t1$day_opportunity = (t1$price - t1$day_median_price)/t1$day_median_price
  t1$day_median_price = NULL
  t1$day_serial = NULL
  t2$day_opportunity = (t2$price - t2$day_median_price)/t2$day_median_price
  t2$day_median_price = NULL
  t2$day_serial = NULL
  
  # t1$hour = lubridate::hour(t1$created)
  # t2$hour = lubridate::hour(t2$created)
  #
  # hour_df = t1[, c("hour", "interest_level")]
  # hour_df = cbind(hour_df, model.matrix( ~ interest_level - 1, data = hour_df))
  #
  # hour_agg = aggregate(cbind(interest_levelhigh, interest_levelmedium, interest_levellow) ~ hour, data = hour_df, FUN = sum)
  # hour_agg$hour_count = rowSums(hour_agg[,c(2:4)])
  #
  # hour_agg[, c(2:4)] = hour_agg[, c(2:4)]/hour_agg$hour_count
  # hour_agg$hour_score = 2*hour_agg$interest_levelhigh + hour_agg$interest_levelmedium
  # hour_agg$interest_levelhigh = NULL
  # hour_agg$interest_levelmedium = NULL
  # hour_agg$interest_levellow = NULL
  # hour_agg$hour_count = NULL
  # hour_agg$interest_level = NULL
  #
  # t1 = merge(t1, hour_agg, by = "hour")
  # t2 = merge(t2, hour_agg, by = "hour")
  #
  # t1$hour = NULL
  # t2$hour = NULL
  
  return (list(t1, t2))
}

get_specialized_mangers = function(t1, t2){
  
  nbd_manager = rbind(t1[, c("listing_id", "town", "manager_id")], t2[, c("listing_id", "town", "manager_id")])
  nbd_manager$dummy = 1
  
  nbd_specialized = aggregate(dummy ~ town + manager_id, data = nbd_manager, FUN = sum)
  colnames(nbd_specialized)[colnames(nbd_specialized) == "dummy"] = "nbd_manager_count"
  
  t1 = merge(t1, nbd_specialized, by = c("town", "manager_id"))
  t2 = merge(t2, nbd_specialized, by = c("town", "manager_id"))

  return (list(t1, t2))  
}

get_bedroom_opportunity = function(t1, t2){
  
  bedroom_price = rbind(t1[, c("bedrooms", "bathrooms", "price")], t2[, c("bedrooms", "bathrooms", "price")])
  
  bedroom_agg = aggregate(price ~ bedrooms + bathrooms, data = bedroom_price, FUN = median)
  colnames(bedroom_agg)[colnames(bedroom_agg) == "price"] = "median_price_bedroom"
  
  t1 = merge(t1, bedroom_agg, by = c("bedrooms", "bathrooms"))
  t1$bedroom_opportunity = (t1$price - t1$median_price_bedroom)/t1$median_price_bedroom
  t1$median_price_bedroom = NULL
  
  t2 = left_join(t2, as.data.table(bedroom_agg), by = c("bedrooms", "bathrooms"))

  t2$bedroom_opportunity = (t2$price - t2$median_price_bedroom)/t2$median_price_bedroom
  t2$median_price_bedroom = NULL
    
  return (list(t1, t2))
}

get_listing_outliers = function(t1, t2){
  t1$yday = lubridate::yday(t1$created)
  t2$yday = lubridate::yday(t2$created)
  listing_df = rbind(t1[, c("listing_id", "yday")], t2[, c("listing_id", "yday")])
  
  # listing_df$normal_limit = 0
  # for (i in 1:length(unique(listing_df$yday))){
  #   yday = unique(listing_df$yday)[i]
  #   
  #   normal_limit = as.numeric(quantile(listing_df$listing_id[listing_df$yday == yday], 0.9))
  #   
  #   listing_df$normal_limit[listing_df$yday == yday] = normal_limit
  # }
  
  # listing_df$outlier = as.factor(listing_df$listing_id > listing_df$normal_limit)
  median_listing = aggregate(listing_id ~ yday, data = listing_df, FUN = median)
  
  colnames(median_listing) = c("yday", "median_listing")
  listing_df = merge(listing_df, median_listing, by = "yday")
  
  listing_df$outlier = (listing_df$listing_id - listing_df$median_listing)/listing_df$median_listing
  listing_df$median_listing = NULL
  
  t1 = merge(t1, listing_df[, c("listing_id", "outlier")], by = "listing_id")
  t2 = merge(t2, listing_df[, c("listing_id", "outlier")], by = "listing_id")
  t1$yday = NULL
  t2$yday = NULL
  return (list(t1, t2)) 
}

validate_gbm = function(t1){
  set.seed(101) 
  
  t1$street_int_id = as.integer(as.factor(t1$display_address))
  
  street_count_df = as.data.frame(table(as.factor(t1$street_int_id)))
  colnames(street_count_df) = c("street_int_id", "street_count")
  street_count_df$street_int_id = as.integer(street_count_df$street_int_id)
  
  t1 = merge(t1, street_count_df, by = "street_int_id")
  # t1$street_int_id = NULL
  t1$display_address = NULL
  
  t1$manager_int_id = as.integer(as.factor(t1$manager_id))
  
  sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]
  
  # gbm_tuning(t1_train, t1_test)
  
  # t1_train = get_last_active(t1_train)
  # t1_test = get_last_active(t1_test)

  # building_res = get_building_scores(t1_train, t1_test)
  # t1_train = building_res[[1]]
  # t1_test = building_res[[2]]

  # time_res = get_time_scores(t1_train, t1_test)
  # t1_train = time_res[[1]]
  # t1_test = time_res[[2]]
  
  # town_res = get_town_scores(t1_train, t1_test)
  # t1_train = town_res[[1]]
  # t1_test = town_res[[2]]
  
  nbd_manager_res = get_specialized_mangers(t1_train, t1_test) #Manager nbd count
  t1_train = nbd_manager_res[[1]]
  t1_test = nbd_manager_res[[2]]
  
  # bulk_res = get_bulk_listing(t1_train, t1_test)
  # t1_train = bulk_res[[1]]
  # t1_test = bulk_res[[2]]

  street_res = get_street_opportunity(t1_train, t1_test) #JBN road in Indiranagar
  t1_train = street_res[[1]]
  t1_test = street_res[[2]]

  # multi_town_res = get_multi_town(t1_train, t1_test)
  # t1_train = multi_town_res[[1]]
  # t1_test = multi_town_res[[2]]

  mtb_opp_res = get_manager_town_opp(t1_train, t1_test)
  t1_train = mtb_opp_res[[1]]
  t1_test = mtb_opp_res[[2]]
  
  mb_count_res = get_manager_building_count(t1_train, t1_test)
  t1_train = mb_count_res[[1]]
  t1_test = mb_count_res[[2]]
  
  # ms_count_res = get_manager_address_count(t1_train, t1_test)
  # t1_train = ms_count_res[[1]]
  # t1_test = ms_count_res[[2]]

  manager_res = get_manager_scores(t1_train, t1_test)
  t1_train = manager_res[[1]]
  t1_test = manager_res[[2]]
  
  hour_res = get_hour_freq(t1_train, t1_test)
  t1_train = hour_res[[1]]
  t1_test = hour_res[[2]]
  
  nbd_res = get_nbd_scores(t1_train, t1_test)
  t1_train = nbd_res[[1]]
  t1_test = nbd_res[[2]]

  # town_res = get_town_opportunity(t1_train, t1_test)
  # t1_train = town_res[[1]]
  # t1_test = town_res[[2]]
  
  # bedroom_res = get_bedroom_opportunity(t1_train, t1_test)
  # t1_train = bedroom_res[[1]]
  # t1_test = bedroom_res[[2]]

  listing_res = get_listing_outliers(t1_train, t1_test)  
  t1_train = listing_res[[1]]
  t1_test = listing_res[[2]]
  
  # renthop_res = get_renthop_score(t1_train, t1_test)
  # t1_train = renthop_res[[1]]
  # t1_test = renthop_res[[2]]

  t1_train$town = NULL
  t1_train$street_type = NULL
  
  res_val = gbm_h2o(t1_train, t1_test)
  # dl_val = dl_h2o(t1_train, t1_test)
  
  print(MLmetrics::ConfusionMatrix(y_pred = res_val$predict, y_true = t1_test$interest_level))
  print(MultiLogLoss(y_true = t1_test$interest_level, y_pred = as.matrix(res_val[,c("high", "low", "medium")])))

  return (res_val[, c("high", "low", "medium")])
  
}

set_xgb = function(t1, t2){
  
  street_address_df = rbind(t1[, c("listing_id", "display_address")], t2[, c("listing_id", "display_address")])
  
  street_address_df$street_int_id = as.integer(as.factor(street_address_df$display_address))
  street_count_df = as.data.frame(table(as.factor(street_address_df$street_int_id)))
  colnames(street_count_df) = c("street_int_id", "street_count")
  street_count_df$street_int_id = as.integer(street_count_df$street_int_id)
  
  street_address_df = merge(street_address_df, street_count_df, by = "street_int_id")
  
  t1 = merge(t1, street_address_df[, c("listing_id", "street_count", "street_int_id")], by = "listing_id")
  t2 = merge(t2, street_address_df[, c("listing_id", "street_count", "street_int_id")], by = "listing_id")
  
  t1$display_address = NULL
  t2$display_address = NULL
  
  manager_int_df = rbind(t1[, c("listing_id", "manager_id")], t2[, c("listing_id", "manager_id")])
  manager_int_df$manager_int_id = as.integer(as.factor(manager_int_df$manager_id))
  t1 = left_join(t1, manager_int_df[, c("listing_id", "manager_int_id")], by = "listing_id")
  t2 = left_join(t2, manager_int_df[, c("listing_id", "manager_int_id")], by = "listing_id")
  
  # t1 = get_last_active(t1)
  # t2 = get_last_active(t2)
  
  nbd_manager_res = get_specialized_mangers(t1, t2)
  t1 = nbd_manager_res[[1]]
  t2 = nbd_manager_res[[2]]
  
  street_res = get_street_opportunity(t1, t2)
  t1 = street_res[[1]]
  t2 = street_res[[2]]
  
  mtb_opp_res = get_manager_town_opp(t1, t2)
  t1 = mtb_opp_res[[1]]
  t2 = mtb_opp_res[[2]]
  
  manager_res = get_manager_scores(t1, t2)
  t1 = manager_res[[1]]
  t2 = manager_res[[2]]
  
  hour_res = get_hour_freq(t1, t2)
  t1 = hour_res[[1]]
  t2 = hour_res[[2]]
  
  nbd_res = get_nbd_scores(t1, t2)
  t1 = nbd_res[[1]]
  t2 = nbd_res[[2]]
  
  listing_res = get_listing_outliers(t1, t2) 
  t1 = listing_res[[1]]
  t2 = listing_res[[2]]
  
  t1$neighborhood = NULL
  t2$neighborhood = NULL
  
  t1$building_id = NULL
  t2$building_id = NULL
  
  t1$manager_id = NULL
  t2$manager_id = NULL
  
  t1$created = NULL
  t2$created = NULL
  
  t1$street_type = NULL
  t2$street_type = NULL
  
  t1$town = NULL
  t2$town = NULL
  
  t1$wday = NULL
  t2$wday = NULL
  
  t1_y = get_train_y(t1)

  t1 = xgb(t1, 1)
  t2 = xgb(t2, 0)
  
  dtrain = xgb.DMatrix(as.matrix(t1), label=t1_y)
  dval = xgb.DMatrix(as.matrix(t2))

  num_seeds = 10
  seeds = as.integer(runif(num_seeds,0,1000))
  
  for (i in 1:num_seeds){
    
    xgb_params = list(
      booster="gbtree",
      nthread=13,
      colsample_bytree = 0.5,
      subsample = 0.7,
      eta = 0.05,
      objective = 'multi:softprob',
      max_depth = 6,
      min_child_weight = 10,
      eval_metric= "mlogloss",
      num_class = 3,
      max_delta_step = 2,
      seed = seeds[i]
    )
    
    #perform training
    gbdt = xgb.train(params = xgb_params,
                     data = dtrain,
                     nrounds = 500,
                     watchlist = list(train = dtrain),
                     print_every_n = 25,
                     early_stopping_rounds=50)
    
    # model <- xgb.dump(gbdt, with_stats = T)
    # names <- dimnames(data.matrix(t1[,-1]))[[2]]
    # importance_matrix <- xgb.importance(names, model = gbdt)
    # xgb.plot.importance(importance_matrix)
    
    pred_df =  (as.data.frame(matrix(predict(gbdt,dval), nrow=dim(t2), byrow=TRUE)))
    
    if (i == 1){
      sum_pred_df = pred_df
    }      
    else{
      sum_pred_df = sum_pred_df + pred_df
    }
  }
  
  sum_pred_df = sum_pred_df/num_seeds
  
  pred_df = cbind(t2$listing_id, sum_pred_df)
  
  colnames(pred_df) = c("listing_id", "low", "medium", "high")
  write.csv(pred_df, "xgb_4.csv", row.names = FALSE)
  
  return(pred_df)
  
}

validate_xgb = function(t1){
  set.seed(101) 
  
  t1$street_int_id = as.integer(as.factor(t1$display_address))

  street_count_df = as.data.frame(table(as.factor(t1$street_int_id)))
  colnames(street_count_df) = c("street_int_id", "street_count")
  street_count_df$street_int_id = as.integer(street_count_df$street_int_id)

  t1 = merge(t1, street_count_df, by = "street_int_id")
  t1$display_address = NULL
  
  t1$manager_int_id = as.integer(as.factor(t1$manager_id))
  
  # sample <- sample.int(nrow(t1), floor(.75*nrow(t1)), replace = F)
  sample <- createDataPartition(t1$interest_level, p = .75, list = FALSE)
  
  t1_train <- t1[sample, ]
  t1_test <- t1[-sample, ]

  # t1_train = get_last_active(t1_train)
  # t1_test = get_last_active(t1_test)
 
  # time_res = get_time_scores(t1_train, t1_test)
  # t1_train = time_res[[1]]
  # t1_test = time_res[[2]]
   
  nbd_manager_res = get_specialized_mangers(t1_train, t1_test) #Manager nbd count
  t1_train = nbd_manager_res[[1]]
  t1_test = nbd_manager_res[[2]]
 
  street_res = get_street_opportunity(t1_train, t1_test) #JBN road in Indiranagar
  t1_train = street_res[[1]]
  t1_test = street_res[[2]]
   
  mtb_opp_res = get_manager_town_opp(t1_train, t1_test)
  t1_train = mtb_opp_res[[1]]
  t1_test = mtb_opp_res[[2]]

  mb_count_res = get_manager_building_count(t1_train, t1_test)
  t1_train = mb_count_res[[1]]
  t1_test = mb_count_res[[2]]
  
  hour_res = get_hour_freq(t1_train, t1_test)
  t1_train = hour_res[[1]]
  t1_test = hour_res[[2]]

  # bulk_res = get_bulk_listing(t1_train, t1_test)
  # t1_train = bulk_res[[1]]
  # t1_test = bulk_res[[2]]
  
  manager_res = get_manager_scores(t1_train, t1_test)
  t1_train = manager_res[[1]]
  t1_test = manager_res[[2]]

  nbd_res = get_nbd_scores(t1_train, t1_test)
  t1_train = nbd_res[[1]]
  t1_test = nbd_res[[2]]

  # bedroom_res = get_bedroom_opportunity(t1_train, t1_test)
  # t1_train = bedroom_res[[1]]
  # t1_test = bedroom_res[[2]]
   
  listing_res = get_listing_outliers(t1_train, t1_test)
  t1_train = listing_res[[1]]
  t1_test = listing_res[[2]]

  t1_train$neighborhood = NULL
  t1_test$neighborhood = NULL

  t1_train$building_id = NULL
  t1_test$building_id = NULL

  t1_train$manager_id = NULL
  t1_test$manager_id = NULL
  
  t1_train$town = NULL
  t1_test$town = NULL

  t1_train$street_type = NULL
  t1_test$street_type = NULL
  
  t1_train$created = NULL
  t1_test$created = NULL
  
  t1_train$wday = NULL
  t1_test$wday = NULL

  train_y_train = get_train_y(t1_train)
  train_y_val = get_train_y(t1_test)
  
  t1_train = xgb(t1_train, 1)
  t1_test = xgb(t1_test, 1)

  pred_df_val = run_xgb(t1_train, train_y_train, t1_test, train_y_val)
  
  train_y_val[train_y_val == 0] = "low"
  train_y_val[train_y_val == 1] = "medium"
  train_y_val[train_y_val == 2] = "high"
  train_y_val = as.factor(train_y_val)
  
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

validate_stacking = function(s, s_label, s_test){
  
  xgb_params = list(
    booster="gbtree",
    nthread=13,
    colsample_bytree = 0.5,
    subsample = 0.7,
    # colsample_bylevel = 0.6,
    eta = 0.05,
    objective = 'multi:softprob',
    max_depth = 6,
    min_child_weight = 10,
    eval_metric= "mlogloss",
    num_class = 3,
    max_delta_step = 2,
    seed = seed
  )
  
  sample <- sample.int(nrow(s), floor(.5*nrow(s)), replace = F)
  # sample <- createDataPartition(s$interest_level, p = .5, list = FALSE)
  
  s1 <- s[sample, ]
  s2 <- s[-sample, ]
  
  s1_label = s_label[sample]
  s2_label = s_label[-sample]
  
  dtrain_s1 = xgb.DMatrix(as.matrix(s1), label=s1_label)
  dtrain_s2 = xgb.DMatrix(as.matrix(s2), label=s2_label)

  dtrain_s3 = xgb.DMatrix(as.matrix(s_test))
  
  level1_s1 = xgb.train(params = xgb_params,
                   data = dtrain_s1,
                   nrounds = 2000,
                   watchlist = list(train = dtrain_s1, val = dtrain_s2),
                   print_every_n = 25,
                   early_stopping_rounds=50)
  
  level1_s1_pred =  (as.data.frame(matrix(predict(level1_s1, dtrain_s2), nrow=dim(s2), byrow=TRUE)))
  colnames(level1_s1_pred) = c("low", "medium", "high")

  level1_s1s3 = xgb.train(params = xgb_params,
                        data = dtrain_s1,
                        nrounds = 300,
                        watchlist = list(train = dtrain_s1),
                        print_every_n = 25,
                        early_stopping_rounds=50)
  
  level1_s1s3_pred =  (as.data.frame(matrix(predict(level1_s1s3, dtrain_s3), nrow=dim(s_test), byrow=TRUE)))
  colnames(level1_s1s3_pred) = c("low", "medium", "high")
  
  level1_s2 = xgb.train(params = xgb_params,
                        data = dtrain_s2,
                        nrounds = 2000,
                        watchlist = list(train = dtrain_s2, val = dtrain_s1),
                        print_every_n = 25,
                        early_stopping_rounds=50)
  
  level1_s2_pred =  (as.data.frame(matrix(predict(level1_s2, dtrain_s1), nrow=dim(s1), byrow=TRUE)))
  colnames(level1_s2_pred) = c("low", "medium", "high")
  
  level1_s2s3 = xgb.train(params = xgb_params,
                          data = dtrain_s2,
                          nrounds = 300,
                          watchlist = list(train = dtrain_s2),
                          print_every_n = 25,
                          early_stopping_rounds=50)
  
  level1_s2s3_pred =  (as.data.frame(matrix(predict(level1_s2s3, dtrain_s3), nrow=dim(s_test), byrow=TRUE)))
  colnames(level1_s2s3_pred) = c("low", "medium", "high")
  
  level2_s3 = (level1_s1s3_pred + level1_s2s3_pred)/2
  dtrain_s3 = xgb.DMatrix(as.matrix(level2_s3))
  
  s_df = rbind(level1_s2_pred, level1_s1_pred)
  dtrain_s = xgb.DMatrix(as.matrix(s_df), label=s_label)

  level2 = xgb.train(params = xgb_params,
                        data = dtrain_s,
                        nrounds = 500,
                        watchlist = list(train = dtrain_s),
                        print_every_n = 25,
                        early_stopping_rounds=50)
  
  level2_pred =  (as.data.frame(matrix(predict(level2, dtrain_s3), nrow=dim(level2_s3), byrow=TRUE)))
  
  pred_df = cbind(s_test$listing_id, level2_pred)
  colnames(pred_df) = c("listing_id", "low", "medium", "high")
  
  return(pred_df)
}

run_xgb = function(t1_train, train_y_train, t1_test, train_y_val){
  
  num_seeds = 1
  seeds = as.integer(runif(num_seeds,0,1000))

  for (i in 1:num_seeds){
    
      seed = seeds[i]
      
      xgb_params = list(
        booster="gbtree",
        nthread=13,
        colsample_bytree = 0.5,
        subsample = 0.7,
        # colsample_bylevel = 0.6,
        eta = 0.05,
        objective = 'multi:softprob',
        max_depth = 6,
        min_child_weight = 10,
        eval_metric= "mlogloss",
        num_class = 3,
        max_delta_step = 2,
        seed = seed
      )
      
      dtrain = xgb.DMatrix(as.matrix(t1_train), label=train_y_train)
      dval = xgb.DMatrix(as.matrix(t1_test), label=train_y_val)
      
      #perform training
      gbdt = xgb.train(params = xgb_params,
                       data = dtrain,
                       nrounds = 2000,
                       watchlist = list(train = dtrain, val=dval),
                       print_every_n = 25,
                       early_stopping_rounds=50)
    
      # model <- xgb.dump(gbdt, with_stats = T)
      # names <- dimnames(data.matrix(t1_train[,-1]))[[2]]
      # importance_matrix <- xgb.importance(names, model = gbdt)
      # xgb.plot.importance(importance_matrix)
      
      pred_df =  (as.data.frame(matrix(predict(gbdt,dval), nrow=dim(t1_test), byrow=TRUE)))
      
      if (i == 1){
        sum_pred_df = pred_df
      }      
      else{
        sum_pred_df = sum_pred_df + pred_df
      }
  }
  
  sum_pred_df = sum_pred_df/num_seeds
  
  pred_df = cbind(t1_test$listing_id, sum_pred_df)
  colnames(pred_df) = c("listing_id", "low", "medium", "high")

  return(pred_df)
  
}

t1 = generate_df(df, 1)

#class_price = aggregate(price ~ interest_level, data = t1, FUN = median)

# t1$price_ratio_high_median = t1$price/class_price$price[class_price$interest_level == "high"]
# t1$price_ratio_low_median = t1$price/class_price$price[class_price$interest_level == "low"]
# t1$price_ratio_medium_median = t1$price/class_price$price[class_price$interest_level == "medium"]

# nbd_count = aggregate(building_id ~ neighborhood, data = t1, FUN=function(x){length(unique(x))})
# colnames(nbd_count) = c("neighborhood", "building_count")
# nbd_count$building_count = as.factor(nbd_count$building_count > 10)
# t1 = merge(t1, nbd_count, by = "neighborhood")

t2 = generate_df(test, 0)

# t2$price_ratio_high_median = t2$price/class_price$price[class_price$interest_level == "high"]
# t2$price_ratio_low_median = t2$price/class_price$price[class_price$interest_level == "low"]
# t2$price_ratio_medium_median = t2$price/class_price$price[class_price$interest_level == "medium"]

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
# train_xgb = xgb(t1, 1)
# test_xgb = xgb(t2, 0)
# train_y = get_train_y(t1)
xgb_val = validate_xgb(t1)
pred_df = run_xgb(train_xgb, train_y, test_xgb)
write.csv(pred_df, "xgb_submission.csv", row.names = FALSE)

#Validation (gbm)
gbm_val = validate_gbm(t1)

street_address_df = rbind(t1[, c("listing_id", "display_address")], t2[, c("listing_id", "display_address")])

street_address_df$street_int_id = as.integer(as.factor(street_address_df$display_address))
street_count_df = as.data.frame(table(as.factor(street_address_df$street_int_id)))
colnames(street_count_df) = c("street_int_id", "street_count")
street_count_df$street_int_id = as.integer(street_count_df$street_int_id)

street_address_df = merge(street_address_df, street_count_df, by = "street_int_id")
  
t1 = merge(t1, street_address_df[, c("listing_id", "street_count", "street_int_id")], by = "listing_id")
t2 = merge(t2, street_address_df[, c("listing_id", "street_count", "street_int_id")], by = "listing_id")

# t1$street_int_id = NULL
t1$display_address = NULL
# t2$street_int_id = NULL
t2$display_address = NULL

manager_int_df = rbind(t1[, c("listing_id", "manager_id")], t2[, c("listing_id", "manager_id")])
manager_int_df$manager_int_id = as.integer(as.factor(manager_int_df$manager_id))
t1 = left_join(t1, manager_int_df[, c("listing_id", "manager_int_id")], by = "listing_id")
t2 = left_join(t2, manager_int_df[, c("listing_id", "manager_int_id")], by = "listing_id")

t1 = get_last_active(t1)
t2 = get_last_active(t2)

time_res = get_time_scores(t1, t2)
t1 = time_res[[1]]
t2 = time_res[[2]]


nbd_manager_res = get_specialized_mangers(t1, t2)
t1 = nbd_manager_res[[1]]
t2 = nbd_manager_res[[2]]

street_res = get_street_opportunity(t1, t2)
t1 = street_res[[1]]
t2 = street_res[[2]]

# multi_town_res = get_multi_town(t1, t2)
# t1 = multi_town_res[[1]]
# t2 = multi_town_res[[2]]

mtb_opp_res = get_manager_town_opp(t1, t2)
t1 = mtb_opp_res[[1]]
t2 = mtb_opp_res[[2]]

mb_count_res = get_manager_building_count(t1, t2)
t1 = mb_count_res[[1]]
t2 = mb_count_res[[2]]

ms_count_res = get_manager_address_count(t1, t2)
t1 = ms_count_res[[1]]
t2 = ms_count_res[[2]]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
manager_res = get_manager_scores(t1, t2)
t1 = manager_res[[1]]
t2 = manager_res[[2]]

hour_res = get_hour_freq(t1, t2)
t1 = hour_res[[1]]
t2 = hour_res[[2]]

nbd_res = get_nbd_scores(t1, t2)
t1 = nbd_res[[1]]
t2 = nbd_res[[2]]

town_res = get_town_opportunity(t1, t2)
t1 = town_res[[1]]
t2 = town_res[[2]]

bedroom_res = get_bedroom_opportunity(t1, t2)
t1 = bedroom_res[[1]]
t2 = bedroom_res[[2]]

listing_res = get_listing_outliers(t1, t2) 
t1 = listing_res[[1]]
t2 = listing_res[[2]]

t1$building_id = NULL
t2$building_id = NULL

pred_df_gbm = gbm_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(pred_df_gbm$high), medium = as.vector(pred_df_gbm$medium), low = as.vector(pred_df_gbm$low))
write.csv(pred, "gbm_30.csv", row.names = FALSE)

#Running RF
res = rf_h2o(t1, t2)
pred <- data.frame(listing_id = as.vector(t2$listing_id), high = as.vector(res$high), medium = as.vector(res$medium), low = as.vector(res$low))
write.csv(pred, "rf_h2o_3.csv", row.names = FALSE)

check = as.data.frame(t1$interest_level)
colnames(check) = c("interest_level")


##FIND THE HIGH FEATURES -- features whose high percentage is greater than a threshold(0.08)
# for(i in 1:length(keywords)){
#   
#    print(i)
#    V1_df = as.data.frame(cbind(grepl(keywords[i], tolower(df$description)), check$interest_level))
#    colnames(V1_df) = c("V1", "interest_level")
#    V1_df$interest_level = as.factor(V1_df$interest_level)
# 
#    V1_df = cbind(V1_df, model.matrix( ~ interest_level - 1, data = V1_df))
#    V1_agg = aggregate(cbind(interest_level1, interest_level3, interest_level2) ~ V1, data = V1_df, FUN = sum)
#    V1_agg$count = rowSums(V1_agg[,c(2:4)])
#    V1_agg[, c(2:4)] = V1_agg[, c(2:4)]/V1_agg$count
#    if (max(V1_agg$interest_level1) > 0.08){
#      print(keywords[i])
#    }
#        
# }
