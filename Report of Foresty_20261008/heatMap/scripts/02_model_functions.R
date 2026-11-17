run_model_pipeline <- function(data_source, seed_val) {
  # 1. 設定亂數種子以確保每次抽樣可重複
  set.seed(seed_val)
  
  # 每 5km 抽一筆
  resample_combine_bb <- data_source %>%
    group_by(cell_id_5km) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  # 2. 整理觀測與 zero-fill 資料
  observations <- resample_combine_bb %>% 
    rename(Macaca_sur = count) %>% 
    filter(Macaca_sur > 0) %>% 
    mutate(species_observed = TRUE)
  
  zf <- left_join(resample_combine_bb, observations) %>%
    mutate(
      species_observed = coalesce(species_observed, FALSE),
      observation_count = count
    )
  
  zf_filtered <- zf
  zf_filtered$type <- if_else(runif(nrow(zf_filtered)) <= 0.8, "train", "test")
  
  # 3. 時空子抽樣
  zf_filtered <- zf_filtered %>%
    bind_cols(
      zf_filtered %>% 
        st_as_sf(coords = c("X_wgs84", "Y_wgs84"), crs = 4326) %>% 
        st_coordinates() %>% 
        as.data.frame() %>% 
        setNames(c("longitude", "latitude"))
    ) %>% 
    mutate(year = Year)
  
  checklists_ss <- grid_sample_stratified(
    zf_filtered,
    res = c(1000, 1000, 7),
    obs_column = "species_observed",
    by_year = TRUE,
    case_control = TRUE,
    sample_by = "type"
  )
  
  # 4. 準備訓練集
  checklists_train <- checklists_ss %>%
    filter(type == "train") %>%
    select(species_observed, observation_count, year, day_of_year, effort_n, -cell_id, `Bio01`:`PROAD`)
  
  detection_freq <- mean(checklists_train$species_observed)
  n_detections <- sum(checklists_train$species_observed)
  
  # 5. 訓練分佈範圍模型 (Range RF)
  train_er <- checklists_train %>% ungroup() %>% select(`species_observed`:`PROAD`, -observation_count)
  train_er$species_observed <- factor(train_er$species_observed)
  rf_range <- ranger(
    formula = species_observed ~ .,
    data = train_er,
    importance = "impurity",
    sample.fraction = c(detection_freq, detection_freq)
  )
  
  # 6. 訓練遭遇率模型 (Encounter Rate RF)
  rf_er <- ranger(
    formula = species_observed ~ .,
    data = train_er,
    importance = "impurity",
    probability = TRUE,
    sample.fraction = c(detection_freq, detection_freq),
    min.node.size = ceiling(n_detections * 0.02)
  )
  
  # 7. 模型校準 (Calibration)
  er_pred <- rf_er$predictions[, "TRUE"]
  det_obs <- as.integer(checklists_train$species_observed == "TRUE")
  obs_pred <- data.frame(obs = det_obs, pred = er_pred)
  
  calibration_model <- scam(
    obs ~ s(pred, k = 6, bs = "mpi"),
    gamma = 2,
    data = obs_pred
  )
  
  # 8. 訓練數量模型 (Count RF)
  train_count <- checklists_train %>%
    filter(!is.na(observation_count), species_observed) %>%
    ungroup() %>%
    select(-species_observed)
  
  predicted_er <- predict(rf_er, data = train_count, type = "response")$predictions[, "TRUE"]
  train_count$predicted_er <- predicted_er
  
  rf_count <- ranger(
    formula = observation_count ~ .,
    data = train_count,
    importance = "impurity"
  )
  
  return(list(
    rf_range = rf_range,
    rf_er = rf_er,
    calibration_model = calibration_model,
    rf_count = rf_count
  ))
}