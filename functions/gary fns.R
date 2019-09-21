#####################
# HELPER FUNCTIONS ##
#####################

#####################
# LABELING ##########
#####################

labeling <- function(x){
  if_else(x > 0, 1, 0)
}

#####################
# DATA PROCESSING ###
#####################

data_processing <- function(notes_training,
                            notes_test,
                            max_length,
                            max_words,
                            exclude_stop_words = FALSE,
                            seed = 1174,
                            train_validation_split,
                            label){
  
  notes_full <- notes_training %>% 
    bind_rows(notes_test)
  print(1)
  exclude_stop_words_function <- function(data){
    data(stop_words)
    
    data_with_id <- data %>% 
      mutate(sent_id = row_number())
    
    data <- data %>% 
      mutate(sent_id = row_number()) %>% 
      unnest_tokens(output = word, input = note_processed, token = 'words') %>% 
      anti_join(stop_words, by = c('word' = 'word')) %>% 
      group_by(sent_id) %>% 
      summarise(note_processed = paste(word, collapse = ' ')) %>% 
      ungroup() %>% 
      left_join(data_with_id, by = "sent_id") %>% 
      select(-note_processed.x, -sent_id) %>% 
      dplyr::rename(note_processed = note_processed.y)
    data
  }
  
  if (exclude_stop_words) {
    notes_training <- exclude_stop_words_function(notes_training)
    notes_test <- exclude_stop_words_function(notes_test)
    notes_full <- exclude_stop_words_function(notes_full)
  }
  
  texts <- notes_full %>% 
    pull(note_processed)
  print(2)
  texts_training <- notes_training %>%
    pull(note_processed)
  labels_training <- notes_training %>% 
    pull(label)
  print(3)
  texts_test <- notes_test %>% 
    pull(note_processed)
  labels_test <- notes_test %>% 
    pull(label)
  print(4)
  # TOKENIZING
  tokenizer <- text_tokenizer(num_words = max_words) %>%
    fit_text_tokenizer(texts)
  word_index = tokenizer$word_index
  print(5)
  # TRAINING
  sequences_training <- texts_to_sequences(tokenizer, texts_training)
  data_training <- pad_sequences(sequences_training, maxlen = max_length)
  labels_training <- as.array(labels_training)
  print(6)
  # TEST
  sequences_test <- texts_to_sequences(tokenizer, texts_test)
  data_test <- pad_sequences(sequences_test, maxlen = max_length)
  labels_test <- as.array(labels_test)
  print(7)
  set.seed(seed)
  training_samples <- floor(nrow(data_training) * train_validation_split)
  validation_samples <- nrow(data_training) - training_samples
  print(8)
  indices <- sample(1:nrow(data_training))
  training_indices <- indices[1:training_samples]
  validation_indices <- indices[(training_samples + 1):(training_samples + validation_samples)]
  print(9)
  x_train <- data_training[training_indices,]
  y_train <- labels_training[training_indices]
  x_val <- data_training[validation_indices,]
  y_val <- labels_training[validation_indices]
  x_test <- data_test
  y_test <- labels_test
  print(10)
  list('x_train' = x_train,
       'y_train' = y_train,
       'x_val' = x_val,
       'y_val' = y_val,
       'x_test' = x_test,
       'y_test' = y_test,
       'word_index' = word_index)
}



pull_function <- function(data, label){
  data %>% 
    pull(label)
}

################################
# DATA PROCESSING MULTILABEL ###
################################

data_processing_multilabel <- function(notes_training,
                                       notes_test,
                                       max_length,
                                       max_words,
                                       exclude_stop_words = FALSE,
                                       seed = 1174,
                                       train_validation_split) {
  
  notes_full <- notes_training %>% 
    bind_rows(notes_test)
  
  print(1)
  
  exclude_stop_words_function <- function(data){
    # Define list of stop words
    data(stop_words)
    
    data_label <- data %>% 
      select(report_no,
             with_labels,
             dyspnea, 
             chest.pain, 
             fatique, 
             nausea, 
             cough)
    
    data <- data %>% 
      unnest_tokens(output = word, input = note_processed, token = 'words') %>% 
      anti_join(stop_words, by = c('word' = 'word')) %>% 
      group_by(report_no) %>% 
      summarise(note_processed = paste(word, collapse = ' ')) %>% 
      ungroup() %>% 
      left_join(data_label, by = c("report_no"))
    data
  }
  
  if (exclude_stop_words) {
    notes_training <- exclude_stop_words_function(notes_training)
    notes_test <- exclude_stop_words_function(notes_test)
    notes_full <- exclude_stop_words_function(notes_full)
  }
  
  texts <- notes_full %>% 
    pull(note_processed)
  texts_training <- notes_training %>%
    pull(note_processed)
  labels_training <- notes_training %>% 
    select(-c(note_processed, report_no, with_labels)) %>% 
    as.matrix()
  
  print(2)
  texts_test <- notes_test %>% 
    pull(note_processed)
  labels_test <- notes_test %>%     
    select(-c(note_processed, report_no, with_labels)) %>% 
    as.matrix()
  
  print(3)
  # TOKENIZING
  tokenizer <- text_tokenizer(num_words = max_words) %>%
    fit_text_tokenizer(texts)
  word_index = tokenizer$word_index
  print(4)
  # TRAINING
  sequences_training <- texts_to_sequences(tokenizer, texts_training)
  data_training <- pad_sequences(sequences_training, maxlen = max_length)
  labels_training <- as.array(labels_training)
  print(5)
  # TEST
  sequences_test <- texts_to_sequences(tokenizer, texts_test)
  data_test <- pad_sequences(sequences_test, maxlen = max_length)
  labels_test <- as.array(labels_test)
  print(6)
  set.seed(seed)
  training_samples <- floor(nrow(data_training) * train_validation_split)
  validation_samples <- nrow(data_training) - training_samples
  print(7)
  indices <- sample(1:nrow(data_training))
  training_indices <- indices[1:training_samples]
  validation_indices <- indices[(training_samples + 1):(training_samples + validation_samples)]
  print(8)
  x_train <- data_training[training_indices,]
  y_train <- labels_training[training_indices,]
  x_val <- data_training[validation_indices,]
  y_val <- labels_training[validation_indices,]
  x_test <- data_test
  y_test <- labels_test
  print(9)
  list('x_train' = x_train,
       'y_train' = y_train,
       'x_val' = x_val,
       'y_val' = y_val,
       'x_test' = x_test,
       'y_test' = y_test,
       'word_index' = word_index)
}

#########################
# TRAIN-VALIDATE-TEST ###
#########################

train_validate_test <- function(data,
                                model, 
                                epoch,
                                batch_size,
                                model_folder){
  # Create history object
  history <- model %>% fit(
    data$x_train, 
    data$y_train,
    epochs = epoch,
    batch_size = batch_size,
    validation_data = list(data$x_val, 
                           data$y_val))
  
  # Saving model weights
  model_name <- deparse(substitute(model))
  file_path <- file.path(model_folder, 
                         paste(model_name, "h5", sep = "."))
  model %>% 
    save_model_hdf5(file_path)
  
  result <- model %>% 
    evaluate(data$x_test, data$y_test)
  
  list('history' = history,
       'result' = result)
}

#########################
# MODEL DEFINITIONS #####
#########################

# DEEP CNN MODEL #######

model_deep_covnet <- function(max_len,
                              max_features,
                              optimizer,
                              loss = "binary_crossentropy",
                              output_unit = 1){
  
  model_deep_covnet <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features, 
                    output_dim = 128,
                    input_length = max_len) %>%
    layer_conv_1d(filters = 32, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 64, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 64, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 128, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_global_max_pooling_1d() %>%
    layer_dense(units = output_unit, activation = "sigmoid")
  
  model_deep_covnet %>% compile(
    optimizer = optimizer,
    loss = loss,
    metrics = c("acc"))
  
  model_deep_covnet
  
}

# DEEP CNN MODEL WITH EMBEDDINGS #######

model_deep_covnet_embeddings <- function(max_len,
                                         max_features,
                                         optimizer,
                                         embedding_dim,
                                         word_index,
                                         embeddings_index,
                                         loss = "binary_crossentropy",
                                         output_unit = 1){
  
  embedding_matrix <- array(0, c(max_features, embedding_dim))
  
  for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index < max_features) {
      embedding_vector <- embeddings_index[[word]]
      if (!is.null(embedding_vector))
        embedding_matrix[index+1,] <- embedding_vector
    }
  }
  
  model_deep_covnet_embeddings <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features, 
                    output_dim = embedding_dim, 
                    input_length = max_len,
                    name = "embedding_1") %>%
    layer_conv_1d(filters = 32, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 64, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 64, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 128, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_global_max_pooling_1d() %>%
    layer_dense(units = output_unit, activation = "sigmoid")
  
  get_layer(model_deep_covnet_embeddings, name = "embedding_1") %>%
    set_weights(list(embedding_matrix)) %>%
    freeze_weights()
  
  model_deep_covnet_embeddings %>% compile(
    optimizer = optimizer,
    loss = loss,
    metrics = c("acc"))
  
  model_deep_covnet_embeddings
  
}

# DEEP CNN MODEL WITH BIDIRECTIONAL LSTM #######

model_deep_covnet_blstm <- function(max_len,
                                    max_features,
                                    optimizer,
                                    loss = "binary_crossentropy",
                                    output_unit = 1){
  
  model_deep_covnet_blstm <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features, 
                    output_dim = 128,
                    input_length = max_len) %>%
    layer_conv_1d(filters = 32, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 64, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 64, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_dropout(0.2) %>% 
    layer_conv_1d(filters = 128, 
                  kernel_size = 3, 
                  activation = "relu") %>%
    bidirectional(layer_lstm(units = 128)) %>%
    layer_dense(units = output_unit, activation = "sigmoid")
  
  model_deep_covnet_blstm %>% compile(
    optimizer = optimizer,
    loss = loss,
    metrics = c("acc"))
  
  model_deep_covnet_blstm
  
}

#########################
# METRICS ###############
#########################

generate_metrics <- function(model,
                             data,
                             threshold){
  y_true <- data$y_test
  x_test <- data$x_test
  y_predicted <- model %>%  
    predict(x_test)
  
  y_predicted[y_predicted < threshold] <- 0
  y_predicted[y_predicted >= threshold] <- 1
  y_predicted <- as.vector(y_predicted)
  
  accuracy <- confusionMatrix(factor(y_predicted, levels = c(0, 1)), 
                              factor(y_true, levels = c(0, 1)), 
                              positive = "1")$overall[1]
  
  precision <- posPredValue(factor(y_predicted, levels = c(0, 1)), 
                            factor(y_true, levels = c(0, 1)),
                            positive="1")
  
  recall <- sensitivity(factor(y_predicted, levels = c(0, 1)), 
                        factor(y_true, levels = c(0, 1)),
                        positive="1")
  
  F1 <- (2 * precision * recall) / (precision + recall)
  
  print(paste("Accuracy:", round(accuracy, 2)))
  print(paste("Precision:", round(precision, 2)))
  print(paste("Recall:", round(recall, 2)))
  print(paste("F1:", round(F1, 2)))
  
  list("accuracy" = accuracy,
       "precision" = precision,
       "recall" = recall,
       "F1" = F1)
}

get_optimal_threshold <- function(model,
                                  data,
                                  interval = 0.05) {
  
  y_true <- data$y_test
  x_test <- data$x_test
  y_predicted_original <- model %>%  
    predict(x_test)
  
  F1_list <- c()
  for (i in seq(0, 1, interval)) {
    
    y_predicted <- y_predicted_original
    y_predicted[y_predicted < i] <- 0
    y_predicted[y_predicted >= i] <- 1
    precision <- posPredValue(factor(y_predicted, levels = c(0, 1)), 
                              factor(y_true, levels = c(0, 1)), 
                              positive="1")
    recall <- sensitivity(factor(y_predicted, levels = c(0, 1)), 
                          factor(y_true, levels = c(0, 1)),
                          positive="1")
    F1 <- (2 * precision * recall) / (precision + recall)
    F1_list <- c(F1_list, F1)
  }
  names(F1_list) <- seq(0, 1, interval)
  optimal_F1 <- F1_list[which.max(F1_list)]
  
  list("optimal_F1" = optimal_F1,
       "optimal_threshold" = as.numeric(names(optimal_F1)))
}
