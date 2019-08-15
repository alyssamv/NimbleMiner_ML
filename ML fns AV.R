#############################################################################################################
# 6. Get precision, recall, and F-score for ML prediction
#############################################################################################################
get_measures <- function(labeled_data, predicted_data, model_type, category = NULL){
  
  prevalence = sum(as.numeric(labeled_data$Label)) / nrow(labeled_data)
  
  label = paste0(model_type, "_LABEL")
  
  pred = factor(as.numeric(predicted_data[[label]]), levels = c(0,1))
  
  true_labels = predicted_data$chunk_cats
    #select(contains("cat"), contains("copy"))
  
  if (is.null(category)) {
    truth = as.numeric(!is.na(true_labels))
  } else {
    truth = as.numeric(grepl(category, true_labels))
  }
  # truth = c()
  # if (is.null(category)) {
  #   for (i in 1:nrow(true_labels)) {
  #     truth[i] = ifelse(!is.na(true_labels), 1, 0) 
  #     #truth[i] = ifelse(TRUE %in% (!is.na(true_labels[i, ])), 1, 0)
  #   }
  # } else {
  #   for (i in 1:nrow(true_labels)) {
  #     truth[i] = ifelse(grepl(category, true_labels) == TRUE, 1, 0) #truth[i] = ifelse(TRUE %in% (category %in% true_labels[i, ]), 1, 0)
  #   }
  # }
  
  n = sum(truth)
  truth = as.factor(truth)
  
  recall = caret::recall(pred, truth, relevant = "1")
  prec = caret::precision(pred, truth, relevant = "1")
  f = caret::F_meas(pred, truth, relevant = "1")
  n.label = sum(as.numeric(predicted_data[[label]]))
  
  return(list(prevalence = prevalence, n.true = n, n.labeled = n.label, Recall = recall, Precision = prec, F_measure = f))
}


#############################################################################################################
# 6. Assigning labels
#############################################################################################################
#############################################################################################################
#  Function getCategoriesList - return vector of all categories 
#############################################################################################################
getCategoriesList <- function(){
  filename <- paste0(app_dir, "simclins_tree.csv")
  simclins_tree_json <- readLines(filename, encoding = "UTF-8")
  simclins_tree_df <- jsonlite::fromJSON(simclins_tree_json)
  return(simclins_tree_df$text)
}
#############################################################################################################
# Function getColumnToUpload - define column with simclins in specified for simclins upload file
# Define column in dataframe data by name columnName (case insensitive) or by type columnTypes
#############################################################################################################    
getColumnToUpload <- function(data,columnName,columnTypes){
  colnames(data)<-tolower(colnames(data))
  columnIndex <- match (tolower(columnName),colnames(data),nomatch=0)
  if(columnIndex==0)
    columnIndex <- match(columnTypes,sapply(data, class),nomatch=0)
  return(columnIndex)
}
#############################################################################################################
#  Function assigneLabels - classification text notes from filename file by simclins list, negations list and exceptions list 
#  Save labeled data in csv files: pos_labeled_data.csv,pos_negated_labeled_data.csv,pos_negative_labeled_data.csv, pos_irrelevant_labeled_data.csv
#  and all data are saved in labeled-data-",format(Sys.time(), "%Y-%m-%d_%H-%M"),".csv
#  Simclins, negations and irrelevant terms are marked in text by html tags, spaces in ngrams are replaced by underscore sign.
#  So use these files for machine learning only with preprocessing (tags and underscore removing removing)
#############################################################################################################
assigneLabels <- function(filename, chunk_size, distance_between_simclin_and_negation = 5, df_simclins, categories_list, app_dir, df_negations = NULL, df_exceptions = NULL, df_irrelevant_terms = NULL, utf8_language = FALSE){
  
  # create pattern for regex from simclins
  # divide pattern by substrings from 50000 chars because of the limit by length for the pattern string
  pattern_limit <- 5000
  pattern_list = c()
  pattern_str <- ""
  
  df_simclins_for_search <- df_simclins[!duplicated(df_simclins$Simclins) & (df_simclins$Category %in% categories_list), ]
  
  if (nrow(df_simclins_for_search) > 0)
    for (i in 1:nrow(df_simclins_for_search)) {
      df_simclins_for_search$Simclins[i] <- gsub("\\+", "", df_simclins_for_search$Simclins[i])
      df_simclins_for_search$Simclins[i] <- gsub("_", " ", df_simclins_for_search$Simclins[i])
      pattern_str <- paste0(pattern_str,"|",df_simclins_for_search$Simclins[i],"")
      #if the length of current substring is limit over - create the new substring
      if (nchar(pattern_str) >= pattern_limit - 50) {
        pattern_str <- substr(pattern_str, 2, nchar(pattern_str))
        pattern_str <- paste0("\\b(", pattern_str, ")\\b")
        pattern_list <- c(pattern_list, pattern_str)
        pattern_str <- ""
        
      }
    }
  
  if (nchar(pattern_str) > 0) {
    pattern_str <- substr(pattern_str,2,nchar(pattern_str))
    pattern_str <- paste0("\\b(",pattern_str,")\\b")
    pattern_list <- c(pattern_list,pattern_str)
  }
  
  if (utf8_language) {
    pattern_list <- enc2utf8(pattern_list)
  }
  
  df_corpus_to_label <- filename#read.csv(filename, header = TRUE, stringsAsFactors=FALSE, comment.char = "", colClasses = "character", fileEncoding = "UTF-8")
  
  notes_column_index <- getColumnToUpload(df_corpus_to_label,"Note", c("character", "factor"))
  
  colnames(df_corpus_to_label)[notes_column_index]<-'Note'
  
  # if(input$unit_type_to_label==2){
  #   df_corpus_to_label<-subset(df_corpus_to_label, select=Note)
  #   df_allNotes<-data.frame(Note = unlist(tokenize_paragraphs(df_corpus_to_label$Note)), stringsAsFactors = F)
  #   info_to_user<-paste0(nrow(df_allNotes)," paragraphs from ",nrow(df_corpus_to_label)," notes")
  # } else if(input$unit_type_to_label==3){
  #   df_corpus_to_label<-subset(df_corpus_to_label, select=Note)
  #   df_allNotes<-data.frame(Note = unlist(tokenize_sentences(df_corpus_to_label$Note)), stringsAsFactors = F)
  #   info_to_user<-paste0(nrow(df_allNotes)," sentences from ",nrow(df_corpus_to_label)," notes")
  # } else {
  df_allNotes <- df_corpus_to_label
  info_to_user <- paste0(nrow(df_allNotes), " notes")
  # }
  
  rm(df_corpus_to_label)
  
  # pre-processing of data for labeling
  df_allNotes$Note <- tolower(df_allNotes$Note)
  # df_allNotes$Note=gsub("[[:punct:]]", " ", df_allNotes$Note) - follow to errors in Hebrew corpus
  df_allNotes$Note = removePunctuation(df_allNotes$Note, preserve_intra_word_contractions = FALSE, preserve_intra_word_dashes = FALSE, ucp = TRUE)
  
  #df_allNotes$NimbleMiner_ID <- seq.int(nrow(df_allNotes))
  
  # labeling all notes by regex with simclins
  df_allNotes$Label <- FALSE
  if(length(pattern_list)>0 & nrow(df_allNotes)>0)
    for(i in 1:length(pattern_list)){
      print(paste0("Filtering positive items from ",info_to_user," by ",i,"/",length(pattern_list)," part of simclins list from ",Sys.time()))
      
      start_time <- Sys.time()
      new_values <- grepl(pattern_list[i], df_allNotes$Note, ignore.case = TRUE)
      df_allNotes$Label <- ifelse(new_values == TRUE, TRUE,df_allNotes$Label)
      print(paste0('Duration: ', round((difftime(Sys.time(),start_time,units = "min")),2)))
      
    }
  
  write.csv(df_allNotes, file = paste0(app_dir,"raw_labeled_data_utf8.csv"), fileEncoding = "UTF-8", row.names = FALSE)
  
  
  fileName_all_labeled_data <- paste0(app_dir,chunk_size,"labeled-data-",format(Sys.time(), "%Y-%m-%d_%H-%M"),".csv")
  write.csv(df_allNotes[df_allNotes[,'Label'] == FALSE,], file = paste0(app_dir,"negative_labeled_data.csv"), fileEncoding = "UTF-8", row.names = FALSE)
  write.csv(df_allNotes[df_allNotes[,'Label'] == FALSE,], file = fileName_all_labeled_data, na = "", fileEncoding = "UTF-8", row.names = FALSE)
  
  orignal_source_colnames <- colnames(df_allNotes)
  
  df_positiveLabels <- data.frame(df_allNotes[df_allNotes[,'Label']==TRUE,],stringsAsFactors=FALSE)
  if (nrow(df_positiveLabels)>0)
    df_positiveLabels$Simclins <-""
  else df_positiveLabels$Simclins <-character(0)
  
  total_notes <- nrow(df_allNotes)
  total_negated_notes <-0
  
  #rm(df_allNotes)
  
  # get pattern for pre-negations search by categories
  
  pattern_pre_negations_list<-vector("list", length(categories_list)+1)
  names(pattern_pre_negations_list)<-c("General",categories_list)
  
  pattern_pre_negations <- ""
  df_pre_negations <- data.table::data.table(df_negations[df_negations$Type=='before',])
  
  # get pattern for general negations
  df_pre_negations_curr_cat <- df_pre_negations[df_pre_negations$Category=="General",]
  if(nrow(df_pre_negations_curr_cat)>0){
    list_pre_negations <- ""
    for(neg_indx in 1:nrow(df_pre_negations_curr_cat))
      list_pre_negations <- paste0(list_pre_negations,"|",df_pre_negations_curr_cat[neg_indx,"Negation"])
    if(nchar(list_pre_negations)>0) {
      list_pre_negations <- substring(list_pre_negations,2,nchar(list_pre_negations))
      pattern_pre_negations <- gsub("_"," ",list_pre_negations)
      if(utf8_language) pattern_pre_negations<-enc2utf8(pattern_pre_negations)
      pattern_pre_negations <- paste0("(\\b(",pattern_pre_negations,")\\b)")
    }
    pattern_pre_negations_list[['General']]<-pattern_pre_negations
  }
  
  # get patterns for all categories
  if(length(categories_list)>0)
    for(i in 1:length(categories_list)){
      pattern_pre_negations <- ""
      list_pre_negations <- ""
      df_pre_negations_curr_cat <- df_pre_negations[df_pre_negations$Category==categories_list[i],]
      
      if(nrow(df_pre_negations_curr_cat)>0){
        for(neg_indx in 1:nrow(df_pre_negations_curr_cat))
          list_pre_negations <- paste0(list_pre_negations,"|",df_pre_negations_curr_cat[neg_indx,"Negation"])
        # pattern_pre_negations <- paste0("(\\b(",list_pre_negations,gsub("_"," ",list_pre_negations),")\\b)")
        if(nchar(list_pre_negations)>0) {
          list_pre_negations <- substring(list_pre_negations,2,nchar(list_pre_negations))
          pattern_pre_negations <- gsub("_"," ",list_pre_negations)
          if(utf8_language) pattern_pre_negations<-enc2utf8(pattern_pre_negations)
          pattern_pre_negations <- paste0("(\\b(",pattern_pre_negations,")\\b)")
        }
        pattern_pre_negations_list[[categories_list[i]]] <- pattern_pre_negations
      }
    }
  rm(df_pre_negations)
  
  pattern_post_negations <- ""
  df_post_negations <-data.table::data.table(df_negations[df_negations$Type=='after',])
  
  pattern_post_negations_list<-vector("list",length(categories_list)+1)
  names(pattern_post_negations_list)<-c("General",categories_list)
  
  # get pattern for general negations
  df_post_negations_curr_cat <- df_post_negations[df_post_negations$Category=="General",]
  if(nrow(df_post_negations_curr_cat)>0){
    list_post_negations <- ""
    for(neg_indx in 1:nrow(df_post_negations_curr_cat))
      list_post_negations <- paste0(list_post_negations,"|",df_post_negations_curr_cat[neg_indx,"Negation"])
    if(nchar(list_post_negations)>0) {
      list_post_negations <- substring(list_post_negations,2,nchar(list_post_negations))
      pattern_post_negations <- paste0(list_post_negations,"|",gsub("_"," ",list_post_negations))
      if(utf8_language) pattern_post_negations<-enc2utf8(pattern_post_negations)
      pattern_post_negations <- paste0("(\\b(",pattern_post_negations,")\\b)")
    }
    pattern_post_negations_list[['General']]<-pattern_post_negations
  }
  
  # get patterns for specific negations for categories
  if(length(categories_list)>0)
    for(i in 1:length(categories_list)){
      pattern_post_negations <- ""
      list_post_negations <- ""
      df_post_negations_curr_cat <- df_post_negations[df_post_negations$Category==categories_list[i],]
      
      if(nrow(df_post_negations_curr_cat)>0){
        for(neg_indx in 1:nrow(df_post_negations_curr_cat))
          list_post_negations <- paste0(list_post_negations,"|",df_post_negations_curr_cat[neg_indx,"Negation"])
        # pattern_post_negations <- paste0("(\\b(",list_post_negations,gsub("_"," ",list_post_negations),")\\b)")
        if(nchar(list_post_negations)>0) {
          list_post_negations <- substring(list_post_negations,2,nchar(list_post_negations))
          pattern_post_negations <- paste0(list_post_negations,"|",gsub("_"," ",list_post_negations))
          if(utf8_language) pattern_post_negations<-enc2utf8(pattern_post_negations)
          pattern_post_negations <- paste0("(\\b(",pattern_post_negations,')\\b)')
        }
        pattern_post_negations_list[[categories_list[i]]] <- pattern_post_negations
      }
    }
  rm(df_post_negations)
  
  
  df_false_positiveLabels <- vector(mode = "logical", length = 0)
  df_irrelevant_positiveLabels <- vector(mode = "logical", length = 0)
  categories_cols_names <- "Simclins"
  
  if(nrow(df_positiveLabels)>0){
    
    if(length(categories_list)>0)
      for (cat_indx in 1:length(categories_list)){
        category_column_simclins_name <- paste0(categories_list[cat_indx],' (simclins)')
        category_column_simclins_count_name <- paste0(categories_list[cat_indx],' (# of simclins)')
        df_positiveLabels[,category_column_simclins_name]=""
        df_positiveLabels[,category_column_simclins_count_name]=0
        categories_cols_names <- paste0(categories_cols_names,",",category_column_simclins_name,",",category_column_simclins_count_name)
      }
    
    
    if (nrow(df_positiveLabels)>1)
      pb <- txtProgressBar(1,nrow(df_positiveLabels), style = 3)
    
    for(i in 1:nrow(df_positiveLabels)){
      
      curr_note = df_positiveLabels[i,"Note"]
      
      if(utf8_language) curr_note <- enc2utf8(curr_note)
      
      noteOfLabel = FALSE
      
      all_simclins_of_note <-c()
      curr_note_negations <- c()
      curr_note_irrelevant_terms <- c()
      
      tags_simclins <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(tags_simclins)<-c('word','start','end')
      tags_negated_simclins <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(tags_negated_simclins)<-c('word','start','end')
      tags_irrelevant_simclins <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(tags_negated_simclins)<-c('word','start','end')
      
      #check all simclins for negation
      #get simclins
      for(pattern_list_indx in 1:length(pattern_list)){
        
        list_simclins = stringi::stri_locate_all(curr_note, regex = pattern_list[pattern_list_indx], opts_regex=stri_opts_regex(case_insensitive=TRUE))
        prev_pos_end = 0
        
        #search simclins in note
        for (j in 1:nrow(list_simclins[[1]])){
          
          pos_start = list_simclins[[1]][j,'start']
          pos_end   = list_simclins[[1]][j,'end']
          
          if(!is.na(pos_start) & !is.na(pos_end)) {
            
            isSimclinIrrelevant = FALSE
            isSimclinNegated = FALSE
            
            curr_simclin = substring(curr_note,pos_start,pos_end)
            
            curr_simclin_categories <- df_simclins_for_search[df_simclins_for_search$Simclins==stri_trans_tolower(curr_simclin),'Category']
            
            #simclin` relevance check
            
            #there are irrelevant expressions for current simclin
            
            df_irrelevant_terms_of_simclin_cat <- df_irrelevant_terms[df_irrelevant_terms$Category %in% curr_simclin_categories,'Similar_term']
            
            if(length(grep(curr_simclin,df_irrelevant_terms_of_simclin_cat,ignore.case = TRUE))!=0){
              df_irrelevant_simclins <-df_irrelevant_terms_of_simclin_cat[grepl(curr_simclin,df_irrelevant_terms_of_simclin_cat,ignore.case = TRUE)]
              
              pattern_irrelevant_terms = paste(df_irrelevant_simclins, collapse = '|', sep="")
              pattern_irrelevant_terms = paste(pattern_irrelevant_terms,"|",gsub("_"," ",pattern_irrelevant_terms), sep="")
              
              if(utf8_language) pattern_irrelevant_terms <- enc2utf8(pattern_irrelevant_terms)
              irr_expressions_in_note = stringi::stri_locate_all(curr_note, regex =  paste("(",pattern_irrelevant_terms,")", sep=""), opts_regex=stri_opts_regex(case_insensitive=TRUE))
              
              #check every irrelevant expression for current simclin
              for (i_irr_expr in 1:nrow(irr_expressions_in_note[[1]])){
                start_pos_expression_in_note = irr_expressions_in_note[[1]][i_irr_expr,'start']
                end_pos_expression_in_note = irr_expressions_in_note[[1]][i_irr_expr,'end']
                if(!is.na(start_pos_expression_in_note)){
                  curr_irrelevant_expression = substring(curr_note,start_pos_expression_in_note,end_pos_expression_in_note)
                  pos_simclin_in_expression = grep(curr_simclin,curr_irrelevant_expression,ignore.case = TRUE)
                  #is it current simclin? pos_start - pos of current simclin in note
                  if(length(pos_simclin_in_expression)>0)
                    for(i_simclin in 1:length(pos_simclin_in_expression)){
                      if(pos_start>=start_pos_expression_in_note+pos_simclin_in_expression[i_simclin]-1)
                        isSimclinIrrelevant = TRUE
                    }
                }
              }
            }
            
            # Negations check (if the simclin is relevant)
            if(!isSimclinIrrelevant){
              pattern_pre_negations <- pattern_pre_negations_list[['General']]
              if(length(curr_simclin_categories)>0)
                for (cat_indx in 1:length(curr_simclin_categories)){
                  pattern_pre_negations <- paste(pattern_pre_negations,pattern_pre_negations_list[[unlist(curr_simclin_categories[cat_indx])]],sep = "")
                }
              
              substring_before_simclin =  substring(curr_note,1,pos_end)
              substring_after_simclin =   substring(curr_note,pos_start)
              
              if(length(pattern_pre_negations)>0){
                
                #get pattern for pre-negations
                
                pattern_pre_negations_with_distance <- paste0(pattern_pre_negations,"\\W*",stri_dup("(\\w*)\\W*",as.character(distance_between_simclin_and_negation)),"(\\b(",curr_simclin,")(?!\\w))")
                # find all negations near the current simclin
                negations_of_curr_simclin = stringi::stri_locate_all(substring_before_simclin, regex = pattern_pre_negations_with_distance, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                pattern_current_pre_negation <- paste0(pattern_pre_negations,"(?=(\\W*",stri_dup("(\\w*)\\W*",as.character(distance_between_simclin_and_negation)),"(\\b(",curr_simclin,")(?!\\w))))")
                # if there are negations - check it for exceptions (pseudo and terminations later)
                if(nrow(negations_of_curr_simclin[[1]])>0 & !is.na(negations_of_curr_simclin[[1]][1,'start']))
                  for (i_negation in nrow(negations_of_curr_simclin[[1]]):1){
                    isNegationPseudo = FALSE
                    
                    if(!isSimclinNegated){
                      pos_start_curr_negation = negations_of_curr_simclin[[1]][i_negation,'start']
                      pos_end_curr_negation = negations_of_curr_simclin[[1]][i_negation,'end']
                      
                      post_start_curr_negation_in_note = pos_start_curr_negation
                      post_end_curr_negation_in_note = pos_end_curr_negation
                      
                      if(!is.na(pos_start_curr_negation) & pos_end_curr_negation>=pos_end){
                        
                        curr_simclin_with_negation = substring(substring_before_simclin,pos_start_curr_negation,pos_end_curr_negation)
                        
                        
                        curr_negation = stri_extract_first(curr_simclin_with_negation, regex = pattern_current_pre_negation, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                        curr_negation <- gsub(x = curr_negation,pattern = " ",replacement = "_")
                        
                        if (nrow(df_exceptions)>0 & length(df_exceptions[df_exceptions$Category %in% curr_simclin_categories,'Exception'])>0) {
                          # for every negation - get list of its exceptions
                          exceptions_pattern_str <-df_exceptions[grepl(curr_negation,df_exceptions[df_exceptions$Category %in% curr_simclin_categories,'Exception'],ignore.case = TRUE),'Exception']
                          
                          # find these exceptions near the current simclin
                          exceptions_near_curr_simclin = stringi::stri_locate_all(substring_before_simclin, regex = paste("(",paste(exceptions_pattern_str, collapse = '|', sep=""),")", sep=""), opts_regex=stri_opts_regex(case_insensitive=TRUE))
                          
                          # for every exception near simclin
                          if(nrow(exceptions_near_curr_simclin[[1]])>0)
                            for (i_exception in 1:nrow(exceptions_near_curr_simclin[[1]])){
                              pos_start_exception = exceptions_near_curr_simclin[[1]][i_exception,'start']
                              pos_end_exception = exceptions_near_curr_simclin[[1]][i_exception,'end']
                              if(!is.na(pos_start_exception)){
                                curr_exception = substring(substring_before_simclin,pos_start_exception,pos_end_exception)
                                pos_negation_in_exception = grep(curr_negation,curr_exception)
                                #is it current negation?
                                if(length(pos_negation_in_exception)>0)
                                  for(i_negation_in_exception in 1:length(pos_negation_in_exception)){
                                    if(pos_start_curr_negation>=pos_negation_in_exception[i_negation_in_exception]+pos_start_exception-1) {
                                      isNegationPseudo = TRUE
                                    }
                                    
                                  }
                              }
                            }
                        }
                        if(!isNegationPseudo) isSimclinNegated = TRUE
                      }
                    } #f(!isSimclinNegated){
                  }#for (i_negation
              } #if(nchar(pattern_pre_negations)>0)
              
              #if negation was not found - check for post-negations
              if(!isSimclinNegated){
                
                pattern_post_negations <- pattern_post_negations_list[['General']]
                if(length(curr_simclin_categories)>0)
                  for (cat_indx in 1:length(curr_simclin_categories)){
                    pattern_post_negations <- paste(pattern_post_negations,pattern_post_negations_list[[unlist(curr_simclin_categories[cat_indx])]],sep = "")
                  }
                
                if(length(pattern_post_negations)>0){
                  #get pattern for post-negations
                  pattern_post_negations_with_distance<-paste0("(\\b(",curr_simclin,")(?!\\w))")
                  pattern_post_negations_with_distance <- paste0(pattern_post_negations_with_distance,stri_dup("\\W*(\\w*)",as.character(distance_between_simclin_and_negation)),"\\W*",pattern_post_negations)
                  
                  # find all negations near the current simclin
                  negations_of_curr_simclin = stringi::stri_locate_all(substring_after_simclin, regex = pattern_post_negations_with_distance, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                  if(nrow(negations_of_curr_simclin[[1]])>0 & !is.na(negations_of_curr_simclin[[1]][1,'start']))
                    for (i_negation in 1:nrow(negations_of_curr_simclin[[1]])){
                      isNegationPseudo = FALSE
                      
                      if(!isSimclinNegated){
                        pos_start_curr_negation = negations_of_curr_simclin[[1]][i_negation,'start']
                        pos_end_curr_negation = negations_of_curr_simclin[[1]][i_negation,'end']
                        
                        post_start_curr_negation_in_note = pos_start_curr_negation + pos_start - 1
                        post_end_curr_negation_in_note = pos_end_curr_negation + pos_start - 1
                        
                        if(!is.na(pos_start_curr_negation)) {
                          curr_simclin_with_negation = substring(substring_after_simclin,pos_start_curr_negation,pos_end_curr_negation)
                          
                          
                          curr_negation = stri_extract_first(curr_simclin_with_negation, regex = pattern_post_negations, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                          df_exceptions_for_current_category <- df_exceptions[df_exceptions$Category %in% curr_simclin_categories,]
                          if (nrow(df_exceptions_for_current_category)>0){
                            # for every negation - get list of its exceptions
                            exceptions_pattern_str <-df_exceptions_for_current_category[grepl(df_exceptions_for_current_category$Exception,curr_negation,ignore.case = TRUE),'Exception']
                            # find these exceptions near the current simclin
                            exceptions_near_curr_simclin = stringi::stri_locate_all(substring_after_simclin, regex = paste("(",paste(exceptions_pattern_str, collapse = '|', sep=""),")", sep=""), opts_regex=stri_opts_regex(case_insensitive=TRUE))
                            
                            # for every exception near simclin
                            
                            for (i_exception in 1:nrow(exceptions_near_curr_simclin[[1]])){
                              pos_start_exception = exceptions_near_curr_simclin[[1]][i_exception,'start']
                              pos_end_exception = exceptions_near_curr_simclin[[1]][i_exception,'end']
                              
                              if(!is.na(pos_start_exception)){
                                curr_exception = substring(substring_after_simclin,pos_start_exception,pos_end_exception)
                                pos_negation_in_exception = grep(curr_negation,curr_exception)
                                #is it current negation?
                                for(i_negation_in_exception in 1:length(pos_negation_in_exception)){
                                  if(pos_start_curr_negation>=pos_negation_in_exception[i_negation_in_exception]+pos_start_exception-1)
                                    isNegationPseudo = TRUE
                                }
                              }
                            } #for (i_exception
                          }
                          if(!isNegationPseudo)
                            isSimclinNegated = TRUE
                        }#if(!is.na(pos_start_curr_negation)) {
                      }#if(!isSimclinNegated){
                    }#for (i_negation
                } # check post negations
              } #if(!isSimclinNegated)
            } #if(!isSimclinIrrelevant) then check negations
            
            if (!isSimclinNegated & !isSimclinIrrelevant) {
              noteOfLabel = TRUE
              curr_simclin <- gsub(x = curr_simclin,pattern = " ",replacement = "_")
              tags_simclins<-rbind(tags_simclins,data.frame("word" = curr_simclin, "start" = pos_start, "end" = pos_end+1)) 
              if(!(curr_simclin %in% all_simclins_of_note)){
                all_simclins_of_note <- c(all_simclins_of_note,curr_simclin)
                if(length(curr_simclin_categories)>0) {
                  df_positiveLabels[i,paste0(curr_simclin_categories[1],' (simclins)')] <- paste(c(unlist(strsplit(df_positiveLabels[i,paste0(curr_simclin_categories[1],' (simclins)')],split = ", ")),curr_simclin),collapse = ", ")
                  df_positiveLabels[i,paste0(curr_simclin_categories[1],' (# of simclins)')] <- (as.integer(df_positiveLabels[i,paste0(curr_simclin_categories[1],' (# of simclins)')]))+1
                  
                }
              }
              
            } else {
              if(isSimclinNegated){
                curr_simclin_with_negation<-sub("\\(","\\\\(",curr_simclin_with_negation)
                curr_simclin_with_negation<-sub("\\)","\\\\(",curr_simclin_with_negation)
                curr_note_negations <- append(curr_note_negations,stri_trans_tolower(curr_negation))
                tags_negated_simclins<-rbind(tags_negated_simclins,data.frame("word" = curr_simclin, "start" = post_start_curr_negation_in_note, "end" = post_end_curr_negation_in_note+1)) 
              }
              else if(isSimclinIrrelevant){
                curr_note_irrelevant_terms<-sub("\\(","\\\\(",curr_note_irrelevant_terms)
                curr_note_irrelevant_terms<-sub("\\)","\\\\(",curr_note_irrelevant_terms)                  
                curr_note_irrelevant_terms <- append(curr_note_irrelevant_terms,stri_trans_tolower(gsub(" ","_",trimws(curr_irrelevant_expression))))
                tags_irrelevant_simclins<-rbind(tags_irrelevant_simclins,data.frame("word" = curr_simclin, "start" = start_pos_expression_in_note, "end" = end_pos_expression_in_note+1)) 
              }
            }
            
          } #if(!is.na(pos_start)&!is.na(pos_end))
          prev_pos_end = pos_end
        }#loop over simclins
        
      }#loop over pattern list
      
      
      #check simclins if they are part of other negated and irrelevant expressions (by coordinates) or other simclins
      if(nrow(tags_simclins)>0) {
        for(tags_indx_simclins in 1:nrow(tags_simclins)){
          if(nrow(tags_negated_simclins)>0) 
            for(tags_indx_negated_simclins in 1:nrow(tags_negated_simclins))
              if (as.numeric(tags_simclins[tags_indx_simclins,'start'])>=as.numeric(tags_negated_simclins[tags_indx_negated_simclins,'start'])
                  & as.numeric(tags_simclins[tags_indx_simclins,'end'])<=as.numeric(tags_negated_simclins[tags_indx_negated_simclins,'end'])){
                tags_simclins[tags_indx_simclins,'start']<- 0
                tags_simclins[tags_indx_simclins,'end']<- 0
              }
          if(nrow(tags_irrelevant_simclins)>0) 
            for(tags_indx_irrelevant_simclins in 1:nrow(tags_irrelevant_simclins))
              if (as.numeric(tags_simclins[tags_indx_simclins,'start'])>=as.numeric(tags_irrelevant_simclins[tags_indx_irrelevant_simclins,'start'])
                  & as.numeric(tags_simclins[tags_indx_simclins,'end'])<=as.numeric(tags_irrelevant_simclins[tags_indx_irrelevant_simclins,'end'])){
                tags_simclins[tags_indx_simclins,'start']<- 0
                tags_simclins[tags_indx_simclins,'end']<- 0
              }  
          #remove simclins which re part of other simclins
          for(tags_indx_simclins2 in 1:nrow(tags_simclins))
            if (rownames(tags_simclins)[tags_indx_simclins]!=rownames(tags_simclins)[tags_indx_simclins2] & as.numeric(tags_simclins[tags_indx_simclins,'start'])>=as.numeric(tags_simclins[tags_indx_simclins2,'start'])
                & as.numeric(tags_simclins[tags_indx_simclins,'end'])<=as.numeric(tags_simclins[tags_indx_simclins2,'end'])){
              tags_simclins[tags_indx_simclins,'start']<- 0
              tags_simclins[tags_indx_simclins,'end']<- 0
            } 
        }
        tags_simclins <- tags_simclins[tags_simclins$start>0 & tags_simclins$end>0,]
        all_simclins_of_note <- paste(sort(unlist(lapply(unique(tags_simclins$word),as.character))),collapse = ', ')     
      }
      curr_note_negations <- paste(sort(unique(curr_note_negations)),collapse = ', ')   
      curr_note_irrelevant_terms <- paste(sort(unique(curr_note_irrelevant_terms)),collapse = ', ')   
      
      #join all tags with their positions
      tags_all <- data.frame(matrix(ncol = 2, nrow = 0))
      colnames(tags_all) <- c("pos","tag")
      
      if(nrow(tags_simclins)>0){
        tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_simclins$start),"tag" = "<span class='true-simclin'>"))
        tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_simclins$end),"tag" = "</span>"))        
      }
      if(nrow(tags_negated_simclins)>0){
        tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_negated_simclins$start),"tag" = "<span class='false-simclin'>"))
        tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_negated_simclins$end),"tag" = "</span>"))        
      }
      if(nrow(tags_irrelevant_simclins)){
        tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_irrelevant_simclins$start),"tag" = "<span class='false-simclin'>"))
        tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_irrelevant_simclins$end),"tag" = "</span>"))        
      }  
      
      #sort tags by their positions in desc order 
      tags_all <- tags_all[with(tags_all, order(pos,decreasing = TRUE)),]
      
      #insert tags
      if(nrow(tags_all)>0)
        for(tag_indx in (1:nrow(tags_all))){
          curr_note <- paste0(substring(curr_note,1,tags_all$pos[tag_indx]-1),tags_all$tag[tag_indx],substring(curr_note,as.numeric(tags_all$pos[tag_indx])))
        }
      
      #if there is at least one not negated and not irrelevant simclin - the patient's note is labeled as TRUE
      if (nrow(tags_simclins) >0 ) {
        
        df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,NA)
        df_false_positiveLabels = append(df_false_positiveLabels,NA)
        df_positiveLabels[i,'Simclins'] <- all_simclins_of_note
        
      }
      else {
        if(isSimclinNegated){
          df_false_positiveLabels = append(df_false_positiveLabels,paste(curr_note_negations, collapse = ", "))
          df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,NA)
          total_negated_notes<-total_negated_notes+1
        } else if(isSimclinIrrelevant){
          df_false_positiveLabels = append(df_false_positiveLabels,NA)
          df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,paste(curr_note_irrelevant_terms, collapse = ", "))
        }
        
      }
      df_positiveLabels[i,"Note"]<-curr_note
      if (nrow(df_positiveLabels)>1)
        setTxtProgressBar(pb, i)
    }# loop over notes
  } 
  
  
  df_labeled_data <- read.csv(fileName_all_labeled_data,col.names = c(orignal_source_colnames), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  df_negatedPosLabels <<- data.table::data.table(df_positiveLabels[!is.na(df_false_positiveLabels), 1:length(orignal_source_colnames)])
  df_negatedPosLabels$Label <<- FALSE
  df_labeled_data <- rbind(df_labeled_data,df_negatedPosLabels)
  df_negatedPosLabels$Label <<- NULL
  df_negatedPosLabels$Negation <<- df_false_positiveLabels[!is.na(df_false_positiveLabels)]
  write.csv(df_negatedPosLabels[,c("Note","Negation")], file = paste0(app_dir,"pos_negated_labeled_data.csv"),fileEncoding = "UTF-8", row.names = FALSE)
  
  
  
  df_irrelevantPosLabels <<- data.table::data.table(df_positiveLabels[!is.na(df_irrelevant_positiveLabels), 1:length(orignal_source_colnames)])
  df_irrelevantPosLabels$Label <<- FALSE
  df_labeled_data <- rbind(df_labeled_data,df_irrelevantPosLabels)
  df_irrelevantPosLabels$Label <<- NULL
  df_irrelevantPosLabels$Similar_term <<- df_irrelevant_positiveLabels[!is.na(df_irrelevant_positiveLabels)]
  write.csv(df_irrelevantPosLabels[,c("Note","Similar_term")], file = paste0(app_dir,"pos_irrelevant_labeled_data.csv"),fileEncoding = "UTF-8", row.names = FALSE)
  
  
  df_positiveLabels <- df_positiveLabels[is.na(df_false_positiveLabels)&is.na(df_irrelevant_positiveLabels), ]
  write.csv(df_positiveLabels, file = paste0(app_dir,"pos_labeled_data.csv"),fileEncoding = "UTF-8", row.names = FALSE)
  
  if(nrow(df_positiveLabels)>0)
    df_positiveLabels$Label<- TRUE
  else df_positiveLabels$Label<- logical(0)
  
  df_labeled_data <- rbind(df_positiveLabels,df_labeled_data, fill = TRUE)
  #df_labeled_data<-df_labeled_data[with(df_labeled_data, order(NimbleMiner_ID)), ]
  #df_labeled_data$NimbleMiner_ID<-NULL
  write.csv(df_labeled_data, file = fileName_all_labeled_data, fileEncoding = "UTF-8", na = "", row.names = FALSE)
  
  total_positive_notes <-nrow(df_positiveLabels)
  total_irrelevant_notes <- nrow(df_irrelevantPosLabels)
  total_negated_notes <- nrow(df_negatedPosLabels)
  total_negative_notes <- total_notes - total_positive_notes - total_negated_notes
  
  rm(df_false_positiveLabels)
  rm(df_irrelevant_positiveLabels)
  
  return (list(labeled_data = df_allNotes, pos_labels = df_positiveLabels, "error_msg" = "","total_notes" = total_notes, "total_positive_notes" = total_positive_notes,"total_irrelevant_notes" = total_irrelevant_notes,"total_negated_notes" = total_negated_notes,"total_negative_notes"=total_negative_notes))
}

#############################################################################################################
# Function generate_corpus - create corpus from positive, negative, and negated labeled data
# param path = working directory
# param notes_of_positive_class = number of notes with positive class, taken from NimbleMiner app
# param notes_of_negative_class = number of notes with negative class, taken from NimbleMiner app
# param notes_of_negated_class = number of notes with negated class, taken from NimbleMiner app
#############################################################################################################  

generate_corpus <- function(path, chunk_size, notes_of_positive_class, notes_of_negative_class, notes_of_negated_class){
  df_new_rows <-  read.csv(paste0(app_dir, "pos_labeled_data.csv"), header = TRUE, stringsAsFactors = FALSE, comment.char = "", nrows = notes_of_positive_class)
  
  if (nrow(df_new_rows) == 0) {
    print("There are no positive labeled data in results of previous labels assignment.")
  } else {
    corpus_df <- data.frame(Note = df_new_rows$Note, Label = TRUE, stringsAsFactors = FALSE)

    df_new_rows <-  read.csv(paste0(app_dir, "negative_labeled_data.csv"), header = TRUE, stringsAsFactors = FALSE, comment.char = "", nrows = notes_of_negative_class)
    if (nrow(df_new_rows) > 0)
      corpus_df <- rbind(corpus_df, data.frame(Note = df_new_rows$Note, Label = FALSE, stringsAsFactors = FALSE))
    
    df_new_rows <-  read.csv(paste0(app_dir, "pos_negated_labeled_data.csv"), header = TRUE, stringsAsFactors = FALSE, comment.char = "", nrows = notes_of_negated_class)
    if (nrow(df_new_rows) > 0)
      corpus_df <- rbind(corpus_df, data.frame(Note = df_new_rows$Note, Label = FALSE, stringsAsFactors = FALSE))
    
    corpus_df <- clear_corpus(corpus_df)
    corpus_df <- corpus_df[sample(nrow(corpus_df)),]
    
    write.csv(corpus_df, file = paste0(app_dir,chunk_size,"_corpus.csv"), row.names = FALSE)
    
    return(corpus_df)
  
  }
}

#############################################################################################################
# Function clear_corpus - clear labeled data from marks of simclins and undescore signs from ngrams
#############################################################################################################  
clear_corpus <- function(corpus){
  corpus$Note <- gsub("[\r\n]", "", corpus$Note)
  corpus$Note <- gsub("<span class='true-simclin'>", "", corpus$Note)
  corpus$Note <- gsub("<span class='false-simclin'>", "", corpus$Note)
  corpus$Note <- gsub("</span>", "", corpus$Note)
  corpus$Note <- gsub("_", " ", corpus$Note)
  corpus$Note <- tm::removePunctuation(corpus$Note, preserve_intra_word_contractions = FALSE, preserve_intra_word_dashes = FALSE, ucp = TRUE)
  corpus$Note <- gsub("[[:digit:]]", "", corpus$Note)
  corpus$Note <- tolower(corpus$Note)
  corpus
}

#############################################################################################################
# Function create_matrix - runtime fix small error of create_matrix from RTextTools package
############################################################################################################# 
create_matrix <- function(textColumns, language = "english", minDocFreq = 1,
                          maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf,
                          ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE,
                          removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE,
                          stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE,
                          weighting = weightTf)
{
  
  stem_words <- function(x) {
    split <- strsplit(x, " ")
    return(wordStem(unlist(split), language = language))
  }
  
  tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, method = "string", n = n)))))
  
  control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)),
                  language = language, tolower = toLower, removeNumbers = removeNumbers,
                  removePunctuation = removePunctuation, stopwords = removeStopwords,
                  stripWhitespace = stripWhitespace, wordLengths = c(minWordLength,
                                                                     maxWordLength), weighting = weighting)
  if (ngramLength > 1) {
    control <- append(control, list(tokenize = tokenize_ngrams),
                      after = 7)
  }
  else {
    control <- append(control, list(tokenize = scan_tokenizer),
                      after = 4)
  }
  if (stemWords == TRUE && ngramLength == 1)
    control <- append(control, list(stemming = stem_words),
                      after = 7)
  trainingColumn <- apply(as.matrix(textColumns), 1, paste,
                          collapse = " ")
  trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"),
                           iconv, to = "UTF8", sub = "byte")
  
  
  corpus <- tm::VCorpus(VectorSource(trainingColumn),readerControl = list(language = language))
  library(stopwords)
  corpus <- tm::tm_map(corpus, removeWords, stopwords("English"))
  #corpus <- tm_map(corpus, removeWords, stopwords::stopwords("he", source = "stopwords-iso"))
  matrix <- tm::DocumentTermMatrix(corpus, control = control)
  
  if (removeSparseTerms > 0)
    matrix <- tm::removeSparseTerms(matrix, removeSparseTerms)
  if (!is.null(originalMatrix)) {
    terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in%
                                               colnames(matrix))])
    
    weight <- 0
    if (attr(weighting, "acronym") == "tf-idf")
      weight <- 1e-09
    amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
    colnames(amat) <- terms
    rownames(amat) <- rownames(matrix)
    fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in%
                                                          colnames(originalMatrix))], amat), weighting = weighting)
    matrix <- fixed
  }
  matrix <- matrix[, sort(colnames(matrix))]
  gc()
  return(matrix)
}


#####

train_ML <- function(app_dir, model_type, chunk_size) {
  
  corpus_df <- read.csv(paste0(app_dir,chunk_size, "_corpus.csv"),header = TRUE, stringsAsFactors=FALSE, comment.char = "", fileEncoding = "UTF-8")
  corpus_df <- clear_corpus(corpus_df)
  trainingSize <- round(nrow(corpus_df)*(2/3),0)
  
  file_folder = paste0("ml_", chunk_size)
  
  dir.create(file.path(app_dir, file_folder), showWarnings = FALSE)
  
  # train a SVM Model (RTextTools package)
  if (model_type == "SVM") {
    
    #progress$set(detail = paste0(nrow(corpus_df)," notes were found in the file. DTM building...") ,value = 2)
    
    # fix error in package
    tmpfun <- get("create_matrix", envir = asNamespace("RTextTools"))
    environment(create_matrix) <- environment(tmpfun)
    assignInNamespace("create_matrix", create_matrix, ns = "RTextTools")
    
    dtMatrix <- create_matrix(corpus_df$Note, toLower = FALSE, removeNumbers = TRUE, #language="he", 
                              removeStopwords = FALSE, removePunctuation=TRUE, stripWhitespace=TRUE, ngramLength =1)
    
    #debug info - print first Note and its most frequently terms
    freqTerms <- findFreqTerms(dtMatrix)
    #print(corpus_df[1,'Note'])
    firtsDoc_freqTerms <- findMostFreqTerms(dtMatrix, 10L)
    #print(firtsDoc_freqTerms[[1]])
    
    write.csv(freqTerms, file = paste0(app_dir, file_folder, "/freqTerms_", model_type, ".csv"), fileEncoding = "UTF-8", row.names = FALSE)
    write.csv(firtsDoc_freqTerms[[1]], file = paste0(app_dir, file_folder, "/firtsDoc_freqTerms_", model_type, ".csv"), fileEncoding = "UTF-8", row.names = FALSE)
    
    # Configure the training data
    #progress$set(detail = paste0("Configure the training data with ",trainingSize," notes.") ,value = 3)
    container <- RTextTools::create_container(dtMatrix, as.numeric(as.vector(corpus_df$Label)), trainSize = 1:trainingSize, testSize = (trainingSize+1):nrow(corpus_df) , virgin=FALSE)
    
    
    svm_method_parameter <- "C-classification"
    svm_cost_parameter <- 4
    svm_gamma_parameter <- 0.5
    svm_kernel_parameter <- "radial"
    parameters_of_model <- paste0("Method: ",model_type,", Cost = ",svm_cost_parameter,", Type =",svm_method_parameter,", Kernel = ",svm_kernel_parameter)
    #progress$set(detail = paste0("Training the model...") ,value = 4)
    model <- train_model(container, "SVM", kernel=svm_kernel_parameter, cost = svm_cost_parameter, method = svm_method_parameter, gamma = svm_gamma_parameter)
    
    #progress$set(detail = "Save the model..." ,value = 5)
    save(model,file=paste0(app_dir, file_folder, "/trainedModel_",model_type,".Rd"))
    save(dtMatrix,file=paste0(app_dir, file_folder, "/originalMatrix.Rd"))
    
    predictionData <- corpus_df[(trainingSize+1):(nrow(corpus_df)),]
    pb <- txtProgressBar(1,nrow(corpus_df), style = 3)
    rm(corpus_df)
    
    #progress$set(detail = "Test the model..." ,value = 6)
    predictedLabels <- classify_model(container, model)
    predictionData$Predicted_labels <- predictedLabels
    
    
    # VIEW THE RESULTS BY CREATING ANALYTICS
    analytics <- create_analytics(container, predictedLabels)
    
    # RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
    # analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
    # analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
    # analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
    # analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()
    
    # WRITE OUT THE DATA TO A CSV
    write.csv(analytics@algorithm_summary,paste0(app_dir,file_folder, "/SampleData_AlgorithmSummary_",model_type,".csv"), row.names = FALSE)
    write.csv(analytics@label_summary,paste0(app_dir,file_folder, "/SampleData_LabelSummary_",model_type,"SVM.csv"), row.names = FALSE)
    write.csv(predictionData,file = paste0(app_dir,file_folder, "/prediction_test_results_",model_type,".csv"), row.names = FALSE)
    
    algorythms_summary_table  <- DT::renderDataTable(analytics@algorithm_summary,  escape = FALSE)
    print(analytics@algorithm_summary)
    precision_results <- paste0("TRUE -  ",analytics@algorithm_summary["1","SVM_PRECISION"],", FALSE - ",analytics@algorithm_summary["0","SVM_PRECISION"])
    recall_results <- paste0("TRUE -  ",analytics@algorithm_summary["1","SVM_RECALL"],", FALSE - ",analytics@algorithm_summary["0","SVM_RECALL"])
    
    test_results_of_model <- paste0("Precision: ",precision_results,"; Recall: ",recall_results)
    #logAction(userId = currentUserId, operation = "Training the model", parameters = parameters_of_model, valueAfter=test_results_of_model)
    #showModal(modalDialog(title = "Information",  paste0("The model is ready ! ",parameters_of_model,". Test results - ",test_results_of_model,". Labeled test data were saved in the file 'ml/prediction_test_results_",model_type,".csv'."),easyClose = TRUE))
    
    return(list(test_results_of_model, dtMatrix))
    
  }
  # train a NNET Model (keras package, see as source https://tensorflow.rstudio.com/keras/articles/examples/imdb_bidirectional_lstm.html)
  else if(model_type == "NNET"){
    
    # should be installed python and keras on computer before
    pb <- txtProgressBar(1,nrow(corpus_df), style = 3)
    
    # Define maximum number of input features
    max_features <- 1000
    
    # Cut texts after this number of words
    # (among top max_features most common words)
    maxlen <- 200
    
    batch_size <- 64
    
    #progress$set(detail = paste0("Configure the training data with ",trainingSize," notes.") ,value = 3)
    # Define training and test sets
    x_train <- corpus_df[1:trainingSize,'Note']
    y_train <- corpus_df[1:trainingSize,'Label']
    corpus_df_test <- corpus_df[(trainingSize+1):nrow(corpus_df),]
    x_test <- corpus_df_test$Note
    y_test <- corpus_df_test$Label
    
    # Output lengths of testing and training sets
    cat(length(x_train), 'train sequences\n')
    cat(length(x_test), 'test sequences\n')
    
    # Pad sequences
    tok <- text_tokenizer(num_words = max_features)
    fit_text_tokenizer(tok, x_train)
    sequences = texts_to_sequences(tok, x_train)
    x_train = pad_sequences(sequences, maxlen = maxlen)
    test_sequences = texts_to_sequences(tok, x_test)
    x_test <- pad_sequences(test_sequences, maxlen = maxlen)
    
    # Output dimensions of training and test inputs
    cat('x_train shape:', dim(x_train), '\n')
    cat('x_test shape:', dim(x_test), '\n')
    
    # Initialize model
    
    model <- keras_model_sequential()
    model %>%
      # Creates dense embedding layer; outputs 3D tensor
      # with shape (batch_size, sequence_length, output_dim)
      layer_embedding(input_dim = max_features,
                      output_dim = 128,
                      input_length = maxlen) %>%
      bidirectional(layer_lstm(units = 64)) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = 'adam',
      metrics = c('accuracy')
    )
    
    # Train model
    #progress$set(detail = paste0("Training the model...") ,value = 4)
    model %>% keras::fit(
      x_train, y_train,
      batch_size = batch_size,
      epochs = 10,
      validation_data = list(x_test, y_test)
    )
    
    # Save model
    #progress$set(detail = "Save the model..." ,value = 5)
    model %>% save_model_hdf5(paste0(app_dir,file_folder, "/trainedModel_",model_type,".h5"))
    
    # Test model
    #progress$set(detail = "Test the model..." ,value = 6)
    predictedLabels <- model %>% predict_classes(
      x_test,
      batch_size = batch_size,
      verbose = 1
    )
    
    corpus_df_test$Predicted_label <- predictedLabels
    
    TP <- nrow(corpus_df_test[corpus_df_test$Label==TRUE & corpus_df_test$Predicted_label==1,])
    FP <- nrow(corpus_df_test[corpus_df_test$Label==FALSE & corpus_df_test$Predicted_label==1,])
    FN <- nrow(corpus_df_test[corpus_df_test$Label==TRUE & corpus_df_test$Predicted_label==1,])
    
    precision_results <- round(TP / (TP+FP),2)
    recall_results <- round(TP / (TP+FN),2)
    
    precision_results
    
    test_results_of_model <- paste0("Precision: ", precision_results, "; Recall: ", recall_results)
    parameters_of_model <- "Bidirectional LSTM, 1000 features, maxlen = 100, keras package"
    
    return(test_results_of_model)
    #logAction(userId = currentUserId, operation = "Training the model", parameters = parameters_of_model, valueAfter=test_results_of_model)
    #showModal(modalDialog(title = "Information",  paste0("The model is ready ! ",parameters_of_model,". Labeled test data were saved in the file 'ml/prediction_test_results_",model_type,".csv'."),easyClose = TRUE))
  }
  
  #progress$close()
}

#############################################################################################################
# Function predictByNNET - classification of notes by LSTM method
#############################################################################################################
predictByNNET <- function(notes,batch_size = 32,max_features = 1000,maxlen = 200){
  
  
  modelFileName <- paste0(app_dir, "ml_", chunk_size, "/trainedModel_NNET.h5")
  #progress <- shiny::Progress$new(min=1, max=2)
  #progress$set(message  = "Predicting data", detail = "Loading the model..." ,value = 1)
  model <- load_model_hdf5(modelFileName)
  
  # Pad sequences
  
  tok <- keras::text_tokenizer(num_words=max_features)
  keras::fit_text_tokenizer(tok,notes)
  sequences = keras::texts_to_sequences(tok,notes)
  notes = keras::pad_sequences(sequences,maxlen=maxlen)
  
  #progress$set(message  = "Predicting data", detail = "New data classification..." ,value = 2)
  
  result <- keras::predict_classes(
    model,
    notes,
    batch_size = batch_size,
    verbose = 1
  )
  
  #progress$close()
  result
}

#############################################################################################################
# Handler event of button click "Predict" (tab 6. Machine learning, section 3. Predict labels)
# Predict labels by Machine Learning methods (SVM and LSTM)  
#############################################################################################################
predict_labels <- function(app_dir, predictionData, dtMatrix, model_type, chunk_size) {
  
  file_folder = paste0("ml_", chunk_size)
  
  # notesToPredictFile <- input$notesToPredictFile
  # 
  # if (is.null(notesToPredictFile)) {
  #   showModal(modalDialog(title = "Error Message",  "Please specify the file with the notes to predict the label!",easyClose = TRUE))
  #   return()
  # } 
  # 
  # predictionData <- read.csv(notesToPredictFile$datapath,header = TRUE, stringsAsFactors=FALSE,comment.char = "", encoding = "UTF-8")
  colnames(predictionData) <- tolower(colnames(predictionData))
  
  # if (is.null(predictionData$note)){
  #   showModal(modalDialog(title = "Error Message",  "There is no column 'note' in the uploaded file.",easyClose = TRUE))
  #   return()
  # } 
  
  # LSTM method
  if (model_type == "NNET") {
    predictedLabels <- predictByNNET(predictionData$note,32)
  } else {
    
    # SVM method  
    #progress <- shiny::Progress$new(min=1, max=3)
    matrixFileName <- paste0(app_dir,file_folder, "/originalMatrix.Rd")
    modelFileName <- paste0(app_dir,file_folder, "/trainedModel_",model_type,".Rd")
    
    load(modelFileName)
    load(matrixFileName)
    #progress$set(message  = "Predicting data", detail = "Creating a prediction document term matrix..." ,value = 2)
    
    # create a prediction document term matrix
    predMatrix <- create_matrix(predictionData$note, originalMatrix = dtMatrix, toLower = FALSE, language="en", removeNumbers = TRUE, removeStopwords = FALSE,removePunctuation=TRUE, stripWhitespace=TRUE,ngramLength =1)
    
    pred_freqTerms <- findFreqTerms(predMatrix)
    write.csv(pred_freqTerms,file=paste0(app_dir,file_folder, "/pred_freqTerms_",model_type,".csv"), fileEncoding="UTF-8", row.names = FALSE)
    
    # create the corresponding container
    predSize = length(predictionData$note)
    predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
    
    # predict
    #progress$set(message  = "Predicting data", detail = "New data classification..." ,value = 3)
    predictedLabels <- classify_model(predictionContainer, model)
    
    #progress$close()
  } # predict by SVM (from RTextTool package)
  
  
  predictedLebel_colName <- paste0(toupper(model_type),"_LABEL")
  
  predictionData[,predictedLebel_colName] <- predictedLabels
  predictionData[,predictedLebel_colName] <- ifelse(predictionData[,predictedLebel_colName]=="0",FALSE,TRUE)
  
  
  fileDateTime <- format(Sys.time(),"%Y-%m-%d_%H-%M")
  write.csv(predictionData,file = paste0(app_dir,file_folder, "/prediction_results_",model_type,"_",fileDateTime,".csv"),fileEncoding = "UTF-8", row.names = FALSE)
  
  return(predictionData)
  #DT::renderDataTable(predictionData,  escape = FALSE)
  #logAction(userId = currentUserId, operation = "Prediction", parameters = paste0("Method: ",input$mlModelToPredict_input))
  #showModal(modalDialog(title = "Information",  paste0("The prediction is completed.  Detailed results were saved in the file 'ml/prediction_results_",input$mlModelToPredict_input,"_",fileDateTime,".csv'."),easyClose = TRUE))
  
}