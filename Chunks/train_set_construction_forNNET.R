app_dir = "~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/Chunks"
source(paste0("~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/", "ML fns AV.R"))
library(tm)
library(data.table)
library(stringi)
library(RTextTools)
library(tidyverse)
library(knitr)

load("~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/Chunks/dyspnea_positive_training_docs_sentences.RData")

chunk_size = 10

start.all = Sys.time()
n.notes = 10000
cat = "Dyspnea"


# all_notes = readr::read_csv(file = paste0(app_dir, "Chunks/data/NOTEEVENTS_reduced.csv"))[1:n.notes,] %>%
#   mutate(TEXT = gsub(pattern = "[0-9]. ", replacement = "", x = .$TEXT)) %>%
#   janitor::clean_names() %>%
#   select(x1, text) %>%
#   rename("Note" = text)

df_simclins = readr::read_csv(file = paste0(app_dir, "/data/simclins.csv")) %>%
  filter(Category == cat)
categories_list = cat # getCategoriesList()
df_irrelevant_terms = readr::read_csv(file = paste0(app_dir, "/data/irrelevant_terms.csv"))
df_negations = readr::read_csv(file = paste0(app_dir, "/data/negations.csv"))
df_exceptions = readr::read_csv(file = paste0(app_dir, "/data/negations-exceptions.csv"))

# # Step 2: Assign labels
# start = Sys.time()
# labeling_documents <- suppressWarnings(
#   assigneLabels(all_notes, chunk_size, distance_between_simclin_and_negation = 5,
#                 df_simclins, categories_list, app_dir,
#                 df_negations, df_exceptions, df_irrelevant_terms,
#                 utf8_language = FALSE)
# )
# end = Sys.time()
# 
# end - start
# 
# 
# labeled_docs = labeling_documents$labeled_data %>% # get index for positively labeled documents
#   filter(Label == TRUE) %>%
#   select(x1) %>%
#   unname %>%
#   unlist
# 
# print(length(labeled_docs)/n.notes)
# 
# 
# 
# sentences =  all_notes[labeled_docs,] %>% #[1:1000,]
#   select(Note) %>%
#   unlist %>%
#   tokenizers::tokenize_sentences() %>%
#   unname %>%
#   unlist
# 
# length(sentences)
# 
# sentences = gsub(pattern = "[0-9]. ", replacement = "", x = sentences)

sentences = sentences[1:(length(sentences)/2)]

new_rows_train = seq(1, ceiling(length(sentences)/chunk_size), 1)

train_df = data.frame(Note = sentences,
                   chunk_id = NA) %>%
  mutate(Note = as.character(Note)) %>%
  mutate(chunk_id = sort(rep(new_rows_train, chunk_size))[1:nrow(.)]) %>%
  group_by(chunk_id) %>%
  summarise(Chunk = str_c(Note, collapse = " ")) %>%
  rename("Note" = Chunk)

start.sentences = Sys.time()
labeling_sentences <- suppressWarnings(
  assigneLabels(train_df, chunk_size = 1, distance_between_simclin_and_negation = 5,
                df_simclins, categories_list, app_dir,
                df_negations, df_exceptions, df_irrelevant_terms,
                utf8_language = FALSE)
)
end.sentences = Sys.time()

end.sentences - start.sentences

labeled_sentences = which(labeling_sentences$labeled_data$Label == TRUE) # get index for positively labeled documents

print(length(labeled_sentences)/nrow(train_df))

# save.image("~/Desktop/DSI NimbleMiner/Machine Learning optimization/train_doc_labeling_allcats.RData")

# for each positive sentence, take the 3 sentences that follow (getting index list)
n = c()
for (i in 1:length(labeled_sentences)) {
  n[[i]] = seq(labeled_sentences[[i]], labeled_sentences[[i]] + 2, 1)
}
n = unlist(n) %>% unique

# new training set based on index established above
ll = labeling_sentences$labeled_data
train_beta = ll[n, ]

file_name = paste0('dyspnea_training_notes_NMlabeled_chunk', chunk_size, '.csv')
write.csv(train_beta, file.path(app_dir, "data", file_name), row.names = FALSE)

# ############### Wrapper function #################
# 
# 
# # Step 2: Assign labels
# labeling_results <- assigneLabels(train_beta, chunk_size = 1, 
#                                   distance_between_simclin_and_negation = 5,
#                                   df_simclins, categories_list, app_dir, 
#                                   df_negations, df_exceptions, df_irrelevant_terms,
#                                   utf8_language = FALSE)      
# labeled_data = labeling_results$labeled_data
# 
# 
# notes_of_positive_class = labeling_results$total_positive_notes # number of notes that were labeled positive
# notes_of_negative_class = labeling_results$total_negative_notes
# notes_of_negated_class = labeling_results$total_negated_notes
# corpus_df <- generate_corpus(app_dir, chunk_size = 1, notes_of_positive_class, 
#                              notes_of_negative_class, notes_of_negated_class)
# 
# # Step 2: Train the model
# model = train_ML(app_dir, "SVM", chunk_size = 1)
# model_matrix = model[[2]] # model matrix to feed to prediction
# 
# 
# # Step 3: Predict labels
# predictions = predict_labels(app_dir, predictionData = test, model_matrix, "SVM", chunk_size = 1)
# 
# get_measures(labeled_data, predictions, "SVM", category = cat)
# end.all = Sys.time()
# save.image("~/Desktop/DSI NimbleMiner/Machine Learning optimization/train_doc_labeling_chunk10_dyspnea.RData")
# 
# end.all - start.all
