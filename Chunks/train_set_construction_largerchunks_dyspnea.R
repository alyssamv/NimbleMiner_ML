app_dir = "~/Desktop/DSI_NimbleMiner/Machine Learning optimization/"
source(paste0(app_dir, "ML fns AV.R"))
library(tm)
library(data.table)
library(stringi)
library(RTextTools)
library(tidyverse)
library(knitr)

load("/Users/Alyssa/Desktop/DSI_NimbleMiner/NimbleMiner_ML/Chunks/train_doc_labeling_dyspnea.RData")

chunk_size = 5
test = readr::read_csv(file = paste0(app_dir, "gold_standard_HF_100_pt_AV.csv"))
new_rows_test = seq(1, ceiling(nrow(test)/chunk_size), 1)

test = test %>%
  mutate(chunk_id = sort(rep(new_rows_test, chunk_size))[1:nrow(.)]) %>%
  tidyr::unite(Categories, contains("category")) %>%
  group_by(chunk_id) %>%
  summarise(Chunk = str_c(Note, collapse = " "),
            Chunk_cats = str_c(Categories, collapse = "_") %>%
              str_replace_all(c("NA_" = "", "_NA" = ""))) %>%
  mutate(Chunk_cats = ifelse(Chunk_cats == "NA", NA, Chunk_cats)) %>%
  rename("Note" = Chunk)


start.all = Sys.time()
n.notes = 10000
cat = "Dyspnea"


# all_notes = readr::read_csv(file = paste0(app_dir, "NOTEEVENTS_reduced.csv"))[1:n.notes,] %>%
#   mutate(TEXT = gsub(pattern = "[0-9]. ", replacement = "", x = .$TEXT)) %>%
#   janitor::clean_names() %>%
#   select(x1, text) %>%
#   rename("Note" = text)
# 
# df_simclins = readr::read_csv(file = paste0(app_dir, "simclins.csv")) %>%
#   filter(Category == cat)
# categories_list = cat # getCategoriesList()
# df_irrelevant_terms = readr::read_csv(file = paste0(app_dir, "irrelevant_terms.csv"))
# df_negations = readr::read_csv(file = paste0(app_dir, "negations.csv"))
# df_exceptions = readr::read_csv(file = paste0(app_dir, "negations-exceptions.csv"))
# 
# # Step 2: Assign labels
# start = Sys.time()
# labeling_documents <- suppressWarnings(
#   assigneLabels(all_notes, chunk_size = 1, distance_between_simclin_and_negation = 5,
#                 df_simclins, categories_list, app_dir,
#                 df_negations, df_exceptions, df_irrelevant_terms,
#                 utf8_language = FALSE)
# )
# end = Sys.time()
# 
# end - start


labeled_docs = labeling_documents$labeled_data %>% # get index for positively labeled documents
  filter(Label == TRUE) %>%
  select(x1) %>%
  unname %>%
  unlist

print(length(labeled_docs)/n.notes)





sentences =  all_notes[labeled_docs,] %>% #[1:1000,]
  select(Note) %>%
  unlist %>%
  tokenizers::tokenize_sentences() %>%
  unname %>%
  unlist

length(sentences)

sentences = gsub(pattern = "[0-9]. ", replacement = "", x = sentences)

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

ll = labeling_sentences$labeled_data
train_beta = ll[n, ]

print(length(labeled_sentences)/nrow(train_df))

# save.image("~/Desktop/DSI NimbleMiner/Machine Learning optimization/train_doc_labeling_allcats.RData")

# for each positive sentence, take the 3 sentences that follow (getting index list)
n = c()
for (i in 1:length(labeled_sentences)) {
  n[[i]] = seq(labeled_sentences[[i]], labeled_sentences[[i]] + 2, 1)
}
n = unlist(n) %>% unique

# new training set based on index established above
train_new = labeled_data[n, ]

train_beta = train_new[1:(4*nrow(test)), ] # make train set for beta testing 2:1 ratio to test


############### Wrapper function #################


# Step 2: Assign labels
labeling_results <- assigneLabels(train_beta, chunk_size = 1, 
                                  distance_between_simclin_and_negation = 5,
                                  df_simclins, categories_list, app_dir, 
                                  df_negations, df_exceptions, df_irrelevant_terms,
                                  utf8_language = FALSE)      
labeled_data = labeling_results$labeled_data


notes_of_positive_class = labeling_results$total_positive_notes # number of notes that were labeled positive
notes_of_negative_class = labeling_results$total_negative_notes
notes_of_negated_class = labeling_results$total_negated_notes
corpus_df <- generate_corpus(app_dir, chunk_size = 1, notes_of_positive_class, 
                             notes_of_negative_class, notes_of_negated_class)

# Step 2: Train the model
model = train_ML(app_dir, "SVM", chunk_size = 1)
model_matrix = model[[2]] # model matrix to feed to prediction


# Step 3: Predict labels
predictions = predict_labels(app_dir, predictionData = test, model_matrix, "SVM", chunk_size = 1)

get_measures(labeled_data, predictions, "SVM", category = cat)
end.all = Sys.time()
save.image("~/Desktop/DSI NimbleMiner/Machine Learning optimization/train_doc_labeling_full_dyspnea.RData")

end.all - start.all




####### full clinical notes ########
chunks1 <- labeling_documents$labeled_data %>% 
  mutate(report_head = str_detect(Note, "^admission date"))

report_head <- chunks1 %>% 
  filter(report_head) %>% 
  select(Note, report_head) %>% 
  mutate(report_no = row_number()) %>% 
  select(-report_head)

full_notes <- labeling_documents$labeled_data %>% 
  # joint with report_head dataframe, report_no show which report each sentence belongs to
  left_join(report_head, by = c("Note")) %>% 
  mutate(report_no = zoo::na.locf(report_no),
         # remove all numbers
         Note = removeNumbers(Note)) %>%
  group_by(report_no) %>% 
  mutate(Label = as.numeric(Label)) %>%
  rename("dyspnea" = Label)

table(full_notes$dyspnea)

full_notes_neg = full_notes %>% filter(dyspnea == 0)
full_notes_neg_index = sample(nrow(full_notes_neg), size = 150)

full_notes_pos = full_notes %>% filter(dyspnea == 1)
full_notes_pos_index = sample(nrow(full_notes_pos), size = 150)

full_notes_training_data = rbind(full_notes_neg[full_notes_neg_index, ],
                                 full_notes_pos[full_notes_pos_index, ])
table(full_notes_training_data$dyspnea)

write.csv(full_notes_training_data, file = "/Users/Alyssa/Desktop/DSI_NimbleMiner/NimbleMiner_ML/Chunks/data/dyspnea_fullnotes_training.csv",
          row.names = FALSE)
####################################
