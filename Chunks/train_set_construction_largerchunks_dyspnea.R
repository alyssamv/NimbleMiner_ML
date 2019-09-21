app_dir = "~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/"
source(paste0(app_dir, "functions/ML fns AV.R"))
library(tm)
library(data.table)
library(stringi)
library(RTextTools)
library(tidyverse)
library(knitr)

load("/Users/Alyssa/Desktop/DSI_NimbleMiner/NimbleMiner_ML/Chunks/train_doc_labeling_dyspnea.RData")



start.all = Sys.time()
n.notes = 10000 # number of full notes to read in 
cat = "Dyspnea"
chunk_size = 10 # size of chunks


################################################ 
## Create test set of given chunk size
################################################
test = readr::read_csv(file = paste0(app_dir, 'data/test_150_sentences.csv'))
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

test$Label = grepl(pattern = cat, x = test$Chunk_cats) %>% as.numeric


################################################ 
## read in and label full notes
################################################
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

################################################
## get index for only positively labeled documents
################################################
labeled_docs = labeling_documents$labeled_data %>% 
  filter(Label == TRUE) %>%
  select(x1) %>%
  unname %>%
  unlist

print(length(labeled_docs)/n.notes) # prevalence of 




################################################
## split into sentences
################################################
sentences =  all_notes[labeled_docs,] %>% #[1:1000,]
  select(Note) %>%
  unlist %>%
  tokenizers::tokenize_sentences() %>%
  unname %>%
  unlist

length(sentences)

sentences = gsub(pattern = "[0-9]. ", replacement = "", x = sentences) # format sentences



################################################
# collapse sentences into chunks 
################################################
new_rows_train = seq(1, ceiling(length(sentences)/chunk_size), 1)

train_df = data.frame(Note = sentences,
                   chunk_id = NA) %>%
  mutate(Note = as.character(Note)) %>%
  mutate(chunk_id = sort(rep(new_rows_train, chunk_size))[1:nrow(.)]) %>%
  group_by(chunk_id) %>%
  summarise(Chunk = str_c(Note, collapse = " ")) %>%
  rename("Note" = Chunk)


################################################
# label chunked notes
################################################
start.sentences = Sys.time()
labeling_sentences <- suppressWarnings(
  assigneLabels(train_df, chunk_size = 1, distance_between_simclin_and_negation = 5,
                df_simclins, categories_list, app_dir = paste0(app_dir, "data/"),
                df_negations, df_exceptions, df_irrelevant_terms,
                utf8_language = FALSE)
)
end.sentences = Sys.time()

end.sentences - start.sentences

labeled_sentences = which(labeling_sentences$labeled_data$Label == TRUE) # get index for positively labeled documents

ll = labeling_sentences$labeled_data


print(length(labeled_sentences)/nrow(train_df))


################################################
## Create training dataset with 1/3 prevalence
################################################

# for each positive sentence, take the 3 sentences that follow (getting index list)
n = c()
for (i in 1:length(labeled_sentences)) {
  n[[i]] = seq(labeled_sentences[[i]], labeled_sentences[[i]] + 2, 1)
}
n = unlist(n) %>% unique

# new training set based on index established above
train_new = ll[n, ]

train_beta = train_new#[1:(2*nrow(test)), ] # make train set for beta testing 2:1 ratio to test


################################################
## Save data
################################################
rdata = paste0("train_labeling_dyspnea", chunk_size, ".RData")
train.csv = paste0("dyspnea_training_notes_NMlabeled_chunk", chunk_size, ".csv")
test.csv = paste0("dyspnea_test_chunk", chunk_size, ".csv")

save.image(paste0(app_dir, "Chunks/", rdata))
write.csv(train_beta, paste0(app_dir, "data/", train.csv), row.names = FALSE)
write.csv(test, paste0(app_dir, "data/", test.csv), row.names = FALSE)

end.all = Sys.time()

end.all - start.all
