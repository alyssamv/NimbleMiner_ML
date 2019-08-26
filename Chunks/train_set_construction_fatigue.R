app_dir = "~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/data/"
source("~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/functions/ML fns AV.R")
library(tm)
library(data.table)
library(stringi)
library(RTextTools)
library(tidyverse)
library(knitr)

#load("/Users/Alyssa/Desktop/DSI_NimbleMiner/NimbleMiner_ML/Chunks/train_doc_labeling_dyspnea.RData")

#chunk_size = 1
# test = readr::read_csv(file = paste0(app_dir, "gold_standard_HF_100_pt_AV.csv"))
# new_rows_test = seq(1, ceiling(nrow(test)/chunk_size), 1)
# 
# test = test %>%
#   mutate(chunk_id = sort(rep(new_rows_test, chunk_size))[1:nrow(.)]) %>%
#   tidyr::unite(Categories, contains("category")) %>%
#   group_by(chunk_id) %>%
#   summarise(Chunk = str_c(Note, collapse = " "),
#             Chunk_cats = str_c(Categories, collapse = "_") %>%
#               str_replace_all(c("NA_" = "", "_NA" = ""))) %>%
#   mutate(Chunk_cats = ifelse(Chunk_cats == "NA", NA, Chunk_cats)) %>%
#   rename("Note" = Chunk)


start.all = Sys.time()
n.notes = 10000
cat = "Fatigue"


all_notes = readr::read_csv(file = paste0(app_dir, "NOTEEVENTS_reduced.csv"))[1:n.notes,] %>%
  mutate(TEXT = gsub(pattern = "[0-9]. ", replacement = "", x = .$TEXT)) %>%
  janitor::clean_names() %>%
  select(x1, text) %>%
  rename("Note" = text)

df_simclins = readr::read_csv(file = paste0(app_dir, "simclins.csv")) %>%
  filter(Category == cat)
categories_list = cat # getCategoriesList()
df_irrelevant_terms = readr::read_csv(file = paste0(app_dir, "irrelevant_terms.csv"))
df_negations = readr::read_csv(file = paste0(app_dir, "negations.csv"))
df_exceptions = readr::read_csv(file = paste0(app_dir, "negations-exceptions.csv"))

# Step 2: Assign labels to all 10,000 documents (at the document level)
start = Sys.time()
labeling_documents <- suppressWarnings(
  assigneLabels(all_notes, chunk_size = 1, distance_between_simclin_and_negation = 5,
                df_simclins, categories_list, app_dir,
                df_negations, df_exceptions, df_irrelevant_terms,
                utf8_language = FALSE)
)
end = Sys.time()

end - start


# get which documents have evidence of the category
labeled_docs = labeling_documents$labeled_data %>% # get index for positively labeled documents
  filter(Label == TRUE) %>%
  select(x1) %>%
  unname %>%
  unlist

print(length(labeled_docs)/n.notes) # prevalence of category




# Now take only the positive documents and split into sentences
sentences =  all_notes[labeled_docs,] %>% 
  select(Note) %>%
  unlist %>%
  tokenizers::tokenize_sentences() %>% # split into sentences
  unname %>%
  unlist

length(sentences)

sentences = gsub(pattern = "[0-9]. ", replacement = "", x = sentences) # remove numbers

#new_rows_train = seq(1, ceiling(length(sentences)/chunk_size), 1)

train_df = data.frame(Note = sentences,
                      chunk_id = 1:length(sentences)) 

start.sentences = Sys.time()
labeling_sentences <- suppressWarnings(
  assigneLabels(train_df, chunk_size = 1, distance_between_simclin_and_negation = 5,
                df_simclins, categories_list, app_dir,
                df_negations, df_exceptions, df_irrelevant_terms,
                utf8_language = FALSE)
)
end.sentences = Sys.time()

end.sentences - start.sentences

labeled_sentences = which(labeling_sentences$labeled_data$Label == TRUE) # get index for positively labeled sentences

#ll = labeling_sentences$labeled_data
#train_beta = ll[n, ]

print(length(labeled_sentences)/nrow(train_df))


## create more balanced dataset (increase prevalence to ~1/3)
# for each positive sentence, take the 2 sentences that follow (getting index list)
n = c()
for (i in 1:length(labeled_sentences)) {
  n[[i]] = seq(labeled_sentences[[i]], labeled_sentences[[i]] + 2, 1)
}
n = unlist(n) %>% unique

# new training set based on index established above
train_new = labeling_sentences$labeled_data[n, ]

#train_beta = train_new[1:(4*nrow(test)), ] # make train set for beta testing 2:1 ratio to test

end.all = Sys.time()
save.image(file.path(app_dir, "train_labeling_fatigue.RData"))
write.csv(train_new, file.path(app_dir, "train_fatigue.csv"), row.names = FALSE)

end.all - start.all


