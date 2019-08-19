
data_folder = "~/Desktop/DSI_NimbleMiner/NimbleMiner_ML/data/"

# LOADING ORIGINAL DATASET (GOLD STANDARD)
file_name <- file.path(data_folder, 'gold_standard_HF_100_pt_AV.csv')

clinical_notes_raw_data <- file_name %>% 
  readr::read_csv() %>% 
  # X1 is the index column, unselect this column
  select(-X1) %>% 
  # report_head indicates the start of a note
  mutate(report_head = str_detect(Note, "^admission date"))


# report_head contains the column report_no, a unique identifier for each report
# the report_head dataframe contain report_no, a unique indentifier for each report
report_head <- clinical_notes_raw_data %>% 
  filter(report_head) %>% 
  select(Note, report_head) %>% 
  mutate(report_no = row_number()) 


clinical_notes_data <- clinical_notes_raw_data %>% 
  # joint with report_head dataframe, report_no show which report each sentence belongs to
  left_join(report_head, by = c("Note")) %>% 
  mutate(report_no = na.locf(report_no)) %>% 
  # remove lines with no sentences
  filter(Note != "") %>% 
  tidyr::unite(Categories, contains("category")) %>%
  select(-contains("copy")) %>%
  # remove unnecessary whitespaces
  mutate(note_processed = str_squish(Note))


full_report_label_test <- clinical_notes_data %>% 
  group_by(report_no) %>% 
  summarize(all_cats = str_c(Categories, collapse = "_")) %>%
  mutate(all_cats = str_replace_all(string = all_cats, 
                                    pattern = "_NA|NA_|NA", 
                                    replacement = ""))

full_clinical_notes_with_labels_test <- clinical_notes_data %>% 
  group_by(report_no) %>% 
  summarise(note_processed_all = str_c(note_processed, collapse = " ")) %>% 
  left_join(full_report_label_test, by = c("report_no")) %>%
  mutate(all_cats = str_split(all_cats, "_"),
         n_cats = NA) # total number of symptoms identified in report

# get unique categories and total number of categories for each report
for (i in 1:nrow(full_clinical_notes_with_labels_test)) {
  full_clinical_notes_with_labels_test$all_cats[[i]] = unique(full_clinical_notes_with_labels_test$all_cats[[i]])
  full_clinical_notes_with_labels_test$n_cats[[i]] = length(full_clinical_notes_with_labels_test$all_cats[[i]])
}

max(full_clinical_notes_with_labels_test$n_cats) # max number of categories

full_docs = full_clinical_notes_with_labels_test %>%
  rowwise %>%
  mutate(all_cats = str_c(all_cats, collapse = "_")) %>%
  separate(all_cats, 
           into = c("Category_1", "Category_2", "Category_3", "Category_4", "Category_5", "Category_6", "Category_7", "Category_8"), 
           sep = "_") %>%
  mutate_if(is_character, funs(na_if(.,"")),) %>% # replace empty strings with NA
  mutate(n_cats = ifelse(is.na(Category_1), 0, n_cats))


write.csv(full_docs, file = file.path(data_folder, "gold_standard_FULL-REPORT.csv"),
          row.names = FALSE)
