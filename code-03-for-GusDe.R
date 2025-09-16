library(tidyverse)
library(DBI)
library(RSQLite)

kahler <- DBI::dbConnect(RSQLite::SQLite(), "kahler.sqlite")

lang_df <- read_rds("lang_df.rds")

kahler_stem_and_subentry <- tbl(kahler, "full") |> 
  collect()

kahler_stem_and_subentry |> colnames()
# [1] "ID"                                  "stem_id"                             "kms_Alphabet"                       
# [4] "kms_page"                            "kms_entry_no"                        "stem_form"                          
# [7] "stem_form_comm_untokenised"          "stem_homonymID"                      "stem_DE"                            
# [10] "stem_EN"                             "stem_IDN"                            "stem_formVarian"                    
# [13] "stem_formVarian_untokenised"         "stem_formVarian_tokenised"           "stem_variant_DE"                    
# [16] "stem_variant_EN"                     "stem_variant_IDN"                    "stem_dialectVariant"                
# [19] "stem_etymological_form"              "stem_etym_form_German"               "stem_etymological_language_donor"   
# [22] "stem_source_form"                    "stem_source_form_homonymID"          "stem_remark_DE"                     
# [25] "stem_remark_EN"                      "stem_remark_IDN"                     "stem_crossref_DE"                   
# [28] "stem_crossref_EN"                    "stem_crossref_IDN"                   "stem_form_comm_tokenised"           
# [31] "example_id"                          "example_form"                        "example_form_comm_untokenised"      
# [34] "example_form_comm_tokenised"         "ex_DE"                               "ex_EN"                              
# [37] "ex_IDN"                              "example_variant"                     "ex_variant_comm_untokenised"        
# [40] "ex_variant_comm_tokenised"           "ex_variant_DE"                       "ex_variant_EN"                      
# [43] "ex_variant_IDN"                      "example_dialect_variant"             "example_etymological_form"          
# [46] "example_etymological_language_donor" "example_loanword_form"               "example_loanword_language_donor"    
# [49] "example_source_form"                 "example_source_form_homonymID"       "ex_remark_DE"                       
# [52] "ex_remark_EN"                        "ex_remark_IDN"                       "ex_crossref_DE"                     
# [55] "ex_crossref_EN"                      "ex_crossref_IDN"                     "ex_concept"                         
# [58] "ex_variant_concept"                  "ex_remark_concept"                   "ex_crossref_concept"                
# [61] "stem_loan_form"                      "stem_loan_lang"                     

mydb <- kahler_stem_and_subentry |> 
  select(ID, stem_id, kms_Alphabet, kms_page, kms_entry_no, stem_forms = stem_form_comm_untokenised, 
         stem_homonymID, stem_GERMAN = stem_DE, stem_ENGLISH = stem_EN, stem_INDONESIAN = stem_IDN,
         stem_form_variant = stem_formVarian_untokenised, stem_variant_GERMAN = stem_variant_DE,
         stem_variant_ENGLISH = stem_variant_EN, stem_variant_INDONESIAN = stem_variant_IDN,
         stem_dialectVariant, stem_etymological_form, stem_etym_form_German,
         stem_etymological_language_donor, stem_source_form, stem_source_form_homonymID, 
         stem_loanword_form = stem_loan_form, 
         stem_loanword_language_donor = stem_loan_lang, 
         stem_remark_GERMAN = stem_remark_DE,
         stem_remark_ENGLISH = stem_remark_EN, 
         stem_remark_INDONESIAN = stem_remark_IDN,
         stem_crossref_GERMAN = stem_crossref_DE, 
         stem_crossref_ENGLISH = stem_crossref_EN,
         stem_crossref_INDONESIAN = stem_crossref_IDN,
         example_id,
         example_forms = example_form_comm_untokenised,
         example_GERMAN = ex_DE,
         example_ENGLISH = ex_EN,
         example_INDONESIAN = ex_IDN,
         example_variantForms = ex_variant_comm_untokenised,
         example_variantForms_GERMAN = ex_variant_DE,
         example_variantForms_ENGLISH = ex_variant_EN,
         example_variantForms_INDONESIAN = ex_variant_IDN,
         example_dialectVariant = example_dialect_variant,
         example_etymological_form,
         sw_id = example_etymological_language_donor,
         example_loanword_form,
         # example_loanword_language_donor, <- no data for this column
         example_source_form,
         example_source_form_homonymID,
         example_remark_GERMAN = ex_remark_DE,
         example_remark_ENGLISH = ex_remark_EN,
         example_remark_INDONESIAN = ex_remark_IDN,
         example_crossref_GERMAN = ex_crossref_DE,
         example_crossref_ENGLISH = ex_crossref_EN,
         example_crossref_INDONESIAN = ex_crossref_IDN) |> 
  distinct()
lang_df1 <- lang_df |> 
  rename(example_etymological_language_donor = sw_name) |> 
  mutate(sw_id = as.character(sw_id))
mydb1 <- mydb |> 
  left_join(lang_df1) |> 
  select(-sw_id) |> 
  relocate(example_etymological_language_donor,
           .before = example_loanword_form) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(across(where(is.double), ~replace_na(., 0L))) |> 
  mutate(across(where(is.integer), ~replace_na(., 0L))) |> 
  mutate(ID_unique = row_number()) |> 
  relocate(ID_unique, .before = ID)

# when ID is duplicated, this could be because the stem_form has several example_form
mydb1 |> 
  filter(ID == 3323) |> as.data.frame()

# kahler_dict_folder <- "https://drive.google.com/drive/folders/1KY-XunZ0mzkXxCJbbrOFDH1vgmCBTpSV"
# 
# googledrive::drive_create(name = "kahler_shiny_db_for_gusde",
#                           path = kahler_dict_folder,
#                           type = "spreadsheet")
# Auto-refreshing stale OAuth token.
# Created Drive file:
#   • kahler_shiny_db_for_gusde <id: 1CYwstiYwhgSNrFcvjTFripmWhFifSeBYRsh7PJgas58>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

# googledrive::drive_create(name = "kahler_shiny_db_column_explanation",
#                           path = kahler_dict_folder,
#                           type = "spreadsheet")
# Created Drive file:
#   • kahler_shiny_db_column_explanation <id: 12DrW4lPcJqQqGxxHROJ4RzHFBU16HMzRNkNPI-MJPzY>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

# googlesheets4::sheet_write(mydb1, ss = '1CYwstiYwhgSNrFcvjTFripmWhFifSeBYRsh7PJgas58', sheet = 'Sheet1')
# googlesheets4::sheet_write(tibble::tibble(column_ID = 1:length(colnames(mydb1)), columns = colnames(mydb1), explanation = ""),
#                            ss = "12DrW4lPcJqQqGxxHROJ4RzHFBU16HMzRNkNPI-MJPzY",
#                            sheet = "Sheet1")
