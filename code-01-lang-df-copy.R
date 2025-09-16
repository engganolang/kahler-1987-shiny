db <- "../kahler1987-2023-06-10/data-raw/primary/20230719-kahler-done-master.csv"
readr::read_csv2(file = db,
                 skip = 4, 
                 n_max = 24) |> 
  readr::write_rds(file = "lang_df.rds")