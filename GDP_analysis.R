library(tidyverse)
library(janitor)
library(stringr)

read_csv("C:/Sirpi R Classes/R class 2/GDP-analysis/GDP_Data/GSDP.csv")->df1
View(df1)

read_csv("C:/Sirpi R Classes/R class 2/GDP-analysis/GDP_Data/NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv")->df_ap
View(df_ap)

"NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv" %>% 
  str_split("-") %>% 
  unlist()-> state_name_vector
state_name_vector[2]->st_name

df_ap %>% 
  slice(-c(7,11,27:33)) %>% 
  pivot_longer(-c(1,2), names_to = "year", values_to = "gsdp") %>% 
  janitor::clean_names() %>% 
  select(-1) %>% 
  mutate(state=st_name)-> df3
View(df3)


folder_path <- "C:/Sirpi R Classes/R class 2/GDP-analysis/GDP_Data"

file_list <- list.files(folder_path, pattern = "^NAD-.*\\.csv$", full.names = TRUE)

final_df <- data.frame()

for (file in file_list) {
  
  df <- read_csv(file)
  
  state_name <- str_split(basename(file), "-") %>% unlist() %>% .[2]
  
  df_clean <- df %>%
    slice(-c(7, 11, 27:33)) %>%
    pivot_longer(-c(1, 2), names_to = "year", values_to = "gsdp") %>%
    janitor::clean_names() %>%
    select(-1) %>%
    mutate(state = state_name)
  
  final_df <- bind_rows(final_df, df_clean)
}

View(final_df)


# ## step 1
# dir(path = "GDP Data",
#     pattern ="NAD") -> state_files
# 
# for (i in state_files) {
#   print(paste0("File name: ",i))
# }
# 
# # Step 2 == extract state names form file name
# #print(paste0("File Name: ", st_name))
# 
# for (i in state_files) {
#   i %>% 
#     str_split("-") %>% 
#     unlist() -> state_name_vector
#   
#   state_name_vector[2] -> st_name
#   
#   print(paste0("State Name: ", st_name))
# }
# 
# #Step3 read all csv fies
# tempdf <- tibble()
# for (i in state_files) {
#   i %>% 
#     str_split("-") %>% 
#     unlist() -> state_name_vector
#   
#   state_name_vector[2] -> st_name
#   
#   print(paste0("State Name: ", st_name))
#   
#   paste0("GDP Data/",i) %>% 
#     read_csv() -> st_df1
#   
#   st_df1 %>% 
#     slice(-c(7, 11, 27:33)) %>% 
#     pivot_longer(-c(1, 2), names_to = "year", values_to = "gsdp") %>% 
#     clean_names() %>% 
#     select(-1) %>% 
#     mutate(state = st_name) -> state_df
#   print(state_df)t 
#   bind_rows(tempdf,state_df) -> tempdf
# }
# 
# tempdf -> final_statewise_gsdp
getwd()

