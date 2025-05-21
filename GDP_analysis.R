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

#  Q1. For every financial year , which sector has performed well 

final_df %>%
  group_by(year, item) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_gsdp, n = 1)

#Q2 . For every financial year which sector has perfrmed least
final_df %>%
  group_by(year, item) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_min(order_by = total_gsdp, n = 1)

#Q3 For every financial year which state has performed well 
final_df %>%
  group_by(year, state) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_gsdp, n = 1)

#Q4.For every financial year which state has performed least 
final_df %>%
  group_by(year, state) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_min(order_by = total_gsdp, n = 1)

# Q5. Top 5 performing states in Manufacturing
final_df %>%
  filter(item=="Manufacturing") %>% 
  group_by(state) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(total_gsdp)) %>%
  slice_head(n = 5)

#Q6. Top 5 performing states in Construction

final_df %>%
  filter(item=="Construction") %>% 
  group_by(state) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(total_gsdp)) %>%
  slice_head(n = 5)

#7. For financial year 2016-17, for every state get top performing sector
final_df %>%
  filter(year == "2016-17") %>% 
  group_by(state, item) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
  group_by(state) %>%
  slice_max(order_by = total_gsdp, n = 1)

#8. For financial year 2016-17, for every state get top 5 performing sectors
final_df %>%
  filter(year == "2016-17") %>% 
  group_by(state, item) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
  group_by(state) %>%
  slice_max(order_by = total_gsdp, n = 5)
#9. How many states are performing well in Manufacturing, (if Manufacturing is in top 3)
final_df %>%
  group_by(state, item) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
  group_by(state) %>% 
  slice_max(order_by = total_gsdp, n = 3)-> df_c %>% 
  View(df_c) %>% 
  filter(item == "Manufacturing")%>% 
  summarise(no_of_states = n_distinct(state))

#10. What is the GROSS GSDP of Karnataka for all financial years
final_df %>%
  filter(state == "Karnataka") %>% 
  group_by(year) %>%
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop")  

# ## 11. what is the percentage contribution of each sector  in the total gsdp of all sectors for  karnataka in the year 2015-16
# final_df %>%
#   filter(state == "Karnataka") %>%
#   filter(year == "2015-16") %>%
#   summarise(total_gsdp = sum(gsdp, na.rm = TRUE))


## 11. what is the percentage contribution of each sector  in the total gsdp of all sectors for  karnataka in the year 2015-16
  final_df %>%
  filter(state == "Karnataka", year == "2015-16") %>%
  group_by(item) %>%
  summarise(sector_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_gsdp = sum(sector_gsdp),
    percentage_contribution = round((sector_gsdp / total_gsdp) * 100, 2)
  ) %>%
  select(item, percentage_contribution)



  