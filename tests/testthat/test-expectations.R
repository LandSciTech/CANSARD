# so far just a list of expectations for incoming data

#1 Incoming threats data should have unique combination of uID and threat_num
# db_2021_th %>% mutate(threat_num = str_replace(threat_num, ",", ".")) %>%
#   group_by(uID, threat_num) %>%
#   summarise(N = n()) %>%
#   filter(N > 1)

#2 threat_num should have . for decimal not , and generally match expected values

#3 Need to handle mixed dates in inputs ie sometimes it is just a year sometimes
# excel formatted date
# db_2021_th <- mutate(
#   db_2021_th,
#   TC_date = case_when(nchar(TC_date) == 6 ~ str_replace(TC_date, "\\.0", ""),
#                       TRUE ~ TC_date) %>%
#     janitor::convert_to_date(
#       character_fun = function(x){
#         lubridate::parse_date_time(x, orders = c("Y", "Ydm"), truncated = 3) %>%
#           as.Date()
#       })
# )

#4 In the future the Threats Calculator data sheet should also include the docID
#from the extracted data sheet to make sure that the correct TC data is recorded
#for each doc if it is included in both.

#5 All rows in TC_columns should be the same

#6 uID can never be NA

#7 If there are level 2 impacts (even negligible) there must be level 1 impacts.
#If only 1 level 2 scores for level 1 are same. If multiple, scores for level 1
#are combination based on overlap (can't automate).

#8 Threats calculator values must be in expected strings see format_threats threat_code
