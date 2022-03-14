classify_actions <- function(df, action_table){
  action_table2 <- action_table %>%
    mutate(`Action sub-types` = tolower(`Action sub-types`)) %>%
    group_by(`Type of action`) %>%
    tidyr::nest() %>%
    mutate(data = purrr::map(data, ~paste0(.x$`Action sub-types`,
                                    collapse = "|")))

  df %>%
    mutate(
      CC_action_subtype = stringr::str_replace(
        CC_action_subtype,
        stringr::regex("manage human threats", ignore_case = TRUE),
        "regulate human activities"
      ) %>%
        stringr::str_replace(
          stringr::regex("manage predation", ignore_case = TRUE),
          "Manage native species negatively impacting species at risk"
        ),
      action_subtype = stringr::str_replace(
        action_subtype,
        stringr::regex("manage human threats", ignore_case = TRUE),
        "regulate human activities"
      ) %>%
        stringr::str_replace(",,|, ,", ",") %>%
        stringr::str_replace(
          regex("manage predation", ignore_case = TRUE),
          "Manage native species negatively impacting species at risk"
        )
    ) %>%
    mutate(
      action_type = purrr::map2_dfc(
        action_table2$data,
        action_table2$`Type of action`,
        ~ ifelse(stringr::str_detect(action_subtype %>% tolower(), .x),
                 .y, NA)
      ),
      CC_action_type = purrr::map2_dfc(
        action_table2$data,
        action_table2$`Type of action`,
        ~ ifelse(stringr::str_detect(CC_action_subtype %>% tolower(), .x),
                 .y, NA)
      )
    ) %>%
    mutate(
      action_type = paste(action_type$...1, action_type$...2,
        action_type$...3, action_type$...4,
        sep = ", "
      ) %>%
        stringr::str_replace("^NA, NA, NA, NA$", NA_character_) %>%
        stringr::str_replace_all("NA, |, NA|NA", ""),
      CC_action_type = paste(CC_action_type$...1, CC_action_type$...2,
        CC_action_type$...3, CC_action_type$...4,
        sep = ", "
      ) %>%
        stringr::str_replace("^NA, NA, NA, NA$", NA_character_) %>%
        stringr::str_remove_all("NA, |, NA|NA")
    )
}
