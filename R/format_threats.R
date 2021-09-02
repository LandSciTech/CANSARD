threat_code <- structure(
  list(
    code = c(0, 1, 2, 3, 4, 5, -1, -2, -2, 1),
    impact = c("not a threat", "negligible", "low", "medium", "high",
               "very high", "unknown", "not calculated", "not scored",
               "insignificant"),
    scope = c("not a threat", "negligible", "small", "restricted",
              "large", "pervasive", "unknown", "not calculated", "not scored",
              "insignificant"),
    severity = c("neutral or potential benefit", "negligible", "slight",
                 "moderate", "serious", "extreme", "unknown", "not calculated",
                 "not scored", "insignificant"),
    timing = c("not a threat", "negligible", "low", "moderate", "high",
               NA, "unknown", "not calculated", "not scored", "insignificant")
  ),
  row.names = c(NA,-10L), class = c("tbl_df", "tbl", "data.frame")
)


#
# x <- "Extreme - Moderate (11-100%)"
# var <- "severity"
#' Convert threats from text to codes used in database
#'
#' If a range of threat levels is given then the average of the two threat codes
#' is used. E.g. a threat level of "Extreme - Moderate (11-100%)" would get a
#' code of 4 which is the average of 3 (Moderate) and 5 (Extreme).
#'
#' @param x A character vector with the threat level
#' @param var which threat component is being converted
#' @param threat_code A data.frame with corresponding threat levels and codes
#'
#' @return a data.frame
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr

recode_threat <- Vectorize(function(x, var, threat_code = threat_code){
  if(is.na(x)){
    return(NA_real_)
  }

  x_in <- x

  x <- x %>% str_to_lower() %>% str_replace("very high", "very") %>%
    str_which(str_replace(threat_code[[var]], "very high", "very"))

  code <- threat_code$code[x] %>% unique()
  if(length(code) == 2){

    code <- (code[1] + code[2])/2
    return(code)
  } else if(length(code) == 1){
    return(code)
  } else if(length(code) > 2) {
    code <- 100
    #return(x_in)
  } else if(length(code) == 0){
    code <- 99
  }
  if(code >=99){
    warning("input ", x_in, " does not have a code", call. = FALSE)
  }
  return(code)
}, SIMPLIFY = TRUE, USE.NAMES = FALSE, vectorize.args = "x")



format_threats <- function(df){
  df_out <- df %>% ungroup() %>%
    mutate(
      threat_num = str_replace(threat_num, ",", "."),
      impact = recode_threat(impact, "impact", threat_code),
      scope = scope %>%
        str_replace("-- Uncertainty Ranges --", NA_character_) %>%
        recode_threat( "scope", threat_code),
      severity = severity %>%
        str_replace("-- Uncertainty Ranges --", NA_character_) %>%
        recode_threat("severity", threat_code),
      timing = timing %>%
        str_replace("-- Uncertainty Ranges --", NA_character_) %>%
        recode_threat("timing", threat_code)
    ) %>%
    # if impact is NA but scope or severity are not then it should be based on that
    mutate(
      impact = case_when(is.na(impact) ~ case_when(scope == 1 | severity == 1 ~ 1,
                                                   scope == 0 | severity == 0 ~ 0,
                                                   scope == -1 | severity == -1 ~ -1,
                                                   scope == -2 | severity == -2 ~ -2,
                                                   is.na(scope) & !is.na(severity) ~ -1,
                                                   !is.na(scope) & is.na(severity) ~ -1,
                                                   TRUE ~ impact),
                         TRUE ~ impact),
      threat_identified = if_else(impact == 0|is.na(impact), 0, 1)
      )%>%
    pivot_wider(id_cols = -impact_code,
                names_from = threat_num,
                names_prefix = "X", names_sep = "_iucn_",
                values_from = c(threat_identified, impact, scope, severity,
                                timing, comments),
                values_fill = list(threat_identified = 0, impact = NA_real_,
                                   scope = NA_real_, severity = NA_real_,
                                   timing = NA_real_, comments = NA_character_)) %>%
    # mutate(TC_calculated_overall_impact = recode_threat(TC_calculated_overall_impact,
    #                                                     "impact", threat_code),
    #        TC_assigned_overall_impact = recode_threat(TC_assigned_overall_impact,
    #                                                   "impact", threat_code)) %>%
    rename_all(~str_replace(.x, "(^.{1,17})_iucn_(X.*$)", "\\2_iucn_\\1") %>%
                 str_replace("iucn_threat", "threat"))

  th_cols <- c("X1_threat_identified", "X1.1_threat_identified", "X1.2_threat_identified",
               "X1.3_threat_identified", "X2_threat_identified", "X2.1_threat_identified",
               "X2.2_threat_identified", "X2.3_threat_identified", "X2.4_threat_identified",
               "X3_threat_identified", "X3.1_threat_identified", "X3.2_threat_identified",
               "X3.3_threat_identified", "X4_threat_identified", "X4.1_threat_identified",
               "X4.2_threat_identified", "X4.3_threat_identified", "X4.4_threat_identified",
               "X5_threat_identified", "X5.1_threat_identified", "X5.2_threat_identified",
               "X5.3_threat_identified", "X5.4_threat_identified", "X6_threat_identified",
               "X6.1_threat_identified", "X6.2_threat_identified", "X6.3_threat_identified",
               "X7_threat_identified", "X7.1_threat_identified", "X7.2_threat_identified",
               "X7.3_threat_identified", "X8_threat_identified", "X8.1_threat_identified",
               "X8.2_threat_identified", "X8.3_threat_identified", "X9_threat_identified",
               "X9.1_threat_identified", "X9.2_threat_identified", "X9.3_threat_identified",
               "X9.4_threat_identified", "X9.5_threat_identified", "X9.6_threat_identified",
               "X10_threat_identified", "X10.1_threat_identified", "X10.2_threat_identified",
               "X10.3_threat_identified", "X11_threat_identified", "X11.1_threat_identified",
               "X11.2_threat_identified", "X11.3_threat_identified", "X11.4_threat_identified",
               "X8.4_threat_identified", "X8.5_threat_identified", "X8.6_threat_identified",
               "X11.5_threat_identified")

  #add any missing threat nums
  cols_to_add <- setdiff(th_cols, colnames(df_out))

  if(length(cols_to_add) > 0){
    alt_df <- as.data.frame(matrix(0, ncol = length(cols_to_add)))
    colnames(alt_df) <- cols_to_add

    df_out <- bind_cols(df_out, alt_df)
  }

  return(df_out)

}

check_threats <- function(df){

  df1 <- mutate(df, across(where(is.character), ~.x %>%  str_to_lower() %>%
                        str_replace("very high", "very") %>%
                          str_replace("-- uncertainty ranges --", NA_character_)))
  threat_code <- mutate(threat_code,
                        across(where(is.character),
                               ~str_replace(.x, "very high", "very")))

  threat_cols <- c("impact", "scope", "severity", "timing")

  miss_l <- purrr::map2_dfc(df1[threat_cols], threat_cols,
                                   ~str_detect(.x, paste0(threat_code[[.y]],
                                                          collapse = "|"),
                                               negate = TRUE)) %>%
    rowwise() %>%
    mutate(wch = ifelse(length(threat_cols[which(c(impact, scope, severity,
                                                   timing))]) == 0,
                        NA,
                        threat_cols[which(c(impact, scope, severity, timing))]),
              miss = sum(impact, scope, severity, timing, na.rm = TRUE))

  df2 <- df[which(miss_l$miss > 0), ]
  df2$wch <- filter(miss_l, miss > 0) %>% pull(wch)

  df2 %>% select(uID, common_name, all_of(threat_cols), wch)
}
