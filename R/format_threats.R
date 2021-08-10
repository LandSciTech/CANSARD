threat_code <- structure(
  list(
    code = c(0, 1, 2, 3, 4, 5, -1, -2),
    impact = c("not a threat", "negligible", "low", "medium", "high",
               "very high", "unknown", "not calculated"),
    scope = c("not a threat", "negligible", "small", "restricted",
              "large", "pervasive", "unknown", "not calculated"),
    severity = c("neutral or potential benefit", "negligible", "slight",
                 "moderate", "serious", "extreme", "unknown", "not calculated"),
    timing = c("not a threat", "negligible", "low", "moderate", "high",
               NA, "unknown", "not calculated")
  ),
  row.names = c(NA,-8L), class = c("tbl_df", "tbl", "data.frame")
)


#
x <- "Extreme - Moderate (11-100%)"
var <- "severity"
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
#' @return
#' @export
#'
#' @examples
recode_threat <- Vectorize(function(x, var, threat_code = threat_code){
  if(is.na(x)){
    return(NA_real_)
  }

  x_in <- x

  x <- x %>% str_to_lower() %>% str_replace("very high", "very") %>%
    str_which(str_replace(threat_code[[var]], "very high", "very"))
  if(length(x) == 2){

    code <- (threat_code$code[x[1]] + threat_code$code[x[2]])/2
  } else if(length(x) == 1){

    code <- threat_code$code[x]
  } else {

    return(x_in)
  }
  if(length(code) == 0){
    code <- 99
  }
  return(code)
}, SIMPLIFY = TRUE, USE.NAMES = FALSE, vectorize.args = "x")



format_threats <- function(df){
  df %>% ungroup() %>%
    mutate(
      threat_num = str_replace(threat_num, ",", "."),
      threat_identified = if_else(str_detect(impact, "Not a Threat")|
                                    is.na(impact),
                                  0, 1),
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
    pivot_wider(id_cols = -impact_code,
                names_from = threat_num,
                names_prefix = "X", names_sep = "_iucn_",
                values_from = c(threat_identified, impact, scope, severity,
                                timing, comments)) %>%
    # mutate(TC_calculated_overall_impact = recode_threat(TC_calculated_overall_impact,
    #                                                     "impact", threat_code),
    #        TC_assigned_overall_impact = recode_threat(TC_assigned_overall_impact,
    #                                                   "impact", threat_code)) %>%
    rename_all(~str_replace(.x, "(^.{1,17})_iucn_(X.*$)", "\\2_iucn_\\1") %>%
                 str_replace("iucn_threat", "threat"))
}
