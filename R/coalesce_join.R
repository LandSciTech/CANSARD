#' Join and Coalesce
#'
#' Join two dfs and then combine duplicate columns by replacing NAs in x
#' with y
#'
#' @param x first data frame
#' @param y second data frame
#' @param by columns to join by
#' @param join type of join

coalesce_join <- function(x, y, by, join = full_join){
  x %>%
    join(y, by = by) %>%
    bind_cols(purrr::map_dfc(grep("\\.x", names(.), value = TRUE), function(x){
      purrr::set_names(
        list(coalesce(.[[x]], .[[gsub("\\.x", ".y", x)]])),
        gsub("\\.x", "", x)
      )
    })) %>%
    select(union(names(x), names(y)))
}
