#' Join and Coalesce
#'
#' Join two dfs and then combine duplicate columns by replacing NAs in x
#' with y
#'

coalesce_join <- function(x, y, by, join = full_join){
  x %>%
    join(y, by = by) %>%
    bind_cols(map_dfc(grep("\\.x", names(.), value = TRUE), function(x){
      set_names(
        list(coalesce(.[[x]], .[[gsub("\\.x", ".y", x)]])),
        gsub("\\.x", "", x)
      )
    })) %>%
    select(union(names(x), names(y)))
}
