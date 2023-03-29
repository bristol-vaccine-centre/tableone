

#' Group data count and calculate proportions by column.
#'
#' @param df a dataframe of linelist items
#' @param rowGroupVars the rows of the table. The last one of these is the denominator grouping
#' @param colGroupVars the column groupings of the table.
#' @param numExpr defines how the numerator is defined in the context of the column and row groups (e.g. dplyr::n())
#' @param denomExpr defines how the numerator is defined in the context of the column and row (ungrouped one level)
#' @param totalExpr defines how the column level total is defined
#' @param subgroupLevel defines how the numerator grouping is defined in terms of the row groupings
#' @param glue a named list of column value specifications.
#'
#' @return a huxtable with the count and proportions of the rows groups
#' @export
#'
#' @examples
#' diamonds %>% count_table(dplyr::vars(cut,clarity), dplyr::vars(color), subgroupLevel = 1)
count_table = function(df, rowGroupVars, colGroupVars, numExpr = dplyr::n(), denomExpr = dplyr::n(), totalExpr = dplyr::n(), subgroupLevel = length(rowGroupVars), glue = list(
  # 'Count (N={sprintf("%d",n)})' = '{sprintf("%d", x)}',
  # 'Proportion [95% CI]' = '{sprintf("%1.1f%% [%1.1f \u2014 %1.1f]", mean*100, lower*100, upper*100)}'
  'Count [%] (N={sprintf("%d",N)})' = '{sprintf("%d/%d [%1.1f%%]", x, n, mean*100)}'
)) {

  .count_data(
    df, rowGroupVars, colGroupVars, {{numExpr}}, {{denomExpr}}, {{totalExpr}}, subgroupLevel, glue
  ) %>%
  .hux_tidy(rowGroupVars = rowGroupVars, colGroupVars = c(colGroupVars,dplyr::vars(tag)))

}

.count_data = function(df, rowGroupVars, colGroupVars, numExpr = dplyr::n(), denomExpr = dplyr::n(), totalExpr = dplyr::n(), subgroupLevel = length(rowGroupVars), glue = list(
  # 'Count (N={sprintf("%d",n)})' = '{sprintf("%d", x)}',
  # 'Proportion [95% CI]' = '{sprintf("%1.1f%% [%1.1f \u2014 %1.1f]", mean*100, lower*100, upper*100)}'
  'Count [%] (N={sprintf("%d",N)})' = '{sprintf("%d/%d [%1.1f%%]", x, n, mean*100)}'
)) {

  numExpr = rlang::enexpr(numExpr)
  denomExpr = rlang::enexpr(denomExpr)
  totalExpr = rlang::enexpr(totalExpr)

  # TODO: nesting of the denominator in row groups (n) vs by column (N). If we
  # do this then we need to differentiate between row level and column level
  # groupings in calculating proportions. Whether we use n (by row group) or N
  # (i.e. by column) for calculating confidence intervals.

  tmp = df %>%
    dplyr::group_by(!!!colGroupVars,!!!rowGroupVars) %>%
    dplyr::mutate(x = !!numExpr) %>%
    dplyr::group_by(!!!colGroupVars, !!!utils::head(rowGroupVars,-subgroupLevel)) %>%
    dplyr::mutate(n = !!denomExpr) %>%
    dplyr::group_by(!!!colGroupVars) %>%
    dplyr::mutate(N = !!totalExpr) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rowGroupVars, !!!colGroupVars, x, n, N) %>%
    dplyr::distinct() %>%
    dplyr::mutate(binom::binom.confint(x,n,method="wilson"))

  tmp3 = tibble::tibble()
  for (colSpec in names(glue)) {
    tmp2 = tmp %>% dplyr::mutate(
      tag = glue::glue(colSpec),
      value = glue::glue(glue[[colSpec]])
    )
    tmp3 = tmp3 %>% dplyr::bind_rows(tmp2)
  }

  tmp3 %>%
    dplyr::select(!!!rowGroupVars, !!!colGroupVars, tag, value)
}
