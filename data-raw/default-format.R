## code to prepare `.default.format` dataset goes here

### formatting column mappers ----

default.format = list(

  relaxed = list(
    # can use {level}, {prob.0.5}, {prob.0.025}, {prob.0.975}, {unit}, {n}, {N}
    # n is subgroup count, N is data count.
    subtype_count = list(
      characteristic = "{level} % [95% CI]",
      value = "{.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%]',prob.0.5*100,prob.0.025*100,prob.0.975*100)}",
      count = "{.sprintf_na('%d/%d',n,N)}"
    ),
    # can use {q.0.5}, {q.0.25}, ..., {unit}, {n}, {N} - n excludes missing, N does not.
    median_iqr = list(
      characteristic = "Median [IQR]",
      value = "{.sprintf_na('%1.3g %s [%1.3g\u2014%1.3g]',q.0.5,unit,q.0.25,q.0.75)}",
      count = "{.sprintf_na('%d',n)}"
    ),
    # can use {mean}, {sd}, {unit}, {n}, {N} - n excludes missing, N does not.
    mean_sd = list(
      characteristic = "Mean \u00B1 SD",
      value = "{.sprintf_na('%1.3g %s \u00B1 %1.3g',mean,unit,sd)}",
      count = "{.sprintf_na('%d',n)}"
    ),
    # can use {mean}, {sd}, {unit}, {n}, {N} - n excludes missing, N does not.
    skipped = list(
      characteristic = "\u2014",
      value = "\u2014",
      count = "{.sprintf_na('%d',n)}"
    )
  ),

  compact = list(
    subtype_count = list(
      characteristic = "{level} % [95% CI] (N)",
      value = "{.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%] (%d)',prob.0.5*100,prob.0.025*100,prob.0.975*100,n)}"
    ),
    median_iqr = list(
      characteristic = "Median [IQR] (N)",
      value = "{.sprintf_na('%1.3g [%1.3g\u2014%1.3g] (%d)',q.0.5,q.0.25,q.0.75,n)}"
    ),
    mean_sd = list(
      characteristic = "Mean \u00B1 SD (N)",
      value = "{.sprintf_na('%1.3g \u00B1 %1.3g (%d)',mean,sd,n)}"
    ),
    skipped = list(
      characteristic = "(N)",
      value = "{.sprintf_na('\u2014 (%d)',n)}"
    )
  ),

  # TODO: putting N into the column name wouldn't work as it is generated as a wide
  # format olumn header (is it now...?) if it were generated as a long format we could
  # support this idea but we woudl need to convert teh names to a factor and push the
  # name into final layer of table nesting to ge the right columns. The problem
  # then is that characteristic is handled differently as it is essentially a row label...
  # we might have to do that as a special case...
  # micro = list(
  #   subtype_count = list(
  #     characteristic = "{level} % (N)",
  #     "value (N={N})" = "{.sprintf_na('%1.1f%% (%d)',prob.0.5*100,n)}"
  #   ),
  #   median_iqr = list(
  #     characteristic = "Median [IQR]",
  #     "value (N={N})" = "{.sprintf_na('%1.3g [%1.3g\u2014%1.3g]',q.0.5,q.0.25,q.0.75)}"
  #   ),
  #   mean_sd = list(
  #     characteristic = "Mean \u00B1 SD",
  #     "value (N={N})" = "{.sprintf_na('%1.3g \u00B1 %1.3g',mean,sd)}"
  #   ),
  #   skipped = list(
  #     characteristic = "\u2014",
  #     "value (N={N})" = "{.sprintf_na('\u2014')}"
  #   )
  # ),

  simple = list(
    subtype_count = list(
      characteristic = "{level}",
      value = "{.sprintf_na('%1.1f%% (%d/%d)',prob.0.5*100,n,N)}"
    ),
    median_iqr = list(
      characteristic = "Median (N)",
      value = "{.sprintf_na('%1.3g (%d)',q.0.5,n)}"
    ),
    mean_sd = list(
      characteristic = "Mean (N)",
      value = "{.sprintf_na('%1.3g (%d)',mean,n)}"
    ),
    skipped = list(
      characteristic = "(N)",
      value = "{.sprintf_na('\u2014 (%d)',n)}"
    )
  ),

  single = list(
    subtype_count = list(
      characteristic = "{level} % [95% CI]",
      value = "{.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%]',prob.0.5*100,prob.0.025*100,prob.0.975*100)}",
      "count (N={N_total})" = "{.sprintf_na('%d/%d',n,N)}"
    ),
    median_iqr = list(
      characteristic = "Median [IQR]",
      value = "{.sprintf_na('%1.3g%s [%1.3g\u2014%1.3g]',q.0.5,.maybe(unit),q.0.25,q.0.75)}",
      "count (N={N_total})" = "{.sprintf_na('%d',n)}"
    ),
    mean_sd = list(
      characteristic = "Mean \u00B1 SD",
      value = "{.sprintf_na('%1.3g%s \u00B1 %1.3g',mean,.maybe(unit),sd)}",
      "count (N={N_total})" = "{.sprintf_na('%d',n)}"
    ),
    skipped = list(
      characteristic = "",
      value = "\u2014",
      "count (N={N_total})" = "{.sprintf_na('%d',n)}"
    )
  ),

  missing = list(
    # can use {level}, {prob.0.5}, {prob.0.025}, {prob.0.975}, {unit}, {n}, {N}
    # n is subgroup count, N is data count.
    subtype_count = list(
      type = "{level}",
      "missing % (N)" = "{.sprintf_na('%1.1f%% (%d/%d)',prob.0.5*100,n,N)}"
    ),
    # never used
    median_iqr = list(),
    # never used
    mean_sd = list(),
    # never used
    skipped = list()
  )
)

usethis::use_data(default.format, overwrite = TRUE)

