## code to prepare `.default.format` dataset goes here

### formatting column mappers ----

default.format = list(

  relaxed = list(
    # can use {level}, {prob.0.5}, {prob.0.025}, {prob.0.975}, {unit}, {n}, {N}
    # n is subgroup count, N is data count.
    subtype_count = list(
      characteristic = "{level} % [95% CI]",
      Value = "{.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%]',prob.0.5*100,prob.0.025*100,prob.0.975*100)}",
      "Count (N={N})" = "{.sprintf_na('%d/%d',x,n)}"
    ),
    # can use {q.0.5}, {q.0.25}, ..., {unit}, {n}, {N} - n excludes missing, N does not.
    median_iqr = list(
      characteristic = "Median [IQR]",
      Value = "{.sprintf_na('%1.3g %s [%1.3g\u2014%1.3g]',q.0.5,unit,q.0.25,q.0.75)}",
      "Count (N={N})" = "{.sprintf_na('%d',n)}"
    ),
    # can use {mean}, {sd}, {unit}, {n}, {N} - n excludes missing, N does not.
    mean_sd = list(
      characteristic = "Mean \u00B1 SD",
      Value = "{.sprintf_na('%1.3g %s \u00B1 %1.3g',mean,unit,sd)}",
      "Count (N={N})" = "{.sprintf_na('%d',n)}"
    ),
    # can use {mean}, {sd}, {unit}, {n}, {N} - n excludes missing, N does not.
    skipped = list(
      characteristic = "\u2014",
      Value = "\u2014",
      "Count (N={N})" = "{.sprintf_na('%d',n)}"
    )
  ),

  compact = list(
    subtype_count = list(
      characteristic = "{level} % [95% CI] (n)",
      "Value (N={N})" = "{.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%] (%d)',prob.0.5*100,prob.0.025*100,prob.0.975*100,x)}"
    ),
    median_iqr = list(
      characteristic = "Median [IQR]",
      "Value (N={N})" = "{.sprintf_na('%1.3g [%1.3g\u2014%1.3g]',q.0.5,q.0.25,q.0.75)}"
    ),
    mean_sd = list(
      characteristic = "Mean \u00B1 SD",
      "Value (N={N})" = "{.sprintf_na('%1.3g \u00B1 %1.3g',mean,sd)}"
    ),
    skipped = list(
      characteristic = "(n)",
      "Value (N={N})" = "{.sprintf_na('\u2014 (%d)',n)}"
    )
  ),

  micro = list(
    subtype_count = list(
      characteristic = "{level} % (n)",
      "Value (N={N})" = "{.sprintf_na('%1.1f%% (%d)',prob.0.5*100,x)}"
    ),
    median_iqr = list(
      characteristic = "Median [IQR]",
      "Value (N={N})" = "{.sprintf_na('%1.3g [%1.3g\u2014%1.3g]',q.0.5,q.0.25,q.0.75)}"
    ),
    mean_sd = list(
      characteristic = "Mean \u00B1 SD",
      "Value (N={N})" = "{.sprintf_na('%1.3g \u00B1 %1.3g',mean,sd)}"
    ),
    skipped = list(
      characteristic = "\u2014",
      "Value (N={N})" = "\u2014"
    )
  ),

  simple = list(
    subtype_count = list(
      characteristic = "{level}",
      "Value (N={N})" = "{.sprintf_na('%1.1f%% (%d/%d)',prob.0.5*100,x,n)}"
    ),
    median_iqr = list(
      characteristic = "Median (n)",
      "Value (N={N})" = "{.sprintf_na('%1.3g (%d)',q.0.5,n)}"
    ),
    mean_sd = list(
      characteristic = "Mean (n)",
      "Value (N={N})" = "{.sprintf_na('%1.3g (%d)',mean,n)}"
    ),
    skipped = list(
      characteristic = "(n)",
      "Value (N={N})" = "{.sprintf_na('\u2014 (%d)',n)}"
    )
  ),

  single = list(
    subtype_count = list(
      characteristic = "{level} % [95% CI]",
      Value = "{.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%]',prob.0.5*100,prob.0.025*100,prob.0.975*100)}",
      "Count (N={N_total})" = "{.sprintf_na('%d/%d',x,n)}"
    ),
    median_iqr = list(
      characteristic = "Median [IQR]",
      Value = "{.sprintf_na('%1.3g%s [%1.3g\u2014%1.3g]',q.0.5,.maybe(unit),q.0.25,q.0.75)}",
      "Count (N={N_total})" = "{.sprintf_na('%d',n)}"
    ),
    mean_sd = list(
      characteristic = "Mean \u00B1 SD",
      Value = "{.sprintf_na('%1.3g%s \u00B1 %1.3g',mean,.maybe(unit),sd)}",
      "Count (N={N_total})" = "{.sprintf_na('%d',n)}"
    ),
    skipped = list(
      characteristic = "",
      Value = "\u2014",
      "Count (N={N_total})" = "{.sprintf_na('%d',n)}"
    )
  ),

  missing = list(
    # can use {level}, {prob.0.5}, {prob.0.025}, {prob.0.975}, {unit}, {n}, {N}
    # n is subgroup count, N is data count.
    subtype_count = list(
      characteristic = "{level}",
      "missing % (N)" = "{.sprintf_na('%1.1f%% (%d/%d)',prob.0.5*100,x,n)}"
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

