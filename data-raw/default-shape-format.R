default.group.format = list(
  single = list(
    subtype_count = "{level} {.sprintf_na('%1.1f%% [%1.1f%%\u2014%1.1f%%] (%d/%d)',prob.0.5*100,prob.0.025*100,prob.0.975*100,x,n)}",
    median_iqr = "{.sprintf_na('%1.3g%s [%1.3g\u2014%1.3g]',q.0.5,.maybe(unit),q.0.25,q.0.75)}",
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
  )
)
