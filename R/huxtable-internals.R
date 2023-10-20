## Huxtable utils ----
# when used outside of 'ggrrr' the project must import
# systemfonts, huxtable, dplyr, tidyr, utils

# .check_font("Helvetica")
.check_font = function(family) {
  f = tryCatch(get("check_font", mode="function", envir = rlang::env_parent()), error = function(e) NULL)
  if (!is.null(f)) return(f(family))
  path = NULL
  match = systemfonts::match_font(family)
  family2 = systemfonts::system_fonts() %>% dplyr::filter(path == match$path) %>% dplyr::pull(family) %>% unique()
  if(length(family2) == 0) stop("No suitable font substitute for: ",family)
  return(family2[[1]])
}

# Fonts and colours from plot ----

#' @noRd
#' @examples
#' .hux_used_fonts(iris %>% hux_default_layout(defaultFont="Roboto"))
.hux_used_fonts = function(hux) {
  tmp2 = attributes(hux)
  return(unique(as.vector(tmp2$font)))
}

# A tidy article theme for huxtables
.hux_default_layout = function(hux, defaultFontSize=8, defaultFont = "Roboto", headerRows = 1) {
  defaultFont = .check_font(defaultFont)
  if(!huxtable::is_hux(hux)) hux = huxtable::as_hux(hux)
  return( hux %>%
            huxtable::set_font_size(huxtable::everywhere,huxtable::everywhere, value = defaultFontSize) %>%
            huxtable::set_all_borders(huxtable::everywhere, huxtable::everywhere, value=0) %>%
            huxtable::set_font(huxtable::everywhere,huxtable::everywhere, value = defaultFont) %>%
            huxtable::set_top_border(1, huxtable::everywhere, value=1) %>%
            huxtable::set_bottom_border(headerRows, huxtable::everywhere, value=1) %>%
            huxtable::set_bottom_border(nrow(hux), huxtable::everywhere, value=1) %>%
            huxtable::set_wrap(huxtable::everywhere, huxtable::everywhere, value=TRUE) %>%
            huxtable::set_top_padding(huxtable::everywhere,huxtable::everywhere, value=0) %>%
            huxtable::set_bottom_padding(huxtable::everywhere,huxtable::everywhere, value=0) %>%
            huxtable::set_left_padding(huxtable::everywhere,huxtable::everywhere, value=2) %>%
            huxtable::set_right_padding(huxtable::everywhere,huxtable::everywhere, value=2) %>%
            huxtable::set_valign(huxtable::everywhere,huxtable::everywhere,value="top")
  )
}

# Set the font family and size in a huxtable globally
.hux_set_font = function(hux, defaultFontSize=8, defaultFont = "Roboto") {
  defaultFont = .check_font(defaultFont)
  hux %>%
    huxtable::set_font_size(huxtable::everywhere,huxtable::everywhere,defaultFontSize) %>%
    huxtable::set_font(huxtable::everywhere,huxtable::everywhere,defaultFont)
}

.hux_add_footer = function(hux, footer) {
  if (!is.null(footer) & length(footer) > 0) {
    hux = hux %>%
      huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="") %>%
      huxtable::set_bottom_border(row=huxtable::final(),value=0)
  }
  return(hux)
}

## symbol conversion ----
.as_symbol_list = function(x,...) {
  UseMethod(".as_symbol_list",x)
}

.as_symbol_list.quosures = function(x,...) {
  x %>% lapply(rlang::as_label) %>% dplyr::syms()
}

.as_symbol_list.list = function(x,...) {
  lapply(x, function(.x) if (is.name(.x)) .x else dplyr::sym(as.character(.x)))
}

.as_symbol_list.default = function(x,...) {
  dplyr::syms(as.character(x))
}

.as_join_list = function(symbols) {
  sapply(symbols, rlang::as_label) %>% unlist() %>% as.character()
}

# tmp = tibble::tibble( x = c("b","a","b","a","b","a"), y = factor(c("d","e","f","d","e","f")))
# .nested_arrange(tmp, dplyr::vars(x,y))
# tmp2 = tibble::tribble(~cat,~char,~grp,~val,~e,~e2,
#   "big","var Z", "level Z", 1,1,1,
#   "big","var Z", "level Y", 2,1,2,
#   "small","var Y", "level Z2", 7,3,1,
#   "small","var Y", "level Y2", 8,3,2,
#   "small","var Y", "missing", 9,3,3,
#   "big","var X", "missing", 5,2,1,
#   "big","var X", "level Z", 6,2,2,
#   "big","var Z", "level X", 3,1,3,
#   "big","var Z", "missing", 4,1,4
# )
# .nested_arrange(tmp2, dplyr::vars(cat,char,grp))
# tmp3 = dplyr::bind_rows(tmp2 %>% dplyr::mutate(bigcat="colgrpZ"), tmp2 %>% dplyr::mutate(bigcat="colgrpA"))
# .nested_arrange(tmp3, dplyr::vars(cat,char,grp))
# .nested_arrange(tmp3, dplyr::vars(bigcat))

# get a groupwise order for a dataframe without using group and arrange which
# enforce alphabetical order on character data. This on the other hand
# sorts by appearance order for characters and factor order by factors.
.nested_arrange = function(tidyDf, groupVars) {
  .o=.o2=NULL
  colOrder = tidyDf %>% dplyr::ungroup() %>% dplyr::select(!!!groupVars) %>% dplyr::distinct() %>% dplyr::mutate(.o="0", .o2=dplyr::row_number())
  mult = ceiling(log10(nrow(colOrder)))
  fmt = paste0("%s-%0",mult,"d")

  for (colGroup in groupVars) {
    col = colOrder %>% dplyr::pull(!!colGroup)
    if (col %>% is.factor()) {
      colOrder = colOrder %>% dplyr::mutate(.o = sprintf(fmt,.o,ifelse(is.na(!!colGroup),10^mult-1,as.integer(!!colGroup))))
    } else {

      # This logic turns out to be unnecessary I think. It woudl be good to test
      # the functionality it was trying to achieve which is the natural ordering of the last column of

      if (rlang::as_label(colGroup) == rlang::as_label(utils::tail(groupVars,1)[[1]])) {
        # a text column in the last group is the row label unless proven otherwise
        # if you want a different order than the exact original data order then
        # convert to a factor
        colOrder = colOrder %>% dplyr::mutate(.o = sprintf(fmt,.o,.o2))
      } else {

        # if the column is not the last one then we want the order to be the
        # unique values of the data in data presentation order
        colOrder = colOrder %>% dplyr::mutate(.o = sprintf(fmt,.o,match(!!colGroup, unique(!!colGroup))))

      }
    }
  }
  colOrder = colOrder %>% dplyr::arrange(.o) %>% dplyr::mutate(.order = dplyr::row_number()) %>% dplyr::select(-.o,-.o2)
  return(tidyDf %>% dplyr::inner_join(colOrder, by=.as_join_list(groupVars)))
}

.fully_tidy = function(df, rowGroupVars, colGroupVars) {
  if (!is.character(rowGroupVars)) rowGroupVars = .as_join_list(rowGroupVars)
  if (!is.character(colGroupVars)) colGroupVars = .as_join_list(colGroupVars)
  left = colnames(df) %>% intersect(rowGroupVars) %>% intersect(col)
  if (length(left)==1) return(TRUE)
  return(FALSE)
}

# Convert a dataframe to a huxtable with nested rows and columns.
.hux_tidy = function(tidyDf, rowGroupVars, colGroupVars, missing="\u2014", na="\u2014", displayRedundantColumnNames = FALSE, ...) {

  if (length(colGroupVars) == 0) {
    displayRedundantColumnNames = TRUE
  }

  name = .y = .x = value = rows = .order = NULL  # remove global binding note
  rowGroupVars = .as_symbol_list(rowGroupVars)
  colGroupVars = .as_symbol_list(colGroupVars)

  if(tidyDf %>% dplyr::group_by(!!!colGroupVars,!!!rowGroupVars) %>% dplyr::count() %>% dplyr::pull(n) %>% max() > 1) stop("rowGroupVars and colGroupVars do not define unique rows (did you forget to summarise?)")

  cols = lapply(colnames(tidyDf),as.symbol)
  data = colnames(tidyDf)[!colnames(tidyDf) %in% sapply(c(rowGroupVars, colGroupVars),rlang::as_label)]
  # dataVars = sapply(data,as.symbol)
  # preserveDataOrder = !(tidyDf %>% dplyr::select(!!!rowGroupVars) %>%
  #                         sapply(function(c) is.factor(c)) %>% all())

  # this is usually correct  we want this to be nested
  # so we really want col1 in order it appears, then col1 & col2, etc.
  tmp = tidyDf %>%
    dplyr::ungroup() %>%
    .nested_arrange(colGroupVars) %>%
    dplyr::rename(.x=.order) %>%
    .nested_arrange(rowGroupVars) %>%
    dplyr::rename(.y=.order)
  tmp = tmp %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(data), as.character)) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(data)) %>%
    # this creates name anv value columns which maybe could collide with existing
    # grouping columns
    dplyr::mutate(name = factor(name,levels=data)) %>%
    dplyr::group_by(!!!colGroupVars,!!!rowGroupVars) %>%
    dplyr::mutate(.x = (.x-1)*dplyr::n()+dplyr::row_number())

    # works for factors:
  # } else {
  #   tmp = tidyDf %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(dplyr::across(.cols = tidyr::all_of(data), as.character)) %>%
  #     tidyr::pivot_longer(cols = data) %>%
  #     dplyr::mutate(name = factor(name,levels=data)) %>%
  #     #TODO formatters?
  #     dplyr::ungroup() %>%
  #     dplyr::group_by(!!!colGroupVars,name) %>%
  #     dplyr::arrange(!!!rowGroupVars) %>%
  #     dplyr::mutate(.x = dplyr::cur_group_id()) %>%
  #     dplyr::group_by(!!!rowGroupVars) %>%
  #     dplyr::mutate(.y = dplyr::cur_group_id())
  # }

  # browser()

  rowHeadings = tmp %>% dplyr::ungroup() %>% dplyr::select(!!!rowGroupVars,.y) %>% dplyr::arrange(.y) %>% dplyr::distinct()
  if (length(unique(tmp$name)) > 1 || displayRedundantColumnNames) {
    colHeadings = tmp %>% dplyr::ungroup() %>% dplyr::select(!!!colGroupVars,name,.x) %>% dplyr::arrange(.x) %>% dplyr::distinct()
  } else {
    colHeadings = tmp %>% dplyr::ungroup() %>% dplyr::select(!!!colGroupVars,.x) %>% dplyr::arrange(.x) %>% dplyr::distinct()
  }

  colHux = as.data.frame(unname(t(colHeadings %>% dplyr::select(-.x))),stringsAsFactors = FALSE)
  colnames(colHux) = 1:length(colHux)

  hux = tmp %>% dplyr::ungroup() %>% dplyr::select(.y,.x,value) %>% dplyr::mutate(value = ifelse(is.na(value), na, value)) %>%
    tidyr::pivot_wider(names_from = .x, values_from = value, values_fill=missing) %>% dplyr::arrange(.y) %>% dplyr::select(-.y)

  rowHux = rowHeadings %>% dplyr::select(-.y) %>% dplyr::mutate(dplyr::across(tidyr::everything(), as.character))

  # browser()
  xOffset = length(colnames(rowHux))
  yOffset = nrow(colHux)
  topCornerHux = as.data.frame(t(matrix(c(rep("",(yOffset-1)*xOffset),colnames(rowHux)),nrow = xOffset,byrow = FALSE)),stringsAsFactors = FALSE)
  colnames(topCornerHux) = colnames(rowHux)
  #browser()
  fullHux = dplyr::bind_cols(
    dplyr::bind_rows(topCornerHux,rowHux),
    dplyr::bind_rows(colHux,hux)
  )

  fullHux = fullHux %>% huxtable::hux(add_colnames = FALSE) %>%
    huxtable::set_header_rows(1:yOffset, TRUE) %>%
    # this leads to small but annoying inherited borders (I think).
    # huxtable::set_header_cols(1:xOffset, TRUE) %>%
    .hux_default_layout(headerRows = yOffset, ...)

  # do column merges
  tmpVars = colGroupVars
  while(length(tmpVars)>0) {
    # This next bit is sensitive to the default behaviour of summarise
    # it throws a message when used outside of a package context
    # but changing it is not a good idea.
    mergeColList = colHeadings %>% dplyr::group_by(!!!tmpVars) %>%
      dplyr::summarise(cols = list(unique(.x))) %>% dplyr::pull(cols)
    for( mergeCols in mergeColList) {
      # mergeCols = colHeadings %>% dplyr::group_by(!!!tmpVars) %>% dplyr::group_data() %>% dplyr::pull(.rows) %>% `[[`(1)
      rowIndex = length(tmpVars)
      l = min(mergeCols)+xOffset
      lr = c(min(mergeCols),max(mergeCols))+xOffset
      #fullHux = fullHux %>% huxtable::set_align(col=lr, row=rowIndex, "center")
      fullHux = fullHux %>% huxtable::merge_cells(col=lr, row=rowIndex)
      # column borders?
    }
    tmpVars = tmpVars %>% utils::head(-1)
  }


  # do row merges
  tmpVars = rowGroupVars
  while(length(tmpVars)>0) {
    # This next bit is sensitive to the default behaviour of summarise
    # it throws a message when used outside of a package context
    # but changing it is not a good idea.
    rowGroups = rowHeadings %>% dplyr::group_by(!!!tmpVars) %>% dplyr::summarise(rows = list(unique(.y)), count=length(unique(.y)))
    # do the merge if and only if there are multiple rows in at least one group.
    if(any(rowGroups$count > 1)) {
      for( mergeRows in rowGroups %>% dplyr::pull(rows)) {
        # mergeCols = colHeadings %>% dplyr::group_by(!!!tmpVars) %>% dplyr::group_data() %>% dplyr::pull(.rows) %>% `[[`(1)
        colIndex = length(tmpVars)
        l = min(mergeRows)+yOffset
        lr = c(min(mergeRows),max(mergeRows))+yOffset
        # TODO: there is an issue in here somehwere.
        # I think if the columns do not nest properly the overlap is
        # badly thrown off. maybe introduced by the keep in order.
        # fullHux = fullHux %>% huxtable::set_valign(lr,colindex,"middle")
        fullHux = fullHux %>% huxtable::merge_cells(row=lr, col=colIndex)
        fullHux = fullHux %>%
          huxtable::set_top_border(l, huxtable::final(ncol(fullHux)-colIndex+1), 0.5) %>%
          # This fills in the bottom border of a merged cell.
          huxtable::set_bottom_border(l, colIndex, 0.5)
        # column borders?
      }
    }
    tmpVars = tmpVars %>% utils::head(-1)
  }

  # Fix merged borders.
  fullHux %>% huxtable::set_bottom_border(nrow(hux), huxtable::everywhere, 0.5)

  return(fullHux)
}


# Make a huxtable narrower
.hux_nest_group = function(t, col=1) {
  # examine content rows
  rows = (1:nrow(t))[!t %>% huxtable::header_rows()]
  # the row spans for this column
  spans = attributes(t)$rowspan[rows,col]
  # to adjust the rows where the row+span is greater than the maximum row+span so far
  toadj = (rows+spans)[rows+spans > cummax(dplyr::lag(rows+spans,default = 0))]
  # reverse them so inserting the rows does not mess up the indices
  toadj = rev(as.integer(names(toadj)))
  t2 = t
  for (row in toadj) {
    # insert the row and copy the content
    t2 = huxtable::insert_row(ht = t2, t[row,1:col],fill = t[row,col], after=row-1)
    # spand all the way accross from col to end
    t2 = t2 %>% huxtable::set_colspan(row = row, col = col, value = ncol(t)-col+1)
    # clear lower border of just spanned columns
    t2 = t2 %>% huxtable::set_bottom_border(row = row, col = col:ncol(t), value = 0)
    # clear the unnested cell
    t2[row+1,col]=""
  }
  # clear the headers for this row (so we can make it small)
  headers = (1:nrow(t))[t %>% huxtable::header_rows()]
  t2[headers,col] = ""
  return(t2)
}



# # TODO: a knitr engine for a long format table
# # Probably needs a longformat -> huxtable converter also.
# .knitr_engine = function(options) {
#   content = options$code
#   # parse content into data frame
#   #
#   #
#   out = "whatever output"
#   # this is for text output... how to return a hux object and have it
#   # rendered by knitr?
#   knitr::engine_output(options, content, out)
# }
#
# knitr::knit_engines$set(tidy_table = .knitr_engine)
