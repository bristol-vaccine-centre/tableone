## Huxtable utils ----
# when used outside of 'ggrrr' the project must import
# systemfonts, huxtable, dplyr, tidyr, utils

# .check_font("Helvetica")
.check_font = function(family) {
  match = systemfonts::match_font(family)
  family2 = systemfonts::system_fonts() %>% dplyr::filter(path == match$path) %>% dplyr::pull(family) %>% unique()
  if(length(family2) == 0) stop("No suitable font substitute for: ",family)
  return(family2[[1]])
}

# A tidy article theme for huxtables
.hux_default_layout = function(hux, defaultFontSize=8, defaultFont = "Roboto", headerRows = 1) {
  defaultFont = .check_font(defaultFont)
  if(!huxtable::is_hux(hux)) hux = huxtable::as_hux(hux)
  return( hux %>%
            huxtable::set_font_size(huxtable::everywhere,huxtable::everywhere,defaultFontSize) %>%
            huxtable::set_font(huxtable::everywhere,huxtable::everywhere,defaultFont) %>%
            huxtable::set_top_border(1, huxtable::everywhere, 1) %>%
            huxtable::set_bottom_border(headerRows, huxtable::everywhere, 1) %>%
            huxtable::set_bottom_border(nrow(hux), huxtable::everywhere, 1) %>%
            huxtable::set_wrap(huxtable::everywhere, huxtable::everywhere, TRUE) %>%
            huxtable::set_top_padding(huxtable::everywhere,huxtable::everywhere,1) %>%
            huxtable::set_bottom_padding(huxtable::everywhere,huxtable::everywhere,0) %>%
            huxtable::set_valign(huxtable::everywhere,huxtable::everywhere,"top")
  )
}

# Set the font family and size in a huxtable globally
.hux_set_font = function(hux, defaultFontSize=8, defaultFont = "Roboto") {
  defaultFont = .check_font(defaultFont)
  hux %>%
    huxtable::set_font_size(huxtable::everywhere,huxtable::everywhere,defaultFontSize) %>%
    huxtable::set_font(huxtable::everywhere,huxtable::everywhere,defaultFont)
}


# Convert a dataframe to a huxtable with nested rows and columns.
.hux_tidy = function(tidyDf, rowGroupVars, colGroupVars, missing="\u2014", na="\u2014", ...) {

  name = .y = .x = value = rows = NULL  # remove global binding note

  if(tidyDf %>% dplyr::group_by(!!!colGroupVars,!!!rowGroupVars) %>% dplyr::count() %>% dplyr::pull(n) %>% max() > 1) stop("rowGroupVars and colGroupVars do not define unique rows (did you forget to summarise?)")

  cols = lapply(colnames(tidyDf),as.symbol)
  data = colnames(tidyDf)[!colnames(tidyDf) %in% sapply(c(rowGroupVars, colGroupVars),rlang::as_label)]

  preserveDataOrder = !(tidyDf %>% dplyr::select(!!!rowGroupVars,!!!colGroupVars) %>%
                          sapply(function(c) is.factor(c)) %>% all())

  if (preserveDataOrder) {
    colJoin = sapply(colGroupVars, rlang::as_label) %>% unlist() %>% as.character()
    rowJoin = sapply(rowGroupVars, rlang::as_label) %>% unlist() %>% as.character()
    colOrder = tidyDf %>% dplyr::select(!!!colGroupVars) %>% dplyr::distinct() %>% dplyr::mutate(.x = dplyr::row_number())
    rowOrder = tidyDf %>% dplyr::select(!!!rowGroupVars) %>% dplyr::distinct() %>% dplyr::mutate(.y = dplyr::row_number())
    tmp = tidyDf %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(.cols = tidyr::all_of(data), as.character)) %>%
      tidyr::pivot_longer(cols = data) %>%
      dplyr::mutate(name = factor(name,levels=data)) %>%
      dplyr::inner_join(colOrder, by=colJoin) %>%
      dplyr::inner_join(rowOrder, by=rowJoin) %>%
      dplyr::group_by(!!!colGroupVars,!!!rowGroupVars) %>%
      dplyr::mutate(.x = (.x-1)*dplyr::n()+dplyr::row_number())

  } else {
    tmp = tidyDf %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(.cols = tidyr::all_of(data), as.character)) %>%
      tidyr::pivot_longer(cols = data) %>%
      dplyr::mutate(name = factor(name,levels=data)) %>%
      #TODO formatters?
      dplyr::ungroup() %>%
      dplyr::group_by(!!!colGroupVars,name) %>%
      dplyr::arrange(!!!rowGroupVars) %>%
      dplyr::mutate(.x = dplyr::cur_group_id()) %>%
      dplyr::group_by(!!!rowGroupVars) %>%
      dplyr::mutate(.y = dplyr::cur_group_id())
  }

  # browser()

  rowHeadings = tmp %>% dplyr::ungroup() %>% dplyr::select(!!!rowGroupVars,.y) %>% dplyr::arrange(.y) %>% dplyr::distinct()
  colHeadings = tmp %>% dplyr::ungroup() %>% dplyr::select(!!!colGroupVars,name,.x) %>% dplyr::arrange(.x) %>% dplyr::distinct()

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
    huxtable::set_header_cols(1:xOffset, TRUE) %>%
    .hux_default_layout(headerRows = yOffset, ...)

  # do column merges
  tmpVars = colGroupVars
  while(length(tmpVars)>0) {
    for( mergeCols in colHeadings %>% dplyr::group_by(!!!tmpVars) %>% dplyr::summarise(cols = list(unique(.x))) %>% dplyr::pull(cols)) {
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
    rowGroups = rowHeadings %>% dplyr::group_by(!!!tmpVars) %>% dplyr::summarise(rows = list(unique(.y)), count=length(unique(.y)))
    # do the merge if and only if there are multiple rows in at least one group.
    if(any(rowGroups$count > 1)) {
      for( mergeRows in rowGroups %>% dplyr::pull(rows)) {
        # mergeCols = colHeadings %>% dplyr::group_by(!!!tmpVars) %>% dplyr::group_data() %>% dplyr::pull(.rows) %>% `[[`(1)
        colIndex = length(tmpVars)
        l = min(mergeRows)+yOffset
        lr = c(min(mergeRows),max(mergeRows))+yOffset
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
