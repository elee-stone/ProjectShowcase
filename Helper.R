#Helpers

library(data.table)
library(magrittr)
library(odbc)
library(RODBC)
library(tidyverse)

con<- odbcConnect("DB")

FundQuery  <- stringr::str_c(
  'select
  f.fundName
  ,fe.wsj_symbol as abbreviation
  ,r.profile_date as PROFILE_DATE 
  ,r.return_code
  ,r.1mReturn
  ,r.1yReturn
  ,r.3yReturn
  ,r.5yReturn
  ,r.7yReturn
  ,r.10yReturn
  ,r.15yReturn
  from fund f
  left join monthly_returns r on f.fund_id = r.fund_id
  left join fund_ext fe on f.fund_id = fe.fund_id
  where f.fund_id in
  (fund numbers
  )
  and return_code in (0, 8, 24)
  ') %>%
  sqlQuery(con, ., stringsAsFactors = F) %>% 
  data.table::data.table(.)  %>%
  data.table::melt(., id.vars = 1:4, variable.name = "TIME_PERIOD",
                   value.name = "RETURN", variable.factor = F) %>%
  .[!is.na(RETURN)] %>%
  .[, PROFILE_DATE := as.Date(PROFILE_DATE )] %>%
  .[, return_code := as.character(return_code)]

odbcCloseAll()


##############
#############
con<- odbcConnect("db")

FundStack  <- stringr::str_c(
  'select
  f.fundName
  ,fe.wsj_symbol as abbreviation
  ,r.profile_date as PROFILE_DATE
  ,r.return_code
  ,r.1mReturn
  ,r.1yReturn
  ,r.3yReturn
  ,r.5yReturn
  ,r.7yReturn
  ,r.10yReturn
  from fund f
  left join monthly_returns r on f.fund_id = r.fund_id
  left join fund_ext fe on f.fund_id = fe.fund_id
  where f.fund_id = fund number
  and return_code in (0, 8, 24)
  ') %>%
  sqlQuery(con, ., stringsAsFactors = F) %>%
  data.table::data.table(.)  %>%
  data.table::melt(., id.vars = 1:4, variable.name = "TIME_PERIOD",
                   value.name = "RETURN", variable.factor = F) %>%
  .[!is.na(RETURN)] %>%
  .[, PROFILE_DATE := as.Date(PROFILE_DATE )] %>%
  .[, return_code := as.character(return_code)] %>%
  mutate(abbreviation = 'Fund') %>%
  data.table::data.table(.)

odbcCloseAll()

# 
FundQuery <- FundStack %>%
  mutate(return_code = as.character(return_code)) %>%
  bind_rows(FundQuery) %>%
  data.table::data.table(.)

# 
returns <- FundQuery %>%
  .[ , lapply(.SD, function(x) gsub("NULL", NA, x))] %>%
  .[complete.cases(short_name)] %>%
  setnames(names(.), tolower(names(.))) %>%
  .[ , time_period := gsub("cleaning up text", "", time_period)] %>%
  .[complete.cases(return)] %>%
  .[ , return := as.numeric(return)] %>%
  mutate(return_type = ifelse(return_code == "0", "TR", ifelse(return_code == "8", "Pre-Liq", "Post_Liq"))) %>%
  setnames("profile_date", "date") %>%
  mutate(date = as.Date(date)) %>%
  data.table::data.table(.) %>%
  unique()

monthly_returns <- returns[time_period == "1m" & return_code == 0]

make_blend <- function (DT, ids, weights, blend_name = "Blendy McBlenderson", 
                        id_col = "Ticker", return_col = "Return", date_col = "Date", 
                        other_group = NULL) {
  assertthat::assert_that(data.table::is.data.table(DT) | is.data.frame(DT), 
                          msg = "DT must be a data.table or data.frame")
  assertthat::assert_that(length(setdiff(c(id_col, return_col, 
                                           date_col, other_group), colnames(DT))) == 0, msg = paste0("These columns:\n\t", 
                                                                                                     paste0(setdiff(c(id_col, return_col, date_col, other_group), 
                                                                                                                    colnames(DT)), collapse = "\n\t"), "\n", "aren't found in the data.table"))
  assertthat::assert_that(length(setdiff(ids, DT[[id_col]])) == 
                            0, msg = "Some ids are not in underlying data.")
  assertthat::assert_that(sum(weights) == 1, msg = "Weights do not sum to 100%")
  DT <- data.table::as.data.table(DT)
  weight_frame <- data.table::data.table(ids, weights)
  data.table::setnames(weight_frame, "ids", id_col)
  blend_frame <- merge(DT, weight_frame, by = id_col)
  blend_frame[, `:=`(min_date, min(get(date_col), na.rm = T)), 
              by = id_col]
  blend_frame <- blend_frame[!is.na(get(date_col)) & get(date_col) >= 
                               max(min_date)]
  blend_frame <- blend_frame[!is.na(get(return_col)), .(Name = blend_name, 
                                                        Return = sum(get(return_col) * (weights), na.rm = F)), 
                             by = c(date_col, other_group)]
}


ann_vol <- function(rets) {
  sd(rets) * sqrt(12)
}

max_dd <- function (x, geometric = T) 
{
  if(length(x) <= 12) {
    return(as.numeric(NA))
  }
  
  if (geometric) {
    cumulative_return <- cumprod(1 + x)
  }
  else {
    cumulative_return <- 1 + cumsum(x)
  }
  max_return <- cummax(c(1, cumulative_return))[-1]
  min(cumulative_return/max_return - 1, 1)
}

up_capture <- function (fund_return, index_return) 
{
  captureFrame <- data.table(index_return, fund_return)[index_return >= 
                                                          0]
  if (length(captureFrame$fund_return) == 0) {
    return(NA)
  }
  fund_linked <- Reduce("*", captureFrame$fund_return + 1)^(1/length(captureFrame$fund_return)) - 
    1
  index_linked <- Reduce("*", captureFrame$index_return + 1)^(1/length(captureFrame$index_return)) - 
    1
  fund_linked/index_linked
}

down_capture <- function (fund_return, index_return) 
{
  captureFrame <- data.table(index_return, fund_return)[index_return < 
                                                          0]
  if (length(captureFrame$fund_return) == 0) 
    return(NA)
  fund_linked <- prod(captureFrame$fund_return + 1)^(1/length(captureFrame$fund_return)) - 
    1
  index_linked <- prod(captureFrame$index_return + 1)^(1/length(captureFrame$index_return)) - 
    1
  fund_linked/index_linked
}

flexible_sub <- function(flex_fund, eq_sub, fi_sub, eq_weighting = .6, fi_weighting = .4, dynamic = T) {
  
  test_names <- flex %>% 
    names() %>% 
    grep(flex_fund, .)
  
  assertthat::are_equal(x = eq_weighting + fi_weighting, y = 1) %>% 
    assertthat::assert_that(., msg = "Weightings must add to 1")
  
  if(dynamic == T) {
    assertthat::assert_that(test_names > 0, msg = "Name not found in flexible fund data.")
    
    fund_weights <- paste0(flex_fund, "_", "eq_weight")
    
    calc_fund <- flex %>%
      setnames("Date", "date") %>% 
      .[ , c("date", paste0(flex_fund, "_EQ")), with = F] %>% 
      .[ , date := as.Date(date, "%m/%d/%Y")] %>% 
      merge(., 
            dcast(returns[time_period == "1m" 
                          & abbreviation %in% c(flex_fund, eq_sub, fi_sub)
                          & return_type == "TR"], 
                  date ~ abbreviation, value.var = "return"), by = "date") %>% 
      merge(., dynamic_weights[ , .(date = Date, get(fund_weights))], by = "date") %>% 
      setnames("V2", "eq_weight") %>% 
      .[ , eq_weight := eq_weight/100] %>% 
      .[ , paste0(flex_fund, "_FI") := (get(flex_fund) - (get(paste0(flex_fund, "_EQ")) * eq_weight))/(1 - eq_weight)] %>%
      # .[ , Static := paste0(eq_weighting * 100, "/", fi_weighting * 100, " ", 
      #                                  eq_sub, "/", fi_sub)] %>% 
      .[ , paste0(eq_weighting * 100, "/", fi_weighting * 100, "_Mixed") := eq_weighting * get(eq_sub) + fi_weighting * get(fi_sub)] %>%
      .[ , Dynamic_Mixed := eq_weight * get(eq_sub) + (1 - eq_weight) * get(fi_sub)] %>% 
      setnames(c(eq_sub, fi_sub), c(paste0(c(eq_sub, fi_sub), c("_EQ", "_FI")))) %>% 
      # .[ , eq_component := eq_sub] %>%
      # .[ , fi_component := fi_sub] %>% 
      # setnames(c(paste0(flex_fund, c("_EQ", "_FI"))), c("Equity", "FixedIncome")) %>% 
      .[ , detail := paste0(flex_fund, " vs ", "static and dynamic ", eq_sub, "/", fi_sub)] %>% 
      .[ , substituting := flex_fund] %>% 
      melt(., id.var = c("date", "eq_weight", "substituting", "detail"), variable.factor = F, value.name = "return") %>% 
      .[ , type := str_extract(variable, "_EQ|_FI|_Mixed")] %>% 
      .[ , comp := c("_EQ" = paste0(flex_fund , "_EQ"), "_FI" = paste0(flex_fund , "_FI"), "_Mixed" = flex_fund)[type]] %>% 
      merge(., .[ , .(comp = variable, date, comparison = return)]) %>% 
      .[!grep("IFA", variable)] %>% 
      .[ , type := NULL] %>% 
      setkey(variable, comp, date, return) %>% 
      .[ , variable := gsub("_EQ|_FI|_Mixed", "", variable)]
  }
  
}

#Applies a rollings feature to returns. Since returns are monthly, it is necessary to calculate rolling annual returns with different periodictities.
apply_rolling <- function (DT, func, value, window, group = "", ..., colname = NULL) 
{
  values <- expand.grid(func = func, value = value, window = window)
  func <- as.character(values$func)
  value <- as.character(values$value)
  window <- as.numeric(values$window)
  for (i in 1:length(func)) {
    if (is.null(colname) & length(unique(value)) == 1) {
      this_col <- paste(func[i], window[i], sep = "_")
    }
    else if (is.null(colname)) {
      this_col <- paste(value[i], func[i], window[i], sep = "_")
    }
    else {
      this_col <- paste(colname, window[i], sep = "_")
    }
    method <- "my_roll_apply"
    DT[, `:=`((this_col), do.call(method, args = list(vector = get(value[i]), 
                                                      width = window[i], func = eval(parse(text = func[i])), 
                                                      ...))), by = group]
  }
  DT[]
}

ann_return <- function (x) {
  if(length(x) <= 12) {
    return(prod(1 + x) - 1)
  }
  as.numeric(prod(1 + x)^(12/length(x)) - 1)
}

my_roll_apply <- function (vector, width, func, ...) 
{
  data <- as.numeric(rep(NA, length(vector)))
  if (length(vector) < width) {
    return(data)
  }
  for (i in width:(length(vector))) {
    data[[i]] <- func(vector[(i - width + 1):i], ...)
  }
  return(data)
}

fund_index_roll_apply <- function (fundReturn, indexReturn, width, func, ...) 
{
  data <- as.numeric(rep(NA, length(fundReturn)))
  if (min(length(fundReturn), length(indexReturn)) < width) {
    return(data)
  }
  else if (width > length(fundReturn)) {
    return(data)
  }
  for (i in width:(length(fundReturn))) {
    data[[i]] <- func(fundReturn[(i - width + 1):i], indexReturn[(i - 
                                                                    width + 1):i], ...)
  }
  data
}



tr_stats <- function(flex_data, idvar = "abbreviation", funcs = c("max_dd", "ann_vol")) {
  
  stats_roll <- copy(flex_data) %>% 
    apply_rolling(DT = ., func = funcs, 
                  value = c("return"), window = c(1, 3, 5,7, 10) * 12, group = idvar) %>% 
    .[ , return := NULL] %>% 
    melt(., id = c(idvar, "date"), 
         variable.factor = F, variable.name = "metric") %>% 
    .[ , time_period := str_extract(metric, "[0-9]+")] %>% 
    .[ , time_period := as.numeric(time_period)/12] %>% 
    .[ , time_period := as.character(time_period)] %>% 
    .[ , metric := gsub(paste0("_", c(1,3,5,7,10) * 12, collapse = "|"), "", metric)] %>% 
    .[complete.cases(value)]
  
  financial_crisis <- flex_data[year(date) %in% c(2008, 2009)] %>% 
    .[ , c("ann_return", "ann_vol", "max_dd") := list(ann_return(return), 
                                                      ann_vol(return),
                                                      max_dd(return)), by = c("abbreviation")] %>% 
    .[ , c("date", "return") := NULL] %>% 
    unique() %>% 
    melt(., id.var = "abbreviation", variable.factor = F, variable.name = "metric", value.name = "2008-2009")
  
  captures <- copy(flex_data) %>% 
    merge(., sp500[ , .(date, sp500_return)], by = "date") %>% 
    merge(., us_agg[ , .(date, agg_return)], by = "date") %>% 
    setkeyv(c(idvar, "date"))
  
  for(i in c(1, 3, 5, 7, 10)) {
    lab_d <- paste0("dc_sp_", i)
    lab_u <- paste0("uc_sp_", i)
    lab_cor_sp <- paste0("cor_sp_", i)
    lab_cor_agg <- paste0("cor_us_agg_", i)
    captures[ , (lab_d) := fund_index_roll_apply(return, sp500_return, i * 12, down_capture), by = idvar] %>% 
      .[ , (lab_u) := fund_index_roll_apply(return, sp500_return, i * 12, up_capture), by = idvar] %>% 
      .[ , (lab_cor_sp) := fund_index_roll_apply(return, sp500_return, i * 12, cor), by = idvar] %>% 
      .[ , (lab_cor_agg) := fund_index_roll_apply(return, agg_return, i * 12, cor), by = idvar]
  }
  
  format_caps <- melt(captures[ , !c("return", "sp500_return", "agg_return")], id.var = c(idvar, "date"),
                      variable.factor = F, variable.name = "metric") %>% 
    .[ , time_period := str_extract(metric, "[0-9]+")] %>% 
    .[ , time_period := factor(time_period, levels = c("1", "3", "5", "7", "10"))] %>% 
    .[ , metric := gsub(paste0("_", c(1,3,5, 7,10), collapse = "|"), "", metric)] %>% 
    .[complete.cases(value)]
  
  rbind(stats_roll, format_caps, use.names = T) %>% 
    merge(., financial_crisis, by = c("metric", "abbreviation"), all.x = T)
  
}
   
