#' @title Descriptive Univariate Statistics
#'
#' @description The function summarizes the input data using different descriptive univariate statistical
#' measures on grouped or ungrouped level.
#'
#' @details The functions calculates following measures on the input data:
#'
#' Measures of Central Tendency: Mean, Median
#'
#' Measures of Distribution: Count, Proportion
#'
#' Measures of Dispersion: Min, Max, Quantile, Standard Deviation, Variance
#'
#' Measures of shape: Skewness, Kurtosis
#'
#' Addition to these measures, the function provides information on the data type, count on no. of rows,
#' unique entries and percentage of missing entries
#'
#' All the above statistics can be generated for the entire data or at a group level. The
#' variables/variables specified to group_variable parameter splits the data into groups based on
#' the unique levels of the variable/variables specified and calculates descriptive statistics on each of
#' these levels.
#'.
#' @param dataset A data.frame object, an input dataset for which descriptive statistics needs
#' to be calculated
#' @param show_levels An integer value. It controls how many top character/factor levels with their
#' proportions needs to be displayed in descending order of their proportions, by default it is set to 5.
#' @param decimal_points An integer value. It controls no of decimal points to which numeric data needs
#' to be rounded off, by default it is set to 2.
#' @param group_variable A character vector. Specify the character or factor variable/variables on
#' whose unique group levels the data should be split and univariate statistics needs to be generated.
#' @param miss_val A character vector. Specify different strings which needs to be considered as
#' missing values.
#'
#' @return A data.frame object with descriptive univariate statistics listed for
#' numerical,categorical and date variables at group level, if specified, else for entire data.
#'
#' @examples
#' desc_stats(iris,show_levels=2,decimal_points=2,group_variable=c("Species"),miss_val=c("unknown"))
#' desc_stats(iris,show_levels=2,decimal_points=2,group_variable=c("Species"))
#' desc_stats(iris,show_levels=2,decimal_points=2)
#' desc_stats(iris,show_levels=2)
#' desc_stats(iris)
#'
#' @importFrom utils head
#' @importFrom stats sd median var quantile
#' @import moments zoo
#'
#' @export desc_stats

desc_stats <- function(dataset,show_levels=5,decimal_points=2,group_variable=NULL,miss_val=NULL){

  if(is.data.frame(dataset)==FALSE)
  {
    stop("Check input format. Input format data.frame is required")
  }

  if(missing(group_variable)==FALSE)
  {
    group_variable_status = TRUE
    if(!group_variable %in% colnames(dataset))
    {
      stop("Group variable should be from one of the columns of the input dataset")
    }
  }
  else
  {
    group_variable_status = FALSE
    group_variable = "dummy_col_default"
    dataset$dummy_col_default <- "All"
  }

  miss_val_recog <- function(dataset,miss_val){

    dataset <- as.data.frame(lapply(dataset,function(x)replace(x,x %in% miss_val,NA)),stringsAsFactors = F)
    return(dataset)
  }
  group_levels <- function(dataset,group_variable){

    group_levels <- unique(dataset[group_variable])
    group_levels[is.na(group_levels)] <- "Not Available"
    group_levels$temp <- apply(group_levels,1, paste, collapse="-")
    group_levels <- split(group_levels$temp,f = group_levels$temp)

    return(group_levels)

  }
  num_cat_diff <- function(level,input_data){

    if(sum(is.na(input_data[,group_variable]))>0)
    {
      input_data[,group_variable][is.na(input_data[,group_variable])] <- "Not Available"
    }

    if(length(group_variable)!=1)
    {
      input_data$key <- apply(input_data[,group_variable],1,paste,collapse="-")
    }
    else
    {
      input_data$key <- as.character(input_data[,which(colnames(input_data)==group_variable)])
    }

    input_data <- input_data[which(input_data$key==level),]

    isnumeric_col <- lapply(input_data,function(x)is.numeric(x))
    isnumeric_col <- as.data.frame(do.call(rbind,isnumeric_col))
    isnumeric_col$columns <- row.names(isnumeric_col)

    num_col <- as.data.frame(input_data[,isnumeric_col[which(isnumeric_col$V1==TRUE),]$columns])
    colnames(num_col) <- isnumeric_col[which(isnumeric_col$V1==TRUE),"columns"]

    cat_col <- as.data.frame(input_data[,isnumeric_col[which(isnumeric_col$V1==FALSE),]$columns],StringsAsFactors=F)
    colnames(cat_col) <- isnumeric_col[which(isnumeric_col$V1==FALSE),"columns"]

    num_col$level <- input_data$key
    colnames(cat_col)[colnames(cat_col)=="key"] <- "level"

    return(list(num_col,cat_col))
  }
  desc_num_stats <- function(level,num_col){

    num_col <- do.call("rbind",num_col)

    if(ncol(num_col)>1)
    {

      num_col <- num_col[which(num_col$level==level),]

      num_col_req <- colnames(num_col)
      num_col_req <- num_col_req[! num_col_req %in% c("level",unlist(group_variable))]

      num_col$level <- NULL

      num_col <- as.data.frame(num_col[,num_col_req])
      colnames(num_col) <- num_col_req

      desc_stat_list <- list("datatype","number","unique","missing","mean","min","median","max","quantile","std","var","skew","kurtosis")

      data_type <- sapply(num_col,function(x)class(x))
      no_rows <- sapply(num_col,function(x)length(x))
      unique_col <- sapply(num_col,function(x)length(unique(x)))
      missing_col <- sapply(num_col,function(x)(sum(is.na(x))/length(x)*100))
      mean_col <- sapply(num_col,function(x)mean(x,na.rm = T))
      min_col <- sapply(num_col,function(x)min(x,na.rm = T))
      median_col <- sapply(num_col,function(x)median(x,na.rm = T))
      max_col <- sapply(num_col,function(x)max(x,na.rm = T))
      quantile_col <- apply(as.data.frame(sapply(num_col,function(x)quantile(x,na.rm=T))),2,function(x)paste0(x, collapse = ", "))
      std_col <- sapply(num_col,function(x)sd(x,na.rm = T))
      var_col <- sapply(num_col,function(x)var(x,na.rm = T))
      skew_col <- sapply(num_col,function(x)skewness(x,na.rm = T))
      kurtosis_col <- sapply(num_col,function(x)kurtosis(x,na.rm = T))

      coll_sats_frame <- cbind.data.frame(data_type,no_rows,unique_col,missing_col,mean_col,min_col,median_col,max_col,quantile_col,std_col,var_col,skew_col,kurtosis_col)
      coll_sats_frame$columns <- row.names(coll_sats_frame)

      colnames(coll_sats_frame) <- c(desc_stat_list,"columns")

      coll_sats_frame <- coll_sats_frame[,c("columns",unlist(desc_stat_list))]

      coll_sats_frame$level <- level

      return(coll_sats_frame)
    }
    if(ncol(num_col)<=1)
    {
      return(NA)
    }

  }
  unique_val_ext <- function(cat_col_uniq_val){

    if(length(cat_col_uniq_val) == sum(is.na(cat_col_uniq_val)))
    {
      return (NA)
    }

    if(length(cat_col_uniq_val) != sum(is.na(cat_col_uniq_val)))
    {
      val_prop <- as.data.frame(prop.table(table(cat_col_uniq_val)))
      val_prop <- val_prop[order(val_prop$Freq,decreasing = T),]
      val_prop <- val_prop[which(val_prop$Freq>0),]
      val_prop <- head(val_prop,show_levels)
      val_prop <- as.data.frame(paste0(val_prop$cat_col_uniq_val,"(",round(val_prop$Freq,2),")",collapse=", "))
      colnames(val_prop) <- "values_prop"
      return(val_prop)
    }
  }
  desc_cat_stats <- function(level,cat_col){

    cat_col <- cat_col[[1]]
    cat_col <- cat_col[which(cat_col$level==level),]

    cat_col_req <- colnames(cat_col)
    cat_col_req <- cat_col_req[! cat_col_req %in% c("level",unlist(group_variable))]

    cat_col$level <- NULL

    cat_col <- as.data.frame(cat_col[,cat_col_req],StringsAsFactors=F)
    colnames(cat_col) <- cat_col_req

    class_cols <- as.data.frame(sapply(cat_col,function(x)paste(class(x),collapse = ",")),stringsAsFactors = F)
    class_cols$columns <- row.names(class_cols)
    class_cols_date <- class_cols[which(class_cols[,1] %in% c("Date","POSIXct","POSIXt","POSIXct,POSIXt")),"columns"]

    date_col <- as.data.frame(cat_col[,colnames(cat_col) %in% class_cols_date],stringsAsFactors = F)
    colnames(date_col) <- class_cols_date

    cat_col <- as.data.frame(cat_col[,!colnames(cat_col) %in% class_cols_date],stringsAsFactors = F)
    colnames(cat_col) <- class_cols[!(class_cols[,1] %in% c("Date","POSIXct","POSIXt","POSIXct,POSIXt")),"columns"]

    if(ncol(cat_col)!=0)
    {

      desc_stat_list <- list("datatype","number","unique","missing","values_prop")

      data_type <- unlist(sapply(cat_col,function(x)class(x)))
      no_rows <- sapply(cat_col,function(x)length(x))
      unique_col <- sapply(cat_col,function(x)length(unique(x[is.na(x)==F])))
      missing_col <- sapply(cat_col,function(x)(sum(is.na(x))/length(x)*100))
      unique_val <- do.call("rbind",lapply(cat_col,unique_val_ext))

      coll_sats_frame <- cbind.data.frame(data_type,no_rows,unique_col,missing_col,unique_val)
      coll_sats_frame$columns <- row.names(coll_sats_frame)

      colnames(coll_sats_frame) <- c(desc_stat_list,"columns")

      coll_sats_frame <- coll_sats_frame[,c("columns",unlist(desc_stat_list))]

      coll_sats_frame$level <- level

    }

    if(ncol(date_col)!=0)
    {

      date_desc_stat_list <- list("datatype","number","unique","missing","date_min","date_max")

      data_type <- sapply(date_col,function(x)paste(class(x),collapse = ","))
      no_rows <- sapply(date_col,function(x)length(x))
      unique_col <- sapply(date_col,function(x)length(unique(x)))
      missing_col <- sapply(date_col,function(x)(sum(is.na(x))/length(x)))
      min_col <- apply(date_col,2,function(x)min(x,na.rm = T))
      max_col <- apply(date_col,2,function(x)max(x,na.rm = T))

      date_coll_sats_frame <- cbind.data.frame(data_type,no_rows,unique_col,missing_col,min_col,max_col)
      date_coll_sats_frame$columns <- row.names(date_coll_sats_frame)

      colnames(date_coll_sats_frame) <- c(date_desc_stat_list,"columns")

      date_coll_sats_frame <- date_coll_sats_frame[,c("columns",unlist(date_desc_stat_list))]

      date_coll_sats_frame$level <- level

    }

    if(ncol(date_col)!=0 & ncol(cat_col)!=0)
    {
      coll_sats_frame <- merge(coll_sats_frame,date_coll_sats_frame,all = T)
    }

    if(ncol(date_col)==0 & ncol(cat_col)==0)
    {
      coll_sats_frame <- NA
    }

    if(ncol(date_col)==0 & ncol(cat_col)!=0)
    {
      coll_sats_frame <- coll_sats_frame
    }

    if(ncol(cat_col)==0 & ncol(date_col)!=0)
    {
      coll_sats_frame <- date_coll_sats_frame
    }

    return(coll_sats_frame)
  }

  miss_val_trt <- miss_val_recog(dataset,miss_val)
  group_lev_data <- group_levels(miss_val_trt,group_variable)
  num_cat_dataset <- do.call("rbind",lapply(group_lev_data,num_cat_diff,miss_val_trt))
  num_stats_frame <- do.call("rbind",lapply(group_lev_data,desc_num_stats,num_cat_dataset[,1]))
  cat_stats_frame <- do.call("rbind",lapply(group_lev_data,desc_cat_stats,num_cat_dataset[,2]))

  num_cat_desc_stat <- merge(num_stats_frame,cat_stats_frame,all = T)

  if("date_min" %in% colnames(num_cat_desc_stat))
  {
    if(!("min" %in% colnames(num_cat_desc_stat)))
    {
      num_cat_desc_stat$min <- NA
      num_cat_desc_stat$max <- NA
    }

    num_cat_desc_stat$min <- as.character(num_cat_desc_stat$min)
    num_cat_desc_stat$date_min <- as.character(num_cat_desc_stat$date_min)
    num_cat_desc_stat$min <- ifelse(num_cat_desc_stat$datatype %in% c("Date","POSIXct","POSIXt","POSIXct,POSIXt"),num_cat_desc_stat$date_min,num_cat_desc_stat$min)

    num_cat_desc_stat$max <- as.character(num_cat_desc_stat$max)
    num_cat_desc_stat$date_max <- as.character(num_cat_desc_stat$date_max)
    num_cat_desc_stat$max <- ifelse(num_cat_desc_stat$datatype %in% c("Date","POSIXct","POSIXt","POSIXct,POSIXt"),num_cat_desc_stat$date_max,num_cat_desc_stat$max)

    num_cat_desc_stat <- num_cat_desc_stat[,!colnames(num_cat_desc_stat) %in% c("date_min","date_max")]

  }

  num_cat_desc_stat_colname <- colnames(num_cat_desc_stat)
  num_cat_desc_stat <- num_cat_desc_stat[order(num_cat_desc_stat$level,num_cat_desc_stat$datatype),c("level",num_cat_desc_stat_colname[!num_cat_desc_stat_colname %in% "level"])]

  if(group_variable_status==FALSE)
  {
    num_cat_desc_stat$level <- NULL
  }

  num_cat_desc_stat <- num_cat_desc_stat[,colSums(is.na(num_cat_desc_stat)) < nrow(num_cat_desc_stat)]

  col_iden_num <- as.data.frame(sapply(num_cat_desc_stat,function(x)is.numeric(x)))
  col_iden_num$columns <- row.names(col_iden_num)
  col_iden_num <- col_iden_num[which(col_iden_num[,1]==TRUE),2]
  num_cat_desc_stat[,col_iden_num] <- round(num_cat_desc_stat[,col_iden_num],decimal_points)

  col_order <- data.frame(columns = colnames(miss_val_trt),order_seq = seq(1,ncol(miss_val_trt),by=1))
  num_cat_desc_stat <- merge(num_cat_desc_stat,col_order,by="columns",all.x=T)

  metric_order <- data.frame(columns = c("columns","datatype","unique","missing","values_prop","min","max","mean","median","quantile","std","var","skew","kurtosis"))
  metric_order$order_seq <- seq(1,nrow(metric_order),by=1)
  metric_order <- metric_order[which(metric_order$columns %in% colnames(num_cat_desc_stat)),]
  metric_order <- as.character(metric_order[order(metric_order$order_seq),"columns"])
  num_cat_desc_stat <- num_cat_desc_stat[order(num_cat_desc_stat$order_seq),metric_order]

  return(num_cat_desc_stat)

}

