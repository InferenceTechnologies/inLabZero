#' @param outlier Numeric. Outlier filtering of data column before plotting. Outliers are data points outside the
#' interval <(Q1-k1*IQR),(Q3+k2*IQR)>, where Q1 and Q3 are quartiles, IQR is interquartile range and
#' k1 and k2 are values specified by \code{outlier} parameter as its first and second element. 
#' If single value is provided, k1 = k2 setting is used. No outlier filtering is applied by default. 
