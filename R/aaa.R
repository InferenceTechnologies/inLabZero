#
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# aaa
# 
# 0.2.0
# 

#
# Cfgs
#

cfgSite <- cfgAppR::cfgSource("inLabZero", "cfgSite")

#
# Imports
#

#' @import data.table
#' @import doMC
#' @import foreach
#' @import tableMatrix
NULL

# set data.table threads
setDTthreads(threads=cfgSite$DTthreads)

# 
# Internal Imports
#

tmName <- tableMatrix:::tmName
tableListWrap <- tableMatrix:::tableListWrap
tableMatrixWrap <- tableMatrix:::tableMatrixWrap

#
# End
#