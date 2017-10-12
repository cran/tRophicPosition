# library(tRophicPosition)
# data("Bilagay")
# Bilagay
# BilagayList <- extractIsotopeData(Bilagay, b1 = "Pelagic_BL", b2 = "Benthic_BL")
#
# rm(parallelTP)
# parallelTP <- function(siDataList = NULL,
#                        adapt = 500,
#                        n.iter = 500,
#                        burnin = 500,
#                        model = "twoBaselinesFull",
#                        ...) {
#
#   message("arguments of function")
#   print(names(match.call()))
#   # print(names(...))
#   cl <- parallel::makeCluster(parallel::detectCores())
#   # parallel::setDefaultCluster(cl)
#   message("cluster")
#   print(cl)
#
#   message("search()/str(siDataList)")
#   print(search())
#   print(str(siDataList))
#   siDataList <- siDataList
#
#   message("search()/str(siDataList <- siDataList)")
#   print(search())
#
#   print(parallel::clusterExport(cl, "siDataList", envir = environment()))
#   print(parallel::clusterEvalQ(cl, "siDataList"))
#
#   parallel::clusterEvalQ(cl, library(tRophicPosition))
#
#   print(parallel::clusterExport(cl, c("siDataList",
#                                       "adapt",
#                                       "n.iter",
#                                       "burnin",
#                                       "model",
#                                       "multiSpeciesTP"), envir = environment()))
#
#   a <- parallel::parLapply(cl, siDataList,
#                                                         multiSpeciesTP,
#                                                         adapt = adapt,
#                                                         n.iter = n.iter,
#                                                         burnin = burnin,
#                                                         model = model)
#   parallel::stopCluster(cl)
#
#   # print(time_parallel)
#
#   return(a)
#
# }
#
# parallelTP(siDataList = BilagayList, quiet = TRUE)
#
#
# cl <- parallel::makePSOCKcluster(parallel::detectCores())
# parallel::setDefaultCluster(cl)
# adder <- function(a, b) a + b
# parallel::clusterExport(NULL, c('adder'))
# clusterEvalQ(NULL, library(MASS))
# parallel::parLapply(NULL, 1:8, function(z) adder(z, 100))
