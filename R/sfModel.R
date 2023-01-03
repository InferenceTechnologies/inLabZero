#
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# Inference Technologies
#
# Model
# 
# 0.83.6
# 

#
# Imports
#

#
# Constants
#

#
# Classes definition
#

#
# Generics
#

# S3 semiFrame generic for modeling
#' @export
model <- function(...) { UseMethod("model") }

#
# Methods Functions
#

probMatToVec <- function(prob, positiveIdx=2) {
	if (is.matrix(prob)) { prob <- prob[,positiveIdx] }
	if (is.factor(prob)) { prob <- ifelse(prob==levels(prob)[setdiff(c(1,2),positiveIdx)],0,1) }
	return(prob)
}

classStat <- function(realRes, predRes, stat=list(), br=NULL, dependLevel=NULL, positiveIdx=2) {

	if (is.null(br)) { br <- length(unique(realRes)) }

	if (is.matrix(predRes)) {
		predResInt <- apply(predRes, 1, function(row) { which.max(row)-1 } )
	} else if (is.factor(predRes)) {
		predResInt <- as.integer(predRes)-1
	} else {
		predResInt <- predRes
	}

	if (is.factor(realRes)) {
		realResInt <- as.integer(realRes)-1
	}


	stat$confMat <- table(predResInt, realResInt, dnn=c("Prediction", "Reference"))
	
	if (nrow(stat$confMat)<2) {
		stat$confMat <- rbind(stat$confMat, rep(0L, 2))
		rownames(stat$confMat)[2] <- setdiff(c(0L,1L), predResInt[1])
		warning("Model predicts only one class")
	}

	stat$acc <- sum(diag(stat$confMat))/sum(stat$confMat)
	Pe <- sum(sapply(1:ncol(stat$confMat), function(i) { (sum(stat$confMat[,i])/sum(stat$confMat))*(sum(stat$confMat[i,])/sum(stat$confMat)) })) 
	stat$kappa <- (stat$acc-Pe)/(1-Pe)
	

	if (br==2&is.null(stat$auc)) {
		stat$pred <- ROCR::prediction(probMatToVec(predRes, positiveIdx=positiveIdx), 
						realRes, label.ordering=dependLevel)
		stat$roc <- ROCR::performance(stat$pred, "tpr", "fpr")
		stat$auc <- ROCR::performance(stat$pred, "auc")@y.values
	}

	return(stat)
}


#
# Classes Generics Methods
#

#' Modeling
#' 
#' @description Models wrapper to perform regression, classification, clustering and dimensionality reduction.
#' 
#' @template stsfobjParam
#' @template dataParam
#' @param type Character. Specifies type of the model (default: \code{NULL}): 
#' \itemize{
#'   \item{ "class" }{ Classification models. }
#'   \item{ "reg" }{ Regression models. }
#'   \item{ "cluster" }{ Clustering. }
#'   \item{ "dim" }{ Dimensionality reduction. }
#' }
#' @param model Character. Specifies the particular model (default: \code{NULL}). 
#' 
#' Regression and classification models:
#' \itemize{
#'   \item{ "lin" }{ Linear model. For more information see \code{?stats::lm} and \code{?stats::glm}. }
#'   \item{ "cart" }{ Classification And Regression Tree. For more information see \code{?rpart::rpart}.}
#'   \item{ "forest" }{ Classification and Regression with Random Forest. For more information see \code{?randomForest::randomForest}. }
#'   \item{ "boost" }{ Generalized Boosted Regression. For more information see \code{?gbm::gbm}.}
#'   \item{ "xboost" }{ eXtreme Gradient Boosting. For more information see \code{?xgboost::xgb.train}. }
#'   \item{ "bayes" }{ Naive Bayes Classifier. For more information see \code{?e1071::naiveBayes}.}
#'   \item{ "knn" }{ k-Nearest Neighbour Classification. For more information see \code{?class::knn}. }
#'   \item{ "svm" }{ Support Vector Machines. For more information see \code{?kernlab::ksvm}. }
#'   \item{ "nnet" }{ Fit Neural network. For more information see \code{?nnet::nnet}. }
#' }
#' Clustering:
#' \itemize{
#'   \item{ "kmeans" }{ K-Means Clustering. For more information see \code{?kmeans}. }
#'   \item{ "kkmeans" }{ Kernel k-means. For more information see \code{?kernlab::kkmeans}. }
#'   \item{ "dbscan" }{  Density-based spatial clustering of applications with noise. For more information see \code{?dbscan::dbscan}. }
#'   \item{ "hdbscan" }{ Hierarchical density-based spatial clustering of applications with noise. For more information see \code{?dbscan::hdbscan}. }
#'   \item{ "optics" }{ Ordering points to identify the clustering structure. For more information see \code{?dbscan::optics}.}
#' }
#' Dimensionality reduction:
#' \itemize{
#'   \item{ "pca" }{  Principal Component Analysis. For more information see \code{?prcomp}. }
#' }
#' @param method Character. Provides additional information about a model (default: \code{NULL}):
#' \itemize{
#'   \item{ "cart" }{ if not specified, automatically determines if \code{class} or \code{anova} method should be applied. }
#'   \item{ "svm" }{ method specifies the output: }
#' 		\itemize{
#'  		 \item{ "p" }{ probabilities. }
#'  		 \item{ "pr" }{  response determined by highest probabilities. }
#'  		 \item{ "r" }{  response. }
#' 		}
#' }
#' @param br Vector. Breaks of the response parameter. Works in conjunction with \code{brMethod} setting.
#' Single integer value determines number of breaks. Vector of \code{length>1} can be numerical,
#' character or POSIXct and represents interval breaking points. Special character values "min", "mean",
#' "median", "max" and "seq" can be used (default: 2).
#' @param brMethod Character. Relevant for classification of noncategorical responses. Works in conjunction with \code{br} setting.
#' Split methods:
#' \itemize{
#'   \item{ "median" }{ Breaks the response parameter with \code{median} (default). }
#'   \item{ "mean" }{ Breaks the response parameter with \code{mean}. }
#'   \item{ "thr" }{ Breaks the response parameter with threshold value specified by \code{br}. }
#'   \item{ "linear" }{ Breaks the response parameter into equidistant intervals, number of splits is given
#'    by \code{br}. }
#' } 
#' @param splitRatio Numeric. Represents the split ratio between train and test set. A splitRatio of 0.7 denotes that 70 percent of the data will be used for training and 30 percent for testing (default: 1).
#' @param formula Standard formula input, which characterizes the response parameter and the input parameters (default: \code{ProbeYield ~ .}).
#' @param balance Numeric. In binary classifications used to balance positive and negative cases in the train set.
#'                \code{balance} parameter defines the multiple of the number of positive cases to be sampled out of negative cases. By default
#'                these negative cases are sampled from the response variable, use \code{balanceCol} to employ a custom variable for sampling. (default: \code{FALSE}).
#' @param balanceCol Character. Defines column name of the variable for negative cases sampling (default: \code{NULL}).
#' @param seed Integer. Sets the seed of the random number generator (default: \code{NULL}).
#' @param classLevel Specifies custom level names for classification modeling (default: \code{NULL}).
#' @param dataType Character. Data type of variable group that will be used as the input for modeling (default: "PC").
#' @param svmType Support Vector Machines specific parameter. See \code{type} parameter in \code{?kernlab::ksvm} for more information.
#' @param remAllNARow Logical. If \code{TRUE} all rows containing \code{NAs} will be discarded (default: \code{FALSE}).
#' @param genStat Logical. If \code{TRUE} model statistics are generated (default: \code{TRUE}).
#' @param toFactor Logical. If \code{TRUE} non numeric columns will be converted into factors (default: \code{TRUE}).
#' @param mapDiceOnly Logical. If \code{TRUE} only cells with dice will be used for maps modeling (default: \code{TRUE}).
#' @param colNameBase Character. Column name base for map data modeling (default: "map").
#' @template verbose
#' @param ... Parameters passed to the model implementation.
#' 
#' @return Copy of the \code{obj} object with the model specific output.
#' 
#' @examples
#' 
#' data(wbm)
#' data(pcy)
#' 
#' # Perform clustering on all wafer bin maps applying the 'kmeans' method with 9 user defined centroids
#' wbm <- model(wbm, data="map", type="cluster", model="kmeans", centers=9)  
#' 
#' # Train CART classification of 'ProbeYield'(median response split by default) on all parameters
#' pcy <- model(pcy, type="class", model="cart", formula="ProbeYield~ .")
#' 
#' # Train Random forest classification
#' pcy <- model(pcy, type="class", model="forest", formula=ProbeYield~.)
#' 
#' # Train eXtreme Gradient Boosting on non-balanced data with a defined 0.7/0.3 train/test data split
#' pcy <- model(pcy, type="class", model="xboost", formula="clear ~ . ", splitRatio=0.7, nrounds=10, max_depth=5)
#' 
#' # Train Random forest classification on balanced data with a defined 0.7/0.3 train/test data split
#' pcy <- model(pcy, type="class", model="forest", formula=edge ~ ., splitRatio=0.7, balance=1.5, balanceCol="clear")
#' 
#' @aliases model
#' 
#' @rdname model.semiFrame
#' @export
model.semiFrame <- function(obj, data="map", type=NULL, model=NULL, method=NULL, br=2, brMethod="median", 
								splitRatio=1, formula=ProbeYield ~ ., balance=FALSE, balanceCol=NULL, seed=NULL, 
								classLevel=NULL, dataType="PC", svmType=NULL, remAllNARow=FALSE, genStat=TRUE, 
								toFactor=TRUE, mapDiceOnly=TRUE, colNameBase="map", verbose=TRUE, ...) {


	checkCharInput(data, c("par", "map"))
	checkCharInput(type, c("class", "reg", "cluster", "dim"))

	ncatn(verbose=verbose)

	formula <- as.formula(formula)
	dependCol <- character()
	if (length(formula)==3) {
		dependCol <- as.character(formula)[2]
	}

	if (data=="map") {
		if (nrow(obj$matDim)>1) { stop("One dimension maps required") }
		ncatn(sprintf("Model %s on maps", model), verbose=verbose)
		if (type=="cluster") {
			dependCol <- character()
		}
		modelData <- obj$mat[[1]]
		if (mapDiceOnly && mapType(obj) %in% c(sfMapTypeGroup$bin, sfMapTypeGroup$bbin)) {
			ncatn("Dice only", verbose=verbose)
			nonDiceCol <- which(apply(modelData, 2, function(col) { any(col<0) }))
			if (length(nonDiceCol)) {
				modelData <- modelData[,-nonDiceCol]
			}
		}
		
	} else {
		modelData <- obj$tab[, c(dependCol, dataType(obj)[[dataType]]), with=FALSE]
		if (remAllNARow) {
			rowNA <- apply(modelData, 1, function(row) sum(is.na(row[2:length(row)])) )
			rowNA <- which(rowNA==(ncol(modelData)-1))
			if (length(rowNA)) {
				modelData <- modelData[-rowNA,]
			}
			ncatn(sprintf("Removed %d full NA row(s)", length(rowNA)), verbose=verbose)
		}

		ncatn(sprintf("Model %s on %s parameters", model, dataType), verbose=verbose)
	}

	m <- NULL
	depend <- NULL
	dependLevel <- NULL
	balanceRow <- NULL
	if (length(dependCol)) {
		if (data=="map") {
			depend <- obj[[dependCol]]
		} else {
			depend <- modelData[[dependCol]]
		}
	}
	
	if (type=="cluster") {
		
		if (length(dependCol)) {
			modelData <- modelData[,-1]
		}

		if (length(formula)==2) {

			if (is.null(colnames(modelData))) {
				colnames(modelData) <- paste0(colNameBase, 1:ncol(modelData))
			}

			formula <- as.formula(paste(Reduce(paste, deparse(formula)), "-1"))
			if (!is.data.frame(modelData)) {
				modelData <- as.data.frame(modelData, stringsAsFactors=FALSE)
			}
			modelData <- model.matrix(formula, modelData)
			
		}

		ncatn(sprintf("Clustering", dependCol), verbose=verbose)

	} else if (type=="class"&data!="map") {
		
		if (names(findDataType(obj,dependCol))==sfDataType$clust) {
			depend <- factor(modelData[[dependCol]])
			if (length(levels(depend))==2) {
				if (is.null(classLevel)) {
					levels(depend) <- c(paste0("w/o", dependCol), dependCol)
				} else {
					levels(depend) <- classLevel
				}
			}
			modelData[, c(dependCol):=list(depend)]
		}

		if (!is.factor(modelData[[dependCol]])) {
			breaks <- breakData(modelData[[dependCol]],br, brMethod)
			ncatn(sprintf("Breaking %s into (%s) for classification", dependCol, paste(signif(breaks,3), collapse=", ")), verbose=verbose)
			breaks[1] <- floor(breaks[1])-1
			breaks[length(breaks)] <- ceiling(breaks[length(breaks)])+1
			# modelData[[dependCol]] <- as.factor(cut(modelData[[dependCol]], breaks=breaks, labels=FALSE)-1)
			depend <- as.factor(cut(depend, breaks=breaks, labels=FALSE, include.lowest=TRUE)-1)
			if (length(levels(depend))==2) {
				if (is.null(classLevel)) {
					levelStr <- substr(dependCol, 1, 3)
					if (names(findDataType(obj,dependCol))==sfDataType$y) { 
						levelStr <- "Y"
						if (grepl("B",dependCol)) { levelStr <- paste0(levelStr, sub("^[^B]*B","B",dependCol)) } 
					}
					levels(depend) <- c(sprintf("%s<%.1f",levelStr,breaks[2]), sprintf("%s>%.1f", levelStr, breaks[2]))
				} else {
					levels(depend) <- classLevel
				}
			}
			modelData[, c(dependCol):=list(depend)]
		} else {
			ncatn(sprintf("Using %s for classification", dependCol), verbose=verbose)
			if (!is.null(classLevel)) {
				levels(depend) <- classLevel
				modelData[, c(dependCol):=list(depend)]
			}
		}
		dependLevel <- levels(depend)
		br <- length(dependLevel)
		ncatn(sprintf("Levels %s",paste(dependLevel,collapse=", ")), verbose=verbose)


		if (balance!=FALSE&&br==2) {
			balanceLev <- as.data.frame(table(depend))
			balanceLev <- as.character(balanceLev[order(balanceLev[,2]),1][1])
			if (is.null(balanceCol)) { balanceCol <- dependCol }
			ncatn(sprintf("Balancing dependent variable, ratio %.1f : %s, using column %s", balance, balanceLev, balanceCol))

			if (balanceCol!=dependCol) {
				balanceData <- obj$tab[[balanceCol]]
				balanceData <- factor(ifelse(balanceData>0, 0, 1))
				dependLevel[dependLevel!=balanceLev] <- balanceCol
				levels(depend) <- dependLevel 
				ncatn(sprintf("Changing levels to %s",paste(dependLevel,collapse=", ")), verbose=verbose)
				levels(balanceData) <- c(balanceCol, balanceLev)
				modelData[, c(dependCol):=list(depend)]
			} else {
				balanceData <- depend
			}

			dependRow <- which(depend==balanceLev)
			sampleFrom <- which(balanceData!=balanceLev)
			sampleSize <- round(balance*length(dependRow))
			if (length(sampleFrom)<sampleSize) { stop(sprintf("Not enough data in %s to sample from", balanceCol)) }
			balanceRow <- sample(sampleFrom, sampleSize)
			balanceRow <- sample(union(dependRow, balanceRow))
			modelData <- modelData[balanceRow,]
			depend <- modelData[[dependCol]]
		}
	} else if (type=="class"&data=="map") {

		ncatn(sprintf("Using %s for classification", dependCol), verbose=verbose)
		depend <- factor(depend)
		if (!is.null(classLevel)) {
			levels(depend) <- classLevel
		}
		dependLevel <- levels(depend)
		br <- length(dependLevel)

	} else if (data!="map") {
		
		ncatn(sprintf("Using %s for regression", dependCol), verbose=verbose)

	}

	if (toFactor && data!="map" && !is.matrix(modelData)) {
		factored <- character()
		for (col in colnames(modelData)[-1]) {
			if (!is.numeric(modelData[[col]]) && !is.factor(modelData[[col]])) {
				factored <- c(factored, col)
				modelData[[col]] <- as.factor(modelData[[col]])
			}
		}
		if (length(factored)) {
			ncatn(sprintf("To factor: %d column(s)", length(factored)))
		}
	}

	if (splitRatio==1) {

		spl <- rep(TRUE, nrow(modelData))
		modelData <- list(train=modelData, test=NULL)
	} else {

		if (!is.null(seed)) { set.seed(seed) }
		
		if (data!="map") {
			spl <- caTools::sample.split(modelData[[dependCol]], SplitRatio=splitRatio)
			modelData <- list(train=modelData[spl], test=modelData[!spl])
		} else {
			if (type!="cluster") {
				spl <- caTools::sample.split(depend, SplitRatio=splitRatio)
				modelData <- list(train=modelData[spl,], test=modelData[!spl,])
			}
		}
	}
	
	ncatn(sprintf("Training on %d%% of data...", round(100*splitRatio)), verbose=verbose)
	
	pred <- list(train=NULL, test=NULL)
	featureName <- NULL

	# dim
	if (model=="pca") {

		if (data!="map") {
			modelData$train <- modelData$train[,-1]
		}
		
		m <- prcomp(modelData$train, scale.=TRUE, center=TRUE, ...)

		if (data=="map") {
			colnames(m$x) <- NULL
			obj$mat[[1]] <- m$x
			obj$aid$mapType <- sfMapType$dr
		} else {
			obj$tab <- cbind(obj$tab, m$x)
			obj$aid$dataType[[sfDataType$dr]] <- colnames(m$x)
		}

		m$x <- NULL

	}

	# linear

	if (model=="lin") {

		if (type=="class") {
			m <- glm(formula, data=modelData$train, family=binomial, ...)
		} else {
			m <- lm(formula, data=modelData$train, ...)
		}
		
		pred$train <- predict(m)
		if (type=="class"&&br==2) {
			pred$train <- cbind(1-pred$train, pred$train)
		}
			
		if (!is.null(modelData$test)) {
			pred$test <- predict(m, newdata=modelData$test)
			if (type=="class"&&br==2) {
				pred$test <- cbind(1-pred$test, pred$test)
			}	
		}
	}

	# clustering

	if (model=="kmeans") {

		if (!is.null(seed)) { set.seed(seed) }
		m <- kmeans(modelData$train, ...)
		obj <- initCluster(obj, m$cluster)
	}
	
	if (model=="kkmeans") {

		if (!is.null(seed)) { set.seed(seed) }
		m <- kernlab::kkmeans(modelData$train, ...)
		obj <- initCluster(obj, as.vector(m))
	}
	
	if (model=="dbscan") {

		if (!is.null(seed)) { set.seed(seed) }
		m <- dbscan::dbscan(modelData$train, ...)
		obj <- initCluster(obj, m$cluster+1)
	}
	
	if (model=="hdbscan") {

		if (!is.null(seed)) { set.seed(seed) }
		m <- dbscan::hdbscan(modelData$train, ...)
		obj <- initCluster(obj, m$cluster+1)
	}
	
	if (model=="optics") {

		if (!is.null(seed)) { set.seed(seed) }
		m <- dbscan::optics(modelData$train, ...)
		obj <- initCluster(obj, m$cluster+1)
	}
	
	# trees

	if (model=="cart") {

		if (is.null(method)) {
			if (type=="class") {
				method <- "class"
			} else {
				method <- "anova"
			}
		}
		ncatn(sprintf("Applying method %s", method), verbose=verbose)

		if (!is.null(seed)) { set.seed(seed) }
		m <- rpart::rpart(formula, data=modelData$train, method=method, ...)
		pred$train <- rpart:::predict.rpart(m, newdata=modelData$train)
		if (!is.null(modelData$test)) {
			pred$test <- rpart:::predict.rpart(m, newdata=modelData$test)
		}

	}

	# random forest

	if (model=="forest") {

		if (!is.null(seed)) { set.seed(seed) }

		if (data=="map") {
			m <- randomForest::randomForest(x=modelData$train, y=depend[spl], ...)
		} else {
			m <- randomForest::randomForest(formula, data=modelData$train, ...)
		}
	
		if (type=="class") {
			rfType <- "prob"
		} else {
			rfType <- "response"
		}
		pred$train <- randomForest:::predict.randomForest(m, newdata=modelData$train, type=rfType)
			
		if (!is.null(modelData$test)) {
			pred$test <- randomForest:::predict.randomForest(m, newdata=modelData$test, type=rfType)
		}
	}

	if (model=="boost") {
		
		if (!is.null(seed)) { set.seed(seed) }

		gbmType <- "response"
		if (type=="class"&br==2) {
			modelData$train[[1]] <- as.integer(modelData$train[[1]])-1
		}
		
		if (data=="map") {
			m <- gbm::gbm.fit(modelData$train, depend[spl], ...)
		} else {
			m <- gbm::gbm(formula, data=modelData$train, ...)
		}

		pred$train <- gbm:::predict.gbm(m, newdata=modelData$train, type=gbmType, n.trees=m$n.trees)
		
		if (type=="class"&br==2) {
			pred$train <- matrix(c(1-pred$train, pred$train), length(pred$train), 2)	
		}

		if (!is.null(modelData$test)) {
			pred$test <- gbm:::predict.gbm(m, newdata=modelData$test, type=gbmType, n.trees=m$n.trees)

			if (type=="class"&br==2) {
				pred$test <- matrix(c(1-pred$test, pred$test), length(pred$test), 2)	
			}
		}
		
	}
	
	if (model=="xboost") {
		
		if (!is.null(seed)) { set.seed(seed) }

		gbmType <- "response"
		if (type=="class") {
			if (br==2) {
				objective <- "binary:logistic"
			} else {
				objective <- "multi:softmax"
			}
			modelData$train[[1]] <- as.integer(modelData$train[[1]])-1
		} else {
			objective <- "reg:linear"
		}
		
		if (data=="map") {
			xgbTrain <- xgboost::xgb.DMatrix(data=modelData$train, label=depend[spl])
			m <- xgboost::xgb.train(data=xgbTrain, nthread=8, ...)
		} else {
			xgbTrain <- xgboost::xgb.DMatrix(data=as.matrix(modelData$train[,-1]), label=modelData$train[[1]])
			watchlist <- list(train=xgbTrain)
			if (!is.null(modelData$test)) {
				xgbValid <- xgboost::xgb.DMatrix(data=as.matrix(modelData$test[,-1]), label=modelData$test[[1]])
				watchlist <- list(train=xgbTrain, eval=xgbValid)
			}
			m <- xgboost::xgb.train(data=xgbTrain, objective=objective, watchlist=watchlist, nthread=8,...)
		}

		pred$train <- xgboost:::predict.xgb.Booster(m, newdata=as.matrix(modelData$train[,-1]))
		
		if (type=="class"&br==2) {
			pred$train <- matrix(c(1-pred$train, pred$train), length(pred$train), 2)	
		}

		if (!is.null(modelData$test)) {
			pred$test <- xgboost:::predict.xgb.Booster(m, newdata=as.matrix(modelData$test[,-1]))

			if (type=="class"&br==2) {
				pred$test <- matrix(c(1-pred$test, pred$test), length(pred$test), 2)	
			}
		}
		
		featureName <- colnames(modelData$train[,-1])
	}

	# naive bayes
	
	if (model=="bayes") {

		if (!is.null(seed)) { set.seed(seed) }

		if (type=="class") {
			for (i in 2:ncol(modelData$train)) {
				modelData$train[[i]] <- as.factor(modelData$train[[i]])
				if (!is.null(modelData$test)) {
					modelData$test[[i]] <- as.factor(modelData$test[[i]])
				}
			}
		}

		m <- e1071::naiveBayes(formula, data=modelData$train, ...)
	
		if (genStat) {

			pred$train <- e1071:::predict.naiveBayes(m, newdata=modelData$train, type="raw")
				
			if (!is.null(modelData$test)) {
				pred$test <- e1071:::predict.naiveBayes(m, newdata=modelData$test, type="raw")
			}
		}
	}

	# k nearest neighbor
	
	if (model=="knn") {
		m <- NULL
		pred$train <- class::knn(modelData$train, modelData$train, depend[spl], ...)
		if (!is.null(modelData$test)) {
			pred$test <- class::knn(modelData$train, modelData$test, depend[spl], ...)
		}
	}
	
	# support vector machines

	if (model=="svm") {
		
		if (is.null(method)) {method <- "r" }

		dependColMatch <- match(dependCol, colnames(modelData$train))
		if (!is.na(dependColMatch)) {
			modelData$train <- as.matrix(modelData$train[, -dependColMatch, with=FALSE])
		}

		if (!is.null(seed)) { set.seed(seed) }
		m <- kernlab::ksvm(x=modelData$train, y=depend[spl], type=svmType, ...)
		
		if (grepl("p", method)) {
			pred$train <- kernlab:::predict(m, modelData$train, type="probabilities")
			if (method=="pr") {
				pred$train <- apply(pred$train, 1, function(row) which.max(row)-1 )
			}
		} else {
			pred$train <- kernlab:::predict(m, modelData$train, type="response")
		}
		
		if (!is.null(modelData$test)) {
			dependColMatch <- match(dependCol, colnames(modelData$test))
			if (!is.na(dependColMatch)) {
				modelData$test <- as.matrix(modelData$test[, -dependColMatch, with=FALSE])
			}
			
			if (grepl("p", method)) {
				pred$test <- kernlab:::predict(m, newdata=modelData$test, type="probabilities")
				if (method=="pr") {
					pred$test <- apply(pred$test, 1, function(row) which.max(row)-1 )
				}
			} else {
				pred$test <- kernlab:::predict(m, newdata=modelData$test, type="response")
			}
		}
	}

	if (model=="nnet") {

		if (is.factor(depend)) {
			nnetDepend <- matrix(,sum(spl),0)
			for (lev in levels(depend)) {
				nnetDepend <- cbind(nnetDepend, ifelse(depend[spl]==lev, 1L, 0L))
			}
		}
		m <- nnet::nnet(x=modelData$train, y=nnetDepend, ...)
		
		pred$train <- nnet:::predict.nnet(m, modelData$train)
			
		if (!is.null(modelData$test)) {
			pred$test <- nnet:::predict.nnet(m, newdata=modelData$test, type="raw")
		}
	}

	stat <- list(train=NULL, test=NULL)
	if (type!="cluster"&genStat) {
	
		if (type=="class") {

			ncatn("Generating statistics...")
			stat$train <- classStat(depend[spl], pred$train, br=br, dependLevel=dependLevel)

			if (!is.null(modelData$test)) {
				stat$test <- classStat(depend[!spl], pred$test, br=br, dependLevel=dependLevel)
			}			
		}
		
		if (type=="reg") {

			ncatn("Generating statistics...")
			stat$train <- list(sse = sum((depend[spl]-pred$train)^2),
									sst = sum((depend[spl]-mean(depend[spl]))^2),
									mae=mean(abs(depend[spl]-pred$train)),
									rmse=sqrt(mean((depend[spl]-pred$train)^2)))
			stat$train$r2 <- 1-stat$train$sse/stat$train$sst
			
			if (!is.null(modelData$test)) {
				stat$test <- list(sse = sum((depend[!spl]-pred$test)^2),
									sst = sum((depend[!spl]-mean(depend[!spl]))^2),
									mae=mean(abs(depend[!spl]-pred$test)),
									rmse=sqrt(mean((depend[!spl]-pred$test)^2)))
				stat$test$r2 <- 1-stat$test$sse/stat$test$sst
			}
		}
	}

	if (is.null(obj$aid$model)) { obj$aid[["model"]] <- list() }
	obj$aid$model[[data]] <- list(type=type, model=model, method=method, stat=stat, dependCol=dependCol, balanceRow=balanceRow,
									spl=spl, depend=depend, dependLevel=dependLevel, pred=pred, br=br, m=m, 
									featureName=featureName)

	obj$aid$lastData <- data

	if (verbose) {
		summary(obj, details=FALSE)
	}

	return(obj)
}

#' @rdname model.semiFrame
#' @export
model.semiTable <- function(...) {
	return(model.semiFrame(..., data="par"))
}

#
# Standard Generics Methods
#

#' Model Summary
#' 
#' @description Produces various model summaries.
#' 
#' @template stsfobjParam
#' @template dataParam
#' @param details Logical. Specifies summary output extent. If \code{TRUE}, detailed function specific output is returned (default: \code{TRUE}).
#' @param special Character. Defines the output to be printed out. If \code{special="cp"}, cross-validation error and standard deviation
#' together with complexity parameter is depicted. If \code{special="importance"}, CART variable
#' importance is shown (default: "none"). 
#' @param modelThr Numeric. Specifies the model threshold (default: 0.5).
#' @param clusterThr Numeric. Threshold for cluster membership (default: 0.5).
#' @param give Logical. If \code{TRUE} returns statistics (default: \code{FALSE}).
#' @param first Integer. Specifies first \code{n} values of importance variables for the \code{pca} model (default: \code{NULL}).
#' @param positiveIdx Integer. In case of binary classifications defines the positive level to build the confusion table. Possible values 1 or 2 (default: 2).
#' 
#' @aliases summary
#' 
#' @return Argument specific summary output with result summaries.
#' 
#' @examples
#' 
#' data(wbm)
#' data(pcy)
#' 
#' # Show cluster membership summary together with particular bin values
#' summary(wbm)
#' 
#' # Train CART classification of 'ProbeYield' (median response split by default) on all parameters
#' pcy2 <- model(pcy, type="class", model="cart", splitRatio=1, formula="ProbeYield~.", cp=0.03)
#' 
#' # Get summary with cross-validation error, standard deviation and complexity parameter information
#' summary(pcy2, special="cp")
#' 
#' @rdname summary.semiFrame
#' @aliases summary
#' 
#' @export
summary.semiFrame <- function(obj, data="last", details=TRUE, special="none", modelThr=0.5, clusterThr=0.5, give=FALSE, first=NULL, positiveIdx=2) {

	ncatn()

	if (data=="last") { data <- obj$aid$lastData }

	type <- obj$aid$model[[data]]$type
	model <- obj$aid$model[[data]]$model

	catExamples <- function() {
		trainStat <- sum(obj$aid$model[[data]]$spl)
		testStat <- obj$aid$model[[data]]$spl
		testStat <- ifelse(is.null(testStat), NA, sum(!testStat))
		ncatn(sprintf("Examples train %d, test %d", trainStat, testStat))
	}

	clustStat <- function() {

		clustSum <- colSums(obj$tab[, dataType(obj)[[sfDataType$clust]], with=FALSE])
		names(clustSum) <- dataType(obj)[[sfDataType$clust]]
		
		nprintn(clustSum, k2=1)
		if (give) { return(clustSum) }

		if (details) {
			binStat <- function(rName, tabNum, matNRow) {
				tabNum <- tabNum[num>=0]
				denom <- matNRow*pdpw
				row <- data.table(matrix(round(100*tabNum[,n]/denom,2), 1, nrow(tabNum)))
				row <- cbind(data.table(rName), row)
				setnames(row, c("Category", paste0("bin", tabNum[, num])))
				return(row)
			}

			pdpw <- max(obj$tab[[sfName$pdpw]])
			binStatTab <- binStat("Total", tableNum(as.vector(obj$mat[[1]])), nrow(obj$mat[[1]]))

			for (clust in obj$aid$dataType[[sfDataType$clust]]) {
				clustRow <- which(obj$tab[.(1)][[clust]]>=clusterThr)
				if (length(clustRow)) {
					binStatTab <- rbind(binStatTab, binStat(clust, 
									tableNum(as.vector(obj$mat[[1]][clustRow,])), length(clustRow)), fill=TRUE)				
				}
			}
			setnames(binStatTab, "Category", "")
			nprintn(binStatTab, k2=1)
		}

	}

	if (is.null(type)) {

		if (all(c(sfName$lotWaferDie, sfName$fail, "ALL") %in% colnames(obj))) {
			if (!sfName$defect %in% colnames(obj)) {
				obj <- variable(obj, "defect", as.numeric(obj$tab$ALL>0),"C")
			}

			if (sfName$step %in% colnames(obj)) {

				for (useStep in unique(obj$tab[[sfName$step]])) {
					ncatn(useStep)
					print(caret::confusionMatrix(factor(obj$tab[eval(.sfName$step)==useStep][[sfName$defect]]), factor(obj$tab[eval(.sfName$step)==useStep][[sfName$fail]]), positive="1"))
				}
			} else {
				print(caret::confusionMatrix(factor(obj$tab[[sfName$defect]]), factor(obj$tab[[sfName$fail]]), positive="1"))
			}

			return(invisible())
		}

		if (sfDataType$clust %in% names(dataType(obj))) { clustStat() }

		return(invisible())
	}

	if (type=="cluster") {
		if (details) {
			ncatn(sprintf("Model %s, clustering", obj$aid$model[[data]]$model))
			catExamples()
		}

		clustStat()
	}
	
	if (type=="class") {
		if (details) {
			ncatn(sprintf("Model %s, classification of %s", obj$aid$model[[data]]$model, obj$aid$model[[data]]$dependCol))
			catExamples()
		}

		ncatn()

		trainStat <- obj$aid$model[[data]]$stat$train$acc
		testStat <- obj$aid$model[[data]]$stat$test$acc
		ncatn(sprintf("Accuracy train %.4f, test %.4f", trainStat, ifelse(is.null(testStat), NA, testStat) ))
		
		trainStat <- obj$aid$model[[data]]$stat$train$kappa
		testStat <- obj$aid$model[[data]]$stat$test$kappa
		ncatn(sprintf("Kappa train %.4f, test %.4f", trainStat, ifelse(is.null(testStat), NA, testStat) ))
		
		if (obj$aid$model[[data]]$br==2) {
			trainStat <- obj$aid$model[[data]]$stat$train$auc
			testStat <- obj$aid$model[[data]]$stat$test$auc
			ncatn(sprintf("AUC train %.3f, test %.3f", trainStat, ifelse(is.null(testStat), NA, testStat) ))

			dependLevel <- obj$aid$model[[data]]$dependLevel
			depend <- probMatToVec(obj$aid$model[[data]]$pred$train, positiveIdx=positiveIdx)
			depend <- factor(ifelse(depend>=modelThr,1,0))
			levels(depend) <- dependLevel
			trainStat <- caret::confusionMatrix(depend, obj$aid$model[[data]]$depend[obj$aid$model[[data]]$spl], positive=dependLevel[2])$byClass["F1"]
			if (!is.null(obj$aid$model[[data]]$pred$test)) {
				depend <- probMatToVec(obj$aid$model[[data]]$pred$test, positiveIdx=positiveIdx)
				depend <- factor(ifelse(depend>=modelThr,1,0))
				levels(depend) <- dependLevel
				testStat <- caret::confusionMatrix(depend, obj$aid$model[[data]]$depend[!obj$aid$model[[data]]$spl], positive=dependLevel[2])$byClass["F1"]
			}
			ncatn(sprintf("F1 train %.3f, test %.3f", trainStat, ifelse(is.null(testStat), NA, testStat) ))
					
					
			if (details) {
				ncatn("Train", k1=1)
				dependLevel <- obj$aid$model[[data]]$dependLevel
				depend <- probMatToVec(obj$aid$model[[data]]$pred$train, positiveIdx=positiveIdx)
				depend <- factor(ifelse(depend>=modelThr,1,0))
				levels(depend) <- dependLevel
				nprintn(caret::confusionMatrix(depend, obj$aid$model[[data]]$depend[obj$aid$model[[data]]$spl], positive=dependLevel[2]))
				if (!is.null(testStat)) {
					ncatn("Test")
					depend <- probMatToVec(obj$aid$model[[data]]$pred$test, positiveIdx=positiveIdx)
					depend <- factor(ifelse(depend>=modelThr,1,0))
					levels(depend) <- dependLevel
					nprintn(caret::confusionMatrix(depend, obj$aid$model[[data]]$depend[!obj$aid$model[[data]]$spl], positive=dependLevel[2]))
				}
			} else{
				ncatn()
			}
		} else {
			ncatn(sprintf("Multiclass: %d levels", obj$aid$model[[data]]$br), k2=2)
		}
	}
	
	if (type=="reg") {
		if (details) {
			ncatn(sprintf("Model %s, regression of %s", obj$aid$model[[data]]$model, obj$aid$model[[data]]$dependCol))
			catExamples()
		}
		trainStat <- obj$aid$model[[data]]$stat$train$mae
		testStat <- obj$aid$model[[data]]$stat$test$mae
		ncatn(sprintf("MAE train %s, test %s", signif(trainStat,3), ifelse(is.null(testStat), NA, signif(testStat, 3)) ))
		trainStat <- obj$aid$model[[data]]$stat$train$rmse
		testStat <- obj$aid$model[[data]]$stat$test$rmse
		ncatn(sprintf("RMSE train %s, test %s", signif(trainStat,3), ifelse(is.null(testStat), NA, signif(testStat, 3)) ))
		trainStat <- obj$aid$model[[data]]$stat$train$r2
		testStat <- obj$aid$model[[data]]$stat$test$r2
		ncatn(sprintf("R2 train %.3f, test %.3f", trainStat, ifelse(is.null(testStat), NA, testStat) ))
		ncatn()
	}

	if (details&&type=="dim") {

		if (model=="pca") {
			importance <- summary(modelM(obj))$importance
			if (!is.null(first)) {
				importance <- importance[,1:first]
			}
			print(importance)
		} else {
			print(summary(modelM(obj)))
		}
	}

	if (details&&model=="cart") {
		
		if (special=="cp") {
			cpTab <- rpart::printcp(obj$aid$model[[data]]$m)
			xErrxStd <- cpTab[nrow(cpTab),"xerror"]+cpTab[nrow(cpTab),"xstd"]
			ncatn("Best xerror + xstd =", xErrxStd, k1=1)
			cpIdx <- sort(which(cpTab[,"xerror"]<xErrxStd))[1]
			ncatn("Corresponding row: ", cpIdx,", cp = ",cpTab[cpIdx,"CP"], sep="")
		}

		if (special=="importance") {
			ncatn("cart variable importance")
			nprintn(modelM(obj)$variable.importance)
		}
		ncatn()
	}

	if (details&&model=="lin") {
		print(summary(obj$aid$model[[data]]$m))
	}

	return(invisible())
}

#' @rdname summary.semiFrame
#' @export
summary.semiTable <- function(...) {
	summary.semiFrame(...)
}

#' @export
print.mapCSR <- function(obj) {
	print(obj$summary)
	return(invisible())
}

#' @export
print.yieldDecomp <- function(obj, probDigits=4, yieldDigits=1) {
	
	yields <- obj$yields

	ncatn("Yield Product Decomposition e.g. Survival Probabilities", k1=1)

	# ncatn("UP = Probe * FOI",sprintf("%s ~ %s * %s",round(obj$product$mean[[yields$up]],probDigits), 
	# 	round(obj$product$mean[[yields$probe]],probDigits), round(obj$product$mean[[yields$foiCon]],probDigits)), k1=1, sep=" => ")

	# ncatn("Probe = Param * Random",sprintf("%s ~ %s * %s",round(obj$product$mean[[yields$probe]],probDigits), 
	# 	round(obj$product$mean[[yields$probeSys]],probDigits), round(obj$product$mean[[yields$probeRndCon]],probDigits)), sep=" => ")
	
	ncatn("UP = Sys * Rnd * FOI",sprintf("%s ~ %s * %s * %s",round(obj$product$mean[[yields$up]],probDigits), 
		round(obj$product$mean[[yields$probeSys]],probDigits), round(obj$product$mean[[yields$probeRndCon]],probDigits), 
		round(obj$product$mean[[yields$foiCon]],probDigits)), k1=1, sep=" => ")

	ncatn("Yield Loss Sum Decomposition", k1=1)
	
	ncatn("UPYieldLoss = SysYieldLoss + RndYieldLoss + FOIYieldLoss",sprintf("%s ~ %s + %s + %s",round(obj$loss$mean[[yields$upLoss]],yieldDigits), 
		round(obj$loss$mean[[yields$probeSysLoss]],yieldDigits), round(obj$loss$mean[[yields$probeRndLoss]],yieldDigits), 
		round(obj$loss$mean[[yields$foiLoss]],yieldDigits)), k1=1, k2=2, sep=" => ")

	return(invisible())
}



#
# Functions
#

#' Completes semiTable from semiImpute
#'
#' Completes semiTable from semiImpute, returns list of semiTables
#' @export
completeImpute <- function(siObj, omitNA=FALSE) {
 
	stList <- list()
	for (level in names(siObj$data)) {
		stList[[level]] <- list()
		for (j in 1:siObj$data[[level]]$m) {
			objTab <- cbind(siObj$meta[eval(as.name(siObj$splitCol))==level], as.data.table(mice::complete(siObj$data[[level]], j)))
			if (omitNA) {
				objTab <- na.omit(objTab)
			}
			stList[[level]][[j]] <- semiTableWrap(objTab, aid=siObj$aid)
		}
	}
	return(stList)
}


initCluster <- function(obj, clust) {

	objTab <- tab(obj)
	if (sfDataType$clust%in%names(dataType(obj))) {
		clustCol <- dataType(obj)[[sfDataType$clust]]
		for (i in 1:length(clustCol)) {
			objTab[,c(clustCol[i]):=list(NULL)]
		}
		obj$aid$dataType[[sfDataType$clust]] <- NULL
	}

	clustNum <- sort(unique(clust))
	clustTab <- matrix(0, length(clust), length(clustNum))

	for (clustN in clustNum) {
		clustTab[which(clust==clustN), clustN] <- 1
	}

	clustName <- paste0(sfName$clust, clustNum)
	clustTab <- setnames(as.data.table(clustTab), clustName)

	clustDataType <-list() 
	clustDataType[[sfDataType$clust]] <- clustName

	stObj <- cbind(semiTableWrap(objTab, dataType(obj)), semiTableWrap(clustTab, clustDataType))

	obj$tab <- 	tab(stObj)
	obj$aid$dataType <- dataType(stObj)
	return(obj)
}

#' Cluster Management
#'
#' @description Cluster management including: creation of new clusters, removing existing clusters, 
#' moving/coping of specific wafers, lots or wafers belonging to a certain cluster to another cluster. 
#' 
#' @template stsfobjParam
#' @param ... Parameters passed to provide bracket functionality for subsetting. In addition 3 parameters can be used:
#' \itemize{
#'   \item{ \code{from} }{ Specifies source cluster }
#'   \item{ \code{to} }{ Specifies target cluster }
#'   \item{ \code{remove} }{ Cluster for removal }
#' }
#' @param add Logical. Enables moving or copying of selected wafers, lots or wafers from a defined cluster 
#' into a cluster of choice. If \code{TRUE}, the selected group will be added (without alteration) to 
#' the specified cluster. If \code{FALSE}, the selected group will be moved to the specified cluster and 
#' removed from the original cluster (default: \code{FALSE}).
#' @param fromThr Numeric. Specifies the cluster threshold and expected in the [0,1] 
#' range. Only cluster group members with presence value bigger than \code{fromThr} will 
#' be included (default: 0.5).
#' @param toVal Numeric. Specifies target cluster value describing cluster presence. 
#' Expected in the [0,1] range. (default: 1)
#' @template verbose 
#' 
#' @return \code{semiTable} or \code{semiFrame} object
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Move all wafers from cluster clear to cluster center
#' cluster(wbm, from=clear, to=center)
#' 
#' # Add all wafers from cluster edge to cluster center
#' cluster(wbm, from=edge, to=center, add=TRUE)
#' 
#' # Add lot LOT003 to cluster clear
#' cluster(wbm, lot=="LOT003", to=clear, add=TRUE)
#' 
#' # Add wafer #9 from lot LOT002 to cluster center
#' cluster(wbm, lotWafer=="LOT002-W09", to=center, add=TRUE)
#' 
#' # Create a new cluster, include lot LOT005 with probeEndDate before 5th March 2016
#' cluster(wbm, lot=="LOT005"&probeEndDate<"2016-03-05", to=newCluster)
#' 
#' # Remove cluster edge
#' cluster(wbm, remove=edge)
#' 
#' @export
cluster <- function(obj, ..., add=FALSE, fromThr=0.5, toVal=1, verbose=TRUE) {

	if (!sfDataType$clust%in%names(dataType(obj))) { stop("There are no clusters") }
	
	clustCol <- dataType(obj)[[sfDataType$clust]]
	argsList <- eval(substitute(alist(...)))
	
	parentFrame <- parent.frame()
	objTab <- tab(obj)
	from  <- NULL
	to <- NULL
	del <- NULL
	if (!is.null(names(argsList))) {
		if ("from"%in%names(argsList)) { 
			from <- gsub("\"*","",deparse(argsList[["from"]]))
			if (from=="") { from <- NULL }
			argsList[["from"]] <- NULL
		}

		if ("to"%in%names(argsList)) { 
			to <- gsub("\"*","",deparse(argsList[["to"]]))
			if (to=="") { to <- NULL }
			argsList[["to"]] <- NULL
		}

		if ("remove"%in%names(argsList)) { 
			rem <- gsub("\"*","",deparse(argsList[["remove"]]))
			argsList[["remove"]] <- NULL
			if (rem%in%names(parentFrame)) { rem <- parentFrame[[rem]] }
			ncatn(sprintf("Removing cluster %s", rem), verbose=verbose)
			objTab[,c(rem):=list(NULL)]
			obj$aid$dataType[[sfDataType$clust]] <- setdiff(obj$aid$dataType[[sfDataType$clust]], rem)
			obj$tab <- objTab
		}
	} 

	if (is.null(from)&&is.null(to)&&!length(argsList)) {
		if (verbose) { summary(obj, details=FALSE) }
		return(obj)
	}
	
	if (length(argsList)>1) { argsList <- argsList[1] }
	if (length(argsList)) {
		if (deparse(argsList[[1]])%in%names(parentFrame)) {
			argsList[[1]] <- parentFrame[[deparse(argsList[[1]])]]
		}
	}

	if (!is.null(from)) {
		if (!from%in%clustCol) { 
			if (from%in%names(parentFrame)) {
				from <- parentFrame[[from]]
				if (from=="") { from <- clustCol }
			} else {
				stop(sprintf("Cluster %s not found", from)) 
			}
		}
		if (length(argsList)) {
			setLotWafer <- do.call('[', list(objTab, argsList[[1]]))[[sfName$lotWafer]]
		} else {
			setLotWafer <- objTab[[sfName$lotWafer]][objTab[[from]]>=fromThr]
		}
	} else {
		from <- clustCol
		setLotWafer <- do.call('[', list(objTab, argsList[[1]]))[[sfName$lotWafer]]
	}

	if (is.null(to)) {
		return(obj[eval(.sfName$lotWafer)%in%setLotWafer])
	}

	if ((!to%in%clustCol)&&(to!="del")) {
		if (to%in%names(parentFrame)) {
			to <- parentFrame[[to]]
		}
		if (!to%in%clustCol) {
			ncatn(sprintf("Creating cluster %s", to), verbose=verbose)
			toDataType <- list()
			toDataType[[sfDataType$clust]] <- to
			stObj <- cbind(semiTableWrap(objTab, dataType(obj)), semiTableWrap(setnames(data.table(rep(0, nrow(obj))), to), 
							toDataType))
			objTab <- stObj$tab
			obj$aid$dataType <- dataType(stObj)
		}
	} 

	if (!add) {
		from <- setdiff(from, to)
		ncatn(sprintf("Removing %d rows from cluster %s", length(setLotWafer), paste(from, collapse=", ")), verbose=verbose)
		objTab[eval(.sfName$lotWafer)%in%setLotWafer, c(from):=lapply(1:length(from), function(i) 0 )]
	}

	if (to!="del") {
		ncatn(sprintf("Setting %d rows to cluster %s", length(setLotWafer), to), verbose=verbose)
		objTab[eval(.sfName$lotWafer)%in%setLotWafer, c(to):=list(toVal)]
	}


	obj$tab <- objTab
	return(obj)
}

#' Create Cluster Heatmap
#' 
#' @description Creates heatmaps by averaging provided maps for each cluster. 
#'
#' @template stsfobjParam
#' @param thr Numeric. Specifies the cluster threshold and should be in the (0,1) range. Only cluster group members with
#' presence value bigger than \code{thr} will be included in the heatmap.
#' 
#' @return \code{semiFrame} object
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Create heatmaps of clusters for all bins
#' heat(bin(wbm, "all"))
#' 
#' @export
heat <- function(obj, thr=0.5) {

	if (!sfDataType$clust%in%names(dataType(obj))) { stop("There are no clusters") }
	clustName <- dataType(obj)[[sfDataType$clust]]

	sfHeat <- semiFrameWrap()
	for (matN in obj$matDim[[1]]) {
		for (clustN in clustName) {
			
			clustRow <- obj$tab[.(matN)][eval(as.name(clustN))>=thr, c(tmName$matN, tmName$matRow), with=FALSE]
			if (!nrow(clustRow)) { next }
			row <- clustRow[[tmName$matRow]] 
			
			rowTab <- obj$tab[which(eval(as.name(tmName$matN))==matN)[1],
				with(sfName, c(tmName$matN, tmName$matRow, lot, wafer, part, pdpw, tech, maskset)), with=FALSE]			
			rowTab[, c(tmName$matN, sfName$lot, sfName$wafer):=list(1L, sfName$clustLot, clustN)]
			rowMat <- list(matrix(colMeans(obj$mat[[matN]][row,,drop=FALSE]),1,ncol(obj$mat[[matN]])))
			rowMatDim <- obj$matDim[.(matN)]
			rowMatDim[, c(tmName$matN):=list(1L)]
			sfHeat <- rbind(sfHeat, semiFrameWrap(rowTab, rowMat, rowMatDim, setKey=TRUE))
		}
	}

	mapType(sfHeat) <- sfMapType$heat
	return(sfHeat)
}

#' CART Pruning
#' 
#' @description Relevant for a \code{semiTable} or \code{semiFrame} object carrying
#'    a CART model. The function determines a nested sequence of subtrees of the supplied 
#'    model by recursively snipping off the least important splits,
#'    based on the complexity parameter - \code{cp}.
#' 
#' @template stsfobjParam
#' @param ... Parameters passed to \code{rpart::prune}, e.g. \code{cp} - the complexity 
#'         parameter to which object will be trimmed.
#' @template dataParam
#' 
#' @details For more information on cost-complexity object pruning see \code{?rpart::prune}.
#'  
#' @return Copy of the \code{obj} object with pruned CART model.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Train CART classification of 'ProbeYield' (median response split by default) on all parameters
#' pcy2 <- model(pcy, type="class", model="cart", splitRatio=1, formula="ProbeYield~.", cp=0.03)
#' 
#' # Default decision tree plot
#' modelPlot(pcy2)
#' 
#' # Get summary with complexity parameter information
#' summary(pcy2, special="cp")
#' 
#' # Prune CART classification using the complexity parameter
#' pcy2 <- prune(pcy2, cp=0.05)
#' 
#' # Plot pruned CART classification 
#' modelPlot(pcy2)
#' 
#' @export
prune <- function(obj, ..., data="last") {

	m <- modelM(obj)
	if (is.null(m)) { stop("No model") }
	if (data=="last") { data <- obj$aid$lastData }
	if (obj$aid$model[[data]]$model!="cart") { stop("No cart model") }
	m <- rpart::prune(m, ...)
	obj$aid$model[[data]]$m <- m
	return(obj)
}

#' Yield Decomposition
#'
#' @description Decomposes unit probe yield into systematic yield, random yield and FOI yield.
#' 
#' @template stsfobjParam
#' @param yields List. Specifies the names of yield columns: 
#' \itemize{
#'   \item{ "up" }{ Unit probe yield column (default: "UPYield"). }
#'   \item{ "probe" }{ Probe yield column (default: "ProbeYield"). }
#'   \item{ "probeSys" }{ Systematic yield column (default: "ProbeYieldSys"). }
#'   \item{ "probeRnd" }{ Random yield column (default: "ProbeYieldRnd"). }
#' }
#' @param foiYield Character. Specifies Final Outgoing Inspection (FOI) yield column (default: "FOIYield").
#' 
#' @return Yield decomposition object.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Decompose unit probe yield into systematic, random and FOI yield 
#' # components.
#' yieldDecomp(pcy, list(up="UPYield", probe="ProbeYield", 
#'             probeSys="ProbeYieldSys", probeRnd="ProbeYieldRnd"), 
#'             foiYield="FOIYield")
#'
#' @export
yieldDecomp <- function(obj, yields=list(up="UPYield", probe="ProbeYield", probeSys="ProbeYieldSys", 
							probeRnd="ProbeYieldRnd"), foiYield="FOIYield") {


	objTab <- tab(obj)
	for (i in 1:length(yields)) {
		if (!yields[[i]]%in%colnames(objTab)) { stop(sprintf("Provide %s"), yields[[i]]) }
		objTab[, c(yields[[i]]):=list(eval(as.name(yields[[i]]))/100)]
	}

	yields$foi <- foiYield
	yields$foiCon <- paste0(yields$foi, "Con")
	yields$probeRndCon <- paste0(yields$probeRnd, "Con")

	obj$tab <- objTab

	obj <- variable(obj, yields$foi, obj$tab[[yields$up]]-obj$tab[[yields$probe]]+1, "y")
	obj <- variable(obj, yields$foiCon, 1-(1-obj$tab[[yields$foi]])/obj$tab[[yields$probe]], "y")
	obj <- variable(obj, yields$probeRndCon, 1-(1-obj$tab[[yields$probeRnd]])/obj$tab[[yields$probeSys]],"y")

	ydObjProduct <- list(mean=list(), median=list())
	for (y in c("up", "probe", "probeSys", "probeRndCon", "foiCon")) {
		ydObjProduct$mean[[yields[[y]]]] <- mean(obj$tab[[yields[[y]]]], na.rm=TRUE)
		ydObjProduct$median[[yields[[y]]]] <- median(obj$tab[[yields[[y]]]], na.rm=TRUE)
	}

	for (y in c("up", "probe", "probeSys", "probeRnd", "foi")) {
		obj <- variable(obj, paste0(yields[[y]],"Loss"), 1-obj$tab[[yields[[y]]]],"y")
		yields[[paste0(y,"Loss")]] <- paste0(yields[[y]],"Loss")
	}

	for (i in 1:length(yields)) {
		obj$tab[, c(yields[[i]]):=list(100*eval(as.name(yields[[i]])))]
	}


	ydObjLoss <- list(mean=list(), median=list())
	for (y in c("upLoss", "probeLoss", "probeSysLoss", "probeRndLoss", "foiLoss")) {
		ydObjLoss$mean[[yields[[y]]]] <- mean(obj$tab[[yields[[y]]]], na.rm=TRUE)
		ydObjLoss$median[[yields[[y]]]] <- median(obj$tab[[yields[[y]]]], na.rm=TRUE)
	}

	ydObj <- list(data=obj, product=ydObjProduct, loss=ydObjLoss, yields=yields)
	class(ydObj) <- "yieldDecomp"
	return(ydObj)
}

#' Process Equipment Table 
#'
#' @description Creates process equipment table from lot equipment history data by counting 
#' usage for each equipment thus transforming per stage view into per equipment view.   
#' 
#' @template stsfobjParam
#' 
#' @return Process equipment table.
#' 
#' @export
peTable <- function(obj) {

	# allPE <- character()
	# for (i in p1st:ncol(obj)) {
	# 	allPE <- c(allPE, as.character(na.omit(obj[,i])))
	# }
	# allPE <- sort(unique(allPE))

	lehCol <- dataType(obj)[[sfDataType$leh]]

	if (is.null(lehCol)) { stop("No leh columns") }

	allPE <- character()	
	for (col in lehCol) {
		allPE <- c(allPE, as.character(na.omit(obj$tab[[col]])))
	}
	allPE <- sort(unique(allPE))

	lehPE <- obj$tab[,lehCol,with=FALSE]

	PEmat <- matrix(0, nrow(lehPE), length(allPE))
	colnames(PEmat) <- allPE

	for (row in 1:nrow(lehPE)) {
		lotPEtab <- table(t(lehPE[row,]))
		PEmat[row, names(lotPEtab)] <- lotPEtab
	}
	
	rownames(PEmat) <- NULL

	PEmatDataType <- list(colnames(PEmat))
	names(PEmatDataType) <- sfDataType$leh
	PEtab <- cbind(obj[,dataType(obj)[[sfDataType$meta]],with=FALSE], semiTableWrap(PEmat,PEmatDataType))

	return(PEtab)
}


#' Multiclass Cluster Column Unfold
#' 
#' @description Unfolds multiclass cluster column into single class cluster columns.
#' 
#' @template stsfobjParam
#' @param col Character. Defines column name of the categorical variable to unfold into separate cluster variables.
#' @param labels Character. Either single name root for new variables or vector of labels for each category (default: "clust").
#' @param add Logical. If \code{TRUE} newly spawned variables will be added to the former ones, if \code{FALSE} former cluster variables will be removed (default: \code{TRUE}).
#' 
#' @return Copy of the semiTable or semiFrame object with unfolded cluster variables.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Create a multi class cluster variable
#' pcy2 <- variable(pcy, "multiClass", c("A", "B", "C")[round(runif(nrow(pcy), 1, 3))], "C")
#' # Unfold a multi class variable into regular cluster variables
#' unfoldCluster(pcy2, "multiClass", add=TRUE)
#' 
#' @export
unfoldCluster <- function(obj, col, labels="clust", add=TRUE) {

	if (is.character(col)&&length(col)==1) {
		col <- obj[[col]]
	}

	clustUniq <- sort(unique(col))

	if (length(labels)!=length(clustUniq)) {
		labels <- paste0(labels[1], clustUniq)
	}

	if (!add) {
		oldCluster <- dataType(obj)[[sfDataType$clust]]
		obj <- variable(obj, oldCluster, NULL)
	}

	for (i in 1:length(clustUniq)) {
		obj <- variable(obj, labels[i], as.integer(col==clustUniq[i]),sfDataType$clust)
	}
	return(obj)
}
