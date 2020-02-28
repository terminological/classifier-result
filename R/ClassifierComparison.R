#### Classifier comparison ----
#' ClassifierComparison class
#'
#' result
#'
#' @keywords distributions
#' @import dplyr
#' @import ggplot2
#' @export
ClassifierComparison = R6::R6Class("ClassifierComparison", public=list(

  #### fields ----
  #' @field data - the classifier prediction probabilities and outcomes
  classifiers = list(),

  #### constructor ----
  initialize = function() {
    self$classifiers = list()
  },

  #### methods ----
  #' adds a classifier result to the list
  #' @param parameterList a list of parameters used to generate this classifier result
  #' @return the ClassifierComparison
  withClassifier = function(parameterList, predicitionProbabilities, obs) {
    classifier = list(
      parameters = parameterList,
      result = ClassifierResult$fromPredictions(predicitionProbabilities, obs)
    )
    self$classifiers = c(self$classifiers, list(classifier)) # list wrapper to prevent R concatenating classifier
    invisible(self)
  },

  #### methods ----
  #' adds a classifier result to the list
  #' @param parameterList a list of parameters used to generate this classifier result
  #' @return the ClassifierComparison
  withResult = function(parameterList, classifierResult) {
    classifier = list(
      parameters = parameterList,
      result = classifierResult
    )
    self$classifiers = c(self$classifiers, list(classifier)) # list wrapper to prevent R concatenating classifier
    invisible(self)
  },

  compareMaxValues = function(statisticVars,...) {
    out = NULL
    for (classifier in self$classifiers) {
      paramsDf = as.data.frame(classifier$parameters) %>% mutate(tmp_join=1)
      valuesDf = classifier$result$maxValues(statisticVars,...) %>% mutate(tmp_join=1)
      out = out %>% bind_rows(paramsDf %>% left_join(valuesDf, by="tmp_join") %>% select(-tmp_join))
    }
    return(out)
  },

  compareDistributionStats = function(...) {
    out = NULL
    for (classifier in self$classifiers) {
      paramsDf = as.data.frame(classifier$parameters) %>% mutate(tmp_join=1)
      valuesDf = classifier$result$distributionStats(...) %>% mutate(tmp_join=1)
      out = out %>% bind_rows(paramsDf %>% left_join(valuesDf, by="tmp_join") %>% select(-tmp_join))
    }
    return(out)
  },

  getInfoStats = function() {
    out = NULL
    for (classifier in self$classifiers) {
      paramsDf = as.data.frame(classifier$parameters) %>% mutate(tmp_join=1)
      valuesDf = classifier$result$getInfoStats() %>% mutate(tmp_join=1)
      out = out %>% bind_rows(paramsDf %>% left_join(valuesDf, by="tmp_join") %>% select(-tmp_join))
    }
    return(out)
  },

  plotRocs = function(titleFromParamsFn = function(params) {paste(names(params),"=",params,collapse = "; ")}) {
    out = list()
    legend = NULL
    for (classifier in self$classifiers) {
      #TODO: Does not work for multiclass classifiers
      title = titleFromParamsFn(classifier$parameters)
      plot = classifier$result$plotRoc(labels=FALSE) + labs(title=title)
      if (is.null(legend)) legend = classifier$result$plotLegend()
      out = c(out,list(plot)) # ggplots are list objects so must be wrapped
    }
    return(
      patchwork::wrap_plots(patchwork::wrap_plots(out,ncol=3),legend,ncol=1,heights = c(ceiling(length(out)/3),0.1))
    )
  }

))

#### static methods ----
