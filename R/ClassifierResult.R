#' ClassifierResult class
#'
#' result
#'
#' @keywords distributions
#' @import dplyr
#' @import ggplot2
#' @export
ClassifierResult = R6::R6Class("ClassifierResult", public=list(

  #### fields ----
  #' @field data - the classifier prediction probabilities and outcomes if from a prediction
  data = NULL,
  #' @field classes - the classes of the outcome as a vector
  classes = NULL,
  #' @field binaryInfo - the binary info stats
  binaryInfo = NULL,

  #### constructor ----
  #' @description Creates a distribution
  #' @param predictionProbabilities a matrix of probabilities with rows for every sample, and colums for every possible class
  #' @param actual the gold standard output as a vector of outcomes for each sample
  #' @param ... passed to pdfFunction and centileFunction
  initialize = function(data, classes) {
    self$data = data
    self$classes = classes

    self$binaryInfo = data %>% group_by(prediction) %>% arrange(probability) %>%
      mutate(
        rank = row_number() # predicted negative (for a given cut off)
      ) %>%
      mutate(
        N=max(rank), # total
        match = ifelse(prediction==obs,1,0)
      ) %>% mutate(
        matches=cumsum(match), # false negatives
        total_matches = sum(match) # total actual positives
      ) %>% mutate(
        # at a given value of probability we have
        truePos = total_matches-matches,
        predPos = N-rank, # total - predicted negative
        obsPos = total_matches,
        total = N
      ) %>% select(
        -c(matches,rank,N,total_matches)
      ) %>%
      tidyinfostats::probabilitiesFromCounts(truePos,obsPos,predPos,total) %>%
      tidyinfostats::calculateConfusionMatrixStats() %>%
      tidyinfostats::calculateBinaryMI()
  },

  #### methods ----
  getInfoStats = function(...) {
    self$binaryInfo %>% filter(...)
  },

  maxValue = function(statisticVar, ...) {
    return(self$maxValues(vars({{statisticVar}}),...))
  },

  maxValues = function(statisticVars, ...) {
    tmp = self$binaryInfo %>% filter(...) %>% group_by(prediction)
    out = NULL
    for (statisticVar in statisticVars) {
      out = out %>% bind_rows(
        tmp %>% mutate(tmp_max = !!statisticVar) %>% filter(tmp_max == max(tmp_max,na.rm=TRUE)) %>% select(-c(tmp_max,obs)) %>%
          mutate(max_of = as_label(statisticVar)) %>%
          group_by(max_of,prediction) %>%
          summarise_all(mean)
      )
    }
    return(out)
  },

  distributionStats = function(...) {
    tmp = self$getInfoStats() %>% filter(...)
    tmp %>% group_by(prediction) %>% group_modify(function(d,g,...) {
      tmp2 = d %>% filter(!is.na(recall) & !is.na(precision))
      out=tibble(
        auroc = DescTools::AUC(d$false_pos_rate, d$true_pos_rate), #na.rm=TRUE), #, from=0, to=1),
        auprc = DescTools::AUC(tmp2$recall, tmp2$precision), #, from=0, to=1),
        mutualInfo = tidyinfostats::calculateDiscreteContinuousMI(d, vars(obs), probability) %>% pull(I),
        max_accuracy = self$maxValue(accuracy, prediction == self$positive)$accuracy,
        max_informedness = self$maxValue(informedness, prediction == self$positive)$informedness,
        outcome_prevalence = max(d$p_x1)
      )
    return(out)
    })
  }

))

#### static methods ----

#' ClassifierResult factory
#'
#'
#' @keywords distributions
#' @param predictionProbabilities a matrix of probabilities with rows for every sample, and colums for every possible class
#' @param obs the gold standard output as a vector of outcomes for each sample
#' @export
ClassifierResult$fromPredictions = function(predictionProbabilities, obs) {
    tmp = data.frame(predictionProbabilities) %>% mutate(tmp_obs=as.character(obs), sample = row_number())
    tmp = tmp %>% tidyr::pivot_longer(cols = colnames(predictionProbabilities), names_to = "prediction", values_to = "probability") %>% rename(obs = tmp_obs)
    classes = unique(c(tmp$prediction, tmp$obs))
    if(length(classes) < 2) stop("not enough classes...?")
    if(length(classes) == 2) {
      # tmp = tmp %>% filter(prediction==classes[[1]])
      return(BinaryClassifierResult$new(tmp,classes))
    } else {
      return(MulticlassClassifierResult$new(tmp,classes))
    }
}

#' ClassifierResult factory
#'
#'
#' @keywords distributions
#' @param predictionProbabilities a matrix of probabilities with rows for every sample, and colums for every possible class
#' @param obs the gold standard output as a vector of outcomes for each sample
#' @export
ClassifierResult$fromConditionalDistribution = function(cd) {
  # OK so we need to:
  ## get the index of the positive class, in terms of distributions
  ## use bigP for each distribution and prevalence measure to decide p_x0y0 and p_x0y1 amd their complements
  ## create a BinaryClassifierResult.
  ## change the constructor to deal with direct provision of a confusion matrix
  tmp = cd$getInverseCdf() %>% mutate(not_x = 1-x) %>% rename(sample = i, obs=y)
  tmp = tmp %>% filter(x!=0 & x!=1) # lose ends as they tend to skew PR curve for example
  tmp = tmp %>% tidyr::pivot_longer(cols = c("x","not_x"), names_to = "prediction", values_to = "probability")
  tmp = tmp %>% mutate(prediction = ifelse(prediction == "x", cd$classes[[2]],cd$classes[[1]]))
  classes = unique(c(tmp$prediction, tmp$obs))
  if(length(classes) != 2) stop("wrong number of classes...?")
  return(BinaryClassifierResult$new(tmp,classes)$setPositive(cd$classes[[1]]))
}

#### TODO: ----
# ClassifierResult from simulation / from probability functions
# This currently create creates a simulated classifier output then calculates the probability distributions.
# However we can bypass this and directly create the "correct" probability stats direct from the underlying distributions
# particularly as we want to do this for lots of different distributions.
# ALSO: if we are generalising this we need to lose the concept of probability as the x axis and have a more generic variable e.g. H.b. and some form of support range
