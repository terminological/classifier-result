#' ClassifierResult class
#'
#' result
#'
#' @keywords distributions
#' @import dplyr
#' @import ggplot2
#' @export
ClassifierResult = R6::R6Class("ClassifierResult", public=list(

  #### Fields ----
  #' @field data - the classifier prediction probabilities and outcomes
  data = NULL,
  #' @field classes - the classes of the outcome as a vector
  classes = NULL,
  #' @field binaryInfo - the binary info stats
  binaryInfo = NULL,

  #### Methods ----
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

  getInfoStats = function() {
    self$binaryInfo
  },

  plotRoc = function() {
    return(
      ggplot(self$binaryInfo, aes(x=false_pos_rate, y=true_pos_rate, colour=prediction))+
        geom_path()+
        xlab("False positive rate")+
        ylab("True positive rate")+
        geom_segment(x=0,y=0, xend=1, yend=1, colour="grey75",linetype="dashed")+
        expand_limits(x=c(0,1),y=c(0,1))
    )
  },

  maxValue = function(statisticVar, ...) {
    statisticVar = ensym(statisticVar)
    return(self$binaryInfo %>% filter(...) %>% group_by(prediction) %>% mutate(tmp_max = !!statisticVar) %>% filter(tmp_max == max(tmp_max,na.rm=TRUE)) %>% select(-tmp_max))
  },

  maxValues = function(statisticVars, ...) {
    tmp = self$binaryInfo %>% filter(...) %>% group_by(prediction)
    out = NULL
    for (statisticVar in statisticVars) {
      out = out %>% bind_rows(
        tmp %>% mutate(tmp_max = !!statisticVar) %>% filter(tmp_max == max(tmp_max,na.rm=TRUE)) %>% select(-tmp_max) %>% mutate(max_of = as_label(statisticVar))
      )
    }
    return(out)
  }


))


#### Multiclass classifier ----

#' MulticlassClassifierResult class
#'
#' result
#'
#' @keywords distributions
#' @import dplyr
#' @import ggplot2
#' @export
MulticlassClassifierResult = R6::R6Class("MulticlassClassifierResult", inherit=ClassifierResult, public=list(

  #### Methods ----
  #' @description Creates a distribution
  #' @param ... passed to pdfFunction and centileFunction
  initialize = function(data, classes) {
    super$initialize(data,classes)
  },

  getBinaryClassifiers = function() {
    out = lapply(self$classes, function(c) {
      BinaryClassifierResult$new(self$data,self$classes)$setPositive(c)
    })
    names(out) <- self$classes
    return(out)
  },

  getClasses = function() {
    return(self$classes)
  }

))

#### Binary classifier ----

#' ClassifierResult class
#'
#' result
#'
#' @keywords distributions
#' @import dplyr
#' @import ggplot2
#' @export
BinaryClassifierResult = R6::R6Class("BinaryClassifierResult", inherit=ClassifierResult, public=list(

  ### Fields ----
  #' @field positive - the "positive" class
  positive = NULL,

  #### Methods ----
  #' @description Creates a distribution
  #' @param predictionProbabilities a matrix of probabilities with rows for every sample, and colums for every possible class
  #' @param actual the gold standard output as a vector of outcomes for each sample
  #' @param ... passed to pdfFunction and centileFunction
  initialize = function(data, classes) {
    super$initialize(data,classes)
    self$positive = self$classes[[1]]
  },

  setPositive = function(className) {
    self$positive = className
    invisible(self)
  },

  plotRoc = function() {
    tmp = self$getInfoStats() %>% filter(prediction == self$positive)
    stats = self$distributionStats()
    annotations = self$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == self$positive)
    #annotations = cr$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == "abnormal")
    return(
      ggplot(tmp, aes(x=false_pos_rate, y=true_pos_rate))+
        geom_path()+
        xlab("false positive rate")+
        ylab("true positive rate")+
        geom_segment(x=0,y=0, xend=1, yend=1, colour="grey75",linetype="dashed")+
        expand_limits(x=c(0,1),y=c(0,1))+
        annotate("label",x=0.99,y=0.01,label=paste0(
          "AUC=",sprintf("%.3f",stats$auroc),
          "\nMI=",sprintf("%.3f",stats$mutualInfo)
        ),vjust=0,hjust=1)+
        geom_point(aes(x=false_pos_rate, y=true_pos_rate, colour=max_of),data=annotations)+
        ggrepel::geom_text_repel(aes(x=false_pos_rate, y=true_pos_rate, colour=max_of, label=paste0("max ",max_of)), hjust=0,vjust=1, nudge_x=0.01,nudge_y=-0.01,data=annotations)+
        guides(colour="none")
    )
  },

  plotDensity = function() {
    tmp = self$getInfoStats() %>% filter(prediction == self$positive)
    annotations = self$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == self$positive)
    return(
      ggplot(tmp, aes(x=probability, fill=obs))+
        geom_density(colour="black",alpha=0.3) +
        geom_vline(aes(xintercept=probability, colour=max_of),data=annotations)+
        ggrepel::geom_text_repel(aes(x=probability, y=Inf, colour=max_of, label=paste0("max ",max_of)),data=annotations, hjust=1,vjust=1, angle=90)+
        guides(colour="none")+xlab(paste0("probability ",self$positive))
    )
  },

  plotStats = function(statisticVars = vars(neg_pred_value,pos_pred_value,specificity,sensitivity,precision,recall,accuracy,f1,mcc,informedness,I)) {
    tmp = self$getInfoStats() %>% filter(prediction == self$positive) %>% ungroup()
    tmp2 = tmp %>% select(probability, !!!statisticVars)
    tmp2 = tmp2 %>% tidyr::pivot_longer(cols=sapply(statisticVars,as_label), names_to = "statistic")
    return(ggplot(tmp2,aes(x=probability, y=value, colour=statistic))+geom_line()+xlab(paste0("cut off ",self$positive)))
  },

  plotPR = function() {
    tmp = self$getInfoStats() %>% filter(prediction == self$positive)
    stats = self$distributionStats()
    annotations = self$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == self$positive)
    ggplot(tmp, aes(x=recall, y=precision))+
      geom_path()+
      xlab("recall")+
      ylab("precision")+
      geom_segment(x=0,y=0.5, xend=1, yend=0.5, colour="grey75",linetype="dashed")+
      expand_limits(x=c(0,1),y=c(0,1))+
      annotate("label",x=0.99,y=0.99,label=paste0(
        "AUC=",sprintf("%.3f",stats$auprc),
        "\nMI=",sprintf("%.3f",stats$mutualInfo)
      ),vjust=1,hjust=1)+
      geom_point(aes(x=recall, y=precision, colour=max_of),data=annotations)+
      ggrepel::geom_text_repel(aes(x=recall, y=precision, colour=max_of, label=paste0("max ",max_of)), hjust=0,vjust=0, nudge_x=0.01,nudge_y=0.01,data=annotations)+
      guides(colour="none")
  },

  distributionStats = function() {
    tmp = self$getInfoStats() %>% filter(prediction == self$positive)
    tmp2 = tmp %>% filter(!is.na(recall) & !is.na(precision))
    out=list(
      auroc = DescTools::AUC(tmp$false_pos_rate, tmp$true_pos_rate), #na.rm=TRUE), #, from=0, to=1),
      auprc = DescTools::AUC(tmp2$recall, tmp2$precision), #, from=0, to=1),
      mutualInfo = tidyinfostats::calculateDiscreteContinuousMI(tmp, vars(obs), probability) %>% pull(I),
      max_accuracy = self$maxValue(accuracy, prediction == self$positive)$accuracy,
      max_informedness = self$maxValue(informedness, prediction == self$positive)$informedness
    )
    return(out)
  }



))

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
