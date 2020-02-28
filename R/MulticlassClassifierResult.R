#' ClassifierResult class
#'
#' result
#'
#' @keywords distributions
#' @import dplyr
#' @import ggplot2
#' @export
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

  #### constructor ----
  #' @description Creates a distribution
  #' @param ... passed to pdfFunction and centileFunction
  initialize = function(data, classes) {
    super$initialize(data,classes)
  },

  #### methods ----
  getBinaryClassifiers = function() {
    out = lapply(self$classes, function(c) {
      tmp = self$data %>% filter(prediction==c) %>% mutate(prediction = as.character(prediction))
      tmp2 = bind_rows(
        tmp,
        tmp %>% mutate(prediction = paste0("not ",prediction), probability = 1-probability)
      )
      return(BinaryClassifierResult$new(tmp2,unique(tmp2$prediction))$setPositive(c))
    })
    names(out) <- self$classes
    return(out)
  },

  getClasses = function() {
    return(self$classes)
  },

  plotRoc = function() {
    stop("not implemented")
    return(
      ggplot(self$binaryInfo, aes(x=false_pos_rate, y=true_pos_rate, colour=prediction))+
        geom_path()+
        xlab("False positive rate")+
        ylab("True positive rate")+
        geom_segment(x=0,y=0, xend=1, yend=1, colour="grey75",linetype="dashed")+
        expand_limits(x=c(0,1),y=c(0,1))
    )
  }

))
