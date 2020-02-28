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

  ### fields ----
  #' @field positive - the "positive" class
  positive = NULL,

  #### constructor ----
  #' @description Creates a distribution
  #' @param predictionProbabilities a matrix of probabilities with rows for every sample, and colums for every possible class
  #' @param actual the gold standard output as a vector of outcomes for each sample
  #' @param ... passed to pdfFunction and centileFunction
  initialize = function(data, classes) {
    super$initialize(data,classes)
    self$positive = self$classes[[1]]
  },

  #### methods ----
  setPositive = function(className) {
    self$positive = className
    invisible(self)
  },

  plotLegend = function() {
    annotations2 = self$maxValues(vars(accuracy,f1,mcc,informedness,I))
    tmp_plot =
      ggplot(annotations2,aes(x=specificity))+
      geom_bar(aes(fill=prediction),colour="black",alpha=0.3,position = "identity")+
      geom_point(aes(colour=paste0("max ",max_of),y=sensitivity),alpha=0.75)+
      geom_vline(aes(colour=paste0("max ",max_of),xintercept=specificity),alpha=0.75)+
      labs(colour=NULL,fill=NULL)+standardPrintOutput::narrowAndTall()+
      scale_colour_brewer(type="qual",palette="Dark2")+
      scale_fill_brewer(type="qual",palette="Set1")
    legend = ggpubr::get_legend(tmp_plot)
    patchwork::wrap_elements(panel = legend)
  },

  plot = function() {
    patchwork::wrap_plots(
        self$plotRoc(labels=FALSE), self$plotPR(labels=FALSE), self$plotDensity(labels=FALSE), nrow = 1
    )
  },

  plotRoc = function(labels=TRUE) {
    tmp = self$getInfoStats(prediction == self$positive)
    stats = self$distributionStats(prediction == self$positive)
    annotations = self$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == self$positive)
    #annotations = cr$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == "abnormal")
    p= ggplot(tmp, aes(x=false_pos_rate, y=true_pos_rate))+
        geom_path()+
        xlab("false positive rate")+
        ylab("true positive rate")+
        geom_segment(x=0,y=0, xend=1, yend=1, colour="grey75",linetype="dashed")+
        expand_limits(x=c(0,1),y=c(0,1))+
        annotate("text",x=0.99,y=0.01,label=paste0(
          "AUROC=",sprintf("%.3f",stats$auroc)
        ),vjust=0,hjust=1,size=(10/ggplot2:::.pt/(96/72)))+
        geom_point(aes(x=false_pos_rate, y=true_pos_rate, colour=max_of),data=annotations,alpha=0.75,show.legend = FALSE)+
        scale_colour_brewer(type="qual",palette="Dark2")+
        guides(colour="none")
    if (labels) p = p +
        ggrepel::geom_text_repel(
          aes(x=false_pos_rate, y=true_pos_rate, colour=max_of, label=paste0("max ",max_of)), hjust=0,vjust=1, nudge_x=0.01,nudge_y=-0.01,data=annotations,
          size=(10/ggplot2:::.pt/(96/72)), show.legend = FALSE)+
        ggnewscale::new_scale_colour()+
        geom_rug(aes(colour=obs), alpha=0.1, show.legend = FALSE) +
        scale_colour_brewer(type="qual",palette="Set1")+
        guides(colour="none")
    return(p)
  },

  plotDensity = function(labels=TRUE) {
    tmp = self$getInfoStats(prediction == self$positive)
    stats = self$distributionStats(prediction == self$positive)
    annotations = self$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == self$positive)
    total = max(tmp$total)
    prevalence = max(tmp$p_x1)
    p = ggplot()+
        geom_density(aes(x=probability, y=stat(count)/total,fill=obs),data = tmp,colour="black",alpha=0.3) +
        scale_fill_brewer(type="qual",palette="Set1")+
        #annotate("label",x=0.01,y=0.01,label=paste0("MI=",sprintf("%.3f",stats$mutualInfo)
        #),vjust=0,hjust=0,size=(10/ggplot2:::.pt/(96/72)))+
        geom_hline(aes(yintercept=p_x1), data = tmp, colour="grey25",linetype="dotted")+
        geom_hline(aes(yintercept=1-p_x1), data = tmp, colour="grey25",linetype="dashed")+
        scale_colour_brewer(type="qual",palette="Dark2")+
        geom_vline(aes(xintercept=probability, colour=max_of),data=annotations, show.legend = FALSE,alpha=0.75)+xlim(0,1)+
      xlab(paste0("prediction ",self$positive))+
      ylab("density")
    if (labels) {
      p = p +  ggrepel::geom_text_repel(
          aes(x=probability, y=Inf, colour=max_of, label=paste0("max ",max_of)),data=annotations, hjust=0,vjust=1, angle=90, show.legend = FALSE,box.padding=0.05,
          size=(10/ggplot2:::.pt/(96/72)))+
        ggnewscale::new_scale_colour()+
        geom_rug(aes(x=probability, colour=obs), data = tmp, sides="b", alpha=0.1, show.legend = FALSE) +
        scale_colour_brewer(type="qual",palette="Set1")
    } else {
      p = p + guides(fill="none")
    }
    return(p)
  },

  plotStats = function(statisticVars = vars(neg_pred_value,pos_pred_value,specificity,sensitivity,precision,recall,accuracy,f1,mcc,informedness,I)) {
    tmp = self$getInfoStats(prediction == self$positive) %>% ungroup()
    tmp2 = tmp %>% select(probability, !!!statisticVars)
    tmp2 = tmp2 %>% tidyr::pivot_longer(cols=sapply(statisticVars,as_label), names_to = "statistic")
    return(ggplot(tmp2,aes(x=probability, y=value, colour=statistic))+geom_line()+xlab(paste0("cut off ",self$positive)))
  },

  plotPR = function(labels=TRUE) {
    tmp = self$getInfoStats(prediction == self$positive)
    stats = self$distributionStats(prediction == self$positive)
    annotations = self$maxValues(vars(accuracy,f1,mcc,informedness,I),prediction == self$positive)
    p = ggplot(tmp, aes(x=recall, y=precision))+
      geom_path()+
      xlab("recall")+
      ylab("precision")+
      geom_segment(aes(x=0,y=p_x1, xend=1, yend=p_x1), colour="grey75",linetype="dashed")+
      expand_limits(x=c(0,1),y=c(0,1))+
      annotate("text",x=0.99,y=0.01,label=paste0(
        "AUPRC=",sprintf("%.3f",stats$auprc)
      ),vjust=0,hjust=1,size=(10/ggplot2:::.pt/(96/72)))+
      geom_point(aes(x=recall, y=precision, colour=max_of),data=annotations,alpha=0.75,show.legend = FALSE)+
      scale_colour_brewer(type="qual",palette="Dark2")
    if (labels) p = p+ggrepel::geom_text_repel(
      aes(x=recall, y=precision, colour=max_of, label=paste0("max ",max_of)), hjust=0,vjust=0, nudge_x=0.01,nudge_y=0.01,data=annotations,
      size=(10/ggplot2:::.pt/(96/72)), show.legend = FALSE)+
      ggnewscale::new_scale_colour()+
      geom_rug(aes(colour=obs), alpha=0.1, show.legend = FALSE) +
      scale_colour_brewer(type="qual",palette="Set1")
    return(p)
  }



))

