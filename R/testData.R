#' tidy dataframe of the simulation of blood test results with known distributions for individual outcomes.
#' 
#' @import dplyr
#' @export
bloodResultsSimulation = function(n,seed = 101) {
  
  set.seed(seed)
  
  hb = ClassifierResult::ConditionalDistribution$new()
  hb$withDistribution(LogNormalDistribution$new(mode=12,sd=1.3), "asymptomatic")
  hb$withDistribution(LogNormalDistribution$new(mode=8,sd=1.5), "tired")
  hb$withDistribution(LogNormalDistribution$new(mode=4,sd=2), "unwell")
  
  k = ClassifierResult::ConditionalDistribution$new()
  k$withDistribution(NormalDistribution$new(mean=1,sd=0.5), "unwell")
  k$withDistribution(NormalDistribution$new(mean=2,sd=1), "asymptomatic")
  k$withDistribution(NormalDistribution$new(mean=8,sd=3), "tired")
  
  mvd = ClassifierResult::MultivariableDistribution$new()
  mvd$withConditionalDistribution(hb,"haemoglobin")
  mvd$withConditionalDistribution(k,"serum k")
  
  mvd$withClassWeights(list(
    unwell=0.2,
    asymptomatic=0.6,
    tired=0.3
  ))
  
  return(list(
    theoretical = tibble(
      feature=c("haemoglobin","serum k"),
      I=c(hb$theoreticalMI(),k$theoreticalMI()),
      mean=c(hb$theoreticalMean(),k$theoreticalMean()),
      var=c(hb$theoreticalVariance(),k$theoreticalVariance())
    ),
    data = mvd$sample(n),
    plot = mvd$plot,
    sample = mvd$sample
  ))
  
}