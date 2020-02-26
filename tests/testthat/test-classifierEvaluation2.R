context("Evaluate classifier")
message("Tests started")
library(tidyinfostats)

describe("for the test classifier", {

  cr = ClassifierResult$fromPredictions(inr.predictions,inr.obs)

  expect_iris_levels = function(x) {
    expect_equal(sort(x),sort(as.character(levels(iris$Species))))
  }



  describe("for the binary classification",{

    cr$getBinaryClassifiers()$setosa

    it("plots a roc curve",{
      p <- cr$getBinaryClassifiers()$virginica$plotRoc()
      expect_true(is.ggplot(p))
      expect_known_output(p,"binClass.Rdata")
    })

    it("calculates an AUC",{skip("not implemented")})
    it("calculates a bootstrapped AUC confidence interval",{skip("not implemented")})
    it("calculates a mutual information",{skip("not implemented")})

  })



})
