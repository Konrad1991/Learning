setwd("/home/konrad/Documents/GitHub/LearningFromBuilding")
roxygen2::roxygenise("RPatternMatching")
install.packages("RPatternMatching", repos = NULL, type = "source")
tinytest::test_package("RPatternMatching")
