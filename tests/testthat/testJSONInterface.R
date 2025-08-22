library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import the JSON Campsis dataset", {
  
  dataset1 <- loadFromJSON(Dataset(), file.path(testFolder, "json_examples", "dataset_example1.json"))
  
  expArm1 <- Arm(subjects=100, label="Arm 1") %>%
    add(Bolus(time=0, amount=50, compartment="ABS", ii=24, addl=6))
  expArm2 <- Arm(subjects=100, label="Arm 2") %>%
    add(Bolus(time=0, amount=100, compartment="ABS", ii=24, addl=6))
  expDataset1 <- Dataset() %>%
    add(expArm1) %>%
    add(expArm2) %>%
    add(Observations(seq(0, 168, by=24)))
  
  expect_equal(dataset1, expDataset1)
})