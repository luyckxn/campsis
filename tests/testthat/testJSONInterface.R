library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import the JSON Campsis dataset", {
  
  dataset1a <- loadFromJSON(Dataset(), file.path(testFolder, "json_examples", "dataset_example1a.json"))
  
  expArm1 <- Arm(subjects=100, label="Arm 1") %>%
    add(Bolus(time=0, amount=50, compartment="ABS", ii=24, addl=6)) %>%
    add(Infusion(time=0, amount=50, compartment="CENTRAL", ii=24, addl=6, duration=2))
  expArm2 <- Arm(subjects=100, label="Arm 2") %>%
    add(Bolus(time=0, amount=100, compartment="ABS", ii=24, addl=6))
  expDataset1a <- Dataset() %>%
    add(expArm1) %>%
    add(expArm2) %>%
    add(Observations(seq(0, 168, by=24)))
  
  expect_equal(dataset1a, expDataset1a)
  
  dataset1b <- loadFromJSON(Dataset(), file.path(testFolder, "json_examples", "dataset_example1b.json"))
  expDataset1b <- Dataset() %>%
    add(expArm1) %>%
    add(expArm2) %>%
    add(Observations(TimeSequence(0, 168, by=1)))
  
  expect_equal(dataset1b, expDataset1b)
})