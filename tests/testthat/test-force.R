
# Example factory:
fun <- function(x, y, z) {
  force(x)
  function(x, y, z) {
    x * y + z
  }
}


# Wrap the tested functions and capture their output:
force_print_list <- purrr::quietly(force_print)(fun)
force_print_all_list <- purrr::quietly(force_print_all)(fun)


test_that("Arguments are counted correctly", {
  sub("arguments.*", "", force_print_list$output) %>% expect_equal("2 out of 3 ")
  force_print_all_list$output %>% expect_equal("force(x)\nforce(y)\nforce(z)")
})


test_that("There are no results if called correctly", {
  force_print_list$result %>% expect_null()
  force_print_all_list$result %>% expect_null()
})

test_that("There are no warnings if called correctly", {
  force_print_list$warnings %>% expect_length(0)
  force_print_all_list$warnings %>% expect_length(0)
})


test_that("There are no messages if called correctly", {
  force_print_list$messages %>% expect_length(0)
  force_print_all_list$messages %>% expect_length(0)
})


test_that("force_print() errors when not provided a function", {
  force_print() %>% expect_error()
  "some string" %>% force_print() %>% expect_error()
})

test_that("force_print() errors when provided a primitive", {
  c %>% force_print() %>% expect_error()
  `[` %>% force_print() %>% expect_error()
  length %>% force_print() %>% expect_error()
})

test_that("force_print_all() errors when not provided a function", {
  force_print_all() %>% expect_error()
  "some string" %>% force_print_all() %>% expect_error()
})

test_that("force_print_all() errors when provided a primitive", {
  c %>% force_print_all() %>% expect_error()
  `[` %>% force_print_all() %>% expect_error()
  length %>% force_print_all() %>% expect_error()
})


