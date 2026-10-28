# Tests for read.ensemble.output

# Stands in for PEcAn.utils::read.output. `values` maps run id to the output
# of that member; a run id that is absent has no output files, for which
# read.output returns an unnamed list holding NA.
fake_read_output <- function(values) {
  function(runid, outdir, start.year, end.year, variables, ...) {
    if (runid %in% names(values)) {
      values[[runid]]
    } else {
      lapply(variables, function(x) NA)
    }
  }
}

read_members <- function(values, expression = "NPP", variables = "NPP") {
  read.ensemble.output <- read.ensemble.output
  mockery::stub(
    read.ensemble.output,
    "PEcAn.utils::read.output",
    fake_read_output(values)
  )
  read.ensemble.output(
    ensemble.size = 3,
    pecandir = "unused",
    outdir = "unused",
    start.year = 2004,
    end.year = 2004,
    variable = list(variables = variables, expression = expression),
    ens.run.ids = data.frame(id = c("run1", "run2", "run3"))
  )
}

test_that("read.ensemble.output returns one value per complete member", {
  res <- read_members(list(
    run1 = list(NPP = c(1, 3)),
    run2 = list(NPP = c(5, 7)),
    run3 = list(NPP = 10)
  ))
  expect_equal(unlist(res), c("1" = 2, "2" = 6, "3" = 10))
})

test_that("read.ensemble.output evaluates the expression for each member", {
  res <- read_members(
    list(run1 = list(A = 2, B = 1), run2 = list(A = 4, B = 3)),
    expression = "A + B",
    variables = c("A", "B")
  )
  expect_equal(res[["1"]], 3)
  expect_equal(res[["2"]], 7)
})

test_that("a member with no output is NA, not the previous member's result", {
  log <- capture.output(
    res <- read_members(list(run1 = list(NPP = 5), run3 = list(NPP = 9))),
    type = "message"
  )
  expect_match(log, "run2", all = FALSE)
  expect_equal(res[["1"]], 5)
  expect_true(is.na(res[["2"]]))
  expect_equal(res[["3"]], 9)

  # the missing member must not follow member 1
  log <- capture.output(
    res_changed <- read_members(list(run1 = list(NPP = 99), run3 = list(NPP = 9))),
    type = "message"
  )
  expect_match(log, "run2", all = FALSE)
  expect_true(is.na(res_changed[["2"]]))
})

test_that("a member missing one of several variables is NA", {
  log <- capture.output(
    res <- read_members(
      list(
        run1 = list(A = 2, B = 1),
        run2 = list(A = 4),
        run3 = list(A = 6, B = 5)
      ),
      expression = "A + B",
      variables = c("A", "B")
    ),
    type = "message"
  )
  expect_match(log, "run2.*B", all = FALSE)
  expect_equal(res[["1"]], 3)
  expect_true(is.na(res[["2"]]))
  expect_equal(res[["3"]], 11)
})
