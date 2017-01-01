#' @importFrom testthat context test_that expect_equal

context("binding")

test_that("keep_list works correctly",
{
  ls <- list(a=1, b=2)
  expect_equal(keep_list(ls, function (x) x == 1), list(a = 1))
  expect_equal(keep_list(ls, function (x) x == 2), list(b = 2))

})

test_that("`decide_order_to_eval()` works correctly",
  {
    binding <- tail(substitute(c(x = a + b, a = 1, b = 2 * a)), - 1)
    order <- decide_order_to_eval(binding)
    expect_equal(order, c("a", "b", "x"))
  })

test_that("velocity function in example of `let()` works correctly",
{
  velocity <- function (x1, x2, t1, t2) let(
    dx = x2 - x1,
    dt = t2 - t1,
    .in = {
      dx / dt
    })
  expect_equal(velocity(80, 90, 0, 0.5), 20)
})

test_that("velocity function in example of `where()` works correctly",
{
  velocity <- (function (x1, x2, t1, t2) {dx / dt }) %>%
  where(
    dx = x2 - x1,
    dt = t2 - t1)
  expect_equal(velocity(80, 90, 0, 0.5), 20)
})