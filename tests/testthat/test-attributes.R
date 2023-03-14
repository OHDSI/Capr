test_that("first occurrence works", {
  tt <- firstOccurrence()
  expect_equal(tt@name, "First")
})

test_that("op integer methods build", {
  #
  t1 <- lt(65L)
  expect_s4_class(t1, "opAttributeInteger")
  expect_equal(t1@op, "lt")
  expect_equal(t1@value, 65L)

  t2 <- lte(65L)
  expect_s4_class(t2, "opAttributeInteger")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, 65L)

  t3 <- gt(18L)
  expect_s4_class(t3, "opAttributeInteger")
  expect_equal(t3@op, "gt")
  expect_equal(t3@value, 18L)

  t4 <- gte(18L)
  expect_s4_class(t4, "opAttributeInteger")
  expect_equal(t4@op, "gte")
  expect_equal(t4@value, 18L)

  t5 <- eq(18L)
  expect_s4_class(t5, "opAttributeInteger")
  expect_equal(t5@op, "eq")
  expect_equal(t5@value, 18L)

  t6 <- bt(18L, 65L)
  expect_s4_class(t6, "opAttributeInteger")
  expect_equal(t6@op, "bt")
  expect_equal(t6@value, 18L)
  expect_equal(t6@extent, 65L)

  t7 <- nbt(18L, 65L)
  expect_s4_class(t7, "opAttributeInteger")
  expect_equal(t7@op, "!bt")
  expect_equal(t7@value, 18L)
  expect_equal(t7@extent, 65L)

})
