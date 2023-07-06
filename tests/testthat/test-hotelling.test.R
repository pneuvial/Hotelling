test_that("permutation p-values with random permutations", {
  data(container.df)
  subs.df <- container.df[1:10,]
  subs.df$gp <- rep(1:2, c(5,5))
  
  fitPerm <- hotelling.test(Al+Fe~gp, data  = subs.df, perm =  TRUE, B = 100)
  stat_perm <- fitPerm$results
  stat <- fitPerm$stats$statistic
  
  perm_p <- (1 + sum(stat_perm >= stat)) / (1 + length(stat_perm))
  expect_equal(fitPerm$pval, perm_p)
})

test_that("permutation p-values with all permutations", {
  data(container.df)
  subs.df <- container.df[1:10,]
  subs.df$gp <- rep(1:2, c(5,5))
  
  fitPerm <- hotelling.test(Al+Fe~gp, data  = subs.df, perm =  TRUE, B = 1000)
  stat_perm <- fitPerm$results
  stat <- fitPerm$stats$statistic
  
  perm_p <- sum(stat_perm >= stat) / length(stat_perm)
  expect_equal(fitPerm$pval, perm_p)
})
