testthat::context('Test mi function')

dat <- tibble::tibble(y = rbinom(100,1,0.6))
dat$y.m <- c(rep(NA, 10), dat$y[11:100])
dat$r <- ifelse(is.na(dat$y.m)==T, 1, 0)
obj1 <- mi(dat, n_mi = 5)
obj2 <- mi(dat, 2, 10, mu_k = 1.2, sd_k = 0.1)


testthat::describe('basic usage',{

  it('class1',{
    testthat::expect_true(inherits(obj1,'tbl'))
  })

  it('class2',{
    testthat::expect_true(inherits(obj2,'tbl'))
  })

  it('dim1',{
    testthat::expect_equal(dim(obj1),c(5,3))
  })

  it('dim2',{
    testthat::expect_equal(dim(obj2),c(2*10,5))
  })

  it('check nobs1',{
    testthat::expect_equal(mean(obj1$n_obs), 100)
  })

  it('check nobs2',{
    testthat::expect_equal(mean(obj2$n_obs), 100)
  })

  it('check diff p',{
    testthat::expect_gt(length(unique(obj2$phat)), 1)
  })

})

