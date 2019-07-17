testthat::context('Test mi function')

dt <- tibble::tibble(y = rbinom(100,1,0.6))
dt$ym <- c(rep(NA, 10), dt$y[11:100])
obj1 <- mi(dt, 5, ym = 'ym')
obj2 <- mi(dt, 2, 10, mu_k = 1.3, sd_k = 0.1, ym = 'ym')


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

