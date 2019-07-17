testthat::context('Test mi function')

obj <- p_rmle(0.1, 100, 100, 0.65, 0.57)

testthat::describe('basic usage',{

  it('class1',{
    testthat::expect_true(inherits(obj,'numeric'))
  })

  it('dim',{
    testthat::expect_equal(length(obj),1)
  })

})

