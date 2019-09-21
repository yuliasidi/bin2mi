testthat::context('Test dt_p2 function')

nobs = 5000

obj  <- dt_p2(nobs, 0.7, 0.65)
objx <- dt_p2(nobs, 0.7, 0.65, add_x = T)
objxc <- dt_p2(nobs, 0.7, 0.65, add_xcont = T)


testthat::describe('basic usage',{

  it('class',{
    testthat::expect_true(inherits(obj,'tbl'))
  })

  it('classx',{
    testthat::expect_true(inherits(objx,'tbl'))
  })

  it('classxc',{
    testthat::expect_true(inherits(objxc,'tbl'))
  })

  it('dim',{
    testthat::expect_equal(dim(obj),c(2*nobs,2))
  })

  it('dimx',{
    testthat::expect_equal(dim(objx),c(4*nobs,4))
  })

  it('dimxc',{
    testthat::expect_equal(dim(objxc),c(2*nobs,3))
  })

})


testthat::describe('conditional probs',{

  it('px strong y=0 in c',{
    testthat::expect_equal(mean(objx$x[objx$trt=='c' & objx$x_desc=='strong' & objx$y==0]), 0.6, tolerance=0.04)
  })

  it('px strong y=1 in c',{
    testthat::expect_equal(mean(objx$x[objx$trt=='c' & objx$x_desc=='strong' & objx$y==1]), 0.2, tolerance=0.04)
  })

  it('px weak in c',{
    testthat::expect_equal(mean(objx$x[objx$trt=='c' & objx$x_desc=='weak']), 0.6, tolerance=0.04)
  })

  it('px strong y=0 in t',{
    testthat::expect_equal(mean(objx$x[objx$trt=='t' & objx$x_desc=='strong' & objx$y==0]), 0.6, tolerance=0.04)
  })

  it('px strong y=1 in t',{
    testthat::expect_equal(mean(objx$x[objx$trt=='t' & objx$x_desc=='strong' & objx$y==1]), 0.2, tolerance=0.04)
  })

  it('px weak in t',{
    testthat::expect_equal(mean(objx$x[objx$trt=='t' & objx$x_desc=='weak']), 0.6, tolerance=0.04)
  })

  })


testthat::describe('y probs',{

  it('y in c in obj',{
    testthat::expect_equal(mean(obj$y[obj$trt=='c']), 0.7, tolerance=0.04)
  })

  it('y in t in obj',{
    testthat::expect_equal(mean(obj$y[obj$trt=='t']), 0.65, tolerance=0.04)
  })


  it('y in c in objx',{
    testthat::expect_equal(mean(objx$y[objx$trt=='c']), 0.7, tolerance=0.04)
  })

  it('y in t in objx',{
    testthat::expect_equal(mean(objx$y[objx$trt=='t']), 0.65, tolerance=0.04)
  })

  it('cor(y,x) in objxc',{
    testthat::expect_gt(cor(objxc$y,
                            objxc$x), 0.2)
  })
})
