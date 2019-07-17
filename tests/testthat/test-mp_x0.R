testthat::context('Test mp_x0 function')


set_test1 <- expand.grid(p_y1 = c(0.65, 0.9),
                       do_tar = c(0.1, 0.3),
                       xs_ass = c('strong', 'weak'))%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar == 0.1, 0.15, 0.4))

set_test_err1 <- expand.grid(p_y1 = c(0.65, 0.9),
                         do_tar = c(0.1, 0.3),
                         xs_ass = c('strong', 'weak'))%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar == 0.1, 1, 1),
                i = seq(1, 8, 1))%>%
  dplyr::filter(i != 4)%>%
  dplyr::select(-i)


set_test2 <- expand.grid(pc = c(0.65, 0.9),
                         m2 = c(0.025, 0.1),
                         do_tar = c(0.1, 0.3),
                         xs_ass = c('strong', 'weak'))%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar == 0.1, 0.15, 0.4),
                p_y1 = pc - m2)%>%
  dplyr::select( -c(pc, m2))

set_test_err2 <- expand.grid(pc = c(0.65, 0.9),
                         do_tar = c(0.1, 0.3),
                         m2 = c(0.025, 0.1),
                         xs_ass = c('strong', 'weak'))%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar == 0.1, 1, 1),
                p_y1 = pc - m2)%>%
  dplyr::select(-c(pc, m2))

obj  <- purrr::pmap_dbl(as.list(set_test1), mp_x0)
obj1 <- purrr::pmap_dbl(as.list(set_test2), mp_x0)

obj_err   <- purrr::pmap(as.list(set_test_err1), mp_x0)
obj_err1  <- purrr::pmap(as.list(set_test_err2), mp_x0)

testthat::describe('basic usage',{

  it('non-negative result',{
    testthat::expect_gte(obj[1], 0)
    testthat::expect_gte(obj[2], 0)
    testthat::expect_gte(obj[3], 0)
    testthat::expect_gte(obj[4], 0)
    testthat::expect_gte(obj[5], 0)
    testthat::expect_gte(obj[6], 0)
    testthat::expect_gte(obj[7], 0)
    testthat::expect_gte(obj[8], 0)
    testthat::expect_gte(obj1[1], 0)
    testthat::expect_gte(obj1[2], 0)
    testthat::expect_gte(obj1[3], 0)
    testthat::expect_gte(obj1[4], 0)
    testthat::expect_gte(obj1[5], 0)
    testthat::expect_gte(obj1[6], 0)
    testthat::expect_gte(obj1[7], 0)
    testthat::expect_gte(obj1[8], 0)

  })

  it('print error',{
    testthat::expect_true(obj_err[[1]] == 'Negative conditional probability')
    testthat::expect_true(obj_err[[2]] == 'Negative conditional probability')
    testthat::expect_true(obj_err[[3]] == 'Negative conditional probability')
    testthat::expect_true(obj_err[[4]] == 'Negative conditional probability')
    testthat::expect_true(obj_err[[5]] == 'Negative conditional probability')
    testthat::expect_true(obj_err[[6]] == 'Negative conditional probability')
    testthat::expect_true(obj_err[[7]] == 'Negative conditional probability')
    testthat::expect_true(obj_err1[[1]] == 'Negative conditional probability')
    testthat::expect_true(obj_err1[[2]] == 'Negative conditional probability')
    testthat::expect_true(obj_err1[[3]] == 'Negative conditional probability')
    testthat::expect_true(obj_err1[[5]] == 'Negative conditional probability')
    testthat::expect_true(obj_err1[[6]] == 'Negative conditional probability')
    testthat::expect_true(obj_err1[[7]] == 'Negative conditional probability')


  })
})

