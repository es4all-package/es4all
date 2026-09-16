.cp2es = function(gt, lt, bool.cont, bool.bin, bool.flip) {
  assert_that( all(gt >= 0), all(lt >= 0)
    , bool.cont %in% c(TRUE, FALSE)
    , bool.bin %in% c(TRUE, FALSE)
    , bool.flip %in% c(TRUE, FALSE))
  sx = gt + lt
  if (bool.cont) {
    # ?all.equal
    assert_that(all( abs(sx - 1) <= 2e-8 ))
  } else {
    assert_that(all(sx >= 0), all(sx <= 1))
  }

  ret = list(flip = FALSE)
  if (bool.flip && getOption("es4all.flip")) {
    if (gt < lt) {
      ret$flip = TRUE
      tmp = gt
      gt = lt
      lt = tmp
    }
  }

  dx = gt - lt
  ret$es = list(
    D = gt
    , d.21 = lt
    , GOR = gt / lt        # binary: OR
    , SD = dx              # binary: RD
  )
  if (bool.cont) {
    ret$es %<>% c(list(
      SMD = qnorm(gt) * sqrt(2)
    ))
  }
  if (bool.bin) {
    xx = rep_len(Inf, length(dx))
    idx = which(dx > 0)
    xx[idx] = 1 / dx[idx]

    ret$es %<>% c(list(
      NNT = xx
    ))
  }
  if (!bool.cont) {
    ret$es %<>% c(list(
      ASD = dx / sx          # SD
      , MSS = 0.5 + 0.5 * dx # D
      , AD = gt / sx         # D
      , d.0 = 1 - sx         # 0
    ))
    ret$es$WO = ret$es$MSS / (1 - ret$es$MSS) # GOR
  }

  ret
}
