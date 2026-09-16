.check_args = function(x, meta, one = FALSE, paired = FALSE, value = FALSE) {
  attr.meta = attr(x, "meta", exact = TRUE)

  data_type = if (value) {
    assert_that(length(x) == 1, noNA(x), is.numeric(x))
    "value"
  } else if (paired) {
    assert_that(is.data.frame(x), nrow(x) >= 1)
    if (nrow(x) == 1) {
      if (!("group" %in% names(x))) {
        x$group = paste0("X", 1:nrow(x))
      }
      assert_that("group" %in% names(x), ncol(x) > 1, noNA(x))
      "summary"
    } else {
      assert_that(ncol(x) == 2L, noNA(x))
      "micro"
    }
  } else if (is.list(x) && !is.data.frame(x)) {
    assert_that(length(x) == 2L, !is.null(names(x)), all(names(x) %>% nzchar))
    for (ii in 1:length(x)) {
      assert_that(!is.null(x[[ii]]), length(x[[ii]]) > 0, noNA(x[[ii]])
                  , is.logical(x[[ii]]) || is.numeric(x[[ii]])
                  , is.null(names(x[[ii]])))
    }
    if (one) {
      assert_that(xor(length(x[[1]]) == 1L, length(x[[2]]) == 1L))
    }
    "micro"
  } else if (is.data.frame(x)) {
    if (!("group" %in% names(x))) {
      x$group = paste0("X", 1:nrow(x))
    }
    assert_that(nrow(x) == 2L, "group" %in% names(x), ncol(x) > 1)
    if (!one) {
      assert_that(noNA(x))
    } else {
      nona_ = c(!any(is.na(x[1,])), !any(is.na(x[2,])))
      assert_that(xor(nona_[1], nona_[2]))
      idx.nona = which(names(x) %in% c("group", "mu", "xbar"))
      idx.na = which( !(names(x) %in% c("group", "mu", "xbar")))
      idx = which(!nona_)
      assert_that(noNA(x[idx,idx.nona]), all(is.na(x[idx,idx.na])))
    }
    "summary"
  } else {
    stop("`x` must be a value, a list, or a data.frame.")
  }

  meta = c(attr.meta, meta)
  if (!is.null(meta)) {
    assert_that(is.list(meta), !is.null(names(meta)), all(names(meta) %>% nzchar))
  }
  list(x = x, meta = meta, data_type = data_type)
}
