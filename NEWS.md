# dfidx 0.1-3

* `dfidx.print_n` is an option to choose the number of lines to print

* `dfidx.pos_idx` is an option to choose the position of the `idx`
  column

* unit tests added

* `transform`, `subset` methods and `organize` function (<=>
  `dplyr::arrange`)

* dfidx now have a sort (`TRUE` by default) and a drop.unused.levels
  (`TRUE` by default) argument

* `na.rm = TRUE` added in the `mean` method

* the `print` method of `dfidx` is simplified

* fix lg 250  if (is.tibble) data <- class(data) <- "data.frame" 

* the print method for dfidx objects now includes directly the
  printing of the index, without relying to the print.idx method

# dfidx 0.1-2

* **dfidx** is now free of tidyverse's dependencies which are now include
  in the new **tidydfidx** package

# dfidx 0.1-1

* the n argument is now operative for the print method of tbl_df objects

* line 188 miscellaneous.R : setdiff extended to the names of the indexes

* the idx is relocated in the model frame so that the model response
  function works properly
  
* the `position` and `name` arguments enables to indicate the name and
  the position of the idx column

* `mutate` works now on the "unfolded" data frame so that indexes'
  names can be accessed directly

# dfidx 0.1-0

* new named vectors interface for defining the indexes

* enhanced print method for tibbles

* a new quarto vignette

* AER, mlogit and plm are no longer suggested packages

# dfidx 0.0-5

* test of the presence of suggested packages are added in the man
  pages and the vignette

# dfidx 0.0-4

* Liming Wang 26 oct 2020, bug for intercept only models fixed in
  unfold_idx function

* reshape is unsafe with tibbles, coerce to data frame and then back
  to tibble in this case

# dfidx 0.0-1

* initial version of dfidx posted on github

