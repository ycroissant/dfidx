# dfidx 0.1-1

* the n argument is now opperative for the print method of tbl_df objects

* line 188 miscellaneous.R : setdiff extended to the names of the indexes

* the idx is relocated in the model frame so that the model response
  function works properly

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

