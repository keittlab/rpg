ezpg
====

An R package for reading from and writing to a PostgreSQL database

If you are tired of struggling with the database packages in R and use only PostgreSQL,
this is the package for you. No more juggling connection and result objects. Just issue
the query and get the results back as a data frame.

I am working on some advanced features like binary cursors.

To install, paste into R:
install.packages("devtools", "Rcpp", "roxygen2")
devtools::github_install("ezpg", "thk686")
