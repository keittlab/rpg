ezpg
====

An R package for reading from and writing to a PostgreSQL database

If you are tired of struggling with the database packages in R and use only PostgreSQL,
this is the package for you. No more juggling connection and result objects. Just issue
the query and get the results back as a data frame.

I am working on some advanced features like binary cursors.

# Installation

You wil need to install [libpq](http://www.postgresql.org/download/) as a requirement.
The configure step will call [pg_config](http://www.postgresql.org/docs/9.1/static/app-pgconfig.html).

If typing

```
system("pg_config --version")
```

in R does not return anything, then this package will likely not install. To install in R, try

```
invisible(lapply(c("devtools", "Rcpp", "roxygen2"), function(x) require(x) || install.packages(x)))
devtools::install_github("ezpg", "thk686")
```

