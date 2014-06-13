ezpg
====

An R package for reading from and writing to a PostgreSQL database

If you are tired of struggling with the database packages in R and use only PostgreSQL,
this is the package for you. No more juggling connection and result objects. Just issue
the query and get the results back as a data frame.

Key features include:
---------------------

1. Simple quick query execution and retrieval
    ````
    library(ezpg)
    res = fetch("select * from mtcars") # query default database
    class(res)                          # data.frame
    ````
Note that any command that requires a connection will attempt to make a connection if one does not exisit; first by resetting the connection, in case its failed, and then attemping
to use the default connection values. In practice this usually mean you just load the
library and start issuing queries. This is intented for interactive use. If you need to
write a database translation engine, there are other packages for that. (Why you would
do this in R is another question.)

2. Simple access to parameterized queries. This means no more escaping query strings!
If you don't know what that means, you will quickly find out if you start using R
database packages.
    ````
    library(ezpg)
    connect("dbname = testing")                            # use any libpq connection string
    query("select * from testtab where a = $1", "yes")     # the value "yes" is substituted for $1
    res = fetch()                                          # returns the query as data.frame
    res = fetch("select * from testtab where a = \'yes\'") # escaped version
    ````

3. Simple access to database cursors. This is the way you were supposed to interact
with your database. Basically a cursor is a prepared query that return a limited number
of rows on demand. This is conveniently wrapped in an R iterator from the iterators
package.
    ````
    library(ezpg)
    library(foreach)
    r3 = cursor("select * from testtab", by = 3)
    x = foreach(i = r3, .combine = rbind) %do%
    {
        colSums(i)   # sum over 3 rows at a time
    }
    ````
Here neither the client nor the server deals with more than three rows of results
at a time. This allows incremental processing of massive tables.

4. Simple access to execution of prepared statements.
    ````
    library(ezpg)
    prepare("insert into mytab (a, b, c) values ($1, $2, $3)")
    params = matrix(rnorm(300), 100)
    execute(params)
    ````
The call to `execute` evalutes the prepared statement for each row of the
supplied parameters. This evaluation loop is in C++.

Installation
------------

You wil need to install [libpq](http://www.postgresql.org/download/) as a requirement.
The configure step will call [pg_config](http://www.postgresql.org/docs/9.1/static/app-pgconfig.html).

If typing

```
system("pg_config --version")
```

in R does not return anything, then this package will likely not install. To install in R, try

```
install.packages(c("devtools", "Rcpp", "roxygen2"))
devtools::install_github("ezpg", "thk686")
```
