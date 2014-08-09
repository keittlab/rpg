rpg
====

An R package for reading from and writing to a PostgreSQL database

If you are tired of struggling with the database packages in R and use only PostgreSQL,
this is the package for you. No more juggling connection and result objects. Just issue
the query and get the results back as a data frame.

Key features include:
---------------------

1. Simple quick query execution and retrieval
    ````
    library(rpg)
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
    library(rpg)
    connect("dbname = testing")                            # use any libpq connection string
    query("select * from testtab where a = $1", "yes")     # the value "yes" is substituted for $1
    res = fetch()                                          # returns the query as data.frame
    res = fetch("select * from testtab where a = \'yes\'") # escaped version
    ````

3. Simple access to database cursors. This is the way you were supposed to interact
with your database. Basically a cursor is a prepared query that returns a limited number
of rows on demand. This is conveniently wrapped in an R iterator from the iterators
package.
    ````
    library(rpg)
    library(foreach)
    r3 = cursor("select * from testtab", by = 3)
    x = foreach(i = r3, .combine = rbind) %do%
    {
        colSums(i)   # sum over 3 rows at a time
    }
    ````
Here neither the client nor the server deals with more than three rows of results
at a time. This allows incremental processing of massive tables. Note that you **can**
do parallel computing with `cursor`. See `help("cursor")` for an example.

4. Simple access to execution of prepared statements.
    ````
    library(rpg)
    query("begin transaction")
    prepare("insert into mytab (a, b, c) values ($1, $2, $3)")
    params = matrix(rnorm(300), 100)
    execute_prepared(params)
    query("commit transaction")
    ````
The call to `execute_prepared` evalutes the prepared statement for each row of the
supplied parameters. This evaluation loop is in C++.

5. Maintain multiple live connections without holding external pointers, creating
finalizers, etc.
    ````
    library(rpg)
    connect("dbname = db1"); push_conn()
    connect("dbname = db2"); push_conn()
    show_conn_stack(); rotate_conn_stack()
    swap_conn(); pop_conn(); pop_conn()
    ````
This means there are no connection objects to save and reload as invalid null pointers. All state lives only for the current session.

6. Simple access to the asynchronous query interface of `libpq`.
    ````
    library(rpg)
    connect()
    # eg some big join
    async_query("select bigtab1.id from bigtab1, bigtab2")
    Sys.sleep(5)
    res = switch(async_status(),
                 BUSY = { cancel(); NULL },
                 PGRES_TUPLES_OK = fetch(),
                 NULL)
    finish_async()
    ````
The `async_status` call will return `BUSY` condition if the server is still working.
A normal status string is returned if results are ready. `DONE` is returned if there
is nothing more to fetch. You can attempt to cancel a command in progress using `cancel`.

Installation
------------

The [libpq](http://www.postgresql.org/download/) files are now included with the source and used if not found on the system. The developement version of rpg is hosted on [github](https://github.com/thk686/rpg). To install from github try

```
install.packages(c("devtools", "Rcpp", "roxygen2"))  # can skip if installed
devtools::install_github("rpg", "thk686")
```

The github repo is manually copied as needed to an [r-forge](https://r-forge.r-project.org/projects/rpg/) repository. Provided the build system on r-forge works, you can install using

```
install.packages("rpg", repos="http://R-Forge.R-project.org")
```

The lastest released version is on [CRAN](http://cran.r-project.org/web/packages/rpg/index.html) so

```
install.packages("rpg")
```

should just work.

