# rpg
Timothy H. Keitt  
04/15/2016  

[![CRAN Version](http://www.r-pkg.org/badges/version/rpg)](http://www.r-pkg.org/badges/version/rpg) [![CRAN Downloads](http://cranlogs.r-pkg.org/badges/rpg)](http://cran.rstudio.com/web/packages/rpg/index.html) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thk686/rpg?branch=master&svg=true)](https://ci.appveyor.com/project/thk686/rpg) [![Travis-CI Build Status](https://travis-ci.org/thk686/rpg.svg?branch=master)](https://travis-ci.org/thk686/rpg) 

This package wraps PostgreSQL's libpq, a library for interacting with a PostreSQL database. Unlike other database access packages for R, ```rpg``` is designed to be specific to PostgreSQL and, as such, exposes a more of the functionality of libpq. A great deal of thought went into making ```rpg``` simple to use. The major difference between ```rpg``` and most other database packages is that ```rpg``` does not use an object-oriented model. There are no connection objects, result objects and so on. This simplifies the interface and makes using ```rpg``` a lot like using psql, PostgreSQL's command line interface. I bascially wrote ```rpg``` as a nice comfy environment for my own work. If you are building infrastructure, you probably want to investigate ```DBI``` and [RPostgres](https://github.com/rstats-db/RPostgres).

### Installation

```
install.packages(rpg)                      # Released
devtools::install_github("thk686/rpg")     # Development
```

### Queries

```rpg``` supports parameterized queries and prepared statements.


```r
library(rpg)
createdb("exampledb"); connect("exampledb")
```

```
## CREATE DATABASE
```

```r
data(mtcars); write_table(mtcars)
```

```
## Initiating new transaction block... done.
```

```
## CREATE TABLE
```

```r
fetch("select * from mtcars where mpg > $1", 30)
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1 32.4   4 78.7  66 4.08 2.200 19.47  1  1    4    1
## 2 30.4   4 75.7  52 4.93 1.615 18.52  1  1    4    2
## 3 33.9   4 71.1  65 4.22 1.835 19.90  1  1    4    1
## 4 30.4   4 95.1 113 3.77 1.513 16.90  1  1    5    2
```

```r
prepared = prepare("select * from mtcars where mpg > $1")
prepared(25); fetch(); prepared(30); fetch()
```

```
## SELECT 6
```

```
##    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 1 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 2 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
## 3 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## 4 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
## 5 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
## 6 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
```

```
## SELECT 4
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1 32.4   4 78.7  66 4.08 2.200 19.47  1  1    4    1
## 2 30.4   4 75.7  52 4.93 1.615 18.52  1  1    4    2
## 3 33.9   4 71.1  65 4.22 1.835 19.90  1  1    4    1
## 4 30.4   4 95.1 113 3.77 1.513 16.90  1  1    5    2
```

### Connection stack

Even though the database connection is implicit, you are not restricted to a single connection. Connections can be managed on a stack.


```r
push_conn()
createdb("exampledb2"); connect("exampledb2")
```

```
## No connection... attempting reset... nope... trying default... that worked.
```

```
## CREATE DATABASE
```

```r
data(iris); write_table(iris); list_tables()
```

```
## Initiating new transaction block... done.
```

```
## CREATE TABLE
```

```
## [1] "iris"
```

```r
swap_conn(); list_tables()
```

```
## [1] "mtcars"
```

```r
show_conn_stack()
```

```
##   host     dbname status.ok
## 1 <NA> exampledb2      TRUE
```

```r
swap_conn(); describe_table("iris")
```

```
##   schema table       column position             type default
## 1 public  iris Sepal.Length        1 double precision    <NA>
## 2 public  iris  Sepal.Width        2 double precision    <NA>
## 3 public  iris Petal.Length        3 double precision    <NA>
## 4 public  iris  Petal.Width        4 double precision    <NA>
## 5 public  iris      Species        5             text    <NA>
```

```r
pop_conn(); get_conn_info("dbname")
```

```
## [1] "exampledb"
```

```r
dropdb("exampledb2")
```

```
## DROP DATABASE
```

### Database cursors and foreach

The ```rpg``` package integrates nicely with the iterators package. This allows you to use ```foreach``` to iterate over query results. This is perhaps the most unique feature of ```rpg```.


```r
library(foreach)
c1 = cursor("SELECT * FROM mtcars", by = 8)
```

```
## Initiating new transaction block... done.
```

```r
x = foreach(i = c1, .combine = rbind) %do% { i$mpg }
print(x, digits = 2)
```

```
##          [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## result.1   21   21   23   21   19   18   14   24
## result.2   23   19   18   16   17   15   10   10
## result.3   15   32   30   34   22   16   15   13
## result.4   19   27   26   30   16   20   15   21
```

This usage is compatible with ```%dopar%```; you can access these cursors from multiple threads of execution and even across a heterogeneous cluster of machines. See the documentation for ```cursor``` for an example.

### Asynchronous queries

You can submit asynchronous queries with ```rpg```. When combined with the connection stack, this allows you to launch queries, push the connection, make a new connection while the first query is completing. By swapping the first connection back to the active state, you can then check for completion or try to cancel the transaction.


```r
async_query("SELECT a.* FROM mtcars a, mtcars b, mtcars c")  # >30,000 rows
```

```
## [1] TRUE
```

```r
count = 0
repeat {
  status = async_status()
  if (status == "BUSY") {
    if (count > 2) {
      cat("calling cancel...\n")
      cancel() }}
  else break
  cat("busy... \n")
  Sys.sleep(1)
  count = count + 1 }
```

```
## busy... 
## busy... 
## busy... 
## calling cancel...
## busy... 
## calling cancel...
## busy... 
## calling cancel...
## busy...
```

```r
print(status)
```

```
## ERROR:  canceling statement due to user request
## 
```

```r
finish_async()
```

### Transactions and save points

A PostgreSQL-specific feature are save points for nested transactions. ```rpg``` provides a convenient wrapper.


```r
sp1 = savepoint(); write_table(iris)
```

```
## Error in current transaction: rolling back... done.
## Initiating new transaction block... done.
```

```
## CREATE TABLE
```

```r
sp2 = savepoint(); data(Loblolly); write_table(Loblolly); list_tables()
```

```
## CREATE TABLE
```

```
## [1] "Loblolly" "iris"     "mtcars"
```

```r
rollback(sp2); list_tables()
```

```
## ROLLBACK
```

```
## [1] "iris"   "mtcars"
```

```r
rollback(sp1); list_tables()
```

```
## ROLLBACK
```

```
## [1] "mtcars"
```

### Object serialization and storage

The ```stow``` and ```stow.image``` functions write entire R objects into a database. You can stow your entire working environment and reload it anywhere else you can reconnect to the database. (A facility listing and commenting images is in the works.)


```r
stow("mtcars")
```

```
## Initiating new transaction block... done.
```

```r
rm(list = objects())
ls()
```

```
## character(0)
```

```r
list_stowed()
```

```
##    objname                  stamp
## 1  mtcars  2017-02-07 16:23:59-06
```

```r
retrieve("mtcars")
ls()
```

```
## [1] "mtcars"
```

```r
disconnect()
dropdb("exampledb")
```

```
## No connection... attempting reset... nope... trying default... that worked.
```

```
## DROP DATABASE
```

### Additional features

1. Intelligent handling of passwords: ```rpg``` will query for the password only if it is needed. You can set a default password.
1. Easy creation of connection aliases: call ```make_service``` and your connection settings will be saved as a named service. You only have to type complex connection options once. If you used a password, that will be saved as well.
1. Lots of options for getting and setting connection options and defaults.
1. Easy tracing of data flowing between client and server.
1. High bandwidth options for reading/writing bulk data from/to the database.

### News

2/7/17 -- Gave up trying to build libpq on the fly and instead borrowed the excellent work done by the [RPostgres](https://github.com/rstats-db/RPostgres) team. RPostgres is looking very promissing for DBI work.

4/15/16 -- I ripped out and replaced the build setup. I was using the autoconf bits and libpq files distributed with RPostgreSQL (a very good package you should check out). However it had a few peculiarities, like never using the included libpq on Linux. Also, I could not check the libpq version number. So now the package will check your postgres install using the pg_config command and if its not found or the version is not new enough, then libpq is downloaded and built.

