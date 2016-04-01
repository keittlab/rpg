#' Create an index
#' 
#' Create and index on an existing table column
#' 
#' @param columnname the name of the column
#' @param tablename the name of the table
#' @param indexname optional index name to use
#' @param unique if true, create a unique index
#' @param using the index method
#' @param collate set collation
#' @param descending if true, sort descending
#' @param tablespace create in this tablespace
#' @param where restrict to rows matching predicate
#' @param schemaname specifically in this schema
#' 
#' @details Build an index on a column.
#' 
#' @author Timothy H. Keitt
#' 
#' @export
#' @rdname sql
create_index = function(columnname,
                        tablename,
                        indexname = NULL,
                        unique = FALSE,
                        using = NULL,
                        collate = NULL,
                        descending = FALSE,
                        tablespace = NULL,
                        where = NULL,
                        schemaname = NULL){
  sp = savepoint(); on.exit(rollback(sp))
  tableschema = format_tablename(tablename, schemaname)
  prefix = if (unique) "CREATE UNIQUE INDEX" else "CREATE INDEX"
  if (!is.null(indexname)) prefix = paste(prefix, indexname)
  if (!is.null(using)) tableschema = paste(tableschema, "USING", using)
  sql = paste(prefix, "ON", tableschema, "(", columnname, ")")
  if (!is.null(collate)) sql = paste(sql, "COLLATE", collate)
  if (descending) sql = paste(sql, "DESC")
  if (!is.null(tablespace)) sql = paste(sql, "TABLESPACE", tablespace)
  if (!is.null(where)) sql = paste(sql, "WHERE", where)
  execute(sql); on.exit(commit(sp))}
