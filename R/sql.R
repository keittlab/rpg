#' Create an index
#' 
#' Create and index on an existing table column
#' 
#' @param tablename the name of the table
#' @param columname the name of the column
#' @param schemaname specifically in this schema
#' 
#' @details This function is not yet finished
#' 
#' @export
#' @rdname sql
create_index = function(tablename,
                        columnname,
                        unique = FALSE,
                        concurrently = FALSE,
                        if_not_exists = FALSE,
                        using = NULL,
                        schemaname = NULL){
  sp = savepoint(); on.exit(rollback(sp))
  tableschema = format_tablename(tablename, schemaname)
  indexname = paste(tablename, cname, "index", sep = "_")
  prefix = if (unique) "CREATE UNIQUE INDEX" else "CREATE INDEX"
  if (concurrently) prefix = paste(prefix, "CONCURRENTLY")
  if (if_not_exists) prefix = paste(prefix, "IF NOT EXISTS")
  if (!is.null(using)) tableschema = paste(tableschema, "USING", using)
  execute(prefix, indexname, "ON", tableschema, "(", cname, ")")
  on.exit(commit(sp))}