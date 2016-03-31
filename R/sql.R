#' Create an index
#' 
#' Create and index on an existing table column
#' 
#' @param tablename the name of the table
#' @param columnname the name of the column
#' @param unique if true, create a unique index
#' @param using the index method
#' @param schemaname specifically in this schema
#' 
#' @details This function is not yet finished
#' 
#' @export
#' @rdname sql
create_index = function(tablename,
                        columnname,
                        unique = FALSE,
                        using = NULL,
                        schemaname = NULL){
  sp = savepoint(); on.exit(rollback(sp))
  tableschema = format_tablename(tablename, schemaname)
  prefix = if (unique) "CREATE UNIQUE INDEX" else "CREATE INDEX"
  if (!is.null(using)) tableschema = paste(tableschema, "USING", using)
  execute(prefix, "ON", tableschema, "(", columnname, ")")
  on.exit(commit(sp))}