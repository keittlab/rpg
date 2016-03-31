# #' @export
create_index = function(tablename, columnname, schemaname = NULL){
  sp = savepoint(); on.exit(rollback(sp))
  tableschema = format_tablename(tablename, schemaname)
  indexname = paste(tablename, cname, "index", sep = "_")
  execute("create index", indexname, "on", tableschema, "(", cname, ")")
  on.exit(commit(sp))}