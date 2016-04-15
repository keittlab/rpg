pg_config_path = Sys.which("pg_config")
if (nzchar(pg_config_path)){
  command = paste(pg_config_path, "--version")
  version_string = system(command, TRUE)
  major_version = unlist(strsplit(version_string, "[ .]"))[2]
  cat(major_version, "\n")} else cat("0\n")
