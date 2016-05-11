pg_config_path = Sys.which("pg_config")
if (nzchar(pg_config_path))
{
  version_string = system2(pg_config_path, "--version", stdout = TRUE, stderr = FALSE)
  major_version = unlist(strsplit(version_string, "[ .]"))[2]
  cat(major_version, "\n")
} else cat("0\n")
