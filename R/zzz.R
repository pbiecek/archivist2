.ArchivistEnv <- new.env()

.onAttach <- function(...) {
  packageStartupMessage( "\n Welcome to the archivist2 package (ver 1.0)." )
  assign( x = "sqlite", value = dbDriver( "SQLite" ), envir = .ArchivistEnv )
  assign( x = ".GithubURL", value = "https://raw.githubusercontent.com/", envir = .ArchivistEnv )
  assign( x = ".remoteRepo", value = "http://beta.icm.edu.pl:8080/archivist2_upload.php", envir = .ArchivistEnv )
  assign( x = ".remoteRepoAntiSpam", value = "787878", envir = .ArchivistEnv )
  assign( x = ".currentRepo", value = list(), envir = .ArchivistEnv )
}

.onDetach <- function( libpath ){
  dbUnloadDriver(get( "sqlite", envir = .ArchivistEnv )) 
}

## no S4 methodology here; speedup :
.noGenerics <- TRUE
