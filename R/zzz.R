.ArchivistEnv <- new.env()

.onAttach <- function(...) {
  packageStartupMessage( "\n Welcome to the archivist2 package (ver 1.0)." )
  assign( x = "sqlite", value = dbDriver( "SQLite" ), envir = .ArchivistEnv )
  assign( x = ".GithubURL", value = "https://raw.githubusercontent.com/", envir = .ArchivistEnv )
  assign( x = ".currentRepo", value = "http://beta.icm.edu.pl:8080/", envir = .ArchivistEnv )

  assign( x = ".remoteRepoAntiSpam", value = "787878", envir = .ArchivistEnv )
  assign( x = ".remoteConfirmation", value = "File uploaded. Database updated. ", envir = .ArchivistEnv )
  assign( x = ".remoteUploadScript", value = "archivist2_upload.php", envir = .ArchivistEnv )
  assign( x = ".remoteQueryScript", value = "archivist2_query.php", envir = .ArchivistEnv )
}

.onDetach <- function( libpath ){
  dbUnloadDriver(get( "sqlite", envir = .ArchivistEnv )) 
}

## no S4 methodology here; speedup :
.noGenerics <- TRUE

.onLoad <- function(libname, pkgname) {
   vig_list = tools::vignetteEngine(package = 'knitr')
   vweave <- vig_list[['knitr::knitr']][c('weave')][[1]]
   vtangle <- vig_list[['knitr::knitr']][c('tangle')][[1]]
   tools::vignetteEngine(pkgname, weave = vweave, tangle = vtangle,
                         pattern = "[.]Rmd$", package = pkgname)
   #register_vignette_engines(pkgname)
}
