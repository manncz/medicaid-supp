Rver <- strsplit(R.version$minor, ".", fixed=T)[[1]][1]
Rver  <- paste0(R.version$major, ".", Rver)
pkgdir  <- file.path("..", "lib", "local", R.version$platform,Rver )
if (!file.exists(pkgdir))
{
    cat(paste0("Please re-start R after creating directory: \n   ",
               pkgdir, "\n",
               "R will now quit. To circumvent this, start R from\n ",
               "a different directory, or delete `.Rprofile` in this\n ",
               "directory.  To restore it later, at the command line\n",
               " enter: `$ git checkout -- .Rprofile` .\n")
        )
    q('no')
} else cat(paste0("Loading\n", file.path(getwd(), ".Rprofile"), "\n"))

.libPaths(c( .libPaths() ,pkgdir)
          )
rm(Rver, pkgdir)
