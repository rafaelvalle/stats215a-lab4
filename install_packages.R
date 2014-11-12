loadPackages <- function(pkg){
"Install not installed packages and loads all required packages
    ARGS
      pkg : list with packages names in string <list string>
"
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {
        install.packages(new.pkg, dependencies = TRUE)
    }
    result = sapply(pkg, require, character.only = TRUE)

    if(!all(result)) {
      stop('Not all packages were installed')
    }
}
