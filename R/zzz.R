# .onLoad <- function(libname, pkgname)
# {
#   library.dynam("mclust", pkgname, libname)
# }

mclustStartupMessage <- function()
{
  # Startup message obtained as
  # > figlet -f slant MCLUST
  #   msg <- c(paste0(
  # "    __  ___________    __  _____________
  #    /  |/  / ____/ /   / / / / ___/_  __/
  #   / /|_/ / /   / /   / / / /\\__ \\ / /
  #  / /  / / /___/ /___/ /_/ /___/ // /
  # /_/  /_/\\____/_____/\\____//____//_/    version ",
  #
  # Startup message obtained as
  # > figlet -f slant mclust
  msg <- c(paste0(
     "          _______   ______________  ______
        / ____/ | / /  _/ ____/  |/  /   |
       / __/ /  |/ // // / __/ /|_/ / /| |
      / /___/ /|  // // /_/ / /  / / ___ |
     /_____/_/ |_/___/\\____/_/  /_/_/  |_|   version ",
    packageVersion("ENIGMA")),
    "\nENIGMA is built for fully deconvolution.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .ENIGMA variable allowing its modification
  #unlockBinding(".enigma", asNamespace("ENIGMA"))
  # startup message
  msg <- mclustStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'ENIGMA' version", packageVersion("ENIGMA"))
  packageStartupMessage(msg)
  invisible()
}
