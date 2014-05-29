is_valid_command <- function(command) is.element(command, commands$CommandName)

on_failure(is_valid_command) <- function(call, env) {
  sprintf("The command %s is not supported by the present version of PraatR.
            Double-check that this is indeed a real Praat command, and then
            feel free to contact the creator of PraatR.",
          deparse(eval(call$command)))
}

is_legal_filetype <- function(x) is.element(x, c("text", "short", "binary"))

on_failure(is_legal_filetype) <- function(call, env) {
  sprintf("Illegal filetype %s: 'filetype' argument must be \"text\", \"short\", or \"binary\".",
          deparse(eval(call$x)))
}



stop_overwrite <- function(path) {
  stop(paste("This 'output' file already exists: (Consider setting overwrite = TRUE)",
             path, sep = "\n       "))
}






validate_command <- function(...) UseMethod("validate_command")

validate_command.Create <- function(command, output, overwrite, filetype, simplify, ...) {

  if (simplify) {
    warning("For Create/Modify commands, leave 'simplify' unspecified; the supplied value has been ignored.")
  }

  assert_that(is_legal_filetype(filetype))

  # If the output is left unspecified, then issue an error and stop computation.
  if (is.null(output)) {
    stop("For this command, you must specify a file path for the 'output' argument.")
  }

  # The user can name the output file itself whatever they want. However, I
  # need to make sure the folder where they indicated that it should be placed
  # actually exists (beyond the mere checking whether it equals "R" just
  # performed).
  if (!file.exists(dirname(output))) {
    stop(paste("The path in the 'output' argument includes a folder that does not exist:",
               dirname(output), sep = "\n       "))
  }
  # file.exists() works fine with directories. However, on Windows it must NOT
  # end in a slash. dirname() does things correctly, so I'm fine.

  # If the 'overwrite' argument is set to FALSE, then if the output file
  # already exists, cease computation
  if (overwrite == FALSE & file.exists(output)) {
    stop_overwrite(output)
  }

  TRUE
}


validate_command.Modify <- function(command, input, output, overwrite, filetype, simplify) {

  if (simplify) {
    warning("For Create/Modify commands, leave 'simplify' unspecified; the supplied value has been ignored.")
  }

  assert_that(is_legal_filetype(filetype))

  # The interplay between 'overwrite' and 'output' is very complex...

  # Assume that the user intends the output file to be the same as the input file
  if (is.null(output) & !is.null(input)) {
    message("Assuming that output file is same as input file %s", input)
    output <- input
  }

  # i.e. in the general case where the 'overwrite' argument is left unspecified
  if (is.null(overwrite)) {
    #... either by manual specification or the assumption from above
    if (output == input) {
      overwrite <- TRUE # Note that this is different from a 'Create' command
    } else { # i.e. if the output file is manually specified to be something other than the input file
      # ???
      if (file.exists(output)) stop_overwrite(output)
    }
  } else { # i.e. if the user explicitly mentions 'overwrite' and assign some value to it in their function call

    # If overwrite is TRUE, then write to the output file regardless of whether
    # it is the same as input or not. So no need to even check that. Rather,
    # have an if statement to check if the user manually specified
    # 'overwrite=FALSE' (for some reason)

    if (overwrite == FALSE) { # i.e. if the user explicitly says 'overwrite=FALSE' in their function call
      if (output == input) { # i.e. if they are one-and-the-same
        stop("Either change 'overwrite' to TRUE or specify an output file different from the input.")
      } else { # i.e. if the output and input are different
        if (file.exists(output)) stop_overwrite(output)
      }
    }
  }

  TRUE
}
