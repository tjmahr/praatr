

# How to load this file:
#source( paste(.libPaths(),"PraatR","PraatR.r",sep="/") )


# Welcome message

cat("
######################
# Welcome to PraatR! #
######################

For documentation on how to use PraatR and information on how to cite it,
visit the homepage at http://www.aaronalbin.com/praatr/

PraatR is released under the the GNU General Public License:
http://www.gnu.org/licenses/.

")

# Perhaps only show this message if there were no errors in loading it? What
# functions are out there for me to check this? Perhaps just {...
# stop();stop();stop(); cat(welcome)}?

# Say 'PraatR is loaded and ready to go!' instead?

###########################
# Load supported commands #
###########################

# Since this isn't a system/shell command, .libPaths() should work on all OSes in this context
# SupportedCommands <- read.table(paste(.libPaths(),"PraatR","SupportedCommands.txt",sep="/"), sep="\t", header=TRUE, quote="")
commands <- read.table("../inst/SupportedCommands.txt", sep = "\t", header = TRUE, quote = "")

PraatCommand <- function(command, type) {
  structure(list(Command = command, Type = type),
            class = c("PraatCommand", "list", type))
}


####################
# praat() function #
####################

#' Execute a Praat command
#'
#' @param command A character string indicating what command should be executed.
#'   This will be checked against the 'SupportedCommands' database, and if it's
#'   not found there, an error message will be issued.
#' @param arguments A list() of arguments. This data structure is used because
#'   it is possible to mix of various classes, e.g. list("character", 12.34, 5,
#'   TRUE).
#' @param input The (ideally full) path to the input file.
#' @param output The (ideally full) path to the output file.
#' @param overwrite Only applicable if the user chooses a 'Create' or 'Modify'
#'   command. (If they choose a 'Query' command, this will be ignored.) This
#'   indicates whether the command should be executed if the file specified in
#'   'output' already exists (thereby overwriting it). By defaulting to FALSE,
#'   the user is protected from accidentally erasing data.
#' @param filetype Determines file format for the output text file. Only
#'   applicable if the user chooses a 'Create' or 'Modify' command. Three
#'   choices: "text" = Save as text file..., "short" = Save as short text
#'   file..., and "binary" = Save as binary file...
#' @param simplify Only applicable if the user chooses a 'Query' command.
#'   Determines whether the queried information will be coerced to numeric (i.e.
#'   just the core data) rather than remain as a text string (potentially along
#'   with various kinds of embellishing explanatory information). Either way,
#'   the result will (initially) be brought into R as a character string. The
#'   user specifies this as TRUE or FALSE. This later gets translated into 1 or
#'   0 when passed to Praat.
#' @return (place-holder text)
#' @export
praat <- function(command, arguments = list(), input = NULL, output = NULL,
                  overwrite = NULL, filetype = "text", simplify = FALSE) {

  ## VALIDATE COMMAND

  # First check whether the command is one of the official list of supported
  # ones (Note that, at present, this does not take into account the object
  # type, and only considers the command itself.)
  assert_that(is_valid_command(command))


  ## CLASSIFY SUPPLIED COMMAND

  # Generally speaking, there is a one-to-one mapping between CommandNames and
  # CommandTypes. Within the current set of supported commands, there are three
  # exceptions.

  # (1) The command 'Rotate...' is of type 'Modify' for a Configuration or
  # Polygon object but of type 'Create' for a Permutation object.

  # (2-3) The commands 'Filter (pass Hann band)...' and 'Filter (stop Hann
  # band)...' are of type 'Modify' for a Spectrum object but of type 'Create'
  # for a Sound object.

  # Fortunately, these conflations do not involve queries. Thus, I can safely
  # assume that randomly sampling one of the two in these ambiguous cases will
  # not obscure the [+/-query] distinction. (I checked and everything I say
  # above doesn't change based on the inclusion of 'Play' commands.)

  RowIndex <- which(command == commands$CommandName)[1]
  # For the three commands mentioned above, this will arbitrarily choose
  # whichever comes first in terms of the ordering of the rows in the dataframe.

  # At present, only three types are supported: Create, Modify, and Query.
  CommandType <- as.character(commands[RowIndex, "CommandType"])

  this_command <- PraatCommand(command, CommandType)


  ## INPUT VALIDATION AND ARGUMENT DEFAULTING

  # Make sure that 'arguments' is a list
  assert_that(inherits(arguments, "list"))

  # Ultimately, this will need to be MUCH more rigid - checking the user's
  # specified arguments against those from the SupportedCommands database to make
  # sure everything lines up OK. There *can* be arguments for 'Play' commands, so
  # this is required even there.

  # Make sure the input file actually exists on the user's hard drive.
  assert_that(is.readable(input))

  validate_command(this_command, input = input, output = output,
                   overwrite = overwrite, filetype = filetype,
                   simplify = simplify)

  # - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - =
  # = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

  if (CommandType == "Query") {

    # If something is specified for 'output', issue a warning that the specified
    # output argument has been ignored, but still proceed normally.
    if (!is.na(output)) {
      warning("For Query commands, leave 'output' unspecified; the supplied value has been ignored.")
    } else {
      # if it is indeed missing (as it should be), then fill it with a dummy 'X'
      # just to make sure something gets passed to the Praat form.
      output = "X"
    }

  # Argument [5]
  # Argument 'overwrite' should be missing. If not, issue a warning.
  if(!is.na(overwrite)){warning("For Query commands, leave 'overwrite' unspecified; the supplied value has been ignored.")}
   # No need for dummy variable since not passed to Praat

  # Argument [6]
  # Argument 'filetype' should be missing. If not, issue a warning. If it is, fill it with a dummy 'X' so something can be passed to Praat
  if(!is.na(filetype)){warning("For Query commands, leave 'filetype' unspecified; the supplied value has been ignored.")}else{filetype="X"}

  # Argument [7]
  # Fill in 'simplify' with its default of FALSE if left unspecified
  if(missing(simplify)){simplify=FALSE}

  } # End if this is a Query command

  # - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - =
  # = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

  if(CommandType=="Play"){
  # All four of the remaining commands are moot, so do similar things to above

  # Argument [4]
  if( !missing(output) ){ warning("For Play commands, leave 'output' unspecified; the supplied value has been ignored.")
  }else{ output="X" } # Dummy

  # Argument [5]
  if(!missing(overwrite)){warning("For Play commands, leave 'overwrite' unspecified; the supplied value has been ignored.")} # No need for dummy

  # Argument [6]
  if(!missing(filetype)){warning("For Play commands, leave 'filetype' unspecified; the supplied value has been ignored.")
  }else{filetype="X"} # Dummy

  # Argument [7]
  if(!missing(simplify)){warning("For Play commands, leave 'simplify' unspecified; the supplied value has been ignored.")
  }else{simplify=FALSE} # Dummy

  } # End if this is a Play command

  # - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - =
  # = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

  ############################
  # ADD UNDERBARS TO COMMAND #
  ############################

  # The command cannot have any spaces when passed to shell(), so replace them with double-underscores as a delimiter.
  # These will then be converted back to spaces inside the Praat functions.
  # I can't use a single underscore because of the following five command names:
  # 1) "Get shimmer (local_dB)..."
  # 2) "Get ln(determinant_group)..."
  # 3) "Get ln(determinant_total)"
  # 4) "To HMM_ObservationSequence..."
  # 5) "To HMM_StateSequence"
  UnderbarCommand = gsub(command, pattern=" ", replacement="__")

  #####################
  # PROCESS ARGUMENTS #
  #####################

  if(missing(arguments)){

  TargetScriptName="ZeroArguments.praat"
  ArgumentString = ""

  }else{ # i.e. if there are any arguments

  # Find out how many arguments there are
  nArguments = length( arguments )

  # First coerce any TRUE to "yes" and any FALSE to "no"
  LogicalCoerced = lapply(arguments,FUN=function(x){
  		if(is.logical(x)){
  			return(c("no","yes")[as.integer(x)+1])
  		}else{
  			return(x)
  		} # End if/else
  	} # End function definition
  ) # End call to lapply()

  # Now classify each argument in terms of whether it is a character string or not
  # (At this point, it's assumed that anything that is not a string is numeric - in R terms, either 'integer' or 'numeric' proper.)
  IsString = sapply(LogicalCoerced,FUN=is.character)
  ArgumentClassifications = c("n","s")[as.integer(IsString)+1] # Only where needed

  # Now use these classifications to determine which script to open
  CollapsedClassifications = paste(ArgumentClassifications,collapse="")
  TargetScriptName = paste(CollapsedClassifications,".praat",sep="")

  # Here again, change spaces in the argument names to double-underscores
  # They can't be single underscores again because of argument names like "Kirshenbaum_espeak".
  UnderscoreSwapped = lapply(1:nArguments,FUN=function(n){
  		if(IsString[n]){
  			return(gsub(LogicalCoerced[[n]], pattern=" ", replacement="__"))
  		}else{
  			return(LogicalCoerced[[n]])
  		} # End if/else
  	} # End function definition
  ) # End call to lapply()

  # Now separate the arguments by spaces (so as to fit the syntax of shell() )
  ArgumentString = paste( UnderscoreSwapped, collapse=" ")

  } # End if/else arguments are missing from the function call

  ##############################################
  # Adjust based on user operating system (OS) #
  ##############################################

  UserOS = .Platform$OS.type # "windows" for my Windows 8, "unix" for Wil's Mac OS 10.6 / Lion.

  # In Windows, R.home("library") is something like...
  # "C:/PROGRA~1/R/R-30~1.3/library"
  # ...whereas .libPaths() is...
  # "C:/Program Files/R/R-3.0.3/library"
  # Since the latter has a space (which doesn't work with the current implementation of the call to shell(), use the former.

  # On a Mac, R.home("library") is...
  # "/Library/Frameworks/R.framework/Resources/library"
  # ...whereas .libPaths() is...
  # "/Library/Frameworks/R.framework/Versions/3.0/Resources/library"
  # The former is not specific to a certain R version and merely redirects to the latter, hence use the latter.

  if(UserOS == "windows"){ LibraryPath=R.home("library"); PraatPath = paste(LibraryPath,"PraatR","praatcon.exe",sep="/") }
  if(UserOS == "unix"   ){ LibraryPath=.libPaths();       PraatPath = "/Applications/Praat.app/Contents/MacOS/Praat"} # Presumably it will always be in this one fixed/stable location

  # The documentation for '.Platform' says the following, suggesting it will *always* be one or the other.
  #  - character string, giving the Operating System (family) of the computer. One of "unix" or "windows".
  # In my context, this means that Linux and Mac will both be treated the same. This is a problem given the path specified above, which contains 'MacOS' - which will clearly not work on Linux
  # I'll perhaps need to straighten this out (i.e. disentangle Mac from Linux) by querying other attributes of the OS with some combination of Sys.info() and .Platform.

  # The target script can be easily identified relative to the library path
  ScriptPath = paste(LibraryPath,"PraatR","PraatScripts",TargetScriptName,sep="/")

  ###########################
  # Assemble command string #
  ###########################

  # The checks performed above guarantee that the CommandType is one of the three I'm dealing with, so do simple if() statements to check which is the case, and react accordingly

  # Don't make things if/else (between Create/Modify/Play and Query for right now because I'm not sure whether I'll include other CommandTypes in the future... And this is safer and more transparent anyway.

  # Use shQuote() for all file paths to protect in case they contain spaces
  if(CommandType == "Create" | CommandType == "Modify" | CommandType == "Play"){
  # These three are treated 100% equivalently for the time being, but it's in principle possible to separate them down the road

  CommandString = paste( PraatPath,
                         ScriptPath,
                         CommandType,
                         UnderbarCommand,
                         ArgumentString, # If empty, this will result in two neigboring spaces, but this isn't a problem
                         input,
                         output,
                         filetype,
                         as.integer(simplify), # Always logical until now; converted to integer at the last minute before going to Praat
                         sep=" ")

  intern = FALSE # Do *not* capture anything from the Info Window of Praat (for bringing back into R)

  # Now, finally issue the instruction to the OS
  if(UserOS == "windows"){ shell(cmd=CommandString, intern=intern) } # For Windows
  if(UserOS == "unix"){ system(command=CommandString, intern=intern) } # For Unix/Mac
  # Eventually, somehow detailedly check the response status from this function call and issue custom messages accordingly to help the user troubleshoot if there are any problems?

  } # End 'if this is a query command'

  if(CommandType == "Query"){

  CommandString = paste( PraatPath,
                         "-a", # This switch to the Praat program makes the output go back into R through the 'standard input' (stdin)
                         ScriptPath,
                         CommandType,
                         UnderbarCommand,
                         ArgumentString,
                         input,
                         output,
                         filetype,
                         as.integer(simplify),
                         sep=" ")

  intern = TRUE # This indicates that the ultimate contents of the Info Window in Praat should be captured and brought back into R

  # Now, finally issue the instruction to the OS, and return the result
  if(UserOS == "windows"){ return(shell(cmd=CommandString, intern=intern)) } # For Windows
  if(UserOS == "unix"){ return(system(command=CommandString, intern=intern)) } # For Unix/Mac

  } # End 'if this is a query command'

}
