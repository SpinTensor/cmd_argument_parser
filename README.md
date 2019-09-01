# cmd_argument_parser

DESCRIPTION:
   abstracts the usage of command line parameters
   every option supports arbritrary many arguments
   options must start with "-". Arguments must not start with "-"
   except numbers (e.g. -13.2)

Compilation:
   Adjust the compiler and flags in the Makefile
   type "make" to build
   type "make test" to run tests


   There are three adjustment possibilities for your fortran standard.
   Add these to the FCFLAGS in the Makefile
   -D__HAS_QP      : Supports quadruple precision for real numbers
                     (4 times size of single precision)
   -D__HAS_IQP     : Supports quadruple precision for integer numbers
                     (4 times size of single precision)

USAGE:
   import module:
      USE cmd_opt_parser
   create a option variable, to store the help information:
      TYPE(cmd_opt_type), DIMENSION(:), POINTER :: cmd_opt
      NULLIFY(cmd_opt)
   Define command line arguments:
      (./mycommand.x 1 2 3)
      CALL get_cmd_arg(arg_pos, description, cmd_opt, variable, opt_present)
      arg_pos is the position in the command line
      description is a string used for the help call
      cmd_opt is the previously defined option variable
      variable, where to store the command line argument
      opt_present is an optional logical, that returns true if argument was parsed successfully
         if not given and argument is faulty, the program terminates
      e.g.:
      CALL get_cmd_arg(1, "this is sparta", cmd_opt, myval)
   Define command line options:
      (./mycommand.x -t 12 -h --version)
      CALL get_cmd_opt(short, long, description, cmd_opt, defaultval, variable, opt_present)
      short and long are two string versions for the option
      description is a string used for the help call
      cmd_opt is the previously defined option variable
      defaultval is the default value (must not be a pointer, but array is ok)
      variable, where to store the result of the cmdline parsing (must be pointer in case of array)
      opt_present is an optional logical, that returns true if option was on command line
      e.g.:
      CALL get_cmd_opt("-t", "--true", "this is sparta", cmd_opt, .FALSE., myval, ispresent)
   Print help text AFTER defining the options with:
      CALL show_help_msg(cmd_opt) 
   Check for unknown options:
      CALL check_invalid_cmd_opt(cmd_opt, error)
      Will print a warning and set a flag in INTEGER error for further actions.
   Finalize the cmdlineparsing to avoid memory leakage:
      CALL end_opt_parsing(cmd_opt)
