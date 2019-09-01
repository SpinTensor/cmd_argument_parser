!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! NAME: 
!!    cmd_opt_parser
!! DESCRIPTION:
!!    abstracts the usage of command line parameters
!!    every option supports arbritrary many arguments
!!    options must start with "-". Arguments must not start with "-"
!!    except numbers (e.g. -13.2)
!!    part of the libfuhl
!! USAGE:
!!    import module:
!!       USE cmd_opt_parser
!!    create a option variable, to store the help information:
!!       TYPE(cmd_opt_type), DIMENSION(:), POINTER :: cmd_opt
!!       NULLIFY(cmd_opt)
!!    Define command line arguments:
!!       (./mycommand.x 1 2 3)
!!       CALL get_cmd_arg(arg_pos, description, cmd_opt, variable, opt_present)
!!       arg_pos is the position in the command line
!!       description is a string used for the help call
!!       cmd_opt is the previously defined option variable
!!       variable, where to store the command line argument
!!       opt_present is an optional logical, that returns true if argument was parsed successfully
!!          if not given and argument is faulty, the program terminates
!!       e.g.:
!!       CALL get_cmd_arg(1, "this is sparta", cmd_opt, myval)
!!    Define command line options:
!!       (./mycommand.x -t 12 -h --version)
!!       CALL get_cmd_opt(short, long, description, cmd_opt, defaultval, variable, opt_present)
!!       short and long are two string versions for the option
!!       description is a string used for the help call
!!       cmd_opt is the previously defined option variable
!!       defaultval is the default value (must not be a pointer, but array is ok)
!!       variable, where to store the result of the cmdline parsing (must be pointer in case of array)
!!       opt_present is an optional logical, that returns true if option was on command line
!!       e.g.:
!!       CALL get_cmd_opt("-t", "--true", "this is sparta", cmd_opt, .FALSE., myval, ispresent)
!!    Print help text AFTER defining the options with:
!!       CALL show_help_msg(cmd_opt) 
!!    Check for unknown options:
!!       CALL check_invalid_cmd_opt(cmd_opt, error)
!!       Will print a warning and set a flag in INTEGER error for further actions.
!!    Finalize the cmdlineparsing to avoid memory leakage:
!!       CALL end_opt_parsing(cmd_opt)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE cmd_opt_parser

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE kinds, ONLY : default_string_length, &
#ifdef __HAS_QP
                     sp, dp, qp, &
                     csp, cdp, cqp, &
#else
                     sp, dp, &
                     csp, cdp, &
#endif
#ifdef __HAS_IQP
                     isp, idp, iqp
#else
                     isp, idp
#endif

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: cmd_opt_type
   PUBLIC :: get_cmd_opt
   PUBLIC :: get_cmd_arg
   PUBLIC :: check_invalid_cmd_opt
   PUBLIC :: show_help_msg
   PUBLIC :: end_opt_parsing

   INTERFACE get_cmd_opt
      MODULE PROCEDURE get_cmd_opt_logical
      MODULE PROCEDURE get_cmd_opt_real_sp
      MODULE PROCEDURE get_cmd_opt_real_dp
#ifdef __HAS_QP
      MODULE PROCEDURE get_cmd_opt_real_qp
#endif
      MODULE PROCEDURE get_cmd_opt_int_sp
      MODULE PROCEDURE get_cmd_opt_int_dp
#ifdef __HAS_IQP
      MODULE PROCEDURE get_cmd_opt_int_qp
#endif
      MODULE PROCEDURE get_cmd_opt_cmplx_sp
      MODULE PROCEDURE get_cmd_opt_cmplx_dp
#ifdef __HAS_QP
      MODULE PROCEDURE get_cmd_opt_cmplx_qp
#endif
      MODULE PROCEDURE get_cmd_opt_string
      MODULE PROCEDURE get_cmd_opt_real_sp_ap
      MODULE PROCEDURE get_cmd_opt_real_dp_ap
#ifdef __HAS_QP
      MODULE PROCEDURE get_cmd_opt_real_qp_ap
#endif
      MODULE PROCEDURE get_cmd_opt_int_sp_ap
      MODULE PROCEDURE get_cmd_opt_int_dp_ap
#ifdef __HAS_IQP
      MODULE PROCEDURE get_cmd_opt_int_qp_ap
#endif
      MODULE PROCEDURE get_cmd_opt_cmplx_sp_ap
      MODULE PROCEDURE get_cmd_opt_cmplx_dp_ap
#ifdef __HAS_QP
      MODULE PROCEDURE get_cmd_opt_cmplx_qp_ap
#endif
      MODULE PROCEDURE get_cmd_opt_string_ap
   END INTERFACE get_cmd_opt

   INTERFACE get_cmd_arg
      MODULE PROCEDURE get_cmd_arg_logical
      MODULE PROCEDURE get_cmd_arg_real_sp
      MODULE PROCEDURE get_cmd_arg_real_dp
#ifdef __HAS_QP
      MODULE PROCEDURE get_cmd_arg_real_qp
#endif
      MODULE PROCEDURE get_cmd_arg_int_sp
      MODULE PROCEDURE get_cmd_arg_int_dp
#ifdef __HAS_IQP
      MODULE PROCEDURE get_cmd_arg_int_qp
#endif
      MODULE PROCEDURE get_cmd_arg_cmplx_sp
      MODULE PROCEDURE get_cmd_arg_cmplx_dp
#ifdef __HAS_QP
      MODULE PROCEDURE get_cmd_arg_cmplx_qp
#endif
      MODULE PROCEDURE get_cmd_arg_string
   END INTERFACE get_cmd_arg

   TYPE cmd_opt_type
      CHARACTER(LEN=default_string_length) :: shortopt
      CHARACTER(LEN=default_string_length) :: longopt
      CHARACTER(LEN=default_string_length) :: descr
      CHARACTER(LEN=default_string_length) :: otype
      CHARACTER(LEN=default_string_length) :: defaultval
      INTEGER :: pos
   END TYPE cmd_opt_type

CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Checks if the first field of a string is a Real number
   !! Variables:
   !!    string_in: input string of unknown length
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   PURE FUNCTION is_numeric(string) RESULT(isnumber)

      IMPLICIT NONE

      CHARACTER(len=*), INTENT(IN) :: string

      LOGICAL :: isnumber
      REAL :: x
      INTEGER :: error

      READ(UNIT=string,FMT=*,IOSTAT=error) x
      isnumber = (error == 0)

      RETURN
   END FUNCTION is_numeric

   SUBROUTINE reallocate_cmd_opt(cmd_opt)

      IMPLICIT NONE

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt
      TYPE(cmd_opt_type), DIMENSION(:), POINTER :: tmp_opt

      INTEGER :: nopts
      
      IF (.NOT.ASSOCIATED(cmd_opt)) THEN
         nopts = 1
         ALLOCATE(cmd_opt(nopts))
      ELSE
         nopts = SIZE(cmd_opt)
         ALLOCATE(tmp_opt(nopts+1))
         tmp_opt(1:nopts) = cmd_opt(1:nopts)
         DEALLOCATE(cmd_opt)
         NULLIFY(cmd_opt)
         cmd_opt => tmp_opt
         NULLIFY(tmp_opt)
         nopts = nopts + 1
      END IF

      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = ""
      cmd_opt(nopts)%otype = ""
      cmd_opt(nopts)%defaultval = ""
      cmd_opt(nopts)%pos = 0

      RETURN

   END SUBROUTINE reallocate_cmd_opt

   SUBROUTINE end_opt_parsing(cmd_opt)

      IMPLICIT NONE

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      DEALLOCATE(cmd_opt)
      NULLIFY(cmd_opt)

   END SUBROUTINE end_opt_parsing

   SUBROUTINE get_cmd_opt_logical(shortopt, longopt, description, cmd_opt, &
                                  defaultvalue, variable)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt
     
      LOGICAL, INTENT(IN) :: defaultvalue
      LOGICAL, INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            variable = .TRUE.
            EXIT
         END IF
      END DO

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "LOGICAL"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_logical

   SUBROUTINE get_cmd_opt_real_sp(shortopt, longopt, description, cmd_opt, &
                                  defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      REAL(KIND=sp), INTENT(IN) :: defaultvalue
      REAL(KIND=sp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "REAL"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_real_sp

   SUBROUTINE get_cmd_opt_real_dp(shortopt, longopt, description, cmd_opt, &
                                  defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      REAL(KIND=dp), INTENT(IN) :: defaultvalue
      REAL(KIND=dp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "REAL"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_real_dp

#ifdef __HAS_QP
   SUBROUTINE get_cmd_opt_real_qp(shortopt, longopt, description, cmd_opt, &
                                  defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      REAL(KIND=qp), INTENT(IN) :: defaultvalue
      REAL(KIND=qp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "REAL"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_real_qp
#endif

   SUBROUTINE get_cmd_opt_int_sp(shortopt, longopt, description, cmd_opt, &
                                 defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      INTEGER(KIND=isp), INTENT(IN) :: defaultvalue
      INTEGER(KIND=isp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "INTEGER"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_int_sp

   SUBROUTINE get_cmd_opt_int_dp(shortopt, longopt, description, cmd_opt, &
                                 defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      INTEGER(KIND=idp), INTENT(IN) :: defaultvalue
      INTEGER(KIND=idp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "INTEGER"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_int_dp

#ifdef __HAS_IQP
   SUBROUTINE get_cmd_opt_int_qp(shortopt, longopt, description, cmd_opt, &
                                 defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      INTEGER(KIND=iqp), INTENT(IN) :: defaultvalue
      INTEGER(KIND=iqp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "INTEGER"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_int_qp
#endif
   
   SUBROUTINE get_cmd_opt_cmplx_sp(shortopt, longopt, description, cmd_opt, &
                                   defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      COMPLEX(KIND=csp), INTENT(IN) :: defaultvalue
      COMPLEX(KIND=csp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "COMPLEX"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_cmplx_sp

   SUBROUTINE get_cmd_opt_cmplx_dp(shortopt, longopt, description, cmd_opt, &
                                   defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      COMPLEX(KIND=cdp), INTENT(IN) :: defaultvalue
      COMPLEX(KIND=cdp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "COMPLEX"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue

      RETURN

   END SUBROUTINE get_cmd_opt_cmplx_dp

#ifdef __HAS_QP
   SUBROUTINE get_cmd_opt_cmplx_qp(shortopt, longopt, description, cmd_opt, &
                                   defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      COMPLEX(KIND=cqp), INTENT(IN) :: defaultvalue
      COMPLEX(KIND=cqp), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "COMPLEX"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue

      RETURN

   END SUBROUTINE get_cmd_opt_cmplx_qp
#endif

   SUBROUTINE get_cmd_opt_string(shortopt, longopt, description, cmd_opt, &
                                 defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      CHARACTER(LEN=*), INTENT(IN) :: defaultvalue
      CHARACTER(LEN=*), INTENT(OUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: nopts
      INTEGER :: optindx

      INTEGER :: readerr

      variable = defaultvalue
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         IF (ncmdargs >= optindx+1) THEN
            CALL GET_COMMAND_ARGUMENT(optindx+1, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               variable = TRIM(cmdarg)
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
               STOP
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "STRING"
      cmd_opt(nopts)%pos = -1
      cmd_opt(nopts)%defaultval = TRIM(defaultvalue)

      RETURN

   END SUBROUTINE get_cmd_opt_string

   SUBROUTINE get_cmd_opt_real_sp_ap(shortopt, longopt, description, cmd_opt, &
                                     defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: defaultvalue
      REAL(KIND=sp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[REAL]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_real_sp_ap
   
   SUBROUTINE get_cmd_opt_real_dp_ap(shortopt, longopt, description, cmd_opt, &
                                     defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: defaultvalue
      REAL(KIND=dp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[REAL]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_real_dp_ap

#ifdef __HAS_QP 
   SUBROUTINE get_cmd_opt_real_qp_ap(shortopt, longopt, description, cmd_opt, &
                                     defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: defaultvalue
      REAL(KIND=qp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[REAL]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_real_qp_ap
#endif

   SUBROUTINE get_cmd_opt_int_sp_ap(shortopt, longopt, description, cmd_opt, &
                                    defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      INTEGER(KIND=isp), DIMENSION(:), INTENT(IN) :: defaultvalue
      INTEGER(KIND=isp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[INTEGER]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_int_sp_ap
   
   SUBROUTINE get_cmd_opt_int_dp_ap(shortopt, longopt, description, cmd_opt, &
                                    defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      INTEGER(KIND=idp), DIMENSION(:), INTENT(IN) :: defaultvalue
      INTEGER(KIND=idp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[INTEGER]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_int_dp_ap

#ifdef __HAS_IQP
   SUBROUTINE get_cmd_opt_int_qp_ap(shortopt, longopt, description, cmd_opt, &
                                    defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      INTEGER(KIND=iqp), DIMENSION(:), INTENT(IN) :: defaultvalue
      INTEGER(KIND=iqp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[INTEGER]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_int_qp_ap
#endif

   SUBROUTINE get_cmd_opt_cmplx_sp_ap(shortopt, longopt, description, cmd_opt, &
                                      defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      COMPLEX(KIND=csp), DIMENSION(:), INTENT(IN) :: defaultvalue
      COMPLEX(KIND=csp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[COMPLEX]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_cmplx_sp_ap

   SUBROUTINE get_cmd_opt_cmplx_dp_ap(shortopt, longopt, description, cmd_opt, &
                                      defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      COMPLEX(KIND=cdp), DIMENSION(:), INTENT(IN) :: defaultvalue
      COMPLEX(KIND=cdp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[COMPLEX]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_cmplx_dp_ap

#ifdef __HAS_QP
   SUBROUTINE get_cmd_opt_cmplx_qp_ap(shortopt, longopt, description, cmd_opt, &
                                      defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      COMPLEX(KIND=cqp), DIMENSION(:), INTENT(IN) :: defaultvalue
      COMPLEX(KIND=cqp), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable(iarg)
                  IF (readerr /= 0) THEN
                     WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                     STOP
                  END IF
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[COMPLEX]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_cmplx_qp_ap
#endif

   SUBROUTINE get_cmd_opt_string_ap(shortopt, longopt, description, cmd_opt, &
                                    defaultvalue, variable, opt_present)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: shortopt
      CHARACTER(LEN=*), INTENT(IN) :: longopt
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 
      CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: defaultvalue
      CHARACTER(LEN=*), DIMENSION(:), POINTER, INTENT(INOUT) :: variable

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg
      INTEGER :: iarg
      INTEGER :: nopts
      INTEGER :: optindx
      INTEGER :: endargsidx

      INTEGER :: readerr

      ncmdargs = COMMAND_ARGUMENT_COUNT()
      optindx = 0
      IF (PRESENT(opt_present)) opt_present = .FALSE.
      DO icmdarg = 1, ncmdargs
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (TRIM(cmdarg) == TRIM(shortopt) .OR. TRIM(cmdarg) == TRIM(longopt)) THEN
            optindx = icmdarg
            IF (PRESENT(opt_present)) opt_present = .TRUE.
            EXIT
         END IF
      END DO
      IF (optindx /= 0) THEN
         endargsidx = ncmdargs
         DO icmdarg = optindx+1, ncmdargs
            CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg, STATUS=readerr)
            IF (INDEX(cmdarg,"-") == 1) THEN
               IF (.NOT.is_numeric(cmdarg(2:2))) THEN
                  endargsidx = icmdarg - 1
                  EXIT
               END IF
            END IF
         END DO
         IF (endargsidx /= optindx) THEN
            ALLOCATE(variable(endargsidx - optindx))
            DO iarg = 1, endargsidx - optindx
               CALL GET_COMMAND_ARGUMENT(optindx+iarg, cmdarg, STATUS=readerr)
               IF(readerr == 0) THEN
                  variable(iarg) = TRIM(cmdarg)
               ELSE
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid argument for option: ", TRIM(shortopt)
                  STOP
               END IF
            END DO
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "No argument given for option: ", TRIM(shortopt)
            STOP
         END IF
      ELSE
         ALLOCATE(variable(SIZE(defaultvalue)))
         variable(:) = defaultvalue(:)
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = shortopt
      cmd_opt(nopts)%longopt = longopt
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "[STRING]"
      cmd_opt(nopts)%pos = -1
      WRITE(UNIT=cmd_opt(nopts)%defaultval,FMT=*) defaultvalue
      
      RETURN
      
   END SUBROUTINE get_cmd_opt_string_ap

   SUBROUTINE get_cmd_arg_logical(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      LOGICAL, INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "LOGICAL"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_logical

   SUBROUTINE get_cmd_arg_real_sp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      REAL(KIND=sp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "REAL"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_real_sp

   SUBROUTINE get_cmd_arg_real_dp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      REAL(KIND=dp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "REAL"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_real_dp

#ifdef __HAS_QP
   SUBROUTINE get_cmd_arg_real_qp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      REAL(KIND=qp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "REAL"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_real_qp
#endif

   SUBROUTINE get_cmd_arg_int_sp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      INTEGER(KIND=isp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "INTEGER"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_int_sp

   SUBROUTINE get_cmd_arg_int_dp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      INTEGER(KIND=idp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "INTEGER"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_int_dp

#ifdef __HAS_IQP
   SUBROUTINE get_cmd_arg_int_qp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      INTEGER(KIND=iqp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)                                                                                          
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "INTEGER"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_int_qp
#endif

   SUBROUTINE get_cmd_arg_cmplx_sp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      COMPLEX(KIND=csp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "COMPLEX"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_cmplx_sp

   SUBROUTINE get_cmd_arg_cmplx_dp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      COMPLEX(KIND=cdp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP    
      END IF     
                 
      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "COMPLEX"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""
                 
      RETURN     
                 
   END SUBROUTINE get_cmd_arg_cmplx_dp

#ifdef __HAS_QP
   SUBROUTINE get_cmd_arg_cmplx_qp(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      COMPLEX(KIND=cqp), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, cmdarg, STATUS=readerr)
            IF(readerr == 0) THEN
               READ(UNIT=cmdarg, FMT=*, IOSTAT=readerr) variable
               IF (readerr /= 0) THEN
                  WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
                  IF (PRESENT(opt_present)) THEN
                     opt_present = .FALSE.
                  ELSE
                     STOP
                  END IF
               END IF
            ELSE
               WRITE(UNIT=ERROR_UNIT, FMT=*) "invalid first argument. check source for usage"
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "COMPLEX"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_cmplx_qp
#endif

   SUBROUTINE get_cmd_arg_string(arg_pos, description, cmd_opt, variable, opt_present)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: arg_pos
      CHARACTER(LEN=*), INTENT(IN) :: description

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(INOUT) :: cmd_opt

      CHARACTER(LEN=*), INTENT(OUT) :: variable
      LOGICAL, INTENT(OUT), OPTIONAL :: opt_present 

      INTEGER :: ncmdargs
      INTEGER :: nopts

      INTEGER :: readerr

      IF (PRESENT(opt_present)) opt_present = .TRUE.
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      IF (arg_pos >= 1) THEN
         IF (ncmdargs >= arg_pos) THEN
            CALL GET_COMMAND_ARGUMENT(arg_pos, variable, STATUS=readerr)
            IF (readerr /= 0) THEN
               WRITE(UNIT=ERROR_UNIT, FMT=*) "Invalid value for command line argument Nr. ", arg_pos
               IF (PRESENT(opt_present)) THEN
                  opt_present = .FALSE.
               ELSE
                  STOP
               END IF
            END IF
         ELSE
            WRITE(UNIT=ERROR_UNIT, FMT=*) "not enough arguments given. check source for usage"
            IF (PRESENT(opt_present)) THEN
               opt_present = .FALSE.
            ELSE
               STOP
            END IF
         END IF
      ELSE
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Error in declaring non optional command line argument Nr. ", arg_pos
         WRITE(UNIT=ERROR_UNIT, FMT=*) "Argument position needs to be positive."
         STOP
      END IF

      CALL reallocate_cmd_opt(cmd_opt)
      nopts = SIZE(cmd_opt)
      cmd_opt(nopts)%shortopt = ""
      cmd_opt(nopts)%longopt = ""
      cmd_opt(nopts)%descr = description
      cmd_opt(nopts)%otype = "STRING"
      cmd_opt(nopts)%pos = arg_pos
      cmd_opt(nopts)%defaultval = ""

      RETURN

   END SUBROUTINE get_cmd_arg_string

   SUBROUTINE show_help_msg(cmd_opt)

      IMPLICIT NONE

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(IN) :: cmd_opt

      INTEGER :: nopts
      INTEGER :: iopt

      INTEGER, PARAMETER :: descrstrl = 50
      INTEGER :: ndescstrings
      INTEGER :: istr
      INTEGER :: strs, stre

      nopts = SIZE(cmd_opt)
      DO iopt = 1, nopts
         ndescstrings = LEN_TRIM(cmd_opt(iopt)%descr)/descrstrl
         IF (cmd_opt(iopt)%pos >= 1) THEN
            WRITE(UNIT=OUTPUT_UNIT,FMT='(A12,1X,I7,7X,A10)') "Argument Nr.", &
               cmd_opt(iopt)%pos, &
               cmd_opt(iopt)%otype
            DO istr = 0, ndescstrings
               strs = istr*descrstrl+1
               stre = (istr+1)*descrstrl
               WRITE(UNIT=OUTPUT_UNIT,FMT='(27X,A)') &
                  TRIM(cmd_opt(iopt)%descr(INT(strs,idp):INT(stre,idp)))
            END DO
         ELSE
            WRITE(UNIT=OUTPUT_UNIT,FMT='(A4,A1,1X,A20,1X,A10)') &
               cmd_opt(iopt)%shortopt, &
               ",", &
               cmd_opt(iopt)%longopt, &
               cmd_opt(iopt)%otype
            WRITE(UNIT=OUTPUT_UNIT,FMT='(27X,A)') "Default: "//TRIM(cmd_opt(iopt)%defaultval)
            DO istr = 0, ndescstrings
               strs = istr*descrstrl+1
               stre = (istr+1)*descrstrl
               WRITE(UNIT=OUTPUT_UNIT,FMT='(27X,A)') &
                  TRIM(cmd_opt(iopt)%descr(INT(strs,idp):INT(stre,idp)))
            END DO
         END IF
      END DO

      RETURN
   END SUBROUTINE show_help_msg

   SUBROUTINE check_invalid_cmd_opt(cmd_opt, error)

      IMPLICIT NONE

      TYPE(cmd_opt_type), DIMENSION(:), POINTER, INTENT(IN) :: cmd_opt
      INTEGER, INTENT(OUT) :: error

      CHARACTER(LEN=default_string_length) :: cmdarg
      INTEGER :: ncmdargs
      INTEGER :: icmdarg

      INTEGER :: nopts
      INTEGER :: iopt

      LOGICAL :: validopt

      error = 0
      nopts = SIZE(cmd_opt)
      ncmdargs = COMMAND_ARGUMENT_COUNT()
      DO icmdarg = 1, ncmdargs
         validopt = .FALSE.
         CALL GET_COMMAND_ARGUMENT(icmdarg, cmdarg)
         IF (INDEX(cmdarg,"-") == 1) THEN
            IF (.NOT.is_numeric(cmdarg(2:2))) THEN
               DO iopt = 1, nopts
                  IF (TRIM(cmdarg) == TRIM(cmd_opt(iopt)%shortopt) .OR. &
                      TRIM(cmdarg) == TRIM(cmd_opt(iopt)%longopt)) THEN
                     validopt = .TRUE.
                     EXIT
                  END IF
               END DO
               IF (.NOT.validopt) THEN
                  WRITE(UNIT=ERROR_UNIT,FMT=*) "Warning: ", TRIM(cmdarg), " is an unknown option."
                  WRITE(UNIT=ERROR_UNIT,FMT=*) "         Check source for help."
               END IF
            END IF
         END IF
      END DO

      RETURN

   END SUBROUTINE
 
END MODULE cmd_opt_parser
