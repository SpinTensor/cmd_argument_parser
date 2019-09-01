PROGRAM test_get_cmd_arg

   USE, INTRINSIC :: ISO_FORTRAN_ENV
#ifdef __HAS_QP
   USE kinds, ONLY : sp, dp, qp, &
                     csp, cdp, cqp, &
#else
   USE kinds, ONLY : sp, dp, qp => dp, &
                     csp, cdp, cqp => cdp, &
#endif
#ifdef __HAS_IQP
                     isp, idp, iqp, &
#else
                     isp, idp, iqp => idp, &
#endif
                     default_string_length
   USE cmd_opt_parser, ONLY : cmd_opt_type, &
                              get_cmd_arg, &
                              show_help_msg, &
                              end_opt_parsing

   IMPLICIT NONE

   CHARACTER(LEN=default_string_length) :: cmdname
   
   TYPE(cmd_opt_type), DIMENSION(:), POINTER :: cmd_opt

   INTEGER(KIND=isp), PARAMETER :: isp_ref = 137_isp
   INTEGER(KIND=idp), PARAMETER :: idp_ref =  42_idp
   INTEGER(KIND=iqp), PARAMETER :: iqp_ref = -17_iqp
   REAL(KIND=sp), PARAMETER :: sp_ref = 137.0_sp
   REAL(KIND=dp), PARAMETER :: dp_ref =  42.0_dp
   REAL(KIND=qp), PARAMETER :: qp_ref = -17.0_qp
   COMPLEX(KIND=csp), PARAMETER :: csp_ref = (137.0_csp,  1.0_csp)
   COMPLEX(KIND=cdp), PARAMETER :: cdp_ref = ( 42.0_cdp, -1.0_cdp)
   COMPLEX(KIND=cqp), PARAMETER :: cqp_ref = (-17.0_cqp,  3.0_cqp)
   LOGICAL, PARAMETER :: logical_ref = .TRUE.
   CHARACTER(LEN=default_string_length), PARAMETER :: char_ref = "ThisIsString"

   INTEGER(KIND=isp) :: isp_res
   INTEGER(KIND=idp) :: idp_res
   INTEGER(KIND=iqp) :: iqp_res
   REAL(KIND=sp) :: sp_res
   REAL(KIND=dp) :: dp_res
   REAL(KIND=qp) :: qp_res
   COMPLEX(KIND=csp) :: csp_res
   COMPLEX(KIND=cdp) :: cdp_res
   COMPLEX(KIND=cqp) :: cqp_res
   LOGICAL :: logical_res
   CHARACTER(LEN=default_string_length) :: char_res

   LOGICAL :: isp_res_present
   LOGICAL :: idp_res_present
   LOGICAL :: iqp_res_present
   LOGICAL :: sp_res_present
   LOGICAL :: dp_res_present
   LOGICAL :: qp_res_present
   LOGICAL :: csp_res_present
   LOGICAL :: cdp_res_present
   LOGICAL :: cqp_res_present
   LOGICAL :: logical_res_present
   LOGICAL :: char_res_present

   LOGICAL :: passed

   NULLIFY(cmd_opt)

   CALL GET_COMMAND_ARGUMENT(0, cmdname)

   CALL get_cmd_arg( 1, "Single precision Int", cmd_opt, isp_res, isp_res_present)
   CALL get_cmd_arg( 2, "Double precision Int", cmd_opt, idp_res, idp_res_present)
   CALL get_cmd_arg( 3, "Quadru precision Int", cmd_opt, iqp_res, iqp_res_present)
   
   CALL get_cmd_arg( 4, "Single precision Real", cmd_opt, sp_res, sp_res_present)
   CALL get_cmd_arg( 5, "Double precision Real", cmd_opt, dp_res, dp_res_present)
   CALL get_cmd_arg( 6, "Quadru precision Real", cmd_opt, qp_res, qp_res_present)

   CALL get_cmd_arg( 7, "Single precision Cmplx", cmd_opt, csp_res, csp_res_present)
   CALL get_cmd_arg( 8, "Double precision Cmplx", cmd_opt, cdp_res, cdp_res_present)
   CALL get_cmd_arg( 9, "Quadru precision Cmplx", cmd_opt, cqp_res, cqp_res_present)

   CALL get_cmd_arg(10, "Logical argument", cmd_opt, logical_res, logical_res_present)
   CALL get_cmd_arg(11, "Character argument", cmd_opt, char_res, char_res_present)

   IF ((.NOT.isp_res_present) .AND. &
       (.NOT.idp_res_present) .AND. &
       (.NOT.iqp_res_present) .AND. &
       (.NOT.sp_res_present) .AND. &
       (.NOT.dp_res_present) .AND. &
       (.NOT.qp_res_present) .AND. &
       (.NOT.csp_res_present) .AND. &
       (.NOT.cdp_res_present) .AND. &
       (.NOT.cqp_res_present) .AND. &
       (.NOT.logical_res_present) .AND. &
       (.NOT.char_res_present)) THEN
      CALL show_help_msg(cmd_opt)
      STOP
   END IF

   CALL end_opt_parsing(cmd_opt)

   passed = (isp_ref == isp_res) .AND. &
            (idp_ref == idp_res) .AND. &
            (iqp_ref == iqp_res) .AND. &
            (sp_ref == sp_res) .AND. &
            (dp_ref == dp_res) .AND. &
            (qp_ref == qp_res) .AND. &
            (csp_ref == csp_res) .AND. &
            (cdp_ref == cdp_res) .AND. &
            (cqp_ref == cqp_res) .AND. &
            (logical_ref .EQV. logical_res) .AND. &
            (char_ref == char_res)
   
   IF (passed) THEN
      WRITE(UNIT=OUTPUT_UNIT, FMT="(A70,2X,A8)") ADJUSTL(cmdname), "[PASSED]"
   ELSE
      WRITE(UNIT=OUTPUT_UNIT, FMT="(A70,2X,A8)") ADJUSTL(cmdname), "  [FAIL]"
   END IF

END PROGRAM test_get_cmd_arg
