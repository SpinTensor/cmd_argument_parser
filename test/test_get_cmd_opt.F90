PROGRAM test_get_cmd_opt

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
                              get_cmd_opt, &
                              show_help_msg, &
                              check_invalid_cmd_opt, &
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

   INTEGER(KIND=isp), DIMENSION(3), PARAMETER :: isp_arr_ref = [137_isp,138_isp,139_isp]
   INTEGER(KIND=idp), DIMENSION(3), PARAMETER :: idp_arr_ref = [ 42_idp, 43_idp, 44_idp]
   INTEGER(KIND=iqp), DIMENSION(3), PARAMETER :: iqp_arr_ref = [-17_iqp,-16_iqp,-15_iqp]
   REAL(KIND=sp), DIMENSION(3), PARAMETER :: sp_arr_ref = [137.0_sp,138.0_sp,139.0_sp]
   REAL(KIND=dp), DIMENSION(3), PARAMETER :: dp_arr_ref = [ 42.0_dp, 43.0_dp, 44.0_dp]
   REAL(KIND=qp), DIMENSION(3), PARAMETER :: qp_arr_ref = [-17.0_qp,-16.0_qp,-15.0_qp]
   COMPLEX(KIND=csp), DIMENSION(3), PARAMETER :: csp_arr_ref = [(137.0_csp,  1.0_csp),(138.0_csp,  1.0_csp),(139.0_csp,  1.0_csp)]
   COMPLEX(KIND=cdp), DIMENSION(3), PARAMETER :: cdp_arr_ref = [( 42.0_cdp, -1.0_cdp),( 43.0_cdp, -1.0_cdp),( 44.0_cdp, -1.0_cdp)]
   COMPLEX(KIND=cqp), DIMENSION(3), PARAMETER :: cqp_arr_ref = [(-17.0_cqp,  3.0_cqp),(-16.0_cqp,  3.0_cqp),(-15.0_cqp,  3.0_cqp)]
   CHARACTER(LEN=default_string_length), DIMENSION(3), PARAMETER :: char_arr_ref = ["ThisIsString1","ThisIsString2","ThisIsString3"]

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
   CHARACTER(LEN=default_string_length) ::  char_res

   INTEGER(KIND=isp), DIMENSION(:), POINTER :: isp_arr_res
   INTEGER(KIND=idp), DIMENSION(:), POINTER :: idp_arr_res
   INTEGER(KIND=iqp), DIMENSION(:), POINTER :: iqp_arr_res
   REAL(KIND=sp), DIMENSION(:), POINTER :: sp_arr_res
   REAL(KIND=dp), DIMENSION(:), POINTER :: dp_arr_res
   REAL(KIND=qp), DIMENSION(:), POINTER :: qp_arr_res
   COMPLEX(KIND=csp), DIMENSION(:), POINTER :: csp_arr_res
   COMPLEX(KIND=cdp), DIMENSION(:), POINTER :: cdp_arr_res
   COMPLEX(KIND=cqp), DIMENSION(:), POINTER :: cqp_arr_res
   CHARACTER(LEN=default_string_length), DIMENSION(:), POINTER :: char_arr_res

   LOGICAL :: show_help
   INTEGER :: error, i
   LOGICAL :: passed

   NULLIFY(cmd_opt)
   NULLIFY(isp_arr_res)
   NULLIFY(idp_arr_res)
   NULLIFY(iqp_arr_res)
   NULLIFY(sp_arr_res)
   NULLIFY(dp_arr_res)
   NULLIFY(qp_arr_res)
   NULLIFY(csp_arr_res)
   NULLIFY(cdp_arr_res)
   NULLIFY(cqp_arr_res)
   NULLIFY(char_arr_res)

   CALL GET_COMMAND_ARGUMENT(0, cmdname)

   CALL get_cmd_opt("-h", "--help", "Show this help mesasge", cmd_opt, &
                    .FALSE., show_help)

   CALL get_cmd_opt("-is", "--ints", "Single precision Int", cmd_opt, &
                    0_isp, isp_res)
   CALL get_cmd_opt("-id", "--intd", "Double precision Int", cmd_opt, &
                    0_idp, idp_res)
   CALL get_cmd_opt("-iq", "--intq", "Quadru precision Int", cmd_opt, &
                    0_iqp, iqp_res)

   CALL get_cmd_opt("-ias", "--intarrs", "Single precision Int array", cmd_opt, &
                    [0_isp,0_isp], isp_arr_res)
   CALL get_cmd_opt("-iad", "--intarrd", "Double precision Int array", cmd_opt, &
                    [0_idp,0_idp], idp_arr_res)
   CALL get_cmd_opt("-iaq", "--intarrq", "Quadru precision Int array", cmd_opt, &
                    [0_iqp,0_iqp], iqp_arr_res)

   CALL get_cmd_opt("-rs", "--reals", "Single precision Real", cmd_opt, &
                    0.0_sp, sp_res)
   CALL get_cmd_opt("-rd", "--reald", "Double precision Real", cmd_opt, &
                    0.0_dp, dp_res)
   CALL get_cmd_opt("-rq", "--realq", "Quadru precision Real", cmd_opt, &
                    0.0_qp, qp_res)

   CALL get_cmd_opt("-ras", "--realarrs", "Single precision Real array", cmd_opt, &
                    [0.0_sp,0.0_sp], sp_arr_res)
   CALL get_cmd_opt("-rad", "--realarrd", "Double precision Real array", cmd_opt, &
                    [0.0_dp,0.0_dp], dp_arr_res)
   CALL get_cmd_opt("-raq", "--realarrq", "Quadru precision Real array", cmd_opt, &
                    [0.0_qp,0.0_qp], qp_arr_res)

   CALL get_cmd_opt("-cs", "--cmplxs", "Single precision Complex", cmd_opt, &
                    (0.0_csp,0.0_csp), csp_res)
   CALL get_cmd_opt("-cd", "--cmplxd", "Double precision Complex", cmd_opt, &
                    (0.0_cdp,0.0_cdp), cdp_res)
   CALL get_cmd_opt("-cq", "--cmplxq", "Quadru precision Complex", cmd_opt, &
                    (0.0_cqp,0.0_cqp), cqp_res)

   CALL get_cmd_opt("-cas", "--cmplxarrs", "Single precision Complex array", cmd_opt, &
                    [(0.0_csp,0.0_csp),(0.0_csp,0.0_csp)], csp_arr_res)
   CALL get_cmd_opt("-cad", "--cmplxarrd", "Double precision Complex array", cmd_opt, &
                    [(0.0_cdp,0.0_cdp),(0.0_cdp,0.0_cdp)], cdp_arr_res)
   CALL get_cmd_opt("-caq", "--cmplxarrq", "Quadru precision Complex array", cmd_opt, &
                    [(0.0_cqp,0.0_cqp),(0.0_cqp,0.0_cqp)], cqp_arr_res)

   CALL get_cmd_opt("-l", "--logical", "Logical variable", cmd_opt, &
                    .FALSE., logical_res)

   CALL get_cmd_opt("-c", "--char", "Character variable", cmd_opt, &
                    "ThisIsDefault", char_res)

   CALL get_cmd_opt("-ca", "--chararr", "Character array", cmd_opt, &
                    ["ThisIsDefault    ","ThisIsAlsoDefault"], char_arr_res)

   CALL check_invalid_cmd_opt(cmd_opt, error)

   IF (show_help.OR.(error /= 0)) THEN
      CALL show_help_msg(cmd_opt)
      STOP
   END IF

   CALL end_opt_parsing(cmd_opt)

   passed = .TRUE.

   passed = passed .AND. (isp_res     ==    isp_ref)
   passed = passed .AND. (idp_res     ==    idp_ref)
   passed = passed .AND. (iqp_res     ==    iqp_ref)
   passed = passed .AND. (sp_res      ==    sp_ref)
   passed = passed .AND. (dp_res      ==    dp_ref)
   passed = passed .AND. (qp_res      ==    qp_ref)
   passed = passed .AND. (csp_res     ==    csp_ref)
   passed = passed .AND. (cdp_res     ==    cdp_ref)
   passed = passed .AND. (cqp_res     ==    cqp_ref)
   passed = passed .AND. (logical_res .EQV. logical_ref)
   passed = passed .AND. (char_res    ==    char_ref)

   DO i = 1, SIZE(isp_arr_res)
      passed = passed .AND. (isp_arr_res(i) == isp_arr_ref(i))
      passed = passed .AND. (idp_arr_res(i) == idp_arr_ref(i))
      passed = passed .AND. (iqp_arr_res(i) == iqp_arr_ref(i))
      passed = passed .AND. (sp_arr_res(i)  == sp_arr_ref(i))
      passed = passed .AND. (dp_arr_res(i)  == dp_arr_ref(i))
      passed = passed .AND. (qp_arr_res(i)  == qp_arr_ref(i))
      passed = passed .AND. (csp_arr_res(i) == csp_arr_ref(i))
      passed = passed .AND. (cdp_arr_res(i) == cdp_arr_ref(i))
      passed = passed .AND. (cqp_arr_res(i) == cqp_arr_ref(i))
      passed = passed .AND. (char_arr_res(i)== char_arr_ref(i))
   END DO

   IF (passed) THEN
      WRITE(UNIT=OUTPUT_UNIT, FMT="(A70,2X,A8)") ADJUSTL(cmdname), "[PASSED]"
   ELSE
      WRITE(UNIT=OUTPUT_UNIT, FMT="(A70,2X,A8)") ADJUSTL(cmdname), "  [FAIL]"
   END IF

   DEALLOCATE(isp_arr_res)
   DEALLOCATE(idp_arr_res)
   DEALLOCATE(iqp_arr_res)
   DEALLOCATE(sp_arr_res)
   DEALLOCATE(dp_arr_res)
   DEALLOCATE(qp_arr_res)
   DEALLOCATE(csp_arr_res)
   DEALLOCATE(cdp_arr_res)
   DEALLOCATE(cqp_arr_res)
   DEALLOCATE(char_arr_res)

   NULLIFY(isp_arr_res)
   NULLIFY(idp_arr_res)
   NULLIFY(iqp_arr_res)
   NULLIFY(sp_arr_res)
   NULLIFY(dp_arr_res)
   NULLIFY(qp_arr_res)
   NULLIFY(csp_arr_res)
   NULLIFY(cdp_arr_res)
   NULLIFY(cqp_arr_res)
   NULLIFY(char_arr_res)

END PROGRAM test_get_cmd_opt
