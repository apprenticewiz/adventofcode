PROGRAM day04a

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE md5_module
  IMPLICIT NONE

  CHARACTER(LEN=:), ALLOCATABLE :: key
  INTEGER :: key_len, result

  IF ( COMMAND_ARGUMENT_COUNT() < 1 ) THEN
    CALL usage()
  END IF
  CALL GET_COMMAND_ARGUMENT(1, LENGTH=key_len)
  ALLOCATE(CHARACTER(LEN=key_len) :: key)
  CALL GET_COMMAND_ARGUMENT(1, VALUE=key)

  result = process(key)
  WRITE (*,'(A,I0)') 'result = ', result
  DEALLOCATE(key)
 
CONTAINS

  SUBROUTINE usage()
    CHARACTER(LEN=:), ALLOCATABLE :: progname
    INTEGER :: progname_len

    CALL GET_COMMAND_ARGUMENT(0, LENGTH=progname_len)
    ALLOCATE(CHARACTER(LEN=progname_len) :: progname)
    CALL GET_COMMAND_ARGUMENT(0, VALUE=progname)
    WRITE (error_unit, *) 'usage: ' // TRIM(progname) // ' <key>' 
    DEALLOCATE(progname)
    CALL EXIT(1)
  END SUBROUTINE usage

  FUNCTION process(key) RESULT(n)
    CHARACTER(LEN=*), INTENT(IN) :: key
    CHARACTER(LEN=:), ALLOCATABLE :: try_key
    CHARACTER(LEN=16) :: n_str
    CHARACTER(LEN=32) :: hex_digest
    INTEGER :: n
    INTEGER :: key_len, nstr_len

    n = 1

    DO
      key_len = LEN_TRIM(key)
      WRITE(n_str, '(I0)') n
      nstr_len = LEN_TRIM(n_str)
      ALLOCATE(CHARACTER(LEN=key_len + nstr_len) :: try_key)
      try_key(1:key_len) = key(1:key_len)
      try_key(key_len+1:) = n_str(1:nstr_len)
      hex_digest = digest(try_key)
      DEALLOCATE(try_key)
      IF ( hex_digest(1:6) == '000000' ) THEN
        EXIT
      ELSE
        n = n + 1
      END IF
    END DO
  END FUNCTION process

END PROGRAM day04a
