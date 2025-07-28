PROGRAM day01a

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE

  CHARACTER(LEN=:), ALLOCATABLE :: filename
  INTEGER :: filename_len, result

  IF ( COMMAND_ARGUMENT_COUNT() < 1 ) THEN
    CALL usage()
  END IF
  CALL GET_COMMAND_ARGUMENT(1, LENGTH=filename_len)
  ALLOCATE(CHARACTER(LEN=filename_len) :: filename)
  CALL GET_COMMAND_ARGUMENT(1, VALUE=filename)

  result = process(filename)
  WRITE (*,'(A,I0)') 'result = ', result
 
CONTAINS

  SUBROUTINE usage()
    CHARACTER(LEN=:), ALLOCATABLE :: progname
    INTEGER :: progname_len

    CALL GET_COMMAND_ARGUMENT(0, LENGTH=progname_len)
    ALLOCATE(CHARACTER(LEN=progname_len) :: progname)
    CALL GET_COMMAND_ARGUMENT(0, VALUE=progname)
    WRITE (error_unit, *) 'usage: ' // TRIM(progname) // ' <input file>' 
    CALL EXIT(1)
  END SUBROUTINE usage

  FUNCTION process(filename) RESULT(counter)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: file, ios
    INTEGER :: counter 
    CHARACTER(LEN=1) :: ch

    counter = 0

    OPEN(NEWUNIT=file, FILE=TRIM(filename), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF ( ios /= 0 ) THEN
      WRITE(error_unit, *) "Error opening file: " // filename
      CALL EXIT(1)
    END IF

    DO
      READ(file, '(A)', ADVANCE='NO', IOSTAT=ios) ch
      IF ( ios /= 0 ) EXIT
      SELECT CASE ( ch )
      CASE ( '(' )
        counter = counter + 1
      CASE ( ')' )
        counter = counter - 1
      END SELECT
    END DO
  END FUNCTION process

END PROGRAM day01a
