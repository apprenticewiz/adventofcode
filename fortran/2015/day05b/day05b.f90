PROGRAM day05b

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

  FUNCTION prop1(str) RESULT(res)
    CHARACTER(LEN=32), INTENT(IN) :: str
    CHARACTER(LEN=2) :: first_pair
    LOGICAL :: res
    INTEGER :: i

    DO i = 1, LEN_TRIM(str) - 3
      first_pair = str(i:i+1)
      IF ( INDEX(str(i+2:), first_pair) /= 0 ) THEN
        res = .TRUE.
        RETURN
      END IF
    END DO
    res = .FALSE.
  END FUNCTION prop1

  FUNCTION prop2(str) RESULT(res)
    CHARACTER(LEN=32), INTENT(IN) :: str
    LOGICAL :: res
    INTEGER :: i

    DO i = 3, LEN_TRIM(str)
      IF ( str(i:i) == str(i-2:i-2) ) THEN
        res = .TRUE.
        RETURN
      END IF
    END DO
    res = .FALSE.
  END FUNCTION prop2

  FUNCTION process(filename) RESULT(count)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: count
    INTEGER :: file_unit, ios
    CHARACTER(LEN=32) :: line

    count = 0

    OPEN(NEWUNIT=file_unit, &
         FILE=TRIM(filename), &
         STATUS='OLD', &
         ACTION='READ', &
         IOSTAT=ios)
    IF ( ios /= 0 ) THEN
      WRITE(error_unit, *) "Error opening file: " // filename
      CALL EXIT(1)
    END IF

    DO
      READ(file_unit, '(A)', iostat=ios) line
      IF ( ios /= 0 ) EXIT
      IF ( prop1(line) .AND. prop2(line) ) THEN
        count = count + 1
      END IF
    END DO

    CLOSE(file_unit)
  END FUNCTION process

END PROGRAM day05b
