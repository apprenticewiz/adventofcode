PROGRAM day08a

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

  INTEGER FUNCTION process(filename) RESULT(res)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: file_unit, ios
    CHARACTER(LEN=80) :: buf
    CHARACTER(:), ALLOCATABLE :: line
    INTEGER :: code_len, mem_len, i

    res = 0
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
      READ(file_unit, '(A)', iostat=ios) buf
      IF ( ios /= 0 ) EXIT
      line = TRIM(buf)
      code_len = LEN(line)
      i = 2
      mem_len = 0
      DO WHILE ( i < LEN(line) )
          IF ( line(i:i) == '\' ) THEN
            SELECT CASE ( line(i+1:i+1) )
              CASE ('\', '"')
                i = i + 2
              CASE ('x')
                i = i + 4
            END SELECT
          ELSE
            i = i + 1
          END IF
          mem_len = mem_len + 1
      END DO
      res = res + (code_len - mem_len)
    END DO
    CLOSE(file_unit)
  END FUNCTION process
END PROGRAM day08a
