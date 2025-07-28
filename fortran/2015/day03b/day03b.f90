PROGRAM day03b

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE string_set_module
  IMPLICIT NONE

  CHARACTER(LEN=:), ALLOCATABLE :: file_unitname
  INTEGER :: file_unitname_len, result

  IF ( COMMAND_ARGUMENT_COUNT() < 1 ) THEN
    CALL usage()
  END IF
  CALL GET_COMMAND_ARGUMENT(1, LENGTH=file_unitname_len)
  ALLOCATE(CHARACTER(LEN=file_unitname_len) :: file_unitname)
  CALL GET_COMMAND_ARGUMENT(1, VALUE=file_unitname)

  result = process(file_unitname)
  WRITE (*,'(A,I0)') 'result = ', result
  DEALLOCATE(file_unitname)
 
CONTAINS

  SUBROUTINE usage()
    CHARACTER(LEN=:), ALLOCATABLE :: progname
    INTEGER :: progname_len

    CALL GET_COMMAND_ARGUMENT(0, LENGTH=progname_len)
    ALLOCATE(CHARACTER(LEN=progname_len) :: progname)
    CALL GET_COMMAND_ARGUMENT(0, VALUE=progname)
    WRITE (error_unit, *) 'usage: ' // TRIM(progname) // ' <input file_unit>' 
    DEALLOCATE(progname)
    CALL EXIT(1)
  END SUBROUTINE usage

  FUNCTION process(file_unitname) RESULT(count)
    CHARACTER(LEN=*), INTENT(IN) :: file_unitname
    INTEGER :: file_unit, ios
    CHARACTER(LEN=1) :: ch
    INTEGER :: santa(2) = [0, 0]
    INTEGER :: robo_santa(2) = [0, 0]
    CHARACTER(LEN=16) :: key
    TYPE(string_set) :: positions
    INTEGER :: pos, count

    WRITE(key, '(I0,A,I0)') santa(1), ',', santa(2)
    CALL positions%insert(key)

    pos = 0

    OPEN(NEWUNIT=file_unit, FILE=TRIM(file_unitname), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF ( ios /= 0 ) THEN
      WRITE(error_unit, *) "Error opening file_unit: " // file_unitname
      CALL EXIT(1)
    END IF

    DO
      pos = pos + 1
      READ(file_unit, '(A)', ADVANCE='NO', IOSTAT=ios) ch
      IF ( ios /= 0 ) EXIT
      IF ( MOD(pos, 2) .EQ. 0 ) THEN
        SELECT CASE ( ch )
          CASE ( '^' )
            santa(2) = santa(2) + 1
          CASE ( 'v' )
            santa(2) = santa(2) - 1
          CASE ( '<' )
            santa(1) = santa(1) - 1
          CASE ( '>' )
            santa(1) = santa(1) + 1
        END SELECT
        WRITE(key, '(I0,A,I0)') santa(1), ',', santa(2)
      ELSE
        SELECT CASE ( ch )
          CASE ( '^' )
            robo_santa(2) = robo_santa(2) + 1
          CASE ( 'v' )
            robo_santa(2) = robo_santa(2) - 1
          CASE ( '<' )
            robo_santa(1) = robo_santa(1) - 1
          CASE ( '>' )
            robo_santa(1) = robo_santa(1) + 1
        END SELECT
        WRITE(key, '(I0,A,I0)') robo_santa(1), ',', robo_santa(2)
      END IF
      CALL positions%insert(key)
    END DO

  CLOSE(file_unit)
  count = positions%size()
  END FUNCTION process

END PROGRAM day03b
