PROGRAM day02b

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

  FUNCTION process(filename) RESULT(total_len)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER :: nl
    INTEGER :: total_len
    INTEGER :: file, file_size, ios
    INTEGER :: start_pos, line_len
    INTEGER :: first_x, second_x
    INTEGER :: l, w, h
    INTEGER :: perim1, perim2, perim3
    INTEGER :: present_len
    INTEGER :: bow_len
    CHARACTER(LEN=:), ALLOCATABLE :: contents, line

    total_len = 0

    OPEN(NEWUNIT=file, &
         FILE=TRIM(filename), &
         STATUS='OLD', &
         ACTION='READ', &
         ACCESS='STREAM', &
         FORM='unformatted', &
         IOSTAT=ios)
    IF ( ios /= 0 ) THEN
      WRITE(error_unit, *) "Error opening file: " // filename
      CALL EXIT(1)
    END IF
    INQUIRE(file, SIZE=file_size)
    ALLOCATE(CHARACTER(LEN=file_size) :: contents)
    READ(file, IOSTAT=ios) contents
    CLOSE(file)

    start_pos = 1
    nl = NEW_LINE('a')
    DO
      line_len = SCAN(contents(start_pos:), nl)
      IF ( line_len == 0 ) EXIT
      ALLOCATE(CHARACTER(LEN=line_len-1) :: line)
      line = contents(start_pos:start_pos+line_len-1)
      first_x = SCAN(line, 'x')
      READ(line(1:first_x-1), *) l
      second_x = SCAN(line(first_x+1:), 'x')
      READ(line(first_x+1:first_x+second_x-1), *) w
      READ(line(first_x+second_x+1:), *) h
      perim1 = 2 * (l + w)
      perim2 = 2 * (l + h)
      perim3 = 2 * (w + h)
      present_len = MIN(perim1, perim2, perim3)
      bow_len = l * w * h
      total_len = total_len + present_len + bow_len
      DEALLOCATE(line)
      start_pos = start_pos + line_len
      IF ( start_pos > file_size ) EXIT
    END DO

    DEALLOCATE(contents)
  END FUNCTION process

END PROGRAM day02b
