PROGRAM day02a

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

  FUNCTION process(filename) RESULT(total_area)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: total_area
    INTEGER :: file_unit, ios
    INTEGER :: first_x, second_x
    INTEGER :: l, w, h
    INTEGER :: area1, area2, area3
    INTEGER :: surface_area, min_area
    CHARACTER(LEN=16) :: line

    total_area = 0

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
      first_x = SCAN(line, 'x')
      READ(line(1:first_x-1), *) l
      second_x = SCAN(line(first_x+1:), 'x')
      READ(line(first_x+1:first_x+second_x-1), *) w
      READ(line(first_x+second_x+1:), *) h
      area1 = l * w
      area2 = l * h
      area3 = w * h
      surface_area = (2 * area1) + (2 * area2) + (2 * area3)
      min_area = MIN(area1, area2, area3)
      total_area = total_area + surface_area + min_area
    END DO

    CLOSE(file_unit)
  END FUNCTION process

END PROGRAM day02a
