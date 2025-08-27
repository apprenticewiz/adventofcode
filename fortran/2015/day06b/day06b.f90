PROGRAM day06b

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

  SUBROUTINE perform(grid, action, r1, c1, r2, c2)
    INTEGER, INTENT(INOUT) :: grid(:,:)
    CHARACTER(LEN=10), INTENT(IN) :: action
    INTEGER, INTENT(IN) :: r1, c1, r2, c2

    IF ( TRIM(action) == 'turn on' ) grid(r1:r2,c1:c2) = grid(r1:r2,c1:c2) + 1
    IF ( TRIM(action) == 'turn off' ) grid(r1:r2,c1:c2) = MAX(0, grid(r1:r2,c1:c2) - 1)
    IF ( TRIM(action) == 'toggle' ) grid(r1:r2,c1:c2) = grid(r1:r2,c1:c2) + 2
  END SUBROUTINE perform

  FUNCTION process(filename) RESULT(total)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: total
    INTEGER :: file_unit, ios
    INTEGER :: grid(1000, 1000)
    CHARACTER(LEN=80) :: line
    CHARACTER(LEN=10) :: action
    CHARACTER(LEN=70) :: coords
    CHARACTER(LEN=10) :: coord1, coord2
    CHARACTER(LEN=5) :: tok
    INTEGER :: tidx
    INTEGER :: r1, c1, r2, c2

    grid(:,:) = 0
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
      IF ( line(1:7) == 'turn on' ) THEN
        action = line(1:7)
        coords = line(9:)
      ELSE IF ( line(1:8) == 'turn off' ) THEN
        action = line(1:8)
        coords = line(10:)
      ELSE IF ( line(1:6) == 'toggle' ) THEN
        action = line(1:6)
        coords = line(8:)
      END IF
      tidx = INDEX(coords, ' through ')
      coord1 = coords(1:tidx - 1)
      coord2 = coords(tidx + 9:)
      tidx = INDEX(coord1, ',')
      tok = coord1(1:tidx - 1)
      READ(tok, '(I3)') r1
      tok = coord1(tidx + 1:)
      READ(tok, '(I3)') c1
      tidx = INDEX(coord2, ',')
      tok = coord2(1:tidx - 1)
      READ(tok, '(I3)') r2
      tok = coord2(tidx + 1:)
      READ(tok, '(I3)') c2
      CALL perform(grid, action, r1, c1, r2, c2)
    END DO

    CLOSE(file_unit)
    total = SUM(grid)
  END FUNCTION process

END PROGRAM day06b
