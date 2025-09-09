PROGRAM day07a

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE

  TYPE :: operation
    INTEGER :: op
    CHARACTER(:), ALLOCATABLE :: src1, src2
    INTEGER :: amt
  END TYPE operation

  TYPE :: op_entry
    CHARACTER(:), ALLOCATABLE :: key
    TYPE(operation) :: val
  END TYPE op_entry

  TYPE :: operation_table
    TYPE(op_entry) :: entries(1000)
    INTEGER :: next_free
  END TYPE operation_table

  TYPE :: cache_entry
    CHARACTER(:), ALLOCATABLE :: key
    INTEGER :: val
  END TYPE cache_entry

  TYPE :: cache_table
    TYPE(cache_entry) :: entries(1000)
    INTEGER :: next_free
  END TYPE

  INTEGER, PARAMETER :: ASSIGN_OP = 1
  INTEGER, PARAMETER :: NOT_OP = 2
  INTEGER, PARAMETER :: AND_OP = 3
  INTEGER, PARAMETER :: OR_OP = 4
  INTEGER, PARAMETER :: LSHIFT_OP = 5
  INTEGER, PARAMETER :: RSHIFT_OP = 6

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
    TYPE(operation_table) :: operations
    TYPE(cache_table) :: cache
    CHARACTER(LEN=*), INTENT(IN) :: filename
    TYPE(operation) :: op
    TYPE(op_entry) :: entry
    INTEGER :: file_unit, ios
    INTEGER :: arrow_idx, op_idx
    CHARACTER(LEN=80) :: buf
    CHARACTER(:), ALLOCATABLE :: line
    CHARACTER(:), ALLOCATABLE :: dest
    CHARACTER(:), ALLOCATABLE :: amt_str

    operations%next_free = 1
    cache%next_free = 1
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
      arrow_idx = INDEX(line, ' -> ')
      dest = TRIM(line(arrow_idx+4:))
      IF ( line(1:3) == 'NOT' ) THEN
          op%op = NOT_OP
          op%src1 = TRIM(line(5:arrow_idx-1))
      ELSE IF ( INDEX(line, " AND ") /= 0 ) THEN
          op_idx = INDEX(line, " AND ")
          op%op = AND_OP
          op%src1 = TRIM(line(1:op_idx-1))
          op%src2 = TRIM(line(op_idx+5:arrow_idx-1))
      ELSE IF ( INDEX(line, " OR ") /= 0 ) THEN
          op_idx = INDEX(line, " OR ")
          op%op = OR_OP
          op%src1 = TRIM(line(1:op_idx-1))
          op%src2 = TRIM(line(op_idx+4:arrow_idx-1))
      ELSE IF ( INDEX(line, " LSHIFT ") /= 0 ) THEN
          op_idx = INDEX(line, " LSHIFT ")
          op%op = LSHIFT_OP
          op%src1 = TRIM(line(1:op_idx-1))
          amt_str = TRIM(line(op_idx+8:arrow_idx-1))
          READ (amt_str, *) op%amt
      ELSE IF ( INDEX(line, " RSHIFT ") /= 0 ) THEN
          op_idx = INDEX(line, " RSHIFT ")
          op%op = RSHIFT_OP
          op%src1 = TRIM(line(1:op_idx-1))
          amt_str = TRIM(line(op_idx+8:arrow_idx-1))
          READ (amt_str, *) op%amt
      ELSE
          op%op = ASSIGN_OP
          op%src1 = TRIM(line(1:arrow_idx-1))
      END IF
      entry%key = dest
      entry%val = op
      operations%entries(operations%next_free) = entry
      operations%next_free = operations%next_free + 1
    END DO

    CLOSE(file_unit)
    res = evaluate(operations, cache, "a")
  END FUNCTION process

  INTEGER FUNCTION cache_find(cache, key) RESULT(idx)
    TYPE(cache_table), INTENT(IN) :: cache
    CHARACTER(*) :: key

    INTEGER :: i

    DO i = 1, (cache%next_free - 1)
      IF ( key == cache%entries(i)%key ) THEN
        idx = i
        RETURN
      END IF
    END DO
    idx = 0
  END FUNCTION cache_find

  INTEGER FUNCTION ops_find(ops, key) RESULT(idx)
    TYPE(operation_table), INTENT(IN) :: ops
    CHARACTER(*) :: key

    INTEGER :: i

    DO i = 1, (ops%next_free - 1)
      IF ( key == ops%entries(i)%key ) THEN
        idx = i
        RETURN
      END IF
    END DO
    idx = 0
  END FUNCTION ops_find

  SUBROUTINE cache_put(cache, key, val)
    TYPE(cache_table), INTENT(INOUT) :: cache
    CHARACTER(*) :: key
    INTEGER :: val
    TYPE(cache_entry) :: entry

    INTEGER :: i

    DO i = 1, (cache%next_free - 1)
      IF ( key == cache%entries(i)%key ) THEN
        cache%entries(i)%val = val
        RETURN
      END IF
    END DO
 
    entry%key = key
    entry%val = val
    cache%entries(cache%next_free) = entry
    cache%next_free = cache%next_free + 1
  END SUBROUTINE cache_put

  RECURSIVE INTEGER FUNCTION evaluate(ops, cache, expr) RESULT(n)
    TYPE(operation_table), INTENT(IN) :: ops
    TYPE(cache_table), INTENT(INOUT) :: cache
    CHARACTER(*) :: expr
    INTEGER :: ios, idx
    INTEGER :: r, a, b, masked
    TYPE(operation) :: op

    READ(expr, *, IOSTAT=ios) r
    IF ( ios == 0 ) THEN
      n = r
    ELSE
      idx = cache_find(cache, expr)
      IF ( idx /= 0 ) THEN
        n = cache%entries(idx)%val
      ELSE
        idx = ops_find(ops, expr)
        op = ops%entries(idx)%val
        SELECT CASE (op%op)
          CASE (ASSIGN_OP)
            a = evaluate(ops, cache, op%src1)
            r = a
          CASE (NOT_OP) 
            a = evaluate(ops, cache, op%src1)
            r = NOT(a)
          CASE (AND_OP)
            a = evaluate(ops, cache, op%src1)
            b = evaluate(ops, cache, op%src2)
            r = IAND(a, b)
          CASE (OR_OP)
            a = evaluate(ops, cache, op%src1)
            b = evaluate(ops, cache, op%src2)
            r = IOR(a, b)
          CASE (LSHIFT_OP)
            a = evaluate(ops, cache, op%src1)
            r = ISHFT(a, op%amt)
          CASE (RSHIFT_OP)
            a = evaluate(ops, cache, op%src1)
            r = ISHFT(a, -op%amt)
        END SELECT
        masked = IAND(r, 65535)
        CALL cache_put(cache, expr, masked)
        n = masked
      END IF
    END IF
  END FUNCTION evaluate
END PROGRAM day07a
