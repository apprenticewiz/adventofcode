PROGRAM test_set
  USE string_set_module
  IMPLICIT NONE

  TYPE(string_set) :: positions
  INTEGER :: count

  positions = create_string_set()
  CALL positions%insert('0,0')
  CALL positions%insert('0,-1')
  CALL positions%insert('1,0')
  CALL positions%insert('0,0')
  PRINT *, positions%has('0,0')
  PRINT *, positions%has('2,2')
  PRINT *, positions%size()

END PROGRAM test_set
