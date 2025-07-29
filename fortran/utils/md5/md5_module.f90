MODULE md5_module
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTERFACE
    SUBROUTINE md5_digest(input, digest) BIND(C, NAME="md5_digest")
      IMPORT :: C_CHAR
      CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: input
      CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: digest
    END SUBROUTINE md5_digest
  END INTERFACE

CONTAINS

 FUNCTION digest(input) RESULT(hex_digest)
   CHARACTER(LEN=*), INTENT(IN) :: input
   CHARACTER(LEN=32) :: hex_digest

   CHARACTER(KIND=C_CHAR), ALLOCATABLE :: c_input(:)
   CHARACTER(KIND=C_CHAR), ALLOCATABLE :: c_output(:)
   INTEGER :: i, len

   len = LEN_TRIM(input)

   ALLOCATE(c_input(0:len))
   DO i = 1, len
     c_input(i - 1) = input(i:i)
   END DO
   c_input(len) = C_NULL_CHAR

   ALLOCATE(c_output(0:32))
   c_output(32) = C_NULL_CHAR

   CALL md5_digest(c_input, c_output)

   DO i = 1, 32
     hex_digest(i:i) = c_output(i - 1)
   END DO
 END FUNCTION digest

END MODULE md5_module
