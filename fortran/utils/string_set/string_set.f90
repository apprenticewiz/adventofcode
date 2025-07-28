MODULE string_set_module
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: string_set, create_string_set

  TYPE :: bst_node
    CHARACTER(LEN=16) :: key
    TYPE(bst_node), POINTER :: left => NULL()
    TYPE(bst_node), POINTER :: right => NULL() 
  END TYPE bst_node

  TYPE :: string_set
    PRIVATE
    TYPE(bst_node), POINTER :: root => NULL()
  CONTAINS
    PROCEDURE :: insert => string_set_insert
    PROCEDURE :: has => string_set_has
    PROCEDURE :: size => string_set_size
  END TYPE string_set

CONTAINS

  FUNCTION create_string_set() RESULT(set)
    TYPE(string_set) :: set
  END FUNCTION create_string_set

  RECURSIVE SUBROUTINE string_set_insert(self, key)
    CLASS(string_set), INTENT(INOUT) :: self
    CHARACTER(LEN=16), INTENT(IN) :: key
    CALL insert_node(self%root, key)
  END SUBROUTINE string_set_insert

  RECURSIVE SUBROUTINE insert_node(node, key)
    TYPE(bst_node), POINTER :: node
    CHARACTER(LEN=16), INTENT(IN) :: key
    IF ( .NOT. ASSOCIATED(node) ) THEN
      ALLOCATE(node)
      node%key = key
    ELSE IF ( key < node % key ) THEN
      CALL insert_node(node%left, key)
    ELSE IF ( key > node % key ) THEN
      CALL insert_node(node%right, key)
    END IF
  END SUBROUTINE insert_node

  RECURSIVE FUNCTION string_set_has(self, key) RESULT(found)
    CLASS(string_set), INTENT(IN) :: self
    CHARACTER(LEN=16), INTENT(IN) :: key 
    LOGICAL :: found
    found = search_node(self%root, key)
  END FUNCTION string_set_has

  RECURSIVE FUNCTION search_node(node, key) RESULT(found)
    TYPE(bst_node), POINTER :: node
    CHARACTER(LEN=16), INTENT(IN) :: key
    LOGICAL :: found
    IF ( .NOT. ASSOCIATED(node) ) THEN
      found = .FALSE.
    ELSE IF ( key == node%key ) THEN
      found = .TRUE.
    ELSE IF ( key < node%key ) THEN
      found = search_node(node%left, key)
    ELSE
      found = search_node(node%right, key)
    END IF
  END FUNCTION search_node

  RECURSIVE FUNCTION string_set_size(self) RESULT(count)
    CLASS(string_set), INTENT(IN) :: self
    INTEGER :: count
    count = count_nodes(self%root)
  END FUNCTION string_set_size

  RECURSIVE FUNCTION count_nodes(node) RESULT(count)
    TYPE(bst_node), POINTER :: node
    INTEGER :: count
    IF ( .NOT. ASSOCIATED(node) ) THEN
      count = 0
    ELSE
      count = 1 + count_nodes(node%left) + count_nodes(node%right)
    END IF
  END FUNCTION count_nodes

END MODULE string_set_module
