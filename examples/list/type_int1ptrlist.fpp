#:include 'fycollections.fypp'

module type_int1ptrlist
  use fycollections
  implicit none
  private

  $:define_list_type(&
      & TYPE_NAME='Int1PtrList',&
      & ITEM_TYPE_IN='integer',&
      & ITEM_ATTRIBS_IN='pointer, dimension(:)',&
      & ITEM_TYPE_OUT='integer',&
      & ITEM_ATTRIBS_OUT='pointer, dimension(:)',&
      & ITEM_COMPARISON=COMPARE_ASSOCIATION,&
      & ITEM_ASSIGNMENT=ASSIGN_POINTER)

end module type_int1ptrlist
