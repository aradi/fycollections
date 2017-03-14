#:include 'fycollections.fypp'

module type_int1list
  use fycollections
  implicit none
  private

  $:define_list_type(&
      & TYPE_NAME='Int1List',&
      & ITEM_TYPE_IN='integer',&
      & ITEM_ATTRIBS_IN='dimension(:)',&
      & ITEM_TYPE_OUT='integer',&
      & ITEM_ATTRIBS_OUT='dimension(:), allocatable')

end module type_int1list
