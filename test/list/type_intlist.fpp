#:include 'fycollections.fypp'

module type_intlist
  use fycollections
  implicit none
  private

  $:define_list_type(&
      & TYPE_NAME='IntList',&
      & ITEM_TYPE_IN='integer',&
      & ITEM_TYPE_OUT='integer')

end module type_intlist
