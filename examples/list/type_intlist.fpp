#:include 'fycollections.fypp'

module type_intlist
  use fycollections
  implicit none
  private

  $:define_list_type(TYPENAME='IntList', ITEMTYPE='integer')

end module type_intlist
