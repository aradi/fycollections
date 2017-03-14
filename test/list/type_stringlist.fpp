#:include 'fycollections.fypp'

module type_stringlist
  use fycollections
  implicit none
  private

  $:define_list_type(&
      & TYPE_NAME='StringList',&
      & ITEM_TYPE_IN='character(*)',&
      & ITEM_TYPE_OUT='character(:)',&
      & ITEM_ATTRIBS_OUT='allocatable')

end module type_stringlist
