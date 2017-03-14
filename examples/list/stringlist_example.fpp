program stringlist_example
  use type_stringlist
  implicit none

  type(StringList), allocatable :: myList

  ! Explicit allocation and deallocation to test finalization
  allocate(myList)
  call StringList_init(myList)
  print *, 'Creating list ["one", "two", "three"]'
  call myList%append('one')
  call myList%append('two')
  call myList%append('three')
  print *, 'Size of the list:', size(myList)   ! Should yield 3
  print *, 'Second element:', myList%get(2)  ! Should yield "two"
  print *, 'Position of item with value "two":', myList%find('two')  ! Should yield 2
  print *, 'Position of item with value "four":', myList%find('four')  ! Should yield 0

  print *, 'Inserting "five" at position 2'
  call myList%insert(2, "five")
  print *, 'List size:', size(myList)  ! 4
  print *, 'Elements 1:3:'
  print *, '1:', myList%get(1)  ! "one"
  print *, '2:', myList%get(2)  ! "five"
  print *, '3:', myList%get(3)  ! "two"
  deallocate(myList)

end program stringlist_example
