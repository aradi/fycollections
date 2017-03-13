program intlist_example
  use type_intlist
  implicit none

  type(IntList), allocatable :: myList

  ! Explicit allocation and deallocation to test finalization
  allocate(myList)
  call IntList_init(myList)
  call myList%append(1)
  call myList%append(2)
  call myList%append(3)
  print *, "Size of the list:", size(myList)   ! Should yield 3
  print *, "Second element:", myList%get(2)  ! Should yield 2
  print *, "Position of item with value 2:", myList%find(2)  ! Should yield 2
  print *, "Position of item with value 5:", myList%find(5)  ! Should yield 0
  deallocate(myList)

end program intlist_example
