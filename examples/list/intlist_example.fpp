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
  print *, myList%get(2)  ! Should yield 2
  print *, myList%find(2)  ! Should yield 2
  print *, myList%find(5)  ! Should yield 0
  deallocate(myList)

end program intlist_example
