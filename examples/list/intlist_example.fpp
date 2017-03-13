program intlist_example
  use type_intlist
  implicit none

  type(IntList), allocatable :: myList

  ! Explicit allocation and deallocation to test finalization
  allocate(myList)
  call IntList_init(myList)
  print *, 'Creating list [1, 2, 3]'
  call myList%append(1)
  call myList%append(2)
  call myList%append(3)
  print *, 'Size of the list:', size(myList)   ! Should yield 3
  print *, 'Second element:', myList%get(2)  ! Should yield 2
  print *, 'Position of item with value 2:', myList%find(2)  ! Should yield 2
  print *, 'Position of item with value 5:', myList%find(5)  ! Should yield 0

  print *, 'Inserting -1 at position 2'
  call myList%insert(2, -1)
  print *, 'List size:', size(myList)  ! 4
  print *, 'Elements 1:3:', myList%get(1), myList%get(2), myList%get(3)  ! 1 -1 2
  deallocate(myList)

end program intlist_example
