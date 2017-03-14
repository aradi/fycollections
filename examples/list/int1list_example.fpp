program int1list_example
  use type_int1list
  implicit none

  type(Int1List), allocatable :: myList

  ! Explicit allocation and deallocation to test finalization
  allocate(myList)
  call Int1List_init(myList)
  print *, 'Creating list [[1, 2], [2, 3, 4], [5, 6, 7, 8]]'
  call myList%append([1, 2])
  call myList%append([2, 3, 4])
  call myList%append([5, 6, 7, 8])
  print *, 'Size of the list:', size(myList)   ! Should yield 3
  print *, 'Second element:', myList%get(2)  ! Should yield 2, 3, 4
  print *, 'Position of item with value [2, 3, 4]:', myList%find([2, 3, 4])  ! Should yield 2
  print *, 'Position of item with value [1, 1]:', myList%find([1, 1])  ! Should yield 0

  print *, 'Inserting [9, 10] at position 2'
  call myList%insert(2, [9, 10])
  print *, 'List size:', size(myList)  ! 4
  print *, 'Elements 1:3:'
  print *, '1:', myList%get(1)  ! 1, 2
  print *, '2:', myList%get(2)  ! 9, 10
  print *, '3:', myList%get(3)  ! 2, 3, 4
  deallocate(myList)

end program int1list_example
