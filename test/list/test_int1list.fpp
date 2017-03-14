#:include 'fytest.fypp'

#:call TEST_MODULE('Int1List')
  use type_int1list
  implicit none

  integer, parameter :: MAXITEMS = 10
  integer, parameter :: MAXSIZE = 10

  integer, parameter :: INT1_TABLE(MAXSIZE, MAXITEMS) = reshape([&
      & 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, &
      & 9, 2, 3, 0, 0, 0, 0, 0, 0, 0, &
      & 1, 7, 3, 4, 0, 0, 0, 0, 0, 0, &
      & 1, 3, 2, 9, 5, 0, 0, 0, 0, 0, &
      & 8, 4, 8, 8, 3, 6, 0, 0, 0, 0, &
      & 5, 5, 5, 5, 5, 5, 7, 0, 0, 0, &
      & 3, 1, 9, 2, 8, 4, 5, 8, 0, 0, &
      & 7, 6, 5, 3, 9, 4, 6, 2, 5, 0, &
      & 1, 3, 3, 1, 9, 8, 5, 3, 4, 1, &
      & 5, 0, 0, 0, 0, 0, 0, 0, 0, 0], [MAXSIZE, MAXITEMS])

  interface isEqual
    module procedure isEqual_Int1List
    module procedure isEqual_Int1
  end interface isEqual

contains


#:call TEST('append')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 1, 2, 3])
  @:ASSERT(isEqual(list, [1, 2, 1, 2, 3]))

#:endcall TEST


#:call TEST('get')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 3])
  @:EXPECT(isEqual(list%get(1), getInt1Array(1)))
  @:EXPECT(isEqual(list%get(2), getInt1Array(2)))
  @:EXPECT(isEqual(list%get(3), getInt1Array(3)))
  @:EXPECT(isEqual(list%get(-3), getInt1Array(1)))
  @:EXPECT(isEqual(list%get(-2), getInt1Array(2)))
  @:EXPECT(isEqual(list%get(-1), getInt1Array(3)))

#:endcall TEST


#:call TEST('set')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 3, 4])

  call list%set(1, getInt1Array(5))
  @:ASSERT(isEqual(list, [5, 2, 3, 4]))

  call list%set(2, getInt1Array(6))
  @:ASSERT(isEqual(list, [5, 6, 3, 4]))

  call list%set(4, getInt1Array(7))
  @:ASSERT(isEqual(list, [5, 6, 3, 7]))

  call list%set(-1, getInt1Array(8))
  @:ASSERT(isEqual(list, [5, 6, 3, 8]))
  
#:endcall TEST


#:call TEST('delete')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

  call list%delete(1)
  @:ASSERT(isEqual(list, [2, 3, 4, 5, 6, 7, 8, 9, 10]))

  call list%delete(2, 2)
  @:ASSERT(isEqual(list, [2, 5, 6, 7, 8, 9, 10]))
  
  call list%delete(7)
  @:ASSERT(isEqual(list, [2, 5, 6, 7, 8, 9]))

  call list%delete(-3)
  @:ASSERT(isEqual(list, [2, 5, 6, 8, 9]))
  
#:endcall TEST


#:call TEST('reset')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 3, 4, 5])

  call list%reset()
  @:ASSERT(isEqual(list, [integer :: ]))

  call list%append(getInt1Array(1))
  @:ASSERT(isEqual(list, [1]))
  
#:endcall TEST


#:call TEST('find')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 3])
  @:EXPECT(list%find(getInt1Array(1)) == 1)
  @:EXPECT(list%find(getInt1Array(2)) == 2)
  @:EXPECT(list%find(getInt1Array(3)) == 3)
  @:EXPECT(list%find(getInt1Array(4)) == 0)

#:endcall TEST


#:call TEST('insertItem')
  type(Int1List) :: list

  call initializeList(list, [1, 2, 3])
  @:ASSERT(size(list) == 3)

  call list%insert(2, getInt1Array(4))
  @:ASSERT(isEqual(list, [1, 4, 2, 3]))

  call list%insert(1, getInt1Array(5))
  @:ASSERT(isEqual(list, [5, 1, 4, 2, 3]))

  call list%insert(6, getInt1Array(6))
  @:ASSERT(isEqual(list, [5, 1, 4, 2, 3, 6]))

  call list%insert(-1, getInt1Array(7))
  @:ASSERT(isEqual(list, [5, 1, 4, 2, 3, 6, 7]))

  call list%insert(-4, getInt1Array(8))
  @:ASSERT(isEqual(list, [5, 1, 4, 2, 8, 3, 6, 7]))

#:endcall TEST

  
#:call TEST('insertList')
  type(Int1List) :: list1, list2

  call initializeList(list1, [1, 2, 3])
  call initializeList(list2, [4, 5, 6])

  call list1%insert(1, list2)
  @:ASSERT(isEqual(list1, [4, 5, 6, 1, 2, 3]))

#:endcall TEST
  

  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Helper functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine initializeList(list, initValues)
    type(Int1List), intent(inout) :: list
    integer, intent(in) :: initValues(:)

    integer :: ii

    call Int1List_init(list)
    do ii = 1, size(initValues)
      call list%append(getInt1Array(initValues(ii)))
    end do

  end subroutine initializeList
  

  function isEqual_Int1List(list, values) result(isEqual)
    type(Int1List), intent(in) :: list
    integer, intent(in) :: values(:)
    logical :: isEqual

    integer :: ii
    integer, allocatable :: buffer(:), reference(:)

    isEqual = .false.
    if (size(list) /= size(values)) then
      print *, "Size mismatch:", size(list), size(values)
      return
    end if
    do ii = 1, size(values)
      buffer = list%get(ii)
      reference = getInt1Array(values(ii))
      if (size(buffer) /= size(reference)) then
        print *, "Mismatch in item size:", ii, size(buffer), size(reference)
        return
      else if (any(buffer /= reference)) then
        print *, "Mismatch in item", ii
        return
      end if
    end do
    isEqual = .true.
    
  end function isEqual_Int1List


  function isEqual_Int1(val1, val2) result (isEqual)
    integer, intent(in) :: val1(:), val2(:)
    logical :: isEqual

    isEqual = .false.
    if (size(val1) /= size(val2)) then
      print *, "SIZE mismatch:", size(val1), size(val2)
      return
    else if (any(val1 /= val2)) then
      print *, "ELEMENT mismatch:"
      print *, val1
      print *, val2
      return
    end if
    isEqual = .true.

  end function isEqual_Int1


  function getInt1Array(ind) result(array)
    integer, intent(in) :: ind
    integer, allocatable :: array(:)

    array = pack(INT1_TABLE(:,ind), INT1_TABLE(:,ind) /= 0)

  end function getInt1Array
    
    

#:endcall TEST_MODULE


$:SERIAL_DRIVER()
