#:include 'fytest.fypp'

#:call TEST_MODULE('IntList')
  use type_intlist
  implicit none

contains


#:call TEST('append')
  type(IntList) :: list

  call initializeList(list, [1, 2])
  ! Note: initialize list uses append
  @:ASSERT(isEqual(list, [1, 2]))

#:endcall TEST


#:call TEST('get')
  type(IntList) :: list

  call initializeList(list, [1, 2, 3])
  @:EXPECT(list%get(1) == 1)
  @:EXPECT(list%get(2) == 2)
  @:EXPECT(list%get(3) == 3)
  @:EXPECT(list%get(-1) == 3)
  @:EXPECT(list%get(-2) == 2)
  @:EXPECT(list%get(-3) == 1)

#:endcall TEST


#:call TEST('set')
  type(IntList) :: list

  call initializeList(list, [1, 2, 3, 4])

  call list%set(1, -1)
  @:ASSERT(isEqual(list, [-1, 2, 3, 4]))

  call list%set(2, -2)
  @:ASSERT(isEqual(list, [-1, -2, 3, 4]))

  call list%set(4, -4)
  @:ASSERT(isEqual(list, [-1, -2, 3, -4]))

  call list%set(-1, 44)
  @:ASSERT(isEqual(list, [-1, -2, 3, 44]))
  
#:endcall TEST


#:call TEST('delete')
  type(IntList) :: list

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
  type(IntList) :: list

  call initializeList(list, [1, 2, 3, 4, 5])

  call list%reset()
  @:ASSERT(isEqual(list, [integer :: ]))

  call list%append(1)
  @:ASSERT(isEqual(list, [1]))
  
#:endcall TEST


#:call TEST('find')
  type(IntList) :: list

  call initializeList(list, [-1, -2, -3])
  @:EXPECT(list%find(-1) == 1)
  @:EXPECT(list%find(-2) == 2)
  @:EXPECT(list%find(-3) == 3)
  @:EXPECT(list%find(4) == 0)

#:endcall TEST


#:call TEST('insertItem')
  type(IntList) :: list

  call initializeList(list, [1, 2, 3])
  @:ASSERT(size(list) == 3)

  call list%insert(2, -1)
  @:ASSERT(isEqual(list, [1, -1, 2, 3]))

  call list%insert(1, -11)
  @:ASSERT(isEqual(list, [-11, 1, -1, 2, 3]))

  call list%insert(6, -3)
  @:ASSERT(isEqual(list, [-11, 1, -1, 2, 3, -3]))

  call list%insert(-1, 33)
  @:ASSERT(isEqual(list, [-11, 1, -1, 2, 3, -3, 33]))

  call list%insert(-4, -2)
  @:ASSERT(isEqual(list, [-11, 1, -1, 2, -2, 3, -3, 33]))

#:endcall TEST

  
#:call TEST('insertList')
  type(IntList) :: list1, list2

  call initializeList(list1, [1, 2, 3])
  call initializeList(list2, [-1, -2, -3])

  call list1%insert(1, list2)
  @:ASSERT(isEqual(list1, [-1, -2, -3, 1, 2, 3]))

#:endcall TEST
  

  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Helper functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine initializeList(list, initValues)
    type(IntList), intent(inout) :: list
    integer, intent(in) :: initValues(:)

    integer :: ii

    call IntList_init(list)
    do ii = 1, size(initValues)
      call list%append(initValues(ii))
    end do

  end subroutine initializeList


  function isEqual(list, values)
    type(IntList), intent(in) :: list
    integer, intent(in) :: values(:)
    logical :: isEqual

    integer :: ii

    isEqual = .false.
    if (size(list) /= size(values)) then
      return
    end if
    do ii = 1, size(values)
      !print *, 'CHECKING:', ii, list%get(ii), values(ii)
      if (list%get(ii) /= values(ii)) then
        return
      end if
    end do
    isEqual = .true.

  end function isEqual
    

#:endcall TEST_MODULE


$:SERIAL_DRIVER()
