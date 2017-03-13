#:include 'fytest.fypp'

#:call TEST_MODULE('IntList')
  use type_intlist
  implicit none

contains


#:call TEST('append')
  type(IntList) :: myList

  call createList(myList, [1, 2])
  @:EXPECT(size(myList) == 2)

#:endcall TEST


#:call TEST('get')
  type(IntList) :: myList

  call createList(myList, [1, 2, 3])
  @:ASSERT(size(myList) == 3)
  @:EXPECT(myList%get(1) == 1)
  @:EXPECT(myList%get(2) == 2)

#:endcall TEST


#:call TEST('find')
  type(IntList) :: myList

  call createList(myList, [-1, -2, -3])
  @:EXPECT(myList%find(-1) == 1)
  @:EXPECT(myList%find(-2) == 2)
  @:EXPECT(myList%find(-3) == 3)
  @:EXPECT(myList%find(4) == 0)

#:endcall TEST

  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Helper functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine createList(list, initValues)
    type(IntList), intent(inout) :: list
    integer, intent(in) :: initValues(:)

    integer :: ii

    call IntList_init(list)
    do ii = 1, size(initValues)
      call list%append(initValues(ii))
    end do

  end subroutine createList

#:endcall TEST_MODULE


$:SERIAL_DRIVER()
