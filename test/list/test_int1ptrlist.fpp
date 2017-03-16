#:include 'fytest.fypp'

#:call TEST_MODULE('Int1PtrList')
  use type_int1ptrlist
  implicit none

  integer, parameter :: MAXITEMS = 10
  integer, parameter :: ITEMSIZE = 2

  type, extends(TestCase) :: Int1PtrListTest
    integer, pointer :: ptrTable(:,:)
  contains
    procedure :: setUp
    procedure :: tearDown
    procedure :: initializeList
    procedure :: getInt1Ptr
    procedure :: isEqual
  end type Int1PtrListTest

contains

  subroutine setUp(this)
    class(Int1PtrListTest), intent(inout) :: this

    allocate(this%ptrTable(ITEMSIZE, MAXITEMS))
    this%ptrTable(:,:) = -1

  end subroutine setUp


  subroutine tearDown(this)
    class(Int1PtrListTest), intent(inout) :: this

    deallocate(this%ptrTable)

  end subroutine tearDown


#:call TEST_F('append', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  call this%initializeList(list, [1, 2, 1, 2, 3])
  @:ASSERT(this%isEqual(list, [1, 2, 1, 2, 3]))

#:endcall TEST_F


#:call TEST_F('get', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  integer, pointer :: act(:), ref(:)

  call this%initializeList(list, [1, 2, 3])
  act => list%get(1)
  ref => this%getInt1Ptr(1)
  @:EXPECT(associated(act, ref))
  act => list%get(2)
  ref => this%getInt1Ptr(2)
  @:EXPECT(associated(act, ref))
  act => list%get(3)
  ref => this%getInt1Ptr(3)
  @:EXPECT(associated(act, ref))
  act => list%get(-3)
  ref => this%getInt1Ptr(1)
  @:EXPECT(associated(act, ref))
  act => list%get(-2)
  ref => this%getInt1Ptr(2)
  @:EXPECT(associated(act, ref))
  act => list%get(-1)
  ref => this%getInt1Ptr(3)
  @:EXPECT(associated(act, ref))

#:endcall TEST_F


#:call TEST_F('set', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  call this%initializeList(list, [1, 2, 3, 4])

  call list%set(1, this%getInt1Ptr(5))
  @:ASSERT(this%isEqual(list, [5, 2, 3, 4]))

  call list%set(2, this%getInt1Ptr(6))
  @:ASSERT(this%isEqual(list, [5, 6, 3, 4]))

  call list%set(4, this%getInt1Ptr(7))
  @:ASSERT(this%isEqual(list, [5, 6, 3, 7]))

  call list%set(-1, this%getInt1Ptr(8))
  @:ASSERT(this%isEqual(list, [5, 6, 3, 8]))
  
#:endcall TEST_F


#:call TEST_F('delete', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  call this%initializeList(list, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

  call list%delete(1)
  @:ASSERT(this%isEqual(list, [2, 3, 4, 5, 6, 7, 8, 9, 10]))

  call list%delete(2, 2)
  @:ASSERT(this%isEqual(list, [2, 5, 6, 7, 8, 9, 10]))
  
  call list%delete(7)
  @:ASSERT(this%isEqual(list, [2, 5, 6, 7, 8, 9]))

  call list%delete(-3)
  @:ASSERT(this%isEqual(list, [2, 5, 6, 8, 9]))
  
#:endcall TEST_F


#:call TEST_F('reset', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  call this%initializeList(list, [1, 2, 3, 4, 5])

  call list%reset()
  @:ASSERT(this%isEqual(list, [integer :: ]))

  call list%append(this%getInt1Ptr(1))
  @:ASSERT(this%isEqual(list, [1]))
  
#:endcall TEST_F


#:call TEST_F('find', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  call this%initializeList(list, [1, 2, 3])
  @:EXPECT(list%find(this%getInt1Ptr(1)) == 1)
  @:EXPECT(list%find(this%getInt1Ptr(2)) == 2)
  @:EXPECT(list%find(this%getInt1Ptr(3)) == 3)
  @:EXPECT(list%find(this%getInt1Ptr(4)) == 0)

#:endcall TEST_F


#:call TEST_F('insertItem', 'Int1PtrListTest')
  type(Int1PtrList) :: list

  call this%initializeList(list, [1, 2, 3])
  @:ASSERT(size(list) == 3)

  call list%insert(2, this%getInt1Ptr(4))
  @:ASSERT(this%isEqual(list, [1, 4, 2, 3]))

  call list%insert(1, this%getInt1Ptr(5))
  @:ASSERT(this%isEqual(list, [5, 1, 4, 2, 3]))

  call list%insert(6, this%getInt1Ptr(6))
  @:ASSERT(this%isEqual(list, [5, 1, 4, 2, 3, 6]))

  call list%insert(-1, this%getInt1Ptr(7))
  @:ASSERT(this%isEqual(list, [5, 1, 4, 2, 3, 6, 7]))

  call list%insert(-4, this%getInt1Ptr(8))
  @:ASSERT(this%isEqual(list, [5, 1, 4, 2, 8, 3, 6, 7]))

#:endcall TEST_F

  
#:call TEST_F('insertList', 'Int1PtrListTest')
  type(Int1PtrList) :: list1, list2

  call this%initializeList(list1, [1, 2, 3])
  call this%initializeList(list2, [4, 5, 6])

  call list1%insert(1, list2)
  @:ASSERT(this%isEqual(list1, [4, 5, 6, 1, 2, 3]))

#:endcall TEST_F
  

  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Helper functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine initializeList(this, list, initValues)
    class(Int1PtrListTest), intent(in) :: this
    type(Int1PtrList), intent(inout) :: list
    integer, intent(in) :: initValues(:)

    integer :: ii
    integer, pointer :: ptr(:)

    call Int1PtrList_init(list)
    do ii = 1, size(initValues)
      ptr => this%ptrTable(:, initValues(ii))
      call list%append(ptr)
    end do

  end subroutine initializeList


  function getInt1Ptr(this, ind) result(ptr)
    class(Int1PtrListTest), intent(in) :: this
    integer, intent(in) :: ind
    integer, pointer :: ptr(:)

    ptr => this%ptrTable(:,ind)

  end function getInt1Ptr
  

  function isEqual(this, list, values)
    class(Int1PtrListTest), intent(in) :: this
    type(Int1PtrList), intent(in) :: list
    integer, intent(in) :: values(:)
    logical :: isEqual

    integer :: ii
    integer, pointer :: buffer(:), reference(:)

    isEqual = .false.
    if (size(list) /= size(values)) then
      print *, "Size mismatch:", size(list), size(values)
      return
    end if
    do ii = 1, size(values)
      buffer => list%get(ii)
      reference => this%getInt1Ptr(values(ii))
      if (.not. associated(buffer, reference)) then
        print *, "Mismatching targets"
        return
      end if
    end do
    isEqual = .true.
    
  end function isEqual


#:endcall TEST_MODULE


$:SERIAL_DRIVER()
