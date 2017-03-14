#:include 'fytest.fypp'

#:call TEST_MODULE('StringList')
  use type_stringlist
  implicit none

  integer, parameter :: MAXITEMS = 10
  integer, parameter :: MAXLEN = 20

  character(MAXLEN), parameter :: STRING_TABLE(MAXITEMS) = [character(MAXLEN) ::&
      & 'alma', 'korte', 'szilva', 'banan', 'marakuja', &
      & 'gorogdinnye', 'sargadinnye', 'kivi', 'narancs', 'csipkebogyo']

contains


#:call TEST('append')
  type(StringList) :: list

  call initializeList(list, [1, 3, 5])
  @:ASSERT(size(list) == 3)
  @:ASSERT(isEqual(list, [1, 3, 5]))

#:endcall TEST


#:call TEST('get')
  type(StringList) :: list

  call initializeList(list, [1, 2, 3])
  @:EXPECT(list%get(1) == trim(STRING_TABLE(1)))
  @:EXPECT(list%get(2) == trim(STRING_TABLE(2)))
  @:EXPECT(list%get(3) == trim(STRING_TABLE(3)))
  @:EXPECT(list%get(-1) == trim(STRING_TABLE(3)))
  @:EXPECT(list%get(-2) == trim(STRING_TABLE(2)))
  @:EXPECT(list%get(-3) == trim(STRING_TABLE(1)))

#:endcall TEST


#:call TEST('set')
  type(StringList) :: list

  call initializeList(list, [1, 2, 3, 4])

  call list%set(1, trim(STRING_TABLE(10)))
  @:ASSERT(isEqual(list, [10, 2, 3, 4]))

  call list%set(2, trim(STRING_TABLE(9)))
  @:ASSERT(isEqual(list, [10, 9, 3, 4]))

  call list%set(4, trim(STRING_TABLE(8)))
  @:ASSERT(isEqual(list, [10, 9, 3, 8]))

  call list%set(-1, trim(STRING_TABLE(7)))
  @:ASSERT(isEqual(list, [10, 9, 3, 7]))
  
#:endcall TEST


#:call TEST('delete')
  type(StringList) :: list

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
  type(StringList) :: list

  call initializeList(list, [1, 2, 3, 4, 5])

  call list%reset()
  @:ASSERT(isEqual(list, [integer :: ]))

  call list%append(trim(STRING_TABLE(1)))
  @:ASSERT(isEqual(list, [1]))
  
#:endcall TEST


#:call TEST('find')
  type(StringList) :: list

  call initializeList(list, [1, 2, 3])
  @:EXPECT(list%find(trim(STRING_TABLE(1))) == 1)
  @:EXPECT(list%find(trim(STRING_TABLE(2))) == 2)
  @:EXPECT(list%find(trim(STRING_TABLE(3))) == 3)
  @:EXPECT(list%find(trim(STRING_TABLE(4))) == 0)

#:endcall TEST


#:call TEST('insertItem')
  type(StringList) :: list

  call initializeList(list, [1, 2, 3])
  @:ASSERT(size(list) == 3)

  call list%insert(2, trim(STRING_TABLE(10)))
  @:ASSERT(isEqual(list, [1, 10, 2, 3]))

  call list%insert(1, trim(STRING_TABLE(9)))
  @:ASSERT(isEqual(list, [9, 1, 10, 2, 3]))

  call list%insert(6, trim(STRING_TABLE(8)))
  @:ASSERT(isEqual(list, [9, 1, 10, 2, 3, 8]))

  call list%insert(-1, trim(STRING_TABLE(7)))
  @:ASSERT(isEqual(list, [9, 1, 10, 2, 3, 8, 7]))

  call list%insert(-4, trim(STRING_TABLE(6)))
  @:ASSERT(isEqual(list, [9, 1, 10, 2, 6, 3, 8, 7]))

#:endcall TEST

  
#:call TEST('insertList')
  type(StringList) :: list1, list2

  call initializeList(list1, [1, 2, 3])
  call initializeList(list2, [10, 9, 8])

  call list1%insert(1, list2)
  @:ASSERT(isEqual(list1, [10, 9, 8, 1, 2, 3]))

#:endcall TEST

  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Helper functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine initializeList(list, initValues)
    type(StringList), intent(inout) :: list
    integer, intent(in) :: initValues(:)

    integer :: ii

    call StringList_init(list)
    do ii = 1, size(initValues)
      call list%append(trim(STRING_TABLE(initValues(ii))))
    end do

  end subroutine initializeList


  function isEqual(list, values)
    type(StringList), intent(in) :: list
    integer, intent(in) :: values(:)
    logical :: isEqual

    integer :: ii
    character(:), allocatable :: item

    isEqual = .false.
    if (size(list) /= size(values)) then
      return
    end if
    do ii = 1, size(values)
      !print *, 'CHECKING:', ii, list%get(ii), values(ii)
      item = list%get(ii)
      if (len(item) /= len_trim(STRING_TABLE(values(ii)))) then
        return
      else if (item /= trim(STRING_TABLE(values(ii)))) then
        return
      end if
    end do
    isEqual = .true.

  end function isEqual
    

#:endcall TEST_MODULE


$:SERIAL_DRIVER()
