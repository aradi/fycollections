module fycollections_ptrlist
  use, intrinsic :: iso_c_binding
  use fycollections_interfaces
  implicit none
  private

  public :: PtrList, PtrList_init, size

  integer, parameter :: LIST_MIN_SIZE = 4
  
  type :: PtrList
    private
    type(c_ptr), allocatable :: items(:)
    integer :: nItems = 0
    integer :: minSize = LIST_MIN_SIZE
    procedure(copyInterface), nopass, pointer :: copy
    procedure(destructInterface), nopass, pointer :: destruct
    procedure(isEqualInterface), nopass, pointer :: isEqual
  contains
    procedure :: getSize
    procedure :: append
    procedure :: get
    procedure :: find
    final :: finalize
    procedure, private :: resizeStorage
  end type PtrList


  interface size
    module procedure PtrList_size
  end interface size

contains

  subroutine PtrList_init(this, copy, destruct, isEqual)
    type(PtrList), intent(out) :: this
    procedure(copyInterface), pointer, intent(in) :: copy
    procedure(destructInterface), pointer, intent(in) :: destruct
    procedure(isEqualInterface), pointer, intent(in) :: isEqual

    allocate(this%items(LIST_MIN_SIZE))
    this%nItems = 0
    this%copy => copy
    this%destruct => destruct
    this%isEqual => isEqual

  end subroutine PtrList_init


  function PtrList_size(this) result(mySize)
    type(PtrList), intent(in) :: this
    integer :: mySize

    mySize = this%getSize()

  end function PtrList_size


  function getSize(this) result(mySize)
    class(PtrList), intent(in) :: this
    integer :: mySize

    mySize = this%nItems

  end function getSize


  subroutine append(this, item)
    class(PtrList), intent(inout) :: this
    type(c_ptr), intent(in) :: item

    integer, pointer :: pItem

    call this%resizeStorage(this%nItems + 1)
    this%nItems = this%nItems + 1
    this%items(this%nItems) = item
    call c_f_pointer(this%items(this%nItems), pItem)

  end subroutine append


  function get(this, ind) result(item)
    class(PtrList), intent(in) :: this
    integer, intent(in) :: ind
    type(c_ptr) :: item

    item = this%items(ind)

  end function get


  function find(this, item) result(ind)
    class(PtrList), intent(in) :: this
    type(c_ptr), intent(in) :: item
    integer :: ind

    logical :: found

    found = .false.
    do ind = 1, this%nItems
      found = this%isEqual(this%items(ind), item)
      if (found) then
        exit
      end if
    end do
    if (.not. found) then
      ind = 0
    end if

  end function find


  subroutine finalize(this)
    type(PtrList), intent(inout) :: this

    integer :: ii

    do ii = 1, this%nItems
      call this%destruct(this%items(ii))
    end do

  end subroutine finalize
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Private routines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine resizeStorage(this, newSize)
    class(PtrList), intent(inout) :: this
    integer, intent(in) :: newSize

    type(c_ptr), allocatable :: items(:)
    integer :: nAlloc, ii

    nAlloc = getStorageSize(this%minSize, size(this%items), newSize)
    if (nAlloc /= size(this%items)) then
      allocate(items(nAlloc))
      do ii = 1, this%nItems
        items(ii) = this%items(ii)
      end do
      call move_alloc(items, this%items)
    end if
    
  end subroutine resizeStorage

  
  function getStorageSize(minSize, oldSize, newSize) result(arraySize)
    integer, intent(in) :: minSize, oldSize, newSize
    integer :: arraySize

    integer :: overAlloc
    
    if (oldSize >= newSize .and. oldSize <= 2 * newSize) then
      arraySize = oldSize
    else
      overAlloc = newSize / 8
      if (newSize < 9) then
        overAlloc = overAlloc + 3
      else
        overAlloc = overAlloc + 6
      end if
      arraySize = newSize + overAlloc
      if (arraySize < minSize) then
        arraySize = max(oldSize, minSize)
      end if
    end if

  end function getStorageSize
    
end module fycollections_ptrlist
