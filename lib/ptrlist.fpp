module fycollections_ptrlist
  use, intrinsic :: iso_c_binding
  use fycollections_common
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
    procedure :: reset
    procedure :: getSize
    procedure :: append
    procedure :: insertItem
    procedure :: insertList
    generic :: insert => insertItem, insertList
    procedure :: get
    procedure :: set
    procedure :: delete
    procedure :: find
    final :: finalize
    procedure, private :: resizeStorage
  end type PtrList


  interface size
    module procedure PtrList_size
  end interface size

contains

  subroutine PtrList_init(this, copy, destruct, isEqual, minSize)
    type(PtrList), intent(out) :: this
    procedure(copyInterface), pointer, intent(in) :: copy
    procedure(destructInterface), pointer, intent(in) :: destruct
    procedure(isEqualInterface), pointer, intent(in) :: isEqual
    integer, intent(in), optional :: minSize

    this%copy => copy
    this%destruct => destruct
    this%isEqual => isEqual
    call this%reset(minSize)

  end subroutine PtrList_init


  subroutine reset(this, minSize)
    class(PtrList), intent(inout) :: this
    integer, intent(in), optional :: minSize

    integer :: ii

    if (present(minSize)) then
      this%minSize = max(minSize, LIST_MIN_SIZE)
    else
      this%minSize = LIST_MIN_SIZE
    end if
    if (allocated(this%items)) then
      do ii = 1, this%nItems
        call this%destruct(this%items(ii))
      end do
      deallocate(this%items)
    end if
    allocate(this%items(this%minSize))
    this%nItems = 0

  end subroutine reset


  function getSize(this) result(mySize)
    class(PtrList), intent(in) :: this
    integer :: mySize

    mySize = this%nItems

  end function getSize


  subroutine append(this, item)
    class(PtrList), intent(inout) :: this
    type(c_ptr), intent(in) :: item

    call this%resizeStorage(this%nItems + 1)
    this%nItems = this%nItems + 1
    this%items(this%nItems) = item

  end subroutine append


  subroutine insertItem(this, ind, item)
    class(PtrList), intent(inout) :: this
    integer, intent(in) :: ind
    type(c_ptr), intent(in) :: item

    integer :: ind0

    ind0 = foldIndex(ind, this%nItems + 1)
    call this%resizeStorage(this%nItems + 1)
    if (ind0 <= this%nItems) then
      this%items(ind0 + 1 : this%nItems + 1) = this%items(ind0 : this%nItems)
    end if
    this%items(ind0) = item
    this%nItems = this%nItems + 1

  end subroutine insertItem


  subroutine insertList(this, ind, list)
    class(PtrList), intent(inout) :: this
    integer, intent(in) :: ind
    type(PtrList), intent(in) :: list

    integer :: ind0
    integer :: newSize, pastedSize, toBeMoved
    integer :: ii

    ind0 = foldIndex(ind, this%nItems + 1)
    pastedSize = size(list)
    newSize = this%nItems + pastedSize
    call this%resizeStorage(newSize)
    toBeMoved = this%nItems + 1 - ind0
    if (toBeMoved > 0) then
      this%items(newSize - toBeMoved + 1 : newSize) = this%items(ind0 : ind0 + toBeMoved - 1)
    end if
    do ii = 1, pastedSize
      call this%copy(list%items(ii), this%items(ind0 + ii - 1))
    end do
    this%nItems = newSize

  end subroutine insertList


  function get(this, ind) result(item)
    class(PtrList), intent(in) :: this
    integer, intent(in) :: ind
    type(c_ptr) :: item

    integer :: ind0

    ind0 = foldIndex(ind, this%nItems)
    item = this%items(ind0)

  end function get


  subroutine set(this, ind, item)
    class(PtrList), intent(inout) :: this
    integer, intent(in) :: ind
    type(c_ptr), intent(in) :: item

    integer :: ind0

    ind0 = foldIndex(ind, this%nItems)
    call this%destruct(this%items(ind0))
    this%items(ind0) = item

  end subroutine set


  subroutine delete(this, ind, items)
    class(PtrList), intent(inout) :: this
    integer, intent(in) :: ind
    integer, intent(in), optional :: items

    integer :: ind0, del
    integer :: ii

    ind0 = foldIndex(ind, this%nItems)
    del = getArgument(1, items)
    do ii = ind0, ind0 + del - 1
      call this%destruct(this%items(ii))
    end do
    this%items(ind0 : this%nItems - del) = this%items(ind0 + del : this%nItems)
    this%nItems = this%nItems - del
    call this%resizeStorage(this%nItems)

  end subroutine delete


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
    

  function PtrList_size(this) result(mySize)
    type(PtrList), intent(in) :: this
    integer :: mySize

    mySize = this%getSize()

  end function PtrList_size



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


  function foldIndex(ind, nItems) result(folded)
    integer, intent(in) :: ind, nItems
    integer :: folded

    if (ind > 0) then
      folded = modulo(ind - 1, nItems) + 1
    else if (ind < 0) then
      folded = modulo(ind, nItems) + 1
    else
      stop 'Invalid index value'
    end if
    
  end function foldIndex

    
end module fycollections_ptrlist
