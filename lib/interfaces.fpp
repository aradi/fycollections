module fycollections_interfaces
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: copyInterface, destructInterface, isEqualInterface

  interface
    subroutine copyInterface(item1, item2)
      import :: c_ptr
      type(c_ptr), intent(in) :: item1
      type(c_ptr), intent(out) :: item2
    end subroutine copyInterface

    subroutine destructInterface(item)
      import :: c_ptr
      type(c_ptr), intent(inout) :: item
    end subroutine destructInterface
    
    function isEqualInterface(item1, item2) result(isEqual)
      import :: c_ptr
      type(c_ptr), intent(in) :: item1, item2
      logical :: isEqual
    end function isEqualInterface
  end interface

end module fycollections_interfaces
