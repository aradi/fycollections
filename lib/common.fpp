module fycollections_common
  implicit none
  private

  public :: getArgument

  interface getArgument
    #:for SUFFIX in ['i0', 'l0']
      module procedure getArgument_${SUFFIX}$
    #:endfor
  end interface getArgument


contains


#:def getArgument_template(SUFFIX, TYPE)

  function getArgument_${SUFFIX}$(defarg, optarg) result(arg)
    ${TYPE}$, intent(in) :: defarg
    ${TYPE}$, intent(in), optional :: optarg
    ${TYPE}$ :: arg
    
    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if
    
  end function getArgument_${SUFFIX}$
  
#:enddef getArgument_template


#:for SUFFIX, TYPE in [('i0', 'integer'), ('l0', 'logical')]
  $:getArgument_template(SUFFIX, TYPE)
#:endfor

end module fycollections_common
