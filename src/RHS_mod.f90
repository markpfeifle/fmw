module RHS_mod
  use, intrinsic :: iso_fortran_env
  use :: Types_mod
  implicit none


  ! everything is private unless otherwise stated
  private
  public :: func
  
  contains

  function func(j, x) result (d)
    implicit none

    !integer,intent(in) :: j, x_num
    integer,intent(in) :: j
    real (kind=dp) :: d
    real (kind=dp), intent(in), dimension(:) :: x

    d = 0.0e+00_dp
  end function
  
end module RHS_mod
 




