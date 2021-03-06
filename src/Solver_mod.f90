module Solver_mod
  !use, intrinsic :: iso_fortran_env
  use iso_fortran_env
  use Types_mod
  use RHS_mod
  implicit none


  ! everything is private unless otherwise stated
  private
  public :: fd1d_heat_explicit
  
  contains

  subroutine fd1d_heat_explicit( x, t, dt, cfl, h, h_new)
    implicit none

    !integer, intent(in) :: x_num

    real (kind=dp), intent(in) :: cfl
    real (kind=dp), intent(in) :: dt
    real (kind=dp), intent(in), dimension(:)  :: h
    real (kind=dp), intent(out), dimension(:) :: h_new
    integer :: j, x_num
    real (kind=dp), intent(in) :: t
    real (kind=dp), intent(in), dimension(:) :: x
    real (kind=dp), dimension(size(h)) :: f

    x_num=size(h)

    do j = 1, x_num
      f(j) = func(j, x)
    end do

    h_new(1) = 0.0e+00_dp

    do j = 2, x_num - 1
      != stencil readOnce centered(depth=1, dim=1) :: h
      != stencil readOnce reflexive(dim=1) :: f
      h_new(j) = h(j) + dt*f(j) + cfl*(h(j-1)-2.0e+00_dp*h(j)+h(j+1))
    end do

! set the boundary conditions again
    h_new(1) = 90.0e+00_dp
    h_new(x_num) = 70.0e+00_dp
  end subroutine
  
end module Solver_mod
 




