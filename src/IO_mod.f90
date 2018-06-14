module IO_mod
  use, intrinsic :: iso_fortran_env
  use :: netcdf
  use :: Types_mod
  implicit none


  ! everything is private unless otherwise stated
  private
  public :: r8mat_write, r8vec_linspace, r8vec_write
  
  contains
  
  subroutine r8mat_write(output_filename, x, t, table)
    implicit none

    real(kind=DP), dimension(:), intent(in) :: x
    real(kind=DP), dimension(:), intent(in) :: t
    
    integer :: m
    integer :: n

    integer :: j, nf_return, dimid_x, dimid_t, varid_H, varid_x, varid_t
    character (len=*), intent(in) :: output_filename
    integer :: output_unit_id
    character (len=30) :: string
    !real (kind=dp), intent(in) :: table(m, n)
    real (kind=dp), intent(in), dimension(:,:) :: table

    m = size( table(:, :), 1 )
    n = size( table(:, :), 2 )
    
    nf_return = NF90_CREATE  ( './'//output_filename, NF90_CLOBBER, output_unit_id )
    nf_return = NF90_PUT_ATT ( output_unit_id, NF90_GLOBAL, 'purpose', 'Fortran workshop' )
    nf_return = NF90_PUT_ATT ( output_unit_id, NF90_GLOBAL, 'name', 'Mark Pfeifle' )
    nf_return = NF90_PUT_ATT ( output_unit_id, NF90_GLOBAL, 'institution', 'University of Stuttgart' )
    nf_return = NF90_DEF_DIM ( output_unit_id, "x", size(x), dimid_x )
    nf_return = NF90_DEF_DIM ( output_unit_id, "t", size(t), dimid_t )
    nf_return = NF90_DEF_VAR ( output_unit_id, "x-range", NF90_DOUBLE, [dimid_x], varid_x )
    nf_return = NF90_DEF_VAR ( output_unit_id, "t-range", NF90_DOUBLE, [dimid_t], varid_t )
    nf_return = NF90_DEF_VAR ( output_unit_id, "solution", NF90_DOUBLE, [dimid_x, dimid_t], varid_H )
    nf_return = NF90_PUT_ATT ( output_unit_id, varid_x, 'units', 'metres')
    nf_return = NF90_PUT_ATT ( output_unit_id, varid_t, 'units', 'seconds')
    nf_return = NF90_PUT_ATT ( output_unit_id, varid_H, 'units', 'Celsius')
    nf_return = NF90_ENDDEF  ( output_unit_id )
    nf_return = NF90_PUT_VAR ( output_unit_id, varid_x, x(:) )
    nf_return = NF90_PUT_VAR ( output_unit_id, varid_t, t(:) )
    nf_return = NF90_PUT_VAR ( output_unit_id, varid_H, table(:,:) )
    nf_return = NF90_CLOSE   ( output_unit_id )
    
    !output_unit_id = 10
    !open (unit=output_unit_id, file=output_filename, status='replace')
    !
    !write (string, '(a1,i8,a1,i8,a1,i8,a1)') '(', m, 'g', 24, '.', 16, ')'
    !
    !do j = 1, n
    !  write (output_unit_id, string) table(1:m, j)
    !end do
    !
    !close (unit=output_unit_id)
  end subroutine

  subroutine r8vec_linspace(a_first, a_last, a)

    implicit none

    integer :: n
    real (kind=dp), intent(out), dimension(:) :: a
    real (kind=dp), intent(in) :: a_first
    real (kind=dp), intent(in) :: a_last
    integer :: i

    n=size(a)
    
    do i = 1, n
      a(i) = (real(n-i,kind=dp)*a_first+real(i-1,kind=dp)*a_last)/ &
        real(n-1, kind=dp)
    end do

  end subroutine

  subroutine r8vec_write(output_filename, x)

    implicit none

    integer :: m
    integer :: n

    integer :: j
    character (len=*), intent(in) :: output_filename
    integer :: output_unit_id
    real (kind=dp), intent(in), dimension(:) :: x

    n=size(x)
    output_unit_id = 11
    open (unit=output_unit_id, file=output_filename, status='replace')

    do j = 1, n
      write (output_unit_id, '(2x,g24.16)') x(j)
    end do

    close (unit=output_unit_id)
  end subroutine

end module IO_mod
 




