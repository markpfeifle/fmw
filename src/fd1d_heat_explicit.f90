!> <Your name>, <Your affiliation>
!> Solves the one dimensional heat diffusion equation
!> \( \frac{\partial H}{\partial t}
!> - \kappa\frac{\partial^{2} H}{\partial x^{2}} = f(x) \)

program fd1d_heat_explicit_prb
  use :: types_mod, only: DP
  use :: RHS_mod
  use :: CFL_mod
  use :: IO_mod
  use :: Solver_mod

  implicit none

  integer, parameter :: t_num=201
  integer, parameter :: x_num=21

  real (kind=dp) :: cfl
  real (kind=dp) :: dt
!  real (kind=dp) :: h(x_num)
  real (kind=dp), allocatable :: h(:)
!  real (kind=dp) :: h_new(x_num)
  real (kind=dp), allocatable :: h_new(:)
! the "matrix" stores all x-values for all t-values
! remember Fortran is column major, meaning that rows are contiguous
!  real (kind=dp) :: hmat(x_num, t_num)
  real (kind=dp), allocatable :: hmat(:,:)
  integer :: i
  integer :: j
  integer :: i_all_stat
  real (kind=dp) :: k
  
  character(len=:),allocatable :: msg

!  real (kind=dp) :: t(t_num)
  real (kind=dp), allocatable :: t(:)
  real (kind=dp) :: t_max
  real (kind=dp) :: t_min
!  real (kind=dp) :: x(x_num)
  real (kind=dp), allocatable :: x(:)
  real (kind=dp) :: x_max
  real (kind=dp) :: x_min

  write (*, '(a)') ' '
  write (*, '(a)') 'FD1D_HEAT_EXPLICIT_PRB:'
  write (*, '(a)') '  FORTRAN77 version.'
  write (*, '(a)') '  Test the FD1D_HEAT_EXPLICIT library.'

  write (*, '(a)') ' '
  write (*, '(a)') 'FD1D_HEAT_EXPLICIT_PRB:'
  write (*, '(a)') '  Normal end of execution.'
  write (*, '(a)') ' '

  write (*, '(a)') ' '
  write (*, '(a)') 'FD1D_HEAT_EXPLICIT_TEST01:'
  write (*, '(a)') '  Compute an approximate solution to the time-dependent'
  write (*, '(a)') '  one dimensional heat equation:'
  write (*, '(a)') ' '
  write (*, '(a)') '    dH/dt - K * d2H/dx2 = f(x,t)'
  write (*, '(a)') ' '
  write (*, '(a)') '  Run a simple test case.'

! heat coefficient
  k = 0.002e+00_dp

! the x-range values
  x_min = 0.0e+00_dp
  x_max = 1.0e+00_dp
! x_num is the number of intervals in the x-direction

  allocate( h(1:x_num),             stat=i_all_stat, errmsg=msg )
  if (i_all_stat /= 0) then
    write(*,*) trim(msg)
  endif
  allocate( h_new(1:x_num),         stat=i_all_stat, errmsg=msg )
  if (i_all_stat /= 0) then
    write(*,*) trim(msg)
  endif
  allocate( hmat(1:x_num, 1:t_num), stat=i_all_stat, errmsg=msg )
  if (i_all_stat /= 0) then
    write(*,*) trim(msg)
  endif
  allocate( t(1:t_num),             stat=i_all_stat, errmsg=msg )
  if (i_all_stat /= 0) then
    write(*,*) trim(msg)
  endif
  allocate( x(1:x_num),             stat=i_all_stat, errmsg=msg )
  if (i_all_stat /= 0) then
    write(*,*) trim(msg)
  endif
  
  call r8vec_linspace( x_min, x_max, x)

! the t-range values. integrate from t_min to t_max
  t_min = 0.0e+00_dp
  t_max = 80.0e+00_dp

! t_num is the number of intervals in the t-direction
  dt = (t_max-t_min)/real(t_num-1, kind=dp)
  call r8vec_linspace( t_min, t_max, t)

! get the CFL coefficient
  call fd1d_heat_explicit_cfl(k, t_num, t_min, t_max, x_num, x_min, x_max, &
    cfl)

  if (0.5e+00_dp<=cfl) then
    write (*, '(a)') ' '
    write (*, '(a)') 'FD1D_HEAT_EXPLICIT_CFL - Fatal error!'
    write (*, '(a)') '  CFL condition failed.'
    write (*, '(a)') '  0.5 <= K * dT / dX / dX = CFL.'
    stop
  end if

! set the initial condition
  do j = 1, x_num
    h(j) = 50.0e+00_dp
  end do

! set the bounday condition
  h(1) = 90.0e+00_dp
  h(x_num) = 70.0e+00_dp

! initialise the matrix to the initial condition
  do i = 1, x_num
    hmat(i, 1) = h(i)
  end do

! the main time integration loop 
  do j = 2, t_num
    call fd1d_heat_explicit( x, t(j-1), dt, cfl, h, h_new )

    do i = 1, x_num
      hmat(i, j) = h_new(i)
      h(i) = h_new(i)
    end do
  end do

!! write data to files
  call r8mat_write('h_test01.txt', hmat)
  call r8vec_write('t_test01.txt', t)
  call r8vec_write('x_test01.txt', x)

  deallocate( h )
  deallocate( h_new )
  deallocate( hmat )
  deallocate( t )
  deallocate( x )
  
end program
