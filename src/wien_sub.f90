!solve x+3(exp(-x)-1)=0
!
subroutine find_root(x_ini, u, eps, x_root)
implicit none

!===definition of variables ===
real(8), intent(in) :: x_ini, u, eps
!    input variable passed from wien_main.f90
real(8), intent(out) :: x_root
!   output variable passed to wien_main.f90

real(8) x,z !variable
!    x & z are local variable used only inside this subroutine
real(8) delta !
