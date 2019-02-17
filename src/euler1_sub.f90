!subroutine to integrate differential eq.
!by Euler method
!compute y(x) at x = xstart + dx
!
subroutine euler(xstart, ystart, dx, yend)
implicit none

real(8), intent(in) :: xstart, ystart, dx
real(8), intent(out) :: yend

real(8) :: f
external f

!_________________________________________________

yend= ystart + f(xstart,ystart) * dx

end subroutine euler
