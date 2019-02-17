! subroutine to integrate differential equation
! by Euler method
! compute y(x) at x = xstart + dx
!
subroutine euler(xstart,ystart,dx, yend)
implicit none
!    xstart: initial x
!    dx: increment in x
!    ystart: y(xstart)
!    yend: y(xend)
!
real(8), intent(in) :: xstart, ystart, dx
real(8), intent(out) :: yend

real(8) :: Func
external Func
!_______________________________________________

yend = ystart + Func(xstart,ystart) * dx


end subroutine euler
