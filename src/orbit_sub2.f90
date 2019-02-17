subroutine euler_sys(t,dt,yi,yf)
implicit none

real(8), intent(in) :: t,dt,yi(4)
real(8), intent(out) :: yf(4)
real(8) dy(4)
integer j


call LHS(t,yi,dy)

do j=1,4
   yf(j)=yi(j) + dy(j)*dt
enddo

end subroutine euler_sys



