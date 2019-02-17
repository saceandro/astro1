subroutine LHS(t,y,dy)
implicit none
 
real(8), intent(in) :: t, y(3)
real(8), intent(out) :: dy(3)
real(8), parameter :: A=10.0, B=8.0/3.0, C=28.0

dy(1)=A*(-y(1)+y(2))
dy(2)=-y(1)*y(3)+C*y(1)-y(2)
dy(3)=y(1)*y(2)-B*y(3)

end subroutine LHS


