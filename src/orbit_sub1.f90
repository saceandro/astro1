subroutine LHS(t,y,dy)
implicit none
 
real(8), intent(in) :: t, y(4)
real(8), intent(out) :: dy(4)


dy(1)=y(3)
dy(2)=y(4)
dy(3)=y(1)*y(4)**2 - 1/y(1)**2
dy(4)=-2*y(3)*y(4)/y(1)

end subroutine LHS
