!function of subprogram
!f(x)=x-cos(u*x^2)
!input u
!output x0
!
function choetsu_cos(u) result(x0)
implicit none

real(8), intent(in) :: u !input variable
real(8) x0 !output variable
real(8) f !function f=x-cos(u*x^2)
real(8) df !derivative of f i.e. df=1+2uxcos(u*x^2)
real(8) delta !difference between x and real solution of f(x)=0
real(8) epsilon !allowed error
real(8) x !variable
real(8) z !variable
real(8) a !initial value of x

a=0
epsilon=1.00D-10
x=a

do
z=x
f=z-cos(u*z**2)
df=1+2*u*z*cos(u*z**2)
delta=-f/df
x=z+delta
if (abs(delta) < epsilon) exit
enddo

   x0=x

end function choetsu_cos




