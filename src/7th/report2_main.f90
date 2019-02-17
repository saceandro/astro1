program OJ287
implicit none

real(8) xi, xf, x, dx  ! x:u (proper time)
real(8) yi(6), yf(6)
! y(1):T (observer's time)
! y(2):R (radius)
! y(3):Phi (polar angle)
! y(4):dT/du
! y(5):dR/du
! y(6):dPhi/du

integer k, N
real(8), parameter :: G=6.67428D-11 ![m^3/s^2/kg] :gravitational constant
real(8), parameter :: c=2.99792458D8 ![m/s] :speed of light in vacuum
real(8), parameter :: Ms=1.989D30 ![kg] :mass of the sun
real(8), parameter :: r_0=2.9541D14 ![m] :distance between the big black hole
!and the small black hole at t=tau=0 (T=u=0)

real(8) r_g  ! juryokuhankei (GMb/c^2)
real(8) Mb   ! mass of the big black hole


Mb = Ms*((1.00D1)**1.00D1)
r_g = G*Mb/c**2.00D0

open(unit=1, file='tmp_report2.dat')

N=100001

xi=0.00D0
xf=4.50D3

dx=(xf-xi)/dble(N-1)

!initial state
x=xi
yi(1)=0.00D0
yi(2)=r_0/r_g
yi(3)=0.00D0
yi(5)=0.00D0
yi(6)=3.00D-1/yi(2)
yi(4)=(((yi(2)*yi(6))**2.00D0 +1.00D0)/(1.00D0 -2.00D0/yi(2)))**5.00D-1

write(1,'(e14.7,1x,e14.7,1x,e14.7,1x,e14.7,1x,e14.7,1x,e14.7,1x,e14.7)')x,yi(1:6)

do k=2,N
   x=x+dx
   call rk4(dx,yi,yf)
   write(1,'(e14.7,1x,e14.7,1x,e14.7,1x,e14.7,1x,e14.7,1x,e14.7,1x,e14.7)')x,yf(1:6)
   yi(1:6)=yf(1:6)
enddo

close(1)
end program OJ287
