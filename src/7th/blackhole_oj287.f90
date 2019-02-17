program OJ287
implicit none

real(8) xi, xf, x, dx  ! x:u (proper time)
real(8) yi(6), yf(6), T(4), to(4)
! y(1):T (observer's time)
! y(2):R (radius)
! y(3):Phi (polar angle)
! y(4):dT/du
! y(5):dR/du
! y(6):dPhi/du
! T(j)(to(j)):the obserber's time when j-th Zoukou occurs  

integer k, N, j
real(8), parameter :: pi=4.00d0*atan(1.00d0)
real(8), parameter :: G=6.67428D-11  ![m^3/s^2/kg] :gravitational constant
real(8), parameter :: c=2.99792458D8 ![m/s] :speed of light in vacuum
real(8), parameter :: Ms=1.989D30    ![kg] :mass of the sun
real(8), parameter :: r_0=2.9541D14  ![m] :distance between the big black hole
!and the small black hole at t=tau=0 (T=u=0)

real(8) r_g  ! juryokuhankei (GMb/c^2)
real(8) Mb   ! mass of the big black hole
real(8) yn(100001,6)
real(8) Alpha, deltaAlpha, theta  !deltaAlpha: kinseitenidou [rad]


Mb = 1.80D0*Ms*((1.00D1)**1.00D1)
r_g = G*Mb/c**2.00D0

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

yn(1,1:6)=yi(1:6)

do k=2,N
   x=x+dx
   call rk4(dx,yi,yf)
   yn(k,1:6)=yf(1:6)
   yi(1:6)=yf(1:6)
enddo

do j=1,4
   theta = pi/2.00D0 + pi*dble(j-1) ![rad]
   do k=1,N-1
      if ((yn(k,3)-theta)*(yn(k+1,3)-theta)<=0) then
         T(j) = ((yn(k+1,1)-yn(k,1))/(yn(k+1,3)-yn(k,3)))*(theta-yn(k,3))&
                +yn(k,1)
         exit
      endif
   enddo
enddo

do k=2,N-1
   if((yn(k,5)<=0).and.(yn(k+1,5)>=0)) then
      Alpha = yn(k,3) - ((yn(k+1,3)-yn(k,3))/(yn(k+1,5)-yn(k,5)))*yn(k,5)![rad]
      exit
   endif
enddo

to(1:4)=r_g*T(1:4)/c/365.242D0/2.40D1/3.60D3
deltaAlpha = Alpha - 2.00D0*pi

open(unit=2, file='blackhole_oj287_year.dat')
write(2, '(e10.4,1x,e10.4,1x,e10.4,1x,e10.4)')to(1:4)
write(2, '(e10.4)') deltaAlpha
close(2)

end program OJ287
