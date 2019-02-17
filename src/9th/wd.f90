program wd
implicit none

real(8),parameter :: pi=4.00D0*atan(1.00D0) !π
real(8),parameter :: c=2.99792458D8 ![m/s]
real(8),parameter :: h=6.626068D-34 ![m^2*kg/s]
real(8),parameter :: me=9.10938D-31 ![kg]
real(8),parameter :: mu=1.66054D-27 ![kg]

real(8) A,B,rhoc,pc,xc,rs
real(8),parameter :: mmwe = 2.00D0
real(8) x,xr,r,y(2),dr
real(8) ri,rf,yi(2),yf(2)
real(8) p,rho
integer j,k






rhoc = 1.00D8
ri = 1.00D-10
rf = 1.00D2
k = 100001
dr  = (rf-ri)/dble(k-1)

A = 8.00D0*pi*mu*(me*c)**3.00D0/3.00D0/h**3.00D0

B = pi* me**4.00D0* c**5.00D0/3.00D0/h**3.00D0

xc = (rhoc/A/mmwe)**(1.00D0/3.00D0)

pc = B*(x*(2.00D0*x**2.00D0-3.00D0)*(x**2.00D0+1.00D0)**5.00D-1+3.00D0*&
    log(x+(x**2.00D0+1.00D0)**5.00D-1))

x = xc-(A*mmwe)**2.00D0*pc*xc**2.00D0*(xc**2.00D0+1.00D0)*5.00D-1 *ri**2.00D0&
      /4.80D1/B/rhoc**2.00D0

xr = -(A*mmwe)**2.00D0*pc*xc**2.00D0*(xc**2.00D0+1.00D0)*5.00D-1 *ri&
      /2.40D1/B/rhoc**2.00D0

yi(1) = 8.00D0*B*rhoc*(sqrt(1.00D0+x**2.00D0)-1.00D0)/A/mmwe/pc

yi(2) = 8.00D0*B*rhoc*x*xr/sqrt(x**2.00D0+1.00D0)/A/mmwe/pc

r = ri

open(unit=1, file='wd_tmp.dat')
do j=2,k
   r=r+dr
   call rk4(A,B,mmwe,rhoc,pc,r,dr,yi,yf)
   write(1,'(e14.7,1x,e14.7,1x,e14.7)') r,yf(1:2)

   x = sqrt((A*mmwe*pc*h/8.00D0/B/rhoc + 1.00D0)**2.00D0 - 1.00D0)

   rho = A*mmwe*X**3.00D0

   p = B*(x*(2.00D0*x**2.00D0-3.00D0)*(x**2.00D0+1.00D0)**5.00D-1+3.00D0*&
    log(x+(x**2.00D0+1.00D0)**5.00D-1))

   if ((yi(1)>=0).and.(yf(1)<=0)) then
      rs = r - yf(1)*dr/(yf(1)-yi(1)) !dimensionless radius of the sun in this model
      write(*,*) rs
      exit
   endif
   yi(1:2)=yf(1:2)
enddo

close(1)

end program wd

      
