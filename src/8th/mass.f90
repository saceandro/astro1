program mass
implicit none

real(8) x,dx,yi(2),yf(2)
real(8) xi, xf, N, r, M, lamdar, lo
integer j,k
real(8), parameter :: pi=4.00D0*atan(1.00D0)

N=3.00D0
xi = 1.00D-10
xf = 1.00D1
k = 100001
dx = (xf-xi)/dble(k-1)

x = xi
yi(1) = 1 - xi**2.00D0/6.00D0 + N*xi**4.00D0/1.20D2
yi(2) = - xi/3.00D0 - N*xi**3.00D0/3.00D1


open(unit=1, file='mass_tmp.dat')
write(1,'(e14.7,1x,e14.7,1x,e14.7)') x,yi(1:2)

do j=2,k
   x=x+dx
   call rk4(N,x,dx,yi,yf)
   write(1,'(e14.7,1x,e14.7,1x,e14.7)') x,yf(1:2)
   if ((yi(1)>=0).and.(yf(1)<=0)) then
      r = x - yf(1)*dx/(yf(1)-yi(1))
      lamdar = yf(2) - yf(1)*(yf(2)-yi(2))/(yf(1)-yi(1))
      M = -r**2.00D0 * lamdar
      lo = M/4/pi/r**3.0
      write(1,'(e14.7)') r, lamdar, M, lo
      exit
   endif
   yi(1:2) = yf(1:2)
enddo



end program mass





   


   



   

