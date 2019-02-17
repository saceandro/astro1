program density
implicit none

real(8) x,dx,yi(2),yf(2),z(2,100001)
real(8) xi, xf, N, rs
integer j,k,l,m

N = 3.50D0
xi = 1.00D-3
xf = 1.00D1
k = 100001
dx = (xf-xi)/dble(k-1)

x = xi
yi(1) = 1 - xi**2.00D0/6.00D0 + N*xi**4.00D0/1.20D2
yi(2) = - xi/3.00D0 - N*xi**3.00D0/3.00D1

z(1,1) = xi
z(2,1) = (yi(1))**N


open(unit=1, file='densitydtb_3and5_tmp.dat')

do j=2,k
   x=x+dx
   call rk4(N,x,dx,yi,yf)
   z(1,j) = x
   z(2,j) = (yf(1))**N
   if ((yi(1)>=0).and.(yf(1)<=0)) then
      m = j-1
      rs = x - yf(1)*dx/(yf(1)-yi(1))
      exit
   endif
   yi(1:2) = yf(1:2)
enddo

do l=1,m
   write(1,'(e14.7,1x,e14.7)') z(1,l)/rs ,z(2,l)
enddo

end program density
