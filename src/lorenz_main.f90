program lorenz
implicit none

real(8) ti, tf, t, dt
real(8) yi(3), yf(3)
integer k, N, j

open(unit=2, file='tmp_lorenz.dat')

N=50001

ti=0.0
tf=50.0

dt=(tf-ti)/dble(N-1)

t=ti
yi(1)=0.0100
yi(2)=0.0000
yi(3)=0.0000

write(2,'(e14.7,2x,e14.7,2x,e14.7,2x,e14.7)') &
t,yi(1),yi(2),yi(3)

do k = 2, N
   t=t+dt
   call euler_sys(t,dt,yi,yf)
   write(2, '(e14.7,2x,e14.7,2x,e14.7,2x,e14.7)')t,yf(1),yf(2),yf(3)
   
   do j =1,3
      yi(j)=yf(j)
   enddo
enddo
close(2)
end program lorenz
