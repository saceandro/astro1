program orbit
implicit none

real(8) ti, tf, t, dt
real(8) yi(4), yf(4)
integer k, N, j

open(unit=2, file='tmp_runge_kutta.dat')

N=50001

ti=0.00D0
tf=2.00D2

dt=(tf-ti)/dble(N-1)

t=ti
yi(1)=6.00D0
yi(2)=0.00D0
yi(3)=0.00D0
yi(4)=8.00D-2

write(2,'(e14.7,2x,e14.7,2x,e14.7,2x,e14.7,2x,e14.7)') &
t,yi(1),yi(2),yi(3),yi(4)

do k = 2, N
   t=t+dt
   call ODE(yi,yf,dt)
   write(2, '(e14.7,2x,e14.7,2x,e14.7,2x,e14.7,2x,e14.7)')t,yf(1),yf(2),yf(3),yf(4)
   
   do j =1,4
      yi(j)=yf(j)
   enddo
enddo
close(2)
end program orbit
