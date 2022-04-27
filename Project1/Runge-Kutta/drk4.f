      program drk4
      real an(200), t(200), eul(200), y(200)
      open (7, file = "pu")
 111  format (3(2x,e12.5))
      print *, "Enter ns" !!! step
      read(5,*) ns
      tau=126.58
      tt=5*tau
      dt=tt*ns/200.0
      nt=200/ns
      y0=100.0
      t(1)=0.0
      do i=1, nt-1
          t(i+1)=(i+1)*dt
          an(i+1)=100.0*exp(-t(i+1)/tau)
ccc          eul(i+1)=eul(i)*(1.0-dt/tau)
c          write(7,111) t(i+1), an(i+1), eul(i+1)
      enddo
      call rk4(100.0, dt, y, nt)
      do i=1, nt-1
        t(i+1)=(i+1)*dt
        an(i+1)=100.0*exp(-t(i+1)/tau)
        write(7,111) t(i+1), an(i+1), y(i+1)
      enddo
      end
      
      function fyt(y)
      real y
      fyt = -y/126.58
      return 
      end
      
      subroutine rk4(y0, dt, y, n)
      external fyt
      real fyt, y(n)
      y(1) = y0
      do i = 1, n-1 
         y105 = y(i)+0.5*fyt(y(i))*dt
         y205 = y(i)+0.5*fyt(y105)*dt
         y11 = y(i)+fyt(y205)*dt
         y(i+1) = y(i)+dt*(fyt(y(i))+2.0*fyt(y105     )+
     *   2.0*fyt(y205)+fyt(y11) )/6.0
      enddo
      return
      end
