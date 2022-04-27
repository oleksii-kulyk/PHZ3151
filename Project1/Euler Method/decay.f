      program decay
      real an(200), t(200), eul(200)
      open (7, file = "pu")
 111  format (3(2x,e12.5))
      print *, "Enter ns" !!! step
      read(5,*) ns
      tau=126.58
      tt=5*tau
      dt=tt*ns/200.0
      nt=200/ns
      eul(1)=100.0
      t(1)=0.0
      do i=1, nt-1
          t(i+1)=(i+1)*dt
          an(i+1)=100.0*exp(-t(i+1)/tau)
          eul(i+1)=eul(i)*(1.0-dt/tau)
          write(7,111) t(i+1), an(i+1), eul(i+1)
      enddo
      end
