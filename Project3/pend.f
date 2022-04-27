      program pendulum
      real antheta(300), theta(300 ), omega(300), t(300)
      open (7, file="pend_out")
  111 format(3(2x,e12.5))
      print*, "Enter theta0"
      read(5, *) theta0
      pi=3.1415927
      theta0=theta0*pi/180.0
      g=9.8 
c the length = 1m
      freq=sqrt(g) ! natural frequency
      period=2.0*pi/freq      
      tint=5.0*period !!! time interval
      dt=tint/300.0
      t(1) = 0.0
      theta(1)=theta0
      omega(1)=0.0 
      do i=1,299
          t(i+1)=t(i)+dt
          antheta(i+1)=theta0*cos(freq*t(i+1))
          omega(i+1)=omega(i)-g*theta(i)*dt
          theta(i+1)=theta(i)+omega(i)*dt
          write(7,111) t(i+1), antheta(i+1), theta(i+1)
      enddo
      
      end
