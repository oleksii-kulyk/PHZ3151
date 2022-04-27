        program pendulum
                real anteta(300),omega(300),teta(300),t(300),l,
     *          tetad(300),omegad(300)
c               open(7,file="pen_out")
                open(8,file="pen_d4")
  111           format(3(2x,e12.5))
                print*,"Enter teta0"
                read(5,*) teta0
                print*,"Enter q"
                read(5,*) q
                print*,"Enter frdr"
                read(5,*) frdr
                pi=3.1415927
                teta0=teta0*pi/180.0
                teta(1)=teta0
                tetad(1)=teta0
                l=1.0 !m
                g=9.8 !m/s2
                t(1)=0.0
                freq=sqrt(g/l)
                frdr=frdr*freq
                period=2.0*pi/freq
                tt=4.0*period
                dt=tt/300.0
                gl=g/l
                omega(1)=0.0
                omegad(1)=0.0
                do i=1,299
                      t(i+1)=t(i)+dt
                      anteta(i+1)=teta0*sin(freq*t(i+1)+0.5*pi)
                      omega(i+1)=omega(i)-gl*teta(i)*dt
                      teta(i+1)=teta(i)+omega(i+1)*dt
                      omegad(i+1)=omegad(i)-(gl*tetad(i)+q*omegad(i)-
     *                0.2*cos(frdr*t(i+1)))*dt !!! remove 0.2*cos(frdr*t(i+1)) = driving force
                      tetad(i+1)=tetad(i)+omegad(i+1)*dt
                      write(8,111) t(i+1),anteta(i+1),tetad(i+1)
                enddo
c                close(7)
                close(8)
         end

