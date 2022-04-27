        program cannon
                real x0(200),y0(200),x(200),y(200),vx(200),vy(200),
     *          xr(200),yr(200),vxr(200),vyr(200)
                b2m=4.0e-5
                v0=700.0 !m/s
                g=9.8 !m/s2
                pi=3.14159265
                print*,"Enter theta"
                read(5,*) teta
                tetarad=teta*pi/180.0
                open(7,file="x0y0")
                open(8,file="xy")
                open(9,file="rhoxy")
                tmax=2.0*v0*sin(tetarad)/g
                h=tmax/200.0 !time step
                x0(1)=0.0
                x(1)=0.0
                y0(1)=0.0
                y(1)=0.0
                xr(1)=0.0
                yr(1)=0.0
                vx(1)=v0*cos(tetarad)
                vy(1)=v0*sin(tetarad)
                vxr(1)=vx(1)
                vyr(1)=vy(1)
                v=v0
                vr=v0
                do i=1,199
                        t=(i+1)*h
                        x0(i+1)=v0*cos(tetarad)*t
                        y0(i+1)=v0*sin(tetarad)*t-0.5*g*t*t
                        vx(i+1)=vx(i)-b2m*v*vx(i)*h
                        vy(i+1)=vy(i)-g*h-b2m*v*vy(i)*h
                        v=sqrt(vx(i+1)**2+vy(i+1)**2)
                        x(i+1)=x(i)+vx(i)*h
                        y(i+1)=y(i)+vy(i)*h
                        b2mr=b2m*(1.0-2.16667e-5*yr(i))**2.5
                        vxr(i+1)=vxr(i)-b2mr*vr*vxr(i)*h
                        vyr(i+1)=vyr(i)-g*h-b2mr*vr*vyr(i)*h
                        vr=sqrt(vxr(i+1)**2+vyr(i+1)**2)
                        xr(i+1)=xr(i)+vxr(i)*h
                        yr(i+1)=yr(i)+vyr(i)*h
                enddo
  111           format(2(2x,e12.5))
                sc1=1000.0
                do i=1,199
                        write(7,111) x0(i+1)/sc1,y0(i+1)/sc1
                enddo
                do i=1,199
                        if (yr(i+1)>0.0) then
                                 write(9,111) xr(i+1)/sc1,yr(i+1)/sc1
                        endif
                enddo
                close(7)
                close(8)
                close(9)
        end
