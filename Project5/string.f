        program string
        real y(1000,3)
        x0=0.3
        sigma=1000.0
        open(7,file="wave")
  111   format(2 (2x,e12.5))
        print*, "Enter the number of time steps"
        read(5, *) nts
        y=0.0
        do i=2,999
            x=0.001*(i-1)
            y(i,1)=exp(-sigma*(x-x0)**2)
            y(i,2)=y(i,1)
        enddo
        do ii=1, nts              ! time steps to make
            do i=2,999
                y(i,3)=y(i+1,2)+y(i-1,2)-y(i,1)
            enddo
            y(1,3)=0.0
            y(1000, 3)=0.0
            do i=1,1000
                do nt=2,3
                    y(i, nt-1)=y(i,nt)
                enddo
            enddo
        enddo                     ! time steps to make

        do i=1,1000               ! data output
            x=0.001*i
            write(7,111) x,y(i,3)
        enddo
        end
