        program rand1d

        real y2a(1000)
        print*, "Enter the number of walkers"
        read(5,*) nw
        open(7, file="walk")
  111   format(2(2x,i4))           ! i4 stands for a four digit integer
        open(8,file="y2aver")
  112   format(2(2x,e12.5))
        iy = 0
        y2a = 0.0
        do j = 1,nw !walkers
            iy = 0
            do i=1,100
                r1=rand(0)
                if(r1 < 0.5) then  ! this if can be replaed by a formula
                    ist=1
                else
                    ist=-1
                endif
                iy=iy+ist
                write(7,111) i,iy
                y2a(i)=y2a(i)+iy*iy
            enddo
        enddo
        do i=1,100
            write(8,112) 1.0*i,y2a(i)
        enddo
        end
