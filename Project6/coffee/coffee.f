        program coffee
        integer iprtx(100), iprty(100)   ! particle coordinates
        open(7, file ="distr2")
  111   format(2(2x,i3))
        print*, "Enter the number of time steps"
        read(5,*) nt
        ix=45
        nj=0 !! particle number
        do i=1,10
          iy=45
          do j=1,10
            nj=nj+1
            iprtx(nj)=ix
            iprty(nj)=iy
            iy=iy+1
          enddo
          ix=ix+1
        enddo
        do it=1,nt
          r1=rand(0)
          nj=int(r1*100)
          r2=rand(0)
          iprtx(nj)=iprtx(nj) + NINT(r2)*2 - 1
          r2=rand(0)
          iprty(nj)=iprty(nj) + NINT(r2)*2 - 1
        enddo
        do i=1,100
          write(7,111) iprtx(i), iprty(i)
        enddo
        end
