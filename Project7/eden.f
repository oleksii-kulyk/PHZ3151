        program eden
        integer iclx(1000), icly(1000), ipx(1000), ipy(1000)
        iclx(1)=100
        icly(1)=100
        open(7, file="eden-cluster")
        open(8, file="eden-perimeter")
  111   format(2(2x,i4))
        open(9, file="eden-fr-dim")
  112   format(2(2x,e12.5))
        print*, "Enter the number of atoms in the cluster"
        read(5, *) nacf
        nac=1
          do while (nac<=nacf) ! cluster atoms
            print*, nac
            nap=0
            do i=1,200   ! x-screening
              do j=1,200 ! y-screening
                is=0
                do ic=1, nac
                  if((i==iclx(ic)) .and. (j==icly(ic))) then
                    is=is+1
                  endif
                enddo
                if(is==0) then
                  do ic=1,nac
                    ix=abs(iclx(ic)-i)
                    iy=abs(icly(ic)-j)
                    if((ix+iy)==1) then
                      nap=nap+1
                      ipx(nap)=i
                      ipy(nap)=j
                    endif
                  enddo
                endif
              enddo      ! x-screening
            enddo        ! y-screening
            r1=rand(0)
            ic=int(nap*r1)
            if(ic==0) then
              ic=1
            endif
            nac=nac+1
            iclx(nac)=ipx(ic)
            icly(nac)=ipy(ic)
          enddo                ! cluster atoms
          ! fractal dimentionality
          do ir=1,60
            is=0
            rd=ir
            do i=1,nacf
              x=iclx(i)-100.0
              y=icly(i)-100.0
              pos=sqrt(x**2+y**2)
              if(pos<rd) then
                is=is+1
              endif
            enddo
            sm=is
            write(9,112) log(rd),log(sm)
          enddo
          ! fractal dimentionality
        do i=1,nacf
          write(7,111) iclx(i), icly(i)
        enddo
        do i=1,nap
          write(8,111) ipx(i), ipy(i)
        enddo
        end
