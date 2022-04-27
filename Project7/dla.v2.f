        program dla
        integer iclx(1000), icly(1000)
        print*, "Enter the final number of atoms in the cluster"
        read(5,*) nacf
        open(7, file="dla-cluster")
        open(8, file="dla-fr-dim")
  111   format(2(2x,i4))
  112   format(2(2x,e12.5))
        iclx(1)=0
        icly(1)=0
        nac=1
        do while(nac<nacf)  !!! over cluster atoms
          rad=0.0
          do i=1,nac
            if(iclx(i)>rad) then
              rad=iclx(i)
            endif
            if(icly(i)>rad) then
              rad=icly(i)
            endif
          enddo
		  rad=rad+5.0
          do iw=1,50000   ! walkers
            r1=rand(0)
            atx=r1*rad
            iatx=int(atx)
            r2=rand(0)
            if (r2<0.5) then
              iatx=-iatx
            endif
            iaty=int(sqrt(rad**2-atx**2))
            r3=rand(0)
            if (r3<0.5) then
              iaty=-iaty
            endif
            do it=1,10000 ! time steps
              r2=rand(0)
              if(r2<0.5) then  ! along x
                if(r2<0.25) then
                  iatx=iatx+1
                else
                  iatx=iatx-1
                endif
              else             ! along y
                if(r2<0.75) then
                  iatx=iatx+1
                else
                  iatx=iatx-1
                endif
              endif
              do i=1,nac
                difx=iatx-iclx(i)
                dify=iaty-icly(i)
                dif=sqrt(difx**2+dify**2)
                if(dif<1.0001) then
                  goto 1
                endif
              enddo
            enddo         ! time steps
          enddo          ! walkers
   1      continue
          if(dif<1.0001) then
            nac=nac+1
            iclx(nac)=iatx
            icly(nac)=iaty
            icna=icna+1
            print*, icna
          endif
        enddo 		!!! over cluster atoms
    ! fractal dimentionality
		do ir=1,60
		  is=0
		  rd=ir
		  do i=1,nacf
		    x=iclx(i)
			  y=icly(i)
			  pos=sqrt(x**2+y**2)
			  if(pos<rd) then
			    is=is+1
			  endif
      enddo
		  sm=is
		  write(8,112) log(rd), log(sm)
		enddo
    ! fractal dimentionality
        do i=1, nacf
          write(7,111) iclx(i), icly(i)
        enddo
        end
