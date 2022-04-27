        program dla
        open(7, file="dla-cluster")
  111   format(2(2x,i4))
        rad=5.0
        do iw=1,1000   ! walkers
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
          write(7,111) iatx,iaty
        enddo          ! walkers
        end
