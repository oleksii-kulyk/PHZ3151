        program montecarlo
          real sm(10,10), esm(10,10)
          ej=1.0
          dt=0.03
          open(7,file="magn-mc")
  111     format(3(2x,e12.5))
c          print*, "Enter temperature factor"
c          read(5,*) itemp
          sm=0.0
          esm=0.0
          smtot=0.0
          do i=1,10
              do j=1,10
                r1=rand(0)
                sm(i,j)=2*NINT(r1)-1.0 !if (r1>0.5) => sm(i,j) = 1.0. if (r1<0.5) => sm(i,j)=-1.0
                smtot=smtot+sm(i,j)
              enddo
          enddo
c          print*, smtot
          do itemp=1,200  !     temperatures loop
            smav=0.0
            esmav=0.0
            temp=itemp*dt
            do its=1,5000   ! time steps
              etot=0.0
              smtot=0.0
              do i=2,9
                do j=2,9
                  sm4=sm(i,j-1)+sm(i,j+1)+sm(i-1,j)+sm(i+1,j)
                  esm(i,j)=-ej*sm(i,j)*sm4
                  call flip(sm(i,j), esm(i,j), temp)
                  etot=etot+esm(i,j)
                  smtot=smtot+sm(i,j)
                enddo
              enddo
! Corner 1
            sm11=sm(1,2)+sm(2,1)+sm(1,10)+sm(10,1)
            esm(1,1)=-ej*sm(1,1)*sm11
            call flip(sm(1,1), esm(1,1), temp)
            etot=etot+esm(1,1)
            smtot=smtot+sm(1,1)
! Corner 2
            sm101=sm(9,1)+sm(10,2)+sm(1,1)+sm(10,10)
            esm(10,1)=-ej*sm(10,1)*sm101
            call flip(sm(10,1), esm(10,1), temp)
            etot=etot+esm(10,1)
            smtot=smtot+sm(10,1)
! Corner 3
            sm110=sm(2,10)+sm(1,9)+sm(10,10)+sm(1,1)
            esm(1,10)=-ej*sm(1,10)*sm110
            call flip(sm(1,10), esm(1,10), temp)
            etot=etot+esm(1,10)
            smtot=smtot+sm(1,10)
! Corner 4
            sm1010=sm(9,10)+sm(10,9)+sm(10,1)+sm(1,10)
            esm(10,10)=-ej*sm(10,10)*sm1010
            call flip(sm(10,10), esm(10,10), temp)
            etot=etot+esm(10,10)
            smtot=smtot+sm(10,10)
! Edge 1
            do j=2,9
              sm1j=sm(1,j-1)+sm(2,j)+sm(1,j+1)+sm(10,j)
              esm(1,j)=-ej*sm(1,j)*sm1j
              call flip(sm(1,j), esm(1,j), temp)
              etot=etot+esm(1,j)
              smtot=smtot+sm(1,j)
! Edge 2
              sm10j=sm(10,j-1)+sm(10,j+1)+sm(9,j)+sm(1,j)
              esm(10,j)=-ej*sm(10,j)*sm10j
              call flip(sm(10,j), esm(10,j), temp)
              etot=etot+esm(10,j)
              smtot=smtot+sm(10,j)
            enddo
! Edge 3
            do i=2,9
              smi1=sm(i-1,1)+sm(i+1,1)+sm(i,2)+sm(i,10)
              esm(i,1)=-ej*sm(i,1)*smi1
              call flip(sm(i,1), esm(i,1), temp)
              etot=etot+esm(i,1)
              smtot=smtot+sm(i,1)
! Edge 4
              smi10=sm(i-1,10)+sm(i+1,10)+sm(i,9)+sm(i,1)
              esm(i,10)=-ej*sm(i,10)*smi10
              call flip(sm(i,10), esm(i,10), temp)
              etot=etot+esm(i,10)
              smtot=smtot+sm(i,10)
            enddo

            smav=smav+smtot
            esmav=esmav+etot
            enddo           ! time steps
            smav=smav/5000.0
            esmav=esmav/5000.0
            write(7,111) temp, smav, esmav
          enddo                   !     temperatures loop
        end

        subroutine flip(smf,esmf,temp)
          esm1=-esmf
          if(esm1<esmf) then
            smf=-smf
            esmf=esm1
          else
            bf=exp(-2.0*esm1/temp)
            r1=rand(0)
            if(r1<bf) then
              smf=-smf
              esmf=esm1
            endif
          endif
          return
        end

c Code Run notes:
c first run with
c temp=2
c then temp=50
c then temp-76
