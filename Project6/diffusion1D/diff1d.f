        program diff1d
        real rho(500,1000), entr(1000)
        print*, "Enter the number of time steps"
        read(5,*) nts
        open(7,file="rho1D")
  111   format(2(2x,e12.5))
        open(8, file="entropy")
        rho=0.0
        do i=230,270
          rho(i,1)=1.0
        enddo
        do nt=1,nts-1
          s=0.0
          do i=2,499
            rho(i,nt+1) = rho(i,nt) + 0.3*( rho(i+1,nt) -
     *      2.0*rho(i,nt) + rho(i-1,nt) )
            rh=rho(i,nt)
            if(rh.ne.0.0) then
              s=s-rh*log(rh)
            endif
          enddo
          entr(nt) = s
          write(8,111) 1.0*nt,entr(nt)
        enddo
        do i=1,500
          si=i
          write(7,111) si, rho(i,nts)
        enddo
        end
