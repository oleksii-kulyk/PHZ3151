        program diff2D
        real rho(500,500)
        print*, "Enter the number of time steps"
        read(5,*) nts
        open(7,file="rho2D")
  111   format(500(2x,e12.5))
        rho=0.0
        do i=230,270
          do j=230,270
            rho(i,j)=1.0
          enddo
        enddo
        do nt=1,nts-1 ! time steps
          do i=2,499 ! not to 500 because the reccurent formula we are using has po(nt+1)
            do j=2,499
              rho(i,j) = rho(i,j) + 0.3*( rho(i+1,j) + rho(i-1,j) + 
     *        rho(i,j+1)+rho(i,j-1) - 4.0*rho(i,j) )
            enddo
          enddo
        enddo ! time steps
        do i=1,500
          write(7,111) (rho(i,j), j=1,500)
        enddo
        end 
