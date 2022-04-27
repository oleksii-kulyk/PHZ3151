        program meanfield
          dt=0.04 ! temperature step
          eps=1.0e-4
          open(7,file="magn-mf")
  111     format(2(2x,e12.5))
          do it=1,200               ! temperature
            sm=0.5
            rt=it*dt
            delta=1.0
            do while (delta > eps)  ! iterations
              rtnh=tanh(4.0*sm/rt)
              fs=sm-rtnh
              dfs=1.0-4.0*(1.0-rtnh**2)/rt
              if (dfs==0.0) then
                dfs=1.0e-7
              endif
              sm0=sm
              sm=sm0-fs/dfs
              delta=abs(sm-sm0)
            enddo                   ! iterations
            write(7,111) rt, sm 
          enddo                     ! temperature
        end
