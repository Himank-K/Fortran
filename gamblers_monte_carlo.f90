	! Gambler's ruin problem using Monte Carlo method
	
	program gambler
    implicit none
	
    integer :: count_A, i, N, m, i0
    real :: xr, p, prob, P_A, q, a
    
	count_A = 0
	N = 20	
	p = 0.4
    q = 0.6
    a = 1.5
    i0 = 2
	
    open(unit=1,file='gamblers_monte_carlo.data',status='new')

    do
		do m = 1, 10000
			i=i0
			do
				xr=rand()
				if(xr<p)then
					i = i+1
				else
					i = i-1
				end if
				if(i == 0 .or. i == N )then
					if(i == N)then
						count_A = count_A + 1
					end if
					exit
				end if
			end do
		end do
		
        prob = (real(count_A))/(10000)
        P_A=(1-a**real(i0))/(1-a**20.0)
		
        write(1,*) i0, prob, P_A
		
		i0 = i0+2
        count_A = 0
        
		if(i0 == 20)then
            exit
        end if
    end do
	close (1)
end program gambler
