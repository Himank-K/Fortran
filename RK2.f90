	!Himank Kavathekar
	!PRN: 2019P038
	!Solving 1st order differential equation by Runge Kutta 2nd order method
	
	program RK2
	implicit none
	
	real, allocatable :: X(:), Y(:)
	real :: h, F, k1, k2
	integer :: i, M, N
	
	h=1.0E-2
	N = int((1/h))
	allocate (X(N), Y(N))
	
	Y(1) = 0.5
	X(1) = 0
	
	do i=1, N
		if (i == N) then
			exit
		else
			k1 = h * F(X(i), Y(i))
			k2 = h * F((X(i) + h), (Y(i) + k1))
			Y(i+1) = Y(i) + 0.5* (k1 + k2)
			X(i+1) = X(i) + h
		end if
	end do

	open(unit=10, file='RK2_01.data', status='new')
		do i=1, N
			write (10,*) X(i), Y(i)
		end do
	close(10)
	
	end program RK2
	
	real function F(x, y)
	implicit none
	
	real :: x, y
	
	F = 2*x*y
	return
	end function F
