	!Solving 1st order differential equation by Euler's method
	
	program EULERDIFF
	implicit none
	
	real, allocatable :: X(:), Y(:)
	real :: h, F
	integer :: i, M, N
	
	h=1.0E-2
	N = int((1/h))
	allocate (X(N+1), Y(N+1))
	
	Y(1) = 0.5
	X(1) = 0
	
	do i=1, N
		Y(i+1) = Y(i) + h*F(X(i),Y(i))
		X(i+1) = X(i) + h
	end do

	open(unit=10, file='eulerdiffeqn_01.data', status='new')
		do i=1, N
			write (10,*) X(i), Y(i)
		end do
	close(10)
	
	end program EULERDIFF
	
	real function F(x, y)
	implicit none
	
	real :: x, y
	
	F = 2*x*y
	return
	end function F
