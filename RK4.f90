	!Solving 1st order differential equation by Runge Kutta 4th order method
	
	program RK4
	implicit none
	
	real, allocatable :: X(:), Y(:)
	real :: h, F, k1, k2, k3, k4
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
			k2 = h * F((X(i) + h/2), (Y(i) + k1/2))
			k3 = h * F((X(i) + h/2), (Y(i) + k2/2))
			k4 = h * F((X(i) + h), (Y(i) + k3))
			Y(i+1) = Y(i) + ((k1 + 2*k2 + 2*k3 + k4)/6)
			X(i+1) = X(i) + h
		end if
	end do

	open(unit=10, file='RK4_01.data', status='new')
		do i=1, N
			write (10,*) X(i), Y(i)
		end do
	close(10)
	
	end program RK4
	
	real function F(x, y)
	implicit none
	
	real :: x, y
	
	F = 2*x*y
	return
	end function F
