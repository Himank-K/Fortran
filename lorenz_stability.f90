	!Linear stability analysis of Lorenz equations
	
	program lorenz_stability_analysis
	implicit none
	
	real :: X(10000), Y(10000), Z(10000), T(10000)
	real :: m, F, G, H
	integer :: i, N
	
	m = 1.0E-2
	X(1) = -0.5
	Y(1) = 1
	Z(1) = 0.4
	T(1) = 0
	
	do i = 1, 9999
		X(i+1) = X(i) + m*F(X(i), Y(i), Z(i))
		Y(i+1) = Y(i) + m*G(X(i), Y(i), Z(i))
		Z(i+1) = Z(i) + m*H(X(i), Y(i), Z(i))
		T(i+1) = T(i) + m
	end do
	
	open(unit = 10, file = "lorenz_stability.data", status  = "old")
		do i = 1, 10000
			write(10,*) T(i), X(i), Y(i), Z(i)
		end do
	close(10)
	
	end program lorenz_stability_analysis
	
	!Differential functions:
	
	real function F(x, y, z)
	implicit none
	
	real :: x, y, z
	
	F = 5.25*(y - x)
	return
	end function F
	
	real function G(x, y, z)
	implicit none
	
	real :: x, y, z
	
	G = x*(0.7 - z) - y
	return
	end function G
	
	real function H(x, y, z)
	implicit none
	
	real :: x, y, z
	
	H = x*y - 4*z
	return
	end function H
	