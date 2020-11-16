	!Calculating area of an ellipse using Monte Carlo method
		
	program ellipse
	implicit none
	
	integer :: i
	real :: xr, yr, xcor, ycor, yfunc, area, Na
	
	Na = 0
	
	do i = 1, 10000
		
		xr = RAND()
		yr = RAND()
		
		xcor = xr * 10
		ycor = yr * 5
				
		yfunc = sqrt(25-((xcor**2)/4))
		
		if (ycor < yfunc) then
			Na = Na + 1
		end if
	end do
	
	area = (Na/10000)*50*4
	
	write (*,*) "Area of the ellipse with semi major axis = 10 and semi minor axis = 5 is ", area
	
	end program ellipse