module globals
	implicit none 
	integer, parameter :: wp = selected_real_kind(p=14)
end module globals

module FNS
implicit none
contains

function test(x)
	use globals
	implicit none
	real(wp), intent(in)  :: x
	real(wp) :: test
	!
	test = sin(x)*cos(x)**2*x**2
	!
end function test

!!!
!
!	Numerical Differentiation Function
!
!	dyn_func = name of the function to differentiate
!	c 			= the single argument of dyn_func
!	h			= step size
!	accuracy = 2,4,6,8
!
!	Matthew Briel - 2012 brielms@gmail.com
!!!
function diffy(dyn_func, c, h,accuracy)
	use globals
	implicit none
  	real(wp) :: c,h
  	real(wp), external :: dyn_func
 	real(wp) :: diffy
	integer :: accuracy
  !
  	if(accuracy.eq.2) then
		diffy = (dyn_func(c+h)-dyn_func(c-h))/(2._wp*h)
	else if(accuracy.eq.4) then
		diffy = (-dyn_func(c+2._wp*h)+8._wp*dyn_func(c+h) &
		&       -8._wp*dyn_func(c-h)+dyn_func(c-2._wp*h))/(12._wp*h)
	else if(accuracy.eq.6) then
		diffy = (-1._wp/60._wp*dyn_func(c-3._wp*h)+3._wp/20._wp*dyn_func(c-2._wp*h)-3._wp/4._wp*dyn_func(c-h)&
		&			+1._wp/60._wp*dyn_func(c+3._wp*h)-3._wp/20._wp*dyn_func(c+2._wp*h)+3._wp/4._wp*dyn_func(c+h))/h
	else if(accuracy.eq.8) then
		diffy = (1._wp/280._wp*dyn_func(c-4._wp*h)-4._wp/105._wp*dyn_func(c-3._wp*h)&
		&			+1._wp/5._wp*dyn_func(c-2._wp*h)-4._wp/5._wp*dyn_func(c-h)&
		&			-1._wp/280._wp*dyn_func(c+4._wp*h)+4._wp/105._wp*dyn_func(c+3._wp*h)&
		&			-1._wp/5._wp*dyn_func(c+2._wp*h)+4._wp/5._wp*dyn_func(c+h))/h
	end if
  !
end function diffy

end module FNS

program Trial
	use globals
	use FNS
	implicit none
	
	real(wp) :: x,h
	integer :: i
	
	h = 0.00000001_wp
	
	do i = 1, 3141
		x = i/1000._wp
		write(*,*) x,test(x),diffy(test,x,h,2),diffy(test,x,h,4),diffy(test,x,h,6),diffy(test,x,h,8)
	end do

end program Trial











