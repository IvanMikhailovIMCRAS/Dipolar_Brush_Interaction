!######################################################################################
program DIP
!######################################################################################
	use CommonParam
	use InputOutput
	implicit none
	integer(4) :: N, N_s, n_layer, nfree
	real(8) :: sigma, p, chi, tau, eta
	common /inter_int/ N, N_s, n_layer, nfree
	common /inter_real / sigma, p, chi, tau, eta
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
		
	call get_input() ! считываем входные данные из файла INPUT (see InputOutput.f90)
	
	call main()      ! выполняем расчёты (see OnePoint.f90 and Lib.f90)
	
	write(*,*) 'Awesome! That worked out!'
	stop
end program DIP
!######################################################################################

!**************************************************************************************
subroutine main()
!**************************************************************************************
	use CommonParam
	use OnePoint
	implicit none
	integer(4) :: N, N_s, n_layer, nfree
	real(8) :: sigma, p, chi, tau, p_s, tau_s, eta
	real(8) :: F ! free energy
	common /inter_int/ N, N_s, n_layer, nfree
	common /inter_real / sigma, p, chi, tau, eta
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	
	!!!! расчёт для одной точки с конкретными параметрами
	!!!! при желании здесь удобно организовать цикл по какому-либо параметру
	
	call calc_one_point(N, sigma, p, chi, tau, N_s, eta, nfree, n_layer, F)
	
 
return
end subroutine main
!**************************************************************************************
