!######################################################################################
module CommonParam 
!######################################################################################
	implicit none
!!!!!!!!! descriptors for input files !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), public, parameter :: n_input = 11
	character(5), public, parameter :: name_input = 'INPUT'
!!!!!!!!! descriptors fot output files !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), public, parameter :: n_error = 21
	character(5), public, parameter :: name_error = 'ERROR'
	integer(4), public, parameter :: n_pro = 23
	character(11), public, parameter :: name_pro = 'profile.out'
	integer(4), public, parameter :: n_spn = 24
	character(9), public, parameter :: name_spn = 'spins.out'
	integer(4), public, parameter :: n_energy = 25
	character(10), public, parameter :: name_energy = 'energy.out'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	real(8), public, parameter :: max_number = 10000000.0
	real(8), public, parameter :: tolerance = 1e-7
	integer(4), public, parameter :: n_max_iter = 50000000
	
Contains

!**************************************************************************************
subroutine calc_trans_probabilities(p,lambda)
!**************************************************************************************
! Находим вероятности перехода по заданному значению длины сегмента Куна p
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	implicit none
	real(8), intent(in) :: p ! длина сегмента Куна
	real(8), intent(out) :: lambda(-1:1) ! вероятности появления конфигураций угла:
	! lambda(-1) - загиб назад, lambda(0) - 90 град., lambda(+1) - прямая конфигурация 
	real(8) exp_K ! exp(К) - экспонента коэффициента жёсткости (корреляции сегментов)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! p = (1 + <cos(ang)>) / (1 - <cos(ang)>),                                      (1)
	! где <cos(ang)> - средний косинус угла между векторами - сегментами цепи
	! <cos(ang)> = cos(pi)*lambda(-1) + cos(pi/2)*lambda(0) + cos(0)*lambda(+1)     (2)
	! U(ang) = K * (1 - cos(ang)) - потенциал угла                                  (3)
	! lambda(-1) = A*exp(-U(pi))                                                    (4)
	! lambda( 0) = A*exp(-U(pi/2))                                                  (5)
	! lambda(+1) = A*exp(-U(0))                                                     (6)
	! решая уравнения (1-6) находим:

	exp_K = sqrt((1.0-p)**2+p) + p - 1.0
	
	lambda(0)  = 1.0/(exp_K + 1.0/exp_K + 4.0)
	lambda(-1) = lambda(0) / exp_K
	lambda(+1) = lambda(0) * exp_K
	lambda(0)  = (1.0 - lambda(-1) - lambda(+1)) / 4.0
	
return
end subroutine calc_trans_probabilities
!**************************************************************************************

end module CommonParam
