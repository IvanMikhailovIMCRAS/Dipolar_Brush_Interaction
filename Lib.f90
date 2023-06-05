!######################################################################################
module Lib
!######################################################################################
Use CommonParam
Use ErrorList

Contains

!**************************************************************************************
subroutine calc_free_energy(n_layer, N, N_s, sigma, phi1, phi2, phis1, phis2, s1_1, s1_2,  &
&      s1_s1, s1_s2, U1, U2, Us1, Us2, alpha, tau, chi, part_fun_pol1, part_fun_pol2, F)
!**************************************************************************************
	integer(4), intent(in) :: n_layer, N, N_s
	real(8), intent(in) :: sigma
	real(8), intent(in) :: phi1(0:n_layer+1,-1:1), U1(0:n_layer+1,-1:1)
	real(8), intent(in) :: phi2(0:n_layer+1,-1:1), U2(0:n_layer+1,-1:1)
	real(8), intent(in) :: phis1(0:n_layer+1,-1:1), Us1(0:n_layer+1,-1:1)
	real(8), intent(in) :: phis2(0:n_layer+1,-1:1), Us2(0:n_layer+1,-1:1)
	real(8), intent(in) :: alpha(0:n_layer+1)
	real(8), intent(in) :: s1_1(0:n_layer+1), s1_2(0:n_layer+1)
	real(8), intent(in) :: s1_s1(0:n_layer+1), s1_s2(0:n_layer+1)
	real(8), intent(in) :: tau, chi, part_fun_pol1, part_fun_pol2
	real(8), intent(out) :: F
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	real(8) phi_pol1(0:n_layer+1), phi_pol2(0:n_layer+1)
	real(8) phi_solv1(0:n_layer+1), phi_solv2(0:n_layer+1)
	real(8) S_tr, S_conf_1, S_conf_2, A_alpha, A_int, F_dd
	integer(4) z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	phi_pol1(:) = phi1(:,-1) + phi1(:,0) + phi1(:,+1)
	phi_pol2(:) = phi2(:,-1) + phi2(:,0) + phi2(:,+1)
	phi_solv1(:) = phis1(:,-1) + phis1(:,0) + phis1(:,+1)
	phi_solv2(:) = phis2(:,-1) + phis2(:,0) + phis2(:,+1)
		!!!! вклады в свободную энергию:
	!! 1) трансляционная энтропия
	S_tr = - 2.0 * sigma * log(sigma) 
	!! 2) конформационная энтропия (полимер №1)
	S_conf_1 = sigma*log(part_fun_pol1/dble(N)) 
	!! 3) конформационная энтропия (полимер №2)
	S_conf_2 = sigma*log(part_fun_pol2/dble(N))	
	!! 4) работа "псевдопотенциала"
	A_alpha = sum(alpha(1:n_layer))
	!! 5) работа поля, описывающего межчастичные взаимодействия
	A_int = sum(U1(1:n_layer,-1)*phi1(1:n_layer,-1)) &
&	      + sum(U1(1:n_layer, 0)*phi1(1:n_layer, 0)) &
&	      + sum(U1(1:n_layer,+1)*phi1(1:n_layer,+1)) &
&	      + sum(U2(1:n_layer,-1)*phi2(1:n_layer,-1)) &
&	      + sum(U2(1:n_layer, 0)*phi2(1:n_layer, 0)) &
&	      + sum(U2(1:n_layer,+1)*phi2(1:n_layer,+1)) &
&	      + sum(Us1(1:n_layer,-1)*phis1(1:n_layer,-1)) &
&	      + sum(Us1(1:n_layer, 0)*phis1(1:n_layer, 0)) &
&	      + sum(Us1(1:n_layer,+1)*phis1(1:n_layer,+1)) & 
&	      + sum(Us2(1:n_layer,-1)*phis2(1:n_layer,-1)) &   
&	      + sum(Us2(1:n_layer, 0)*phis2(1:n_layer, 0)) &
&	      + sum(Us2(1:n_layer,+1)*phis2(1:n_layer,+1))  

		
	!! 6) энергия диполь-дипольных взаимодействий      
	F_dd = 0.0
	
	do z = 1, n_layer
		F_dd = F_dd + (s1_1(z) + s1_s1(z) - s1_2(z) - s1_s2(z))**2
	enddo 
			
	
	F_dd = 0.5 * tau * F_dd
	
	
	F = - S_tr - S_conf_1 - S_conf_2 - A_alpha - A_int + F_dd
	
	open(n_energy,file=name_energy)
	write(n_energy,*) n_layer, F, F_dd-A_int, - S_tr*0.5 - S_conf_1 - sum(alpha(1:n_layer)*phi_pol1(1:n_layer)), &
&	      - sum(alpha(1:n_layer)*(phi_solv1(1:n_layer)+phi_solv2(1:n_layer))), &
&         -S_conf_1
   	close(n_energy) 
	
	
	return
end subroutine calc_free_energy
!**************************************************************************************

!**************************************************************************************
subroutine check_deviation(dev, dev_old, eta, iter)
!**************************************************************************************
! градиентный спуск, проверка отклонения
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(8), intent(inout) :: dev, dev_old, eta
    integer(4), intent(inout) :: iter
    real(8), parameter :: golden_ratio = 1.6180339887498948
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    eta = eta / golden_ratio
    iter = 0.0
    dev = max_number
    dev_old = dev + 1.0

    if (eta.lt.tolerance) then
        call ERROR(12) 
        stop
    endif
    
	return    
end subroutine check_deviation
!**************************************************************************************


!**************************************************************************************
subroutine end_point(n_layer, N, Gf, Gb, WB, dist, k)
!**************************************************************************************
! вычисление распределения объёмной доли концевых звеньев
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer(4), intent(in) :: n_layer, N, k 
    real(8), intent(in) :: Gf(0:n_layer+1,k:N,-1:1), Gb(0:n_layer+1,1:N,-1:1)
    real(8), intent(in) :: WB(0:n_layer+1,-1:1)
    real(8), intent(out) :: dist(0:n_layer+1)
    real(8) :: sum_dist
    integer(4) z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    dist(:) = 0.0
    sum_dist = 0.0

    do z = 1, n_layer
        dist(z) = Gf(z,N,-1) * Gb(z,N,+1) / WB(z,-1) + &
        &         Gf(z,N, 0) * Gb(z,N, 0) / WB(z, 0) * 4.0 + &
        &         Gf(z,N,+1) * Gb(z,N,-1) / WB(z,+1) 
    enddo
    
    sum_dist = sum(dist(1:n_layer))

    do z = 1, n_layer
        dist(z) = dist(z)/sum_dist
    enddo
    
    return
end subroutine end_point
!**************************************************************************************

!**************************************************************************************
subroutine calc_dip_dip_interaction(n_layer, s1_1, s1_2, s1_s1, s1_s2, tau, U1, U2, Us1, Us2)
!**************************************************************************************
! диполь-дипольные взаимодействия
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: n_layer
	real(8), intent(in) :: tau
	real(8), intent(in) :: s1_1(0:n_layer+1), s1_2(0:n_layer+1)
	real(8), intent(in) :: s1_s1(0:n_layer+1), s1_s2(0:n_layer+1) 
	real(8), intent(inout) :: U1(0:n_layer+1,-1:1), U2(0:n_layer+1,-1:1)
	real(8), intent(inout) :: Us1(0:n_layer+1,-1:1), Us2(0:n_layer+1,-1:1)
	integer(4) z
	real(8) dip_contrib
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	do z = 1, n_layer
       	dip_contrib = tau * (s1_1(z) - s1_2(z) + s1_s2(z) - s1_s2(z))
        !!!!
        U1(z,-1) = U1(z,-1) -  dip_contrib
        U1(z,+1) = U1(z,+1) +  dip_contrib
        !!!!
        U2(z,-1) = U2(z,-1) +  dip_contrib
        U2(z,+1) = U2(z,+1) -  dip_contrib
        !!!!
        Us1(z,-1) = Us1(z,-1) -  dip_contrib
        Us1(z,+1) = Us1(z,+1) +  dip_contrib
        !!!!
        Us2(z,-1) = Us2(z,-1) +  dip_contrib
        Us2(z,+1) = Us2(z,+1) -  dip_contrib
    enddo
	
	return
end subroutine calc_dip_dip_interaction
!**************************************************************************************


!**************************************************************************************
subroutine calc_U_Flory(n_layer, chi, phi, phi_bulk, U)
!**************************************************************************************
! взаимодействия Флори-Хаггинса 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: n_layer
	real(8), intent(in) :: chi, phi_bulk
	real(8), intent(inout) :: phi(0:n_layer+1)
	real(8), intent(inout) :: U(0:n_layer+1,-1:1)
	integer(4) z
	real(8) mean_phi
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	do z = 1, n_layer
       	mean_phi = Lb * phi(z-1) + Lp * phi(z) + Ls * phi(z+1)
        !!!!
        U(z,-1) = U(z,-1) + chi * (mean_phi - phi_bulk)
        U(z, 0) = U(z, 0) + chi * (mean_phi - phi_bulk) 
        U(z,+1) = U(z,+1) + chi * (mean_phi - phi_bulk) 
    enddo
	
	return
end subroutine calc_U_Flory
!**************************************************************************************

!**************************************************************************************
subroutine calc_orientation(n_layer, phi, s1)
!**************************************************************************************
! параметр порядка ориентации сегментов s_1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: n_layer
	real(8), intent(in) :: phi(0:n_layer+1,-1:1)
	real(8), intent(out) :: s1(0:n_layer+1)
	integer(4) z
	
	do z = 0, n_layer+1
       	if ((phi(z,+1)+phi(z,0)+phi(z,-1)).gt.0.0) then
       		s1(z) = phi(z,+1)-phi(z,-1) 
       	else
       		s1(z) = 0.0
       	endif
    enddo
       
	return
end subroutine calc_orientation
!**************************************************************************************

!**************************************************************************************
subroutine phi_calc(n_layer, N, Gf, Gb, WB, phi, part_fun, n1)
!**************************************************************************************
! расчёт ненормированных профилей плотности и стат.суммы
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: n_layer, N, n1
	real(8), intent(in) :: Gf(0:n_layer+1,n1:N,-1:1), Gb(0:n_layer+1,1:N,-1:1)
	real(8), intent(in) :: WB(0:n_layer+1,-1:1)
	real(8), intent(inout) ::  phi(0:n_layer+1,-1:1)
	real(8), intent(out) :: part_fun
	integer(4) z, s
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	 
	
	phi(:,:) = 0.0
	
	do s = 1, N
		do z = 1, n_layer
			phi(z,-1) = phi(z,-1) + Gf(z,s,-1) * Gb(z,s,+1) / WB(z,-1) 
			phi(z, 0) = phi(z, 0) + Gf(z,s, 0) * Gb(z,s, 0) / WB(z, 0) 
			phi(z,+1) = phi(z,+1) + Gf(z,s,+1) * Gb(z,s,-1) / WB(z,+1) 
		enddo
	enddo
	
	phi(:,0) = phi(:,0) * 4.0
	
	part_fun = sum(phi(1:n_layer,-1) + phi(1:n_layer,0) + phi(1:n_layer,+1))
		
	return
end subroutine phi_calc
!**************************************************************************************

!**************************************************************************************
subroutine forward_propagate(N, n_layer, lambda, WB, Gf, n1)
!**************************************************************************************
! заполнение матрицы "прямого" пропогатора
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
	integer(4), intent(in) :: N, n_layer, n1
	real(8), intent(in) :: lambda(-1:1)
	real(8), intent(in) :: WB(0:n_layer+1,-1:1)
	real(8), intent(inout) :: Gf(0:n_layer+1,n1:N,-1:1)
	integer(4) s, z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!! n1 - с какого сегмента начинаем блуждание (с нулевого n1=0 или с первого n1=1)
    do s = 1+n1, N
    	do z = 1, n_layer
    		Gf(z,s,-1) = WB(z,-1)*( &
       		&   lambda(-1)*Gf(z-1,s-1,1)  &
       		& + 4.0*lambda(0)*Gf(z,s-1,0)  &
       		& + lambda(+1)*Gf(z+1,s-1,-1) ) 
       		!!!
       		Gf(z,s,0) = WB(z,0)*( &
       		&   lambda(0)*Gf(z-1,s-1,1)  &
       		& + (2.0*lambda(0) + lambda(+1) + lambda(-1))*Gf(z,s-1,0)  &
       		& + lambda(0)*Gf(z+1,s-1,-1)  )
       		!!!
       		Gf(z,s,+1) = WB(z,+1)*( &
       		&   lambda(+1)*Gf(z-1,s-1,1)  &
       		& + 4.0*lambda(0)*Gf(z,s-1,0)  &
       		& + lambda(-1)*Gf(z+1,s-1,-1) )
       	enddo
    enddo

return
end subroutine forward_propagate
!**************************************************************************************

!**************************************************************************************
subroutine back_propagate(N, n_layer, lambda, WB, Gb)
!**************************************************************************************
! заполнение матрицы "обратного" пропагатора
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: N, n_layer
	real(8), intent(in) :: lambda(-1:1)
	real(8), intent(in) :: WB(0:n_layer+1,-1:1)
	real(8), intent(inout) :: Gb(0:n_layer+1,1:N,-1:1)
	integer(4) s, z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 	
	do s = 2, N
    	do z = 1, n_layer
    		Gb(z,N-s+1,-1) = WB(z,+1) * ( &
       		&   lambda(-1)*Gb(z+1,N-s+2,1) &
       		& + 4.0*lambda(0)*Gb(z+1,N-s+2,0) &
       		& + lambda(+1)*Gb(z+1,N-s+2,-1) )
       		!!!
       		Gb(z,N-s+1,0) = WB(z,0) * ( &
       		&   lambda(0)*Gb(z,N-s+2,1) &
       		& + (2.0*lambda(0) + lambda(+1) + lambda(-1))*Gb(z,N-s+2,0) &
       		& + lambda(0)*Gb(z,N-s+2,-1) )
       		!!!
       		Gb(z,N-s+1,+1) = WB(z,-1) * ( &
       		&   lambda(+1)*Gb(z-1,N-s+2,1) &
       		& + 4.0*lambda(0)*Gb(z-1,N-s+2,0) &
       		& + lambda(-1)*Gb(z-1,N-s+2,-1) )
       	enddo
   enddo
		
return
end subroutine back_propagate
!**************************************************************************************

!**************************************************************************************
subroutine forward_propagateR(N, n_layer, lambda, WB, Gf, n1)
!**************************************************************************************
! заполнение матрицы "прямого" пропагатора для полимера растущего против оси z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: N, n_layer, n1
	real(8), intent(in) :: lambda(-1:1)
	real(8), intent(in) :: WB(0:n_layer+1,-1:1)
	real(8), intent(inout) :: Gf(0:n_layer+1,n1:N,-1:1)
	integer(4) s, z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    do s = 1+n1, N
    	do z = 1, n_layer
    		Gf(z,s,-1) = WB(z,-1)*( &
       		&   lambda(-1)*Gf(z+1,s-1,1)  &
       		& + 4.0*lambda(0)*Gf(z,s-1,0)  &
       		& + lambda(+1)*Gf(z-1,s-1,-1) ) 
       		!!!
       		Gf(z,s,0) = WB(z,0)*( &
       		&   lambda(0)*Gf(z+1,s-1,1)  &
       		& + (2.0*lambda(0) + lambda(+1) + lambda(-1))*Gf(z,s-1,0)  &
       		& + lambda(0)*Gf(z-1,s-1,-1)  )
       		!!!
       		Gf(z,s,+1) = WB(z,+1)*( &
       		&   lambda(+1)*Gf(z+1,s-1,1)  &
       		& + 4.0*lambda(0)*Gf(z,s-1,0)  &
       		& + lambda(-1)*Gf(z-1,s-1,-1) )
       	enddo
    enddo

return
end subroutine forward_propagateR
!**************************************************************************************

!**************************************************************************************
subroutine back_propagateR(N, n_layer, lambda, WB, Gb)
!**************************************************************************************
! заполнение матрицы "обратного" пропагатора для полимера растущего против оси z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer(4), intent(in) :: N, n_layer
	real(8), intent(in) :: lambda(-1:1)
	real(8), intent(in) :: WB(0:n_layer+1,-1:1)
	real(8), intent(inout) :: Gb(0:n_layer+1,1:N,-1:1)
	integer(4) s, z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!! n1 - с какого сегмента начинаем блуждание (с нулевого n1=0 или с первого n1=1) 	
	do s = 2, N
    	do z = 1, n_layer
    		Gb(z,N-s+1,-1) = WB(z,+1) * ( &
       		&   lambda(-1)*Gb(z-1,N-s+2,1) &
       		& + 4.0*lambda(0)*Gb(z-1,N-s+2,0) &
       		& + lambda(+1)*Gb(z-1,N-s+2,-1) )
       		!!!
       		Gb(z,N-s+1,0) = WB(z,0) * ( &
       		&   lambda(0)*Gb(z,N-s+2,1) &
       		& + (2.0*lambda(0) + lambda(+1) + lambda(-1))*Gb(z,N-s+2,0) &
       		& + lambda(0)*Gb(z,N-s+2,-1) )
       		!!!
       		Gb(z,N-s+1,+1) = WB(z,-1) * ( &
       		&   lambda(+1)*Gb(z+1,N-s+2,1) &
       		& + 4.0*lambda(0)*Gb(z+1,N-s+2,0) &
       		& + lambda(-1)*Gb(z+1,N-s+2,-1) )
       	enddo
   enddo
		
return
end subroutine back_propagateR
!**************************************************************************************

end module Lib
