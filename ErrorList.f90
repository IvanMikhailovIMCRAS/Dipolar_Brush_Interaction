!######################################################################################
module ErrorList
!######################################################################################
use CommonParam
implicit none
	
Contains

!**************************************************************************************
subroutine ERROR(n)
!**************************************************************************************
	implicit none
    integer(4), intent(in) :: n
	
	open(n_error,file=name_error)
	
    select case(n)
        case(0)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'Input file does not exist'
        case(1)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'N value is not correct'
        case(2)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'sigma value is not correct'
        case(3)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'p value is not correct'
        case(4)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'chi value is not correct'
        case(5)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'tau value is not correct'
        case(6)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'N_s value is not correct'
        case(7)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'p_s value is not correct'
        case(8)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'tau_s value is not correct'
        case(9)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'eta value is not correct'
        case(10)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'nfree value is not correct'
        case(11)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'Number of iterations > max'
        case(12)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'eta < tolerance'
        case(13)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'n_layer value is not correct'
        case(14)
            write(n_error,'(a,I0,1x,a)') 'Error', n, 'n_layer < theta'
        case default
            write(n_error,'(a)') 'Unknown ERROR! Upps))'
    end select

	close(n_error)
	
    stop
end subroutine ERROR
!**************************************************************************************

!######################################################################################
end module ErrorList
!######################################################################################
