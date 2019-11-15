	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This subroutine is the main subroutine of Inertial model  (Inertial version).
    !
    !
    ! Usage: flood_timestep, flood_continuity and flood_discharge
    !
    ! *
    !
    ! uses modules, functions, and subroutines
    !
    ! * USE VARS_MAIN
    ! * USE VARS_INERC (only to Inertial version)
    !
    ! opens
    !
    ! * no files are created in this routine
    !
    ! reads
    !
    ! * no files are created in this routine
    !
    ! creates
    !
    ! * no files are created in this routine
    !
    !---------------------------------------------------------------------------------
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. - VER ISSO.
    !
    !  Version/Modified: 
    !
    !  2015.07.06 - 07 June 2015 (by Paulo Pontes)
	!  2016.07.17 - 17 July 2016 (by Vinícius Siqueira)
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes Rógenes
    !    * Mino  Viana Sorribas
    !    * Fernando Mainardi Fan
    !    * Juan Martin Bravo 
	!    * Ayan Santos Fleischmann 
	!    * Vinícius Alencar Siqueira
    !
    !  Main Reference:
    !
    !    Walter Collischonn,
    !    Modelo de Grandes Bacias - Thesis
    !    Porto Alegre, 2001
    !    ISBN: XXXXXXXXXXX,
    !
    !---------------------------------------------------------------------------------
    ! Variables and Parameters:
    ! *Variables declarations and routines calls are all commented below.
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------

    SUBROUTINE flood_inercial
    
    ! Variables and parameters
    USE VARS_MAIN
    USE VARS_INERC
    use IFPORT
    
    implicit none
    REAL(8) elapsed_time 
    
    
    !-------------------------------------------------------------------------------------
	!Initializes aggregating time step loop variable
    tflood=0.0
    
!	AFLTUDO(1,IT)=0.0 !inicializa a variável de areas inundadas no inland delta
    
	!Intertial Routing loop
    do while (tflood<dtflood0)
        
		!Calls the subroutine that computes the minimum value of timestep among catchments
		call flood_timestep
        
		!Needed to adjust routing time step at last loop interval (i.e. in order to not exceed water balance [DTP] time step)
        dtflood=min(dtflood,dtflood0-tflood)
		
		!Updates the loop variable
        tflood=tflood+dtflood
        
		!Write information about timestep, iteration and time interval on console
!		write(*,*) 'Flood inundation iT=',iT, 100.*tflood/dtflood0,'%',', dt=',dtflood,' s'
        
		! Calls the inertial routing equation:
        call flood_discharge
        ! Calls the continuity equation:
        call flood_continuity 
          
    enddo
    
	return
	endsubroutine
    
    
    