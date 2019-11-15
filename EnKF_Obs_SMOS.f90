 !---------------------------------------------------------------------------------
    !  Licensing:
    !
	!Modelo de Grandes Bacias, South America version (MGB-SA). 
    !Copyright (C) 2019  Hidrologia de Grande Escala (HGE)
	!
    !This program is free software: you can redistribute it and/or modify
    !it under the terms of the GNU General Public License as published by
    !the Free Software Foundation, either version 3 of the License, or
    !any later version.
	!
    !This program is distributed in the hope that it will be useful,
    !but WITHOUT ANY WARRANTY; without even the implied warranty of
    !MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !GNU General Public License for more details.
	!
    !You should have received a copy of the GNU General Public License
    !along with this program.  If not, see <https://www.gnu.org/licenses/>.	
	!
	!---------------------------------------------------------------------------------
    !  Version/Modified: 
    !
    !    2019.15.01 - 15 January 2019 (By: Sly Wongchuig)    
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
    !    * Vinicius Alencar Siqueira
	!	 * Ayan Santos Fleischmann
	!	 * Sly Wongchuig Correa
    !
    !  Main Reference:
    !
    !    Rodrigo Paiva,
    !    Paiva, R.C.D. Collischonn, W., Bonnet, M.P., de Goncalves, L.G.G., Calmant Stéphane, Getirana, A., da Silva J. S., 2013.
    !    Assimilating in situ and radar altimetry data into a large-scale hydrologic-hydrodynamic model for streamflow forecast 
    !    in the Amazon. Hydrol. Earth Syst. Sci., 17, 2929-2946.
    
    !    Sly Wongchuig,
    !    Thesis
    !    Porto Alegre, 2019
    !
	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !   This sub-routine reads the soil moisture observations and calculates the number of observations
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
	! nObs_t=Dimension of temporal observations
    ! *Variables declarations and routines calls are all commented below.
    
	! ********************************  Description of main input files:  ***********************
    ! EnKF_Obs_SMOS.som - Record of soil moisture
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    subroutine EnKF_Obs_SMOS
	!------------------------------------------------------------------------------
	!-----------------------------------------------------------------------------
	! Declaration of variables:
    USE VARS_MAIN
    use VARS_INERC
	USE EnKF_MOD
	implicit none

	! Local variables:
	integer:: i,j,k

    read(55588,*) i,j,k,(Obs_SMOS(iObs),iObs=1,nObs_SMOS)

    
    if (iT>=iTini_EnKF) then
        ! Verifies the station with observations:
        nObs_t=0     
        do iObs=1,nObs_SMOS
            if (Obs_SMOS(iObs)/=-9999.0) then
            
                nObs_t=nObs_t+1
            endif
        enddo        
    endif

    
    end subroutine EnKF_Obs_SMOS