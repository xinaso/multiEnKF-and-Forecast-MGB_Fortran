	!
    !  SUBROUTINE STAGEAREA is the routine that interpolates stage-area-volume values.
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine interpolates water volume after continuity computations. Vi=(Qin-Qout)*dt) +Vi-1
	!	 Identifies the corresponding water level and Flooded Area for actual Vi
	!
	!	 STAGEAREA is called inside subroutine Flood_Continuity.
	!
	!	 Saves global variables used for Inertial Routing in Flood_Continuity.
	! 		jtab: array index of stage-area-volume  
	! 		yjtab: Interpolated channel water level (m)
	! 		Areajtab: Interpolated Flooded Area (km²)
	!
	!
    !  	Usage:
    !
    !    CALL STAGEAREA
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
    !    * module     VARS_MAIN   in      VARS_MAIN.f90
	!    * module     VARS_INERC   in      VARS_INERC.f90
    !
    !	 opens
    !
    !    * Does not open files
    !
    !    reads
    !
    !    * Does not read files
    !
    !    creates
    !
    !    * Does not create files
    !    
    !
    !---------------------------------------------------------------------------------
    !  Licensing:
    !
    !    This code is distributed under the ...
    !
    !  Version/Modified: 
    !
    !    2016.15.7 - 25 July 2016 (By: Vinícius Alencar Siqueira and Rodrigo Paiva)    
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
	!	 * Vinícius Alencar Siqueira
    !
    !  Main Reference:
    !
    !    Walter Collischonn,
    !    Modelo de Grandes Bacias - Thesis
    !    Porto Alegre, 2001
    !    ISBN: XXXXXXXXXXX,
    !
    !---------------------------------------------------------------------------------
    !  Variables and Parameters:
    !
    !   *Variables declarations and routines calls are all commented below.
	!   *All variables are global
    !---------------------------------------------------------------------------------		

SUBROUTINE STAGEAREA
	
	USE VARS_INERC
	USE VARS_MAIN
    implicit none
			
	do
		!If the channel (+floodplain) volume is between actual and next index of stage-area-volume table:
	   if(  (Vol2(iC)>=VTAB( jtab(iC) ,iC) )   .AND.   ( Vol2(iC) <= VTAB( jtab(iC)+1 ,iC))   )then
	   
			!Finds the % over the actual index of stage-area-volume table:
	        Volume_temp = (Vol2(iC) - VTAB( jtab(iC) ,iC))/(VTAB( jtab(iC)+1 ,iC) - VTAB( jtab(iC) ,iC))
			!Interpolates values based on % above:
	        yjtab=ZTAB( jtab(iC) ,IC)+(ZTAB( jtab(iC)+1 ,IC)-ZTAB( jtab(iC) ,IC))* Volume_temp
	        Areajtab=ATAB( jtab(iC) ,IC)+(ATAB( jtab(iC)+1 ,IC)-ATAB( jtab(iC) ,IC))* Volume_temp
	        
			exit
	    
		!If the channel (+floodplain) volume is greater than next index of stage-area-volume table, increases index:		
	   elseif(Vol2(iC)>VTAB( jtab(iC)+1 ,iC))then
	        jtab(iC)=jtab(iC)+1
	        
			!Check for outside (upper) table bounds
	        if(jtab(iC)>(NPFL(IC)+1))then
	            
				!In this case, an extrapolation is needed. We consider a constant flooded area over the upper bound in ZTAB
				!Calculates the difference (dV) between the actual volume and max volume of stage-area-volume table
				!Level is computed as Zmax + dV/Amax, where A is assumed to be constant in this interval.
	            
	            yjtab = ZTAB( jtab(iC) ,IC) + (Vol2(iC) - VTAB( jtab(iC) ,iC)) / (ATAB( jtab(iC) ,IC)*1000000.0)
				!Areajtab remains unchanged
	            Areajtab=ATAB( jtab(iC) ,IC)
	            
				!Avoids access violation of array upper bounds (when calling for this subroutine again)  
	            jtab(iC)=jtab(iC)-1 
                exit !Exit subroutine %Vinicius Siqueira, 27/12/2016
	        endif
	   
	   !If the channel (+floodplain) volume is lower than actual index of stage-area-volume table, decreases index:		
	   else
	        jtab(iC)=jtab(iC)-1	
	   endif
	   
	       
    enddo

	END SUBROUTINE STAGEAREA
