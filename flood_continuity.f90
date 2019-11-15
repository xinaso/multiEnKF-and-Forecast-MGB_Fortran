	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This subroutine calculates the level and depth for each catchment from Continuity equation   (Inertial version).
    !
    !
    ! Usage:
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
    ! 2015.07.06 - 07 July 2015 (by Paulo Pontes)
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
    !    * Vinícius Alencar Siqueira 
    !    * Juan Martin Bravo 
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

	subroutine flood_continuity


	!--------------------------------------------------------------------------------------
	! Variables and parameters:
	use VARS_INERC
	use VARS_MAIN
	implicit none
		
	!-------------------------------------------------------------------------------------
        
    !$OMP PARALLEL NUM_THREADS(8)
    !$OMP DO PRIVATE(Nentradas,SumQup,Areajtab,yjtab,Volume_temp)
    do iC=1,nC
    
        IB=IBAC(iC) 
		IF((IB<SUBini).OR.(IB>SUBfim))CYCLE ! Manual controls for especialized calibration
    
        
            !Number of upstream catchments of IC
            Nentradas = MINIMONT(iC,1)
            
            !Sum of downstream IC flows
            if(MINIMONT(iC,1)==0)then
                SumQup=0.0
            else
                SumQup = SUM(Q2fl(MINIMONT(iC,2:1+Nentradas)))
            endif
            
            !to use lateral connections
            !Vol2(iC) = Vol1(iC)+dtflood*(SumQup+QCEL2(iC)-Q2fl(iC)+Q2viz(IC))-(EVQ(IC)*dtflood*Area2(iC)*1000000.0/(DTP*1000.))+(P(IC)*dtflood*Area2(iC)*1000000.0/(DTP*1000.))
            
            !without lateral connections in the continuity equation (just remove Q2viz(IC) from above equation
            !Vol2(iC) = Vol1(iC)+dtflood*(SumQup+QCEL2(iC)-Q2fl(iC))-(EVQ(IC)*dtflood*Area2(iC)*1000000.0/(DTP*1000.))+(P(IC)*dtflood*Area2(iC)*1000000.0/(DTP*1000.))
            
            !Vol2(iC) = Vol1(iC)+dtflood*(SumQup+QCEL2(iC)-Q2fl(iC))-(E0agua(IC)+DINFILT(IC)-P(IC))*dtflood*Area2(iC)*1000000.0/(DTP*1000.)                                                                                    
            !Vol2(iC) = Vol1(iC)+dtflood*(SumQup+QCEL2(iC)-Q2fl(iC)+Q2viz(IC)-(E0agua(IC)+DINFILT(IC)-P(IC))*Area2(iC)*1000000.0/(DTP*1000.))                                                         
             
            !Vol2(iC) = Vol1(iC)+dtflood*(SumQup+QCEL2(iC)-Q2fl(iC))-(E0agua(IC)-P(IC))*dtflood*Area2(iC)*1000000.0/(DTP*1000.)                                             
            Vol2(iC) = Vol1(iC)+dtflood*(SumQup+QCEL2(iC)-Q2fl(iC)+Q2viz(IC))-(E0agua(IC)+DINFILT(IC)-P(IC))*dtflood*Area2(iC)*1000000.0/(DTP*1000.)                                                          
            
            Vol2(iC) = max(Vol2(iC),0.0)
                                       
            !Interpolates the Area and Level from Volume
  !         CALL hunt(VTAB(:,iC),Vol2(iC),jtab(iC),ATAB(:,iC),Areajtab,ZTAB(:,IC),yjtab,NPFL(IC)+2)           
            CALL STAGEAREA
                  
            Area2(iC) = min(Areajtab,ACEL(iC))
            
            !Updates variables:
            Yfl(iC)=max(yjtab,ZTAB(1,IC)+0.001)
            !y2_fl=max(yjtab,ZTAB(1,IC)+0.001)
            !y2_fl=y2_fl+0.001
            
            !Calculates depth
            Hfl(iC)=Yfl(iC)-ZTAB(1,IC)
            !Hfl(iC)=y2_fl-ZTAB(1,IC)
            !Yfl(iC)=y2_fl       
            
            !Updates the volume
            Vol1(iC)=Vol2(iC)      
            
            
           !Added to include evaporation of open waters from floodplain dynamics 
           EVAPTUDO(IC)=EVAPTUDO(IC)+((E0agua(IC)*dtflood*Area2(iC)*1000000.0/(DTP*1000.))/(ACEL(IC)*1000)) !em mm/DTP 
           
            !Writes TWS for GRACE comparison
            if (tflood/dtflood0==1) then
                                     
                !TWS(iC) = (TWS(ic)+(SumQup*DTP)-(Q2fl(iC)*DTP)+(Q2viz(IC)*DTP)+(P(iC)*ACEL(iC)*1000.)-(max((EVAPTUDO(IC)*(ACEL(iC)-Area2(iC))*1000.),0.0))-(E0agua(IC)*Area2(iC)*1000.))                              
                TWS2(IC)=TWS2(IC)+real(Vol2(IC)/dble(ACEL(IC)*1000.0))
                
            end if

            
    enddo
	!$OMP END DO
	!$OMP END PARALLEL
	
	endsubroutine