    !*********************************************************************************
    !
    !  SUBROUTINE MODELO controls the main time loop in MGB-IPH and call routines
	!					that realize catchment and river routing
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine has the main loop of the MGB-IPH, the time loop, from iT=1,nT
    !     where iT is the time interval and nT is the number of time steps.
	!
	!	 For each time interval date info is set, then rainfall and climate data are
	!	   loaded through subroutines LECHUVA and LECLIMA. Afterwards catchment flow
	!	   generation is done using subroutine CELULA. Finally, river routing is 
	!	   achieved by calling (i) REDE, for Muskingum-Cunge Method or
	!	   (ii) flood_inertial, for a 1D Hydrodynamic Model (w/ inertia and pressure)
	!	
	!	At the end discharge time series are stored in :
	!		QRB: calculated discharge time series in all subbasins
	!		QRG: calculated discharge time series in catchments pre-defined by user
	!		QR:  calculated discharge time series in subbasin outlet w observed data
	!	Those are recorded in files at the end when returns to SIMULA
	!
	!
	!	 * iT value should not be changed inside subroutines!
	!
	!    * AUX_MOD module from full hydrodynamic is deactivated (commented)!
	!    * Hidrodinamico2 subroutine from full hydrodynamic is deactivated (commented)!
    !
    !  Usage:
    !
    !    CALL MODELO
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
	!	 * function CALDAT	      		in	  caldat.f90
    !    * module     VARS_MAIN   		in    VARS_MAIN.f90
    !    * module     VARS_INERC  		in    VARSINERC.f90  !Quais? 
 	!	 * subroutine LECHUVA	  		in	  LECHUVA.f90
	!	 * subroutine LECLIMA	  		in	  LECLIMA.f90
	!	 * subroutine CELULA	  		in	  CELULA.f90
	!	 * subroutine REDE	      		in	  REDE.f90	
	!	 * subroutine flood_inercial	in	  flood_inercial.f90
	!
	!    Deactivated:	
	!	 * module	  AUX_MOD        	in	  HD_AUX_MOD.f90
	!	 * subroutine Hidrodinamico2	in	  Hidrodinamico2.f90
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
    !    2014.25.11 - 25 November 2014 (By: Mino V. Sorribas)    
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
    !   * Variables declarations and routines calls are all commented below.
    !
    !---------------------------------------------------------------------------------
	SUBROUTINE MODELO

	USE VARS_MAIN
	USE VARS_INERC
	!USE AUX_MOD
	IMPLICIT NONE
	INTEGER K,KHID 					!indexes and counters
	INTEGER KC,JB,JC,KB,JCB,MWP 	!... same
      
	if (flag_WriteHot==3) then 
        !Then open file for read
         open(46,FILE=INPUT_DIRECTORY // 'HotstartList.prev',STATUS='OLD',ACTION='READ')
        
         WriteHotFile=0
         DO IT=1,NT !Check which ITs we need to write hotstart values
            READ(46,*)WriteHotFile(IT)    
         ENDDO
    endif
  
    ! Initialize
	IT=0  
    
    Write(*,*)'         IT   Max_Flow   Percent_Complete' 
    
    DO WHILE (IT<NT)
		
		IT=IT+1
		write(*,*)iT,maxval(QJ2),(100.*real(it)/real(NT)), ' %' !write time interval on screen			        
		
        !Write hotstart file for water balance and hydraulic state variables (only if Inertial routing is enabled) #VAS 
    if (flag_WriteHot >= 2) then
             call flood_WRITEHOT              
    end if
        
        !if(mod(it,100)==0)  write(*,*)100*IT/NT,' %'
                 
		JDIA=IDINI+INT((IT+HORAINI-1)/(86400./DTP)) 	! Gets calendar day (big number)
		CALL CALDAT(JDIA,IMES,IDIA,IANO)

		DIAH(IT)=IDIA !stores day corresponding to iT time interval
		MESH(IT)=IMES !stores month corresponding to iT time interval
		ANOH(IT)=IANO !stores year corresponding to iT time interval
		HORAH(IT)=MOD(IT,24)+HORAINI 	!hour of day corresponding to iT time interval
              
		! Read rainfall data for all catchments in iT time-step
		ITCHUVA=IT
		CALL LECHUVA
        	    
		DO KC=1,NC
			PM(KC)=PM(KC)+P(KC) !Cumulative rainfall (used later for average)
		ENDDO		
	
		! Reads climate data
		TONTEM=TA
		CALL LECLIMA
	
		! Calls catchment routing/discharge for lateral inflow
        DINFILT=0.0
        
        ! Initializes Evaptudo
        EVAPTUDO=0.0
        
		CALL CELULA		
		
		! Saves detailed info for soil state variables in file NOSOLO.HIG.
		! Uses JC index for catchment and JB index for URH of interest.
		IF(ICALIB.EQ.0)THEN !only if not calibrating.
			JB=1
			JC=1
			!JC=57
			WRITE(FILSOL,75)IT,P(JC),W(JC,JB),SI(JC,JB),ET(JC,JB),CAF(JC,JB),QBAS(JC),QINT(JC),QSUP(JC)
			JB=2
			JC=2
			WRITE(FILSOL2,75)IT,P(JC),W(JC,JB),SI(JC,JB),ET(JC,JB),CAF(JC,JB),QBAS(JC),QINT(JC),QSUP(JC)			

!			write(971,66) (E0agua(iC),iC=1,nC)
!			write(972,66) (E0topo(iC),iC=1,nC)
!			write(973,66) (E0sup(iC),iC=1,nC)

		ENDIF

	    ! Call main river routing routine using Muskingum-Cunge Method
		if(hdFLAG(1)==0)then									
		    CALL REDE
		endif     
		
		! Calculates lateral inflow 
        do ic=1,nc
		    QCEL2(IC)=QBAS(IC)+QINT(IC)+QSUP(IC) !sums surface, subsurface and 
		enddo

        ! Calls river routing routine using Inertial Method
        if(hdFLAG0>0)then
            CALL flood_inercial
        endif        

		! Stores calculated discharges in sub-basins with observed data - for calibration and assessment.
		DO K=1,NOBS
			KHID=IQOBS(K) 		! outlet catchment id
			QR(K,IT)=QJ2(KHID)  ! saves on QR(K,IT), that will be compared to QOBS in FOBJ routine.
        ENDDO
        
        ! Calculate the catchments envolved to calculate the averaged TWS
            DO IGC=1,NGC
                    nClocal=0           ! Calculate the number of catchments
                    TSWacum=0           ! Calculate the acumulated TWS
                    DO IC=1,NC
                        if (LargeB_GRACE(IC,IGC)/=0) then
                            nClocal=nClocal+1
                            TSWacum=TSWacum+TWS2(IC)
                        endif
                    ENDDO
                TWS2mean(IGC)=TSWacum/nClocal
            ENDDO
            
        ! Calculate the catchments envolved to calculate the acumulated Flooded Area2
            DO IFC=1,NFC
                    FLOODEDacum=0           ! Calculate the acumulated TWS
                    DO IC=1,NC
                        if (LargeB_FLOODED(IC,IFC)/=0) then
                            FLOODEDacum=FLOODEDacum+Area2(IC)
                        endif
                    ENDDO
                FLOODEDsum(IFC)=FLOODEDacum
            ENDDO
            
        !Modified in order to write results in disk for each IT #Vinicius Siqueira 21/12/2016
        !=====================================================================
                   
        
            DO IC=1,NC
                QTUDO(iC)=Q2fl(iC)                          !! Sly + Vinicius  !! Alocar 4 bytes
                YTUDO(iC)=Yfl(iC)
                TWSTUDO(iC) =TWS2(iC)
                ALAGTUDO(iC)=Area2(iC)
                RUNTUDO(iC)=QCEL2(iC)
                PefTUDO(iC)=Pef(iC)
            ENDDO
            
            DO IGC=1,NGC
               TWSmeanTUDO(IGC)=TWS2mean(IGC)       ! 2019
            ENDDO
                
            DO IFC=1,NFC
               FLOODEDsumTUDO(IFC)=FLOODEDsum(IFC)  ! 2019
            ENDDO
        
        WRITE(FILTUD,REC=IT)(QTUDO(IC),IC=1,NC) !Flow        
        WRITE(1974,REC=IT)(EVAPTUDO(IC),IC=1,NC) !Evapo values
        WRITE(1975,REC=IT)(YTUDO(IC),IC=1,NC) !Y values
        WRITE(1977,REC=IT)(TWSTUDO(IC),IC=1,NC) !TWS GRACE
        WRITE(1978,REC=IT)(ALAGTUDO(IC),IC=1,NC) !Flooded Areas
        WRITE(1980,REC=IT)(RUNTUDO(IC),IC=1,NC) !Runoff
        WRITE(1981,REC=IT)(PefTUDO(IC),IC=1,NC) ! P efective
        WRITE(1982,REC=IT)(TWSmeanTUDO(IGC),IGC=1,NGC) ! TWSmean
        WRITE(1983,REC=IT)(FLOODEDsumTUDO(IFC),IFC=1,NFC) ! FloodedAreasum
        !=====================================================================                
                  
        
		! Stores discharges for file ouput when in simulation model
		IF(ICALIB.EQ.0)THEN 	! only if it is not calibration
			
            !This part had to be removed because Subasins are in irregular positions in South America Model #VAS
			!DO KB=1,NB				! store discharge in sub-basin
			!	JCB=IEXUT(KB) 		! outlet catchment of KB-th subbasin
			!	QRB(KB,IT)=QJ2(JCB)
			!ENDDO
	
			DO K=1,NUMHIDG 				! store discharge in catchments defined in PARHIG.HIG
				KHID=IHIDG(K) 			! indexed catchment
				QRG(K,IT)=QJ2(KHID) 	! stored in QRG
				
			if(hdFLAG0>0)then !Stores the water level for inertial model 	           
                HRG(K,IT)=Hfl(KHID) ! stored in HRG                  
            endif     
				
			ENDDO
	
			! Store discharge by water origin (i.e. surface, subsurface, groundwater) in a specified catchment
			MWP=1 						!catchment (this is manual)
			QB(IT)=QJ2(MWP)*PJB2(MWP)
			QBI(IT)=QB(IT)+QJ2(MWP)*PJI2(MWP)
			QBIS(IT)=QBI(IT)+QJ2(MWP)*PJS2(MWP)
		ENDIF
        
	
    ENDDO !End time loop
	    

75	FORMAT(I6,8F10.4)

66	FORMAT(<nC>F10.4)

     
	RETURN
	END
