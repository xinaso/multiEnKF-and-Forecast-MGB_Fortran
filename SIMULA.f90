    !*********************************************************************************
    !
    !  SUBROUTINE SIMULA is the main routine for hydrological simulation
	!                    (wo calibration)
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
    !    CALL SIMULA
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
	!	 * module     IFPORT  			from (visual studio 2008)
    !    * module     VARS_MAIN   		in    VARS_MAIN.f90
	!    * module     VARS_CALIB   		in    VARS_CALIB.f90
    !    * module     VARS_INERC  		in    VARSINERC.f90  !Quais? 
 	!	 * subroutine CONDINIC	  		in	  CONDINIC.f90
	!	 * subroutine MODELO	  		in	  MODELO.f90
	!	 * subroutine FOBJ	  			in	  FOBJ.f90	
	!
    !
    !	 opens
    !
    !    * Opens QPROP.PRP  	   output  ascii  file with calculated discharge by origin (i.e. surface, subsurface, gw)
	!	 * Opens VAZAO.QCL 		   output  ascii  file with calculated discharge in subbasins defined in setup file
	!	 * Opens QTUDO.QBI 		   output  binary file with calculated discharge in all catchments
	!	 * Opens Qmes90.TXT 	   output  ascii  file with calculated Q90 in all catchments
	!	 * Opens Qmes95.TXT 	   output  ascii  file with calculated Q95 in all catchments
	!	 * Opens RESUMO_SIAQUA.TXT output  ascii  file for SIAQUA	
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
    ! 2015.06.21 - 21 June 2015 (By: Fernando Mainardi Fan)   
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

	SUBROUTINE SIMULA	
	USE VARS_MAIN
	USE VARS_CALIB
	USE VARS_INERC
	USE IFPORT

	IMPLICIT NONE
	INTEGER KB,K					!counters
	CHARACTER*10 ALFAQ(NC)			!auxiliar string
	REAL,ALLOCATABLE:: QSOR(:) 		!observed discharges (without "missing data") for sorting and exceedance probaility curve
	CHARACTER (10):: strIHIDG
	CHARACTER (50):: strARQUIVO	
	INTEGER MES,KSOR,IPOS90,IPOS95 				  !indexes and counter
	INTEGER CONTAMES(12)						  !counts number of existent data for mean calculation
	INTEGER IAUX,IPOS,IPERMA
	
	!REAL RUGMAN						! Manning bed roughness coeficient !RUGMAN IS DECLARED IN VARS_MAIN AND ALLOCA_VARS SUBROUTINES	!FMF 09/09/2015 
    INTEGER ITini
    INTEGER,ALLOCATABLE:: ANOaux(:)  ! RP abril/2013
    INTEGER:: i,j
    REAL AREA_SUBBACIA !AREA TOTAL DA SUB-BACIA DO INLAND DELTA
	REAL std,Qmean
	
	!Activate all basins to simulate
    !ATIVABACIA=1 

    
    ! Outputs
	!WRITE(FILLOG,*)'Writting outputs...'
    !Opening files before MODELO subroutine #Vinicius Siqueira
    OPEN(FILTUD,FILE=OUTPUT_DIRECTORY // 'QTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')    
    !OPEN(FILPRP,FILE=OUTPUT_DIRECTORY // 'QPROP.PRP',STATUS='UNKNOWN')
	!OPEN(FILHID,FILE=OUTPUT_DIRECTORY // 'VAZAO.QCL',STATUS='UNKNOWN')	
    !OPEN(FILTUD,FILE=OUTPUT_DIRECTORY // 'QTUDO.QBI',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
	!OPEN(1960,FILE=OUTPUT_DIRECTORY // 'Qmes90.TXT',STATUS='UNKNOWN',ACTION='WRITE')
	!OPEN(1961,FILE=OUTPUT_DIRECTORY // 'Qmes95.TXT',STATUS='UNKNOWN',ACTION='WRITE')
	!OPEN(1971,FILE=OUTPUT_DIRECTORY // 'RESUMO_SIAQUA.TXT',STATUS='UNKNOWN',ACTION='WRITE')
	!OPEN(1972,FILE=OUTPUT_DIRECTORY // 'QITUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') 
	!OPEN(1973,FILE=OUTPUT_DIRECTORY // 'QBASTUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
	OPEN(1974,FILE=OUTPUT_DIRECTORY // 'EVAPTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1975,FILE=OUTPUT_DIRECTORY // 'YTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
	!OPEN(1975,FILE=OUTPUT_DIRECTORY // 'WTUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    !OPEN(1976,FILE=OUTPUT_DIRECTORY // 'HAND_TUDO.QBI',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1977,FILE=OUTPUT_DIRECTORY // 'TWSTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1978,FILE=OUTPUT_DIRECTORY // 'FLOODTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    !OPEN(1979,FILE=OUTPUT_DIRECTORY // 'FlowVelocity.bin',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1980,FILE=OUTPUT_DIRECTORY // 'RUNTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')  
    OPEN(1981,FILE=OUTPUT_DIRECTORY // 'PefTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1982,FILE=OUTPUT_DIRECTORY // 'TWSmeanTUDO.MGB',STATUS='REPLACE',RECL=NGC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1983,FILE=OUTPUT_DIRECTORY // 'FLOODsumTUDO.MGB',STATUS='REPLACE',RECL=NFC,FORM='UNFORMATTED',ACCESS='DIRECT')
        
	! Main Routines
	WRITE(FILLOG,*)'Calling initial conditions...'
	CALL CONDINIC  ! Initial Conditions
    
    !Read hotstart file for water balance and hydraulic state variables (only if Inertial routing is enabled) #VAS 
    if (flag_ReadHot==1) then
            call flood_READHOT 
    end if
    
	WRITE(FILLOG,*)'Running main model routine...'
	CALL MODELO    ! Time Loop
	!WRITE(FILLOG,*)'Calculating objective functions...'
	!CALL FOBJ 	   ! Error/objective Functions Evaluation

    !Write hotstart file for water balance and hydraulic state variables (only if Inertial routing is enabled) #VAS 
    if (flag_WriteHot==1) then
            call flood_WRITEHOT 
    end if
    
    !!Close file units
	!CLOSE (FILBAC)
	!CLOSE (FILHID)
	!CLOSE (FILPRP)
	CLOSE (FILTUD)  !Close Streamflows file
	!CLOSE (1972)
	!CLOSE (1973)
	CLOSE (1974)    ! Evapo
	CLOSE (1975)    ! Water level
    !CLOSE (1976)  !Close HANDtudo file      
    CLOSE (1977) !Close TWS
    CLOSE (1978)  !Close Flooded Area
    !CLOSE (1979)  !Close Velocities
    CLOSE (1980)    ! Close runoff
    CLOSE (1981)    ! Close Pef
    CLOSE (1982)    ! Close Mean of TWS2
    CLOSE (1983)    ! Close sum FLOODED
    
    ! Record header in RESUMO_SIAQUA.TXT
!    write(1971,'(A240)')'      MINI       Q5       Q10       Q30       Q50       Q70       Q90       Q95       V10       V30       V50       V70       V90   LARGURA     Qmean     Qmax     Qmin    Qmax05ano    Qmax10ano     Qmax25ano' ! RP abril/2013
!
!	! Store string for catchment record (?!) - na real nao sei bem onde usa isso
!	DO IC=1,NC
!		IF(IC<NC)THEN
!			WRITE(ALFAQ(IC),'(I9,A1)')IC,';'
!		ELSE 	!if it is the last column, doesnt rec ";"
!			WRITE(ALFAQ(IC),'(I9)')IC
!		ENDIF
!	ENDDO
!
!	!WRITE(FILTUD,'(A11,<NC>A10)')'DD;MM; ANO;',(ALFAQ(IC),IC=1,NC)
!	
!	!-------------------------------------------------------------
!	! Record outputs in files
!	
!	DO IT=1,NT
!		! Record Discharge Time Series in NB predefined catchments outlets 
!		WRITE(FILHID,71)IT,(QRG(KB,IT),KB=1,NUMHIDG)
!		
!		! Record Special characteristics for a defined (below) sub-basin
!		IB=1 		!subbasin
!		WRITE(FILBAC,'(2F10.2,8F10.3)')WBM(IB,IT),PM2(IB,IT),SIM(IB,IT),EM(IB,IT),DSUM(IB,IT),DINM(IB,IT),DBAM(IB,IT)	! Water Balance Components Time Series 
!		WRITE(FILPRP,71)IT,QB(IT),QBI(IT),QBIS(IT)																		! Discharge by Origin (i.e. surface, subs, gw) Time Series
!		
!		! Record binary file with Discharge Time Series for all catchments
!		WRITE(FILTUD,REC=IT)(QTUDO(IC,IT),IC=1,NC)
!		WRITE(1972,REC=IT)(QITUDO(IC,IT),IC=1,NC)
!		WRITE(1973,REC=IT)(QBASTUDO(IC,IT),IC=1,NC)
!		WRITE(1974,REC=IT)(EVAPTUDO(IC,IT),IC=1,NC)
!		WRITE(1975,REC=IT)(WTUDO(IC,IT),IC=1,NC)
!
!!!ATENÇÃO, VERIFICAR AS LINHAS DE CÓDIGO ABAIXO COM A GALERA
!!!		DO IC=1,NC
!!!			exit	! ? what?
!!!
!!!			IF(IC<NC)THEN
!!!				WRITE(ALFAQ(IC),'(F9.2,A1)')QTUDO(IC,IT),';'
!!!			ELSE 
!!!				WRITE(ALFAQ(IC),'(F9.2)')QTUDO(IC,IT)
!!!			ENDIF
!!!			
!!!		ENDDO
!
!		!WRITE(*,'(10A10)')(ALFAQ(IC),IC=1,10)
!		JDIA=IDINI+IT-1 !VERIFICA QUAL É O DIA DO CALENDÁRIO 
!		CALL CALDAT(JDIA,IMES,IDIA,IANO)
!		!WRITE(FILTUD,'(I2,A1,I2,A1,I4,A1,<NC>A10)')IDIA,';',IMES,';',IANO,';',(ALFAQ(IC),IC=1,NC)
!	ENDDO
	!
	
	
	!-------------------------------------------------------------
	! Calculates mean annual precipitation by basin (long-term)
	PMB=0.0
	KPM=0
	PM=(PM/NT)*(365.*24.*3600./DTP)

	DO IC=1,NC
		IB=IBAC(IC)
    	PMB(IB)=PMB(IB)+PM(IC)
		KPM(IB)=KPM(IB)+1
	ENDDO
	DO IB=1,NB
		PMB(IB)=PMB(IB)/KPM(IB)
		WRITE(*,*)'Sub-basin ',IB,'   Annual Rainfall',PMB(IB)
		WRITE(FILLOG,*)'Sub-basin ',IB,'   Annual Rainfall',PMB(IB)
	ENDDO
				    
    !! Open files and record individual catchment discharge time series
    !DO K=1,NUMHIDG
    !    WRITE(strIHIDG,'(I10)')IHIDG(K)
    !    !strARQUIVO = 'SIM_'//TRIM(adjustl((strIHIDG)))//'.TXT'
    !    if(hdFLAG0>1)then 
    !        strARQUIVO = 'SIM_INERC_'//TRIM(adjustl((strIHIDG)))//'.TXT'
    !    elseif(hdFLAG0==0)then
    !        strARQUIVO = 'SIM_MC_'//TRIM(adjustl((strIHIDG)))//'.TXT'
    !    endif
    !    !OPEN(801,FILE=OUTPUT_DIRECTORY // ''//'SIM_'//ARQHID(K),STATUS='UNKNOWN',ACTION='WRITE')
    !    OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
    !        !WRITE(801,*)'   DIA   MES   ANO  HORA        Q_(m3/s)'
    !        DO IT=1,NT
    !            WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),QRG(K,IT)
    !        ENDDO
    !    CLOSE(801)
    !ENDDO
    
 !   if(hdFLAG0>1)then 
 !   ! Open files and record individual catchment water level time series
 !   ITini= 500 !367 !Should be 365 days due to intial conditions	
	!KSOR=NT-ITini+1 ! RP abril/2013
 !   DO K=1,NUMHIDG
 !       Qmean=sum(HRG(K,ITini:NT))/KSOR
 !       !std = sqrt((sum(HRG(K,ITini:NT)**2)-sum(HRG(K,ITini:NT))**2/size(HRG(K,ITini:NT)))/(size(HRG(K,ITini:NT))-1))     
 !       WRITE(strIHIDG,'(I10)')IHIDG(K)
 !       strARQUIVO = 'SIM_INERC_HflAnomaly_'//TRIM(adjustl((strIHIDG)))//'.TXT'
 !       OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
 !           DO IT=1,NT
 !               !WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),(HRG(K,IT)-Qmean)/std
 !               WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),(HRG(K,IT)-Qmean)
 !           ENDDO
 !       CLOSE(801)
 !   ENDDO
 !   endif
    
    ! if(hdFLAG0>1)then 
    !! Open files and record individual catchment water level time series
    !DO K=1,NUMHIDG
    !    WRITE(strIHIDG,'(I10)')IHIDG(K)
    !    strARQUIVO = 'SIM_INERC_Hfl_'//TRIM(adjustl((strIHIDG)))//'.TXT'
    !    OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
    !        DO IT=1,NT
    !            WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),HRG(K,IT)
    !            !write(*,*)Qmean,std,HRG(K,IT),KSOR
    !        ENDDO
    !    CLOSE(801)
    !ENDDO
    ! endif
    !
    !if(hdFLAG0>1)then 
    !! Open files and record individual connection discharges
    !DO K=1,NUMHIDG
    !    WRITE(strIHIDG,'(I10)')IHIDG(K)
    !    strARQUIVO = 'SIM_INERC_Connection_'//TRIM(adjustl((strIHIDG)))//'.TXT'
    !    OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
    !        DO IT=1,NT
    !            WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),QRG_viz(K,IT)
    !        ENDDO
    !    CLOSE(801)
    !ENDDO
    !endif

          
	RETURN

71	FORMAT(I10,<NUMHIDG>F15.3)
72	FORMAT(<NUMHIDG>F10.3)
	END
