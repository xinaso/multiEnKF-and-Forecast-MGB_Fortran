    !*********************************************************************************
    !
    !  SUBROUTINE LECLIMED reads meteorological data
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine reads meteorological data

	!
	!	 LECLIMED is called inside subroutine MGB_Inercial.
	!
	!	 Saves global variable: 
	!     	XYC = coordinates of stations
	!		TAMM = monthly climatology of near surface temperature
	!       URMM = monthly climatology of near surface relative humidity (%)  
	!       SOLMM = monthly climatology of sunshine hours (hours/day)  
	!       VVMM = monthly climatology of near surface wind speed)  
	!       URMM = monthly climatology of near surface relative humidity (%)  
	!       PAMM = monthly climatology of near surface atmospheric pressure (kPa) 
	!
    !
    !  	Usage:
    !
    !    * no subroutine is called in this subroutine
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
    !    * module     VARS_MAIN   in      VARS_MAIN.f90
    !
    !	 opens
    !
    !    * Opens ACLIMED file  containing monthly climatological data.
    !
    !    reads
    !
    !    * Reads ACLIMED file containing monthly climatological data.
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
    !    2016.18.07 - 18 July 2016 (By: Vinícius Siqueira)    
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
    !  Variables and Parameters:
    !
    !   * Variables declarations and routines calls are all commented below.
	!	* All variables are global!?
    !
    !---------------------------------------------------------------------------------		

	SUBROUTINE LECLIMED_bin
	USE VARS_MAIN
	IMPLICIT NONE
	INTEGER IPCL,K,KK

    !Open File of coordinates 
	OPEN(FILMED,FILE=INPUT_DIRECTORY // ''//ACLIMED,STATUS='OLD')
   
	! Geographic coordinates of stations
	DO IPCL=1,NCLI
!		READ(FILMED,710)XYC(IPCL,1),XYC(IPCL,2)
		READ(FILMED,*)XYC(IPCL,1),XYC(IPCL,2) ! RP formato livre
	ENDDO

    CLOSE(FILMED)
    OPEN(FILCLIMATEbin,FILE=INPUT_DIRECTORY // 'medias_cru.bin',STATUS='old',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') !Input binary file with interpolated rainfall data
    
    !Binary file of CRU climatology. Order of values:
    !1 - Surface Temperature        [months: 1 ... 12]
    !2 - Relative Humidity          [months: 1 ... 12]
    !3 - Sunshine hours             [months: 1 ... 12]
    !4 - Wind Speed                 [months: 1 ... 12]
    !5 - Atmospheric pressure       [months: 1 ... 12]
    
    KK=1
    ! near surface air temperature (oC)
	DO K=1,12
	    READ(FILCLIMATEbin,REC=KK)(TAMM(IPCL,K),IPCL=1,NC)
        KK=KK+1
    ENDDO
    
	! near surface relative humidity (%)
	DO K=1,12
		READ(FILCLIMATEbin,REC=KK)(URMM(IPCL,K),IPCL=1,NC)
        KK=KK+1
	ENDDO
	
	! Sunshine hours (horas/dia)
 	DO K=1,12
 	    READ(FILCLIMATEbin,REC=KK)(SOLMM(IPCL,K),IPCL=1,NC)
        KK=KK+1
    ENDDO
	
	! Wind speed (m/s)
	DO K=1,12
		READ(FILCLIMATEbin,REC=KK)(VVMM(IPCL,K),IPCL=1,NC)
        KK=KK+1
	ENDDO
	
	! Atmospheric pressure (kPa)
	DO K=1,12
		 READ(FILCLIMATEbin,REC=KK)(PAMM(IPCL,K),IPCL=1,NC)
         KK=KK+1
	ENDDO

	CLOSE(FILCLIMATEbin)
	  
	RETURN
	
	END SUBROUTINE