    !*********************************************************************************
    !
    !  SUBROUTINE LEVAR reads file with fixed parameters (ALBEDOS, IAF, RS, Z; mounthly) (File: ALBIAF.FIX)
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine reads file with fixed parameters (ALBEDOS, IAF, RS, Z; mounthly) (File: ALBIAF.FIX)

	!
	!	 LEVAR is called inside 1main.
	!
	!	 Saves discharge time series: 
	!     	ALB(IU,K) = albedo
	!     	RIAF(IU,K) = leaf area index.
	!     	Z(IU,K) = vegetation height.
	!     	RS(IU,K) = surface resistance.
	!
	!
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
    !    * file with calibration parameters (File: PARUSO.CAL). 
    !
    !    reads
    !
    !    * file with calibration parameters (File: PARUSO.CAL)
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
    !    2015.27.05 - 27 MAY 2015 (By: Rodrigo Paiva)    
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
    !   *Variables declarations and routines calls are all commented below.
	!	* All variables are global!?
    !
    !---------------------------------------------------------------------------------		
	SUBROUTINE LEVAR
	! File with fixed parameters for each HRU (ALBEDOS, IAF, RS, Z; mounthly) (File: ALBIAF.FIX)
	USE VARS_MAIN
	IMPLICIT NONE
	INTEGER K,R,I!,IU
    INTEGER M
	character (20) CABEV
	CHARACTER (5) AMES(12)
	OPEN(FILVAR,FILE=INPUT_DIRECTORY // 'ALBIAF.fix',STATUS='OLD')
	    
    DO R=1,NREG !Loop through Climate regions, for vegetation parameters #VAS 01/02/2017
        ! Read albedo:
        READ(FILVAR,71)cabev
	    READ(FILVAR,71)cabev !#Region
        READ(FILVAR,71)cabev
	    READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	    DO IU=1,NU
	    	READ(FILVAR,72)AUSO(IU),(ALB(IU,K,R),K=1,12)
	    	!WRITE(*,72)AUSO(IU),(ALB(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	    ENDDO
	    ! Read leaf area index:
	    READ(FILVAR,71)cabev
	    READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	    DO IU=1,NU
	    	READ(FILVAR,72)AUSO(IU),(RIAF(IU,K,R),K=1,12)
	    	!WRITE(*,72)AUSO(IU),(RIAF(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	    ENDDO
	    ! Read vegetation height:
	    READ(FILVAR,71)cabev
	    READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	    DO IU=1,NU
	    	READ(FILVAR,72)AUSO(IU),(Z(IU,K,R),K=1,12)
	    	!WRITE(*,72)AUSO(IU),(Z(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	    ENDDO
	    ! Read surface resistance:
	    READ(FILVAR,71)cabev
	    READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	    DO IU=1,NU
	    	READ(FILVAR,72)AUSO(IU),(RS(IU,K,R),K=1,12)
	    	!WRITE(*,72)AUSO(IU),(RS(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
        ENDDO
    END DO    
    
    CLOSE(FILVAR)
    
        DO R=1,NREG !Loop through Climate regions, for vegetation parameters and multibly by a factor 1.5 !2019
	        DO IU=1,NU
                DO K=1,12
                    ALB(IU,K,R)=ALB(IU,K,R)*1.5       ! Albedo
                    RIAF(IU,K,R)=RIAF(IU,K,R)*1.5      ! Leaf area index
                    Z(IU,K,R)=Z(IU,K,R)*1.5         ! Vegetation height
                    RS(IU,K,R)=RS(IU,K,R)*1.5        ! Surface resistance
                ENDDO
            ENDDO
        ENDDO

    !Open Climate Region file
    OPEN(1960,FILE=INPUT_DIRECTORY // 'climate_regions.fix',STATUS='OLD')
    READ(1960,71)cabev
    DO I=1,NC
        READ(1960,74)M,REGCLI(I)
    END DO
        
    
71  FORMAT(A10)
72	FORMAT(A10,12F5.2)
73  FORMAT(A10,12A5)
74  FORMAT(2I6)
    
	RETURN
	END
