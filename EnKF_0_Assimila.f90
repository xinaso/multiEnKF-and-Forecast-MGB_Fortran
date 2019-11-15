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
    !    2019.04.07 - 04 July 2019 (By: Sly Wongchuig)    
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
    !   This sub-routine uses the Data Assimilation technique ensemble Kalman filter (EnKF) by Evensen (2002) 
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
    ! W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2. These are the State variables of the model.
	! nStatVar=Dimension of the state variable vector
    ! *Variables declarations and routines calls are all commented below.
    
	! ********************************  Description of main input files:  ***********************
	! EnKF_MainInfo.txt - Main informations
    ! EnKF_correlation.txt or EnKF_correlation.bin - Matrix of correlations
    ! EnKF_Perror.txt - Parameters used to perturb the rainfall
    ! EnKF_VarStaError.txt - Parameters used to perturb the state variables of the model
    ! EnKF_Obs_Info.txt - Information of the discharge stations
    ! EnKF_Obs_Q.txt - Record of discharge
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    
    SUBROUTINE EnKF_0_Assimila

	USE VARS_MAIN
	USE EnKF_MOD
    USE VARS_INERC
	IMPLICIT NONE
    
    integer ibobo, iw
    real ran1,xbobo

	integer:: nTfim,iKK
	integer julday	
	INTEGER I,J,K,KHIDAss
    CHARACTER (10):: strIHIDGAss
	CHARACTER (50):: strARQUIVOAss
    CHARACTER (5):: strnEns
    REAL*4, ALLOCATABLE :: Q_grav(:), Yfl_grav(:)
    integer:: ii
    real*8:: trash1,trash2
    CHARACTER (8) trash3
    integer:: ikind
    integer:: xkind,ykind
    integer:: ckind
    integer:: TestGRACE       !2019

!*************************************************************************************
	! Read parameters of the EnKF method
	OPEN(555,FILE='.\input\EnKF_MainInfo.txt',STATUS='OLD',ACTION='READ')
	read(555,*)
	read(555,*) nEns
	read(555,*)
	read(555,*) iTini0_EnKF
	read(555,*)
	read(555,*) iTini_EnKF
	read(555,*)
    read(555,*) iTfim_EnKF
	read(555,*)
    read(555,*) nkindObs

    allocate(obstype_EnKF(nkindObs))
    allocate(nObs_t_kind(nkindObs))
    
    read(555,*)
	read(555,*) obstype_EnKF
    read(555,*)
	read(555,*) Local_INDEX
	read(555,*)
	read(555,*) MiniCorr
	CLOSE(555)

    !! Read the matrix of correlations among catchments                  !! Sly + Siqueira
    !IF (Local_INDEX==1) THEN
    !    OPEN(555,FILE='.\input\EnKF_correlation.txt',STATUS='OLD',ACTION='READ')
    !    ALLOCATE (Corr_EnKF(MiniCorr,MiniCorr))
    !        do yMini=1,MiniCorr
    !           read(555,*) (Corr_EnKF(yMini,xMini),xMini=1,MiniCorr)
    !        enddo
    !    CLOSE (555)
    !ENDIF
    
    ! Read the matrix of correlations among catchments

    IF (Local_INDEX==1) THEN
        WRITE(*,*) 'Reading correlation Matrix.... Please wait, this can take several minutes'
        ALLOCATE (Corr_EnKF(MiniCorr,MiniCorr))
        CALL LECORR_bin
    ENDIF
    

    ! Read parameters of the perturbations of Rainfall
	OPEN(555,FILE='.\input\EnKF_Perror.txt',STATUS='OLD',ACTION='READ')
	read(555,*)
	read(555,*) nlat,nlon
	read(555,*)
	read(555,*) latmin,lonmin
	read(555,*)
	read(555,*) res
	read(555,*)
	read(555,*) rh
	read(555,*)
	read(555,*) ErroP
	read(555,*)
	read(555,*) tal
	CLOSE(555)
	! Convert units from degrees to number of grids
	rh=rh/res
	
    ! Read parameters of the perturbations of State variables of the model
	OPEN(555,FILE='.\input\EnKF_VarStaError.txt',STATUS='OLD',ACTION='READ')
	read(555,*)
	read(555,*)
	read(555,*)
	read(555,*)
	read(555,*)
	read(555,*)
	read(555,*)
	read(555,*) rhVars
	read(555,*)
    read(555,*)
	read(555,*) ErroW, ErroVBAS, ErroVINT, ErroVSUP
	read(555,*)
	read(555,*) talVars
    read(555,*)
	read(555,*)
    read(555,*) FLAGPertVars
	CLOSE(555)
    ! Convert units from degrees to number of grids
    rhVars=rhVars/res
    
    ! Read main informations of the observations
    do ikind=1,nkindObs
        if (obstype_EnKF(ikind)==1.or.obstype_EnKF(ikind)==2) then
            ! Read parameters of discharge, also valid for discharge logarithm
	        OPEN(555,FILE='.\input\EnKF_Obs_Info.txt',STATUS='OLD',ACTION='READ')
	        read(555,*)
	        read(555,*)
	        read(555,*) nObs_Q
	        allocate(miniObs_Q(nObs_Q),Obs_Q(nObs_Q),FLAGObs_Q(nObs_Q),Erro_Q(nObs_Q),Erro_Qlog(nObs_Q))
	        read(555,*)
	        read(555,*) ErroQobs
	        read(555,*)
	        do iObs=1,nObs_Q
	            if (ErroQobs<0.0) then
	                read(555,*) miniObs_Q(iObs),FLAGObs_Q(iObs),Erro_Q(iObs)
                    Erro_Qlog(iObs)=Erro_Q(iObs)
	            else
	                read(555,*) miniObs_Q(iObs),FLAGObs_Q(iObs)
	                Erro_Q(iObs)=ErroQobs
                    Erro_Qlog(iObs)=ErroQobs
	            endif
	        enddo
	        CLOSE(555)
            ! Open the file of observations
            OPEN(5556,FILE='.\input\EnKF_Obs_Q.txt',STATUS='OLD',ACTION='READ')
            read(5556,*)
        endif
        
        if (obstype_EnKF(ikind)==3) then
            ! Read parameters of satelite altimetry
	        OPEN(55512,FILE='.\input\EnKF_Alt_SAT_Info.txt',STATUS='OLD',ACTION='READ')
	        read(55512,*)
	        read(55512,*) nObs_ASAT
	        allocate(XsecObs_ASAT(nObs_ASAT),Obs_ASAT(nObs_ASAT),MEAN_ASAT(nObs_ASAT))
	        read(55512,*)
	        read(55512,*) ErroASATobs
	        read(55512,*)
	        do iObs=1,nObs_ASAT 
	            read(55512,*) trash1,trash2,XSecObs_ASAT(iObs),MEAN_ASAT(iObs)
	        enddo   
	        CLOSE(55512)
            ! Abrir archivos de error de altimetria
                !open(5558600,FILE='.\input\EnKF_ERROR_Altimetry_SAT.alt',STATUS='OLD',ACTION='READ')
                !read(5558600,*)
            ! Open the file of anomaly of the observations
            OPEN(55586,FILE='.\input\EnKF_Obs_AltimetryAnom_SAT.alt',STATUS='OLD',ACTION='READ')
            read(55586,*)
        endif
        
        if (obstype_EnKF(ikind)==4) then
            ! Read parameters of Flooded water extent SWAF
            OPEN(555000,FILE='.\input\EnKF_AreaSWAF_Info.txt',STATUS='OLD',ACTION='READ')
	        read(555000,*)
	        read(555000,*) nObs_SWAF
            allocate(XminiObs_SWAF(nObs_SWAF),Obs_SWAF(nObs_SWAF))
            read(555000,*)
            read(555000,*) ErroSWAF
            read(555000,*)
	        do iObs=1,nObs_SWAF
                read(555000,*) trash1,trash2,XminiObs_SWAF(iObs)
            enddo
            close(555000)
            ! Open the file of observations
            OPEN(5558,FILE='.\input\EnKF_Obs_AreaSWAF.are',STATUS='OLD',ACTION='READ')
            read(5558,*)
        
        endif
        
        if (obstype_EnKF(ikind)==5) then
            ! Read parameters of the Flooded water extent from GIEMS product
            OPEN(5550002,FILE='.\input\EnKF_AreaGIEMS_Info.txt',STATUS='OLD',ACTION='READ')
	        read(5550002,*)
	        read(5550002,*) nObs_GIEMS
            allocate(XsubObs_GIEMS(nObs_GIEMS),Obs_GIEMS(nObs_GIEMS))
            read(5550002,*)
            read(5550002,*) ErroGIEMS
            read(5550002,*)
	        do iObs=1,nObs_GIEMS
                read(5550002,*) trash3,XsubObs_GIEMS(iObs)
            enddo
            CLOSE(5550002)
            ErroGIEMS=ErroGIEMS/100
            
            ! Open the file of observations
            OPEN(5558002,FILE='.\input\EnKF_Obs_AreaGIEMS.are',STATUS='OLD',ACTION='READ')
            read(5558002,*)
        endif
        
        if (obstype_EnKF(ikind)==6) then
            ! Read parameters of the Total water storage from GRACE product
            OPEN(55513,FILE='.\input\EnKF_GRACE_Info.txt',STATUS='OLD',ACTION='READ')
	        read(55513,*)
	        read(55513,*) nObs_GRACE
	        allocate(XminiObs_GRACE(nObs_GRACE),Obs_GRACE(nObs_GRACE),MEAN_GRACE(nObs_GRACE),XsubObs_GRACE(nObs_GRACE))
	        read(55513,*)
	        read(55513,*) ErroGRACE
	        read(55513,*)
	        do iObs=1,nObs_GRACE 
	            read(55513,*) trash3,XsubObs_GRACE(iObs),trash1,trash2,XminiObs_GRACE(iObs),MEAN_GRACE(iObs)
	        enddo  
            CLOSE(55513)
            ! Open the file of the anomalies of observations
            OPEN(55587,FILE='.\input\EnKF_Obs_AnomGRACE.gra',STATUS='OLD',ACTION='READ')
            read(55587,*)
                       
            !! open matrix of correlation for GRACE impact
            !ALLOCATE (CorrGRACE_EnKF(MiniCorr,MiniCorr))
            !CALL LECORR_GRACEbin
        endif
        
        if (obstype_EnKF(ikind)==7) then
            ! Read parameters of the Soil moisture from SMOS
            OPEN(55514,FILE='.\input\EnKF_SMOS_Info.txt',STATUS='OLD',ACTION='READ')
            CLOSE(55514)
            ! Open the file of observations
            open(55588,FILE='.\input\EnKF_Obs_SMOS.som',STATUS='OLD',ACTION='READ')
            read(55588,*)
        endif

        if (obstype_EnKF(ikind)==8) then
            ! Read parameters of SWOT water level product
            OPEN(555001,FILE='.\input\EnKF_SWOT_Alt_Info.txt',STATUS='OLD',ACTION='READ')
	        read(555001,*)
	        read(555001,*) nObs_SWOT
            allocate(XminiObs_SWOT_Alt(nObs_SWOT),Obs_SWOT_Alt(nObs_SWOT),Erro_SWOT_Alt(nObs_SWOT),meanERRO_SWOT_Alt(nObs_SWOT))
            read(555001,*)
	        do iObs=1,nObs_SWOT
	            read(555001,*) trash1,trash2,XminiObs_SWOT_Alt(iObs)
            enddo
            CLOSE(555001)
        endif
  
        if (obstype_EnKF(ikind)==8) then
            ! Open the file of observations
            OPEN(55582,FILE='.\input\EnKF_Y_Obs_SWOT.alt',STATUS='OLD',ACTION='READ')
            !read(55582,*)
            ! Open the file of errors of observations
            OPEN(55581,FILE='.\input\EnKF_ERROR_SWOT.alt',STATUS='OLD',ACTION='READ')
            !read(55581,*)
            ! Open the file of the mean of errors of observations
            OPEN(5558300,FILE='.\input\Mean_SWOT.alt',STATUS='OLD',ACTION='READ')
            !read(5558300,*)
        endif
   
        if (obstype_EnKF(ikind)==9) then
            ! Read parameters of SWOT flooded water extent product
            OPEN(555001,FILE='.\input\EnKF_SWOT_Area_Info.txt',STATUS='OLD',ACTION='READ')
	        read(555001,*)
	        read(555001,*) nObs_SWOT
            allocate(XminiObs_SWOT_A(nObs_SWOT),Obs_SWOT_A(nObs_SWOT))
            read(555001,*)
	        read(555001,*) Erro_SWOT_A                                  ! Segun documento oficial es de ~15%
            read(555001,*)
	        do iObs=1,nObs_SWOT
	            read(555001,*) trash1,trash2,XminiObs_SWOT_A(iObs)
            enddo
            close(555001)
            ! Open the file of observations
            OPEN(55584,FILE='.\input\EnKF_Area_Obs_SWOT.are',STATUS='OLD',ACTION='READ')
            !read(55584,*)
        endif
        
        if (obstype_EnKF(ikind)==10) then
            ! Read parameters of SWOT discharge product
            OPEN(555001,FILE='.\input\EnKF_SWOT_Q_Info.txt',STATUS='OLD',ACTION='READ')
	        read(555001,*)
	        read(555001,*) nObs_SWOT
            allocate(XminiObs_SWOT_Q(nObs_SWOT),Obs_SWOT_Q(nObs_SWOT))
            read(555001,*)
	        read(555001,*) Erro_SWOT_Q
            read(555001,*)
	        do iObs=1,nObs_SWOT
	            read(555001,*) trash1,trash2,XminiObs_SWOT_Q(iObs)
            enddo
            CLOSE(555001)
            ! Open the file of observations
            OPEN(55585,FILE='.\input\EnKF_Q_Obs_SWOT.vaz',STATUS='OLD',ACTION='READ')
            !read(55585,*)
        endif
  
    enddo
    
    
    ! Read the file to define the state variables to be updated
    OPEN(111,FILE='.\input\EnKF_FLAGVars.txt',STATUS='OLD',ACTION='READ')
    read(111,*)
    read(111,*)
    read(111,*) nVars
    allocate(FLAGVars(nVars))
    read(111,*)
    read(111,*)
        do iVars=1,nVars
            read(111,*) trash3,FLAGVars(iVars)
        enddo
    CLOSE(111)

	! Define the dimensions of the vector of the State variables
    nStatVar= FLAGVars(1)*nC*nU+FLAGVars(2)*nC+FLAGVars(3)*nC+FLAGVars(4)*nC+FLAGVars(5)*nC+FLAGVars(6)*nC+FLAGVars(7)*nC*nU+FLAGVars(8)*nC+FLAGVars(9)*nC+FLAGVars(10)*nC+FLAGVars(11)*nC+FLAGVars(12)*nC+FLAGVars(13)*NGC+FLAGVars(14)*NFC   !2019
    
    allocate(stateVec(nStatVar))
    
	allocate(Akf(nStatVar,nEns),Q2fl_ens(nC,nEns),Yfl_ens(nC,nEns),Area2_ens(nC,nEns),Vol1_ens(nC,nEns),TWS2_ens(nC,nEns))
    allocate(TWS2mean_ens(NGC,nEns))    ! 2019
    allocate(FLOODEDsum_ens(NFC,nEns))  ! 2019
    allocate(noise(nC,nEns))            ! Noise of Rainfall
    allocate(noiseVars(nC,nEns,4))      ! Noise of the State variables
    noise=0.0
    noiseVars=0.0

    !**********************************************
    ! Start the ensemble of the State variables
    call CondInic
    do ic=1,NC
       TWS2(IC)=real(Vol2(IC)/dble(ACEL(IC)*1000.0))          ! 2019
    ENDDO
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
    !TWS2=0.0
    !TWS2mean=0.0

    call EnKF_StatVars(1,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)
    !write(*,*) "EnKF_StatVars DONE"
    
    do iEns=1,nEns
        Akf(:,iEns)=StateVec
    enddo
    !*************************************************
	
	IT=0
	nTfim=nT

    ALLOCATE (Q_grav(NUMHIDG))
    ALLOCATE (Yfl_grav(NUMHIDG))

    
    ! Define and open the txt files of the outputs
    do ikind=1,nkindObs
        DO K=1,NUMHIDG
        WRITE(strIHIDGAss,'(I10)')IHIDG(K)
        WRITE(strnEns,'(I5)') nEns
            ! For discharge
            if (obstype_EnKF(ikind)==1) then
            strARQUIVOAss = 'ASSIM_Q_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==2) then
            strARQUIVOAss = 'ASSIM_LOG_Q_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==3) then
            strARQUIVOAss = 'ASSIM_ASAT_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==4) then
            strARQUIVOAss = 'ASSIM_AREA_SWAF_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==5) then
            strARQUIVOAss = 'ASSIM_GIEMS_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==6) then
            strARQUIVOAss = 'ASSIM_GRACE_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==7) then
            strARQUIVOAss = 'ASSIM_SMOS_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==8) then
            strARQUIVOAss = 'ASSIM_ALT_SWOT_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==9) then
            strARQUIVOAss = 'ASSIM_AREA_SWOT_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==10) then
            strARQUIVOAss = 'ASSIM_Q_SWOT_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            endif
 
            OPEN(80123+K,FILE=OUTPUT_DIRECTORY // ''//strARQUIVOAss,STATUS='UNKNOWN')
            
            ! For water level
            if (obstype_EnKF(ikind)==1) then
            strARQUIVOAss = 'ASSIM_Q_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==2) then
            strARQUIVOAss = 'ASSIM_LOG_Q_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==3) then
            strARQUIVOAss = 'ASSIM_ASAT_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==4) then
            strARQUIVOAss = 'ASSIM_AREA_SWAF_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==5) then
            strARQUIVOAss = 'ASSIM_GIEMS_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==6) then
            strARQUIVOAss = 'ASSIM_GRACE_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==7) then
            strARQUIVOAss = 'ASSIM_SMOS_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==8) then
            strARQUIVOAss = 'ASSIM_ALT_SWOT_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==9) then
            strARQUIVOAss = 'ASSIM_AREA_SWOT_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            else if (obstype_EnKF(ikind)==10) then
            strARQUIVOAss = 'ASSIM_Q_SWOT_Yfl_'//TRIM(adjustl((strIHIDGAss)))//'_'//TRIM(adjustl((strnEns)))//'_E'//'.TXT'
            endif
            
            OPEN(8012311+K,FILE=OUTPUT_DIRECTORY // ''//strARQUIVOAss,STATUS='UNKNOWN')
            
        ENDDO
    enddo
    
    OPEN(1973,FILE=OUTPUT_DIRECTORY // 'QTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')    
	!OPEN(1974,FILE=OUTPUT_DIRECTORY // 'EVAPTUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1975,FILE=OUTPUT_DIRECTORY // 'YTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    !OPEN(1977,FILE=OUTPUT_DIRECTORY // 'TWS2.bin',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1977,FILE=OUTPUT_DIRECTORY // 'TWSTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    !OPEN(1978,FILE=OUTPUT_DIRECTORY // 'FloodedArea.bin',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1978,FILE=OUTPUT_DIRECTORY // 'FLOODTUDO.MGB',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1982,FILE=OUTPUT_DIRECTORY // 'TWSmeanTUDO.MGB',STATUS='REPLACE',RECL=NGC,FORM='UNFORMATTED',ACCESS='DIRECT')  ! 2019
    OPEN(1983,FILE=OUTPUT_DIRECTORY // 'FLOODsumTUDO.MGB',STATUS='REPLACE',RECL=NFC,FORM='UNFORMATTED',ACCESS='DIRECT') ! 2019
               
    CALL SEMENTE(seed) ! Generates a random number based on the clock	
    
	do while (IT<nTfim)
		IT=IT+1

        ! (1) Simulation:
        write(*,*) "(1) Simulation"
            
            do iEns=1,nEns
                write(*,*) "iT= ", iT,"iEns= ",iEns
                
                ! Recover the state variables
                StateVec=Akf(:,iEns)  
                call EnKF_StatVars(2,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)
                ! Perturbate the State variables
                if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF.and.FLAGPertVars(1)==1) call RandomField_W
		        if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF.and.FLAGPertVars(2)==1) call RandomField_VBAS
                if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF.and.FLAGPertVars(3)==1) call RandomField_VINT
                if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF.and.FLAGPertVars(4)==1) call RandomField_VSUP
                
				! Filter bizarre values
                if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF.and.(FLAGPertVars(1)==1.or.FLAGPertVars(2)==1.or.FLAGPertVars(3)==1.or.FLAGPertVars(4)==1)) then
                    ! Correction of errors
                    do iC=1,nC
                        iB=IBAC(iC)
                        do iU=1,nU     
                            if (W(iC,iU)<0.0.or.W(iC,iU)>WM(iB,iU)) then
                                W(iC,iU)=min(WM(iB,iU),W(iC,iU))
                                W(iC,iU)=max(0.0,W(iC,iU))
                            endif
                        enddo
                    enddo
                                            
                    do iC=1,nC
                        VSUP(iC)=max(VSUP(iC),0.0)
                        VINT(iC)=max(VINT(iC),0.0)
                        VBAS(iC)=max(VBAS(iC),0.0)
                    enddo
                endif
                

		        ! Start the model 		    
		        JDIA=IDINI+IT-1 ! Verify the calendar
                
		        CALL CALDAT(JDIA,IMES,IDIA,IANO)
                
		        ! Read the Rainfall file
		        ITCHUVA=IT
		        CALL LECHUVA
                                
		        ! Perturb the Rainfall
		        if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF) call RandomField
		        ! Read the climatology file
		        TONTEM=TA
		        CALL LECLIMA

                
		        ! Calls catchment routing/discharge for lateral inflow
                DINFILT=0.0
                EVAPTUDO=0.0
		        CALL CELULA
		        ! Sums surface, subsurface and groundwater runoff
                do iC=1,nC
		            QCEL2(iC)=QBAS(iC)+QINT(iC)+QSUP(iC) 
                enddo
                ! Routing discharge
                if(hdFLAG0>0) CALL flood_inercial
                
                ! Average the TWS2                  2019
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
                    
                ! Save the State variables
                    call EnKF_StatVars(1,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)
                ! Store the ensemble of State variables
                Akf(:,iEns)=StateVec
                
                ! Store updated State variables
                Q2fl_ens(:,iEns)=Q2fl
                Yfl_ens(:,iEns)=Yfl
                Area2_ens(:,iEns)=Area2
                Vol1_ens(:,iEns)=Vol1
                TWS2_ens(:,iEns)=TWS2
                TWS2mean_ens(:,iEns)=TWS2mean
                FLOODEDsum_ens(:,iEns)=FLOODEDsum
                
               
                ! Store the same State variable vector into the ensemble matrix         
                if (iT<iTini0_EnKF.or.iT>=iTfim_EnKF) then
                    do ii=1,nEns
                        Akf(:,ii)=StateVec
                    enddo
                    exit
                endif 
            enddo
            
            DIAH(IT)=IDIA !stores day corresponding to iT time interval
		    MESH(IT)=IMES !stores month corresponding to iT time interval
		    ANOH(IT)=IANO !stores year corresponding to iT time interval

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       ! Store the State variables to be saved
        if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF) THEN
            ! Discharge
            DO IC=1,NC
                !QTUDO(iC,IT)=SUM(Q2fl_ens(iC,:))/nEns          !! Sly + Vinicius
                QTUDO(iC)=SUM(Q2fl_ens(iC,:))/nEns
                YTUDO(iC)=SUM(Yfl_ens(iC,:))/nEns
                TWSTUDO(iC)=SUM(TWS2_ens(iC,:))/nEns
                ALAGTUDO(iC)=SUM(Area2_ens(iC,:))/nEns
            ENDDO
            
            DO IGC=1,NGC
               TWSmeanTUDO(IGC)=sum(TWS2mean_ens(IGC,:))/nEns   ! 2019
            ENDDO
            
            DO IFC=1,NFC
               FLOODEDsumTUDO(IFC)=sum(FLOODEDsum_ens(IFC,:))/nEns  !2019
            ENDDO
            
            DO IC=1,NC
                AFLTUDO(1,IT)=AFLTUDO(1,IT)+ALAGTUDO(IC)
            ENDDO
            
             DO K=1,NUMHIDG 				! store discharge in catchments
                KHIDAss=IHIDG(K) 			! indexed catchment
                Q_grav(K)=QTUDO(KHIDAss) 	! stored in QRG
                Yfl_grav(K)=YTUDO(KHIDAss) 	! stored in Yfl
             enddo
             
        
        ELSE
            ! Discharge
            DO IC=1,NC
                QTUDO(iC)=Q2fl(iC)
                YTUDO(iC)=Yfl(iC)
                TWSTUDO(iC) =TWS2(iC)
                ALAGTUDO(iC)=Area2(iC)
            ENDDO
            
            DO IGC=1,NGC
               TWSmeanTUDO(IGC)=TWS2mean(IGC)       ! 2019
            ENDDO
            
            DO IFC=1,NFC
               FLOODEDsumTUDO(IFC)=FLOODEDsum(IFC)  ! 2019
            ENDDO
            
            DO IC=1,NC
                AFLTUDO(1,IT)=AFLTUDO(1,IT)+Area2(IC)
            ENDDO
            
            DO K=1,NUMHIDG 				! store discharge in catchments
                KHIDAss=IHIDG(K) 			! indexed catchment
                Q_grav(K)=Q2fl(KHIDAss) 	! stored in QRG
                Yfl_grav(K)=Yfl(KHIDAss) 	! stored in Hfl
            enddo

        ENDIF

        
        ! Writing files
        DO K=1,NUMHIDG
            KHIDAss=IHIDG(K)
            if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF) THEN
               WRITE(80123+K,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),QTUDO(KHIDAss)
            else
               WRITE(80123+K,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),Q2fl(KHIDAss)
            endif
        ENDDO
        
        DO K=1,NUMHIDG
            KHIDAss=IHIDG(K)
            if (iT>=iTini0_EnKF.and.iT<iTfim_EnKF) THEN
               WRITE(8012311+K,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),YTUDO(KHIDAss)
            else
               WRITE(8012311+K,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),Yfl(KHIDAss)
            endif
        ENDDO

        WRITE(1973,REC=IT)(QTUDO(IC),IC=1,NC) !Flow        
        !WRITE(1974,REC=IT)(EVAPTUDO(IC),IC=1,NC) !Evapo values
        WRITE(1975,REC=IT)(YTUDO(IC),IC=1,NC) !Y values
        WRITE(1977,REC=IT)(TWSTUDO(IC),IC=1,NC) !TWS GRACE
        WRITE(1978,REC=IT)(ALAGTUDO(IC),IC=1,NC) !Flooded Areas
        WRITE(1982,REC=IT)(TWSmeanTUDO(IGC),IGC=1,NGC) !TWSmean GRACE 2019
        WRITE(1983,REC=IT)(FLOODEDsumTUDO(IFC),IFC=1,NFC) !FLOODEDsum 2019
        
        
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        ! (2) Observations:
        write(*,*) "(2) Observations"
        nObs_t=0
        nObs_t_kind(:)=0
        nObs_t_total=0

        do ikind=1,nkindObs
                if (obstype_EnKF(ikind)==1) then
                            call EnKF_Obs_Q
                elseif (obstype_EnKF(ikind)==2) then
                            call EnKF_Obs_Qlog
                elseif (obstype_EnKF(ikind)==3) then
                            call EnKF_Obs_Alt_SAT
                elseif (obstype_EnKF(ikind)==4) then
                            call EnKF_Obs_SWAF
                elseif (obstype_EnKF(ikind)==5) then
                            call EnKF_Obs_GIEMS
                elseif (obstype_EnKF(ikind)==6) then
                            call EnKF_Obs_GRACE
                elseif (obstype_EnKF(ikind)==7) then
                            call EnKF_Obs_SMOS
                elseif (obstype_EnKF(ikind)==8) then
                            call EnKF_Obs_SWOT_Alt
                elseif (obstype_EnKF(ikind)==9) then
                            call EnKF_Obs_SWOT_Area
                elseif (obstype_EnKF(ikind)==10) then
                            call EnKF_Obs_SWOT_Q
                endif
             nObs_t_kind(ikind)=nObs_t
        enddo

        ! Sum total observations
        nObs_t_total=sum(nObs_t_kind(:))  
        
        write(*,*) nObs_t_total, "Total observations"
        
        if (iT>=iTini_EnKF.and.iT<iTfim_EnKF) then

            ! Allocate matrix used in the multi-observation scheme
            if (nObs_t_total==1) then
                allocate(Rkfm(1,1),Ekfm(1,nEns),Skfm(1,nEns),Dkfm(1,nEns),innovm(1),rhom(nStatVar,1),rhocm(1,1))
            elseif  (nObs_t_total>1) then
                allocate(Rkfm(nObs_t_total,nObs_t_total),Ekfm(nObs_t_total,nEns),Skfm(nObs_t_total,nEns),Dkfm(nObs_t_total,nEns),innovm(nObs_t_total),rhom(nStatVar,nObs_t_total),rhocm(nObs_t_total,nObs_t_total))
            endif
        
            if (nObs_t_total>=1) then
            ! Matrix of covariance of errors of observations (special for multi-observations)
                do xkind=1,nObs_t_total
                    do ykind=1,nObs_t_total
                        Rkfm(ykind,xkind)=0
                    enddo
                enddo
            ! Matrix of correlations 1
                do xkind=1,nObs_t_total
                    do ykind=1,nObs_t_total
                        rhocm(ykind,xkind)=0
                    enddo
                enddo
            endif
            
        endif

        ckind=0
        do ikind=1,nkindObs
                if (obstype_EnKF(ikind)==1) then
                            call EnKF_Measure
                elseif (obstype_EnKF(ikind)==2) then
                            call EnKF_Measure_log
                elseif (obstype_EnKF(ikind)==3) then
                            call EnKF_Measure_Alt_SAT
                elseif (obstype_EnKF(ikind)==4) then
                            call EnKF_Measure_SWAF
				elseif (obstype_EnKF(ikind)==5) then
                            call EnKF_Measure_GIEMS
                elseif (obstype_EnKF(ikind)==6) then
                            call EnKF_Measure_GRACE
				elseif (obstype_EnKF(ikind)==7) then
                            call EnKF_Measure_SMOS   
                elseif (obstype_EnKF(ikind)==8) then
                            call EnKF_Measure_SWOT_Alt
                elseif (obstype_EnKF(ikind)==9) then
                            call EnKF_Measure_SWOT_Area
                elseif (obstype_EnKF(ikind)==10) then
                            call EnKF_Measure_SWOT_Q
                endif

            if (iT>=iTini_EnKF .and. iT<iTfim_EnKF .and. nObs_t_kind(ikind)>=1)  then
                ! Matrix of covariance of errors of observations
                do xkind=1,nObs_t_kind(ikind)
                    do ykind=1,nObs_t_kind(ikind)
                        Rkfm(ykind + ckind,xkind + ckind)=Rkf(ykind,xkind)
                    enddo
                enddo
                ! Matrix of errors of observations
                do ykind=1,nObs_t_kind(ikind)
                    Ekfm(ykind + ckind,:)=Ekf(ykind,:)
                enddo
                ! Matrix of errors of the State variables
                do ykind=1,nObs_t_kind(ikind)
                    Skfm(ykind + ckind,:)=Skf(ykind,:)
                enddo
                ! Matrix of innovation for EnKF
                do ykind=1,nObs_t_kind(ikind)
                    Dkfm(ykind + ckind,:)=Dkf(ykind,:)
                enddo
                ! Matrix of innovation for SRKF
                do ykind=1,nObs_t_kind(ikind)
                    innovm(ykind + ckind)=innov(ykind)
                enddo
                ! Matrix of correlations 1
                do xkind=1,nObs_t_kind(ikind)
                    rhom(:,xkind + ckind)=rho(:,xkind)
                enddo
                ! Matrix of correlations 2
                do xkind=1,nObs_t_kind(ikind)
                    do ykind=1,nObs_t_kind(ikind)
                        rhocm(ykind + ckind,xkind + ckind)=rhoc(ykind,xkind)
                    enddo
                enddo

                ckind=ckind + nObs_t_kind(ikind)       
            endif
            
            if (iT<iTini_EnKF) then
                if (allocated(Ykf))    deallocate(Ykf)
                if (allocated(HAkf))    deallocate(HAkf)
                if (allocated(Skf))    deallocate(Skf)
                if (allocated(Dkf))    deallocate(Dkf)
                if (allocated(Ekf))    deallocate(Ekf)
                if (allocated(D2kf))    deallocate(D2kf)
                if (allocated(innov))    deallocate(innov)
                if (allocated(Rkf))    deallocate(Rkf)
                if (allocated(rho))    deallocate(rho)
                if (allocated(rhoc))    deallocate(rhoc)
            endif

        enddo

        if (nObs_t==0) cycle
        
        if (iT<iTini_EnKF.or.iT>=iTfim_EnKF) cycle
        
        ! (3) Data assimilation:
        if (iT>=iTini_EnKF.and.iT<iTfim_EnKF) write(*,*) "(3) Analysis"
        
        
        ! Verificar la existencia de GRACE !2019
        !TestGRACE=0
        !do ikind=1,nkindObs
        !    if (obstype_EnKF(ikind)==6) then
        !        TestGRACE=1
        !    endif
        !enddo
        
        if (iT>=iTini_EnKF.and.iT<iTfim_EnKF) then
            IF (Local_INDEX==1) THEN
                call analysis_new_local(Akf, Rkfm, Ekfm, Skfm, Dkfm, innovm, rhom, rhocm, nStatVar, nEns, nObs_t_total,.true., DBLE(0.9999),11,.true.)
            ELSE
                !!write(*,*) "Start subroutine analysis"
                !if (TestGRACE==1) then
                !    call analysis_newGRACE(Akf, Rkfm, Ekfm, Skfm, Dkfm, innovm, rhom, rhocm, nStatVar, nEns, nObs_t_total,.true., DBLE(0.9999), 11,.true.)
                !else
                    call analysis_new(Akf, Rkfm, Ekfm, Skfm, Dkfm, innovm, nStatVar, nEns, nObs_t_total,.true., DBLE(0.9999), 11,.true.)
                !endif
            ENDIF
        endif
        
        ! Check bizarre values for the State variables:
        if (iT>=iTini_EnKF.and.iT<iTfim_EnKF) write(*,*) "(4) Verification of errors"
        
        if (iT>=iTini_EnKF.and.iT<iTfim_EnKF) call EnKF_StatCorrection
                if (allocated(Rkfm))    deallocate(Rkfm)
                if (allocated(Ekfm))    deallocate(Ekfm)
                if (allocated(Skfm))    deallocate(Skfm)
                if (allocated(Dkfm))    deallocate(Dkfm)
                if (allocated(innovm))    deallocate(innovm)
                if (allocated(rhom))    deallocate(rhom)
                if (allocated(rhocm))    deallocate(rhocm)
                !write(*,*) "Se finalizó la verificacion de errores"
     enddo
! END OF THE ASSIMILATION SCHEME
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>     
        ! Close dicharge files
       DO K=1,NUMHIDG
            CLOSE(80123+K)
       ENDDO
        ! Close water level files
       DO K=1,NUMHIDG
            CLOSE(8012311+K)
       ENDDO

      !  ! Write total flooded water extent text file
        OPEN(19751,FILE=OUTPUT_DIRECTORY // 'TOTAL_FLOODED_AREAS.FLOOD',STATUS='UNKNOWN')
        DO IT=1,NT
		    WRITE(19751,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),AFLTUDO(1,IT)
        ENDDO
        CLOSE(19751)
        
    !! Sly + Vinicius
	CLOSE (1973)  !Close Streamflows file
	!CLOSE (1974)    ! Evapo
	CLOSE (1975)    ! Water level
    CLOSE (1977) !Close TWS
    CLOSE (1978)  !Close Flooded Area
    CLOSE (1982) !Close TWSmean 2019
    CLOSE (1983)    ! Close sum FLOODED
        
!*****************************************************************************************

close(5556)     ! Discharge
close(55586)    ! Satelite altimetry
close(5558)     ! SWAF
close(5558002)  ! GIEMS
close(55587)    ! GRACE
close(55588)    ! SMOS

close(55581)    ! SWOT error water level
close(55582)    ! SWOT obs water level
close(5558300)  ! SWOT mean obs's errors
close(55583)    ! SWOT errors of flooded water extent
close(55584)    ! SWOT obs of flooded water extent
close(55585)    ! SWOT obs of discharge


71	FORMAT(13I6,<NUMHIDG>F12.3)
75	FORMAT(I6,24F7.0)
777	FORMAT(2I7,<nC>F12.3)
778	FORMAT(<nC>F12.3)
779	FORMAT(I7,<nC>F12.3)

     
	RETURN
	END

