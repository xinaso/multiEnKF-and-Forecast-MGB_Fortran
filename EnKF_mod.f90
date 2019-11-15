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
    !    2017.20.10 - 20 October 2017 (By: Sly Wongchuig)    
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
    !   This module is used to declarate the EnKF variables
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
    ! *Variables declarations and routines calls are all commented below.
    ! End of header
    !---------------------------------------------------------------------------------
    MODULE EnKF_mod
	!------------------------------------------------------------------------------------------------
	IMPLICIT NONE

    real:: rh,rhVars(4),ErroP,tal,talVars(4),ErroW,ErroVBAS,ErroVINT,ErroVSUP
    integer:: nlon,nlat
    real:: latmin,lonmin,res
    
    integer:: nEns,iEns,nObs_t,iObs,iVars,xMini,yMini,MiniCorr,Local_INDEX        !2018loc
    real,allocatable:: Akf(:,:),Akf0(:,:),Akf1(:,:),Akf_lag0(:,:),Q2fl_ens(:,:)
    integer,allocatable:: passaFLAG(:)

    real,allocatable:: noise(:,:)
    
    real,allocatable:: noiseVars(:,:,:)
    
    real,allocatable:: Dkf(:,:),Ekf(:,:),Skf(:,:),Ykf(:),HAkf(:,:)  !Julho
    
    real,allocatable:: rho(:,:),rhoc(:,:)                                         !2018loc
    !real,allocatable:: rhoGRACE(:,:),rhocGRACE(:,:)                                         !2019
    
        real*8, allocatable :: I1N(:,:)
        real*8, allocatable :: Ap(:,:)
        real*8, allocatable :: Reps(:,:)
        real*8, allocatable :: Reps2(:,:)
        real*8, allocatable :: Pe(:,:)
        real*8, allocatable :: Pet(:,:)
        real*8, allocatable :: Peinv(:,:)
        real*8, allocatable :: ZP(:,:)
        real*8, allocatable :: eig(:)
        real*8, allocatable :: ST(:,:)
    
    integer:: iTini0_EnKF,iTini_EnKF,iTini2_EnKF,nObs_Q,nVars
    integer:: iTfim_EnKF  
    integer:: nkindObs                                              !2018multi
    integer:: nObs_t_total                                          !2018multi
    
    integer,allocatable:: miniObs_Q(:),FLAGObs_Q(:)
    real,allocatable:: Obs_Q(:)
    real:: erroQobs

    real,allocatable:: stateVec(:)
    integer:: nStatVar
    integer:: seed
    
    integer FLAGPertVars(4)
    integer,allocatable:: FLAGVars(:)

    integer:: nObs_Z
    integer, allocatable:: obstype_EnKF(:),nObs_t_kind(:)                  !2018multi
    integer,allocatable:: XsecObs_Z(:)
    real,allocatable:: Obs_Z(:),BIAS_Z(:)
    real:: ErroZobs
    real, allocatable:: Vol1_ens(:,:), Yfl_ens(:,:), Area2_ens(:,:), TWS2_ens(:,:)
    real, allocatable:: TWS2mean_ens(:,:)                                   ! Sly 2019
    real, allocatable:: FLOODEDsum_ens(:,:)                                 ! Sly 2019

   
    ! 2019
    !! Variables ASAT
    integer:: nObs_ASAT
    integer,allocatable:: XsecObs_ASAT(:)
    real,allocatable:: Obs_ASAT(:), MEAN_ASAT(:)
    real:: ErroASATobs
    !real,allocatable:: ErroASATobs(:), Erro_ASAT_t(:)
    real,allocatable:: Erro_ASAT_t(:)
    
    !! Variables SMOS
    integer:: nObs_SMOS
    integer,allocatable:: XminiObs_SMOS(:)
    real,allocatable:: Obs_SMOS(:)
    real:: ErroSMOS
    real,allocatable:: Erro_SMOS_t(:)
    
    !! Variables GRACE
    integer:: nObs_GRACE
    integer,allocatable:: XminiObs_GRACE(:)
    integer,allocatable:: XsubObs_GRACE(:) !2019
    real,allocatable:: Obs_GRACE(:), MEAN_GRACE(:)
    real:: ErroGRACE
    real,allocatable:: Erro_GRACE_t(:)

    real,allocatable:: Erro_Q(:),Erro_Q_t(:)
    real,allocatable:: Erro_Qlog(:),Erro_Qlog_t(:)
    integer:: iTiniPrev_EnKF
    integer:: KK,lead
    integer,allocatable:: Data_ESP(:,:)
    integer:: iprev
    integer:: Anoini_ESP,Anofim_ESP
    
    !! Variables de SWAF
    integer:: nObs_SWAF
    integer,allocatable::XminiObs_SWAF(:)
    real,allocatable:: Obs_SWAF(:)
    real:: ErroSWAF
    real,allocatable:: Erro_SWAF_t(:)
    
    !! Variables de GIEMS
    integer:: nObs_GIEMS
    integer,allocatable::XsubObs_GIEMS(:)   ! 2019
    real,allocatable:: Obs_GIEMS(:)
    real:: ErroGIEMS
    real,allocatable:: Erro_GIEMS_t(:)
    
    
    integer:: nObs_SWOT
    integer,allocatable:: XminiObs_SWOT_Alt(:), XminiObs_SWOT_A(:), XminiObs_SWOT_Q(:)          !2018multi
    
    real,allocatable:: Obs_SWOT_Alt(:), Obs_SWOT_A(:), Obs_SWOT_Q(:)
    real,allocatable:: Erro_SWOT_Alt(:),Erro_SWOT_Alt_t(:), meanERRO_SWOT_Alt(:), meanERRO_SWOT_Alt_t(:)
    real,allocatable:: Erro_SWOT_A_t(:),Erro_SWOT_Q_t(:)
    real:: Erro_SWOT_A, Erro_SWOT_Q
    

    real*8,allocatable:: D2kf(:,:),innov(:),Rkf(:,:)
    REAL(4),ALLOCATABLE:: Corr_EnKF(:,:),CorrGRACE_EnKF(:,:)                       !2018loc
    
    real*8,allocatable:: Rkfm(:,:),Ekfm(:,:),Skfm(:,:),Dkfm(:,:),innovm(:),rhom(:,:),rhocm(:,:)    !2018multi
    
    INTEGER,PARAMETER:: CORRbin=20  !Binary file of matrix of correlation
    INTEGER,PARAMETER:: CORRGRACEbin=20  !Binary file of matrix of correlation for GRACE

       
	END MODULE