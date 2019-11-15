	MODULE VARS_INERC
	!DECLARA��O DE VARI�VEIS RELACIONADAS AO MODELO DE PROPAGA��O INERCIAL
	IMPLICIT NONE
	SAVE

    !REAL*8,PARAMETER:: nMan=0.030             !Rugosidade considerada no modelo Inercial !FMF 09/09/2015 
    !REAL*8,PARAMETER:: alpha=0.3              !Coeficiente alpha utilizado para calculo do dt ideal do modelo Inercial
    REAL*8 alpha                               !Coeficiente alpha, moved to infoMGB.sim #VAS
    REAL*8,PARAMETER:: g=9.81                 !Coeficiente alpha utilizado para calculo do dt ideal do modelo Inercial
    REAL*8,PARAMETER:: dxart=5.               !dx artificial caso o comprimento do rio de uma minibacia seja muito pequeno Ver flood_timestep
    
    REAL*8,ALLOCATABLE:: nMan(:)            !Rugosidade considerada no modelo Inercial !FMF 09/09/2015 
    
    INTEGER,ALLOCATABLE:: HDFLAG(:)             !C�digo do modelo hidrodin�mico ou inercial
    INTEGER hdFLAG0                             !Flag do modelo hidrin�mico ou inercial
    INTEGER,ALLOCATABLE:: MINIMONT(:,:)         !MATRIZ COM RELA��ES TOPOL�GICAS DE CADA MINI-BACIA
    INTEGER,ALLOCATABLE:: NPFL(:)               !NUMERO DE PONTOS DA TABELA COTA-AREA EM CADA MINI-BACIA
    INTEGER,ALLOCATABLE:: ZFUNDOFL(:)           !COTA DO FUNDO DA PLANICIE DA TABELA COTA-�REA
    INTEGER,ALLOCATABLE:: ZFL(:,:)              !COTA DA PLANICIE PARA TABELA COTA-AREA
    REAL*8,ALLOCATABLE:: AFL(:,:)                 !�REA DA PLANICIE PARA TABELA COTA-AREA
    REAL*8,ALLOCATABLE:: HRIO(:)                  !PROFUNDIDADE DE CALHA CHEIA DO RIO
    REAL*8 HRX                                    !PROFUNDIDADE DE CALHA CHEIA DO RIO (VARI�VEL AUXILIAR) 
    REAL*8,ALLOCATABLE:: ZTAB(:,:)                !COTA PARA TABELA COTA-VOLUME DE CADA MINI-BACIA
    REAL*8,ALLOCATABLE:: VTAB(:,:)                !VOLUME PARA TABELA COTA-VOLUME DE CADA MINI-BACIA
    REAL*8,ALLOCATABLE:: ATAB(:,:)
    REAL*8 dtfloodmax,dtflood,dtflood0,tflood     !Vari�veis relacionadas ao intervalo de tempo do modelo inercial
    REAL*8,ALLOCATABLE:: dtfloodIC(:)
    REAL*8 hmaxfl                                 !Vari�vel que recebe a profundidade m�xima do vetor Hfl
    REAL*8,ALLOCATABLE:: Q2fl(:),Vel2fl(:)        !Vaz�o e velocidade calculada pelo modelo inercial em cada minibacia
    REAL*8,ALLOCATABLE:: Qmont(:),Vol2(:),Vol1(:) !Vaz�o a montante e Volumes no tempo t e t+1 em uma determinada minibacia
    REAL*8:: SumQup
    REAL*8,ALLOCATABLE:: Area2(:)
    REAL*8,ALLOCATABLE:: Hfl(:),Yfl(:)    !Profundidade e N�vel de �gua em cada minibacia
    REAL*8 nfroude                                !Numero de Froude para testes de regime supercritico
    REAL,ALLOCATABLE:: YRG(:,:)	 !ARMAZENA HIDROGRAMAS ONDE SE DESEJA GRAVAR
    REAL,ALLOCATABLE:: HRG(:,:)	 !ARMAZENA HIDROGRAMAS ONDE SE DESEJA GRAVAR
    REAL,ALLOCATABLE:: AFLRG(:,:)!ARMAZENA HIDROGRAMAS ONDE SE DESEJA GRAVAR
    REAL,ALLOCATABLE:: AFLTUDO(:,:) !�REA ALAGADA EM TODAS AS MINIBACIAS EM TODOS OS INTERVALOS DE TEMPO !UTILIZADA AQUI PARA FAZER A MEDIA DE INUNDA��O NA SUB-BACIA DO INLAND DELTA
    REAL,ALLOCATABLE:: S_AFLTUDO(:,:) !�REA ALAGADA SOUTHERN DELTA
    REAL,ALLOCATABLE:: N_AFLTUDO(:,:) !�REA ALAGADA NORTHERN DELTA
    REAL,ALLOCATABLE:: STUDO_DELTA(:,:) !soil moisture saturation everytime in the InlandDelta	
    REAL,ALLOCATABLE:: wwm_mean(:) !average soil moisture saturation in the InlandDelta
    REAL,ALLOCATABLE:: HAND(:)
                                                
    
    INTEGER YFLOOD_RECORD(10) !TIME INTERVALS TO SAVE WATER LEVELS (YFLOOD) FILES. MAXIMUM NUMBER OF 5 ITs 
    INTEGER TIMEINT_YFLOOD  !NUMBER OF TIME INTERVALS TO SAVE WATER LEVELS (YFLOOD) FILES
    
    !Vari�veis da rotina discharge
    real*8 z1,y1,z2,y2,Sflow,hflow
	real*8 dxflow,bflow,q0,q, xMan
    real*8 bflow_overland,xMan_overland         !Parameters for lateral connections, considering overland flow
    real*8 WSslope
	integer iCJus
    
    !Vari�veis da rotina continuity
    integer Nentradas, Kent, Jent,itab1,itab2,imeio
    real*8 y2_fl 
    
    INTEGER :: nFACE,iFACE,KCAT,KCAT2                             !NUMERO DE PONTOS DA TABELA DE FACES
    REAL*8,ALLOCATABLE:: nFACECAT1(:),nFACECAT2(:),Q2face(:),nFACEY1(:),nFACEY2(:),nFACEDX(:),Q2viz(:)                     !Vaz�o nas faces
    integer,allocatable:: jtab(:)
    REAL,ALLOCATABLE:: QRG_viz(:,:)	!Stores connection flows where you want to record them
    
    INTEGER,ALLOCATABLE:: INLAND_DELTA(:) !Flag se minibacia est� no inland delta 
    INTEGER,ALLOCATABLE:: NORTHERN_DELTA(:) !Flag se minibacia est� no inland delta 
    INTEGER,ALLOCATABLE:: SOUTHERN_DELTA(:) !Flag se minibacia est� no inland delta 
    
    REAL,ALLOCATABLE:: DINFILT(:) !INFILTRATION FROM FLOODPLAIN TO SOIL
    REAL DINFILTX !INFILTRATION FROM FLOODPLAIN TO SOIL
    
    real*8 areajtab, yjtab !Para c�lculo na curva cota-Area
    real*8 volume_temp !Para c�lculo na curva cota �rea
    real*8,allocatable::  TWS(:) !For comparison to GRACE data

        
    integer flag_WriteHot, flag_ReadHot !Flag for writing or reading hotstart values
    
    END MODULE
