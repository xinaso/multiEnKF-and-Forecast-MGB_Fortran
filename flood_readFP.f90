    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This sub-routine reads the level x flooded area from PrePro (Inertial version).
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
    ! * COTA_AREA.FLP
    !
    ! reads
    !
    ! * COTA_AREA.FLP
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
    ! 2015.07.20 - 20 July 2016 (by Vinícius Siqueira))
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
    ! Variables and Parameters:
    ! *Variables declarations and routines calls are all commented below.
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    
    SUBROUTINE flood_readFP

    !  Variables and Parameters:
    USE VARS_MAIN
    USE VARS_INERC
    IMPLICIT NONE
    INTEGER aux,K,COUNTER
    INTEGER, PARAMETER:: dy_floodplain=1  !Increment in floodplain is assumed to be 1m         
    INTEGER, PARAMETER:: points_floodplain=100 !Standard number of elevation points in floodplain
    
    NPFL=points_floodplain !Setting the number of floodplain elevations
    
    !Opening the level x flooded area files    
    OPEN(FILSTAGE,FILE=INPUT_DIRECTORY // 'Flood_Stage.bin',STATUS='old',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') !Input binary file with interpolated rainfall data
           
    !The Structure of StageArea.bin file is as follows:
    !2 - Water Level elevations [IC = 1,2... NC] as INTEGER(KIND=4)
    !3 - Floodplain elevations  [i = 1, 2 ... points_floodplain] as REAL(KIND=8)
    
    K=1
    READ(FILSTAGE,REC=K)(ZFUNDOFL(IC),IC=1,NC) 
    !Fills the Z values of floodplain increment
    DO IC=1,NC
        DO aux=1,points_floodplain
            !For each point floodplain increment, computes the elevation
            ZFL(aux,IC)=INT(dy_floodplain*aux)+ZFUNDOFL(IC) !Multiplications between Integer and Real Must be verified 
        ENDDO
    ENDDO
    
    CLOSE (FILSTAGE) !Close water level binary file
    
    !Reads the Floodplain Areas
    !Values are in real*8. RECL must be multiplied by 2    
    OPEN(FILAREA,FILE=INPUT_DIRECTORY // 'Flood_Area.bin',STATUS='old',RECL=(2*points_floodplain),FORM='UNFORMATTED',ACCESS='DIRECT') !Input binary file with Floodplain area 
    
    K=1
    DO IC=1,NC
         READ(FILAREA,REC=K)(AFL(aux,IC),aux=1,points_floodplain)
         K=K+1
    ENDDO
    
    CLOSE (FILAREA) !Close floodplain area binary file
         
    !ABRE ARQUIVO DE FACES
    if (PSEUDO_TwoD_INDEX==1) then
    OPEN(42,FILE=INPUT_DIRECTORY // 'face.con',STATUS='OLD',action='READ')
    
    !Número da face / Minibacia 1 / Minibacia 2 / Ymin da fronteira (m) / DX entre as minibacias (m):
    aux=0
    DO WHILE(.NOT.EOF(42))
        aux=aux+1
	    READ(42,*) nFACECAT1(aux),nFACECAT2(aux),nFACEDX(aux)
	ENDDO
    CLOSE(42)
    nFACE=aux
    endif      
      
    RETURN
    END