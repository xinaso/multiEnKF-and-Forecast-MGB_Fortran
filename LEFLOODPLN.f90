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
    
    SUBROUTINE LEFLOODPLN

    !  Variables and Parameters:
    USE VARS_MAIN
    USE VARS_INERC
    IMPLICIT NONE
    integer aux
    
     !Lê índice se minibacia pertence a NigerInlandDelta ou não
    aux=0
      OPEN(43,FILE=INPUT_DIRECTORY // 'flag_inland_delta.txt',STATUS='OLD',action='READ')
        DO WHILE(.NOT.EOF(43))
        aux=aux+1
	    READ(43,*) INLAND_DELTA(AUX)
        ENDDO
      CLOSE(43)
        
      
    !Opening the level x flooded area file
    OPEN(41,FILE=INPUT_DIRECTORY // 'COTA_AREA.FLP',STATUS='OLD',action='READ')
    
    ! Reading data:
    READ(41,*)
    nPfl=0
    DO WHILE(.NOT.EOF(41))
	    READ(41,*) IC,ZFUNDOFL(IC),ZFL(NPFL(IC)+1,IC),AFL(NPFL(IC)+1,IC)
        
        if (northern_delta(ic)==1)then
            ZFUNDOFL(IC)=262
            ZFL(NPFL(IC)+1,IC)=ZFUNDOFL(IC)+NPFL(iC)+1
        endif
        NPFL(iC)=NPFL(IC)+1
	ENDDO
    CLOSE(41)
   
    !ABRE ARQUIVO DE FACES
    if (PSEUDO_TwoD_INDEX==1) then
    OPEN(42,FILE=INPUT_DIRECTORY // 'face.con',STATUS='OLD',action='READ')
    
    !Número da face / Minibacia 1 / Minibacia 2 / Ymin da fronteira (m) / DX entre as minibacias (m):
    aux=0
    DO WHILE(.NOT.EOF(42))
        aux=aux+1
	    READ(42,*) aux,nFACECAT1(aux),nFACEY1(aux),nFACECAT2(aux),nFACEY2(aux),nFACEDX(aux)
	ENDDO
    CLOSE(42)
    nFACE=aux
    endif

   
      
      !DO aux=1,NC
      !    if (NORTHERN_DELTA(aux)==1) nMan(aux)=0.06 !tests for NorthernDelta Manning coefficient
      !ENDDO
    RETURN
    END