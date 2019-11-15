	SUBROUTINE ALLOCA_VARSINERC(IOP)
	!SUBROTINA DE ALLOCA��O DE MEM�RIA DAS VARI�VEIS PRINCIPAIS
	USE VARS_MAIN
	use vars_calib !RP
    USE VARS_INERC
    
	IMPLICIT NONE	
	INTEGER IOP
    SAVE

	ALLOCV_CASE: SELECT CASE (IOP) !VERIFICA SE ALLOCA OU DEALLOCA
	CASE (0) ! ALLOCA
		ALLOCATE (HDFLAG(NC))               !C�digo para rodar hidrodin�mico ou inercial
		ALLOCATE (MINIMONT(NC,10))           !MATRIZ DE LIGA��ES TOPOLOGICAS PARA PROPAGA��O MODELO INERCIAL
		ALLOCATE (NPFL(NC),ZFUNDOFL(NC))    !N�mero de pontos do arquivo Cota-Area e N�vel de Fundo da plan�cie
		ALLOCATE (ZFL(100,NC))               !COTAS PARA TABELA COTA-AREA DA PLANICIE
		ALLOCATE (AFL(100,NC))               !�REAS PARA TABELA COTA-AREA DA PLANICIE
		ALLOCATE (HRIO(NC))                 !PROFUNDAIDE DE CALHA CHEIA DO RIO
		ALLOCATE (ZTAB(102,NC),VTAB(102,NC))  !COTAS E VOLUMES PARA TABELA COTA-VOLUME
		ALLOCATE (ATAB(102,NC))
		ALLOCATE (dtfloodIC(NC))            !Intervalo de tempo em cada minibacia
		ALLOCATE (Q2fl(NC),Vel2fl(NC))      !Vaz�o e velocidade calculada pelo modelo inercial em cada minibacia
		ALLOCATE (Qmont(3))                 !Vaz�o a montante de uma determinada minibacia
		ALLOCATE (Vol2(NC),Vol1(NC))        !Volumes no tempo t e t+1 em uma determinada minibacia
		ALLOCATE (Area2(nc))
		ALLOCATE (Hfl(NC),Yfl(NC),HAND(NC))    !Profundidade e N�vel de �gua em cada minibacia
		ALLOCATE (nFACECAT1(NC),nFACECAT2(NC),nFACEY1(NC),nFACEY2(NC),nFACEDX(NC),Q2face(nC),Q2viz(nC))   !VARIAVEIS DA FACE
		ALLOCATE (YRG(NUMHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
		ALLOCATE (HRG(NUMHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
		ALLOCATE (AFLRG(NUMHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
		!ALLOCATE (AFLTUDO(NC,NT)) !�REA ALAGADA EM TODAS AS MINIBACIAS EM TODOS OS INTERVALOS DE TEMPO
		ALLOCATE (jtab(NC))
		ALLOCATE (QRG_viz(NUMHIDG,NT)) !Hydrograph for recording
		!FMF 09/09/2015 
		ALLOCATE (nMan(NC))
        ALLOCATE(AFLTUDO(1,NT)) !TOTAL INLAND DELTA FLOODED AREA AT EACH IT
        ALLOCATE(S_AFLTUDO(1,NT)) !TOTAL INLAND DELTA FLOODED AREA AT EACH IT
        ALLOCATE(N_AFLTUDO(1,NT)) !TOTAL INLAND DELTA FLOODED AREA AT EACH IT
        ALLOCATE(INLAND_DELTA(NC)) !Flag se minibacia est� no inland delta 
        ALLOCATE(NORTHERN_DELTA(NC))
        ALLOCATE(SOUTHERN_DELTA(NC))
        ALLOCATE(DINFILT(NC)) !INFILTRATION FROM FLOODPLAIN TO SOIL
        ALLOCATE (STUDO_DELTA(NC,NT)) !soil moisture saturation everytime and everywhere	
        ALLOCATE(wwm_mean(NT)) !W/WM AVERAGE FOR EACH FLOW GAUGE
        allocate (TWS(NC)) ! Storage anomaly GRACE
        
	CASE (1) ! DEALLOCA
		DEALLOCATE (HDFLAG)
		DEALLOCATE (MINIMONT) 
		DEALLOCATE (NPFL,ZFUNDOFL)		
		DEALLOCATE (ZFL)
		DEALLOCATE (AFL)
		DEALLOCATE (HRIO)
		DEALLOCATE (ZTAB,VTAB)
		DEALLOCATE (ATAB)
		DEALLOCATE (dtfloodIC)
		DEALLOCATE (Q2fl,Vel2fl)
		DEALLOCATE (Qmont)
		DEALLOCATE (Vol2,Vol1)
		DEALLOCATE (Area2)
		DEALLOCATE (Hfl,Yfl)
		DEALLOCATE (nFACECAT1,nFACECAT2,nFACEY1,nFACEY2,nFACEDX,Q2face,Q2viz)
		DEALLOCATE (YRG,HRG,AFLRG)
		DEALLOCATE (AFLTUDO)
		DEALLOCATE (jtab)
		deallocate (TWS) ! Storage anomaly GRACE
                
		!FMF 09/09/2015 
		DEALLOCATE (nMan)
		
	CASE DEFAULT
		STOP ' ERRO: IOP DESCONHECIDO NO ROTINA ALLOCA_CALIB!!!'
	END SELECT ALLOCV_CASE


	END