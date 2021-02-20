      SUBROUTINE EXLINM(KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,
     1       VKGS,VKGD,VKGI,VFG,VCORG,KDLNC,KNEQ,VRES,VDLE)
C=====================================================================
C     pg 408
C=====================================================================
C     TO EXECUTE BLOCK 'LINM'                                          
C     ASSEMBLE AND SOLVE A LINEAR PROBLEM IN CORE                      
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)                                        
      COMMON/ASSE/NSYM,NKG,NKE,NDLE                                   
      COMMON/RESO/NEQ,NRES,MRES                                       
      COMMON/ES/M,MR,MP,M1,M2,M3                                      
      DIMENSION KLD(*),VDIMP(*),KLOCE(*),VCORE(*),VPRNE(*),VPREE(*),                    
     1 KNE(*),VKE(*),VFE(*),VKGS(*),VKGD(*),VKGI(*),VFG(*),VCORG(*),                  
     2 KDLNC(*),KNEQ(*),VRES(*),VDLE(*)                               
C----------------------------------------------------------------------
      REWIND M3                                                       
C                                                                      
C         ASSEMBLE KG                                                  
C                                                                      
C--------    SAVE UNMODIFIED VECTOR FG (BY B.C.) ON FILE M3               
      WRITE(M3) (VFG(I),I=1,NEQ)                                      
      IF(M.GE.2) WRITE(MP,2000) (VFG(I),I=1,NEQ)                      
2000  FORMAT(/' GLOBAL LOAD VECTOR NON MODIFIED BY B.C. (FG)'            
     1/(1X,10E12.5))                                                    
C--------    ASSEMBLE KG, MODIFY FG FOR THE B.C. AND SAVE THEM             
      CALL ASKG(KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,VKGS,                
     1  VKGD,VKGI,VFG,VDLE,VRES)                                        
      WRITE(M3) (VFG(I),I=1,NEQ)                                       
      WRITE(M3) (VKGS(I),I=1,NKG),(VKGD(I),I=1,NEQ)                    
      IF(NSYM.EQ.1) WRITE(M3) (VKGI(I),I=1,NKG)                        
C         PRINT KG AND FG                                               
      IF(M.LT.2) GO TO 20                                              
      WRITE(MP,2005) (VKGS(I),I=1,NKG)                                 
2005  FORMAT(/' GLOBAL MATRIX (KG)'/'     UPPER TRIANGLE'/               
     1 (1X,10E12.5))                                                   
      WRITE(MP,2010) (VKGD(I),I=1,NEQ)                                 
2010  FORMAT(' DIAGONAL'/(1X,10E12.5))                                 
      IF(NSYM.EQ.1) WRITE(MP,2020) (VKGI(I),I=1,NKG)                   
2020  FORMAT(' LOWER TRIANGLE'/(1X,10E12.5))                           
      WRITE(MP,2030) (VFG(I),I=1,NEQ)                                  
2030  FORMAT(/' GLOBAL LOAD VECTOR MODIFIED BY THE B.C. (FG)'            
     1 /(1X,10E12.5))                                                   
C                                                                       
C-----    SOLVE                                                         
C                                                                       
20    CALL SOL(VKGS,VKGD,VKGI,VFG,KLD,NEQ,MP,1,1,NSYM,ENERG)           
      IF(NSYM.NE.1) WRITE(MP,2035) ENERG                               
2035  FORMAT(15X,'ENERGY (ENERG)=',1E12.5)                             
      IF(M.LT.2) GO TO 30                                              
      WRITE(MP,2040) (VKGS(I),I=1,NKG)                                 
2040  FORMAT(/' TRIANGULARIZED MATRIX (KG)'/'  UPPER TRIANGLE'/    
     1  (1X,10E12.5))                         
      WRITE(MP,2010) (VKGD(I),I=1,NEQ)                         
      IF(NSYM.EQ.1) WRITE(MP,2020) (VKGI(I),I=1,NKG)                   
C-----    PIVOTS OF KG AND DETERMINANT                                  
30    CALL PRPVTS(VKGD)                                                
C-----    EVALUATE AND PRINT RESIDUAL VECTOR K.U - F                    
      IF(NRES.EQ.1) CALL PRRESD(VKGS,VKGD,VKGI,VFG,KLD,VRES)           
C-----    PRINT THE SOLUTION                                            
      WRITE(MP,2050)                                                   
2050  FORMAT(//' SOLUTION'//)                         
      CALL PRSOL(KDLNC,VCORG,VDIMP,KNEQ,VFG)                         
C
C         EVALUATE AND PRINT GRADIENTS (STRESSES)                       
C                                                                       
      CALL ASGRAD(KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,VKGS,   
     1  VKGD,VKGI,VFG,VDLE,VRES)                                     
C                                                                       
C         EVALUATE AND PRINT EQUILIBRIUM RESIDUAL VECTOR                
C                                                                       
C         READ VECTOR FG AND CHANGE SIGN                                
      REWIND M3                                                        
      READ(M3) (VRES(I),I=1,NEQ)                                       
      DO 40 I=1,NEQ                                                    
40    VRES(I)=-VRES(I)                                                 
C-----   ASSEMBLE THE RESIDUALS                                        
      CALL ASRESD(1,1,KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,               
     1  VKGS,VKGD,VKGI,VFG,VDLE,VRES,VRES(NEQ+1))                       
C-----    PRINT THE RESIDUALS                                           
      WRITE(MP,2060)                                                   
2060  FORMAT(//' EQUILIBRIUM RESIDUALS AND REACTIONS'//)                 
      CALL PRSOL(KDLNC,VCORG,VRES(NEQ+1),KNEQ,VRES)                     
      RETURN                                                            
      END                                                               