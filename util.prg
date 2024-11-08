SET PROCEDURE TO windowslogon.prg

Procedure getPath(firma,yer)
		_BoyutDrive=""
		_BoyutPath=""
		DO CASE
		CASE firma="local"
			a = wlogon("um1","\\192.168.239.128","123456")
			IF yer="boyut" then	
				_BoyutDrive="\\192.168.239.128\Boyut\"
				RETURN _BoyutDrive
			ELSE
				_BoyutPath="\\192.168.239.128\Boyut\data\"
				RETURN _BoyutPath
			ENDIF 
**SEL�UK	
		CASE firma="selcuk" 
			a = wlogon("um1","\\BSERVER","123456")
			**CEPDEPO SEL�UK
			IF DIRECTORY("\\BSERVER\BOYUTC\SELDATA") then
					a = wlogon("um1","\\BSERVER","123456")
				IF yer="boyut"
					_BoyutDrive="\\BSERVER\BOYUTC\"
					RETURN _BoyutDrive
				ELSE 
					_BoyutPath="\\BSERVER\BOYUTC\SELDATA\"
					RETURN _BoyutPath
				ENDIF 
				return "oldu"
			ELSE 
				IF yer="boyut" THEN 
					_BoyutDrive="\\BSERVER\BOYUT\"
					RETURN _BoyutDrive
				ELSE 
					_BoyutPath="\\BSERVER\BOYUT\DATA\"
					RETURN _BoyutPath
				ENDIF 
			ENDIF
		ENDCASE 
		
		return "olmad�"		
ENDPROC

PROCEDURE serviseSorsunmu(sepetkodu)



ENDPROC 



PROCEDURE otologyaz
PARAMETERS msj, gc
PRIVATE dsy
	dsy="c:\net\foxproSevkLoglari.txt" 
	IF FILE(dsy)  && Does file exist? 
	   fh = FOPEN(dsy,12)  && If so, open read/write
	ELSE
	   fh = FCREATE(dsy)  && If not, create it
	ENDIF

	IF fh < 0  && Check for error opening file
	   RETURN ""
	ELSE  && If no error, write to file
	   pnSize = FSEEK(fh,0,2) 
	   =FPUTS(fh, TTOC(DATETIME()))
	   =FPUTS(fh, "")
	   =FPUTS(fh, msj)
	   =FPUTS(fh, "*")
	ENDIF
	=FCLOSE(fh)  && Close file
ENDPROC   
	

	
Procedure ZamanSayi
parameter ptrh,psaat 
if type("ptrh")<>"D"
  ptrh  = boyutdate()  
  psaat = boyutzaman()  
endif
return (ptrh-ctod("01/01/2001"))*1440+val(left(psaat,2))*60+val(subs(psaat,4,2))

ENDPROC

proc BOYUTDATE
private xxDT
xxDt=DAte()
return xxDt
ENDPROC

proc boyutzaman
	private xxt,xxsaat,xxdk,xxsn
	xxt=round(seconds(),0)
	xxsn  =sifir(mod(xxt,60),2)
	xxdk  =sifir(int(mod(xxt,3600)/60),2)
	xxsaat=sifir(int(xxt/3600),2)
	return xxsaat+":"+xxdk+":"+xxsn
endproc


Procedure sifir
PARA num,len,dec
if type("dec")="N"
   if num=0
      retu repl("0",len)+"."+repl("0",dec)   
   else
      if mod(num,int(num))=0
         retu right(repl("0",len)+ltrim(str(num,len)),len)+"."+repl("0",dec)
      else
         retu right(repl("0",len)+ltrim(str(int(num),len)),len)+"."+str((10**dec)*mod(num,int(num)),dec)
      endif   
   endif   
else
   retu right(repl("0",len)+ltrim(str(num,len)),len)
endif  


procedure softseek
parameter anahtari,indexi
if type("INDEXI")="N"
   set order to indexi
ENDIF
if version()='FoxPro'
   set near on
   SEEK anahtari
   set near off
else
   anah=anahtari
   IF TYPE("anah")='C'
      uzun=len(anah)
      uz=uzun
      seek anah
      do while .not. found() .and. uz>0
         uz=uz-1
         anah=left(anah,uz)
         seek anah
      enddo
   ELSE
      SEEK anah
   ENDIF
endif
RETURN 

**CTOTIME FUNC
Procedure CtoTime
Param TNumC
PRIVATE TNUM, ALSEC, ALSAAT, ALDAK, ALSAN
if TNumC="  "
   ALSAAT=0
   ALDAK =0
else
   ALSAAT=mod(asc(subs(tnumc,1,1)),24)
   ALDAK=mod(asc(subs(tnumc,2,1)),60)
endif
return SIFIR(ALSAAT,2)+":"+SIFIR(ALDAK,2)


PROCEDURE logYaz
PARAMETERS dosyaadi, yazilacakLog
	dosyayolu = "c:\net\"+dosyaadi
	IF .not. FILE(dosyayolu) THEN 
		txtFile = FCREATE(dosyayolu)
		FCLOSE(txtFile)
	ENDIF 
	txtFile = FOPEN(dosyayolu,12) && read, write, no buffer
	FSEEK(txtFile,0,2) && go to end of file
	FWRITE(txtFile,yazilacakLog)
	FCLOSE(txtFile)
	RETURN 
ENDPROC 

*----------------------------------------------------------------------------------------------
Procedure FatnoCoz
Param pfno
Private dondgr,kk, nmrc
nmrc=.t.
do case
case pfno="CF".and.val(subs(pfno,3))<>0
  if nmrc
     dondgr=val(subs(pfno,3))
  else
     dondgr=pfno
  endif   

otherwise
  for kk=1 to len(pfno)
   if .not. subs(pfno,kk,1)$" 0123456789-"
      nmrc=.f.
      exit
   endif
  next
  if nmrc
     dondgr=pfno
  else
     dondgr=substr(pfno,5,3)+str(TexttoLong(pfno),7)
  endif   
endcase
return dondgr
*----------------------------------------------------------------------------------------------


*----------------------------------------------------------------------------------------------
Procedure FatnoStr
Param psr, pfno
return longtotext(pfno)+padr(psr,3)
*----------------------------------------------------------------------------------------------

*----------------------------------------------------------------------------------------------
Procedure LongToText
para nums
private hexs,ss,CRC,k1
ss=abs(nums)
hexs=""
for k1=1 to iif(nums<0,3,4)
   hexs=chr(mod(ss,210)+33)+hexs
   ss=int(ss/210)
endfor
hexs=iif(nums<0,chr(31),"")+hexs
return hexs
*----------------------------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------
Procedure TexttoLong
para hexs
PRIVATE K, SS, NUMS
nums=0
if hexs=space(4)
   return nums
endif

for k=iif(nums<0,2,1) to 4
    ss=asc(subs(hexs,k,1))-33
    if ss<210 .and. ss>=0
       nums=nums+ss*(210**(4-k))
    endif
endfor
NUMS=nums*iif(subs(hexs,1,1)=chr(31),-1,1)
return nums
*---------------------------------------------------------------------------------------------
Procedure sifir
PARA num,len,dec
if type("dec")="N"
   if num=0
      retu repl("0",len)+"."+repl("0",dec)   
   else
      if mod(num,int(num))=0
         retu right(repl("0",len)+ltrim(str(num,len)),len)+"."+repl("0",dec)
      else
         retu right(repl("0",len)+ltrim(str(int(num),len)),len)+"."+str((10**dec)*mod(num,int(num)),dec)
      endif   
   endif   
else
   retu right(repl("0",len)+ltrim(str(num,len)),len)
endif 

*---------------------------------------------------------------------------------------------

*----------------------------------------------------------------------------------
PROCEDURE etkstr
PARAMETERS LBOLGE,LSAAT,LSEVK,LECZANE,LKOD,LECZACI,LADRES1,LADRES2,LSEHIR,LSAYISAL,LRENK,LSEPET,LSEPET2,SEVKBILGI 
PRIVATE lstr,crlf
crlf=chr(13)+chr(10)
lstr=""
lstr=lstr + "[Etiket]"+crlf
lstr=lstr + "Width=10"+crlf
lstr=lstr + "Height=5"+crlf
lstr=lstr + "        "+crlf
lstr=lstr + "[lbolge]"+crlf   
lstr=lstr + "Text="+LBOLGE+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=10"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=14"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lsaat]"+crlf
lstr=lstr + "Text="+LSAAT+crlf
lstr=lstr + "Align=right"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=140"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=10"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lsevk]"+crlf
lstr=lstr + "Text="+LSEVK+crlf
lstr=lstr + "Align=right"+crlf
lstr=lstr + "X=50"+crlf
lstr=lstr + "Y=10"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=15"+crlf
lstr=lstr + "FontStyle=reverse"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[leczane]"+crlf
lstr=lstr + "Text="+LECZANE+crlf
lstr=lstr + "Align=center"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=40"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=16"+crlf
lstr=lstr + "FontStyle=Bold"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lkod]"+crlf
lstr=lstr + "Text="+LKOD+crlf
lstr=lstr + "Align=right"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=63"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=9"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[leczaci]"+crlf
lstr=lstr + "Text="+LECZACI+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=70"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=12"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[ladres1]"+crlf
lstr=lstr + "Text="+LADRES1+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=90"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=12"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[ladres2]"+crlf
lstr=lstr + "Text="+LADRES2+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=110"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=12"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lsehir]"+crlf
lstr=lstr + "Text="+LSEHIR+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=130"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=12"+crlf
lstr=lstr + "FontStyle=bold"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lsayisal]"+crlf
lstr=lstr + "Text="+LSAYISAL+crlf
lstr=lstr + "Align=right"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=165"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=9"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lrenk]"+crlf
lstr=lstr + "Text="+LRENK+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=155"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=10"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lsepet1]"+crlf
lstr=lstr + "Text="+LSEPET+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=70"+crlf
lstr=lstr + "Y=155"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=10"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[lsepet2]"+crlf
lstr=lstr + "Text="+LSEPET2+crlf
lstr=lstr + "Align=left"+crlf
lstr=lstr + "X=0"+crlf
lstr=lstr + "Y=165"+crlf
lstr=lstr + "FontName=verdana"+crlf
lstr=lstr + "FontSize=10"+crlf
lstr=lstr + ""+crlf
lstr=lstr + "[datamatrix]"+crlf
lstr=lstr + "Text="+SEVKBILGI+crlf
lstr=lstr + "Align=Right"+crlf
lstr=lstr + "X=300"+crlf
lstr=lstr + "Y=80"+crlf
lstr=lstr + "Size=3"+crlf
return lstr
ENDPROC 
*---------------------------------------------------------------------------------------------------------

Procedure YazSevkAcik
Parameters psevk
Private rack
rack=""
Do Case
Case psevk="2"
   rack="ACYL"
Case psevk="3"
   rack="MOTOKURYE"
Case psevk="4"
   rack="KENDY ALACAK"
Case psevk="5"
   rack="N�BET�Y"
Case psevk="6"
   rack="YLAVE"
Case psevk="7"
   rack="KARGO"
Case psevk="8"
   rack="FYRMA"
Case psevk="9"
   rack=irsrpr->aciklama
Case irsrpr->giren="eTic" .And. _satinackl>0
   rack="(e)"
Case irsrpr->Statu="KF"
   rack="(kf)"
Endcase
Return rack
*---------------------------------------------------------------------------------------------------------------------------

Procedure DTrkWstr
Parameters ptxt
Return Cpconvert(857,1254,ptxt)
*----------------------------------------------------------------------------------
Procedure WTrkDstr
Parameters ptxt
Return Cpconvert(1254,857,ptxt)

*----------------------------------------------------------------------------------------------
Procedure LongToText
para nums
private hexs,ss,CRC,k1
ss=abs(nums)
hexs=""
for k1=1 to iif(nums<0,3,4)
   hexs=chr(mod(ss,210)+33)+hexs
   ss=int(ss/210)
endfor
hexs=iif(nums<0,chr(31),"")+hexs
return hexs
*----------------------------------------------------------------------------------------------
Procedure TexttoLong
para hexs
PRIVATE K, SS, NUMS
nums=0
if hexs=space(4)
   return nums
endif

for k=iif(nums<0,2,1) to 4
    ss=asc(subs(hexs,k,1))-33
    if ss<210 .and. ss>=0
       nums=nums+ss*(210**(4-k))
    endif
endfor
NUMS=nums*iif(subs(hexs,1,1)=chr(31),-1,1)
return nums