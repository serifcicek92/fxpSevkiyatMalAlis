SET PROCEDURE TO util.prg, windowslogon.prg
SET EXCLUSIVE OFF
**set classlib to myclasslib,_utility
DEFINE CLASS sevkiyatmalalis  AS Custom OLEPUBLIC
	HIDDEN BPATH
	HIDDEN STKPATH
	HIDDEN KLASOR
	HIDDEN _CEPNO
	HIDDEN _SUBENO
	HIDDEN _DEPOADI
	
**------------------------------------------------------------------------------------------------------------------------------	
	PROCEDURE definePaths
		
		this.STKPATH = getPath("selcuk","data")
		this.BPATH	= getpath("selcuk","boyut")
		
		BPATH = this.BPATH
		
		RESTORE FROM &BPATH.KLASOR.mem ADDITIVE 
		this.KLASOR = BPATH+klasor
				
		RESTORE FROM &BPATH.SUBENO.mem ADDITIVE 
		IF TYPE("_CEPNO") = "U" OR EMPTY(_CEPNO)            
			this._CEPNO = "01"        
		ELSE            
			this._CEPNO = _CEPNO        
		ENDIF
		this._SUBENO = SUBENO
		
		RESTORE FROM &BPATH.memfile.mem ADDITIVE
		IF TYPE("DEPOADI") = "U" OR EMPTY(DEPOADI)            
			this._DEPOADI = "Boyut"        
		ELSE            
			this._DEPOADI = DEPOADI
		ENDIF
		
		
		**wlogout()
		RETURN
	ENDPROC
**------------------------------------------------------------------------------------------------------------------------------
	FUNCTION init
		set talk off
		set cent on
		set dele on
		set safe off
		SET EXCLUSIVE OFF
		set esca off
		set notify off
		SET TABLEVALIDATE TO 0
		
	ENDFUNC

	PROCEDURE bolgeyiServistenTara(bolgeNo,okunanTarih, pOkunanTakipNo)
		this.definePaths()
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		KLASOR = this.KLASOR
		
		IF FILE(STKPATH+"sepet.dbf")
		   USE &STKPATH.sepet SHARED
		ELSE
		   USE &STKPATH.sepetler ALIAS sepet SHARED
		ENDIF
		SELECT 0

		IF "SELDATA" $ this.STKPATH THEN 
			USE &STKPATH.c100 SHARED
		    if empty(cdx(1))
		      set index to &STKPATH.c110
		    endif
		    set order to 1 
		    SELECT 0
		ELSE
			USE &KLASOR.c100 SHARED
		    if empty(cdx(1))
		      set index to &KLASOR.c110
		    endif
		    set order to 1 
		    SELECT 0
		ENDIF 

		USE &STKPATH.ftrcikis SHARED
		SET ORDER TO 1
		SELECT 0

		USE &STKPATH.ftrrpr SHARED
		IF EMPTY(CDX(1))
		   SET INDEX TO &STKPATH.iftrrpr, &STKPATH.iftrrprk, &STKPATH.iftrrprt
		ENDIF
		SET ORDER TO 0
		SELECT 0

		
		JSONdon = this.bolgeTara(bolgeNo,okunanTarih, pOkunanTakipNo)
		RETURN JSONdon
	ENDPROC

	**------------------------------------------------------------------------------------------------------------------------------	
	********* bolgeyi full tara	
	PROCEDURE bolgeTara(bolgeNo, okunanTarih, pOkunanTakipNo)
	    LOCAL JSONmetin, sepetstr, ftno, ftnovar, slistno
	    JSONmetin = "[]" && Baþlangýçta boþ bir JSON dizisi
	    
	    SELECT ftrrpr 
	    ftnovar = (TYPE("ftrrpr->faturano")="C")
	    
	    SET ORDER TO 3
	    *-- SEEK ve LOCATE ile sadece ilgili tarihteki kayýtlarý tara
	    SEEK DTOC(okunanTarih, 1)
	    
	    IF FOUND()
	        JSONmetin = "[" && Kayýt varsa dizi baþlat
	        
	        LOCATE WHILE DTOC(tarih, 1) = DTOC(okunanTarih, 1) FOR bolge = bolgeNo
	        
	        DO WHILE FOUND() .AND. !EOF()
	            ftno = iif(ftnovar, ftrrpr->faturano, STR(ftrrpr->no1, 7))
	            
	            SELECT c100
	            SET ORDER TO 1
	            SEEK ftrrpr.eczanekodu
	            
	            IF FOUND()
	                SELECT ftrcikis
	                SET ORDER TO 1
	                SEEK ftno
	                LOCATE WHILE faturano = ftno FOR tarih = ftrrpr.tarih .and. eczanekodu = ftrrpr.eczanekodu
	                
	                IF !FOUND()
	                    APPEND BLANK
	                    REPLACE faturano WITH ftno, ;
	                            eczanekodu WITH ftrrpr->eczanekodu, ;
	                            tarih WITH ftrrpr->tarih, ;
	                            kolisay WITH 0, posetsay WITH 0, buzluksay WITH 0, sepetsay WITH 0
	                ENDIF 
	                
	                *-- Filtreleme Kontrolü
	                IF EMPTY(sevksaati) .OR. (TYPE("pOkunanTakipNo") = "C" .AND. ALLTRIM(ftrrpr.takipno) == ALLTRIM(pOkunanTakipNo))
	                    
	                    JSONmetin = JSONmetin + "{" 
	                    JSONmetin = JSONmetin + "'ftrrec':'" + ALLTRIM(STR(RECNO("ftrrpr"))) + "',"
	                    JSONmetin = JSONmetin + "'hesapkodu':'" + ftrrpr.eczanekodu + "',"
	                    JSONmetin = JSONmetin + "'adi':'" + ALLTRIM(CPCONVERT(857, 1254, c100.adi)) + "',"
	                    JSONmetin = JSONmetin + "'cepno':'" + ALLTRIM(ftrrpr.cepno) + "',"
	                    JSONmetin = JSONmetin + "'semt':'" + ALLTRIM(CPCONVERT(857, 1254, c100.semt)) + "',"
	                    JSONmetin = JSONmetin + "'sehir':'" + ALLTRIM(CPCONVERT(857, 1254, c100.sehir)) + "',"
	                    JSONmetin = JSONmetin + "'bolge':'" + c100.bolge + "',"
	                    JSONmetin = JSONmetin + "'faturano':'" + FatnoCoz(ftno) + "',"
	                    JSONmetin = JSONmetin + "'takipno':'" + ftrrpr.takipno + "'," 
	                    slistno = TRANSFORM(IIF(TRIM(ftrrpr.sevklistno)=="", "", TexttoLong(ftrrpr.sevklistno)))
	                    JSONmetin = JSONmetin + "'sevklistno':'" + slistno + "',"
	                    JSONmetin = JSONmetin + "'kolisay':'" + LTRIM(STR(ftrcikis.kolisay)) + "',"
	                    JSONmetin = JSONmetin + "'posetsay':'" + LTRIM(STR(ftrcikis.posetsay)) + "',"
	                    JSONmetin = JSONmetin + "'buzluksay':'" + LTRIM(STR(ftrcikis.buzluksay)) + "',"
	                    JSONmetin = JSONmetin + "'sepetsay':'" + LTRIM(STR(ftrcikis.sepetsay)) + "',"
	                    JSONmetin = JSONmetin + "'sevksekli':'" + ftrrpr.sevksekli + "',"
	                    JSONmetin = JSONmetin + "'ftrsaati':'" + ctotime(subs(ftrrpr.saati, 1, 2)) + "',"
	                    JSONmetin = JSONmetin + "'sipsaati':'" + ctotime(subs(ftrrpr.saati, 4, 2)) + "',"
	                    JSONmetin = JSONmetin + "'ftrtarih':'" + TRANSFORM(ftrrpr.tarih) + "',"
	                    
	                    *-- SEPETLER
	                    sepetstr = ""
	                    SELECT sepet
	                    LOCATE FOR takipno = ftrrpr.takipno .and. eczanekodu = ftrrpr.eczanekodu
	                    DO WHILE FOUND() .AND. !EOF()
	                        IF kapak $ "123K"
	                            sepetstr = sepetstr + ALLTRIM(sepetkodu) + ","
	                        ENDIF
	                        CONTINUE 
	                    ENDDO 
	                    
	                    IF LEN(sepetstr) > 0
	                        sepetstr = LEFT(sepetstr, LEN(sepetstr) - 1)
	                    ENDIF
	                    
	                    JSONmetin = JSONmetin + "'sepetler':'" + ALLTRIM(sepetstr) + "'},"
	                ENDIF 
	            ENDIF
	            
	            SELECT ftrrpr
	            CONTINUE && LOCATE WHILE devam eder
	        ENDDO 
	        
	        *-- Eðer döngü sonunda JSONmetin hala "[" deðilse (yani kayýt eklenmiþse) son virgülü sil ve kapat
	        IF LEN(JSONmetin) > 1
	            JSONmetin = LEFT(JSONmetin, LEN(JSONmetin) - 1) + "]"
	        ELSE
	            JSONmetin = "[]"
	        ENDIF
	    ENDIF

	    RETURN JSONmetin 
	ENDPROC
**------------------------------------------------------------------------------------------------------------------------------	
	
	PROCEDURE okunanBilgiGetir(okunanTakipNo,okunanTarihg,okunanBolgeNo)
			LOCAL JSONmetin, ftno, ftnovar, slistno, sepetstr
			JSONmetin = "[]" && Baþlangýçta geçerli bir boþ dizi
			
			SELECT ftrrpr
			
			IF ftrrpr.bolge $ okunanBolgeNo THEN 
					ftnovar = (TYPE("ftrrpr->faturano")="C")
					ftno = iif(ftnovar, ftrrpr->faturano, STR(ftrrpr->no1,7))
					
					SELECT c100
					SET ORDER TO 1
					SEEK ftrrpr.eczanekodu
					
					IF FOUND() THEN
						JSONmetin = "[{" && Kayýt varsa dizi ve obje açýlýþý
						
						SELECT ftrcikis
						SET ORDER TO 1
						SEEK ftno
						LOCATE WHILE faturano = ftno FOR tarih = ftrrpr.tarih .and. eczanekodu = ftrrpr.eczanekodu
						
						** Bilgileri doldurma (Ortak kýsýmlarý dýþarýda toplamak daha temiz olur ama yapýný koruyorum)
						IF EMPTY(sevksaati) .and. FOUND() THEN 
							JSONmetin = JSONmetin + "'ftrrec':'" + ALLTRIM(STR(RECNO("ftrrpr"))) + "',"
							JSONmetin = JSONmetin + "'hesapkodu':'" + ftrrpr.eczanekodu + "',"
							JSONmetin = JSONmetin + "'adi':'" + ALLTRIM(CPCONVERT(857,1254,c100.adi)) + "',"
							JSONmetin = JSONmetin + "'kolisay':'" + LTRIM(STR(ftrcikis.kolisay)) + "',"
							JSONmetin = JSONmetin + "'posetsay':'" + LTRIM(STR(ftrcikis.posetsay)) + "',"
							JSONmetin = JSONmetin + "'buzluksay':'" + LTRIM(STR(ftrcikis.buzluksay)) + "',"
							JSONmetin = JSONmetin + "'sepetsay':'" + LTRIM(STR(ftrcikis.sepetsay)) + "',"
						ELSE
							JSONmetin = JSONmetin + "'ftrrec':'" + TRANSFORM(RECNO("ftrrpr")) + "',"
							JSONmetin = JSONmetin + "'hesapkodu':'" + ftrrpr.eczanekodu + "',"
							JSONmetin = JSONmetin + "'adi':'" + ALLTRIM(CPCONVERT(857,1254,c100.adi)) + "',"
							
							SET DELETED OFF 
							SELECT irsrpr
							IF TYPE("sepetirsrecno") != "U" .AND. RECCOUNT() >= sepetirsrecno .AND. sepetirsrecno > 0 THEN 
								GO sepetirsrecno
								JSONmetin = JSONmetin + "'kolisay':'" + LTRIM(STR(irsrpr.kolisay)) + "',"
								JSONmetin = JSONmetin + "'posetsay':'" + LTRIM(STR(irsrpr.posetsay)) + "',"
								JSONmetin = JSONmetin + "'buzluksay':'" + LTRIM(STR(irsrpr.buzluksay)) + "',"
								JSONmetin = JSONmetin + "'sepetsay':'" + LTRIM(STR(irsrpr.sepetsayi)) + "',"
							ELSE 
								JSONmetin = JSONmetin + "'kolisay':'0','posetsay':'0','buzluksay':'0','sepetsay':'0',"
							ENDIF
							SET DELETED ON
						ENDIF 

						** Geri kalan ortak alanlar
						JSONmetin = JSONmetin + "'cepno':'" + ALLTRIM(ftrrpr.cepno) + "',"
						JSONmetin = JSONmetin + "'semt':'" + ALLTRIM(CPCONVERT(857,1254,c100.semt)) + "',"
						JSONmetin = JSONmetin + "'sehir':'" + ALLTRIM(CPCONVERT(857,1254,c100.sehir)) + "',"
						JSONmetin = JSONmetin + "'bolge':'" + c100.bolge + "',"
						JSONmetin = JSONmetin + "'faturano':'" + FatnoCoz(ftno) + "',"
						JSONmetin = JSONmetin + "'takipno':'" + ftrrpr.takipno + "',"
						
						slistno = TRANSFORM(IIF(TRIM(ftrrpr.sevklistno)=="", "", TexttoLong(ftrrpr.sevklistno)))
						JSONmetin = JSONmetin + "'sevklistno':'" + slistno + "',"
						JSONmetin = JSONmetin + "'sevksekli':'" + ftrrpr.sevksekli + "',"
						JSONmetin = JSONmetin + "'ftrsaati':'" + ctotime(subs(ftrrpr.saati,1,2)) + "',"
						JSONmetin = JSONmetin + "'sipsaati':'" + ctotime(subs(ftrrpr.saati,4,2)) + "',"
						JSONmetin = JSONmetin + "'ftrtarih':'" + TRANSFORM(ftrrpr.tarih) + "',"

						** SEPETLERÝ GETÝR (GÜVENLÝ YÖNTEM)
						sepetstr = ""
						SELECT sepet
						LOCATE FOR takipno = ftrrpr.takipno .and. eczanekodu = ftrrpr.eczanekodu
						DO WHILE FOUND() .AND. !EOF()
							IF kapak $ "123K"
								sepetstr = sepetstr + ALLTRIM(sepetkodu) + ","
							ENDIF
							CONTINUE 
						ENDDO 
						
						** Virgül temizleme kontrolü
						IF LEN(sepetstr) > 0
							sepetstr = LEFT(sepetstr, LEN(sepetstr) - 1)
						ENDIF
						
						JSONmetin = JSONmetin + "'sepetler':'" + ALLTRIM(sepetstr) + "'}]"
						SELECT ftrrpr
					ENDIF 
			ENDIF 
			
			** Eðer hala "[]" ise veya bozuksa C# için temiz bir dizi dön
			RETURN JSONmetin
	ENDPROC
**------------------------------------------------------------------------------------------------------------------------------	
**Dönüþ Yap
	PROCEDURE donusYap(plkNo)
		this.definePaths()
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		USE &STKPATH.sevklist SHARED
		SET ORDER TO 3 desc
		SEEK plkNo
		LOCATE WHILE plakano = plkNo FOR donustarih<CTOD("1/1/1") 
		**.and. sevktarihi>DATE()-2
		IF FOUND() then
			REPLACE donustarih WITH DATE(), donussaati WITH TIME(1)
			RETURN "true" 
		ELSE 
			RETURN "false"
		ENDIF 
	ENDPROC 
	

**------------------------------------------------------------------------------------------------------------------------------	
** Çýkýþ Yapmýþ Plakanýn Kaldýðý Yerden Devam Etmesi Ýçin Verileri Getirir
	PROCEDURE PlakaKaldigiYerdenGetir(plkNo)
		this.definePaths()
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		KLASOR = this.KLASOR

		** 1. AKTÝF SEVK LÝSTESÝNÝ BUL
		USE &STKPATH.sevklist SHARED
		SELECT 0
		SELECT sevklist
		
		* Ýndekse (SEEK) güvenmiyoruz, boþluklarý silerek tam eþleþme ve "dönüþü boþ (aktif)" kontrolü yapýyoruz:
		LOCATE FOR ALLTRIM(plakano) == ALLTRIM(plkNo) .and. EMPTY(donustarih)
		
		IF !FOUND()
			wlogout()
			RETURN "false"
		ENDIF
		
		** --- YENÝ: 3 GÜN KONTROLÜ ---
		** DATE() - sevktarihi iþleminin sonucu gün farkýný verir
		IF (DATE() - sevklist.sevktarihi) > 3
			wlogout()
			RETURN "eskisevkiyat"
		ENDIF
		
		** ----------------------------
		aktifSevkNo = sevklist.sevklistno
		
		** TABLOLARI AÇ
		IF FILE(STKPATH+"sepet.dbf")
		   USE &STKPATH.sepet SHARED
		ELSE
		   USE &STKPATH.sepetler ALIAS sepet SHARED
		ENDIF
		SELECT 0

		IF "SELDATA" $ this.STKPATH THEN 
			USE &STKPATH.c100 SHARED
		    if empty(cdx(1))
		      set index to &STKPATH.c110
		    endif
		    set order to 1 
		    SELECT 0
		ELSE
			USE &KLASOR.c100 SHARED
		    if empty(cdx(1))
		      set index to &KLASOR.c110
		    endif
		    set order to 1 
		    SELECT 0
		ENDIF 

		USE &STKPATH.ftrcikis SHARED
		SET ORDER TO 1
		SELECT 0

		USE &STKPATH.ftrrpr SHARED
		IF EMPTY(CDX(1))
		   SET INDEX TO &STKPATH.iftrrpr, &STKPATH.iftrrprk, &STKPATH.iftrrprt
		ENDIF
		SET ORDER TO 0
		SELECT 0

		** 2. BU SEVK NUMARASINA AÝT FATURALARI TOPLA
		JSONmetin = "["
		sayac = 0

		SELECT ftrrpr
		** Hýz için son 15.000 kayda bakýyoruz (Senin paketSorgula mantýðýnla ayný)
		GO RECCOUNT()-15000
		LOCATE REST FOR sevklistno = aktifSevkNo

		DO WHILE FOUND() .and. !EOF()
			ftno=iif(TYPE("ftrrpr->faturano")="C", ftrrpr->faturano, STR(ftrrpr->no1,7))

			SELECT c100
			SEEK ftrrpr.eczanekodu

			SELECT ftrcikis
			SEEK ftno
			LOCATE WHILE faturano = ftno FOR tarih = ftrrpr.tarih .and. eczanekodu=ftrrpr.eczanekodu
			
			** Eðer ftrcikis'te varsa oradaki adetleri al (Yani önceden okutulmuþlar), yoksa 0
			v_koli = IIF(FOUND(), ftrcikis.kolisay, 0)
			v_poset = IIF(FOUND(), ftrcikis.posetsay, 0)
			v_buzluk = IIF(FOUND(), ftrcikis.buzluksay, 0)
			v_sepet = IIF(FOUND(), ftrcikis.sepetsay, 0)

			JSONmetin = JSONmetin+"{'ftrrec':'"+LTRIM(STR(RECNO("ftrrpr")))+"',"
			JSONmetin = JSONmetin+"'hesapkodu':'"+ftrrpr.eczanekodu+"',"
			JSONmetin = JSONmetin+"'adi':'"+RTRIM(LTRIM(CPCONVERT(857,1254,c100.adi)))+"',"
			JSONmetin = JSONmetin+"'cepno':'"+RTRIM(LTRIM(ftrrpr.cepno))+"',"
			JSONmetin = JSONmetin+"'semt':'"+RTRIM(LTRIM(CPCONVERT(857,1254,c100.semt)))+"',"
			JSONmetin = JSONmetin+"'sehir':'"+RTRIM(LTRIM(CPCONVERT(857,1254,c100.sehir)))+"',"
			JSONmetin = JSONmetin+"'bolge':'"+c100.bolge+"',"
			JSONmetin = JSONmetin+"'faturano':'"+FatnoCoz(ftno)+"',"
			JSONmetin = JSONmetin+"'takipno':'"+ftrrpr.takipno+"',"
			slistno = TRANSFORM(IIF(TRIM(ftrrpr.sevklistno)=="", "", TexttoLong(ftrrpr.sevklistno)))
			JSONmetin = JSONmetin+"'sevklistno':'"+slistno+"',"
			
			** DÝKKAT: Cihaza oss, ops, oks, obs olarak okutulmuþ gibi gidecek
			JSONmetin = JSONmetin+"'kolisay':'"+LTRIM(STR(v_koli))+"',"
			JSONmetin = JSONmetin+"'posetsay':'"+LTRIM(STR(v_poset))+"',"
			JSONmetin = JSONmetin+"'buzluksay':'"+LTRIM(STR(v_buzluk))+"',"
			JSONmetin = JSONmetin+"'sepetsay':'"+LTRIM(STR(v_sepet))+"',"
			
			JSONmetin = JSONmetin+"'sevksekli':'"+ftrrpr.sevksekli+"',"
			JSONmetin = JSONmetin+"'ftrsaati':'"+ctotime(subs(ftrrpr.saati,1,2))+"',"
			JSONmetin = JSONmetin+"'sipsaati':'"+ctotime(subs(ftrrpr.saati,4,2))+"',"
			JSONmetin = JSONmetin+"'ftrtarih':'"+TRANSFORM(ftrrpr.tarih)+"',"

			** SEPET KODLARINI BUL
			sepetstr=""
			SELECT sepet
			LOCATE FOR takipno=ftrrpr.takipno .and. eczanekodu=ftrrpr.eczanekodu
			DO WHILE !EOF()
				IF kapak$"123K"
					sepetstr = sepetstr + sepetkodu+","
				ENDIF
				CONTINUE 
			ENDDO 
			sepetstr = LEFT(sepetstr,LEN(sepetstr)-1)
			JSONmetin = JSONmetin + "'sepetler':'"+RTRIM(LTRIM(sepetstr))+"'},"

			sayac = sayac + 1
			
			SELECT ftrrpr
			CONTINUE 
		ENDDO 

		IF sayac > 0
			JSONmetin = LEFT(JSONmetin,LEN(JSONmetin)-1)
			JSONmetin = JSONmetin+"]"
			wlogout()
			RETURN JSONmetin 
		ELSE 
			wlogout()
			RETURN "false"
		ENDIF
	ENDPROC

**------------------------------------------------------------------------------------------------------------------------------	
**sepetokut veya plaka sorgula
	PROCEDURE barkodOkut(barkod,taransinmi)
		this.definePaths()
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		KLASOR = this.KLASOR
		barkod = UPPER(LTRIM(RTRIM(barkod)))
		blength = LEN(barkod)
		
		
		**sepetler için hangi tablo varsa onu al
    	IF FILE(STKPATH+"sepet.dbf")
		   USE &STKPATH.sepet SHARED
		ELSE
		   USE &STKPATH.sepetler ALIAS sepet SHARED
		endif   
			SELECT 0
		**IF "SELCEP" $ this.STKPATH THEN 
		IF "SELDATA" $ this.STKPATH THEN 
			USE &STKPATH.c100 SHARED
		    if empty(cdx(1))
		      set index to &STKPATH.c110
		    endif
		    set order to 1 
		    SELECT 0
		ELSE
			USE &KLASOR.c100 SHARED
		    if empty(cdx(1))
		      set index to &KLASOR.c110
		    endif
		    set order to 1 
		    SELECT 0
		ENDIF 
		
		SET TABLEVALIDATE to 0
		USE &STKPATH.ftrcikis
		SET ORDER TO 1
		SELECT 0
		
		
			
		USE &STKPATH.ftrrpr SHARED
		IF EMPTY(CDX(1))
		   SET INDEX TO &STKPATH.iftrrpr, &STKPATH.iftrrprk, &STKPATH.iftrrprt
		endif
		SET ORDER TO 1 desc
		SELECT 0
		
		USE &STKPATH.irsrpr SHARED
		SET ORDER to 0
		SELECT 0
			
		USE &STKPATH.araclar SHARED
		SELECT 0
		
		USE &STKPATH.sevklist SHARED
		SET ORDER TO 3 desc
		SELECT 0
		
		DO CASE 
		**9 haneliyse ve cep depoysa
		CASE barkod="S" .and.;
			 blength = 9 .and.;
			 SUBSTR(barkod,4,2)<>this._CEPNO .and.;
			 this._SUBENO = SUBSTR(barkod,2,2) 
			 
			 RETURN "cep depo barkodu"
			
			 
		**PLAKA ÝSE OKUNAN	 
		CASE blength >=7 .and. VAL(LEFT(barkod,2))=>0.and.val(LEFT(barkod,2))<=99.and.LEFT(barkod,1)!="S".and.LEFT(barkod,1)!="B"
			SET DELETED OFF 
			SELECT araclar
			LOCATE FOR STRTRAN(plakano," ","")=STRTRAN(barkod," ","")
			plkno = araclar.plakano
			IF FOUND()
				use
				SELECT sevklist
				SEEK plkno
				IF plakano=plkno.and.EMPTY(donustarih)
					**çýkýþ yapýlmýþ yapmak istediðiniz iþlem
					wlogout()
					RETURN "cikisyapmis"
				ELSE 	
					wlogout()
					RETURN "ilkokutma"
				ENDIF
				 
				wlogout()
				RETURN "true"
			ELSE 
				
				wlogout()
				RETURN "false"
			ENDIF 
			SET DELETED ON  
		Case Len(barkod)=13.and.barkod="0".and.subs(barkod,2,2)=subeno.or.;
       	 Len(barkod)=12.and.left(barkod,2)=subeno
	         tkpno = strtran(ALLTRIM(barkod),CHR(3),"")
	         tkpno = LEFT(RIGHT(tkpno,8),7)
	         sptkd=""
	         xobje="F"
         	 RETURN tkpno+":bilinmeyen"
		
		**paket barkodu eski
		Case barkod="B".and.Len(barkod)=16
	         tkpno = strtran(ALLTRIM(barkod),CHR(3),"")
	         tkpno = LEFT(RIGHT(tkpno,8),7)
	         sptkd=""
	         xobje="P"
	         
			 ptakipno =tkpno+""
	         sptkd=""
	         xobje="P"
	         SELECT ftrrpr
	         SET ORDER TO 0
	         GO RECCOUNT()-15000
	         LOCATE REST FOR takipno = ptakipno
	         IF FOUND() then
	        	ftnovar=(TYPE("ftrrpr->faturano")="C")
	        	IF taransinmi="true" then
					JSONdon = this.bolgeTara(ftrrpr.bolge,ftrrpr.tarih, ftrrpr.takipno)
				ELSE 
					JSONDonen=this.okunanBilgiGetir(ftrrpr.takipno,"",ftrrpr.bolge)
				ENDIF 
	         	**JSONDonen=this.okunanBilgiGetir(ftrrpr.takipno,"",ftrrpr.bolge)
	         	wlogout()
	        	RETURN JSONDonen
	         ENDIF 
	         	wlogout()
	         RETURN "BULMADI"
			 
		**paket barkodu yeni
		Case barkod="B".and.Len(barkod)>21.and.Len(barkod)<24
	         tkpno = strtran(ALLTRIM(barkod),CHR(3),"")
	         prcno = subs(tkpno,16,7)
	         ptakipno = subs(tkpno,9,7)
	         ptakipno =ptakipno+""
	         sptkd=""
	         xobje="P"
	         SELECT ftrrpr
	         SET ORDER TO 0
	         GO RECCOUNT()-15000
	         LOCATE REST FOR takipno = ptakipno
	         IF FOUND() then
	        	ftnovar=(TYPE("ftrrpr->faturano")="C")
	        	**BOLGEDEKÝ SEPETLERÝ TARA
	        	IF taransinmi="true" then
					JSONDonen= this.bolgeTara(ftrrpr.bolge,ftrrpr.tarih, ftrrpr.takipno)
				ELSE 
					JSONDonen=this.okunanBilgiGetir(ftrrpr.takipno,"",ftrrpr.bolge)
				ENDIF 
	         	**JSONDonen=this.okunanBilgiGetir(ftrrpr.takipno,"",ftrrpr.bolge)
	         	wlogout()
	        	RETURN JSONDonen
	         ENDIF 
	         	wlogout()
	         RETURN "BULMADI"
	         
	    **SEPET local SEPETSE 9 VE 5 HANELÝ
	    CASE BETWEEN(LEN(barkod),4,6).or.barkod="S"
	    	**sepetin son 4 hanesini al
	    	parcalaSepet=iif(upper(subs(barkod,1,1))="S".and.len(barkod)>4,iif(len(barkod)=9,right(barkod,4),subs(barkod,2)),barkod)
	    	
			**sepetler tablosundan bilgileri al
			SELECT sepet
			LOCATE FOR sepetkodu=parcalaSepet
			
			
			sepetkodu=sepet.sepetkodu
         	takipno=sepet.takipno 		 
         	sepettarih=sepet.tarih
         	sepetirsrecno = sepet.irsrecno
         	sepeteczanekodu = sepet.eczanekodu
         	sepettakipno = sepet.takipno
         	sepetidosya = sepet.idosya

			IF right(sepet.idosya,1)="."	
				SELECT ftrrpr
				ftnovar=(TYPE("ftrrpr->faturano")="C")
				ftno=IIF(ftnovar,LEFT(sepetidosya,7),VAL(sepetidosya))
           		SEEK ftno
           		IF FOUND().and.ftrrpr.eczanekodu=sepeteczanekodu .and.ftrrpr.takipno<>sepettakipno
                  takipno=ftrrpr.takipno 
                ENDIF 
                
				bolgeNo = ftrrpr.bolge
				JSONdon="dene"
				IF taransinmi="true" then
				**BOLGEDEKÝ SEPETLERÝ TARA
					JSONdon = this.bolgeTara(bolgeNo,ftrrpr.tarih, ftrrpr.takipno)
				ELSE 
					JSONdon = this.okunanBilgiGetir(takipno,ftrrpr.tarih,bolgeNo)
				endif
				
				
				wlogout()
               **RETURN yazz
               IF LEN(JSONdon)>0 THEN 
               	logYaz("sevktest.txt",JSONdon)
               ENDIF 
               RETURN JSONdon
            ELSE 
            	SET DELETED off
            		SELECT irsrpr					
					if sepetirsrecno>recc().or.sepetirsrecno=0
					   go bott
					   on error *
					   skip iif(eof(),0,1)
					   **on error do hata with lineno()
					else
					   go sepetirsrecno 
					endif
					**IF eczanekodu=sepet.eczanekodu.and.cepno=_cepno
		              ** =LOGYAZ("FORM1.KEYPRESS:"+ALLTRIM(STR(LINENO())))
		              SELECT sepet
		              SELECT irsrpr
	               IF sepet.tarih=date.and. sepet.bitzaman > zamansayi(date,TIME())-120   
	                  tkpno=irsrpr.takipno
	                  srvsor=.f.
	               ELSE
	                  trhbak=irsrpr.tarih
	               endif  
            	SET DELETED on	
            ENDIF        
		ENDCASE
		RETURN "FALSE SON"
	ENDPROC 
*------------------------------------------------------------------------------------------------------------
**FATURADAN CEP SEPET OKUTULUNCA BÝLGÝLERÝ GETÝR-------------------------------------------------------------
	PROCEDURE FaturadanCepSepetBilgiGetir(xptarih,phesapkod,pfaturano)
		this.definePaths()
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		KLASOR = this.KLASOR	
		
		xpktfno= UPPER(LTRIM(RTRIM(pfaturano)))
		xftrseri=left(xpktfno,2)
		xftrnum =INT(val(subs(xpktfno,at(" ",xpktfno))))
		xftrno=fatnostr(xftrseri, xftrnum)
		IF "SELDATA" $ this.STKPATH THEN 
			USE &STKPATH.c100 SHARED
		    if empty(cdx(1))
		      set index to &STKPATH.c110
		    endif
		    set order to 1 
		    SELECT 0
		ELSE
			USE &KLASOR.c100 SHARED
		    if empty(cdx(1))
		      set index to &KLASOR.c110
		    endif
		    set order to 1 
		    SELECT 0
		ENDIF 
			
		USE &STKPATH.ftrrpr SHARED
		IF EMPTY(CDX(1))
		   SET INDEX TO &STKPATH.iftrrpr, &STKPATH.iftrrprk, &STKPATH.iftrrprt
		endif
		SET ORDER TO 1 desc
		SELECT 0
		JSONMetin = ""
		SELECT ftrrpr 
		SET ORDER TO 3
		SEEK DTOC(CTOD(SUBSTR(xptarih,4,2)+"/"+SUBSTR(xptarih,1,2)+"/"+SUBSTR(xptarih,7,4)),1)+xftrno
		**LOCATE  FOR faturano=xftrno .and. eczanekodu=ALLTRIM(phesapkod)
		**.and.tarih=ctod(xptarih)
			
		IF FOUND() THEN
			SELECT c100 
			SET ORDER TO 1
			SEEK ftrrpr.eczanekodu
			JSONMetin = "[{'ftrrec':'"+LTRIM(RTRIM(STR(RECNO("ftrrpr"))))+"',"
			JSONmetin = JSONmetin+"'hesapkodu':'"+ftrrpr.eczanekodu+"',"
    		JSONmetin = JSONmetin+"'adi':'"+LTRIM(RTRIM(CPCONVERT(857,1254,c100.adi)))+"',"
    		JSONmetin = JSONmetin+"'cepno':'"+RTRIM(LTRIM(ftrrpr.cepno))+"',"
    		JSONmetin = JSONmetin+"'semt':'"+LTRIM(RTRIM(CPCONVERT(857,1254,c100.semt)))+"',"
    		JSONmetin = JSONmetin+"'sehir':'"+LTRIM(RTRIM(CPCONVERT(857,1254,c100.sehir)))+"',"
    		JSONmetin = JSONmetin+"'bolge':'"+c100.bolge+"',"
    		JSONmetin = JSONmetin+"'faturano':'"+FatnoCoz(xftrno)+"',"
    		JSONmetin = JSONmetin+"'takipno':'"+ftrrpr.takipno+"',"
    		slistno = TRANSFORM(IIF(TRIM(ftrrpr.sevklistno)=="", "", TexttoLong(ftrrpr.sevklistno)))
    		JSONmetin = JSONmetin+"'sevklistno':'"+slistno+"',"
    		JSONmetin = JSONmetin+"'kolisay':'',"
    		JSONmetin = JSONmetin+"'posetsay':'',"
    		JSONmetin = JSONmetin+"'buzluksay':'',"
    		JSONmetin = JSONmetin+"'sepetsay':'',"
    		JSONmetin = JSONmetin+"'sevksekli':'"+ftrrpr.sevksekli+"',"
    		JSONmetin = JSONmetin+"'ftrsaati':'"+ctotime(subs(ftrrpr.saati,1,2))+"',"
    		JSONmetin = JSONmetin+"'sipsaati':'"+ctotime(subs(ftrrpr.saati,4,2))+"',"
    		JSONmetin = JSONmetin+"'ftrtarih':'"+TRANSFORM(ftrrpr.tarih)+"',"
    		**SEPETLERY GETYR
    		sepetstr=""
    		JSONmetin = JSONmetin + "'sepetler':'"+sepetstr+"'}]"
			wlogout()
			RETURN JSONmetin
		ENDIF 
		
		wlogout()
		RETURN "false"
	ENDPROC 
**-----------------------------------------------------------------------------------------------------------------------------
**Sepetten	Etiket Bas
	PROCEDURE etiketBaski(barkod,xetkyazicino)
		this.definePaths()
		etkbasSubeNo = this._SUBENO
		barkod= UPPER(LTRIM(RTRIM(barkod)))
		blength = LEN(barkod)
		IF EMPTY(etkbasSubeNo) then
			wlogout()
			RETURN "asdf"
		ENDIF
	
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		KLASOR = this.KLASOR
		IF FILE(STKPATH+"sepet.dbf")
		   USE &STKPATH.sepet SHARED
		ELSE
		   USE &STKPATH.sepetler ALIAS sepet SHARED
		endif   
			SELECT 0
		IF "SELDATA" $ this.STKPATH THEN 
			USE &STKPATH.c100 SHARED
		    if empty(cdx(1))
		      set index to &STKPATH.c110
		    endif
		    set order to 1 
		    SELECT 0
		ELSE
			USE &KLASOR.c100 SHARED
		    if empty(cdx(1))
		      set index to &KLASOR.c110
		    endif
		    set order to 1 
		    SELECT 0
		ENDIF 
		
		SET TABLEVALIDATE to 0
		USE &STKPATH.ftrcikis
		SET ORDER TO 1
		SELECT 0
		USE &STKPATH.irsrpr SHARED
		SET ORDER TO 0
		SELECT 0
			
		USE &STKPATH.ftrrpr SHARED
		IF EMPTY(CDX(1))
		   SET INDEX TO &STKPATH.iftrrpr, &STKPATH.iftrrprk, &STKPATH.iftrrprt
		endif
		SET ORDER TO 1 desc
		SELECT 0
	
		USE &BPATH.depogln SHARED
		SET ORDER TO 0
		SELECT 0
		DO CASE 
			**cep sepetiyse
			CASE barkod="S" .and.;
				 blength = 9 .and.;
				 SUBSTR(barkod,4,2)<>this._CEPNO .and.;
				 this._SUBENO = SUBSTR(barkod,2,2) 
				 wlogout()
				 RETURN "cep depo barkodu"
				 
			**kendi sepetiyse
			CASE BETWEEN(LEN(barkod),4,6).or.barkod="S"
	    		parcalaSepet=iif(upper(subs(barkod,1,1))="S".and.len(barkod)>4,iif(len(barkod)=9,right(barkod,4),subs(barkod,2)),barkod)
	    		SELECT sepet
				LOCATE FOR sepetkodu=parcalaSepet
				sepetkodu=sepet.sepetkodu
	         	ltakipno=sepet.takipno 		 
	         	sepettarih=sepet.tarih
	         	sepetirsrecno = sepet.irsrecno
	         	sepeteczanekodu = sepet.eczanekodu
	         	sepettakipno = sepet.takipno
	         	sepetidosya = sepet.idosya
	         	jsonStr = "";
	         	
	         	SELECT ftrrpr
	         	SET ORDER TO 0
	         	GO RECCOUNT()-10000
	         	LOCATE REST FOR takipno = ltakipno
	         	IF FOUND() then
	         		LBOLGE = ftrrpr.bolge
	         		LSAAT =""
					**ÝRSRPR DE IZAMAN
					LSEVK=YazSevkAcik(ftrrpr->sevksekli)
					
					SELECT c100
					SET ORDER TO 1
					SEEK ftrrpr.eczanekodu
					
					jsonstr=c100.adi
					IF FOUND() THEN
						LECZANE=DTrkWstr(c100->adi)
						
					ELSE 
						LECZANE=""
					ENDIF 					
					LKOD=ltakipno
					LECZACI=DTrkWstr(c100->ilgili)
					LADRES1=DTrkWstr(c100->adres)
					LADRES2=DTrkWstr(c100->adres2)
					LSEHIR  = DTrkWstr(Ltrim(Trim(c100->semt)+" "+Trim(c100->sehir)))
					SELECT irsrpr
					go sepet.irsrecno
					LSAYISAL=Ltrim(Str(irsrpr->KUTUSAYISI,7)+' / '+Ltrim(Str(irsrpr->KALEMSAY,6)))
					If .Not. Empty(irsrpr->ayracno)
    				    LRENK=TRIM(irsrpr->ayracno)
    				ELSE
    					LRENK=Trim(irsrpr->siprenk)
   				    Endif
					LSEPET = barkod
					LSEPET2=""
					etiketbilgisi=""
					SELECT depogln
					LOCATE REST FOR depogln.depokodu="SED" .and. depogln.sube=RIGHT(etkbasSubeNo,1)
					IF FOUND()
						dgln = TRANSFORM(depogln.gln)			
						etiketbilgisi = "B"+SUBSTR(dgln,4,3)+this._SUBENO+RIGHT(this._CEPNO,1)+"O"+ltakipno+"Y"+sifir(MOD(SECONDS()*10,1000000),6)
						SEVKBILGI = etiketbilgisi
					
					ENDIF 	
					**IF xetkyazicino="0" .or. xetkyazicino="" THEN 
						**RESTORE FROM &STKPATH.etkayar.mem ADDITIVE 
						**etkyazicino = LTRIM(RTRIM(AYR_ETIKETYAZ))
					**ELSE 
						**etkyazicino = xetkyazicino
					**endif
					etkyazicino = xetkyazicino
					dosyaYolu = BPATH+ "yazici\E"+etkyazicino + "\"+ LKOD +sifir(MOD(SECONDS()*10,1000000),6)+".txt"
					
					yazilacakEtiketMetni = etkstr(LBOLGE,LSAAT,LSEVK,LECZANE,LKOD,LECZACI,LADRES1,LADRES2,LSEHIR,LSAYISAL,LRENK,LSEPET,LSEPET2,SEVKBILGI)
					fh = FCREATE(dosyaYolu)
					FWRITE(fh,yazilacakEtiketMetni)
					FCLOSE(fh)
					**btxt = FOPEN(dosyaYolu,12)
					**FSEEK(btxt,0,2)
					**FWRITE(btxt,yazilacakEtiketMetni)
					**FCLOSE(btxt)
					
					logYaz("denemelog.txt",yazilacakEtiketMetni )
					jsonSonuc = '{"durum":"OK", "eczane":"' + ALLTRIM(c100.adi) + '", "bolge":"' + ALLTRIM(LBOLGE) + '", "takipno":"' + ltakipno + '"}'
					
					wlogout()
					RETURN jsonSonuc
	         	ELSE
	         		**irsrpr den al
	         		SELECT irsrpr 
	         		SET ORDER TO 2
	         		
	         	ENDIF 
	         		
	         	
		ENDCASE 
		CLOSE DATABASES
		wlogout()
		RETURN '{"Durum":"HATA", "Mesaj":"Fatura bulunamadý"}'
	ENDPROC 
	
**------------------------------------------------------------------------------------------------------------------------------	
**------------------------------------------------------------------------------------------------------------------------------	
**FaturaNo dan etiket bas
	PROCEDURE FaturadanEtiketBas(pfaturano,xetkYaziciNo)
		this.definePaths()
		BPATH = this.BPATH
		STKPATH = this.STKPATH
		KLASOR = this.KLASOR	
		etkbasSubeNo = this._SUBENO	
		xftrseri=left(pfaturano,2)
		xftrnum =INT(val(subs(pfaturano,at(" ",pfaturano))))
		xftrno=fatnostr(xftrseri, xftrnum)

		jsonstr=""
		
		IF "SELDATA" $ this.STKPATH THEN 
			USE &STKPATH.c100 SHARED
		    if empty(cdx(1))
		      set index to &STKPATH.c110
		    endif
		    set order to 1 
		    SELECT 0
		ELSE
			USE &KLASOR.c100 SHARED
		    if empty(cdx(1))
		      set index to &KLASOR.c110
		    endif
		    set order to 1 
		    SELECT 0
		ENDIF 

		USE &BPATH.depogln SHARED
		SET ORDER TO 0
		SELECT 0

		USE &STKPATH.irsrpr SHARED
		SET ORDER TO 0
		SELECT 0

		USE &STKPATH.ftrrpr SHARED
		IF EMPTY(CDX(1))
		   SET INDEX TO &STKPATH.iftrrpr, &STKPATH.iftrrprk, &STKPATH.iftrrprt
		endif
		SET ORDER TO 1 desc
		SELECT 0

		SELECT ftrrpr 
		SET ORDER TO 1
		SEEK xftrno
		
		

		**SET ORDER TO 3
		**SEEK DTOC(CTOD(SUBSTR(xptarih,4,2)+"/"+SUBSTR(xptarih,1,2)+"/"+SUBSTR(xptarih,7,4)),1)+xftrno
		**SEEK DTOC(CTOD(SUBSTR(xptarih,4,2)+"/"+SUBSTR(xptarih,1,2)+"/"+SUBSTR(xptarih,7,4)),1)+xftrno
		**LOCATE  FOR faturano=xftrno .and. eczanekodu=ALLTRIM(phesapkod)
		**.and.tarih=ctod(xptarih)
		
		IF FOUND() THEN
			LBOLGE = ftrrpr.bolge
     		LSAAT =""
			**ÝRSRPR DE IZAMAN
			LSEVK=YazSevkAcik(ftrrpr->sevksekli)
			ltakipno = ftrrpr.takipno
			lsayisal=""
			lrenk=""
			SELECT c100
			SET ORDER TO 1
			SEEK ftrrpr.eczanekodu
			
			jsonstr=c100.adi
			
			IF FOUND() THEN
				LECZANE=DTrkWstr(c100->adi)
				
			ELSE 
				LECZANE=""
			ENDIF 					
			LKOD=ftrrpr.takipno 
			LECZACI=DTrkWstr(c100->ilgili)
			LADRES1=DTrkWstr(c100->adres)
			LADRES2=DTrkWstr(c100->adres2)
			LSEHIR  = DTrkWstr(Ltrim(Trim(c100->semt)+" "+Trim(c100->sehir)))
			**SELECT irsrpr
			**go sepet.irsrecno
			**LSAYISAL=Ltrim(Str(irsrpr->KUTUSAYISI,7)+' / '+Ltrim(Str(irsrpr->KALEMSAY,6)))
			**If .Not. Empty(irsrpr->ayracno)
			  **  LRENK=TRIM(irsrpr->ayracno)
			**ELSE
				**LRENK=Trim(irsrpr->siprenk)
   			**Endif
			LSEPET = ""
			LSEPET2=""
			etiketbilgisi=""
			SELECT depogln
			LOCATE REST FOR depogln.depokodu="SED" .and. depogln.sube=RIGHT(etkbasSubeNo,1)
			IF FOUND()
				dgln = TRANSFORM(depogln.gln)			
				etiketbilgisi = "B"+SUBSTR(dgln,4,3)+this._SUBENO+RIGHT(this._CEPNO,1)+"O"+ltakipno+"Y"+sifir(MOD(SECONDS()*10,1000000),6)
				SEVKBILGI = etiketbilgisi
			
			ENDIF 	
			**IF xetkyazicino="0" .or. xetkyazicino="" THEN 
				**RESTORE FROM &STKPATH.etkayar.mem ADDITIVE 
				**etkyazicino = LTRIM(RTRIM(AYR_ETIKETYAZ))
			**ELSE 
				**etkyazicino = xetkyazicino
			**endif
			etkyazicino = xetkyazicino
			dosyaYolu = BPATH+ "yazici\E"+etkyazicino + "\"+ LKOD+sifir(MOD(SECONDS()*10,1000000),6)+".txt"			
			yazilacakEtiketMetni = etkstr(LBOLGE,LSAAT,LSEVK,LECZANE,LKOD,LECZACI,LADRES1,LADRES2,LSEHIR,LSAYISAL,LRENK,LSEPET,LSEPET2,SEVKBILGI)
			fh = FCREATE(dosyaYolu)
			FWRITE(fh,yazilacakEtiketMetni)
			FCLOSE(fh)
			
			jsonstr = '{"Durum":"OK", ' + ;          
					  '"Eczane":"' + ALLTRIM(DTrkWstr(c100.adi)) + '", ' + ;          
					  '"Bolge":"' + ALLTRIM(LBOLGE) + '", ' + ;          
					  '"Takipno":"' + ALLTRIM(ltakipno) + '", ' + ;          
					  '"Mesaj":"Basým Baþarýlý"}'
			logYaz("denemelog.txt",yazilacakEtiketMetni )
			wlogout()
			RETURN jsonstr			
		ENDIF 
		
		wlogout()
		RETURN '{"Durum":"HATA", "Mesaj":"Fatura bulunamadý"}'
	ENDPROC 
*--------------------------------------------------------------------------------------------------------------------------
	PROCEDURE test
		logYaz("denemelog.txt","DEMEDÝrR")
		return
	ENDPROC 
*----------------------------------------------------------------------------------------------

*--------------------------------------------------------------------------------------------------
	PROCEDURE cikis
		CLOSE DATABASES
		wlogout()
		RELEASE ALL()
	ENDPROC  
*--------------------------------------------------------------------------------------------------
*Þube Ve Cep No
	PROCEDURE getSubeNo
		this.definePaths()
		BPATH = this.BPATH
		RESTORE FROM &BPATH.subeno.mem ADDITIVE 
		this._SUBENO = SUBENO
		RETURN SUBENO
	ENDPROC 
	PROCEDURE getCepNo
		this.definePaths()
		BPATH = this.BPATH
		RESTORE FROM &BPATH.subeno.mem ADDITIVE 
		this._CEPNO = _CEPNO
		RETURN _CEPNO
	ENDPROC
	PROCEDURE getSubeCepVeFirma
		jsonDon = ""
		this.definePaths()
		**BPATH = this.BPATH
		**STKPATH = this.STKPATH
		**RESTORE FROM &BPATH.subeno.mem ADDITIVE 
		**RESTORE FROM &BPATH.memfile.mem ADDITIVE
		

		**jsonDon = "{'subeno':'"+this._SUBENO+"','cepno':'"+this._CEPNO+"','depoadi':'"+DEPOADI+"'}"
		jsonDon = "{'subeno':'"+this._SUBENO+"','cepno':'"+this._CEPNO+"','depoadi':'"+this._DEPOADI+"'}"
		RETURN jsonDon	
	ENDPROC  
*--------------------------------------------------------------------------------------------------
*Plaka Sevk Saati Ekle
	PROCEDURE plakaSevkSaatiEkle(plkNo)
		this.definePaths()
		STKPATH = this.STKPATH 
				
		USE &STKPATH.sevklist SHARED 
		SELECT 0
		
		use &STKPATH.belgeno SHARED
		SELECT 0
		
		USE &STKPATH.araclar SHARED
		SET ORDER TO 1
		SELECT 0
		
		USE &STKPATH.surucu SHARED
		SET ORDER TO 1
		SELECT 0

		strh = DATE()

		SELECT sevklist
		SET ORDER TO 3 DESCENDING 
		SEEK plkNo
		**LOCATE FOR plakano=plkNo .and. donustarih<CTOD("1/1/1") .and. sevktarihi>DATE()-2
		LOCAL lKayitBulundu,svkno
		svkno=""
		lKayitBulundu = .F.

		IF FOUND() then
			LOCATE REST WHILE plakano = plkNo FOR donustarih < CTOD("1/1/1") .AND. sevktarihi > DATE()-2
			**svkno = TRANSFORM(INT(texttolong(sevklistno)))
			**replace NEXT 1 sevktarihi WITH DATE(),sevksaati WITH TIME(1)
			**RETURN svkno
			IF FOUND()
				lKayitBulundu = .T.
	            svkno = TRANSFORM(INT(texttolong(sevklistno)))
	            REPLACE NEXT 1 sevktarihi WITH DATE(), sevksaati WITH TIME(1)
	            RETURN svkno
	        ENDIF
		ENDIF
		
		IF lKayitBulundu = .F. then	

			  SELECT belgeno
		      loca for belgetipi="SEVKNO"
		      if eof()
		         appe blan
		         repl NEXT 1 belgetipi with "SEVKNO", xbno with 1000000*VAL(_cepno)
		      else
		         repl NEXT 1 xbno with xbno+1
		      ENDIF
		      svkno=xbno
			  SELECT araclar
			  SEEK RTRIM(plkNo)
			  
			  SELECT surucu 
			  SEEK araclar.surucukodu
			  		      
		      SELECT sevklist
		      GO bott
		      APPEND BLANK
		      REPLACE NEXT 1 sevklistno WITH LongtoText(svkno), sevktarihi WITH DATE(), sevksaati WITH TIME(1), plakano WITH plkNo,;
		      				surucukodu WITH araclar->surucukodu, sevkdepo WITH this._CEPNO
			RETURN TRANSFORM(INT(svkno))
		ENDIF 
		
		RETURN svkno
	ENDPROC 
	
	PROCEDURE UseTables()
		this.definePaths()
		STKPATH = this.STKPATH 
		
		
		USE &STKPATH.sevklist SHARED
		SELECT 0
		*
		USE &STKPATH.araclar SHARED
		SET ORDER TO 1
		SELECT 0
		*
		USE &STKPATH.surucu SHARED
		SET ORDER TO 1
		SELECT 0
		*
		USE &STKPATH.ftrrpr SHARED
		SET ORDER TO 1
		SELECT 0
		
		USE &STKPATH.ftrcikis SHARED
		SELECT 0
	ENDPROC 
	*--------------------------------------------------------------------------------------------------
	*Eczane Sevk Kayýt Ýþlemleri
	PROCEDURE EczaneCikisSevkKayit(eczkod,tkpno,fatno,sevkno,plkno,ss,ps,bs,ks)	
		seccep="01,02,03,04,05,06,07,08,09"
		xftrseri=left(fatno,2)
		xftrnum =INT(val(subs(fatno,at(" ",fatno))))
		xftrno=fatnostr(xftrseri, xftrnum)
		
		SELECT ftrrpr
		SEEK xftrno	
		LOCATE WHILE faturano=xftrno FOR eczanekodu=eczkod .and. takipno=tkpno
		
		IF FOUND() then
			REPLACE NEXT 1 sevklistno WITH LongtoText(INT(VAL(ALLTRIM(sevkno)))), sevktarihi WITH DATE()
			SELECT sevklist 
			SET ORDER to 1
			SEEK ftrrpr->sevklistno
			LOCATE WHILE sevklistno=ftrrpr.sevklistno FOR EMPTY(sevkdepo).or.sevkdepo$seccep
			IF !FOUND() then
				SELECT araclar
				SET ORDER TO 1
				SEEK RTRIM(plkno)
				*
				SELECT surucu
				SET ORDER TO 1
				SEEK araclar.surucukodu
				*
				SELECT sevklist
				APPEND BLANK
				REPLACE NEXT 1 sevklistno WITH ftrrpr->sevklistno, sevktarihi WITH DATE(), sevksaati WITH TIME(1),;
        		plakano WITH plkno, surucukodu WITH araclar->surucukodu, sevkdepo WITH this._CEPNO
			ENDIF 
			SELECT ftrcikis
			SET ORDER TO 1
			SEEK xftrno
			LOCATE WHILE faturano=xftrno  FOR eczanekodu = ftrrpr.eczanekodu.and.tarih=ftrrpr.tarih 
			IF !FOUND()
				APPEND BLANK
				REPLACE NEXT 1 faturano WITH ftno, eczanekodu WITH ftrrpr->eczanekodu, tarih WITH ftrrpr->tarih
			ENDIF 
			REPLACE NEXT 1 sevklistno WITH ftrrpr->sevklistno, sevktarihi WITH DATE(), sevksaati WITH TIME(1),;
        	kolisay WITH INT(VAL(ALLTRIM(TRANSFORM(ks)))), posetsay WITH INT(VAL(ALLTRIM(TRANSFORM(ps)))),;
			buzluksay WITH INT(VAL(ALLTRIM(TRANSFORM(bs)))), sepetsay with INT(VAL(ALLTRIM(TRANSFORM(ss))))  	
        	RETURN "true" 
		ENDIF
		RETURN "false" 
	ENDPROC 
	
	Procedure FatnoStr
		Param psr, pfno
		return longtotext(pfno)+padr(psr,3)
	ENDPROC
	
	
	**------------------------------------------------------------------------------------------------------------------------------	
	** Son faturalarý, þifreli saatleri çözerek ve baðlý sepetlerle birlikte getirir
	PROCEDURE getSonSevkiyatDetayli(pGeriyeBak)
	    this.definePaths()
	    STKPATH = this.STKPATH
	    
	    IF TYPE("pGeriyeBak") != "N" OR pGeriyeBak <= 0
	        pGeriyeBak = 1000
	    ENDIF

	    ** Tablolarý aç
	    IF FILE(STKPATH+"sepet.dbf")
	       USE &STKPATH.sepet SHARED IN 0
	    ELSE
	       USE &STKPATH.sepetler ALIAS sepet SHARED IN 0
	    ENDIF
	    USE &STKPATH.ftrrpr SHARED IN 0
	    
	    SELECT ftrrpr
	    SET ORDER TO 0  
	    
	    ** Son kayýtlara zýpla
	    IF RECCOUNT() > pGeriyeBak
	        GO RECCOUNT() - pGeriyeBak
	    ELSE
	        GO TOP
	    ENDIF

	    JSONMetin = "["
	    
	    
	    SCAN REST FOR ALLTRIM(TRANSFORM(ftrrpr.cepno)) == ALLTRIM(TRANSFORM(this._CEPNO))
	    	IF EMPTY(ftrrpr.takipno) OR ISNULL(ftrrpr.takipno)
	    		LOOP   
	    	ENDIF
	    
	        cTkpNo = ftrrpr.takipno
	        cEczKodu = ftrrpr.eczanekodu
	        
	       
	        sepetstr = ""
	        SELECT sepet
	        SET ORDER TO 0
	        GO TOP
	      
	        LOCATE FOR takipno == cTkpNo .AND. eczanekodu == cEczKodu
	        
	        DO WHILE FOUND() .AND. !EOF()
	            IF kapak $ "123K"
	                sepetstr = sepetstr + "{'sepetno':'" + ALLTRIM(TRANSFORM(sepetkodu)) + "','zaman':'" + ALLTRIM(TRANSFORM(bitzaman)) + "'},"
	            ENDIF
	            CONTINUE 
	        ENDDO 

	        ** Sepeti olmayan faturayý es geç 
	        IF EMPTY(sepetstr)
	            SELECT ftrrpr
	            LOOP
	        ENDIF

	        ** Sepetlerin sonundaki virgülü temizle
	        sepetstr = LEFT(sepetstr, LEN(sepetstr)-1)

	        SELECT ftrrpr
	        ** JSON satýrýný oluþtur
	        lcSatir = "{'subeno':'" + ALLTRIM(TRANSFORM(this._SUBENO)) + "'," + ;
	                  "'cepno':'" + ALLTRIM(TRANSFORM(ftrrpr.cepno)) + "'," + ;
	                  "'faturano':'" + ALLTRIM(FatnoCoz(faturano)) + "'," + ;
	                  "'takipno':'" + ALLTRIM(cTkpNo) + "'," + ;
	                  "'ftrtarih':'" + TRANSFORM(ftrrpr.tarih) + "'," + ;
	                  "'ftrsaati':'" + ctotime(subs(ftrrpr.saati,1,2)) + "'," + ;
	                  "'sipsaati':'" + ctotime(subs(ftrrpr.saati,4,2)) + "'," + ;
	                  "'sepetler':[" + sepetstr + "]},"
	        
	        ** Olasý enter karakterlerini temizle ve ana metne ekle
	        JSONMetin = JSONMetin + STRTRAN(STRTRAN(lcSatir, CHR(13), ""), CHR(10), "")
	    ENDSCAN

	    ** JSON dizisini kapat
	    IF RIGHT(JSONMetin, 1) == ","
	        JSONMetin = LEFT(JSONMetin, LEN(JSONMetin)-1)
	    ENDIF
	    JSONMetin = JSONMetin + "]"

	    wlogout()
	    RETURN JSONMetin
	ENDPROC
	
ENDDEFINE


