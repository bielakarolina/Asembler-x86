;Karolina Biela 
;poniedziałek 16:15 A

dane1 segment

liniapolecen db 161 dup(0) ; tu będą zapisywane argumentu uż po przetworzeniu,1(ilosc argumentow)+32 (liczby w 16)+128(przetworzone na binarny 4*32)
plikwejsciowy db 13 dup(0) ; nazwa pliku wejściowego,gdy nazwa będzie zawierała więcej niż 12 znaków wyskoczy bład, wymogi dosa
plikwyjsciowy db 13 dup(0); nazwa pliku wyjsciowego 
wskazniknawyjsciowy dw ?
wskazniknawejsciowy dw ?

errorbrakargumentow db "NIE PODALES ARGUMENTOW $"
errorzmienne1 db "PODALES ZLA ILOSC ZMIENNYCH $"
errorzmienne2 db "BLAD PLIKU WEJSCIOWEGO, COS JEST NIE TAK $"
errorzmienne3 db "BLAD PLIKU WYJSCIOWEGO $"
errorzmienne4 db "COS JEST NIE TAK $"
buforwejsciowy db 16384 dup(?)
buforwyjsciowy db 16384 dup(?)			; rozmiar ustawiony na 16 kb 
buforwejsciowypozycja dw 16384			
buforwyjsciowypozycja dw 0 
buforibajty     dw 16384
xflaga	db 0      ; flaga dla dekompresji lub kompresji

dane1 ends

code1 segment 
start:


	mov ax,seg wstosu       ;inicjalizacja stosu
	mov ss,ax 				;do rejestrow segmentowych przesylamy przez inne rejestry
	mov	sp,offset wstosu    ;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu.
						;więc tu: przesyłanie do rejestru sp "adresu" wstosu


	
	mov ax,ds				;zapamietanie adresu PSP(struktura która przechowuje stan "programu") w es
	mov es,ax
							
	mov	si,82h	   ;82h oznacza pierwszy znak,bo 80h to liczba wpisanych argumentow,81h to spacja
	
	mov ax,seg liniapolecen				;inicjalizacja wskaźnika segmentu danych
	mov ds, ax							;załadowanie rejestru segmenttowego ds
	
	
	 
;////////PROCEDURY SPRAWDZAJĄCE ZGODNOŚĆ ARGUMENTÓW Z WYTYCZNYMI,NASTĘPNIE ZAPISUJĄCE JE/////////////// 
;//////// NA POCZĄTKU SPRAWDZA CZY SĄ JAKIEŚ ARGUMENTY JEŻELI TAK DO WYWOŁUJEMY PROCEDURE parse/////////
;////////NASTĘPNIE SPRAWDZAMY CZY ARGUMENTY SĄ ZGODNE Z WYMOGAMI///////////////////////////////////////	 
	 
	 
	cmp	byte ptr es:[80h], 0	;sprawdza czy liczba znakow w linii komend jest zero , cmp porównuje nie zapisując wyniku
	je brakargumentow			; gdy brak argumentów, gdy zf=1
	
	mov	di, offset liniapolecen	;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	call parse							
	
	cmp	byte ptr ds:[di], 2		;sprawdza czy są podane dwa argumenty
	je robkompresje 
	
	cmp	byte ptr ds:[di], 3		;sprawdza czy są podane trzy argumenty
	je robdekompresje 
	jmp zlailoscargumentow
	
	robkompresje:      ; gdy podane są dwa argumenty bez -d czyli wykonujemy kompresje
	call sprawdzanieargumentowkompresja		; i przepisujemy argumenty do odpowiednich tablic
	jmp otwieranie
	
	robdekompresje:			; gdy sa 3 argumenty
	call sprawdzanieargumentowdekompresja
	jmp otwieranie
	
	
	otwieranie:
	
	mov al, 0				;ustawiamy priorytety gdy al=0 to odczyt gdy =1 to zapis =2 dla obu 
	mov	ah, 3dh				           ;otwieranie pliku za pomoca procedury 3dh przerwania 21h
	mov	dx, offset plikwejsciowy		; gdy otwieranie przebieglo pomyslnie ustawia cf=0,OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	int	21h								; gdy nie udalo otworzyc pliky cf=1
	jc errorotwieraniewejsciowego		; skok gdy cf=1
	mov	word ptr ds:[wskazniknawejsciowy], ax ;uchwyt do pliku do pamięci, jedna z wytycznych procedury
												; ustawia uchwyt do pliku dla cf =0 w ax 

	
	mov	cx, 0					;tworzy nowy plik za pomoca procedyry 3ch przerwania 21h , w cl ustawia sie atrybuty =0 to odczyt =1 to ukryty =2 to systemowy =5 archiwalny
	mov ah, 3ch				; ustawia się atrybuty nowo powstalego pliku nadajac cl odpowiednie wartosci np 0(tylko do odczytu)
	mov	dx, offset plikwyjsciowy	;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	int	21h
	jc  errorotwieraniewyjsciowego	; tak jak w poprzednim przypadku gdy cf=1 to wystapił bład i wykonujemy skok
	mov	word ptr ds:[wskazniknawyjsciowy], ax ;a gdy wszystko ok w ax jest uchwyt pliku do pamieci

	
	cmp	ds:[xflaga], 1			;ustawiamy co chcemy wykonac gdy xflaga=1 to dekompresja
	je	wywolaniedekompresji       ; jeżeli tak to wykonujemy dekompresje
	
	wywolaniekompresji:			; w innym przypadku wykonujemy kompresje
	call	kompresja
	jmp	zakonczprogram
	
	
	
	wywolaniedekompresji:     
	call	dekompresja
	jmp	zakonczprogram
	
	
	
	
	
	
	

;///////PODSTAWOWE PROCEDURY ZAMYKANIA ORAZ WYWOŁYWANE PODCZAS BLĘDÓW W ARGUMENTACH/////////////////////	
	
	zakonczprogram:
		
		mov	bx, word ptr ds:[wskazniknawejsciowy]
		mov	ah, 3eh									; do zamkniecia pliku uzywa sie procedury 3eh przerwania 21h
		int	21h				;zamknięcie wejscia		
	
		mov	bx, word ptr ds:[wskazniknawyjsciowy]
		mov	ah, 3eh
		int	21h				;zamknięcie wyjscia
			
		mov ax, 1h	
		int	16h                           ; nie może być zwykłego zamknięcia programu
		mov ax,04c00h						; gdyż szybciej zamyka program niż zdąrzy coś wyświetlić	
		int 21h							; dlatego dopisałam aby oczekiwał na klawisz przed zamknięciem
	
	;////////PROCEDURY BŁEDÓW PODCZAS WYKONYWANIA ///////////////////////

	brakargumentow:
		mov dx, offset errorbrakargumentow     ;gdy nie podamy żadnych argumentów ,
		call wykazbledow					;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
		jmp zakonczprogram							;skok bezwarunkowy

	zlailoscargumentow:	
		mov dx, offset errorzmienne1			;gdy podamy złą ilość
		call wykazbledow
		jmp zakonczprogram
	cosjestnietak:
		mov dx, offset errorzmienne4
		call wykazbledow
		jmp zakonczprogram
		
	errorotwieraniewejsciowego:
		mov dx, offset errorzmienne2
		call wykazbledow
		jmp zakonczprogram
		
	errorotwieraniewyjsciowego:
		mov dx, offset errorzmienne3
		call wykazbledow
		jmp zakonczprogram
	


		
;////////Procedura parse ///////////////////                                     
	                                                     ;es:si -linia komend,wejście 
	                                                      ;ds:di-wyjście 
                                                                  
parse:	
	push si
	push di
	push ax					
	push bx					
	push bp
	pushf
	
	mov	bp, di				;bp jest licznikiem argumentów
	mov	byte ptr ds:[bp], 0	;zerowanie argumentów
	add	di, 33			;di wskazuje na pierwsze wolne pole w liniapolecen gdzie znajdują się już argumenty bez białych znaków, sklejone argumenty 
						; jeżeli tego nie ustawimy program będzie głupiał i wyrzucał błąd odnośnie ilości zmiennych
	
	zapisargumentow: ;pomija białe znaki przechodzac po linii polecen i zapisuje je do liniapolecen
		call outbialeznaki ;wywołujemy pomijanie znaków białych
		cmp al,1
		je konieczapisu  ;konczy proces gdy napotka koniec linii,rodzaj pętli typu while
		inc byte ptr ds:[bp]  ; analizuje linie polecen i aktualizuje ilość wpisanych argumentow
		call zapis           ; zapisywanie argumentow 

	push    di
	push    ax				;musimu je odłożyć aby mieć tymczasową pamięć
	mov	al, byte ptr ds:[bp]		;w al znajduje się numer obecnego argumentu
	mov	ah, 0						;zerujemy
	mov di,ax						;przenosimy do di numer argumentu
	mov	byte ptr ds:[bp+di], cl		;wpisuje długość argumentu 
		
	pop	ax				
	pop	di
		
	cmp	al, 1			; kończymy gdy napotkamy znak końca linii
	je konieczapisu
		
	jmp zapisargumentow  ;jeżeli nie to powtarzamy operacje
		
	
	konieczapisu:
		popf
		pop bp
		pop bx
		pop ax
		pop di
		pop si
		ret
		
zapis:

	;procedura zapisywania
	pushf
	mov	al, 0				;flaga końca linii nie jest ustawiona 
	mov	cx, 0				;dlugosc argumentu

	poczatekzapisu:
		mov	ah, byte ptr es:[si]		;przenoszenie znaku do ah
		
		cmp ah,13				    ;gdy koniec linii ustaw flage i zakoncz zapis
		je zapiskoniecprocedury2					
		cmp ah,9d				;gdy trafi na tabulator zakoncz zapis
		je zapiskoniecprocedury1
		cmp ah,' '				;gdy trafi na spacje zakoncz zapis
		je zapiskoniecprocedury1	
										;gdy nasz znak nie jest białym znakiem
		mov	byte ptr ds:[di], ah		;przepisywanie argumentow bez białych znaków,od teraz są w ds:di
		inc	si							;inkrementujemy kolejny element do zapisu
		inc	di							;inkrementujemy w tablicy liniapolecen
		inc	cx							;zwiększenie licznika długości
		jmp poczatekzapisu		        ; powtarzanie
		
	zapiskoniecprocedury2:
		mov al,1	;flaga konca linii komend
	
	zapiskoniecprocedury1:
		popf
		ret
		
	
	
	
;////////Procedura outbialeznaki////////////

outbialeznaki:
	pushf
	mov	al, 0	;flaga końca linii nie jest ustawiona
	
	poczatekoutbialeznaki:
		mov	ah, byte ptr es:[si]		;wczytuje kolejny znak do ah,es:si wskazuje na poczatek analizy
		cmp ah,9d				;gdy trafi na tabulator wczytuje następny znak
		je poczatekoutbialeznakinastepny ;powtarza procedurę 
		cmp ah,' '				;gdy trafi na znak spacja wczytuje nastepny znak
		je poczatekoutbialeznakinastepny		;powtarza procedurę 	
		cmp ah,13				;gdy koniec linii
		je poczatekoutbialeznaki1				;ustawienie flagi na 1
						
		jmp poczatekoutbialeznakikoniec ;znak nie jest końcem linii, spacją ani tabem więc zakońćz procedurę
		
	poczatekoutbialeznaki1:
		mov al,1
		
	poczatekoutbialeznakinastepny:
		inc	si					   ;inkrementacja, idziemy dalej po znakach w linii komend
		jmp poczatekoutbialeznaki
		
	poczatekoutbialeznakikoniec:
		popf
		ret
		

;///////SPRAWDZA LINIE POLECEN DLA KOMPRESJI , WPISUJE DO PAMIECI NAZWY PLIKÓW WE I WY
sprawdzanieargumentowkompresja:
	
	pushf
	push	si
	push	di
	push	cx
	push	bx
	push	ax
	
	
						
	mov	cl, 0
	mov	cl, byte ptr ds:[di+1]		;dlugosc argumentu znajduje sie w cx
	cmp	cl, 12						; porównujemy
	ja	errorotwieraniewejsciowego		;gdy nazwa pliku ma wiecej niz 12 znakow wyrzuca blad przy otwieraniu,cf=0,zf=0
	mov	si, di							;dlatego aby zapobiec niejasnoscia dodałam sprawdzanie dlugosc argumentow i wyrzucanie odpowiedniego bledu, w di liniapolecen
	add	si, 33				;poczatek argumentow
	mov	bx, offset plikwejsciowy		;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
								
	sprawdzanieargumentowkompresjaprzepisywaniewe:
		mov	al, byte ptr ds:[si]	;w al znajduje sie kolejny bajt nazwy,si na poczatu to 33
		mov	byte ptr ds:[bx], al	;przenosimy do al
		inc	si			;zwiekszamy kolejno 	,w bx jest adres plikwejsciowy
		inc	bx			;loop działa dziei cx, zmniejsza go o 1 za każym raze wykonywania petli do czasu gdy cx=0
		loop	sprawdzanieargumentowkompresjaprzepisywaniewe	;pętla przechodzaca po znakach w nazwie
		
						;przepisywanie nazwy pliku wyjściowego do pamięci
	mov	cl, byte ptr ds:[di+2]		;sprawdzamy drugi argument , tak samo nazwa nie moze przekraczac 12 znakow
	cmp	cl, 12						; porównujemy 
	ja	errorotwieraniewyjsciowego	;jezeli wieksze (bez znaku ) to wyrzuca bład
	mov	bx, offset plikwyjsciowy		;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	
	sprawdzanieargumentowkompresjaprzepisywaniewy:
		mov	al, byte ptr ds:[si]	;al to kolejny bajt nazwy pliku
		mov	byte ptr ds:[bx], al	;przepisanie do pamięci
		inc	si			;zwiększenie indeksów, w bx jest adres plikwyjsciowy
		inc	bx		;loop działa dziei cx, zmniejsza go o 1 za każym raze wykonywania petli do czasu gdy cx=0
		loop	sprawdzanieargumentowkompresjaprzepisywaniewy	;pętla wykona się tyle razy, ile znaków ma argument drugi (nazwa pliku wyjścia)
		
	mov	ds:[xflaga],	0	;ustawia flagę na kompresję

	
	pop ax
	pop	bx
	pop	cx
	pop	di
	pop	si
	popf
	ret

	;/////////////////CHODZENIE PO LINII POLECEN DLA DEKOMPRESJI/////////////////////////////
;//////zapisywanie nazwy pliku , sprawdzanie czy wpisane argumenty spełniaja odpowiednie zalożenia/////////
sprawdzanieargumentowdekompresja:
	
	pushf
	push	si
	push	di
	push	cx
	push	bx
	push	ax
	
	
	mov cl, 0
	cmp	byte ptr ds:[di+1], 2		;czy pierwszy argument ma dwa znaki
	jne	cosjestnietak				;jeśli nie - błąd,zf=0
	mov si,di						; w di liniapolecen
	add si, 33						;poczatek argumentow
	cmp	byte ptr ds:[si], '-'		;sprawdzamu czy pierwszy to "-", si to poczatek pierwszego argumentu
	jne	cosjestnietak				; jesli nie to bład ,zf=0
	cmp	byte ptr ds:[si+1], 'd'		; i czy drugi to "d",przesuwamy sie na drugi znak
	jne	cosjestnietak				;jesli nie to blad,zf=0
	
	
	
	mov	cl, byte ptr ds:[di+2]		;długość drugiego argumentu jest w cl
	cmp	cl, 12						; porównujemy nie działa dla nazwy pliku dluzszej niz 12 znaków
	ja	errorotwieraniewejsciowego		;dlatego porównujemy czy nazwa ma wiecej niz 12 znakoów jeżeli tak to,zf=0,cf=0
	mov	si, di							; wyrzuca błąd, pomaga nam to zakwalifikować bład
	add	si, 35				;poczatek drugiego argumentu, bo dlugosc pierszego to 2 wiec 33+2
	mov	bx, offset plikwejsciowy   ;wyrzucamy gdzie chcemy zapisac nazw pliku wejsciowego
	mov	ah, 0						;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	sprawdzanieargumentowdekompresjaprzepisywaniewe:
		mov	al, byte ptr ds:[si]	;tutaj musimy wykonac pętle gdyż nie wiemy ile znaków ma nazwa pliku wejsciowego
		mov	byte ptr ds:[bx], al	;przenosimy do al
		inc	si			;zwiekszamy
		inc	bx			;w si jest poczatek argumentu a bx to licznik dla plikwejsciowy
		loop	sprawdzanieargumentowdekompresjaprzepisywaniewe
						;dzięki tej pętli w si jest pokazane gdzie konczy sie drugi argument
						
	
	
	mov	cl, byte ptr ds:[di+3]		;sprawdzamy teraz 3 argument , a dokładnie jego długość 
	cmp	cl, 12						; aby wyeliminowac blad zbyt dlugiej nazwy plu ktora rowniez dotyczy pliku tworzonego
	ja	errorotwieraniewyjsciowego		;jezeli nazwa jest dluzsza niz 12 znakow to blad,cf=0,zf=0
	mov	bx, offset plikwyjsciowy		;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	
	sprawdzanieargumentowdekompresjaprzepisywaniewy:
		mov	al, byte ptr ds:[si]	;i tak jak w poprzednim przypadku chodzmy po każdym znaku i zapisujemy do pamieci
		mov	byte ptr ds:[bx], al	
		inc	si			;zwiększamy
		inc	bx			;tak jak wyżej
		loop	sprawdzanieargumentowdekompresjaprzepisywaniewy	;przechodzimy po nazwie aż natrafimy na jej koniec
	
	mov	ds:[xflaga], 1	;ustawia flagę na dekompresję
	
	
	pop ax
	pop	bx
	pop	cx
	pop	di
	pop	si
	popf
	ret
	
;////////TERAZ BĘDZIEMY DOKONYWAĆ KOMPRESJI ////////////	


kompresja:

	pushf
	push	bx
	push	cx
	push 	ax
	
	
	call	czytajznaki			; zwraca wartosc ah = 1 gdy jest koniec pliku lub 0 gdy nie jest to koniec pliku
	cmp	ah, 1					;jeżeli są rowne to znaczy ze jest to koniec pliku
	je	kompresjakoncawiersza		; wykonujemy skok,zf=0
		
	mov	bl, al			;do bl wysyłamy poprzedni znak który jest w al 
	mov	cx, 0			; w cx jest licznik
	
		kompresjapokoleji:
		
			cmp	al, bl			;sprawdzamy czy znaki sa takie same
			jne	kompresjainny		;jezeli nie to ,zf=0
			cmp	cx, 255			;sprawdzamy czy mamy 255, bajt może pomieścić max 255 znaków a licznik w tym przypadku ma rozmiar 1 bajtu
			je	kompresjainny   ;jezeli jednak sa rowne to traktujemy to jako inny znak, na licznik powtórzeń rezerwujemy jeden bajt 
						;zf=0		;wiec nie może on przekroczyć 255
								;jeśli więc w pliku wejsciowym wystepuje ciag wiecej niz 255 identycznych bajtow to kompresujemy kazde 255 bajtow tego ciagu z osobna
	
		komprejsjatakisamznak:			;gdy znak jest taki sam jak poprzedni
			inc	cl						; zwiększamy licznik tego znaku
			jmp	kompresjakoniectensam	
		
		kompresjainny:			;gdy znak jest inny niz jego poprzednik
			cmp	cl, 3		;jezeli jest mniej niz 3 to sa przepisywane,jezeli jest wiecej niz 3 to traktujemy to jako sekwencje,porównujemy licznik 
			ja	kompresjasekwencja ;jeśli większe niż 3,cf=0,zf=0
		
		kompresjaprzepisz:
			call	kompresjaprzepiszznak  ; jeżeli nie jest to sekwencja znakow
			jmp	kompresjakoniecinny	
		
		kompresjasekwencja:				;procedura wywoływana w przypadku znaków pojawiajacych sie czesciej niz 3 razy
			call	kompresjanabajty
			jmp	kompresjakoniecinny		
	
	
	
		kompresjakoniecinny:	;gdy znaki były inne 
			mov	bl, al			; z al przenosimy do bl,w al jest poprzedni znak a w bl jest aktulalny 
			mov	cx, 1			;ustawiamy cx na 1 czyli tak jakby zamiast kilku byl 1,ustawiamy licznik na 1
	
		kompresjakoniectensam:		
			call	czytajznaki
			cmp	ah, 1					; gdy koniec wiersza to
			je	kompresjakoncawiersza	;to skaczemy,zf=1
			jne	kompresjapokoleji		;jezeli to nie koniec wiersza,zf=0,jeżeli to nie jest koniec wiersza to wykonujemy opreacje jeszcze raz

	
		kompresjakoncawiersza:				
		cmp	cl,3			;sprawdzamy czy ostatni wiersz ma wiecej niz 3 znaki
		jna	kompresjakoncawierszaprzepisywanie		;jezeli nie to przepisujemy cf=1 lub zf=1
		ja	kompresjakoncawierszanabajty		;jezeli tak to traktujemy to jako sekwencje cf=0,zf=0
		
		kompresjakoncawierszaprzepisywanie:
		call	kompresjaprzepiszznak
		jmp 	kompresjakoncawierszabufor		;wypisujemy wykonana rzecz czyli skompresowane dane
	
		kompresjakoncawierszanabajty:
		call	kompresjanabajty				;wykonujemy to w przypadku gdy koniec wiersza jest sekwencja
		
		kompresjakoncawierszabufor:
		call	wypiszbufor
			
		pop	ax
		pop	cx
		pop	bx
		popf
		ret

kompresjaprzepiszznak:
						;w bl jest zapisany znak w cx jest licznik
		pushf
		push	bx
		push	cx
		push 	ax

	cmp	bl, 0			;sprawdzamy czy jest rowne zero
	jne	kompresjaprzepiszznakstart	;gdy nie jest zero to przepisujemy,zf=0	
	cmp	cx, 1			;sprawdzamy , dla zera ile ich jest 
	je	kompresjaprzepiszznakzero		;	gdy jedno zero	zf=1	 
	call kompresjanabajty	;jeżeli wiecej niz jedno zero to traktujemy to jako sekwencje
	jmp	kompresjaprzepiszznakkoniec
	
	kompresjaprzepiszznakzero:
	mov	cx,2			; gdy jest jedno zero to zapisujemy jako 2, wymóg zadania
	
	kompresjaprzepiszznakstart:
	mov	al, bl					;przepisujemy znak bo nie jest równy zero , czyli aktulalny znak staje sie poprzednim
	
	kompresjaprzepiszznakzapisz:
	call	zapiszznakdopliku   ;zapisjumey znak
	loop kompresjaprzepiszznakzapisz  ;dopóki cx=0
	
	kompresjaprzepiszznakkoniec:
	
		pop	ax
		pop	cx
		pop	bx
		popf
		ret
	
kompresjanabajty:				;wywołujemy gdy mamy więcej niż jedno zero lub sekwencje czyli więcej niż 3 znaki 
	
	pushf
	push ax
	
	
	mov	al, 0					;wypisujemy zero 
	call	zapiszznakdopliku
	mov	al, cl 					;w cx znajduje się ilość bajtów , w cx jest licznik  
	call	zapiszznakdopliku
	mov	al, bl					;w bl zapisany jest znak ,znak aktualny staje sie poprzednim
	call	zapiszznakdopliku   ;musimy zapisac te dane aby móc wykonać prawidłową kompresje 
	
	pop ax
	popf
	ret
	
dekompresja:

	push 	ax
	push	bx
	push	cx
	pushf
	
	dekompresjapokoleji:
	call	czytajznaki	;czytaj znaki
	cmp	ah, 1			;sprawdzamy czy jest to koniec pliku, ah jest flaga która ustawiona mówi czy to koniec czy nie 
	je	dekompresjakoncawiersza ; jeżeli tak to wykonujemy skok,zf=1
	
	cmp	al, 0			;czy bajt jest zerem
	je	dekompresjapominbajt ; jezeli tak to go pomijamy,zf=1				
	call	zapiszznakdopliku   ;jak nie jest zerem
	jmp	dekompresjapokoleji	;az do skutku 
	
	dekompresjapominbajt:
	call	czytajznaki	
	
	cmp	ah, 1			;jeśli jest to koniec wiersza to występuje błąd który powoduje niemożliwość wykonania dekompresji
						; po zero powinno wystepowac drugie zero lub licznik 
	je	cosjestnietak	; gdy bład to skacze na końcu powinna być 1 ,zf=1 na końcu nie może być zera dlatego wystepuje blad jezeli tak nie jest
	
	cmp	al, 0			;dla dwoch zer wypisujemy jedno
	jne	dekompresjasekwencja	;gdy jednak to nie są 2 zera to jest to sekwencja zf=0
	call	zapiszznakdopliku		;a gdy nie jest to sekwencja 
	jmp	dekompresjapokoleji	;robimy od poczatku
	
	dekompresjasekwencja:		; gdy występuje sekwencja
	mov	cl, 0			;zerujemy licznik
	mov	cl, al			;ilosc powtórzen tego samego znaku przenosimy do cl
	call czytajznaki	
	cmp	ah, 1			;sprawdzamy czy to jest koniec pliku
	je	cosjestnietak	; jeżeli nie to występuje bład który powoduje że nie możemy wykonać dekompresji
					
	dekompresjazapisywanie:		; gdy nie ma błedu, zapisujemy znak tyle razy ile wynosi licznik
		call	zapiszznakdopliku
		loop	dekompresjazapisywanie ;loop wykonuje sie dopoki cx=0
		loop	dekompresjapokoleji	;wykonujemy aż do skutku,wykonujemy operacje jeszcze raz 
	
	dekompresjakoncawiersza:
		call	wypiszbufor
		
		popf
		pop	cx
		pop	bx
		pop	ax
		ret
	 	
;/////////////procedury wewnetrzne///////////////////////////
	
;procedura czytajznaki która dla pliku jeszcze nie skonczonego zwraca 0 w ah, a dla konca 1 
czytajznaki:
	
	push	bx
	push	cx
	pushf
	

	czytajznakiwczytywanie:	
	mov	bx, word ptr ds:[buforwejsciowypozycja]	;pozycja bufora 
	cmp	bx, ds:[buforibajty]		;sprawdza czy jest cos jeszcze do wczytania
	jb	czytajznakibufor				;jesli tak to wykonuje skok, cf=1, gdy mniejsze
	
							 
	cmp	ds:[buforibajty], 16384					;sprawdzamy czy sa równe
	je	czytajznakiwypelniaczbufora				;jesli jest rowno to wykonujemy skok, istnieje mozliwosc że w pliku cos pozostalo,zf=1
	
	mov	ah, 1					;ustawiamy flage gdy juz nic nie ma
	mov	al, 0					;zerujemy al ,wczytanym bajtem jest bajt zerowy
	jmp	czytajznakikoniec			;zakoncz procedure
	
	
		;///////////WYPELNIA BUFOR WEJSCIA/////////////
		czytajznakiwypelniaczbufora: 
	
			push 	ax
			push	bx
			push	cx
			push    dx
			pushf	
					;//procedura odczytywania 3fh przerwania 21h
			mov	bx, word ptr ds:[wskazniknawejsciowy];     wskaźnik dla pliku wejściowego , w bx jest uchwyt pliku 
			mov	cx, 16384				; w tej procedurze w cx musi być liczba bajtow do wczytania 
			mov	ah, 3fh					;procedura 3fh odczytywania z pliku przerwania 21h
			mov	dx, offset buforwejsciowy			;OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
			int 21h
			mov	word ptr ds:[buforibajty], ax	; liczba wczytanych bajtow 
			mov	word ptr ds:[buforwejsciowypozycja], 0  ;zerujemy bufor wejsciowy bo jak będziemy robić pętle to pozycje będziemy przenosić z bx
	
			popf
			pop 	dx
			pop 	cx
			pop		bx
			pop 	ax

			jmp	czytajznakiwczytywanie					
	
		czytajznakibufor:						;cos jest w buforze, wiec musimy to odczytac
			mov	ah, 0					;ustawiamy flage konca pliku na zero, czyli to nie koniec
			mov	al, byte ptr ds:[buforwejsciowy+bx]	;przechodzimy po buforze bajt po bajcie
			inc	ds:[buforwejsciowypozycja]		;zwiekszamy
	
	
czytajznakikoniec:
	
	popf
	pop	cx
	pop	bx
	ret 

	
;///////zapisuje bajt do al////////////////
zapiszznakdopliku:
	
	push	ax
	push	bx
	push	cx
	pushf
	
	
	mov	bx, word ptr ds:[buforwyjsciowypozycja]	;pozycja bufora wyjsciowego
	mov	byte ptr ds:[buforwyjsciowy + bx], al	;wpisujemy kolejny znak
	inc	bx					;przesuwamy sie po nim
	mov	word ptr ds:[buforwyjsciowypozycja], bx  ;przenosimy
	cmp	bx, 16384				;sprawdzamy czy jest miejsce
	jb	zapiszznakdoplikukoniec			;gdy jest jeszcze miejsce to skaczemy,cf=1					
	call	wypiszbufor			;gdy nie ma to wypisujemy co w nim jest

	zapiszznakdoplikukoniec:
	
	
	popf
	pop	cx
	pop	bx
	pop	ax
	ret 

;//////PROCEDURA WYPISUJACA BUFOR///////
wypiszbufor:

	
	push	ax
	push	bx
	push	cx
	push	dx
	pushf	
		
	
	mov	cx, word ptr ds:[buforwyjsciowypozycja]			;przesyla pozycje w buforze do cx,to co jest nim to costanie wypisane
	mov	dx, offset buforwyjsciowy					;w ds:dx jest ścieżka dostępu, OFFSET jest to operator wydzielający z wyrażenia adres początku względem początku segmentu
	mov	bx, ds:[wskazniknawyjsciowy]				;w bx uchwyt pliku
	mov	ah, 40h							;procedura 40h przerwania 21h, zapis do dojscia
	int	21h	
	mov	word ptr ds:[buforwyjsciowypozycja], 0			;i zerujemy ,aby przesunac sie na poczatek 
	
	popf
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
	
		


wykazbledow:					
	pushf			;wyrzuca tekst błedu 
	push ax
	mov	ah, 9			
	int	21h
	pop ax
	popf
	ret
	
code1 ends



stos1 segment stack
	dw 200 dup(?)
	wstosu	dw ?
stos1 ends


end start	

			
		
