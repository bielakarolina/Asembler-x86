;Karolina Biela 
;poniedziałek 16:15 A

dane1 segment

liniapolecen db 200 dup(0) ; tu będą zapisywane argumentu uż po przetworzeniu
xflaga db 0 ;opcja zmiany zero lub jeden 
szachownica db 153 dup(0)
xzmienna db 0 ; zmienna używana przy modyfikacji
znakiascii db '.','o','+','=','*','B','O','X','@','%','&','#','/','^'

errorbrakargumentow db "NIE PODALES ARGUMENTOW $"
errorzmienne1 db "PODALES ZLA ILOSC ZMIENNYCH $"
errorzmienne2 db "PIERWSZY ARGUMENT JEST ZLY, POWINIENES PODAC 0 LUB 1 $"
errorzmienne4 db "WPISALES ZLY ZNAK W PIERWSZYM ARGUMENCIE, POWINIENES PODAC 0 LUB 1 $"
errorzmienne3 db "DRUGI ARGUMENT POWINNIEN MIEC 32 ZNAKI $"
errorzmienne5 db "DRUGI ARGUMENT ZAWIERA NIEODPOWIEDNIE ZNAKI LUB DUZE LITERY $"
dane1 ends

code1 segment 
start:
	
	mov ax,seg wstosu       ;inicjalizacja stosu
	mov ss,ax 				;do rejestrow segmentowych przesylamy przez inne rejestry
	mov	sp,offset wstosu

	mov ax,ds
	mov es,ax
							
	mov	si,82h	   ;82h oznacza pierwszy znak,bo 80h to liczba wisanych argumentow,81h to spacja
	
	mov ax,seg szachownica
	mov ds, ax	
	
;////////PROCEDURY SPRAWDZAJĄCE ZGODNOŚĆ ARGUMENTÓW Z WYTYCZNYMI,NASTĘPNIE ZAPISUJĄCE JE/////////////// 
;//////// NA POCZĄTKU SPRAWDZA CZY SĄ JAKIEŚ ARGUMENTY JEŻELI TAK DO WYWOŁUJEMY PROCEDURE parse/////////
;////////NASTĘPNIE SPRAWDZAMY CZY ARGUMENTY SĄ ZGODNE Z WYMOGAMI///////////////////////////////////////	
	cmp	byte ptr es:[80h], 0	;sprawdza czy liczba znakow w linii komend jest zero 
	je brakargumentow			; gdy brak argumentów 
	
	mov	di, offset liniapolecen	
	call parse
	
	cmp	byte ptr ds:[di], 2		;sprawdza czy są podane dwa argumenty
	jne zlailoscargumentow
	cmp	byte ptr ds:[di+1], 1	;sprawdza czy argument nr.jeden ma długość 1
	jne zladlugoscpierwszegoargumentu
	cmp	byte ptr ds:[di+2], 32	;sprawdza czy drugi argument ma 32 znaki
	jne zladlugoscdrugiegoargumentu		
	mov	al, byte ptr ds:[di+33]		;sprawdza czy pierwszy argument jest 0 lub 1
	call sprawdzaniepierwszejzmiennej

	
	add di, 34                ;di jest wskaźnikiem drugiego argumenu wpisanego w linie komend
	
	
	call modyfikacjadrugiegoargumentu   ; wywołujemy procedure która będzie pracowała na drugim argumencie
										; sprawdzi ona czy znaki są z zakresu 0-9 a-f bez dużych liter
										;później przetworzy argument na binarny
	
	
	call zrobobrazek					;wywołuje na ekran obrazek storzony przez program na podstawie argumentów	

	
;///////PODSTAWOWE PROCEDURY ZAMYKANIA ORAZ WYWOŁYWANE PODCZAS BLĘDÓW W ARGUMENTACH/////////////////////	
	
	zakonczprogram:
		mov ax, 04ch	
		int	16h                           ; nie może być zwykłego zamknięcia programu
		mov ax,04ch						; gdyż szybciej zamyka program niż zdąrzy coś wyświetlić	
		int 21h							; dlatego dopisałam aby oczekiwał na klawisz przed zamknięciem
	

	brakargumentow:
		mov dx, offset errorbrakargumentow     ;gdy nie podamy żadnych argumentów
		call wykazbledow
		jmp zakonczprogram							

	zlailoscargumentow:	
		mov dx, offset errorzmienne1			;gdy podamy złą ilość
		call wykazbledow
		jmp zakonczprogram

	zladlugoscpierwszegoargumentu:
		mov dx, offset errorzmienne2			;gdy pierwszy argument nie ma długości jeden
		call wykazbledow
		jmp zakonczprogram		
	
	zladlugoscdrugiegoargumentu:
		mov dx , offset errorzmienne3			;gdy drugi argument nie ma długości 32
		call wykazbledow
		jmp zakonczprogram		
	
	zlypierwszyargument2:	
		mov dx, offset errorzmienne4
		call wykazbledow                       ;wywoływana w późniejszych procedurach, gdy pierwszy
		jmp zakonczprogram						; argument nie ma postaci zerojedynkowej 

	zleznakiwdrugim:
		mov dx, offset errorzmienne5			;wywoływana w póżniejszych procedurach, gdy drugi
		call wykazbledow						; argument ma nieodpowiednie znaki
		jmp zakonczprogram
	

;////////PROCEDURA sprawdzaniepierwszejzmiennej///////////
;///////SPRAWDZA CZY ARGUMENT NUMER 1 JEST ZEREM LUB JEDYNKĄ///////////

sprawdzaniepierwszejzmiennej:

	push bx
	pushf

	mov	bx, offset xflaga		;xflaga to rodzaj modyfikacji (0 lub 1)

	cmp	al, '0'				;sprawdza czy jest zerem
	jne	pierwszazmiennaniejestzerem			;jeżeli nie to musi być jedynką
	mov	byte ptr ds:[bx], 0	;jest zerem	
	jmp sprawdzaniepierwszejzmiennejkoniec  ;zakończenie sprawdzania
	
	pierwszazmiennaniejestzerem:
		cmp al,'1'               ;sprawdza czy jest jedynką
		jne zlypierwszyargument2  ;jeżeli nie jest to wywołąj błąd
		mov	byte ptr ds:[bx], 1	;nie wywołano błedu czyli jest jedynka
	
	sprawdzaniepierwszejzmiennejkoniec:
		pop bx								;zakończenie procedury 
		popf
		ret

		
;////////Procedura parse ///////////////////

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
		je konieczapisu  ;konczy proces gdy napotka koniec linii
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
		mov	ah, byte ptr es:[si]		;wczytuje kolejny znak do ah
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

;////////////Procedury poruszania skoczkiem,modyfikacji argumentu i wyświetlania wyniku//////////////////


modyfikacjadrugiegoargumentu:		
push si
push bx
push cx

		mov si, offset szachownica ; poczatek szachownicy w si, obsługa ruchu pionkiem
		mov bx, 76 ; zaczynamy w tym polu, który jest środkiem szachownicy 
		mov cx, 16 ; 32 bajty to 16 ich par do modyfikacji 
		
		modyfikacjapoczatek:
		mov al, byte ptr ds:[di]  ;pobiera najmłodszy znak z pary czyli prawy
		inc di					; przesuwa się do kolejnej w argumencie drugim	
		mov ah, byte ptr ds:[di] ;pobiera najstarszy z pary czyli lewy
	;musimy teraz wywołać procedure zmiany drugiego argumentu z postaci szesnastkowej na binarny 
	;oraz sprawdzić czy spełnia odpowiednie kryteria poza podstawowymi 
	
	call paranabinarny
	cmp ah, 1      ;warunek gdy argument nie spełnia kryteriów
	je zleznakiwdrugim ; wywołuje błąd drugiego argumentu
	inc di         ;gdy wszystko ok to jedziemy dalej
	
	call ruchpionkiem ; włączmy nasz pionek i w zależności od przetworzonego drugiego argumentu ruszamy nim
	
	loop modyfikacjapoczatek  ;powtarzanie procedury aż do ostatniej pary 
	
	mov byte ptr ds:[si+76], -2  ; wstawianie znaku specialnego S dla poczatku ruchu gonca czyli w polu 76
	mov byte ptr ds:[si+bx], -3  ;wstawianie znaku specialnego E dla koncowego pola ruchu gonca 
	
	pop cx
	pop bx
	pop si
	ret
	
;/////////zamiana argumentów na postać binarną///////////////////////////
;/////////wszystkie wartości są w ah, drugi argument oraz postać już przetworzona/////////
;////////dl używane jako flaga błędu, która wyrzuca na ekran error gdy drugi argument ma złą składnie///////

nabinarny:

	mov dl, 0 ;zaczynamy z flagą błedu ustawioną na 0
	
	cmp ah, '0'
	jb nabinarnyblad    ; sprawdza czy w drugim argumencie są liczby mniejsze od zera
	cmp ah, '9'
	jna nabinarnycyferka ; sprawdza czy jest cyferką
	cmp ah, 'f' 
	ja nabinarnyblad   ; sprawdza czy w drugim argumencie sa literki większe od flaga
	cmp ah, 'a'
	jnb nabinarnymalaliterka ; sprawdza czy jest małą literką a jak tak czy jest z zakresu a-f
	
	; jeżeli żadne z tych kryteriów nie zostało spełnione to mamy tylko jedną możliwość
	; a tą możliwością jest duża literka
	jmp nabinarnyduzaliterka 
			
	nabinarnyblad:
		mov dl, 1   ;ustawiamy gdy w drugim argumencie nie zostały spełnione odpowiednie kryteria
	
	nabinarnycyferka:
		sub ah, '0'
		jmp nabinarnykoniec
	
	nabinarnymalaliterka:
		sub ah, 'a'
		add ah, 10
		jmp nabinarnykoniec
	
	nabinarnyduzaliterka:
		call nabinarnyblad
		jmp nabinarnykoniec
	
	nabinarnykoniec:
		ret
		
;/////procedura zamieniająca pare znaków na binarny/////////////
;//// gdy ustawimy dl na 1 to oznacza że nie udało się zamienić ///////
paranabinarny:

	pushf
	push cx
	push bx
	push dx

		mov bl, 0  ; w bl będziemy przechowywać wyniki
		call nabinarny ;wywołujemy procedure zmiany na bajty
		
		cmp dl, 1 
		je paranabinarnyerror ; wyrzuca gdy nie da się zamienić
		
		add bl,ah ; dodajemy do obecnego wyniku który znajduje się w ah-patrz nabinarny
		mov cl, 4  
		shl bl,cl ;przesuwamy o 4 bity 
		
		mov ah,al
		call nabinarny 
		cmp dl, 1  
		je paranabinarnyerror
		
		add bl,ah   ;dodajemy wyniki
		mov al,bl   ; przesyłamy do al
		mov ah, 0   ; gdy nie wystąpił bład podczas zamiany flage błedu ustawiamy na 0
		jmp paranabinarnykoniec
				
		paranabinarnyerror:
			mov ah, 1            ;gdy podczas zamiany wystąpi błąd , ustawiamy flage błędu
		
		paranabinarnykoniec:
			pop dx
			pop bx
			pop cx
			popf
			ret
			
;/////////musimy teraz napisać procedure ruchu pionka po szachownicy////////////
;/////////naszym pionkiem jest skoczek który wykonuje za każdym razem 4 ruchy ///////
;////////będziemy korzystać oczywiście z poprzednich procedur///////////
;/////// z których wynika że bity znajdują się w al////////
;///////dodatkowo w bx zapisane jest miejsce pobytu pionka/////

ruchpionkiem:
	
	pushf
	push cx
	
	mov cx, 4          ; poruszamy się skoczkiem, a on wykonuje 4 ruchy
	
	ruchpionkiempoziomo:
	shr al, 1          ; dla bitu po prawej czyli młodszego, ruch w prawo dla 1, w lewo dla 0
	jc ruchwprawo
	jmp ruchwlewo
	
	ruchpionkiempionowo:
	shr al, 1         ;dla bitu po lewej czyli starszego, ruch w góre dla 0, w dół dla 1
	jc ruchwgore
	jmp ruchwdol
	
	liczbaodwiedzin:
	inc byte ptr ds:[si+bx]   ;dodajemy odwiedziny
	loop ruchpionkiempoziomo
	
	ruchpionkiekoniec:
	pop cx
	popf
	ret

;///każde z ruchów ma pewne ograniczenie,koniec szachownicy, które musimy uwzględnić w ruchu///////	
	ruchwprawo:     ;ruch w prawo nie może się odbywać gdy pole na którym znajduje się pionek
	pushf           ;jest wielkrotnością k*17-1
	push ax
	push dx
	mov ax,bx
	add ax,1        ;dlatego dodajemy jeden do miejsca gdzie znajduje się pionek i dzielimy przez 17
	mov dl,17  
	div dl 
	cmp ah, 0       ; jeżeli resztą z dzielenia będzie 0 to znaczy że ruch w prawo jest niewykonalny  
	je ruchwprawokoniec
	inc bx          ;jeżeli nie jest 0 to wykonujemy ruch w prawo
	
	ruchwprawokoniec:
	pop dx
	pop ax
	popf
	jmp ruchpionkiempionowo

	ruchwlewo:		; w tym przypadku ruch nie może się odbyć gdy pole jest wielokrotnością 17
	pushf
	push ax
	push dx
	
	mov ax,bx
	mov dl, 17    
	div dl
	cmp ah,0          ; jeżeli resztą z dzielenia będzie 0 to ruch w lewo jest niewykonalny
	je ruchwlewokoniec
	dec bx            ; gdy reszta z dzielenia nie jest 0 
	
	ruchwlewokoniec:
	pop dx
	pop ax
	popf
	jmp ruchpionkiempionowo
	
	
	ruchwgore: ; jeżeli pole jest poniżej 17 to nie możemy wykonać ruchu
	pushf
	
	cmp bx , 17
	jb ruchwgorekoniec
	sub bx, 17   ;przesuwamy się o jeden wiersz do góry
	
	ruchwgorekoniec:
	popf
	jmp liczbaodwiedzin 
	
	ruchwdol:  ;jeżeli pole jest powyżej 135 to nie możemy wykonac ruchu 
	pushf
	
	cmp bx, 135
	ja ruchwdolkoniec
	add bx, 17 ;przesuwamy się o jedną linijke niżej
	
	ruchwdolkoniec:
	popf
	jmp liczbaodwiedzin 

	
;////////////////////////////////////////////////////////////////////////////////////////////////////
;///////tworzenie naszego obrazka ///////////
;///////////////////////////////////////////////////////////////////////////////////////////////////////////

;//////przypisywanie znaku do liczby odwiedzin//////////

przypiszznak:
	pushf
	push ax
	mov bh, 0
	
	cmp bl, 0   ;gdy 0 odwiedzin to spacja
	jne znakspecialny2
	mov bl, ' '                ; wywołujemy na początku, bo jest najwięcej
	jmp przypiszznakkoniec
	
	znakspecialny2:
	cmp bl, -2
	jne znakspecialny3
	mov bl, 'S'                  ;znak spcialny dla pola 76 gdzie zaczyna droge nasz pionek
	jmp przypiszznakkoniec
	
	znakspecialny3:
	cmp bl, -3
	jne powyzej14                       ;znak specialny dla końca drogi naszego pionka
	mov bl, 'E'
	jmp przypiszznakkoniec
	
	powyzej14:
	cmp bl, 14
	jna przypiszznakzwykly      ;gdy odwiedzimy pole 14 lub wiecej razy to przypisujemy ten sam znak
	mov bl, 14      			;więc dla tych przydaków ustawiamy tak jak dla 14 
	
	przypiszznakzwykly:
	dec bx
	mov bl, byte ptr ds:[si+bx]    ;pobieramy znak z znakiascii, oczywiśie odpowiedni do liczby odwiedzin
	jmp przypiszznakkoniec

	przypiszznakkoniec:
	pop ax
	popf
	ret 
	
zrobobrazek:
	pushf
	push si
	push di
	push dx
	push cx 
	push bx 
	push ax 
	
	mov di, offset szachownica
	mov si, offset znakiascii
	
	cmp byte ptr ds:[xflaga], 1
	je zrobobrazekdlajedynki      ; gdy pierwszy argument jest 1 czyli mamy modyfikacje
	jmp zrobobrazekdlazera        ; gdy nie to bez modyfikacji
	
	
	zrobobrazekdlazera:              ;bez modyfikacji, dla 9 wierszy i 17 kolumn
	mov cx, 9   ; mamy 9 wierszy
	
	zrobobrazeklinie:
		push cx		;dzięki niemu możemy mieć pętle
		mov cx, 17    ; 17 kolumn 
		
		zrobobrazekwypisz:
			mov bh, 0 
			mov bl, byte ptr ds:[di] ; w bx jest liczba odwiedzin
			call przypiszznak
			mov dl,bl ; znak wyświetli się na ekranie
			call wyrzucobrazek
		
		inc di ; przesuwamy się do kolejnego pola na szachownicy
		loop zrobobrazekwypisz
			
		mov dl, 10
		call wyrzucobrazek
		mov dl, 13
		call wyrzucobrazek
		pop cx
		loop zrobobrazeklinie

	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	pop si 
	popf	
	ret	
	
	zrobobrazekdlajedynki:						; warjant gdy mamy modyfikacje czyli 17 wierszy i 9 kolumn
	mov cx, 17   ; mamy 17 wierszy
	
	zrobobrazeklinie1:
		push cx		;dzięki niemu możemy mieć pętle
		mov cx, 9    ; 9 kolumn 
		
		zrobobrazekwypisz1:
			mov bh, 0 
			mov bl, byte ptr ds:[di] ; w bx jest liczba odwiedzin
			call przypiszznak
			mov dl,bl ; znak wyświetli się na ekranie
			call wyrzucobrazek
		
		inc di ; przesuwamy się do kolejnego pola na szachownicy
		loop zrobobrazekwypisz1
			
		mov dl, 10
		call wyrzucobrazek   ;gdy mamy modyfikajce to obraca nam tablice na końcu na wymiary gdy nie ma 
		mov dl, 13			 ;modyfikacji, jest czytelna, gdy to usuniemy tablica będzie się zgadzała z 	
		call wyrzucobrazek		;wymiarami zgodnymi z daną modyfikacją
		pop cx
		loop zrobobrazeklinie1

	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	pop si 
	popf	
	ret	
	
wyrzucobrazek:
	push ax
	pushf   			;wyrzuca obrazek na ekran

	mov ah, 2h
	int 21h
	popf
	pop ax
	ret	
	

		
wykazbledow:					
	pushf			;wyrzuca teskt błedu 
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

	
	
	
	
	
	
	

		
		
