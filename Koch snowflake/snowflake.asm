

.387
dane1 segment
	
	

	liniapolecen	db 161 dup(0)		;zapisane argumenty z linii polecen
	liczbaiteracji		db 0		;zalezy od niej rozdzielczosc krzywej 
	dlugoscodcinka		dw 0		
	kroki		dw 0 		; przechowuje finalny ciag krokow
	krok1		db 3000 dup(0)
	krok2		db 3000 dup(0)
	czescpoczatkowa	db "F-F++F-F"	; fragment ktory bedziemy wstawiac generujac wynikowy ciag, w pojedynczej iteracji l-systemu kazdy symbol
									;F w bierzacym napisie zastepuje powyzszym napisem ,powstaly w ten sposób dłuższy napis stanowi wejscie do kolejnej 
									;iteracji l-systemu 
	
	errorbrakargumentow db "NIE PODALES ARGUMENTOW $"
	errorzmienne1 db "PODALES ZLA ILOSC ZMIENNYCH $"
	errorzmienne2 db "BLAD PIERWSZEJ ZMIENNEJ, POPRAWY ZAKRES TO OD 1 DO 4 $"
	errorzmienne3 db "BLAD DRUGIEJ ZMIENNEJ, MASZ PODAC LICZBE A NIE LITERKE $"
	errorzmienne4 db "COS JEST NIE TAK $"

	
		
	; współrzędne funkcji
	x1	dw	0
	y1	dw 	0
	x2	dw	0
	y2	dw 	0
	kieruneky1 	db 0	
	kierunekx1 	dw 0	; dla wolnego poruszania
	kierunekx2 	dw 0	; dla szybkiego poruszania
	kierunekruchu 		dw 0	; krok dla szybiego kierunku
	
	xzmienna 		dw 0   ;zmienna pomocnicza 
	przyblizenie	dd 0.5	
	
	
	
dane1 ends
;segment kodu
code1 segment
	
	
start:
	mov ax, dane1		
	mov ds, ax			
	
	mov ax, seg wstosu
	mov ss, ax			; inicjowanie stosu
	mov sp, offset wstosu		
	
	fninit			;inicjalizacja fpu,
	;Powoduje zainicjowanie pracy koprocesora. Zaznacza wszystkie rejestry koprocesora jako puste, mimo że nie zmienia ich zawartości, słowo kontrolne koprocesora przyjmuje wartość 03FF, słowo stanu przyjmuje wartość zero. 
	;Nie czeka przy tym, w odróżnieniu od instrukcji finit, na wyniki wykonywania wyjątków które mogły się w międzyczasie pojawić. 


	cmp	byte ptr es:[80h], 0	;sprawdza czy liczba znakow w linii komend jest zero , cmp porównuje nie zapisując wyniku
	je brakargumentow			; gdy brak argumentów, gdy zf=1
	
	
	call parse		; usuwa biale znaki, liczy ilosc i dlugosc argumentow,zwraca w al ilosc argumentow
	
	
	call sprawdzanieargumentow	; wyodrebnia potrzebne informacje i sprawdza poprawnosc
	call tworzkroki	; tworzy ciag krokow dla funkcji
	call trybgraficzny	; wlaczamy tryb graficzny
	call rysowaniefunkcji		; rysuje krzywa na poddstawie wygenerowanej tablicy
	call wychodzenieztrybgraficznego	; czeka na zacisniecie esc i wychodzi do trybu tekstowego
	
	; koniec programu
	mov	ax, 04c00h		
	int	21h 			
	
	trybgraficzny :
	push ax
	
	mov ah,0h			
	mov al,13h			; uruchomienie trybu graficznego 320x200 
	int 10h				
	
	mov ax,0a000h		; w trybie graficznym to adres segmentu
	mov es,ax			; jezeli go nie dodam grafika sie nie wyswietli
	
	pop ax
	ret


wychodzenieztrybgraficznego :
	
	push ax
			
	mov ax, 1h			;czekamy na enter
	int	16h
	
	mov ah,0h
	mov al,3h		; przechodzimy w tryb tekstowy
	int 10h			
	
	pop ax
	ret
	

;/////Krzywa Kocha powstaje z odcinka, poprzez podzielenie go na 3 części i zastąpienie środkowej ząbkiem (o ramieniu długości równej 1/3 odcinka)
; takim, że wraz z usuwaną częścią tworzy trójkąt równoboczny. 
;Krok ten jest powtarzany dla każdego fragmentu odcinka.
;Stos koprocesora składa się z 8 poziomów ponumerowanych od 0 do 7. 
;Szczyt stosu ma numer 0. Pola wskazuje się poprzez napisanie ich jako parametr np.: 
;Koprocesor umieszcza każdą nową daną na szczycie stosu (st0), przesuwając pozostałe o jedno pole w dół. Działa więc inaczej niż stos procesora. 
;Pojedyncza precyzja. Liczby takie zajmują po 4 bajty i ich wartość maksymalna wynosi ok.  (10^39).



	
; rysuje krzywa
rysowaniefunkcji :
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov si, word ptr ds:[kroki]	; w si jest przechowywane sa kroki
	
	call obliczniepunktupoczatkowego	;oblicza punkt poczatkowy
	
	fldpi		; wczytujemy wartosc pi 
	mov word ptr ds:[xzmienna], 3  ;w xzmiennej ustawiamy 3 bo 180/3 to jest 60 czyli jeden obrót
	fild word ptr ds:[xzmienna]	;Ładuje liczbę całkowitą z wskazanego miejsca pamięci , przekształca ją na rzeczywistą i wstawia na wierzchołek stosu zmiennoprzecinkowego
	fdivr st(0), st(1)		;wartosc na pierwszego st(0) to 60 a dla drugiego st(1) to 180
	
	fild word ptr ds:[dlugoscodcinka]	; wczytujemy  dlugosc odcinka
	;Ładuje liczbę całkowitą z wskazanego miejsca pamięci , przekształca ją na rzeczywistą i wstawia na wierzchołek stosu zmiennoprzecinkowego
	;dlugoscodcinka to wartosc dlugosci prostej którą będziemy dzielić na 3 cześci i w zależności od liczby iteracji
	;wykonywac dzielenie na podzielonych juz fragmentach 
	
	fild word ptr ds:[y1]
	fild word ptr ds:[x1]	; wczytujemy współrzedne poczatkowe 

	;teraz sprawdzamy rodzaj znaku, gdy mamy + to obracamy sie o 60 stopni w prawo'zgodnie z kierunkiem ruchu zegara
	;gdy mamy - to obracamy  sie o 60 stopni w lewo,przeciwnie do ruchu wskazówek zegara
	;a gdy F to przesuwamy o "dlugoscodcinka" pikseli zgodnie z jego aktualnym kierunkiem	
	
	rysowaniefunkcjipetla:
		cmp byte ptr ds:[si], '+'
		jne rysowaniefunkcjikolejny ;jezeli to nie jest + to idziemy na kolejny
			fld st(4)		; wartosc kata na poczatku,powoduje załadowanie liczby rzeczywistej z adresu podanego argumentem
			fadd st(0), st(4)	; dodajemy obrot,dodaje do siebie operandy i wynik zostawia w drugim
			fstp st(5)		;wynik dodawania,Powoduje zapisanie liczby rzeczywistej do adresu podanego argumentem ,wskaznik stosu jest zwiekszany o 1
			jmp rysowaniefunkcjisprawdzznak
		rysowaniefunkcjikolejny:
		cmp byte ptr ds:[si], '-' ;sprawdzanie czy to jest -
		jne rysowaniefunkcjikolejny2			;jezeli nie to przeskok
			fld st(4)		;wartosc kata na poczatku,powoduje załadowanie liczby rzeczywistej z adresu podanego argumentem
			fsub st(0), st(4)	; dodajemy obrot,dodaje do siebie operandy i wynik zostawia w drugim
			fstp st(5)			;wynik dodawania ,Powoduje zapisanie liczby rzeczywistej do adresu podanego argumentem,wskaznik stosu jest zwiekszany o jeden
			jmp rysowaniefunkcjisprawdzznak
		rysowaniefunkcjikolejny2: ;skoro to nie - ani + to musi byc to F,czyli przesuwamy o dlugoscodcinka pikseli zgodnie z kierunkiem 
				;ladujemy wartosci x2 i y2,zapisujemy stare wartosci
			fist word ptr ds:[x2] ; ładuje liczbę całkowitą z szczytu wierzchołka stosu do podanego miejsca w pamięci
			fxch st(1)            ;Zamienia zawartość st0 z podanym operandem
			fist word ptr ds:[y2]   ;ładuje liczbę całkowitą z szczytu wierzchołka stosu do podanego miejsca w pamięci
			fxch st(1)			;Zamienia zawartość st0 z podanym operandem
			;teraz bedziemy liczyc nowy wierzcholek
			;licze kolejny punkt do zamalowania za pomoca wspolrzednych biegunowych dodajc odpowiednie wartosci cos lub sin kata
			;obliczenie nowego y1
			fld st(4)	; poczatkowy kat,Powoduje załadowanie liczby rzeczywistej z adresu podanego argumentem
			fsin		; wartosc sinusa, Oblicza sinus kąta o wartości podanej w radianach w st0 i wynik zapamiętuje w st0
			fmul st(0), st(3)	; teraz mnozymy dlugosc i sinus
			faddp st(2), st(0)	; dodanie do wartosci y1
			
			;teraz bedziemy liczyc x1 
			fld st(4)	; wartosc kata na poczatku,Powoduje załadowanie liczby rzeczywistej z adresu podanego argumentem
			fcos		; wartosc cosinusa, Oblicza cosinus kąta o wartości podanej w radianach w st0 i wynik zapamiętuje w st0
			fmul st(0), st(3)	; teraz mnozymy dlugos razy cosinus
			faddp st(1), st(0)	; dodajemy do wartosci x1
			
			
			
			;teraz musimy zapisac obliczone wyzej wartosci
			fist word ptr ds:[x1]  ;ładuje liczbę całkowitą z szczytu wierzchołka stosu do podanego miejsca w pamięci
			fxch st(1)				;Zamienia zawartość st0 z podanym operandem
			fist word ptr ds:[y1] ;ładuje liczbę całkowitą z szczytu wierzchołka stosu do podanego miejsca w pamięci
			fxch st(1)				;Zamienia zawartość st0 z podanym operandem

			call rysowanielinii
		
		rysowaniefunkcjisprawdzznak:
		inc si
		cmp byte ptr ds:[si], 0				;jezeli wartosc dodawania jest rozna od zera 
		jne rysowaniefunkcjipetla			; to chodzimy dalej
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
;funkcja wylicza wartosci punktu poczatkowego

obliczniepunktupoczatkowego :
	push ax
	push cx
	
	
	mov word ptr ds:[x1], 160 ;zaczynamy na srodku czyli 320/2 i 200/2 
	mov word ptr ds:[y1], 100		
	
	;teraz obliczamy przesuniecie dla y1
	mov word ptr ds:[xzmienna], 1
	fild word ptr ds:[xzmienna]   ; Ładuje liczbę całkowitą z wskazanego miejsca pamięci , przekształca ją na rzeczywistą i wstawia na wierzchołek stosu zmiennoprzecinkowego
	mov word ptr ds:[xzmienna], 3	;odpowiada za przeuniecie obrazka na srodek 
	fild word ptr ds:[xzmienna]  ; Ładuje liczbę całkowitą z wskazanego miejsca pamięci , przekształca ją na rzeczywistą i wstawia na wierzchołek stosu zmiennoprzecinkowego
	
	mov cl, byte ptr ds:[liczbaiteracji]
	cmp cl, 1
	je obliczniepunktupoczatkowegojedynkay
			; czynnik 3^x przesuniecia
		mov ch, 0
		dec cl		; skoro korzystamy z loop to musimy odjac od cx 1 , w cx jest wartosc ilosci iteracji 
		wartoscy1:
			fmul st(1), st(0)  ;Mnoży wierzchołek stosu zmiennoprzecinkowego przez podany operand traktowany jako liczba rzeczywista i wynik zachowuje na wierzchołku stosu. (w rejestrze st0)
		loop wartoscy1
	
	
	obliczniepunktupoczatkowegojedynkay:
	
	fsqrt    ;Powoduje obliczenie pierwiastka kwadratowego z st0 i wynik zachowuje w st0.
	fmulp st(1), st(0)  ;Mnoży wierzchołek stosu zmiennoprzecinkowego przez podany operand traktowany jako liczba rzeczywista i wynik zachowuje w adresie podanym przez operand.
	fild word ptr ds:[dlugoscodcinka] ;Ładuje liczbę całkowitą z wskazanego miejsca pamięci 
	fmulp st(1), st(0) ;Mnoży wierzchołek stosu zmiennoprzecinkowego
	mov word ptr ds:[xzmienna], 2
	fild word ptr ds:[xzmienna] ;Ładuje liczbę całkowitą z wskazanego miejsca pamięci 
	fdivp st(1), st(0)   ;Dzieli stos przez operand i wynik składuje na stosie
	fistp word ptr ds:[xzmienna]  ; ładuje liczbę całkowitą z szczytu wierzchołka stosu do podanego miejsca w pamięci
	mov ax, word ptr ds:[xzmienna]
	add word ptr ds:[y1], ax		; dodajemy to co obliczylismy do zmiennej y1 
	
	;liczbymy teraz wartosc przesuniecia dla x1
	mov word ptr ds:[xzmienna], 3
	fild word ptr ds:[xzmienna] ;Ładuje liczbę całkowitą z wskazanego miejsca pamięci ;odpowiada za przesuneicie obrazka na srodek
	fild word ptr ds:[xzmienna] ;Ładuje liczbę całkowitą z wskazanego miejsca pamięci 
	
	mov cl, byte ptr ds:[liczbaiteracji]
	cmp cl, 1
	je obliczniepunktupoczatkowegojedynkaX
		
		mov ch, 0		
		dec cx	; w cx mamy liczbe iteracji odejmujemy jeden bo wykorzystujemy loop, w cx jest zapisana liczba iteracji
		wartoscx1:
			fmul st(1), st(0) ;w takim razie mnozymy tyle razy ile mamy iteracji
		loop wartoscx1
	
	
	obliczniepunktupoczatkowegojedynkaX:
		
	mov word ptr ds:[xzmienna], 6
	fild word ptr ds:[xzmienna]   ;Ładuje liczbę całkowitą z wskazanego miejsca pamięci 
	fdivp st(1), st(0)  ;Dzieli stos przez operand i wynik składuje na stosie
	fmulp st(1), st(0)	;Mnoży wierzchołek stosu zmiennoprzecinkowego	
	fild word ptr ds:[dlugoscodcinka] ;Ładuje liczbę całkowitą z wskazanego miejsca pamięci 
	fmulp st(1), st(0)   ;Mnoży wierzchołek stosu zmiennoprzecinkowego
	fistp word ptr ds:[xzmienna]  ;ładuje liczbę całkowitą z szczytu wierzchołka stosu do podanego miejsca w pamięci
	mov ax, word ptr ds:[xzmienna]
	add word ptr ds:[x1], ax		;przenosi obliczona wartosc do x1
	
	
	
	pop cx
	pop ax
	ret
 
;-------------------------------------------------------------------------------------------------------------------
;procedura rysujaca linie, parametry: x, y, x2, y2
rysowanielinii :
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov byte ptr ds:[kieruneky1], 0 ;zerujemy kieruneky1 ;zerujemy bo dopiero zaczynamy
	
	; obliczbie wartosci x
	mov ax, word ptr ds:[x2] ;przenosimy do ax wartosc x2
	sub ax, word ptr ds:[x1] ;teraz liczymy roznice miedzy poczatkiem a koncem
	mov word ptr ds:[kierunekx2], ax ; zapisujemy wynik do zmiennej	
	
	; obliczbie wartosci y
	mov ax, word ptr ds:[y2] ; przenosimy do ax wartosc y2
	sub ax, word ptr ds:[y1] ;	teraz liczymy roznice miedzy poczatkiem a koncem
	mov word ptr ds:[kierunekx1], ax ; zapisujemy wynik  do odpowiedniej zmiennnej
	
		
	mov bx, word ptr ds:[kierunekx1]	; kierunek x1
	cmp bx, 0
	jl odwrocznakx1				;skok jesli mniejsze (ze znakiem),ustawia zf=1 gdy wartosc odejmowania,roznicy jest ujemna
	jmp brakodwracaniax1
	
	odwrocznakx1:
		neg bx					; odwraca znak liczby zapisanej w operandzie
	
	brakodwracaniax1:
		mov ax, word ptr ds:[kierunekx2]	; kierunek x2
	cmp ax, 0
	jl odwrocznakx2  ;skok jesli mniejsze (ze znakiem), ustawia zf=1,gdy roznica jest ujemna
	jmp brakodwracaniax2
	
	odwrocznakx2:
	neg ax							;odwraca znak liczby zapisanej w operandzie
	
	
	brakodwracaniax2:
		
	cmp bx, ax			;musimy zamienic wartosci gdy bc>ax
	ja zamienwartosci ;skacze gdy wieksze 
	jmp idziemydalej
	
	zamienwartosci:	
		mov cx, word ptr ds:[kierunekx1];robimy swap czyli zamieniamy wartosci
		mov dx, word ptr ds:[kierunekx2]
		mov word ptr ds:[kierunekx2], cx
		mov word ptr ds:[kierunekx1], dx
		
		mov byte ptr ds:[kieruneky1], 1 ;ustawiamy flage na 1 
	
	
	
	; teraz będziemy obliczac kroki dla poszczegolnych przypadkow
	
	idziemydalej:
	
	
	mov ax, word ptr ds:[kierunekx2]
	cmp ax, 0
	jl idziemydalejkierunekujemny   ; skacze gdy zf=0 dla mniejszego (ze znakiem)
		
		mov word ptr ds:[kierunekruchu], 1 ;gdy wieksze od zero to ruch w prawo
		jmp idziemydalejkolejny
	
	idziemydalejkierunekujemny:	
		mov word ptr ds:[kierunekruchu], -1 ;gdy mniejsze od zera to ruch w lewo
		
	
	idziemydalejkolejny:
	
	;obliczamy wartosc kierunku
	mov ax, word ptr ds:[kierunekx1]
	cmp ax, 0; sprawdzamy czy jest zerowy
	je kierunekzerowyx
				
		fild word ptr ds:[kierunekx2] ;teraz st(0)=kierunekx2
		fild word ptr ds:[kierunekx1] ;teraz st(0)=kierunekx1
		fdivp st(1), st(0) ;a tutaj st(1)= ilorazowi tych dwoch zmiennych 1/0
		jmp idziemydalejkolejny2
	
	
	kierunekzerowyx:	
	fild word ptr ds:[kierunekx2] ;st(0)=kierunekx2	
		
	idziemydalejkolejny2:
	
	mov bx, 0 ; zerujemy licznik
	mov al, byte ptr ds:[kieruneky1]
	cmp al, 0; sprawdzamy czy jest zero
	je kierunekzerowyy
	
	;gdy nie jest zerem
	mov cx, 0;zerujemy licznik
	cmp word ptr ds:[kierunekx2], cx  ;gdy oba są rowne zero ,czyli nic nie ma
	je rysowanieliniikoniec  ;to konczymy
		
	rysowanieliniipetla:	
		mov word ptr ds:[xzmienna], cx	;w cx jest licznikiem krokow
		fild word ptr ds:[xzmienna]
		fdiv st(0), st(1)	; iloraz xzmiennej i st(1)-tutaj wspolczynnik kierunkowy
		;Pojedyncza precyzja. Liczby takie zajmują po 4 bajty i ich wartość maksymalna wynosi ok.  (10^39).
		fld dword ptr ds:[przyblizenie];dword - 4 bajty Powoduje załadowanie liczby rzeczywistej z adresu podanego argumentem do wierzchołka stosu zmiennoprzecinkowego
		fsubp st(1), st(0) ;roznica ,odejmowanie operandow
		fistp word ptr ds:[xzmienna] ;zaokraglamy i zapisujemy do xzmienna,ladowaie wartosci do zmiennej
		
		mov bx, word ptr ds:[y1]
		add bx, cx ;czyli w bx bedzie wartosc sumy cx i y1
		mov ax, word ptr ds:[xzmienna]
		add ax, word ptr ds:[x1] ;w ax bedzie xzmienna + x1		
		
		call rysujpunkt			; funkcja rysujaca punkt
		
		add cx, word ptr ds:[kierunekruchu] ;dodajemy wartosc licznika korkow do zmiennej 
		cmp cx, word ptr ds:[kierunekx2] ; i jezeli wartosci sa rowne 
		je rysowanieliniikoniec ;to konczymy a jezeli nie to petla
	jmp rysowanieliniipetla
	

	kierunekzerowyy:
	mov cx, 0
	cmp word ptr ds:[kierunekx2], cx  ; jezeli wartosci w zmiennej sa rowne zero to konczymy 
	je rysowanieliniikoniec
	
	rysowanieliniipetla2:
		mov word ptr ds:[xzmienna], cx  ; tak jak wyzej cx to licznik krokow
		fild word ptr ds:[xzmienna]  ;ladujemy zmiennna
		fdiv st(0), st(1) ; iloraz xzmiennej(st(0)) i wspólczynnnika kierunkowego
		
		fld dword ptr ds:[przyblizenie] ;ladujemy przyblizenie, dword to 4 bajty
		fsubp st(1), st(0)				;roznica
		fistp word ptr ds:[xzmienna]	;zaokraglamy stosujac zmienna przyblizenie i zapisujemy w xzmienna
		
		mov bx, word ptr ds:[y1]
		add bx, word ptr ds:[xzmienna] ;dodajemy
		mov ax, word ptr ds:[x1]  ;przenosimy wartosc x1 do ax
		add ax, cx ;i znowu dodajemy
		
		call rysujpunkt
		
		add cx, word ptr ds:[kierunekruchu] ;dodajemy
		cmp cx, word ptr ds:[kierunekx2] ;jezeli wartosci rowne to koniec a jezeli nie to petla
		je rysowanieliniikoniec
	jmp rysowanieliniipetla2

	rysowanieliniikoniec:
	fistp word ptr ds:[xzmienna]	
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret



rysujpunkt :
	push si
	push cx
	
	cmp ax, 319
	ja rysujpunktkoniec
	cmp ax, 0				;sprawdzamy czy punkt miesci sie w okienku 320x200
	jb rysujpunktkoniec		;jezeli nie to nie ma sensu rysowac punktu
	
	cmp bx, 199				; w ax i bx sa wspólrzedne punktu
	ja rysujpunktkoniec
	cmp bx, 0
	jb rysujpunktkoniec
	
	
	
	xchg ax, bx   ;swap wymienia wartosci miedzy soba 
	mov cx, 320
	mul cx						;funkcja mul to mnożenie wartosci ax razy operand 
	add ax, bx					; tutaj wspołrzedne y sa zapisane w bx wiec za pomoca xchg
								;wymieniamy wartosci, dodajemy do siebie wspólrzedne 
	mov si, ax					
	mov al, 1fh					;wyrzucamy obrazek ,tutaj na biało ale jeżeli chcemy kolorowy to np 2fh to zielony 3fh-rozowy 4fh-niebieski
	mov byte ptr es:[si], al		;przesyłamy wartosc kolory pod adres piksela,czyli wartosci odpowiednich punktow
	
	rysujpunktkoniec:
	pop cx
	pop si
	ret


tworzkroki :
	push di
	push si
	push cx
	push bx
	push ax
	
	; poczatkowy stan
	mov di, offset krok1				;tworzymy poczatkowy stan reprezentujacy krzywą
	mov byte ptr ds:[di], 'F'			; w pojedynczej iteracji kaazdy symbol F jest zastepowany jest "czescpoczatkowa"		
	inc di								; czyli ciagiem znaków zapisaniej w niej  
	mov byte ptr ds:[di], '+'		; zapisujemy poczatek do krok1
	inc di
	mov byte ptr ds:[di], '+'
	inc di
	mov byte ptr ds:[di], 'F'
	inc di
	mov byte ptr ds:[di], '+'
	inc di
	mov byte ptr ds:[di], '+'
	inc di
	mov byte ptr ds:[di], 'F'
	
	mov ch, 0
	mov cl, byte ptr ds:[liczbaiteracji]	; ilosc iteracji
	mov bl, 0	
	tworzkrokipetla:
		
		cmp bl, 0	;sprawdzamy czy jest zero czy nie
		je gdyparzysta
			mov di, offset krok1
			mov si, offset krok2
		jmp gdynieparzysta
		
		
		gdyparzysta:
			mov si, offset krok1
			mov di, offset krok2
		
		gdynieparzysta:
		
			tworzkrokipetla1:	;petla chodzi po znakach 
			cmp byte ptr ds:[si], 'F'; sprawdzamy czy to jest F
			jne tworzkrokipetlakolejnyznak	; gdy to nie jest F,to go przepisujemy 
				call gdytrafimynaF ; jezeli to jest F
				jmp dlaznakuF
			tworzkrokipetlakolejnyznak:	; robimy to gdy nie był to F zapisujemy go i idziemy na kolejny
				mov bh, byte ptr ds:[si]
				mov byte ptr ds:[di], bh
				inc di
		dlaznakuF:
		inc si
		cmp byte ptr ds:[si], 0		
		jne tworzkrokipetla1
		
		xor bl, 1	; zamienia 0 na 1 i 1 na 0
		
	loop tworzkrokipetla i powtarzamy operacje 
	
	
	cmp bl, 0  ;wybór sekwencji krokow zalezy od liczby iteracji l-systemu
	jne tworzkrokijeden
	;w zmiennej kroki zostanie zapisany koncowy ciag krokow
		mov word ptr ds:[kroki], offset krok1	;
		jmp tworzkrokikoniec
	
	
	tworzkrokijeden:
		mov word ptr ds:[kroki], offset krok2
	
	
	
	tworzkrokikoniec:
	
	pop ax
	pop bx
	pop cx
	pop si
	pop di
	ret


gdytrafimynaF :				; procedur zastepujaca F na ciag znaków 
	push ax
	push cx
	push si
	
	mov si, offset czescpoczatkowa					; ciag znaków zastepujacy pojedyncze F
	mov cx, 8	;dlugosc nowej czesci				; w di jest wskazany poczatek wykresu
	gdytrafimynaFpetla:
		mov al, byte ptr ds:[si]
		mov byte ptr ds:[di], al
		inc di
		inc si
	loop gdytrafimynaFpetla
	
	pop si
	pop cx
	pop ax
	ret



	brakargumentow:
		mov dx, offset errorbrakargumentow     ;gdy nie podamy żadnych argumentów
		call wykazbledow
		jmp zakonczprogram							;skok bezwarunkowy

	zlailoscargumentow:	
		mov dx, offset errorzmienne1			;gdy podamy złą ilość
		call wykazbledow
		jmp zakonczprogram
	cosjestnietak:
		mov dx, offset errorzmienne4
		call wykazbledow
		jmp zakonczprogram
		
	bladpierwszejzmiennej:
		mov dx, offset errorzmienne2			;gdy podamy złą ilość
		call wykazbledow
		jmp zakonczprogram
	
	bladdrugiejzmiennej:
		mov dx, offset errorzmienne3			;gdy podamy złą ilość
		call wykazbledow
		jmp zakonczprogram
;-------------------------------------------------------------------------------------------------------------------

parse:
	push si
	push di
	push es
	push cx

	
		
	mov	si, 82h					; ;82h oznacza pierwszy znak,bo 80h to liczba wisanych argumentow,81h to spacja
	mov di, offset liniapolecen		
	
	mov cx, 0			; zerowanie liczników
	mov ax, 0
	
	zapis:		
		mov ah, 0	
		
		outbialeznaki: 
			mov	al, es:[si]		
			inc si				
			cmp	al, 9d			; sprawdz czy tabulator
			je outbialeznakijest		
			cmp	al, ' '			; sprawdz czy spacja
			je outbialeznakijest		
			jmp outbialeznakinie	; nie trafil na bialy znak, nic juz nie pomijamy
			
			outbialeznakijest:	; gdy bialy znak
			inc ah		; dodajemy
			jmp outbialeznaki
		
			outbialeznakinie:	
			inc ch		
		
		cmp ah, 0d	 
		je kolejnyargument	; kolejny argument
		cmp al, 13	;sprawdzamy czy to jest koniec argumentow
		je kolejnyargument
		mov ah, 10
		mov	byte ptr ds:[di], ah	
		inc di			
		inc cl			
			
		kolejnyargument:	
		mov byte ptr ds:[di], al		
		inc di		

	cmp	al, 13	
	jne zapis	
	
	mov ah, ch	
	mov al, cl	
	inc al
	dec ah				;zmniejszamy o 1
	
	pop cx
	pop es
	pop di
	pop si	
	ret
		
;-------------------------------------------------------------------------------------------------------------------	
sprawdzanieargumentow:
	
	
	push dx
	push cx
	push bx
	push si
	push di
	
	mov cl, al		; 
	add cl, ah		; suma dlugosci argumentow
	mov ch, 0
	
	mov si, offset liniapolecen	
	;-----------------------sprawdzamy ilosc argumentow
	cmp	al, 2		;sprawdza czy są podane dwa argumenty
	jne zlailoscargumentow
	
	
	; sprawdzamy czy pierwsza zmienna miesci się miedzy 1-4
	mov bl, byte ptr ds:[si]
	cmp bl, '1'
	jb bladpierwszejzmiennej
	cmp bl, '4'
	ja bladpierwszejzmiennej
	
	sub bl, 48						;zapisujemy wartosc liczbowa pierwszej wpsianej zmiennej 
	mov byte ptr ds:[liczbaiteracji], bl
	
	inc si	;przesuwamy sie na nastepny
	dec cl	;odejmujemy jeden
	
	mov bl, byte ptr ds:[si]	; nastepny powinien byc odstep
	
	
	inc si	;przesuwamy sie na nastepny
	sub cl, 2

	;teraz sprawdzamy drugi argument
	;czy jest liczba
	mov ch, 0	; w cl ilosc pozostalych znakow
	mov dx, 0
	mov bh, 0
	sprawdzanieargumentowdrugi:
		mov bl, byte ptr ds:[si]
		cmp bl, '0'		
		jb bladdrugiejzmiennej
		cmp bl, '9'	;
		ja bladdrugiejzmiennej
		
		push ax
		push cx
			mov ax, dx
			mov cl, 10
			mul cl     ;mnozy wartosc rejestru cl razy al
			mov dx, ax
			mov bh, 0
			sub bl, 48
			add dx, bx
		pop cx
		pop ax
		
		inc si	;chodzimy po linii komend
	loop sprawdzanieargumentowdrugi
	
	mov word ptr ds:[dlugoscodcinka], dx  ; zapisz wartosc drugiego argumentu do zmiennej
	
	pop di
	pop si
	pop bx
	pop cx
	pop dx
	ret
		
	
	
	

zakonczprogram:
		mov ax, 1h	
		int	16h                           ; nie może być zwykłego zamknięcia programu
		mov ax,04c00h						; gdyż szybciej zamyka program niż zdąrzy coś wyświetlić	
		int 21h							; dlatego dopisałam aby oczekiwał na klawisz przed zamknięciem
	
	


wykazbledow:					
	pushf			;wyrzuca tekst błedu 
	push ax
	mov	ah, 9			
	int	21h
	pop ax
	popf
	ret
	
	
;-------------------------------------------------------------------------------------------------------------------
code1 ends
;segment stosu
stos1	segment stack
	dw	200 dup(?)	
	wstosu	dw	?		
stos1	ends

end start
