[org 0x100]
jmp start

	game_n: db ' ROAD FIGHTER '
	game_len: dw 14

	ending_script: db ' Do you want to PLAY AGAIN ? (Y / N) '
	len_str: dw 37

	partners: db ' Developed by Team M_H (Musa & Hussain) '
	partner_len: dw 40

	script1: db ' Press Y to PLAY AGAIN & Press N to QUIT '
	len_script1: dw 41

	note: db ' If "Player" reached Score limit " 700 " then he will be considered a WINNER '
	note_len: dw 77

	play: db ' Press "P" to PLAY GAME '
	play_len: dw 24

	playgame: db ' Use Left and Right Arrow Keys to Play Game '
	playgame_len: dw 44

	escape_: db ' Press "ESCAPE" to QUIT '
	escape_len: dw 24

	sccore: db ' Score : '
	sccore_len: dw 9

	Fuel: db ' FUEL : '
	Fuel_len: dw 8

	win: dw 700

	scores: dw 0;A green
	fuels: dw 974
	random: dd 01010101101010101010101010101010b,101010101010101010101010101101b;random number
	rando: dd 10101010101010101010101010101010b,101010101010101010101010101010b
	rand: dw 0;random number
    ran: dw 0;random number
    move: dw 3436
	bool: dw 0

	_path: dw 0
	trees: dw 0
	_curve: dw 0

	collision1: times 3 dw 0
	collision2: times 3 dw 0
	collision3: times 3 dw 0
	collision4: times 3 dw 0
	
	boolcar1: dw 0
	boolcar2: dw 0
	boolcar3: dw 0
	boolcar4: dw 0

	roadmap: times 50 dw 0
	
	right: dw 20
	bool_curve: dw 0

	oldisr: dd 0 
; space for saving old isr keyboard interrupt service routine

;subroutine to clear the screen
clrscr: 

	push es
	push ax
	push di
		
	mov ax, 0xb800
	mov es, ax 
	mov di, 0 
		
	nextloc: 
		
		mov word [es:di], 0x0720 
		add di, 2 
	
	cmp di, 4000 
	jne nextloc 
	
	pop di
	pop ax
	pop es

ret 

;subroutine to delay 4 in the program
delay:

	push cx
	mov cx, 4 ; change the values  to increase delay time

	delay_loop1:
	
	push cx
	mov cx, 0xFFFF

	delay_loop2:

	loop delay_loop2

	pop cx
	loop delay_loop1

	pop cx
ret

;subroutine to delay 20 in the program
_delay_:

	push cx
	mov cx, 20 ; change the values  to increase delay time

	_delay_loop1:
	
	push cx
	mov cx, 0xFFFF

	_delay_loop2:

	loop _delay_loop2

	pop cx
	loop _delay_loop1

	pop cx
ret

;subroutine to delay 1 in the program
delay_:
	
	push cx

	mov cx, 1 ; change the values  to increase delay time

	delay_loop1_:
	
	push cx
	mov cx, 0xFFFF

	delay_loop2_:

	loop delay_loop2_

	pop cx
	loop delay_loop1_

	pop cx
	
ret

;subroutine for setting all the variables to reset to play game again
System_Renew:

	mov word[scores],0

	mov word[trees],0

	mov word[_path],0

	mov word[_curve],0

	mov word[fuels],974

	mov dword[random],01010101101010101010101010101010b
	mov dword[random+2],101010101010101010101010101101b
	
	mov dword[rando],10101010101010101010101010101010b
	mov dword[rando+2],101010101010101010101010101010b

	mov word[rand],0

	mov word[ran],0

	mov word[move],3436

	mov word[bool],0

	mov word[collision1],0
	mov word[collision1+2],0
	mov word[collision1+4],0

	mov word[collision2],0
	mov word[collision2+2],0
	mov word[collision2+4],0

	mov word[collision3],0
	mov word[collision3+2],0
	mov word[collision3+4],0

	mov word[collision4],0
	mov word[collision4+2],0
	mov word[collision4+4],0
	
	mov word[boolcar1],0

	mov word[boolcar2],0

	mov word[boolcar3],0

	mov word[boolcar4],0

	mov dword[oldisr],0 

	mov word[win],700

	mov word[right],20

	mov word[bool_curve],0

ret

;subroutine to show rotation
rotation_d:
	
	push ds
	push si
	push di
	push ax
	push cx
	
    std
	
	mov si,3840
	mov di,4000
	
	sub di,40
	sub si,40
	
	mov ax,0xb800
	
	mov es,ax
	mov ds,ax
	
    mov ax,0

	;loop for rotating screen from row 1 to row 24
	row:
	
		mov cx,51
		std
		
		rep movsw
		inc ax
		
		sub di,58
		sub si,58
		
	cmp ax,24
	jne row

	mov cx,23
	mov si,3886

	;end subroutine for removing
	call rotate_remove

	mov si,3858
	
	mov di,18
	
	mov cx,51
	
	cld

	;string operation for moving last row to 1st row	
	rep movsw 
	
	
	pop cx
	pop ax
	pop di
	pop si
	pop ds

ret

;subroutine to tilt the road towards the right
curve_rightpath:
	
	push cx
	push ax
	push si
	push bx

	cmp word[bool_curve],1		;check whether the road is curved or not
	je ennd

	cmp word[right],42
	jge ennd

	mov ax,0xb800
	mov es,ax

	mov si,[right]

	mov bx,0
	; mov ax,[es:118]
	; mov word[rio],ax
	; mov ax,[es:116]
	; mov word[rio+2],ax

	mov cx,50

	loo:

		mov ax,[roadmap+bx]
		mov [es:si],ax

		call main_car

		add bx,2
		add si,2

	loop loo

	mov word[es:120],0x0000
	mov word[es:122],0x0000
	mov word[es:124],0x0000
	mov word[es:126],0x0000
	mov word[es:128],0x0000

	call delay
	
	add word[right],2

	mov word[trees],3
	mov word[_path],3


	jmp bol_end

	bool_c:

		mov word[bool_curve],1
		jmp bol_end

	ennd:

	cmp word[right],42
	jge bool_c

	bol_end:

	pop bx
	pop si
	pop ax
	pop cx

ret

;subroutine to tilt the road towards the left
curve_leftpath:

	push cx
	push ax
	push si
	push bx

	cmp word[bool_curve],0		;check whether the road is curved or not
	je ennd2

	cmp word[right],20
	jle ennd2

	mov ax,0xb800
	mov es,ax

	mov si,[right]

	mov bx,0
	; mov ax,[es:118]
	; mov word[rio],ax
	; mov ax,[es:116]
	; mov word[rio+2],ax

	mov cx,50

	loo_:

		mov ax,[roadmap+bx]
		mov [es:si],ax

			call main_car

		add bx,2
		add si,2

	loop loo_

	mov word[es:120],0x0000
	mov word[es:122],0x0000
	mov word[es:124],0x0000
	mov word[es:126],0x0000
	mov word[es:128],0x0000

	call delay
	
	sub word[right],2

	mov word[trees],3
	mov word[_path],3


	jmp bol_end2

	bool_c2:
		mov word[bool_curve],1
		jmp bol_end2

	ennd2:

	cmp word[right],20
	jle bool_c

	bol_end2:

	pop bx
	pop si
	pop ax
	pop cx

ret

;general subroutine to play sound by giving frequency and notes
_sound:

	push bp
	mov bp,sp

    mov     al, 182
    out     0x43, al
	mov     cx,[bp+4]
    mov     ax,cx
	mov 	bx,[bp+6]

    out     0x42, al
    mov     al, ah
    out     0x42, al
    in      al, 0x61

    or      al, 00000011b
    out     0x61, al

    pause1:
     
	    mov cx, 65535

    pause2:

        dec cx
        jne pause2
        dec bx
        jne pause1

        in  al, 0x61
        and al, 11111100b
        out 0x61, al

		pop bp

ret 4

;general subroutine to play sound by giving notes
sound:

	push bp
	mov bp,sp

	mov     al, 182	    	; Prepare the speaker for the
	out     43h, al 	   	;  note.
    mov     ax, [bp+4]  	; Frequency number (in decimal)
                            ;  for middle C.
    out     42h, al         ; Output low byte.
    mov     al, ah          ; Output high byte.
    out     42h, al 
    in      al, 61h         ; Turn on note (get value from
                            ;  port 61h).
    or      al, 00000011b   ; Set bits 1 and 0.
    out     61h, al         ; Send new value.
    mov     bx, 25          ; Pause for duration of note.
	
	_pause1:
        
		mov     cx, 65535
	
	_pause2:
        
		dec     cx
        jne     _pause2
        dec     bx
        jne     _pause1
       
	    in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.

	pop bp

ret 2

;subroutine to play sound at the end of game
EndingGameSound:

    push 7
	push 3333
	call _sound
    
    push 7
	push 4444
	call _sound
    
    push 7
	push 5555
	call _sound
    
    push 7
	push 6666
	call _sound

    push 7
	push 7777
	call _sound

    push 7
	push 8888
	call _sound

    push 7
	push 7742  
    call _sound
    
    push 7
	push 9876
	call _sound

ret

;subroutine to play sound at the start of game
Starttng:

    push 4
	push 6670
	call _sound

    push 3
	push 7742  
    call _sound

    push 3
	push 8888
	call _sound
    push 4
	push 7670
	call _sound
    

    push 5
	push 6666
	call _sound
    
    push 4
	push 5555
	call _sound
    
    push 5
	push 4574
	call _sound

ret

;subroutine to play sound during the game
during:

    push 3
    push 8234;7878
    call _sound

    push 2
    push 9898
    call _sound

ret

;subroutine to store path in roadmap variable
storing_path:

	push cx
	push ax
	push si
	push bx

	mov cx,50
	mov si,18
	mov bx,0

	looper:

		mov ax,[es:si]
		mov word[roadmap+bx],ax

		add si,2
		add bx,2

	loop looper

	pop bx
	pop si
	pop ax
	pop cx

ret

;subroutine to remove objects from 25th row
rotate_remove:

	push cx
	push si
	push bx
	push ax

	mov ax,0xb800
	mov es,ax

	mov si,3958
	;sub si,26;------------------------------------------

	mov cx,12
	
	mov bl,0xDB
	mov bh,42

	road_g2:
	
		mov word[es:si],bx
		sub si,2
	
	loop road_g2
	
	;constructing dots
	mov word[es:si],0x212E
	sub si,2
	
	;constructing white line
	mov word[es:si],0x0FC7
	
	sub si,2
		
	mov cx,23
	
	mov bl,0xDB
	mov bh,0x37
	
	;constructing road
	road_:
	
		mov word[es:si],bx
		sub si,2
	
	loop road_
	
	;constructing white line
	mov word[es:si],0x0FC7
	sub si,2
	
	;constructing dots
	mov word[es:si],0x212E
	sub si,2
	;sub si,24;------------------------------------

	mov cx,11
	
	mov bl,0xDB
	mov bh,42

	road_g1:
	
		mov word[es:si],bx
		sub si,2
	
	loop road_g1
	
	;constructing side black road
	mov cx,2
	right_side_black_road_:
	
		mov word[es:si],0x0000
		sub si,2
	
	loop right_side_black_road_
	
	
	;constructing first line
	mov bl,0xB3
	mov bh,07
	mov word[es:si],bx
	sub si,2
	
	;constructing side path small road
	mov cx,4
	small_road_:
	
		mov word[es:si],0x2FDB
		sub si,2
	
	loop small_road_
	
	;constructing second line
	mov bl,0xB3
	mov bh,07
	mov word[es:si],bx
	sub si,2
	
	;constructing side black road
	mov cx,2
	side_black_road_:
	
		mov word[es:si],0x0000
		sub si,2
	
	loop side_black_road_

	pop ax
	pop bx
	pop si 
	pop cx

ret

;creating module 1 for figure 2
creating_Module:
	
	mov ax,0xb800
	mov es,ax
	
	mov di,0
	
	mov dl,0xdb		;background color
	mov dh,42     ;15 white  ;42 light green
	
	mov si,0
	mov ax,0
	mov cx,60
	
	;constructing background color
	background:
	
		mov word[es:di],dx			;loop for rows
		add di,2
	
	loop background
	
	inc ax
	;
	
	mov si,di
	sub si,26
	
	;constructing dots
	mov word[es:si],0x212E
	sub si,2
	
	;constructing white line
	mov word[es:si],0x0FC7
	
	sub si,2
		
	mov cx,23
	
	mov bl,0xDB
	mov bh,0x37
	
	;constructing road
	road:
	
		mov word[es:si],bx
		sub si,2
	
	loop road
	
	;constructing white line
	mov word[es:si],0x0FC7
	sub si,2
	
	;constructing dots
	mov word[es:si],0x212E
	sub si,24
	
	;constructing side black road
	mov cx,2
	right_side_black_road:
	
		mov word[es:si],0x0000
		sub si,2
	
	loop right_side_black_road
	
	
	;constructing first line
	mov bl,0xB3
	mov bh,07
	mov word[es:si],bx
	sub si,2
	
	;constructing side path small road
	mov cx,4
	small_road:
	
		mov word[es:si],0x2FDB
		sub si,2
	
	loop small_road
	
	;constructing second line
	mov bl,0xB3
	mov bh,07
	mov word[es:si],bx
	sub si,2
	
	;constructing side black road
	mov cx,2
	side_black_road:
	
		mov word[es:si],0x0000
		sub si,2
	
	loop side_black_road
	
	;
	
	cmp ax,24
	je return
	
	mov cx,60
	add di,40
	
	jmp background
	
	return:
	
	;printing score
	call score
	
	;printing fuel
	call fuel

	;print road mid track
	call road_track
	
	;printing left side of trees
	call trees1
	
	;printing right side of trees
	call trees2
	
	;blinking variable
	mov word[es:3689],0x02db
	
ret

;subroutine to print number on the screen
printnumber:
	
	push bp
    mov bp, sp

    push es
    push ax
    push bx
    push cx
    push dx
    push di
    
	mov ax, 0xb800
    mov es, ax 				; point es to video base
    mov ax, [bp+4] 			; load number in ax
    mov bx, 10 				; use base 10 for division
    mov cx, 0 				; initialize count of digits

    nextdigit1: 
		
		mov dx, 0 			; zero upper half of dividend
    	div bx 				; divide by 10
    	add dl, 0x30 		; convert digit into ascii value
    	push dx 			; save ascii value on stack
    	inc cx 				; increment count of values
    	cmp ax, 0 			; is the quotient zero
    
	jnz nextdigit1 			; if no divide it again
    
	mov di, [bp+6] 			; point di to 70th column
    
	nextpos1: 
	
		pop dx 				; remove a digit from the stack
    	mov dh, 0xB0 		; use normal attribute
    	mov [es:di], dx 	; print char on screen
    	add di, 2 			; move to next screen location
    
	loop nextpos1 			; repeat for all digits on stack
    
	pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    
	pop bp

ret 4

;subroutine for display fuel capacity on screen
fuel_counter:

	sub word[fuels],1

	push 2380;42;400
    push word [fuels]
    call printnumberf

ret

;general subroutine to print number on certain screen colors
printnumberf: 
	
	push bp
    mov bp, sp
    
	push es
    push ax
    push bx
    push cx
    push dx
    push di
    
	mov ax, 0xb800
    mov es, ax 				; point es to video base
    mov ax, [bp+4] 			; load number in ax
    mov bx, 10 				; use base 10 for division
    mov cx, 0 				; initialize count of digits

    nextdigit1_: 
	
		mov dx, 0 			; zero upper half of dividend
    	div bx 				; divide by 10
    	add dl, 0x30	 	; convert digit into ascii value
    	push dx 			; save ascii value on stack
    	inc cx 				; increment count of values
    	cmp ax, 0 			; is the quotient zero
    
	jnz nextdigit1_ 		; if no divide it again
    
	mov di, [bp+6] 			; point di to 70th column
    
	nextpos1_: 
		
		pop dx 				; remove a digit from the stack
    	mov dh, 0x0C 		; use normal attribute
    	mov [es:di], dx 	; print char on screen
    	add di, 2 			; move to next screen location
    
	loop nextpos1_ 			; repeat for all digits on stack
   
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    
	pop bp

ret 4

;subrotine to design cars
cars:
	
	push di

	;checking first random variable for two cars
	inc word[rand]
	cmp word[rand],35
	je secret1
	jmp ret_1

	;loop to create car 1
	car1:

		;car3

		mov di,72
	
		mov word[es:di],0x76D4
		add di,2
		mov word[es:di],0x74D2
		add di,2
		mov word[es:di],0x76BE
		add di,156
		mov word[es:di],0x74C6
		add di,2
		mov word[es:di],0x70CE
		add di,2
		mov word[es:di],0x74B5
		add di,156
		mov word[es:di],0x71CD;D4
		mov word[collision3],di

		add di,2
		mov word[es:di],0x71CA
		mov word[collision3+2],di

		add di,2
		mov word[es:di],0x71CD;BE
		mov word[collision3+4],di

		mov word[boolcar3],1
		
		jmp car_ret1

		;loop to change rand 1st variable to defaults
		secret1:
		
		mov word[rand],10

		shr dword[random],1
		rcr dword[random+2],1
		jc car1
		jmp car2
		
	;loop to create car 2
	car2:

		;car1

		mov di,48

		mov word[es:di],0x76D4
		add di,2
		mov word[es:di],0x74D2
		add di,2
		mov word[es:di],0x76BE
		add di,156
		mov word[es:di],0x74C6
		add di,2
		mov word[es:di],0x70CE
		add di,2
		mov word[es:di],0x74B5
		add di,156
		mov word[es:di],0x71CD;D4
		mov word[collision1],di

		add di,2
		mov word[es:di],0x71CA
		mov word[collision1+2],di

		add di,2
		mov word[es:di],0x71CD;BE
		mov word[collision1+4],di

		mov word[boolcar1],1

		car_ret1:
		ret_1:

		;checking other random variale for other two cars
		inc word[ran]
		cmp word[ran],23
		je secret2
		jmp ret_2

	;loop to create car 3
	car3:

		;car4

		mov di,84
	
		mov word[es:di],0x76D4
		add di,2
		mov word[es:di],0x74D2
		add di,2
		mov word[es:di],0x76BE
		add di,156
		mov word[es:di],0x74C6
		add di,2
		mov word[es:di],0x70CE
		add di,2
		mov word[es:di],0x74B5
		add di,156
		mov word[es:di],0x71CD;D4
		mov word[collision4],di

		add di,2
		mov word[es:di],0x71CA
		mov word[collision4+2],di

		add di,2
		mov word[es:di],0x71CD;BE
		mov word[collision4+4],di

		mov word[boolcar4],1

		jmp car_ret2

		;loop to change rand other variable to defaults
		secret2:
		
		mov word[ran],0

		shr dword[rando],1
		rcr dword[rando+2],1
		jc car3
		jmp car4
	
	;loop to create car 4
	car4:

		;car2

		mov di,60
	
		mov word[es:di],0x76D4
		add di,2
		mov word[es:di],0x74D2
		add di,2
		mov word[es:di],0x76BE
		add di,156
		mov word[es:di],0x74C6
		add di,2
		mov word[es:di],0x70CE
		add di,2
		mov word[es:di],0x74B5
		add di,156
		mov word[es:di],0x71CD;D4
		mov word[collision2],di

		add di,2
		mov word[es:di],0x71CA
		mov word[collision2+2],di

		add di,2
		mov word[es:di],0x71CD;BE
		mov word[collision2+4],di

		mov word[boolcar2],1

		car_ret2:
		ret_2:

	pop di
	
ret

;subroutine to print fuel string on screen
fuel:

	;mov si,2044
	;printing string on screen
	mov ax, 2204
	push ax ; push y position
	mov ax,0xF ; blue on black attribute
	push ax ; push attribute
	mov ax, Fuel
	push ax ; push address of message
	push word [Fuel_len] ; push message length
	
	call printstr

ret

;handles score less than 10 in scores counter subroutine
less_10:
	
	mov dl,48
	add dl,[scores]
	
	mov dh,15
	
	mov si,1900
	mov ax,0xb800
	
	mov es,ax
	
	mov word[es:si],dx
	
jmp ret1

;handles score less than 100 in scores counter subroutine
less_100:
	
	mov ax,0xb800
	
	mov es,ax
	
	mov ax,0
	
	mov al,[scores]
	
	mov bl,10
	
	div bl
	
	mov dl,48
	add dl,al
	
	mov dh,15
	
	mov si,1900
	
	mov word[es:si],dx
	
	add si,2
	
	mov dl,48
	
	add dl,ah
	
	mov word[es:si],dx

jmp ret2

;handles score less than 1000 in scores counter subroutine
less_1000:

	mov ax,0xb800
	
	mov es,ax
	
	mov ax,0
	
	mov ax,[scores]
	
	mov bx,10
	
	mov dx,0
	
	div bx
	
	mov cx,dx
	
	mov si,1900
	
	div bl
	
	mov dl,48
	add dl,al
	
	mov dh,15
	
	mov word[es:si],dx
	
	add si,2
	
	mov dl,48
	
	add dl,ah
	
	mov dh,15
	
	mov word[es:si],dx

	mov dl,48
	add dl,cl
	
	mov dh,15
	
	add si,2
	
	mov word[es:si],dx

jmp ret3

;handles score less than 10000 in scores counter subroutine
less_10000:

	mov ax,0xb800
	
	mov es,ax
	
	mov ax,0
	
	mov ax,[scores]
	
	mov bx,10
	
	mov dx,0
	
	div bx
	
	mov cl,dl
	
	mov dx,0
	
	div bx
	
	mov ch,dl
	
	mov dx,0
	
	div bl
	
	mov si,1900
	
	mov dl,48
	add dl,al
	
	mov dh,15
	
	mov word[es:si],dx
	
	add si,2
	
	mov dl,48
	
	add dl,ah
	
	mov dh,15
	
	mov word[es:si],dx

	mov dl,48
	add dl,ch
	
	mov dh,15
	
	add si,2
	
	mov word[es:si],dx
	
	mov dl,48
	add dl,cl
	
	mov dh,15
	
	add si,2
	
	mov word[es:si],dx

jmp ret4

;subrotine to increase the above four operation of scores
scores_counter:

	push ax
	push bx
	push cx
	push dx

	inc word[scores]

	cmp word[scores],9			;score check between 0 and 9
	jl less_10;
	
	cmp word[scores],100		;score check between 10 and 100
	jl less_100
	
	cmp word[scores],1000		;score check between 100 and 1000
	jl less_1000
	
	cmp word[scores],10000		;score check between 1000 and 10,000
	jl less_10000
	
	ret1:
	ret2:
	ret3:
	ret4:
	
	pop dx
	pop cx
	pop bx
	pop ax
	
ret

;design trees of left side of cars
trees1:
	
	push dx
	push ax
	push cx
	push di
	push si
	
	mov dh,32
	mov dl,0xA9
	
	mov ax,0xb800
	mov es,ax
	
	mov di,504;664
	
	mov si,672;832
	
	;counter for tree,s
	mov cx,4
	
	;loop for different shape ascii's and colours for tree
	t:
	
		mov [es:di],dx
		mov dl,0xC4
		add di,2
	
		mov [es:di],dx
		mov dl,0xAA
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,156
	
		mov [es:di],dx
		mov dl,0xB2
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,156
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xA9
	
		add di,476
	
	loop t
	
	;counter for tree's shadow
	mov cx,4
	
	mov dh,0x12
	mov dl,0xCB
	
	;loop for different shape ascii's and colours for tree's shadow
	t1:
	
		mov [es:si],dx
		mov dl,0xC4
		add di,2
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		mov dl,0xB0
		add di,156
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,156
		mov dl,0xCB
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xA9
	
		add si,638
	
	loop t1
	
	pop si
	pop di
	pop cx
	pop ax
	pop dx

ret

;design trees of right side of cars
trees2:
	
	push dx
	push ax
	push cx
	push di
	push si
	
	mov dh,32
	mov dl,0xA9
	
	mov ax,0xb800
	mov es,ax
	
	mov di,580;740
	
	mov si,588;748
	
	;counter for trees
	mov cx,4
	
	;loop for different shape ascii's and colours for tree
	tr:
	
		mov [es:di],dx
		mov dl,0xC4
		add di,2
	
		mov [es:di],dx
		mov dl,0xAA
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,156
	
		mov [es:di],dx
		mov dl,0xB2
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,156
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xA9
	
		add di,476
	
	loop tr
	
	;counter for tree,s shadow 
	mov cx,4
	
	mov dh,0x12
	mov dl,0xCB
	
	;loop for different shape ascii's and colours for tree's shadow
	tre:
	
		mov [es:si],dx
		mov dl,0xC4
		add di,2
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		mov dl,0xB0
		add di,156
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,156
		mov dl,0xCB
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xA9
	
		add si,638
	
	loop tre
	
	pop si
	pop di
	pop cx
	pop ax
	pop dx

ret

;subroutine to be called in tree rotation to perform rotation functionality
t_rotate:

	mov dh,32
	mov dl,0xA9
	
	mov ax,0xb800
	mov es,ax
	
	mov di,24;664
	
	mov si,192;832
	
	;for different shape ascii's and colours for tree
	
		mov [es:di],dx
		mov dl,0xC4
		add di,2
	
		mov [es:di],dx
		mov dl,0xAA
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,156
	
		mov [es:di],dx
		mov dl,0xB2
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,156
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xA9
	
	mov dh,0x12
	mov dl,0xCB
	
	;for different shape ascii's and colours for tree's shadow
	
		mov [es:si],dx
		mov dl,0xC4
		add di,2
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		mov dl,0xB0
		add di,156
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,156
		mov dl,0xCB
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xA9


		mov dh,32
	mov dl,0xA9
	
	mov ax,0xb800
	mov es,ax
	
	mov di,100;740
	
	mov si,108;748
		
	;for different shape ascii's and colours for tree
	
		mov [es:di],dx
		mov dl,0xC4
		add di,2
	
		mov [es:di],dx
		mov dl,0xAA
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,156
	
		mov [es:di],dx
		mov dl,0xB2
		add di,2
	
		mov [es:di],dx
		mov dl,0xB0
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,156
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xEC
		add di,2
	
		mov [es:di],dx
		mov dl,0xA9
	
	mov dh,0x12
	mov dl,0xCB
	
	;for different shape ascii's and colours for tree's shadow
	
		mov [es:si],dx
		mov dl,0xC4
		add di,2
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		mov dl,0xB0
		add di,156
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,2
		mov dl,0xB1
	
		mov [es:si],dx
		add si,156
		mov dl,0xCB
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xEC
		add di,2
	
		mov [es:si],dx
		mov dl,0xA9


ret

;subroutine to print trees during rotation
tree_rotation:



	push dx
	push ax
	push cx
	push di
	push si

	inc word[trees]

	cmp word[trees],7		;check when to print tree and shadow
	jne tree_end

	
	call t_rotate
	
	mov word[trees],0
	
	tree_end:

	pop si
	pop di
	pop cx
	pop ax
	pop dx

ret

;subrotine to design the mid track of mid path of road
road_track:

	push si
	push ax
	push di
	push dx
	push cx
	
	mov dh,0x7F
	mov dl,0xDE
	
	mov ax,0xb800
	mov es,ax
	
	mov word[es:388],dx
	mov word[es:548],dx
	
	mov di,1188
	
	mov cx,4
	
	path:
	
	mov [es:di],dx
	add di,160
	mov [es:di],dx

	add di,640

	loop path
	
	pop cx
	pop dx
	pop di
	pop ax
	pop si
	
ret

;subroutine to print score on certain screen coordinates
score:
	
	push si
	
	mov si,1724
	
	push dx
	
	mov dh,15
	mov dl,0x53
	
	mov word[es:si],dx
	add si,2
	mov dl,0x43
	
	mov word[es:si],dx
	add si,2
	mov dl,0x4F
	
	mov word[es:si],dx
	add si,2
	mov dl,0x52
	
	mov word[es:si],dx
	add si,2
	mov dl,0x45
	
	mov word[es:si],dx
	add si,2
	mov dl,0x3A
	
	mov word[es:si],dx
	
	pop dx
	pop si
	
ret

;subroutine for creating main car
main_car:

    push di

    mov di,[move]
	
		mov word[es:di],0x79D4
		add di,2
		mov word[es:di],0x79D2
		add di,2
		mov word[es:di],0x79BE
		add di,156
		mov word[es:di],0x79C6
		add di,2
		mov word[es:di],0x79CE
		add di,2
		mov word[es:di],0x79B5
		add di,156
		mov word[es:di],0x79CD;D4
		add di,2
		mov word[es:di],0x79CA
		add di,2
		mov word[es:di],0x79CD;

    pop di

ret

;subroutine to display car at the start of game
CAR:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	
	here1:
		mov word [es: di], 0x0fdb
		add di, 2
		cmp di, 4000
	jne here1
		
	
	; Green
	mov ax, 0x02db
		
		mov cx, 15
		mov di, 0
			Greens:
				mov word [es: di + (80*3+25)*2], ax
				mov word [es: di + (80*3+40)*2], ax
				mov word [es: di + (80*6+25)*2], ax
				mov word [es: di + (80*6+40)*2], ax
				mov word [es: di + (80*13+10)*2], ax
				mov word [es: di + (80*13+54)*2], ax
				mov word [es: di + (80*17+7)*2], ax
				mov word [es: di + (80*17+22)*2], ax
				mov word [es: di + (80*17+37)*2], ax
				mov word [es: di + (80*17+52)*2], ax
				mov word [es: di + (80*18+16)*2], ax
				mov word [es: di + (80*18+31)*2], ax
				mov word [es: di + (80*18+46)*2], ax
				add di, 2
			loop Greens
		
		mov cx, 5
		mov di, 0
			Green:
				mov word [es: di + (80*7+26)*2], ax
				mov word [es: di + (80*7+60)*2], ax
				mov word [es: di + (80*8+13)*2], ax
				mov word [es: di + (80*11+63)*2], ax
				mov word [es: di + (80*12+64)*2], ax
				add di, 2
			loop Green
		
		; Brwon
		mov ax, 0x0adb
		
		mov cx, 10
		mov di, 0
			Browns1:
				mov word [es: di + (80*4+25)*2], ax
				mov word [es: di + (80*4+35)*2], ax
				mov word [es: di + (80*4+45)*2], ax
				mov word [es: di + (80*5+25)*2], ax
				mov word [es: di + (80*5+35)*2], ax
				mov word [es: di + (80*5+45)*2], ax
				mov word [es: di + (80*9+8)*2], ax
				mov word [es: di + (80*10+8)*2], ax
				mov word [es: di + (80*11+8)*2], ax
				mov word [es: di + (80*12+6)*2], ax
				mov word [es: di + (80*12+16)*2], ax
				mov word [es: di + (80*12+60)*2], ax
				mov word [es: di + (80*14+20)*2], ax
				mov word [es: di + (80*14+30)*2], ax
				mov word [es: di + (80*14+40)*2], ax
				mov word [es: di + (80*14+50)*2], ax
				mov word [es: di + (80*14+60)*2], ax
				add di, 2
			loop Browns1
		
		mov cx, 20
		mov di, 0
			Brwons2:
				mov word [es: di + (80*15+10)*2], ax
				mov word [es: di + (80*15+30)*2], ax
				mov word [es: di + (80*15+50)*2], ax
				mov word [es: di + (80*16+8)*2], ax
				mov word [es: di + (80*16+28)*2], ax
				mov word [es: di + (80*16+48)*2], ax
				add di, 2
			loop Brwons2
		
		mov cx, 5
		mov di, 0
			Browns:
				mov word [es: di + (80*6+26)*2], ax
				mov word [es: di + (80*8+61)*2], ax
				mov word [es: di + (80*9+62)*2], ax
				mov word [es: di + (80*10+63)*2], ax
				mov word [es: di + (80*13+20)*2], ax
				mov word [es: di + (80*14+8)*2], ax
				mov word [es: di + (80*15+6)*2], ax
				add di, 2
			loop Browns
		
		; Black
		mov ax, 0x0720
		
		mov cx, 10
		mov di, 0
			Black10:
				mov word [es: di + (80*2+30)*2], ax
				mov word [es: di + (80*7+43)*2], ax
				mov word [es: di + (80*12+58)*2], ax
				mov word [es: di + (80*13+25)*2], ax
				mov word [es: di + (80*13+42)*2], ax
				mov word [es: di + (80*14+9)*2], ax
				mov word [es: di + (80*19+20)*2], ax
				mov word [es: di + (80*19+30)*2], ax
				mov word [es: di + (80*19+40)*2], ax
				add di, 2
			loop Black10
			
		mov cx, 7
		mov di, 0
			Black7:
				mov word [es: di + (80*7+31)*2], ax
				mov word [es: di + (80*12+19)*2], ax
				mov word [es: di + (80*18+8)*2], ax
				mov word [es: di + (80*19+50)*2], ax
				add di, 2
			loop Black7
			
		mov cx, 5
		mov di, 0
			Black5:
				mov word [es: di + (80*7+12)*2], ax
				mov word [es: di + (80*13+35)*2], ax
				mov word [es: di + (80*17+13)*2], ax
				mov word [es: di + (80*17+59)*2], ax
				mov word [es: di + (80*18+16)*2], ax
				mov word [es: di + (80*18+62)*2], ax
				add di, 2
			loop Black5
		
		mov cx, 4
		mov di, 0
			Horizontal_Black4:
				mov word [es: di + (80*2+40)*2], ax
				mov word [es: di + (80*2+44)*2], ax
				mov word [es: di + (80*2+48)*2], ax
				mov word [es: di + (80*3+27)*2], ax
				mov word [es: di + (80*3+51)*2], ax
				mov word [es: di + (80*6+58)*2], ax
				mov word [es: di + (80*6+62)*2], ax
				mov word [es: di + (80*7+17)*2], ax
				mov word [es: di + (80*7+38)*2], ax
				mov word [es: di + (80*13+52)*2], ax
				mov word [es: di + (80*13+56)*2], ax
				mov word [es: di + (80*18+57)*2], ax
				add di, 2
			loop Horizontal_Black4
		
		mov cx, 3
		mov di, 0
			Horizontal_Black3:
				mov word [es: di + (80*5+21)*2], ax
				mov word [es: di + (80*5+56)*2], ax
				mov word [es: di + (80*6+19)*2], ax
				mov word [es: di + (80*7+26)*2], ax
				mov word [es: di + (80*8+10)*2], ax
				mov word [es: di + (80*8+29)*2], ax
				mov word [es: di + (80*9+8)*2], ax
				mov word [es: di + (80*9+28)*2], ax
				mov word [es: di + (80*10+26)*2], ax
				mov word [es: di + (80*11+24)*2], ax
				mov word [es: di + (80*11+67)*2], ax
				mov word [es: di + (80*21+14)*2], ax
				mov word [es: di + (80*21+60)*2], ax
				add di, 2
			loop Horizontal_Black3
				
		mov cx, 4
		mov di, 0
			Vertical_Black4:
				mov word [es: di + (80*3+25)*2], ax
				mov word [es: di + (80*3+26)*2], ax
				mov word [es: di + (80*4+53)*2], ax
				mov word [es: di + (80*4+54)*2], ax
				mov word [es: di + (80*8+18)*2], ax
				mov word [es: di + (80*10+7)*2], ax
				mov word [es: di + (80*10+41)*2], ax
				mov word [es: di + (80*12+69)*2], ax
				mov word [es: di + (80*12+70)*2], ax
				mov word [es: di + (80*13+19)*2], ax
				mov word [es: di + (80*14+7)*2], ax
				mov word [es: di + (80*16+39)*2], ax
				add di, 160
			loop Vertical_Black4
			
		mov cx, 3
		mov di, 0
			Vertical_Black3:
				mov word [es: di + (80*7+42)*2], ax
				mov word [es: di + (80*11+6)*2], ax
				mov word [es: di + (80*13+40)*2], ax
				mov word [es: di + (80*14+6)*2], ax
				mov word [es: di + (80*14+59)*2], ax
				add di, 160
			loop Vertical_Black3
		
		mov cx, 2
		mov di, 0
			Black2:
				mov word [es:di + (80*4+23)*2], ax
				mov word [es:di + (80*4+55)*2], ax
				mov word [es:di + (80*7+65)*2], ax
				mov word [es:di + (80*8+54)*2], ax
				mov word [es:di + (80*8+66)*2], ax
				mov word [es:di + (80*9+55)*2], ax
				mov word [es:di + (80*9+67)*2], ax
				mov word [es:di + (80*10+56)*2], ax
				mov word [es:di + (80*10+68)*2], ax
				mov word [es:di + (80*11+57)*2], ax
				add di, 2
			loop Black2
		
		mov cx, 2
		mov di, 0
			Black22:
				mov word [es:di + (80*13+8)*2], ax
				mov word [es:di + (80*15+35)*2], ax
				mov word [es:di + (80*15+43)*2], ax
				mov word [es:di + (80*16+68)*2], ax
				mov word [es:di + (80*17+67)*2], ax
				mov word [es:di + (80*19+12)*2], ax
				mov word [es:di + (80*19+17)*2], ax
				mov word [es:di + (80*19+58)*2], ax
				mov word [es:di + (80*19+63)*2], ax
				mov word [es:di + (80*20+13)*2], ax
				mov word [es:di + (80*20+16)*2], ax
				mov word [es:di + (80*20+59)*2], ax
				mov word [es:di + (80*20+62)*2], ax
				add di, 2
			loop Black22
				
				
			; remainings
			mov word [es:(80*7+59)*2], ax
			mov word [es:(80*8+60)*2], ax
			mov word [es:(80*9+61)*2], ax
			mov word [es:(80*10+8)*2], ax
			mov word [es:(80*10+62)*2], ax
			mov word [es:(80*11+19)*2], ax
			mov word [es:(80*11+62)*2], ax
			mov word [es:(80*17+8)*2], ax
			mov word [es:(80*17+19)*2], ax
			mov word [es:(80*18+21)*2], ax

	;printing string on screen
	mov ax, 3584
	push ax ; push y position
	mov ax,0x0F ; blue on black attribute
	push ax ; push attribute
	mov ax, game_n
	push ax ; push address of message
	push word [game_len] ; push message length
	call printstr
	
	pop di
	pop cx
	pop ax
	
ret

;subroutine to create path in the centre of road during rotation
pathcar_rotation:

	push dx
	push ax

	inc word[_path]

	cmp word[_path],5		;check for creating path after some time
	jne path_end

	mov dh,0x7F
	mov dl,0xDE
	
	mov ax,0xb800
	mov es,ax
	
	mov word[es:68],dx
	mov word[es:228],dx	

	mov word[_path],0

	path_end:

	pop ax
	pop dx

ret

;subroutine to blink the main car when the game stops
blink_main_car:

    push di

	mov di,0xb800
	mov es,di

    mov di,[move]
	
		mov word[es:di],0x86D4
		add di,2
		mov word[es:di],0x84D2
		add di,2
		mov word[es:di],0x86BE
		add di,156
		mov word[es:di],0x84C6
		add di,2
		mov word[es:di],0x8BCE
		add di,2
		mov word[es:di],0x84B5
		add di,156
		mov word[es:di],0x8ECD;D4
		add di,2
		mov word[es:di],0x8ECA
		add di,2
		mov word[es:di],0x8ECD;

    pop di

ret

;subroutine when the left key is pressed then do this function
left_key:

	sub word[move],2
	mov di,[move]
	
	mov word[es:di],0x79D4
	add di,2
	mov word[es:di],0x79D2
	add di,2
	mov word[es:di],0x79BE
       
    mov word[es:di+2],0x07DB
		
    add di,156
	mov word[es:di],0x79C6
	add di,2
	mov word[es:di],0x79CE
	add di,2
	mov word[es:di],0x79B5

    mov word[es:di+2],0x07DB

	add di,156
	mov word[es:di],0x79CD;D4
	add di,2
	mov word[es:di],0x79CA
	add di,2
	mov word[es:di],0x79CD;

    mov word[es:di+2],0x07DB

ret

;subroutine when the right key is pressed then do this function
right_key:

	add word[move],2
	mov di,[move]

    mov word[es:di-2],0x07DB

	mov word[es:di],0x79D4
	add di,2
	mov word[es:di],0x79D2
	add di,2
	mov word[es:di],0x79BE
    add di,156

    mov word[es:di-2],0x07DB

	mov word[es:di],0x79C6
	add di,2
	mov word[es:di],0x79CE
	add di,2
	mov word[es:di],0x79B5
	add di,156

    mov word[es:di-2],0x07DB

	mov word[es:di],0x79CD;D4
	add di,2
	mov word[es:di],0x79CA
	add di,2
	mov word[es:di],0x79CD;

ret

;interrupt for controlling the hardware interrupts
keys_isr: 

    push cx
	push ax
	push es
	push si
	push dx

	mov dx,0x0FDB
	mov ax, 0xb800
	mov es, ax ; point es to video memory

	mov si,[move]
	;mov [es:si],dx
	in al, 0x60 ; read a char from keyboard port
    mov cl,al
	cmp al, 75 ; is the key left key
	jne nextcmp ; no, try next comparison

	cmp word[move],3408
	jl nextcmp
	
	call left_key

	jmp nomatch ; leave interrupt routine
	
	nextcmp: 
		
		cmp al, 77 ; is the key right key
		jne nextcmp_Esc ; no, leave interrupt routine
		
		cmp word[move],3444
		jg nextcmp_Esc
        
		call right_key

		jmp nomatch

	nextcmp_Esc:

		cmp al, 1 ; is the key right shift
		jne nomatch
		
		escape:
			mov word[bool],1


	nomatch: 
        mov al,0x20
        out 0x20,al

	 	pop dx
	 	pop si
	 	pop es
	 	pop ax
        
        pop cx

iret

;subroutine to call different varieties of functions before the start of game to initialize game preferences
game_start:

	call System_Renew

	call _delay_

	call clrscr

	call three
	
	call _delay_

	call clrscr

	call two
	 
	call _delay_

	call clrscr

	call one

	call _delay_

	call clrscr

	call go

	call _delay_

	call clrscr

	;hooking and storing old ISR
	xor ax, ax
	mov es, ax ; point es to IVT base
	mov ax, [es:9*4]
	mov [oldisr], ax ; save offset of old routine
	mov ax, [es:9*4+2]
	mov [oldisr+2], ax ; save segment of old routine

	cli ; disable interrupts
	mov word[es:9*4], keys_isr ; store offset at n*4
	mov [es:9*4+2], cs ; store segment at n*4+2
	sti ; enable interrupts

	call clrscr
	
	call creating_Module
	
	call Starttng

	call storing_path

ret

;subroutine to print three on the screen
three:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 544 ; (80*3+32)*2
		mov cx, 18
			horizontal_lines_for3:
				mov word [es:di], ax ; (80*3+32)*2
				mov word [es:di + 1280], ax ; (80*11+32)*2
				mov word [es:di + 2560], ax ; (80*19+32)*2
				add di, 2
			loop horizontal_lines_for3
	
		mov di, 738 ; (80*4+49)*2
		mov cx, 7
			vertical_lines_for3:
				mov word [es:di], ax ; (80*4+49)*2
				mov word [es:di + 1280], ax ; (80*12+49)*2
				add di, 2
				mov word [es:di], ax ; (80*4+50)*2 
				mov word [es:di + 1280], ax ; (80*12+50)*2
				add di, 158
			loop vertical_lines_for3
	
	pop di
	pop cx
	pop ax
	
ret

;subroutine to print two on the screen	
two:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 544 ; (80*3+32)*2
		mov cx, 18
			horizontal_lines_for2:
				mov word [es:di], ax ; (80*3+32)*2
				mov word [es:di + 1280], ax ; (80*11+32)*2
				mov word [es:di + 2560], ax ; (80*19+32)*2
				add di, 2
			loop horizontal_lines_for2
		
		mov di, 738 ; (80*4+49)*2
		mov cx, 7
			vertical_lines_for2:
				mov word [es:di], ax ; (80*4+49)*2
				mov word [es:di + 1244], ax ; (80*12+31)*2
				add di, 2
				mov word [es:di], ax ; (80*4+50)*2 
				mov word [es:di + 1244], ax ; (80*12+32)*2
				add di, 158
			loop vertical_lines_for2
	
	pop di
	pop cx
	pop ax
	
ret

;subroutine to print one on the screen
one:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 558 ; (80*3+39)*2
		mov cx, 17
			vertical_line_for1:
				mov word [es:di], ax ; (80*3+39)*2
				add di, 2
				mov word [es:di], ax ; (80*3+40)*2
				add di, 158
			loop vertical_line_for1
	
	pop di
	pop cx
	pop ax
	
ret

;subroutine to print GO alphabets on the screen
go:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 516 ; (80*3+18)*2
		mov cx, 16
			horizontal_lines_forGO:
				mov word [es:di], ax ; (80*3+18)*2
				mov word [es:di + 56], ax ; (80*3+46)*2
				mov word [es:di + 2560], ax ; (80*19+18)*2
				mov word [es:di + 2560 + 56], ax ; (80*19+46)*2
				add di, 2
			loop horizontal_lines_forGO
		
		mov di, 674 ; (80*4+17)*2 
		mov cx, 15
			vertical_lines_forGO:
				mov word [es:di], ax ; (80*4+17)*2
				mov word [es:di + 56], ax ; (80*4+45)*2
				mov word [es:di + 88], ax ; (80*4+61)*2
				add di, 2
				mov word [es:di], ax ; (80*4+18)*2 
				mov word [es:di + 56], ax ; (80*4+46)*2
				mov word [es:di + 88], ax ; (80*4+62)*2
				add di, 158
			loop vertical_lines_forGO
			
		mov di, 1986 ; (80*12+33)*2
		mov cx, 7
			small_vertical_lines_forGO:
				mov word [es:di], ax ; (80*12+33)*2
				add di, 2
				mov word [es:di], ax ; (80*12+34)*2 
				add di, 158
			loop small_vertical_lines_forGO
	
	pop di
	pop cx
	pop ax

ret

;subroutine to print gameover at the end of the screen
gameover:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0xaadb
	
		mov cx, 8
		mov di, 332 ; (80*2+6)*2
			horizontal_lines_for_GO:
				mov word [es:di], ax ; (80*2+6)*2
				mov word [es:di + 40], ax ; (80*2+26)*2
				mov word [es:di + 120], ax ; (80*2+66)*2
				mov word [es:di + 480 + 120], ax ; (80*5+66)*2
				mov word [es:di + 1120], ax ; (80*9+26)*2
				mov word [es:di + 1120 + 120], ax ; (80*9+66)*2
				mov word [es:di + 2080], ax ; (80*15+6)*2
				mov word [es:di + 2080 + 80], ax ; (80*15+46)*2
				mov word [es:di + 2080 + 120], ax ; (80*15+66)*2
				mov word [es:di + 2560 + 80], ax ; (80*18+46)*2
				mov word [es:di + 2560 + 120], ax ; (80*18+66)*2
				mov word [es:di + 3200], ax ; (80*22+6)*2
				mov word [es:di + 3200 + 80], ax ; (80*22+46)*2
				add di, 2
			loop horizontal_lines_for_GO
		
		mov cx, 6
		mov di, 0
			vertiacal_lines_for_GO:
				mov word [es:di + (80*3+5)*2], ax
				mov word [es:di + (80*3+6)*2], ax
				mov word [es:di + (80*3+25)*2], ax
				mov word [es:di + (80*3+26)*2], ax
				mov word [es:di + (80*3+33)*2], ax
				mov word [es:di + (80*3+34)*2], ax
				mov word [es:di + (80*3+45)*2], ax
				mov word [es:di + (80*3+46)*2], ax
				mov word [es:di + (80*3+53)*2], ax
				mov word [es:di + (80*3+54)*2], ax
				mov word [es:di + (80*3+65)*2], ax
				mov word [es:di + (80*3+66)*2], ax
				mov word [es:di + (80*16+5)*2], ax
				mov word [es:di + (80*16+6)*2], ax
				mov word [es:di + (80*16+13)*2], ax
				mov word [es:di + (80*16+14)*2], ax
				mov word [es:di + (80*16+45)*2], ax
				mov word [es:di + (80*16+46)*2], ax
				mov word [es:di + (80*16+65)*2], ax
				mov word [es:di + (80*16+66)*2], ax
				add di, 160
			loop vertiacal_lines_for_GO
			
		; all remaining manually
			; for G 
				mov word [es: (80*6+13)*2], ax
				mov word [es: (80*6+14)*2], ax
				mov word [es: (80*7+13)*2], ax
				mov word [es: (80*7+14)*2], ax
				mov word [es: (80*8+13)*2], ax
				mov word [es: (80*8+14)*2], ax
			
			; for A
				mov word [es: (80*5+27)*2], ax
				mov word [es: (80*5+28)*2], ax
				mov word [es: (80*5+29)*2], ax
				mov word [es: (80*5+30)*2], ax
				mov word [es: (80*5+31)*2], ax
			
			; for M
				mov word [es: (80*2+45)*2], ax
				mov word [es: (80*2+46)*2], ax
				mov word [es: (80*2+47)*2], ax
				mov word [es: (80*2+52)*2], ax
				mov word [es: (80*2+53)*2], ax
				mov word [es: (80*2+54)*2], ax
				mov word [es: (80*3+47)*2], ax
				mov word [es: (80*3+48)*2], ax
				mov word [es: (80*3+51)*2], ax
				mov word [es: (80*3+52)*2], ax
				mov word [es: (80*4+48)*2], ax
				mov word [es: (80*4+49)*2], ax
				mov word [es: (80*4+50)*2], ax
				mov word [es: (80*4+51)*2], ax
				mov word [es: (80*5+49)*2], ax
				mov word [es: (80*5+50)*2], ax
			
			; for Es
				mov word [es: (80*2+74)*2], ax
				mov word [es: (80*9+74)*2], ax
				mov word [es: (80*15+54)*2], ax
				mov word [es: (80*22+54)*2], ax
			
			; for V
				mov word [es: (80*15+25)*2], ax
				mov word [es: (80*15+26)*2], ax
				mov word [es: (80*15+33)*2], ax
				mov word [es: (80*15+34)*2], ax
				mov word [es: (80*16+25)*2], ax
				mov word [es: (80*16+26)*2], ax
				mov word [es: (80*16+33)*2], ax
				mov word [es: (80*16+34)*2], ax
				mov word [es: (80*17+26)*2], ax
				mov word [es: (80*17+27)*2], ax
				mov word [es: (80*17+32)*2], ax
				mov word [es: (80*17+33)*2], ax
				mov word [es: (80*18+26)*2], ax
				mov word [es: (80*18+27)*2], ax
				mov word [es: (80*18+32)*2], ax
				mov word [es: (80*18+33)*2], ax
				mov word [es: (80*19+27)*2], ax
				mov word [es: (80*19+28)*2], ax
				mov word [es: (80*19+31)*2], ax
				mov word [es: (80*19+32)*2], ax
				mov word [es: (80*20+27)*2], ax
				mov word [es: (80*20+28)*2], ax
				mov word [es: (80*20+31)*2], ax
				mov word [es: (80*20+32)*2], ax
				mov word [es: (80*21+28)*2], ax
				mov word [es: (80*21+29)*2], ax
				mov word [es: (80*21+30)*2], ax
				mov word [es: (80*21+31)*2], ax
				mov word [es: (80*22+28)*2], ax
				mov word [es: (80*22+29)*2], ax
				mov word [es: (80*22+30)*2], ax
				mov word [es: (80*22+31)*2], ax
				
			; for R
				mov word [es: (80*16+73)*2], ax
				mov word [es: (80*16+74)*2], ax
				mov word [es: (80*17+73)*2], ax
				mov word [es: (80*17+74)*2], ax
				mov word [es: (80*19+67)*2], ax
				mov word [es: (80*19+68)*2], ax
				mov word [es: (80*19+69)*2], ax
				mov word [es: (80*19+70)*2], ax
				mov word [es: (80*20+69)*2], ax
				mov word [es: (80*20+70)*2], ax
				mov word [es: (80*20+71)*2], ax
				mov word [es: (80*21+70)*2], ax
				mov word [es: (80*21+71)*2], ax
				mov word [es: (80*21+72)*2], ax
				mov word [es: (80*22+71)*2], ax
				mov word [es: (80*22+72)*2], ax
				mov word [es: (80*22+73)*2], ax
				mov word [es: (80*22+74)*2], ax
				mov word [es: (80*22+65)*2], ax
				mov word [es: (80*22+66)*2], ax
		
	pop di
	pop cx
	pop ax

ret

;subroutine to print loser when the player car collides
loser:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0xaadb
	
		mov cx, 6
		mov di, 0
			horizontal_lines_for_loser:
				mov word [es:di + (80*8+25)*2], ax
				mov word [es:di + (80*8+38)*2], ax
				mov word [es:di + (80*8+49)*2], ax
				mov word [es:di + (80*8+61)*2], ax
				mov word [es:di + (80*12+38)*2], ax
				mov word [es:di + (80*12+49)*2], ax
				mov word [es:di + (80*12+61)*2], ax
				mov word [es:di + (80*16+14)*2], ax
				mov word [es:di + (80*16+25)*2], ax
				mov word [es:di + (80*16+36)*2], ax
				mov word [es:di + (80*16+49)*2], ax
				add di, 2
			loop horizontal_lines_for_loser
			
		mov cx, 7
		mov di, 0
			vertiacal_lines_for_loser:
				mov word [es:di + (80*8+12)*2], ax
				mov word [es:di + (80*8+13)*2], ax
				mov word [es:di + (80*9+24)*2], ax
				mov word [es:di + (80*9+25)*2], ax
				mov word [es:di + (80*9+30)*2], ax
				mov word [es:di + (80*9+31)*2], ax
				mov word [es:di + (80*9+48)*2], ax
				mov word [es:di + (80*9+49)*2], ax
				mov word [es:di + (80*9+60)*2], ax
				mov word [es:di + (80*9+61)*2], ax
				add di, 160
			loop vertiacal_lines_for_loser
		
		; all remaining manually
			; for L
				mov word [es:(80*15+12)*2], ax
				mov word [es:(80*15+13)*2], ax
				mov word [es:(80*16+12)*2], ax
				mov word [es:(80*16+13)*2], ax
			; for S
				mov cx, 5
				mov di, 0
					verical_for_S:
						mov word [es:di + (80*8+36)*2], ax
						mov word [es:di + (80*8+37)*2], ax
						mov word [es:di + (80*12+42)*2], ax
						mov word [es:di + (80*12+43)*2], ax
						add di, 160
					loop verical_for_S
					
				mov word [es:(80*9+43)*2], ax
				mov word [es:(80*15+36)*2], ax
			; for E
				mov word [es:(80*8+55)*2], ax
				mov word [es:(80*16+55)*2], ax
			; for R
				mov word [es:(80*9+66)*2], ax
				mov word [es:(80*9+67)*2], ax
				mov word [es:(80*10+66)*2], ax
				mov word [es:(80*10+67)*2], ax
				mov word [es:(80*11+66)*2], ax
				mov word [es:(80*11+67)*2], ax
				mov word [es:(80*13+62)*2], ax
				mov word [es:(80*13+63)*2], ax
				mov word [es:(80*14+63)*2], ax
				mov word [es:(80*14+64)*2], ax
				mov word [es:(80*15+64)*2], ax
				mov word [es:(80*15+65)*2], ax
				mov word [es:(80*16+65)*2], ax
				mov word [es:(80*16+66)*2], ax
				mov word [es:(80*16+67)*2], ax
				mov word [es:(80*16+60)*2], ax
				mov word [es:(80*16+61)*2], ax
			
	pop di
	pop cx
	pop ax

ret

;subroutine to print winner when the player wins	
winner_:

	mov ax, 0xb800
	mov es, ax
	mov ax, 0xaadb
		
		mov cx, 6
		mov di, 0
			horizontal_lines_for_winner:
				mov word [es:di + (80*9+56)*2], ax
				mov word [es:di + (80*9+68)*2], ax
				mov word [es:di + (80*12+55)*2], ax
				mov word [es:di + (80*12+68)*2], ax
				mov word [es:di + (80*15+56)*2], ax
				add di, 2
			loop horizontal_lines_for_winner
	
		mov cx, 7
		mov di, 0
			vertiacal_lines_for_winner:
				mov word [es:di + (80*9+5)*2], ax
				mov word [es:di + (80*9+6)*2], ax
				mov word [es:di + (80*9+14)*2], ax
				mov word [es:di + (80*9+15)*2], ax
				mov word [es:di + (80*9+21)*2], ax
				mov word [es:di + (80*9+22)*2], ax
				mov word [es:di + (80*9+29)*2], ax
				mov word [es:di + (80*9+30)*2], ax
				mov word [es:di + (80*9+37)*2], ax
				mov word [es:di + (80*9+38)*2], ax
				mov word [es:di + (80*9+41)*2], ax
				mov word [es:di + (80*9+42)*2], ax
				mov word [es:di + (80*9+49)*2], ax
				mov word [es:di + (80*9+50)*2], ax
				mov word [es:di + (80*9+55)*2], ax
				mov word [es:di + (80*9+56)*2], ax
				mov word [es:di + (80*9+66)*2], ax
				mov word [es:di + (80*9+67)*2], ax
				add di, 160
			loop vertiacal_lines_for_winner
		
		; all remaining manually
			; for W
				mov word [es:(80*11+10)*2], ax
				mov word [es:(80*12+9)*2], ax
				mov word [es:(80*12+10)*2], ax
				mov word [es:(80*12+11)*2], ax
				mov word [es:(80*13+8)*2], ax
				mov word [es:(80*13+9)*2], ax
				mov word [es:(80*13+11)*2], ax
				mov word [es:(80*13+12)*2], ax
				mov word [es:(80*14+7)*2], ax
				mov word [es:(80*14+8)*2], ax
				mov word [es:(80*14+12)*2], ax
				mov word [es:(80*14+13)*2], ax
				mov word [es:(80*15+7)*2], ax
				mov word [es:(80*15+13)*2], ax
				
			; for I
				mov word [es:(80*9+20)*2], ax
				mov word [es:(80*9+23)*2], ax
				mov word [es:(80*15+20)*2], ax
				mov word [es:(80*15+23)*2], ax
		
			; for Ns
				mov cx, 6
				mov di, 0
					diagonal_for_N:
						mov word [es:di + (80*9+31)*2], ax
						mov word [es:di + (80*9+43)*2], ax
						add di, 160
						mov word [es:di + (80*9+31)*2], ax
						mov word [es:di + (80*9+43)*2], ax
						add di, 2
					loop diagonal_for_N
			
			; for R
				mov word [es:(80*10+73)*2], ax
				mov word [es:(80*10+74)*2], ax
				mov word [es:(80*11+73)*2], ax
				mov word [es:(80*11+74)*2], ax
				mov word [es:(80*13+68)*2], ax
				mov word [es:(80*13+69)*2], ax
				mov word [es:(80*13+70)*2], ax
				mov word [es:(80*14+70)*2], ax
				mov word [es:(80*14+71)*2], ax
				mov word [es:(80*14+72)*2], ax
				mov word [es:(80*15+72)*2], ax
				mov word [es:(80*15+73)*2], ax
				mov word [es:(80*15+74)*2], ax

ret

;subroutine to check win variable that the player win or not when the game stop
winner_loser:
	push ax
	push cx
	push di
	push si
	
	cmp word[win],0			;check for win
	jne wd

	mov ax,0xb800
	mov es,ax
	mov si,1280

	mov cx,720				;print black behind winner screen
	ww:

		mov word[es:si],0x0000
		add si,2

	loop ww

	call winner_

	jmp win_end

	wd:

	cmp word[win],0			;check for loser
	je win_end

	mov ax,0xb800
	mov es,ax
	mov si,1120;1280

	mov cx,880				;print black behind loser screen
	ll:

		mov word[es:si],0x0000
		add si,2

	loop ll

	call loser

	win_end:
	
	pop si			
	pop di
	pop cx
	pop ax

ret

;subroutine to check collision between player and enemy cars
collision1_check:

	mov si,[move]
	
	cmp word[collision1],si
	je collision1_ending
	
	cmp word[collision1+2],si
	je collision1_ending

	cmp word[collision1+4],si
	je collision1_ending

	add si,2
	cmp word[collision1],si
	je collision1_ending
	;-------------------------------

	mov si,[collision1+4]
	sub si,160
	cmp word[move],si
	je collision1_ending

	sub si,160
	cmp word[move],si
	je collision1_ending

	mov di,[move]
	add di,160
	cmp di,si
	je collision1_ending

	add di,160
	cmp di,si
	je collision1_ending

	add di,160
	cmp di,si
	je collision1_ending

	jmp c_ret

	collision1_ending:
	jmp ending 

	c_ret:

ret

;subroutine to check collision between player and enemy cars
collision2_check:

	mov si,[move]
	cmp word[collision2],si
	je collision2_ending

	cmp word[collision2+2],si
	je collision2_ending

	cmp word[collision2+4],si
	je collision2_ending

	;mov si,[move]
	add si,2
	cmp word[collision2],si
	je collision2_ending

	add si,2
	cmp word[collision2],si
	je collision2_ending

	;-----------------------------------------------

	mov si,[collision2+4]
	sub si,160
	cmp word[move],si
	je collision2_ending

	sub si,160
	cmp word[move],si
	je collision2_ending

	mov di,[move]
	add di,160
	cmp di,si
	je collision2_ending

	add di,160
	cmp di,si
	je collision2_ending

	add di,160
	cmp di,si
	je collision2_ending
	jne t_part

	collision2_ending:
	jmp ending 

	;-------------------------------------------------

	t_part:
	mov si,[collision2]
	sub si,160
	mov di,[move]
	add di,4
	cmp di,si
	je collision2_ending

	sub si,160
	cmp di,si
	je collision2_ending

	add di,160
	cmp di,si
	je collision2_ending

	add di,160
	cmp di,si
	je collision2_ending

	add di,160
	cmp di,si
	je collision2_ending

	jmp c_ret2

	c_ret2:

ret

;subroutine to check collision between player and enemy cars
collision3_check:

	mov si,[move]
	cmp word[collision3],si
	je collision3_ending

	cmp word[collision3+2],si
	je collision3_ending

	cmp word[collision3+4],si
	je collision3_ending

	;mov si,[move]
	add si,2
	cmp word[collision3],si
	je collision3_ending

	add si,2
	cmp word[collision3],si
	je collision3_ending

	;-----------------------------------------------

	mov si,[collision3+4]
	sub si,160
	cmp word[move],si
	je collision3_ending

	sub si,160
	cmp word[move],si
	je collision3_ending

	mov di,[move]
	add di,160
	cmp di,si
	je collision3_ending

	add di,160
	cmp di,si
	je collision3_ending

	add di,160
	cmp di,si
	je collision3_ending
	jne s_part

	collision3_ending:
	jmp ending 

	;-------------------------------------------------
	s_part:
	mov si,[collision3]
	sub si,160
	mov di,[move]
	add di,4
	cmp di,si
	je collision3_ending

	sub si,160
	cmp di,si
	je collision3_ending

	add di,160
	cmp di,si
	je collision3_ending

	add di,160
	cmp di,si
	je collision3_ending

	add di,160
	cmp di,si
	je collision3_ending

	jmp c_ret3

	c_ret3:

ret

;subroutine to check collision between player and enemy cars
collision4_check:

	mov si,[move]
	cmp word[collision4],si
	je collision4_ending

	cmp word[collision4+2],si
	je collision4_ending

	cmp word[collision4+4],si
	je collision4_ending

	;mov si,[move]
	add si,2
	cmp word[collision4],si
	je collision4_ending

	add si,2
	cmp word[collision4],si
	je collision4_ending

	;---------------------------------------------------------

	mov si,[collision4]
	sub si,160
	mov di,[move]
	add di,4
	cmp di,si
	je collision4_ending

	sub si,160
	cmp di,si
	je collision4_ending

	add di,160
	cmp di,si
	je collision4_ending

	add di,160
	cmp di,si
	je collision4_ending

	add di,160
	cmp di,si
	je collision4_ending

	jmp c_ret4

	collision4_ending:
	jmp ending 

	c_ret4:

ret

;subroutine for checking if collision between cars exits
ending:
	pop cx
	mov cx,1
jmp cx_ret

;label to call collision check functions
collision1_:

	add word[collision1],160
	add word[collision1+2],160
	add word[collision1+4],160

	call collision1_check

	cmp word[collision1],3888
	je boolcar1_collision
	bool_ret1:

jmp collision1_ret

;collision bool label
boolcar1_collision:
	;mov word[boolcar1],0
	push 3
    push 8234;7878
    call _sound
jmp bool_ret1

;label to call collision check functions
collision2_:

	add word[collision2],160
	add word[collision2+2],160
	add word[collision2+4],160

	call collision2_check

	cmp word[collision2],3900
	je boolcar2_collision
	bool_ret2:

jmp collision2_ret

;collision bool label
boolcar2_collision:
	;mov word[boolcar2],0
	push 3
    push 8234;7878
    call _sound
jmp bool_ret2

;label to call collision check functions
collision3_:

	add word[collision3],160
	add word[collision3+2],160
	add word[collision3+4],160

	call collision3_check

	cmp word[collision3],3912;3592
	je boolcar3_collision
	bool_ret3:

jmp collision3_ret

;collision bool label
boolcar3_collision:
	;mov word[boolcar3],0
	push 3
    push 8234;7878
    call _sound
jmp bool_ret3

;label to call collision check functions
collision4_:

	add word[collision4],160
	add word[collision4+2],160
	add word[collision4+4],160

	call collision4_check

	cmp word[collision4],3924
	je boolcar4_collision
	bool_ret4:

jmp collision4_ret

;collision bool label
boolcar4_collision:
	;mov word[boolcar4],0
	push 3
    push 8234;7878
    call _sound
jmp bool_ret4

;main collision subroutine to check different possibilities of collisions
collisions:

	cmp word[boolcar1],1
	je collision1_
	collision1_ret:

	cmp word[boolcar2],1
	je collision2_
	collision2_ret:

	cmp word[boolcar3],1
	je collision3_
	collision3_ret:

	cmp word[boolcar4],1
	je collision4_
	collision4_ret:

ret

;subroutine to print screen when the player car stops
end_screen:

	push ax
	push di
	push bx
	push si
	push cx

	;to check if player wins or loses
	call winner_loser

	;to unhook interrupts
	call Unhook_Interrupts

	;to blink player car when player car stops
	call blink_main_car
	
	;to play sound at the end of game
	call EndingGameSound

	;ending the blinking variable into red colour
	mov word[es:3689],0x0204

	;to create some delay when the player car stops
	mov cx,30
	ending_delay:

		call delay

	loop ending_delay

	mov ax,0xb800
	mov es,ax
	mov di,0
	mov bx,0
	mov si,3998

	loop2:

	mov cx,80
	mov ax,0
	inc bx

	call delay

	cmp bx,14
	je screen_end

	loop1:

		inc ax

		;to print colour from start
		mov word[es:di],0x07DB
		add di,2

		;to print colour from end
		mov word[es:si],0x0FDB
		sub si,2

		cmp ax,80
		je loop2

	loop loop1

	screen_end:

	mov ax, 680
	push ax ; push y position
	mov ax,0x71 ; blue on black attribute
	push ax ; push attribute
	mov ax, ending_script
	push ax ; push address of message
	push word [len_str] ; push message length
		
	call printstr ; call the printstr subroutine

	mov ax, 836
	push ax ; push y position
	mov ax,71 ; blue on black attribute
	push ax ; push attribute
	mov ax,script1
	push ax ; push address of message
	push word [len_script1] ; push message length

	call printstr ; call the printstr subroutine

	mov ax, 2464
	push ax ; push y position
	mov ax,0xB0 ; blue on black attribute
	push ax ; push attribute
	mov ax, sccore
	push ax ; push address of message
	push word [sccore_len] ; push message length
	call printstr

	;to print score when the collision stops
	push 2482
	push word[scores]
	call printnumber

	pop cx
	pop si
	pop bx
	pop di
	pop ax

ret

;subroutine to initialize the staring screen
start_screen:

	call clrscr

	call CAR

	mov cx,30
	ending_delay_:

		call delay

	loop ending_delay_

	call clrscr

	;printing string on screen
	mov ax, 690
	push ax ; push y position
	mov ax,0xF ; blue on black attribute
	push ax ; push attribute
	mov ax, play
	push ax ; push address of message
	push word [play_len] ; push message length
	call printstr

	;printing string on screen
	mov ax, 1010
	push ax ; push y position
	mov ax,0xF ; blue on black attribute
	push ax ; push attribute
	mov ax, escape_
	push ax ; push address of message
	push word [escape_len] ; push message length
	call printstr

	;printing string on screen
	mov ax, 1924
	push ax ; push y position
	mov ax,0x4F ; blue on black attribute
	push ax ; push attribute
	mov ax, note
	push ax ; push address of message
	push word [note_len] ; push message length
	call printstr

	;printing string on screen
	mov ax, 1470
	push ax ; push y position
	mov ax,0x1F ; blue on black attribute
	push ax ; push attribute
	mov ax, playgame
	push ax ; push address of message
	push word [playgame_len] ; push message length
	call printstr

	;printing string on screen
	mov ax, 3758
	push ax ; push y position
	mov ax,0x5F ; blue on black attribute
	push ax ; push attribute
	mov ax, partners
	push ax ; push address of message
	push word [partner_len] ; push message length
	call printstr

ret

;subroutine to print string
printstr: 
	
	push bp
	mov bp, sp
	
	push es
	push ax
	push cx
	push si
	push di

	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di,	[bp+10] ; point di to required location
	mov si, [bp+6] ; point si to string
	mov cx, [bp+4] ; load length of string in cx
	mov ah, [bp+8] ; load attribute in ah

	cld ; auto increment mode
	nextchar: 
	
		lodsb ; load next char in al
		stosw ; print char/attribute pair
		
	loop nextchar ; repeat for the whole string
	
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp

ret 8

;subroutine to remove the last screen before gamover screen like animations
removing_with_style:

	push ax
	push cx
	push si
	push di
	push bx
	push bp

	mov cx,40
	mov si,3920
	mov di,78
	mov ax,0xb800
	mov es,ax
	mov bx,0
	mov bp,3918
	mov bx,80

	mov cx,10
	loooper:

		push cx
		;add bx,80
		mov cx,40
		;sub bp,8

		loper:

			mov word[es:di],0x0000				;setting colour
            mov word[es:di+160],0x0000

			mov word[es:bx],0x0000				;setting colour
            mov word[es:bx+160],0x0000

			mov word[es:si],0x0000				;removing previous colour
            mov word[es:si-160],0x0000

			mov word[es:bp],0x0000				;removing previous colour
            mov word[es:bp-160],0x0000

			add bx,2
			add si,2
			sub di,2
			sub bp,2

			call delay_

		loop loper

		sub si,480
		add di,480
		sub bp,320
		add bx,320

		pop cx

	loop loooper

	pop bp
	pop bx
	pop di
	pop si
	pop cx
	pop ax

ret

;subroutine to unhook interrupt
Unhook_Interrupts:

	push ax
	push bx

	xor ax,ax
	mov es,ax


	mov ax, [cs:oldisr] ; read old offset in ax
 	mov bx, [cs:oldisr+2] ; read old segment in bx
 	cli ; disable interrupts
 	mov [es:9*4], ax ; restore old offset from ax
 	mov [es:9*4+2], bx ; restore old segment from bx
	;mov cs,oldisr
 	sti ; enable interrupts

	pop bx
	pop ax

ret

;subroutine for setting black colour on certain line
setting:

	push ax
	push cx
	push si

	mov ax,0xb800
	mov es,ax
	mov cx,20
	mov si,120

	s_set:

		mov word[es:si],0x0000
		add si,2

	loop s_set

	pop si
	pop cx
	pop ax

ret

;subroutine for changing curves
changer:

	cmp word[scores],250		;check for curves
	jne nx

	mov word[bool_curve],0		;setting curve bool to true

	nx:
	cmp word[scores],550		;check for curves
	jne e_

	mov word[bool_curve],0		;setting curve bool to true

	e_:

ret

;subroutine for handling curves
curves:

	cmp word[_curve],20			;check for start delay before tilted raod
	jle try

	
	call tree_rotation			;subroutine for tree on both sides of raod

	call changer				;subroutine for setting curve

	try:

	call curve_rightpath		;subroutine to tilt road towards right

	call curve_leftpath			;subroutine to tilt road towards left

ret

;main function
start:
	
	;subroutine to initialize the start screen
	call start_screen
	
	;loop for checking interrupts of welcome screen
	looper_:

	    mov ah, 0 ; service 0 – get keystroke
		int 0x16 ; call BIOS keyboard service

	    cmp al,27 ;Check for Escape Key    
		je terminate

		cmp al,0x70
		je display

		cmp al,0x50
		je display

	jmp looper_

	;label for moving towards the starting of the game
	display:

	;subroutine to intializing the gaming initial prefrences
	call game_start

	;copy limit to win the game
	mov cx,[win]
	
	;game main loop which performs main function playing
	counter:

		push cx

			call rotation_d

			call curves

			call setting

			call scores_counter

			call fuel_counter

            call pathcar_rotation

            call main_car

			call cars

			call collisions

			call delay

			dec word[win]

			inc word[_curve]

		pop cx

	cx_ret:
	loop counter
	
	;subroutine to handle the end screen display
	call end_screen

	;Interrupts to check whether to play again or not
	mov ax,0
	infinity:

		mov ah,0
		int 0x16
		
		cmp al,0x4E   	;N
		je eenndd

		cmp al,0x6E		;n
		je eenndd

		cmp al,0x79		;y
		je display

		cmp al,0x59		;Y
		je display

	jmp infinity

;just a label if a person decides to quit
eenndd:

	call removing_with_style

;terminating program
terminate:

	call clrscr

	call gameover

mov ax,0x4c00
int 0x21