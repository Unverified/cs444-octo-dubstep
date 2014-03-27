;TODO: generate constructor assembly
global _start
_start:
push ebp
mov ebp, esp
call test
pop ebp
mov eax, 1
int 0x80
int 3
test:
;TODO: generate method assembly
;varassign
;literal val 69
	mov ebx,69
	push ebx	;ebp-off 32
;varassign
;binop plus
	push eax	;ebp-off 64
;literal val 1
	mov ebx,1
	mov eax,ebx
;varuse id: x
	mov ebx,[ebp]
	add ebx,eax
	pop eax	;ebp-off 32
	mov [ebp],ebx
;TODO: generate return assembly
	add esp,32
