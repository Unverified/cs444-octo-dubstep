extern NATIVEjava.io.OutputStream.nativeWrite
debugwrite:
;print eax
push ebp
mov ebp,esp
call NATIVEjava.io.OutputStream.nativeWrite
pop ebp

;print newline
mov eax,14602
push ebp
mov ebp,esp
call NATIVEjava.io.OutputStream.nativeWrite
pop ebp
ret
;TODO: generate constructor assembly

;@@@@@@@@@@@@@ ENTRY POINT @@@@@@@@@@@@@
global _start
_start:

;=== METHODCALL TO test ===
call test
;===methodcall end===

push eax
call debugwrite
pop eax

mov eax, 1
int 0x80
int 3
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;######## METHOD test ########
test:	;TODO: set the label for this method
push ebp
mov ebp,esp
;varassign x
mov eax,14602	;literal val 14602
push eax	;assign x to eax


;FOR
;varassign i
mov eax,0	;literal val 0
push eax	;assign i to eax
jmp g131
g132:
;varassign x
;binop plus
push ebx	;saving
mov eax,1	;literal val 1
mov ebx,eax	;save binop rs result
;varuse id: x
mov eax,[ebp-4]
add eax,ebx
pop ebx	;restoring
mov [ebp-4],eax	;assign x to eax
g131:
;binop lt
push ebx	;saving
mov eax,14657	;literal val 14657
mov ebx,eax	;save binop rs result
;varuse id: x
mov eax,[ebp-4]
cmp eax,ebx
jl g134
mov eax,0
jmp g135
g134:
mov eax,1
g135:
pop ebx	;restoring
mov ecx,1
cmp eax,ecx
jne g133
jmp g132
g133:


;;RETURN
;varuse id: x
mov eax,[ebp-4]
add esp,4
pop ebp
ret
add esp,4

pop ebp
ret
;#############METHOD############
