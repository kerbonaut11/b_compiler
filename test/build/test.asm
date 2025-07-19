format ELF
public main
extrn malloc
extrn printf
globals: dd 0,10,strings+0,strings+6,0
strings: db "hello",0,"b",0,"World",0,0
main:
  push ebp
  mov ebp,esp
  sub esp, 12
  mov edx, 10
  mov dword [ebp-8], edx
  mov edx, test
  mov edi, dword [ebp-8]
  push edi
  mov edi, 1
  push edi
  call edx
  mov edx, eax
  mov dword [ebp-8], edx
  mov edx, strings+8
  mov dword [ebp-12], edx
  mov edx, printf
  mov edi, dword [globals+8]
  push edi
  call edx
  mov edx, eax
  mov edx, 0
  mov eax, edx
  add esp, 12
  leave
  ret
test:
  push ebp
  mov ebp,esp
  sub esp, 4
  mov ebx, dword [ebp+8]
  mov esi, dword [ebp+12]
  add ebx, esi
  mov eax, ebx
  add esp, 4
  leave
  ret
