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
  mov eax, 10
  mov dword [ebp-8], eax
  mov eax, test
  mov ecx, dword [ebp-8]
  push ecx
  mov ecx, 1
  push ecx
  mov dword [ebp-4], eax
  call dword [ebp-4]
  mov dword [ebp-4], eax
  mov dword [ebp-8], ecx
  mov ecx, dword [ebp-4]
  mov dword [ebp-8], ecx
  lea ecx, dword [ebp-8]
  mov dword [ebp-8], ecx
  mov ecx, dword [ebp-8]
  mov eax, 1
  mov ecx, [ecx+eax*4]
  mov dword [ebp-8], ecx
  mov ecx, dword [ebp-8]
  mov eax, 1
  lea ecx, [ecx+eax*4]
  mov dword [ebp-8], ecx
  mov ecx, strings+8
  mov dword [ebp-12], ecx
  mov ecx, printf
  mov eax, dword [globals+8]
  push eax
  mov dword [ebp-8], eax
  call ecx
  mov ecx, eax
  mov ecx, 0
  mov eax, ecx
  add esp, 12
  leave
  ret
test:
  push ebp
  mov ebp,esp
  sub esp, 4
  mov eax, dword [ebp+8]
  mov dword [ebp-4], eax
  mov eax, dword [ebp+12]
  mov dword [ebp-8], eax
  mov eax, dword [ebp-4]
  add eax, dword [ebp-8]
  mov eax, eax
  add esp, 4
  leave
  ret
